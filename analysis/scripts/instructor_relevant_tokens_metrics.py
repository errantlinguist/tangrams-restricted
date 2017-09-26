#!/usr/bin/env python3

import argparse
import csv
import re
import sys
from collections import defaultdict
from typing import FrozenSet, Iterable, Iterator, Optional, Tuple, TypeVar

import numpy as np
import pandas as pd

CELL_MULTIVALUE_DELIM_PATTERN = re.compile("\\s*,\\s*")
COREF_SEQ_COL_NAME_SUFFIX = "_COREF_SEQ"
OVERLAP_COL_NAME_SUFFIX = "_OVERLAP"
OVERLAP_NULL_VALUE = np.NaN

T = TypeVar('T')


class Coreference(object):
	"""
	This class represents a single reference in a coreference chain.
	"""

	def __init__(self, chain_seq_no: int, instructor: str, round_id: int, tokens: FrozenSet[str],
				 antecedent: Optional["Coreference"]):
		self.chain_seq_no = chain_seq_no
		self.instructor = instructor
		self.round_id = round_id
		self.tokens = tokens
		self.antecedent = antecedent

	@property
	def __key(self):
		return self.chain_seq_no, self.instructor, self.round_id, self.tokens, self.antecedent

	def __eq__(self, other):
		return (self is other or (isinstance(other, type(self))
								  and self.__key == other.__key))

	def __ne__(self, other):
		return not (self == other)

	def __hash__(self):
		return hash(self.__key)

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	@property
	def antecedents(self) -> Iterator["Coreference"]:
		last_antecedent = self.antecedent
		while last_antecedent is not None:
			last_antecedent = last_antecedent.antecedent
			yield last_antecedent


def next_complement_coref(instructor: str, first_coref: Coreference) -> Optional[Coreference]:
	result = first_coref
	while result is not None and result.instructor != instructor:
		result = result.antecedent
	return result


def create_token_type_other_overlap_series(df: pd.DataFrame, referent_id_col_name: str,
										   token_set_col_name: str):
	coref_seq_col_name = token_set_col_name + COREF_SEQ_COL_NAME_SUFFIX + "_OTHER"
	overlap_col_name = token_set_col_name + OVERLAP_COL_NAME_SUFFIX + "_OTHER"

	dyad_last_corefs = defaultdict(dict)
	for idx, cols in df.iterrows():
		dyad = cols["DYAD"]
		last_corefs = dyad_last_corefs[dyad]
		current_instructor = cols["INSTRUCTOR"]
		current_round = cols["ROUND"]
		current_tokens = cols[token_set_col_name]

		referent_id = cols[referent_id_col_name]
		try:
			last_coref = last_corefs[referent_id]
			assert last_coref.round_id < current_round
			current_coref = Coreference(last_coref.chain_seq_no + 1, current_instructor, current_round, current_tokens,
										last_coref)

			last_complement_coref = next_complement_coref(current_instructor, last_coref)
			if last_complement_coref:
				overlap = set_overlap(current_coref.tokens, last_complement_coref.tokens)
			else:
				overlap = OVERLAP_NULL_VALUE
		except KeyError:
			current_coref = Coreference(1, current_instructor, current_round, current_tokens, None)
			overlap = OVERLAP_NULL_VALUE

		df.loc[idx, coref_seq_col_name] = current_coref.chain_seq_no
		df.loc[idx, overlap_col_name] = overlap
		last_corefs[referent_id] = current_coref


def create_token_type_self_overlap_series(df: pd.DataFrame, col_name: str) -> pd.Series:
	intersected_token_set_sizes = (
		OVERLAP_NULL_VALUE if pd.isnull(previous_tokens) else len(previous_tokens.intersection(own_tokens)) for
		own_tokens, previous_tokens in
		zip_previous_row_values(df, col_name))
	intersected_token_set_size_series = pd.Series(intersected_token_set_sizes, index=df.index)
	unified_token_set_sizes = (
		OVERLAP_NULL_VALUE if pd.isnull(previous_tokens) else len(previous_tokens.union(own_tokens)) for
		own_tokens, previous_tokens in
		zip_previous_row_values(df, col_name))
	unified_token_set_size_series = pd.Series(unified_token_set_sizes, index=df.index)
	# Perform vectorized division rather than division on each individual scalar value
	return intersected_token_set_size_series / unified_token_set_size_series


def parse_set(cell_value: str) -> FrozenSet[str]:
	return frozenset(CELL_MULTIVALUE_DELIM_PATTERN.split(cell_value))


def set_overlap(first: FrozenSet[T], complement: FrozenSet[T]) -> float:
	intersection = first.intersection(complement)
	union = first.union(complement)
	return len(intersection) / len(union)


def token_type_other_overlap(row_col_values: pd.Series, df: pd.DataFrame,
							 token_set_col_name: str) -> float:
	current_instructor = row_col_values["INSTRUCTOR"]
	current_round = row_col_values["ROUND"]
	preceding_complement_referent_rows = df.loc[
		(df["ROUND"] < current_round) & (
			df["INSTRUCTOR"] != current_instructor)]
	try:
		last_preceding_complement_referent_row = preceding_complement_referent_rows.loc[
			preceding_complement_referent_rows["ROUND"].idxmax()]
		own_tokens = row_col_values[token_set_col_name]
		preceding_tokens = last_preceding_complement_referent_row[token_set_col_name]
		result = set_overlap(own_tokens, preceding_tokens)
	except ValueError:
		# No preceding rows found
		result = OVERLAP_NULL_VALUE
	return result


def zip_previous_row_values(df: pd.DataFrame, col_name: str) -> Iterator[Tuple[T, T]]:
	return zip(df[col_name], df[col_name].shift())


def __token_type_overlap(df: pd.DataFrame, token_col_name: str, referent_col_name: str):
	"""
	See <https://stackoverflow.com/a/46402641/1391325>
	:param df: The DataFrame instance to process.
	"""
	# previous_rounds = df["ROUND"].transform(lambda game_round: game_round - 1)
	# df["PREVIOUS_ROUND"] = previous_rounds
	# dyad_referent_groups = df.groupby("DYAD")
	# df["COREF_SEQ_REFERENT_COMBINED"] = dyad_referent_groups.cumcount().transform(lambda seq_no : seq_no + 1)

	print("Calculating self overlap for \"{}\".".format(token_col_name), file=sys.stderr)
	instructor_referent_levels = ("DYAD", "INSTRUCTOR", referent_col_name)
	dyad_instructor_referent_groups = df.groupby(instructor_referent_levels)
	group_referent_token_self_overlap_series = dyad_instructor_referent_groups.apply(
		lambda group_df: create_token_type_self_overlap_series(group_df, token_col_name))
	referent_token_self_overlap_col_name = token_col_name + OVERLAP_COL_NAME_SUFFIX + "_SELF"
	referent_token_self_overlap_df = group_referent_token_self_overlap_series.reset_index(
		level=instructor_referent_levels,
		name=referent_token_self_overlap_col_name)
	df[
		token_col_name + COREF_SEQ_COL_NAME_SUFFIX + "_SELF"] = dyad_instructor_referent_groups.cumcount().transform(
		lambda seq_no: seq_no + 1)
	df[referent_token_self_overlap_col_name] = referent_token_self_overlap_df[referent_token_self_overlap_col_name]

	df.sort_values("ROUND", inplace=True)

	print("Calculating other overlap for \"{}\".".format(token_col_name), file=sys.stderr)
	create_token_type_other_overlap_series(df, referent_col_name, token_col_name)


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Measure referent token type overlap in coreference chains in each game session, using only instructor language to build coreference chains.")
	result.add_argument("inpath", metavar="INPATH",
						help="The file to process.")
	result.add_argument("-r", "--referent", metavar="COL_NAME", required=True,
						help="The column to use as a referent ID.")
	result.add_argument("-t", "--tokens", metavar="COL_NAME", required=True,
						help="The column to use as relevant tokens.")
	return result


def __main(args):
	inpath = args.inpath
	token_col_name = args.tokens
	referent_col_name = args.referent
	print("Reading \"{}\" using tokens from column \"{}\" and values from column \"{}\" as referent IDs.".format(inpath,
																												 token_col_name,
																												 referent_col_name),
		  file=sys.stderr)
	round_tokens = pd.read_csv(inpath, sep="\t", dialect=csv.excel_tab, encoding="utf-8", float_precision="high",
							   memory_map=True,
							   converters={"RELEVANT_TOKENS_REFERENT": parse_set, "RELEVANT_TOKENS_SHAPE": parse_set})

	__token_type_overlap(round_tokens, token_col_name, referent_col_name)

	round_tokens.sort_values(["DYAD", "REFERENT", "INSTRUCTOR", "ROUND"], inplace=True)
	round_tokens["RELEVANT_TOKENS_REFERENT"] = round_tokens["RELEVANT_TOKENS_REFERENT"].map(
		__token_set_repr)
	round_tokens["RELEVANT_TOKENS_SHAPE"] = round_tokens["RELEVANT_TOKENS_SHAPE"].map(
		__token_set_repr)
	round_tokens.to_csv(sys.stdout, index_label="INDEX", sep="\t", na_rep="N/A")


def __token_set_repr(tokens: Iterable[str]) -> str:
	return ','.join(sorted(tokens))


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
