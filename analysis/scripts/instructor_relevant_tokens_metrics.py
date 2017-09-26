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
OUTPUT_NA_REPR = "N/A"
OTHER_METRIC_COL_NAME_SUFFIX = "_OTHER"
OVERLAP_COL_NAME_SUFFIX = "_OVERLAP"
OVERLAP_NULL_VALUE = np.NaN
SELF_METRIC_COL_NAME_SUFFIX = "_SELF"

T = TypeVar('T')


class Coreference(object):
	"""
	This class represents a single reference in a coreference chain.
	"""

	def __init__(self, chain_seq_no: int, instructor: str, round_id: int, tokens: FrozenSet[str]):
		self.chain_seq_no = chain_seq_no
		self.instructor = instructor
		self.round_id = round_id
		self.tokens = tokens

	@property
	def __key(self):
		return self.chain_seq_no, self.instructor, self.round_id, self.tokens

	def __eq__(self, other):
		return (self is other or (isinstance(other, type(self))
								  and self.__key == other.__key))

	def __ne__(self, other):
		return not (self == other)

	def __hash__(self):
		return hash(self.__key)

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)


def next_complement_coref(instructor: str, first_coref: Coreference) -> Optional[Coreference]:
	result = first_coref
	while result is not None and result.instructor == instructor:
		result = result.antecedent
	return result


def create_token_type_other_overlap_series_OLD(df: pd.DataFrame, referent_id_col_name: str,
											   token_set_col_name: str):
	coref_seq_col_name = token_set_col_name + COREF_SEQ_COL_NAME_SUFFIX + OTHER_METRIC_COL_NAME_SUFFIX
	overlap_col_name = token_set_col_name + OVERLAP_COL_NAME_SUFFIX + OTHER_METRIC_COL_NAME_SUFFIX
	df.sort_values("ROUND", inplace=True)

	dyad_coref_chains = defaultdict(lambda: defaultdict(list))
	for idx, cols in df.iterrows():
		dyad = cols["DYAD"]
		coref_chains = dyad_coref_chains[dyad]
		current_instructor = cols["INSTRUCTOR"]
		current_round = cols["ROUND"]
		current_tokens = cols[token_set_col_name]

		referent_id = cols[referent_id_col_name]
		coref_chain = coref_chains[referent_id]
		prev_complement_corefs = tuple(coref for coref in coref_chain if coref.instructor != current_instructor)
		next_coref_seq_no = len(prev_complement_corefs) + 1
		current_coref = Coreference(next_coref_seq_no, current_instructor, current_round,
									current_tokens)
		if next_coref_seq_no > 1:
			last_prev_complement_coref = prev_complement_corefs[len(prev_complement_corefs) - 1]
			assert last_prev_complement_coref.round_id < current_round
			overlap = set_overlap(current_coref.tokens, last_prev_complement_coref.tokens)
		else:
			overlap = OVERLAP_NULL_VALUE

		df.loc[idx, coref_seq_col_name] = current_coref.chain_seq_no
		df.loc[idx, overlap_col_name] = overlap
		coref_chain.append(current_coref)

	for dyad, coref_chains in dyad_coref_chains.items():
		for referent, coref_chain in coref_chains.items():
			prev_coref_seq_no = 0
			prev_round = 0
			for coref in coref_chain:
				assert prev_coref_seq_no < coref.chain_seq_no
				assert prev_round < coref.round_id
				prev_coref_seq_no = coref.chain_seq_no
				prev_round = coref.round_id


def iterate_prev_complement_rows(df: pd.DataFrame, cols: pd.Series) -> Iterator[pd.Series]:
	current_row = cols
	while True:
		current_round = current_row["ROUND"]
		print("Current round: {}".format(current_round), file=sys.stderr)
		current_instructor = current_row["INSTRUCTOR"]
		print("Current instructor to find complement of: {}".format(current_instructor), file=sys.stderr)
		prev_complement_rows = df.loc[(df["ROUND"] < current_round) & (df["INSTRUCTOR"] != current_instructor)]
		# print("prev_complement_rows" + prev_complement_rows)
		try:
			last_prev_complement_row = prev_complement_rows.loc[prev_complement_rows["ROUND"].argmax()]
			yield last_prev_complement_row
			current_row = last_prev_complement_row
		except ValueError:
			break


def iterate_prev_complement_rows_DYAD(df: pd.DataFrame, cols: pd.Series, referent_id_col_name: str) -> Iterator[
	pd.Series]:
	current_row = cols
	while True:
		dyad = current_row["DYAD"]
		referent_id = current_row[referent_id_col_name]
		current_round = current_row["ROUND"]
		# print("Current round: {}".format(current_round), file=sys.stderr)
		current_instructor = current_row["INSTRUCTOR"]
		# print("Current instructor to find complement of: {}".format(current_instructor), file=sys.stderr)
		prev_complement_rows = df.loc[
			(df["DYAD"] == dyad) & (df[referent_id_col_name] == referent_id) & (df["ROUND"] < current_round) & (
			df["INSTRUCTOR"] != current_instructor)]
		# print("prev_complement_rows" + prev_complement_rows)
		try:
			last_prev_complement_row = prev_complement_rows.loc[prev_complement_rows["ROUND"].argmax()]
			yield last_prev_complement_row
			current_row = last_prev_complement_row
		except ValueError:
			break


def create_token_type_other_overlap_series_DYAD(df: pd.DataFrame, referent_id_col_name: str,
												token_set_col_name: str):
	coref_seq_col_name = token_set_col_name + COREF_SEQ_COL_NAME_SUFFIX + OTHER_METRIC_COL_NAME_SUFFIX
	overlap_col_name = token_set_col_name + OVERLAP_COL_NAME_SUFFIX + OTHER_METRIC_COL_NAME_SUFFIX

	for idx, cols in df.iterrows():
		prev_complement_rows = tuple(iterate_prev_complement_rows_DYAD(df, cols, referent_id_col_name))
		coref_seq_no = len(prev_complement_rows) + 1
		if coref_seq_no > 1:
			last_prev_complement_row = prev_complement_rows[len(prev_complement_rows) - 1]
			overlap = set_overlap(cols[token_set_col_name], last_prev_complement_row[token_set_col_name])
		else:
			overlap = OVERLAP_NULL_VALUE

		df.loc[idx, coref_seq_col_name] = coref_seq_no
		df.loc[idx, overlap_col_name] = overlap


def create_coref_seq_other_overlap_series(df: pd.DataFrame,
										  token_set_col_name: str) -> pd.Series:
	series_vals = []
	for idx, cols in df.iterrows():
		prev_complement_rows = tuple(iterate_prev_complement_rows(df, cols))
		coref_seq_no = len(prev_complement_rows) + 1
		if coref_seq_no > 1:
			last_prev_complement_row = prev_complement_rows[len(prev_complement_rows) - 1]
			overlap = set_overlap(cols[token_set_col_name], last_prev_complement_row[token_set_col_name])
		else:
			overlap = OVERLAP_NULL_VALUE
		series_vals.append(overlap)
	return pd.Series(series_vals, index=df.index)


def create_token_type_other_overlap_series(df: pd.DataFrame,
										   token_set_col_name: str) -> pd.Series:
	series_vals = []
	for idx, cols in df.iterrows():
		prev_complement_rows = tuple(iterate_prev_complement_rows(df, cols))
		coref_seq_no = len(prev_complement_rows) + 1
		if coref_seq_no > 1:
			last_prev_complement_row = prev_complement_rows[len(prev_complement_rows) - 1]
			overlap = set_overlap(cols[token_set_col_name], last_prev_complement_row[token_set_col_name])
		else:
			overlap = OVERLAP_NULL_VALUE
		series_vals.append(overlap)
	return pd.Series(series_vals, index=df.index)


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


def prettify_token_set_series(df: pd.DataFrame):
	df["RELEVANT_TOKENS_REFERENT"] = df["RELEVANT_TOKENS_REFERENT"].map(
		__token_set_repr)
	df["RELEVANT_TOKENS_SHAPE"] = df["RELEVANT_TOKENS_SHAPE"].map(
		__token_set_repr)


def read_round_tokens(inpath: str, **kwargs) -> pd.DataFrame:
	return pd.read_csv(inpath, sep="\t", dialect=csv.excel_tab, encoding="utf-8", float_precision="high",
					   memory_map=True,
					   converters={"RELEVANT_TOKENS_REFERENT": parse_set, "RELEVANT_TOKENS_SHAPE": parse_set}, **kwargs)


def set_overlap(first: FrozenSet[T], complement: FrozenSet[T]) -> float:
	intersection = first.intersection(complement)
	union = first.union(complement)
	return len(intersection) / len(union)


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
	dyad_instructor_referent_levels = ("DYAD", "INSTRUCTOR", referent_col_name)
	dyad_instructor_referent_groups = df.groupby(dyad_instructor_referent_levels)
	group_token_self_overlap_series = dyad_instructor_referent_groups.apply(
		lambda group_df: create_token_type_self_overlap_series(group_df, token_col_name))
	token_self_overlap_col_name = token_col_name + OVERLAP_COL_NAME_SUFFIX + SELF_METRIC_COL_NAME_SUFFIX
	token_self_overlap_df = group_token_self_overlap_series.reset_index(
		level=dyad_instructor_referent_levels,
		name=token_self_overlap_col_name)
	df[
		token_col_name + COREF_SEQ_COL_NAME_SUFFIX + SELF_METRIC_COL_NAME_SUFFIX] = dyad_instructor_referent_groups.cumcount().transform(
		lambda seq_no: seq_no + 1)
	df[token_self_overlap_col_name] = token_self_overlap_df[token_self_overlap_col_name]

	df.sort_values("ROUND", inplace=True)

	# df.reset_index()
	print("Calculating other overlap for \"{}\".".format(token_col_name), file=sys.stderr)
	# create_token_type_other_overlap_series(df, referent_col_name, token_col_name)
	# dyad_referent_levels = ("DYAD", referent_col_name)
	# dyad_referent_groups = df.groupby(dyad_referent_levels)
	# other_overlap_series = dyad_referent_groups.apply(
	#	lambda group_df: create_coref_seq_other_overlap_series(group_df, token_col_name))
	# other_overlap_series.app
	create_token_type_other_overlap_series_DYAD(df, referent_col_name, token_col_name)


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
	round_tokens = read_round_tokens(inpath)

	__token_type_overlap(round_tokens, token_col_name, referent_col_name)

	round_tokens.sort_values(["DYAD", "REFERENT", "INSTRUCTOR", "ROUND"], inplace=True)
	prettify_token_set_series(round_tokens)
	round_tokens.to_csv(sys.stdout, index_label="INDEX", sep="\t", na_rep=OUTPUT_NA_REPR)


def __token_set_repr(tokens: Iterable[str]) -> str:
	return ','.join(sorted(tokens))


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
