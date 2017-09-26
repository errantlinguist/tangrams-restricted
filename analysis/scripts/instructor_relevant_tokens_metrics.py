#!/usr/bin/env python3

import argparse
import csv
import re
import sys
from enum import Enum, unique
from typing import FrozenSet, Iterable, Iterator, Tuple, TypeVar

import numpy as np
import pandas as pd

CELL_MULTIVALUE_DELIM_PATTERN = re.compile("\\s*,\\s*")
OUTPUT_NA_REPR = "N/A"
OVERLAP_NULL_VALUE = np.NaN

T = TypeVar('T')


@unique
class DataColumn(Enum):
	COREF_SEQ = "_COREF_SEQ"
	OVERLAP = "_OVERLAP"


@unique
class Metric(Enum):
	SELF = "_SELF"
	OTHER = "_OTHER"
	EITHER = "_EITHER"


def iterate_prev_rows(df: pd.DataFrame, cols: pd.Series, referent_id_col_name: str) -> Iterator[
	pd.Series]:
	current_row = cols
	while True:
		dyad = current_row["DYAD"]
		referent_id = current_row[referent_id_col_name]
		current_round = current_row["ROUND"]
		# print("Current round: {}".format(current_round), file=sys.stderr)
		prev_complement_rows = df.loc[
			(df["DYAD"] == dyad) & (df[referent_id_col_name] == referent_id) & (df["ROUND"] < current_round)]
		# print("prev_complement_rows" + prev_complement_rows)
		try:
			last_prev_complement_row = prev_complement_rows.loc[prev_complement_rows["ROUND"].argmax()]
			yield last_prev_complement_row
			current_row = last_prev_complement_row
		except ValueError:
			break


def iterate_prev_complement_rows(df: pd.DataFrame, cols: pd.Series, referent_id_col_name: str) -> Iterator[
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


def create_token_type_other_overlap_series(df: pd.DataFrame, referent_id_col_name: str,
										   token_set_col_name: str):
	coref_seq_col_name = token_set_col_name + DataColumn.COREF_SEQ.value + Metric.OTHER.value
	overlap_col_name = token_set_col_name + DataColumn.OVERLAP.value + Metric.OTHER.value

	for idx, cols in df.iterrows():
		prev_complement_rows = tuple(iterate_prev_complement_rows(df, cols, referent_id_col_name))
		coref_seq_no = len(prev_complement_rows) + 1
		if coref_seq_no > 1:
			last_prev_complement_row = prev_complement_rows[len(prev_complement_rows) - 1]
			overlap = set_overlap(cols[token_set_col_name], last_prev_complement_row[token_set_col_name])
		else:
			overlap = OVERLAP_NULL_VALUE

		df.loc[idx, coref_seq_col_name] = coref_seq_no
		df.loc[idx, overlap_col_name] = overlap


def create_token_type_either_overlap_series(df: pd.DataFrame, referent_id_col_name: str,
											token_set_col_name: str):
	coref_seq_col_name = token_set_col_name + DataColumn.COREF_SEQ.value + Metric.EITHER.value
	overlap_col_name = token_set_col_name + DataColumn.OVERLAP.value + Metric.EITHER.value

	for idx, cols in df.iterrows():
		prev_complement_rows = tuple(iterate_prev_rows(df, cols, referent_id_col_name))
		coref_seq_no = len(prev_complement_rows) + 1
		if coref_seq_no > 1:
			last_prev_complement_row = prev_complement_rows[len(prev_complement_rows) - 1]
			overlap = set_overlap(cols[token_set_col_name], last_prev_complement_row[token_set_col_name])
		else:
			overlap = OVERLAP_NULL_VALUE

		df.loc[idx, coref_seq_col_name] = coref_seq_no
		df.loc[idx, overlap_col_name] = overlap


def create_token_type_self_overlap_series(df: pd.DataFrame, token_set_col_name: str) -> pd.Series:
	return pd.Series(((OVERLAP_NULL_VALUE if pd.isnull(preceding_tokens) else set_overlap(own_tokens, preceding_tokens)) for	(own_tokens, preceding_tokens) in zip_previous_row_values(df, token_set_col_name)), index=df.index)


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
	if first:
		intersection = first.intersection(complement)
		union = first.union(complement)
		result = len(intersection) / len(union)
	else:
		# Don't compute overlap for utterances which don't have any relevant tokens
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
	dyad_instructor_referent_levels = ("DYAD", "INSTRUCTOR", referent_col_name)
	dyad_instructor_referent_groups = df.groupby(dyad_instructor_referent_levels)
	group_token_self_overlap_series = dyad_instructor_referent_groups.apply(
		lambda group_df: create_token_type_self_overlap_series(group_df, token_col_name))
	token_self_overlap_col_name = token_col_name + DataColumn.OVERLAP.value + Metric.SELF.value
	token_self_overlap_df = group_token_self_overlap_series.reset_index(
		level=dyad_instructor_referent_levels,
		name=token_self_overlap_col_name)
	df[
		token_col_name + DataColumn.COREF_SEQ.value + Metric.SELF.value] = dyad_instructor_referent_groups.cumcount().transform(
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
	create_token_type_other_overlap_series(df, referent_col_name, token_col_name)
	print("Calculating either overlap for \"{}\".".format(token_col_name), file=sys.stderr)
	create_token_type_either_overlap_series(df, referent_col_name, token_col_name)


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
