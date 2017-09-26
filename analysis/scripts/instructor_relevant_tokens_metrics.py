#!/usr/bin/env python3

import argparse
import csv
import re
import sys
from typing import FrozenSet, Iterable, Iterator, Tuple, TypeVar

import numpy as np
import pandas as pd

CELL_MULTIVALUE_DELIM_PATTERN = re.compile("\\s*,\\s*")
OVERLAP_NULL_VALUE = np.NaN

T = TypeVar('T')


def create_token_type_other_overlap_series(df: pd.DataFrame, col_name: str) -> pd.Series:
	own_token_sets = df[col_name]


# TODO: Finish
# df[df['B']==3]['A']


def create_token_type_self_overlap_series(df: pd.DataFrame, col_name: str) -> pd.Series:
	intersected_token_set_sizes = (
		OVERLAP_NULL_VALUE if pd.isnull(previous_tokens) else len(previous_tokens.intersection(own_tokens)) for
		own_tokens, previous_tokens in
		zip_previous_row_values(df, col_name))
	intersected_token_set_size_series = pd.Series(intersected_token_set_sizes, index=df.index)
	unified_token_set_sizes = (OVERLAP_NULL_VALUE if pd.isnull(previous_tokens) else len(previous_tokens.union(own_tokens)) for
							   own_tokens, previous_tokens in
							   zip_previous_row_values(df, col_name))
	unified_token_set_size_series = pd.Series(unified_token_set_sizes, index=df.index)
	# Perform vectorized division rather than division on each individual scalar value
	return intersected_token_set_size_series / unified_token_set_size_series


def parse_set(cell_value: str) -> FrozenSet[str]:
	return frozenset(CELL_MULTIVALUE_DELIM_PATTERN.split(cell_value))


def zip_previous_row_values(df: pd.DataFrame, col_name: str) -> Iterator[Tuple[T, T]]:
	return zip(df[col_name], df[col_name].shift(1))


def __token_type_overlap(df: pd.DataFrame):
	"""
	See <https://stackoverflow.com/a/46402641/1391325>
	:param df: The DataFrame instance to process.
	"""
	referent_levels = ("DYAD", "INSTRUCTOR", "REFERENT")
	dyad_instructor_referent_groups = df.groupby(referent_levels)
	group_referent_token_self_overlap_series = dyad_instructor_referent_groups.apply(
		lambda group_df: create_token_type_self_overlap_series(group_df, "RELEVANT_TOKENS_REFERENT"))
	referent_token_self_overlap_col_name = "RELEVANT_TOKENS_REFERENT_OVERLAP_SELF"
	referent_token_self_overlap_df = group_referent_token_self_overlap_series.reset_index(level=referent_levels,
																						  name=referent_token_self_overlap_col_name)
	df[referent_token_self_overlap_col_name] = referent_token_self_overlap_df[referent_token_self_overlap_col_name]
	df["COREF_SEQ_REFERENT"] = dyad_instructor_referent_groups.cumcount()

	shape_levels = ("DYAD", "INSTRUCTOR", "SHAPE")
	dyad_instructor_shape_groups = df.groupby(shape_levels)
	group_referent_token_self_overlap_series = dyad_instructor_shape_groups.apply(
		lambda group_df: create_token_type_self_overlap_series(group_df, "RELEVANT_TOKENS_SHAPE"))
	shape_token_self_overlap_col_name = "RELEVANT_TOKENS_SHAPE_OVERLAP_SELF"
	shape_token_self_overlap_df = group_referent_token_self_overlap_series.reset_index(level=shape_levels,
																					   name=shape_token_self_overlap_col_name)
	df[shape_token_self_overlap_col_name] = shape_token_self_overlap_df[shape_token_self_overlap_col_name]
	df["COREF_SEQ_SHAPE"] = dyad_instructor_shape_groups.cumcount()


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Measure referent token type overlap in coreference chains in each game session, using only instructor language to build coreference chains.")
	result.add_argument("inpath", metavar="INPATH",
						help="The file to process.")
	return result


def __main(args):
	inpath = args.inpath
	print("Reading \"{}\".".format(inpath), file=sys.stderr)
	round_tokens = pd.read_csv(inpath, sep="\t", dialect=csv.excel_tab, encoding="utf-8", float_precision="high",
							   memory_map=True,
							   converters={"RELEVANT_TOKENS_REFERENT": parse_set, "RELEVANT_TOKENS_SHAPE": parse_set})

	__token_type_overlap(round_tokens)

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
