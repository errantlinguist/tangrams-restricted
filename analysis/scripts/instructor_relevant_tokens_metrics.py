#!/usr/bin/env python3

import argparse
import csv
import re
import sys
from typing import FrozenSet, Iterable

import numpy as np
import pandas as pd

CELL_MULTIVALUE_DELIM_PATTERN = re.compile("\\s*,\\s*")


def create_token_type_self_overlap_series(df: pd.DataFrame, col_name: str) -> pd.Series:
	intersected_token_sets = (previous_tokens.intersection(own_tokens) if pd.notnull(previous_tokens) else None for
							  own_tokens, previous_tokens in
							  zip(df[col_name], df[col_name].shift(1)))
	intersected_token_set_sizes = (np.NaN if unified_token_set is None else len(unified_token_set) for unified_token_set
								   in
								   intersected_token_sets)
	intersected_token_set_size_series = pd.Series(intersected_token_set_sizes, index=df.index)
	unified_token_sets = (previous_tokens.union(own_tokens) if pd.notnull(previous_tokens) else None for
						  own_tokens, previous_tokens in
						  zip(df[col_name], df[col_name].shift(1)))
	unified_token_set_sizes = (np.NaN if unified_token_set is None else len(unified_token_set) for unified_token_set in
							   unified_token_sets)
	unified_token_set_size_series = pd.Series(unified_token_set_sizes, index=df.index)
	return intersected_token_set_size_series / unified_token_set_size_series


def parse_set(cell_value: str) -> FrozenSet[str]:
	return frozenset(CELL_MULTIVALUE_DELIM_PATTERN.split(cell_value))


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

	shape_levels = ("DYAD", "INSTRUCTOR", "SHAPE")
	dyad_instructor_referent_groups = df.groupby(shape_levels)
	group_referent_token_self_overlap_series = dyad_instructor_referent_groups.apply(
		lambda group_df: create_token_type_self_overlap_series(group_df, "RELEVANT_TOKENS_SHAPE"))
	shape_token_self_overlap_col_name = "RELEVANT_TOKENS_SHAPE_OVERLAP_SELF"
	shape_token_self_overlap_df = group_referent_token_self_overlap_series.reset_index(level=shape_levels,
																					   name=shape_token_self_overlap_col_name)
	df[shape_token_self_overlap_col_name] = shape_token_self_overlap_df[shape_token_self_overlap_col_name]

	df.sort_values(["DYAD", "REFERENT", "INSTRUCTOR", "ROUND"], inplace=True)


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
	round_tokens["RELEVANT_TOKENS_REFERENT"] = round_tokens["RELEVANT_TOKENS_REFERENT"].map(
		__token_set_repr)
	round_tokens["RELEVANT_TOKENS_SHAPE"] = round_tokens["RELEVANT_TOKENS_SHAPE"].map(
		__token_set_repr)
	round_tokens.to_csv(sys.stdout, index_label="INDEX", sep="\t", na_rep="N/A")


def __token_set_repr(tokens: Iterable[str]) -> str:
	return ','.join(sorted(tokens))


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
