#!/usr/bin/env python3

import argparse
import csv
import re
import sys
from typing import FrozenSet, Iterable

import numpy as np
import pandas as pd

CELL_MULTIVALUE_DELIM_PATTERN = re.compile("\\s*,\\s*")


def parse_set(cell_value: str) -> FrozenSet[str]:
	return frozenset(CELL_MULTIVALUE_DELIM_PATTERN.split(cell_value))


def token_type_overlap(df: pd.DataFrame, col_name: str) -> pd.Series:
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


def __token_type_overlap(df: pd.DataFrame) -> pd.DataFrame:
	"""
	See <https://stackoverflow.com/a/46402641/1391325>
	:param df: The DataFrame instance to process.
	:return: A new DataFrame with token overlap ratios.
	"""
	tokens = df.RELEVANT_TOKENS_REFERENT
	result = df.assign(RELEVANT_TOKENS_REFERENT=tokens)
	dyad_instructor_referent_groups = result.groupby(["DYAD", "INSTRUCTOR", "REFERENT"])
	# group_overlap_series = dyad_instructor_referent_groups.apply(
	#	lambda x: (x.RELEVANT_TOKENS_REFERENT.str.len() -
	#			   x.RELEVANT_TOKENS_REFERENT.diff().str.len()) \
	#			  / pd.Series([len(k[0].union(k[1]))
	#						   for k in
	#						   zip(x.RELEVANT_TOKENS_REFERENT, x.RELEVANT_TOKENS_REFERENT.shift(1).fillna(''))],
	#						  index=x.index))  # the for loop is part of this huge line
	group_overlap_series = dyad_instructor_referent_groups.apply(
		lambda group_df: token_type_overlap(group_df, "RELEVANT_TOKENS_REFERENT"))

	# result = group_overlap_series.reset_index(level=[0, 1], name="TokenOverlap")
	# result = group_overlap_series.reset_index(name="TokenOverlap")

	# result = group_overlap_series.reset_index(level=[0, 1, 2], name="TokenOverlap")
	# result = group_overlap_series.reset_index(level=[0])
	result = result.assign(ROUND=df.ROUND, RELEVANT_TOKENS_REFERENT=df.RELEVANT_TOKENS_REFERENT)

	result = result.sort_values(["DYAD", "REFERENT", "INSTRUCTOR", "ROUND"])
	# result = result[["DYAD", "ROUND", "INSTRUCTOR", "RELEVANT_TOKENS_REFERENT", "TokenOverlap"]]
	return result


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Print out relevant tokens for each instructor utterance for each round in each session.")
	result.add_argument("inpath", metavar="INPATH",
						help="The file to process.")
	return result


def __main(args):
	inpath = args.inpath
	print("Reading \"{}\".".format(inpath), file=sys.stderr)
	round_tokens = pd.read_csv(inpath, sep="\t", dialect=csv.excel_tab, float_precision="high", memory_map=True,
							   converters={"RELEVANT_TOKENS_REFERENT": parse_set, "RELEVANT_TOKENS_SHAPE": parse_set})
	round_token_overlaps = __token_type_overlap(round_tokens)
	round_token_overlaps["RELEVANT_TOKENS_REFERENT"] = round_token_overlaps["RELEVANT_TOKENS_REFERENT"].map(
		__token_set_repr)
	round_token_overlaps["RELEVANT_TOKENS_SHAPE"] = round_token_overlaps["RELEVANT_TOKENS_SHAPE"].map(
		__token_set_repr)
	round_token_overlaps.to_csv(sys.stdout, index_label="INDEX", sep="\t", na_rep="N/A")


def __token_set_repr(tokens: Iterable[str]) -> str:
	return ','.join(sorted(tokens))


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
