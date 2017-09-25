#!/usr/bin/env python3

import argparse
import csv
import re
import sys
from typing import FrozenSet

import numpy as np
import pandas as pd

CELL_MULTIVALUE_DELIM_PATTERN = re.compile("\\s*,\\s*")

_EMPTY_SET = frozenset()


def parse_set(cell_value: str) -> FrozenSet[str]:
	return frozenset(CELL_MULTIVALUE_DELIM_PATTERN.split(cell_value))


# def token_type_overlap(dyad):
#	own_relevant_tokens = dyad["RELEVANT_TOKENS_REFERENT"]
#	prev_tokens = dyad.shift(-1)["RELEVANT_TOKENS_REFERENT"]
#	union = own_relevant_tokens.union(prev_tokens)
#	overlap = own_relevant_tokens.intersection(prev_tokens)
#	return len(overlap) / len(union)


def token_type_overlap(x: pd.DataFrame) -> pd.Series:
	unions = pd.Series((len(previous_tokens.union(own_tokens))
						for previous_tokens, own_tokens in
						zip(x.RELEVANT_TOKENS_REFERENT, x.RELEVANT_TOKENS_REFERENT.shift(1).fillna(''))),
					   index=x.index)
	return (x.RELEVANT_TOKENS_REFERENT.str.len() -
			x.RELEVANT_TOKENS_REFERENT.diff().str.len()) \
		   / unions


def token_type_overlap_backup(x: pd.DataFrame) -> pd.Series:
	unified_token_sets = (previous_tokens.union(own_tokens) if pd.notnull(previous_tokens) else None for
						  own_tokens, previous_tokens in
						  zip(x.RELEVANT_TOKENS_REFERENT, x.RELEVANT_TOKENS_REFERENT.shift(1)))
	unified_token_set_sizes = (np.NaN if unified_token_set is None else len(unified_token_set) for unified_token_set in
							   unified_token_sets)
	unified_token_set_size_series = pd.Series(unified_token_set_sizes, index=x.index)
	return (x.RELEVANT_TOKENS_REFERENT.str.len() -
			x.RELEVANT_TOKENS_REFERENT.diff().str.len()) \
		   / unified_token_set_size_series


def previous_token_sets(x: pd.DataFrame) -> pd.Series:
	return x.RELEVANT_TOKENS_REFERENT.shift(1).fillna(None)


def __token_type_overlap(df: pd.DataFrame) -> pd.DataFrame:
	"""
	See <https://stackoverflow.com/a/46402641/1391325>
	:param df: The DataFrame instance to process.
	:return: A new DataFrame with token overlap ratios.
	"""
	dyad_instructor_groups = df.groupby(["DYAD", "INSTRUCTOR", "REFERENT"])
	group_overlap_series = dyad_instructor_groups.apply(token_type_overlap)

	result = group_overlap_series.reset_index(level=[0, 1], name="TokenOverlap")
	result = result.assign(ROUND=df.ROUND, RELEVANT_TOKENS_REFERENT=df.RELEVANT_TOKENS_REFERENT)
	# result = result.sort_values(["DYAD", "ROUND", "INSTRUCTOR"])
	# result = result[["DYAD", "ROUND", "INSTRUCTOR", "RELEVANT_TOKENS_REFERENT", "TokenOverlap"]]
	return result


def __token_type_overlap_backup(df: pd.DataFrame) -> pd.DataFrame:
	"""
	See <https://stackoverflow.com/a/46402641/1391325>
	:param df: The DataFrame instance to process.
	:return: A new DataFrame with token overlap ratios.
	"""
	tokens = df.RELEVANT_TOKENS_REFERENT
	result = df.assign(RELEVANT_TOKENS_REFERENT=tokens)
	dyad_instructor_groups = result.groupby(["DYAD", "INSTRUCTOR"])
	# group_overlap_series = dyad_instructor_groups.apply(
	#	lambda x: (x.RELEVANT_TOKENS_REFERENT.str.len() -
	#			   x.RELEVANT_TOKENS_REFERENT.diff().str.len()) \
	#			  / pd.Series([len(k[0].union(k[1]))
	#						   for k in
	#						   zip(x.RELEVANT_TOKENS_REFERENT, x.RELEVANT_TOKENS_REFERENT.shift(1).fillna(''))],
	#						  index=x.index))  # the for loop is part of this huge line
	group_overlap_series = dyad_instructor_groups.apply(token_type_overlap_backup)

	result = group_overlap_series.reset_index(level=[0, 1], name="TokenOverlap")
	result = result.assign(ROUND=df.ROUND, RELEVANT_TOKENS_REFERENT=df.RELEVANT_TOKENS_REFERENT)
	result = result.sort_values(["DYAD", "ROUND", "INSTRUCTOR"]).fillna("(no value)")
	result = result[["DYAD", "ROUND", "INSTRUCTOR", "RELEVANT_TOKENS_REFERENT", "TokenOverlap"]]
	return result


def __create_argparser():
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
	round_token_overlaps = __token_type_overlap_backup(round_tokens)
	round_token_overlaps.to_csv(sys.stdout, sep="\t", na_rep="N/A")


# print(round_token_overlaps)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
