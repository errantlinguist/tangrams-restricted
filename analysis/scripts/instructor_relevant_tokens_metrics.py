#!/usr/bin/env python3

import argparse
import csv
import re
import sys
from typing import FrozenSet

import pandas as pd

CELL_MULTIVALUE_DELIM_PATTERN = re.compile("\\s*,\\s*")


def parse_set(cell_value: str) -> FrozenSet[str]:
	return frozenset(CELL_MULTIVALUE_DELIM_PATTERN.split(cell_value))


# def token_type_overlap(dyad):
#	own_relevant_tokens = dyad["RELEVANT_TOKENS_REFERENT"]
#	prev_tokens = dyad.shift(-1)["RELEVANT_TOKENS_REFERENT"]
#	union = own_relevant_tokens.union(prev_tokens)
#	overlap = own_relevant_tokens.intersection(prev_tokens)
#	return len(overlap) / len(union)


def token_type_overlap(x: pd.DataFrame) -> pd.Series:
	overlaps = (len(preceding_tokens.intersection(own_tokens)) / len(preceding_tokens.union(own_tokens))
				for (preceding_tokens, own_tokens) in
				zip(x.RELEVANT_TOKENS_REFERENT, x.RELEVANT_TOKENS_REFERENT.shift(1).fillna('')))
	return pd.Series(overlaps,
					 index=x.index)


def __token_type_overlap(df: pd.DataFrame, tokens: pd.Series) -> pd.DataFrame:
	# https://stackoverflow.com/a/46402641/1391325
	df2 = df.assign(RELEVANT_TOKENS_REFERENT=tokens)
	df2 = df2.groupby(['DYAD', 'INSTRUCTOR']).apply(
		lambda x: (x.RELEVANT_TOKENS_REFERENT.str.len() -
				   x.RELEVANT_TOKENS_REFERENT.diff().str.len()) \
				  / pd.Series([len(k[0].union(k[1]))
							   for k in
							   zip(x.RELEVANT_TOKENS_REFERENT, x.RELEVANT_TOKENS_REFERENT.shift(1).fillna(''))],
							  index=x.index))  # the for loop is part of this huge line

	df2 = df2.reset_index(level=[0, 1], name='TokenOverlap')
	df2 = df2.assign(ROUND=df.ROUND, RELEVANT_TOKENS_REFERENT=df.RELEVANT_TOKENS_REFERENT)
	df2 = df2.sort_values(['DYAD', 'ROUND', 'INSTRUCTOR']).fillna('(no value)')
	df2 = df2[['DYAD', 'ROUND', 'INSTRUCTOR', 'RELEVANT_TOKENS_REFERENT', 'TokenOverlap']]
	return df2


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
	# round_tokens.sort_values(["DYAD", "ROUND"])

	# https://stackoverflow.com/a/46402641/1391325
	tokens = round_tokens.RELEVANT_TOKENS_REFERENT
	round_token_overlaps = __token_type_overlap(round_tokens, tokens)
	print(round_token_overlaps)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
