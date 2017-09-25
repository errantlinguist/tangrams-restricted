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
	return (x.RELEVANT_TOKENS_REFERENT.str.len() -
			x.RELEVANT_TOKENS_REFERENT.diff().str.len()) \
		   / pd.Series([len(k[0].union(k[1]))
						for k in zip(x.RELEVANT_TOKENS_REFERENT, x.RELEVANT_TOKENS_REFERENT.shift(1).fillna(''))],
					   index=x.index)  # the for loop is part of this huge line


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
	round_tokens.sort_values(["DYAD", "ROUND"])

	# https://stackoverflow.com/a/46402641/1391325
	tokens = round_tokens.RELEVANT_TOKENS_REFERENT
	round_token_overlaps = round_tokens.assign(RELEVANT_TOKENS_REFERENT=tokens)
	round_token_overlaps = round_token_overlaps.groupby(['DYAD', 'INSTRUCTOR']).apply(token_type_overlap)

	round_token_overlaps = round_token_overlaps.reset_index(level=[0, 1], name='TokenOverlap')
	round_token_overlaps = round_token_overlaps.assign(ROUND=round_tokens.ROUND,
													   RELEVANT_TOKENS_REFERENT=round_tokens.RELEVANT_TOKENS_REFERENT)
	round_token_overlaps = round_token_overlaps.sort_values(['DYAD', 'ROUND', 'INSTRUCTOR']).fillna('(no value)')
	round_token_overlaps = round_token_overlaps[
		['DYAD', 'INSTRUCTOR', 'ROUND', 'RELEVANT_TOKENS_REFERENT', 'TokenOverlap']]
	print(round_token_overlaps)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
