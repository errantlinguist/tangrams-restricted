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

def token_type_overlap(dyad):
	own_relevant_tokens = dyad["RELEVANT_TOKENS_REFERENT"]
	prev_tokens = dyad.shift(-1)["RELEVANT_TOKENS_REFERENT"]
	union = own_relevant_tokens.union(prev_tokens)
	overlap = own_relevant_tokens.intersection(prev_tokens)
	return len(overlap) / len(union)





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
	#round_tokens = pd.read_csv(inpath, sep="\t", dialect=csv.excel_tab, dtype={"RELEVANT_TOKENS_REFERENT" : str, "RELEVANT_TOKENS_SHAPE" : str })
	#tokens = round_tokens["RELEVANT_TOKENS_REFERENT"].str.split(',', expand=False).apply(frozenset)
	# https://stackoverflow.com/a/46402641/1391325
	round_tokens.sort_values(["DYAD", "ROUND"])
	df =  round_tokens.groupby(['DYAD', 'INSTRUCTOR']).apply(\
				   lambda x: (x["RELEVANT_TOKENS_REFERENT"].str.len() -
						  x["RELEVANT_TOKENS_REFERENT"].diff().str.len()) \
						/ pd.Series([len(k[0].union(k[1]))
	   for k in zip(x["RELEVANT_TOKENS_REFERENT"], x["RELEVANT_TOKENS_REFERENT"].shift(1).fillna(''))], index=x.index))\
			.reset_index(level=[0, 1], name='TokenOverlap')\
			.assign(ROUND=round_tokens.ROUND, Tokens=round_tokens["RELEVANT_TOKENS_REFERENT"])\
			.sort_values(['DYAD', 'ROUND', 'INSTRUCTOR'])\
			.fillna('(no value)')\
			 [['DYAD', 'INSTRUCTOR', 'ROUND', 'RELEVANT_TOKENS_REFERENT', 'TokenOverlap']]
	# dyad_names = round_tokens["DYAD"].unique()
	# for dyad_name in dyad_names:
	#	dyad_rows = round_tokens[round_tokens["DYAD"] == dyad_name]
	#	process_dyad(dyad_rows)
	#dyad_referents = round_tokens.groupby(["DYAD", "REFERENT"])#["ROUND", "RELEVANT_TOKENS_REFERENT"]  # .transform(token_types)
	#for dyad_referent in dyad_referents:
	#	group = dyad_referent[0]
	#	data = dyad_referent[1]
	#	for row in data.iterrows():
	#		print(type(row["RELEVANT_TOKENS_REFERENT"]))
	#round_tokens["TOKEN_OVERLAP_REFERENT"] =  round_tokens.groupby(["DYAD", "REFERENT"]).apply(lambda x: token_type_overlap(round_tokens))


	#dyad_referents.apply(lambda df : df.shift(-1) )
	#dyad_referents.shift(-1)
	#for bla in round_tokens.groupby(["DYAD", "REFERENT"]):
	#	 print (bla)
	#	 break
	#for dyad_referent in dyad_referents:
	#	group_key = dyad_referent[0]
	#	print(group_key)
	#	row = dyad_referent[1]
		#dyad_referent["OVERLAP_SELF_REFERENT"] = token_type_overlap(dyad_referent)
	#	print(type(row))
	#	print(row["RELEVANT_TOKENS_REFERENT"])
	#	print(len(row["RELEVANT_TOKENS_REFERENT"]))
		#for idx, stupid_thing in enumerate(dyad):
		#	print("{}\t{}".format(idx, stupid_thing))



# for dyad in dyads:
#	print(dyad)


# round_tokens.sort(columns=["DYAD", "ROUND"], inplace=True)
# print(type(round_tokens))
# dyads = round_tokens.groupby("DYAD")
# for dyad in dyads:
#	process_dyad(dyad)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
