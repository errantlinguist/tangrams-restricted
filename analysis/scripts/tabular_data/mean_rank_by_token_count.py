#!/usr/bin/env python3

from collections import defaultdict
from statistics import mean

__COL_DELIM = "\t"
__RANK_COL_NAME = "RANK"
__TOKEN_COUNT_COL_NAME = "TOKEN_COUNT"

def __token_count_rank_idxs(header):
	header = header.strip()
	col_names = header.split(__COL_DELIM)
	rank_idx = col_names.index(__RANK_COL_NAME)
	token_count_idx = col_names.index(__TOKEN_COUNT_COL_NAME)
	return token_count_idx, rank_idx

def parse_token_count_ranks(lines):
	result = defaultdict(list)
	token_count_idx, rank_idx = __token_count_rank_idxs(next(lines))
	for line in lines:
		line = line.strip()
		row_vals = line.split(__COL_DELIM)
		token_count = int(row_vals[token_count_idx])
		rank = float(row_vals[rank_idx])
		result[token_count].append(rank)
		
	return result

if __name__ == "__main__":
	import sys
	with open(sys.argv[1]) as infile:
		token_count_ranks = parse_token_count_ranks(infile)

	mean_ranks = dict((token_count, mean(ranks)) for token_count, ranks in token_count_ranks.items())
	sorted_mean_ranks = sorted(mean_ranks.items(), key=lambda x: x[0])
	print(__COL_DELIM.join(("tokencount", "meanrank")))
	for token_count, mean_rank in sorted_mean_ranks:
		print(__COL_DELIM.join((str(token_count), str(mean_rank))))