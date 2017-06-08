#!/usr/bin/env python3

from collections import defaultdict
from statistics import median

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

	median_ranks = dict((token_count, median(ranks)) for token_count, ranks in token_count_ranks.items())
	sorted_median_ranks = sorted(median_ranks.items(), key=lambda item: item[0])
	print(__COL_DELIM.join(("tokencount", "medianrank")))
	for token_count, median_rank in sorted_median_ranks:
		print(__COL_DELIM.join((str(token_count), str(median_rank))))