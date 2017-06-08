#!/usr/bin/env python3

from collections import defaultdict
from statsmodels import robust
import numpy

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
	with open(sys.argv[1], 'r') as infile:
		token_count_ranks = parse_token_count_ranks(infile)
	
	print(__COL_DELIM.join(("tokencount", "medianrank", "rankmad")))	
	for token_count, ranks in sorted(token_count_ranks.items(), key=lambda item: item[0]):
		rank_arr = numpy.array(ranks, copy=False)
		median = numpy.median(rank_arr)
		mad = robust.mad(rank_arr)
		row_vals = (token_count, median, mad)
		print(__COL_DELIM.join((str(token_count), str(median), str(mad))))
