#!/usr/bin/env python3

from collections import defaultdict

import numpy

from statsmodels import robust
from token_count_ranks import COL_DELIM, parse_token_count_ranks


if __name__ == "__main__":
	import sys
	with open(sys.argv[1], 'r') as infile:
		token_count_ranks = parse_token_count_ranks(infile)
	
	print(COL_DELIM.join(("tokencount", "medianrank", "rankmad")))	
	for token_count, ranks in sorted(token_count_ranks.items(), key=lambda item: item[0]):
		rank_arr = numpy.array(ranks, copy=False)
		median = numpy.median(rank_arr)
		mad = robust.mad(rank_arr)
		row_vals = (token_count, median, mad)
		print(COL_DELIM.join((str(token_count), str(median), str(mad))))
