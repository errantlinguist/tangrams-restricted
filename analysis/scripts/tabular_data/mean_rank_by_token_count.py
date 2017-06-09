#!/usr/bin/env python3

import numpy

from token_count_ranks import COL_DELIM, parse_token_count_ranks


if __name__ == "__main__":
	import sys
	with open(sys.argv[1], 'r') as infile:
		token_count_ranks = parse_token_count_ranks(infile)

	print(COL_DELIM.join(("tokencount", "meanrank", "ranksd")))	
	for token_count, ranks in sorted(token_count_ranks.items(), key=lambda item: item[0]):
		rank_arr = numpy.array(ranks, copy=False)
		mean = numpy.mean(rank_arr)
		std = numpy.std(rank_arr)
		row_vals = (token_count, mean, std)
		print(COL_DELIM.join((str(token_count), str(mean), str(std))))
