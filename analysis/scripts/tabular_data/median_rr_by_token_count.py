#!/usr/bin/env python3

from collections import defaultdict

import numpy

from common import COL_DELIM, parse_token_count_ranks
from statsmodels import robust


if __name__ == "__main__":
	import sys
	
	rank_cell_val_transformer= lambda rank_cell_value : 1.0 / numpy.float128(rank_cell_value)
	with open(sys.argv[1], 'r') as infile:
		common = parse_token_count_ranks(infile, rank_cell_val_transformer)
	
	print(COL_DELIM.join(("tokencount", "medianrr", "rrmad")))	
	for token_count, ranks in sorted(common.items(), key=lambda item: item[0]):
		rank_arr = numpy.array(ranks, copy=False)
		median = numpy.median(rank_arr)
		mad = robust.mad(rank_arr)
		row_vals = (token_count, median, mad)
		print(COL_DELIM.join((str(token_count), str(median), str(mad))))
