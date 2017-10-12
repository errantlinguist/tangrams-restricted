#!/usr/bin/env python3

from decimal import Decimal

import numpy
from statsmodels import robust

from common import COL_DELIM, parse_token_count_ranks

__RECIPROCAL = Decimal("1.0")


def __rr(value: str) -> numpy.longfloat:
	decimal_value = Decimal(value)
	decimal_result = __RECIPROCAL / decimal_value
	return numpy.longfloat(decimal_result)


if __name__ == "__main__":
	import sys

	with open(sys.argv[1], 'r', encoding='utf-8') as infile:
		common = parse_token_count_ranks(infile, __rr)

	print(COL_DELIM.join(("tokencount", "medianrr", "rrmad")))
	for token_count, ranks in sorted(common.items(), key=lambda item: item[0]):
		rank_arr = numpy.array(ranks, copy=False)
		median = numpy.median(rank_arr)
		mad = robust.mad(rank_arr)
		row_vals = (token_count, median, mad)
		print(COL_DELIM.join((str(val) for val in row_vals)))
