#!/usr/bin/env python3

from collections import Counter
from decimal import Decimal

from common import COL_DELIM, RANK_COL_NAME, parse_row_cells


def __rank_idx(header):
	col_names = parse_row_cells(header)
	return col_names.index(RANK_COL_NAME)

def parse_rank_counts(lines):
	result = Counter()
	rank_idx = __rank_idx(next(lines))
	for line in lines:
		row_vals = parse_row_cells(line)
		rank = Decimal(row_vals[rank_idx])
		result[rank] += 1
		
	return result

if __name__ == "__main__":
	import sys
	if len(sys.argv) != 2:
		raise ValueError("Usage: %s INFILE > OUTFILE" % sys.argv[0])
	else:
		with open(sys.argv[1], 'r') as infile:
			rank_counts = parse_rank_counts(infile)

		sorted_rank_counts = sorted(rank_counts.items(), key=lambda item: item[0])
		print(COL_DELIM.join(("rank", "count")))
		for rank, count in sorted_rank_counts:
			print(COL_DELIM.join((str(rank), str(count))))
