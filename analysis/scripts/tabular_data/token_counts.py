#!/usr/bin/env python3

from common import TOKEN_COUNT_COL_NAME, parse_row_cells


def __token_count_idx(header):
	col_names = parse_row_cells(header)
	return col_names.index(TOKEN_COUNT_COL_NAME)

if __name__ == "__main__":
	import sys
	if len(sys.argv) != 2:
		raise ValueError("Usage: %s INFILE > OUTFILE" % sys.argv[0])
	else:
		print("tokencount")
		with open(sys.argv[1], 'r') as infile:
			token_count_idx = __token_count_idx(next(infile))
			for line in infile:
				row_vals = parse_row_cells(line)
				token_count = row_vals[token_count_idx]
				print(token_count)
