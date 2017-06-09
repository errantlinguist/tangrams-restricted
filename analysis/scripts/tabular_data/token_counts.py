#!/usr/bin/env python3

from common import COL_DELIM, TOKEN_COUNT_COL_NAME


def __token_count_idx(header):
	header = header.strip()
	col_names = header.split(COL_DELIM)
	return col_names.index(TOKEN_COUNT_COL_NAME)

if __name__ == "__main__":
	import sys
	
	print("tokencount")
	with open(sys.argv[1], 'r') as infile:
		token_count_idx = __token_count_idx(next(infile))
		for line in infile:
			line = line.strip()
			row_vals = line.split(COL_DELIM)
			token_count = row_vals[token_count_idx]
			print(token_count)