#!/usr/bin/env python3

__COL_DELIM = "\t"
__TOKEN_COUNT_COL_NAME = "TOKEN_COUNT"

def __token_count_idx(header):
	header = header.strip()
	col_names = header.split(__COL_DELIM)
	return col_names.index(__TOKEN_COUNT_COL_NAME)

if __name__ == "__main__":
	import sys
	
	print("tokencount")
	with open(sys.argv[1], 'r') as infile:
		token_count_idx = __token_count_idx(next(infile))
		for line in infile:
			line = line.strip()
			row_vals = line.split(__COL_DELIM)
			token_count = row_vals[token_count_idx]
			print(token_count)