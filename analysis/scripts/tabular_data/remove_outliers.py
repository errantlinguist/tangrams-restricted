#!/usr/bin/env python3

import csv

from common import COL_DELIM, TOKEN_COUNT_COL_NAME

MAX_TOKEN_COUNT = 200


def print_filtered_results(rows, max_token_count):
	header = next(rows)
	print(COL_DELIM.join(header))
	col_name_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(header))
	token_count_idx = col_name_idxs[TOKEN_COUNT_COL_NAME]
	for row in reader:
		token_count = int(row[token_count_idx])
		if token_count <= max_token_count:
			print(COL_DELIM.join(row))


if __name__ == "__main__":
	import sys

	if len(sys.argv) != 2:
		raise ValueError("Usage: %s INFILE > OUTFILE" % sys.argv[0])
	else:
		with open(sys.argv[1]) as inf:
			reader = csv.reader(inf, delimiter=COL_DELIM, skipinitialspace=True)
			print_filtered_results(reader, MAX_TOKEN_COUNT)
