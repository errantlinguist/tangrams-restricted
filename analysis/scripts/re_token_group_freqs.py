#!/usr/bin/env python3

import itertools
import sys
from collections import Counter
from decimal import Decimal, ROUND_HALF_UP, getcontext

from re_token_group_counts import read_annot_token_group_counts, read_token_group_dict
from xml_files import walk_xml_files

COL_DELIM = '\t'


def print_tabular_freqs(infile_token_group_counts, group_count_sums, file):
	item_key_getter = lambda item: item[0]
	ordered_group_counts = tuple(
		(group, Decimal(count)) for group, count in sorted(group_count_sums.items(), key=item_key_getter))
	ordered_groups = tuple(group for (group, _) in ordered_group_counts)
	header_cells = itertools.chain(("DYAD",), (group for (group, _) in ordered_group_counts))
	print(COL_DELIM.join(header_cells), file=file)

	for infile, token_group_counts in sorted(infile_token_group_counts.items(), key=item_key_getter):
		counts = tuple(Decimal(token_group_counts.get(group, 0)) for group in ordered_groups)
		dyad_total_count = Decimal(sum(counts))
		freqs = (count / dyad_total_count for count in counts)
		print(COL_DELIM.join(itertools.chain((infile,), (str(freq) for freq in freqs))),
			  file=file)

	summary_counts = tuple(count for (_, count) in ordered_group_counts)
	summary_total_count = sum(summary_counts)
	summary_freqs = (count / summary_total_count for count in summary_counts)
	summary_row_cells = itertools.chain(("TOTAL",), (str(freq) for freq in summary_freqs))
	print(COL_DELIM.join(summary_row_cells))


if __name__ == "__main__":
	if len(sys.argv) < 3:
		raise ValueError("Usage: {} TOKEN_GROUP_FILE INPATHS... > OUTFILE".format(sys.argv[0]))
	else:
		token_group_file = sys.argv[1]
		print("Reading token groups from \"{}\".".format(token_group_file), file=sys.stderr)
		token_groups = read_token_group_dict(token_group_file)
		print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

		inpaths = sys.argv[2:]
		infiles = walk_xml_files(*inpaths)
		infile_token_group_counts = dict((infile, read_annot_token_group_counts(infile, token_groups)) for infile in infiles)

		group_count_sums = Counter()
		for group_counts in infile_token_group_counts.values():
			for group, count in group_counts.items():
				group_count_sums[group] += count

		ctx = getcontext()
		ctx.prec = 3
		ctx.rounding = ROUND_HALF_UP
		print_tabular_freqs(infile_token_group_counts, group_count_sums, sys.stdout)