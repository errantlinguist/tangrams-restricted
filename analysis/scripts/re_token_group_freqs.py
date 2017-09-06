#!/usr/bin/env python3

import argparse
import itertools
import sys
from collections import Counter
from decimal import Decimal, ROUND_HALF_UP, Context, localcontext
from typing import Dict, TextIO

from re_token_group_counts import read_annot_token_group_counts, read_token_group_dict
from xml_files import walk_xml_files

COL_DELIM = '\t'


def print_tabular_freqs(infile_token_group_counts: Dict[str, Dict[str, int]], group_count_sums: Dict[str, int],
						decimal_printing_ctx: Context, file: TextIO):
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
		with localcontext(decimal_printing_ctx) as _:
			print(COL_DELIM.join(itertools.chain((infile,), (str(freq) for freq in freqs))),
				  file=file)

	summary_counts = tuple(count for (_, count) in ordered_group_counts)
	summary_total_count = sum(summary_counts)
	summary_freqs = (count / summary_total_count for count in summary_counts)
	summary_row_cells = itertools.chain(("TOTAL",), (str(freq) for freq in summary_freqs))
	with localcontext(decimal_printing_ctx) as _:
		print(COL_DELIM.join(summary_row_cells))


def __create_argparser():
	result = argparse.ArgumentParser(description="Count frequencies of referring token groups.")
	result.add_argument("token_group_file", metavar="path",
						help="The path to the token group mapping file to use.")
	result.add_argument("inpaths", metavar="path", nargs='+',
						help="The paths to search for sessions to process.")
	result.add_argument("-r", "--round-split", metavar="count", type=int,
						help="When this option is supplied, each session is split into half, with the first half comprising this many game rounds.")
	return result


def __process_all_tokens(inpaths):
	infiles = walk_xml_files(*inpaths)
	infile_token_group_counts = dict(
		(infile, read_annot_token_group_counts(infile, token_groups)) for infile in infiles)
	print("Read token counts for {} file(s).".format(len(infile_token_group_counts)), file=sys.stderr)

	group_count_sums = Counter()
	for group_counts in infile_token_group_counts.values():
		for group, count in group_counts.items():
			group_count_sums[group] += count

	printing_ctx = Context(prec=3, rounding=ROUND_HALF_UP)
	print_tabular_freqs(infile_token_group_counts, group_count_sums, printing_ctx, sys.stdout)


if __name__ == "__main__":
	args = __create_argparser().parse_args()
	print(args)
	token_group_file_path = args.token_group_file
	print("Reading token groups from \"{}\".".format(token_group_file_path), file=sys.stderr)
	token_groups = read_token_group_dict(token_group_file_path)
	print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

	__process_all_tokens(args.inpaths)
