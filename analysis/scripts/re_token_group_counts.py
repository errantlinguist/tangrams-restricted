#!/usr/bin/env python3

import csv
import itertools
import sys
import xml.etree.ElementTree
from collections import Counter

from annotations import ANNOTATION_NAMESPACES
from xml_files import walk_xml_files

COL_DELIM = '\t'
GROUP_LIST_DELIM = ","


class TokenGroupDataColumn(object):
	GROUP = "GROUP"
	TOKEN = "TOKEN"


def count_token_groups(infile_path, token_groups):
	result = Counter()

	print("Reading XML file \"{}\".".format(infile_path), file=sys.stderr)
	doc_tree = xml.etree.ElementTree.parse(infile_path)
	token_annots = doc_tree.iterfind(".//hat:t", ANNOTATION_NAMESPACES)
	tokens = (annot.text for annot in token_annots)
	group_sets = (token_groups.get(token) for token in tokens)
	for group_set in group_sets:
		if group_set:
			result.update(group_set)

	return result


def create_token_group_counts(token_counts, token_groups):
	result = Counter()
	for token, count in token_counts.items():
		try:
			group = token_groups[token]
			result[group] += count
		except KeyError:
			# No group for given token; Ignore it
			pass
	return result


def read_token_groups(infile_path):
	with open(infile_path, 'r') as infile:
		rows = csv.reader(infile, dialect="excel-tab")
		col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
		token_col_idx = col_idxs[TokenGroupDataColumn.TOKEN]
		group_col_idx = col_idxs[TokenGroupDataColumn.GROUP]
		token_groups = ((row[token_col_idx], row[group_col_idx]) for row in rows)
		return dict((token, frozenset(group.split(GROUP_LIST_DELIM))) for (token, group) in token_groups if group)


def print_tabular_counts(infile_token_group_counts, group_count_sums, file):
	item_key_getter = lambda item: item[0]
	ordered_group_counts = tuple(sorted(group_count_sums.items(), key=item_key_getter))
	ordered_groups = tuple(group for (group, _) in ordered_group_counts)
	header_cells = itertools.chain(("DYAD",), (group for (group, _) in ordered_group_counts), ("SUM",))
	print(COL_DELIM.join(header_cells), file=file)

	for infile, token_group_counts in sorted(infile_token_group_counts.items(), key=item_key_getter):
		counts = tuple(token_group_counts.get(group, 0) for group in ordered_groups)
		dyad_total_count = sum(counts)
		print(COL_DELIM.join(itertools.chain((infile,), (str(count) for count in counts), (str(dyad_total_count),))),
			  file=file)

	summary_counts = tuple(count for (_, count) in ordered_group_counts)
	summary_total_count = sum(summary_counts)
	summary_row_cells = itertools.chain(("SUM",), (str(count) for count in summary_counts), (str(summary_total_count),))
	print(COL_DELIM.join(summary_row_cells))


if __name__ == "__main__":
	if len(sys.argv) < 3:
		raise ValueError("Usage: {} TOKEN_GROUP_FILE INPATHS... > OUTFILE".format(sys.argv[0]))
	else:
		token_group_file = sys.argv[1]
		print("Reading token groups from \"{}\".".format(token_group_file), file=sys.stderr)
		token_groups = read_token_groups(token_group_file)
		print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

		inpaths = sys.argv[2:]
		infiles = walk_xml_files(*inpaths)
		infile_token_group_counts = dict((infile, count_token_groups(infile, token_groups)) for infile in infiles)

		group_count_sums = Counter()
		for group_counts in infile_token_group_counts.values():
			for group, count in group_counts.items():
				group_count_sums[group] += count

		print_tabular_counts(infile_token_group_counts, group_count_sums, sys.stdout)
