#!/usr/bin/env python3

import argparse
import csv
import itertools
import sys
from collections import Counter
from enum import Enum, unique
from typing import Dict, FrozenSet, Iterable, Iterator, Tuple

import utterances
from xml_files import walk_xml_files

COL_DELIM = '\t'
GROUP_LIST_DELIM = ","

_TokenGroupDict = Dict[str, Iterable[str]]


@unique
class TokenGroupDataColumn(Enum):
	GROUP = "GROUP"
	TOKEN = "TOKEN"


def create_utt_token_group_counts(utts: Iterable[utterances.Utterance], token_groups: _TokenGroupDict) -> \
		Dict[str, int]:
	result = Counter()

	tokens = semantically_relevant_tokens(utts)
	group_sets = (token_groups.get(token) for token in tokens)
	for group_set in group_sets:
		if group_set:
			result.update(group_set)

	return result


def print_tabular_counts(infile_token_group_counts, group_count_sums, outfile):
	ordered_group_counts = tuple(sorted(group_count_sums.items(), key=__get_item_key))
	ordered_groups = tuple(group for (group, _) in ordered_group_counts)
	header_cells = itertools.chain(("DYAD",), (group for (group, _) in ordered_group_counts), ("SUM",))
	print(COL_DELIM.join(header_cells), file=outfile)

	for infile, token_group_counts in sorted(infile_token_group_counts.items(), key=__get_item_key):
		counts = tuple(token_group_counts.get(group, 0) for group in ordered_groups)
		dyad_total_count = sum(counts)
		print(COL_DELIM.join(itertools.chain((infile,), (str(count) for count in counts), (str(dyad_total_count),))),
			  file=outfile)

	summary_counts = tuple(count for (_, count) in ordered_group_counts)
	summary_total_count = sum(summary_counts)
	summary_row_cells = itertools.chain(("SUM",), (str(count) for count in summary_counts), (str(summary_total_count),))
	print(COL_DELIM.join(summary_row_cells))


def read_token_group_dict(infile_path: str) -> Dict[str, FrozenSet[str]]:
	with open(infile_path, 'r') as inf:
		token_groups = readtoken_groups(inf)
		return dict(token_groups)


def readtoken_groups(infile) -> Iterator[Tuple[str, FrozenSet[str]]]:
	rows = csv.reader(infile, dialect="excel-tab")
	col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
	token_col_idx = col_idxs[TokenGroupDataColumn.TOKEN.value]
	group_col_idx = col_idxs[TokenGroupDataColumn.GROUP.value]
	token_groups = ((row[token_col_idx], row[group_col_idx]) for row in rows)
	return ((token, frozenset(group.split(GROUP_LIST_DELIM))) for (token, group) in token_groups if group)


def read_utt_token_group_counts(infile: str, token_groups: _TokenGroupDict,
								seg_utt_factory: utterances.SegmentUtteranceFactory):
	segments = utterances.read_segments(infile)
	utts = seg_utt_factory(segments)
	return create_utt_token_group_counts(utts, token_groups)


def semantically_relevant_tokens(utts: Iterable[utterances.Utterance]) -> Iterator[str]:
	# https://stackoverflow.com/a/18551476/1391325
	all_tokens = (token for utt in utts for token in utt.content)
	non_fillers = (token for token in all_tokens if token not in utterances.FILLER_TOKENS)
	return (token for token in non_fillers if not utterances.is_disfluency(token))


def __create_argparser():
	result = argparse.ArgumentParser(description="Count referring token groups.")
	result.add_argument("token_group_file", metavar="TOKEN_GROUP_FILEPATH",
						help="The path to the token group mapping file to use.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The paths to search for sessions to process.")
	return result


def __get_item_key(item):
	return item[0]


def __main(args):
	token_group_file_path = args.token_group_file
	print("Reading token groups from \"{}\".".format(token_group_file_path), file=sys.stderr)
	token_groups = read_token_group_dict(token_group_file_path)
	print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

	__process_whole_sessions(args.inpaths, token_groups, sys.stdout)


def __process_whole_sessions(inpaths: Iterable[str], token_groups: _TokenGroupDict, outfile):
	infiles = walk_xml_files(*inpaths)
	seg_utt_factory = utterances.SegmentUtteranceFactory()
	infile_token_group_counts = dict(
		(infile, read_utt_token_group_counts(infile, token_groups, seg_utt_factory)) for infile in infiles)
	print("Read token counts for {} file(s).".format(len(infile_token_group_counts)), file=sys.stderr)

	group_count_sums = Counter()
	for group_counts in infile_token_group_counts.values():
		for group, count in group_counts.items():
			group_count_sums[group] += count
	print_tabular_counts(infile_token_group_counts, group_count_sums, outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
