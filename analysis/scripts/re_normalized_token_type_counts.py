#!/usr/bin/env python3

import argparse
import itertools
import sys
from collections import Counter
from decimal import Decimal
from typing import Dict, Iterable, Sequence, Container, TypeVar

import utterances
from re_token_group_counts import read_token_group_dict
from re_token_group_freqs import game_round_start_end_times, read_round_start_times, \
	game_round_utterances
from session_data import walk_session_data

from re_token_type_counts import session_token_type_data

COL_DELIM = '\t'
NULL_VALUE_REPR = '?'

__COLS_BEFORE_ROUND_COLS = ("DYAD",)


def __create_argparser():
	result = argparse.ArgumentParser(description="Count normalized token-type ratios.")
	result.add_argument("token_group_file", metavar="TOKEN_GROUP_FILEPATH",
						help="The path to the token group mapping file to use.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The paths to search for sessions to process.")
	return result


def __create_rounded_decimal_repr(value: Decimal):
	return str(value)


def __main(args):
	token_group_file_path = args.token_group_file
	print("Reading token groups from \"{}\".".format(token_group_file_path), file=sys.stderr)
	token_groups = read_token_group_dict(token_group_file_path)
	print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

	named_sessions = walk_session_data(args.inpaths)
	outfile = sys.stdout
	token_type_data = session_token_type_data(named_sessions, token_groups.keys())
	for dyad_id, round_token_type_data in token_type_data:
		round_token_type_counts = round_token_type_data.round_token_type_counts
		total_token_counts = round_token_type_data.total_token_counts
		token_total = sum(total_token_counts.values())
		type_total = len(total_token_counts.keys())

		norm_vals = []
		for token_type_count in round_token_type_counts:
			norm_token_count = token_type_count.tokens / token_total
			norm_type_count = token_type_count.types / type_total
			diff = norm_token_count - norm_type_count
			norm_vals.append(diff)

		# TODO: Finish

	if args.ratios:
		__print_ratios(token_type_data, outfile)
	else:
		__print_token_type_counts(token_type_data, outfile)


def __print_ratios(token_type_data, outfile):
	ordered_session_round_token_type_data = tuple(sorted(token_type_data, key=lambda item: item[0]))
	max_round_count = max(
		round_token_type_data.round_count for _, round_token_type_data in ordered_session_round_token_type_data)
	round_col_idx_offset = len(__COLS_BEFORE_ROUND_COLS)
	row_cell_count = max_round_count + round_col_idx_offset
	round_colnames = ("R{}".format(i) for i in range(1, max_round_count + 1))
	print(COL_DELIM.join(itertools.chain(__COLS_BEFORE_ROUND_COLS, round_colnames)), file=outfile)
	for dyad_id, round_token_type_data in ordered_session_round_token_type_data:
		row = [NULL_VALUE_REPR] * row_cell_count
		row[0] = dyad_id
		for col_idx, token_type_counts in enumerate(round_token_type_data.round_token_type_counts,
													start=round_col_idx_offset):
			round_token_type_ratio = Decimal(token_type_counts.types) / Decimal(token_type_counts.tokens)
			row[col_idx] = __create_rounded_decimal_repr(round_token_type_ratio)
		print(COL_DELIM.join(row), file=outfile)


def __print_token_type_counts(token_type_data, outfile):
	ordered_session_round_token_type_data = tuple(sorted(token_type_data, key=lambda item: item[0]))
	max_round_count = max(
		round_token_type_data.round_count for _, round_token_type_data in ordered_session_round_token_type_data)
	round_col_idx_offset = len(__COLS_BEFORE_ROUND_COLS)
	round_datapoint_vector_size = 2
	row_cell_count = ((max_round_count * round_datapoint_vector_size) + round_col_idx_offset)
	header_cells = [NULL_VALUE_REPR] * row_cell_count
	header_idx = 0
	for prev_col_name in __COLS_BEFORE_ROUND_COLS:
		header_cells[header_idx] = prev_col_name
		header_idx += 1
	for round_id in range(1, max_round_count + 1):
		header_cells[header_idx] = "R{}_TOKENS".format(round_id)
		header_idx += 1
		header_cells[header_idx] = "R{}_TYPES".format(round_id)
		header_idx += 1
	print(COL_DELIM.join(header_cells), file=outfile)

	for dyad_id, round_token_type_data in ordered_session_round_token_type_data:
		row = [NULL_VALUE_REPR] * row_cell_count
		row[0] = dyad_id

		row_idx = round_col_idx_offset
		for token_type_counts in round_token_type_data.round_token_type_counts:
			row[row_idx] = str(token_type_counts.tokens)
			row_idx += 1
			row[row_idx] = str(token_type_counts.types)
			row_idx += 1
		print(COL_DELIM.join(row), file=outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
