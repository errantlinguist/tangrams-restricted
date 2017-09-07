#!/usr/bin/env python3

import argparse
import itertools
import sys
from collections import Counter
from decimal import Decimal, ROUND_HALF_UP
from typing import Dict, Iterable, Sequence, Container, TypeVar

import utterances
from re_token_group_counts import read_token_group_dict
from re_token_group_freqs import game_round_start_end_times, read_round_start_times, \
	game_round_utterances
from session_data import walk_session_data

COL_DELIM = '\t'

TOTAL_RESULTS_ROW_NAME = "TOTAL"

T = TypeVar('T')

__COLS_BEFORE_ROUND_COLS = ("DYAD",)
__DECIMAL_REPR_ROUNDING_EXP = Decimal('1.0000')


class RoundTokenTypeData(object):
	def __init__(self, round_token_type_counts, total_token_counts):
		self.round_token_type_counts = round_token_type_counts
		self.total_token_counts = total_token_counts

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	@property
	def round_count(self):
		return len(self.round_token_type_counts)


class SessionRoundTokenCounter(object):
	def __init__(self, seg_utt_factory: utterances.SegmentUtteranceFactory):
		self.seg_utt_factory = seg_utt_factory

	def __call__(self, named_sessions):
		result = {}
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
			round_start_end_times = tuple(game_round_start_end_times(iter(read_round_start_times(session))))
			round_count = len(round_start_end_times)
			print("Read {} game round(s).".format(round_count), file=sys.stderr)

			segments = utterances.read_segments(session.utts)
			utts = tuple(self.seg_utt_factory(segments))
			round_utts = (game_round_utterances(start_time, end_time, utts) for start_time, end_time in
						  round_start_end_times)
			round_token_counts = tuple(_count_utt_tokens(utts) for utts in round_utts)
			result[dyad_id] = round_token_counts

		return result


def find_falsey_tail_start_idx(elems: Sequence[T]):
	reversed_elems = reversed(elems)
	max_valid_idx = len(elems) - 1
	return next((max_valid_idx - i for i, elem in enumerate(reversed_elems) if elem), None)


def game_round_token_type_counts(round_token_counts: Iterable[Dict[str, int]]) -> RoundTokenTypeData:
	round_token_type_counts = []
	total_token_counts = Counter()
	for token_counts in round_token_counts:
		total_token_counts.update(token_counts)
		token_count = sum(total_token_counts.values())
		type_count = len(total_token_counts.keys())
		round_token_type_counts.append((token_count, type_count))

	return RoundTokenTypeData(round_token_type_counts, total_token_counts)


def session_round_token_type_data(named_sessions, relevant_tokens: Container[str]):
	seg_utt_factory = utterances.SegmentUtteranceFactory(lambda token: token in relevant_tokens)
	token_counter = SessionRoundTokenCounter(seg_utt_factory)
	session_round_token_counts = token_counter(named_sessions)
	for dyad_id, round_token_counts in session_round_token_counts.items():
		falsey_tail_start_idx = find_falsey_tail_start_idx(round_token_counts)
		if falsey_tail_start_idx:
			old_len = len(round_token_counts)
			round_token_counts = round_token_counts[:falsey_tail_start_idx + 1]
			print("Trimmed {} empty round(s).".format(old_len - len(round_token_counts)), file=sys.stderr)

		round_token_type_data = game_round_token_type_counts(round_token_counts)
		yield dyad_id, round_token_type_data


def _count_utt_tokens(utts: Iterable[utterances.Utterance]):
	result = Counter()
	for utt in utts:
		result.update(utt.content)
	return result


def __create_argparser():
	result = argparse.ArgumentParser(description="Count ratios of new token types occurring.")
	result.add_argument("token_group_file", metavar="TOKEN_GROUP_FILEPATH",
						help="The path to the token group mapping file to use.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The paths to search for sessions to process.")
	return result


def __create_rounded_decimal_repr(value: Decimal):
	return str(value.quantize(__DECIMAL_REPR_ROUNDING_EXP, ROUND_HALF_UP))


def __main(args):
	token_group_file_path = args.token_group_file
	print("Reading token groups from \"{}\".".format(token_group_file_path), file=sys.stderr)
	token_groups = read_token_group_dict(token_group_file_path)
	print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

	named_sessions = walk_session_data(args.inpaths)
	outfile = sys.stdout

	ordered_session_round_token_type_data = tuple(
		sorted(session_round_token_type_data(named_sessions, token_groups.keys()), key=lambda item: item[0]))
	max_round_count = max(
		round_token_type_data.round_count for _, round_token_type_data in ordered_session_round_token_type_data)
	round_col_idx_offset = len(__COLS_BEFORE_ROUND_COLS)
	round_colnames = ("R{}".format(i) for i in range(1, max_round_count + 1))
	print(COL_DELIM.join(itertools.chain(__COLS_BEFORE_ROUND_COLS, round_colnames)), file=outfile)
	for dyad_id, round_token_type_data in ordered_session_round_token_type_data:
		row = [""] * (max_round_count + round_col_idx_offset)
		row[0] = dyad_id
		for col_idx, token_type_counts in enumerate(round_token_type_data.round_token_type_counts,
													start=round_col_idx_offset):
			round_token_type_ratio = Decimal(token_type_counts[1]) / Decimal(token_type_counts[0])
			row[col_idx] = __create_rounded_decimal_repr(round_token_type_ratio)
		print(COL_DELIM.join(row), file=outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
