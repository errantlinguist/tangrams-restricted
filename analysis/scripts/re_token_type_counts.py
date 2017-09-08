#!/usr/bin/env python3

import argparse
import sys
from collections import Counter
from decimal import Decimal
from typing import Dict, Iterable, Iterator, Sequence, Container, Tuple, TypeVar

import utterances
from re_token_group_counts import read_token_group_dict
from re_token_group_freqs import game_round_start_end_times, read_round_start_times, \
	game_round_utterances
from session_data import walk_session_data

COL_DELIM = '\t'
NULL_VALUE_REPR = '?'

T = TypeVar('T')


class TokenTypeDatum(object):
	def __init__(self, token_counts: Dict[str, int]):
		self.token_counts = token_counts

	@property
	def token_types(self):
		return self.token_counts.keys()

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)


class RoundTokenTypeDatum(object):
	def __init__(self, round_data: TokenTypeDatum, cumulative_data: TokenTypeDatum):
		self.round_data = round_data
		self.cumulative_data = cumulative_data

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)


class SessionTokenTypeDatum(object):
	@staticmethod
	def __add_round(token_counts: TokenTypeDatum, total_token_counts: Dict[str, int]) -> RoundTokenTypeDatum:
		cumulative_token_counts = TokenTypeDatum(Counter(total_token_counts))
		result = RoundTokenTypeDatum(token_counts, cumulative_token_counts)
		total_token_counts.update(token_counts.token_counts)
		return result

	def __init__(self, round_token_counts: Iterable[TokenTypeDatum]):
		self.total_token_counts = TokenTypeDatum(Counter())
		self.round_token_type_data = tuple(
			self.__add_round(token_counts, self.total_token_counts.token_counts) for token_counts in round_token_counts)

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	@property
	def round_count(self):
		return len(self.round_token_type_data)


class SessionRoundTokenCounter(object):
	@staticmethod
	def __count_utt_tokens(utts: Iterable[utterances.Utterance]) -> TokenTypeDatum:
		counts = Counter()
		for utt in utts:
			counts.update(utt.content)
		return TokenTypeDatum(counts)

	def __init__(self, seg_utt_factory: utterances.SegmentUtteranceFactory):
		self.seg_utt_factory = seg_utt_factory

	def __call__(self, named_sessions):
		result = {}
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
			round_token_counts = tuple(self.__session_token_type_counts(session))
			result[dyad_id] = round_token_counts

		return result

	def __session_token_type_counts(self, session) -> Iterator[TokenTypeDatum]:
		round_start_end_times = tuple(game_round_start_end_times(iter(read_round_start_times(session))))
		round_count = len(round_start_end_times)
		print("Read {} game round(s).".format(round_count), file=sys.stderr)

		segments = utterances.read_segments(session.utts)
		utts = tuple(self.seg_utt_factory(segments))
		round_utts = (game_round_utterances(start_time, end_time, utts) for start_time, end_time in
					  round_start_end_times)
		return (self.__count_utt_tokens(utts) for utts in round_utts)


def find_last_truthy_elem_idx(elems: Sequence[T]):
	reversed_elems = reversed(elems)
	max_valid_idx = len(elems) - 1
	return next((max_valid_idx - i for i, elem in enumerate(reversed_elems) if elem), -1)


def session_token_type_data(named_sessions, relevant_tokens: Container[str]) -> Iterator[
	Tuple[str, SessionTokenTypeDatum]]:
	seg_utt_factory = utterances.SegmentUtteranceFactory(lambda token: token in relevant_tokens)
	token_counter = SessionRoundTokenCounter(seg_utt_factory)
	session_round_token_counts = token_counter(named_sessions)
	for dyad_id, round_token_counts in session_round_token_counts.items():
		last_truthy_elem_idx = find_last_truthy_elem_idx(round_token_counts)
		max_valid_idx = len(round_token_counts) - 1
		if -1 < last_truthy_elem_idx < max_valid_idx:
			old_len = len(round_token_counts)
			round_token_counts = round_token_counts[:last_truthy_elem_idx + 1]
			print("Trimmed {} empty round(s) from session \"{}\".".format(old_len - len(round_token_counts), dyad_id),
				  file=sys.stderr)

		session_token_type_datum = SessionTokenTypeDatum(round_token_counts)
		yield dyad_id, session_token_type_datum


def __create_argparser():
	result = argparse.ArgumentParser(description="Count token/type counts per round in each game session.")
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
	__print_token_type_data(token_type_data, outfile)


def __print_token_type_data(token_type_data: Iterable[Tuple[str, SessionTokenTypeDatum]], outfile):
	print(COL_DELIM.join(("DYAD", "ROUND", "ROUND_TOKENS", "ROUND_TYPES", "TOTAL_TOKENS", "TOTAL_TYPES")), file=outfile)

	ordered_session_token_type_data = sorted(token_type_data, key=lambda item: item[0])
	for dyad_id, session_token_type_datum in ordered_session_token_type_data:
		for round_id, round_token_type_datum in enumerate(session_token_type_datum.round_token_type_data, start=1):
			round_data = round_token_type_datum.round_data
			cumulative_data = round_token_type_datum.cumulative_data
			row = (dyad_id, str(round_id), str(sum(round_data.token_counts.values())), str(len(round_data.token_types)),
				   str(sum(cumulative_data.token_counts.values())), str(len(cumulative_data.token_types)))
			print(COL_DELIM.join(row), file=outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
