#!/usr/bin/env python3

import argparse
import sys
from collections import Counter
from decimal import Decimal
from typing import Dict, Iterable, Iterator, Sequence, Container, Tuple, TypeVar
from enum import Enum, auto, unique

import utterances
from re_token_group_counts import read_token_group_dict
from re_token_group_freqs import game_round_start_end_times, read_round_start_times, \
	game_round_utterances
from session_data import walk_session_data
import copy

COL_DELIM = '\t'
NULL_VALUE_REPR = '?'

T = TypeVar('T')

class TokenTypeDatum(object):
	def __init__(self, token_counts: Dict[str, int] = None):
		self.token_counts = Counter() if token_counts is None else token_counts

	@property
	def token_types(self):
		return self.token_counts.keys()

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def copy(self):
		return TokenTypeDatum(Counter(self.token_counts))

	def update(self, other):
		self.token_counts.update(other.token_types)


class RoundTokenTypeDatum(object):
	def __init__(self, round_data: TokenTypeDatum, cumulative_data: TokenTypeDatum):
		self.round_data = round_data
		self.cumulative_data = cumulative_data

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

@unique
class TokenMetatype(Enum):
	ALL = auto()
	RELEVANT = auto()


class SessionTokenTypeDatum(object):
	@staticmethod
	def __add_round(round_datum: Dict[TokenMetatype, TokenTypeDatum], total_token_counts: Dict[TokenMetatype, TokenTypeDatum]) -> RoundTokenTypeDatum:
		for metadata_type, data in round_datum.items():
			total_token_counts[metadata_type].update(data)
		cumulative_data = copy.deepcopy(total_token_counts)
		result = RoundTokenTypeDatum(round_datum, cumulative_data)
		return result

	def __init__(self, round_token_counts: Iterable[Dict[TokenMetatype, TokenTypeDatum]]):
		self.total_data = dict((metadata_type, TokenTypeDatum()) for metadata_type in TokenMetatype)
		self.round_data = tuple(
			self.__add_round(token_counts, self.total_data) for token_counts in round_token_counts)

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	@property
	def round_count(self):
		return len(self.round_data)


class SessionRoundTokenCounter(object):

	def __init__(self, seg_utt_factory: utterances.SegmentUtteranceFactory, token_filter=lambda _ : True):
		self.seg_utt_factory = seg_utt_factory
		self.token_filter = token_filter

	def __call__(self, named_sessions):
		result = {}
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
			round_token_counts = tuple(self.__session_token_type_counts(session))
			result[dyad_id] = round_token_counts

		return result

	def __count_utt_tokens(self, utts: Iterable[utterances.Utterance]) -> Dict[TokenMetatype, TokenTypeDatum]:
		all_token_counts = Counter()
		relevant_token_counts = Counter()
		for utt in utts:
			all_token_counts.update(utt.content)
			relevant_token_counts.update(token for token in utt.content if self.token_filter(token))
		return {TokenMetatype.ALL : TokenTypeDatum(all_token_counts), TokenMetatype.RELEVANT : TokenTypeDatum(relevant_token_counts)}


	def __session_token_type_counts(self, session) -> Iterator[Dict[TokenMetatype, TokenTypeDatum]]:
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
	seg_utt_factory = utterances.SegmentUtteranceFactory()
	token_counter = SessionRoundTokenCounter(seg_utt_factory, lambda token: token in relevant_tokens)
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
	print(COL_DELIM.join(("DYAD", "ROUND", "ROUND_TOKENS", "ROUND_TYPES", "ROUND_TYPE_RATIO", "TOTAL_TOKENS", "TOTAL_TYPES", "TOTAL_TYPE_RATIO")), file=outfile)

	ordered_session_token_type_data = sorted(token_type_data, key=lambda item: item[0])
	for dyad_id, session_token_type_datum in ordered_session_token_type_data:
		for round_id, round_token_type_datum in enumerate(session_token_type_datum.round_data, start=1):
			round_data = round_token_type_datum.round_data[TokenMetatype.RELEVANT]
			round_token_count = sum(round_data.token_counts.values())
			round_type_count = len(round_data.token_types)
			round_ratio = __ratio(round_type_count, round_token_count)
			cumulative_data = round_token_type_datum.cumulative_data[TokenMetatype.RELEVANT]
			cumulative_token_count = sum(cumulative_data.token_counts.values())
			cumulative_type_count = len(cumulative_data.token_types)
			cumulative_ratio = __ratio(cumulative_type_count, cumulative_token_count)
			row = (dyad_id, str(round_id), str(round_token_count), str(round_type_count), str(round_ratio),
				   str(cumulative_token_count), str(cumulative_type_count), str(cumulative_ratio))
			print(COL_DELIM.join(row), file=outfile)


def __ratio(tokens, types):
	return tokens / types

if __name__ == "__main__":
	__main(__create_argparser().parse_args())
