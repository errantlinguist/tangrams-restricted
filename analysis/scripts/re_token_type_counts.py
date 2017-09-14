#!/usr/bin/env python3

import argparse
import re
import sys
from collections import Counter
from decimal import Decimal
from typing import Any, Callable, Dict, Iterable, Iterator, Sequence, Container, Tuple, TypeVar

import utterances
from session_data import SessionData
from session_data import walk_session_data
from token_groups import read_token_group_dict

COL_DELIM = '\t'

T = TypeVar('T')


class FilteredTokenCountDatum(object):
	def __init__(self, all_tokens: "TokenCountDatum" = None, relevant_tokens: "TokenCountDatum" = None):
		self.all_tokens = TokenCountDatum() if all_tokens is None else all_tokens
		self.relevant_tokens = TokenCountDatum() if relevant_tokens is None else relevant_tokens
		self.utt_seqs = []

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def add(self, utts: Sequence[utterances.Utterance], token_filter: Callable[[str], bool] = lambda _: True):
		for utt in utts:
			self.all_token_counts.update(utt.content)
			self.relevant_token_counts.update(token for token in utt.content if token_filter(token))
		self.utt_seqs.append(utts)

	def copy(self):
		return FilteredTokenCountDatum(self.all_tokens.copy(), self.relevant_tokens.copy())

	def update(self, other: "FilteredTokenCountDatum"):
		self.all_tokens.update(other.all_tokens)
		self.relevant_tokens.update(other.relevant_tokens)


class FilteringTokenCounter(object):
	def __init__(self, token_filter: Callable[[str], bool] = lambda _: True):
		self.token_filter = token_filter

	def __call__(self, utts: Sequence[utterances.Utterance]) -> FilteredTokenCountDatum:
		result = FilteredTokenCountDatum()
		result.add(utts)
		return result


class RoundTokenCountDatum(object):
	"""
	This class represents token counts for a single round in a game.
	"""

	def __init__(self, round_data: FilteredTokenCountDatum, cumulative_data: FilteredTokenCountDatum):
		"""

		:param round_data: Token counts for the individual round this object represents.
		:type round_data: FilteredTokenCountDatum
		:param cumulative_data: Cumulative counts for the entire game up to and including the individual round this object represents.
		:type cumulative_data: FilteredTokenCountDatum
		"""
		self.round_data = round_data
		"""Token counts for the individual round this object represents."""
		self.cumulative_data = cumulative_data
		"""Cumulative counts for the entire game up to and including the individual round this object represents."""

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)


class SessionTokenCountDatum(object):
	@staticmethod
	def __add_round(round_datum: FilteredTokenCountDatum,
					total_token_counts: FilteredTokenCountDatum) -> RoundTokenCountDatum:
		total_token_counts.update(round_datum)
		cumulative_data = total_token_counts.copy()
		return RoundTokenCountDatum(round_datum, cumulative_data)

	@staticmethod
	def __round_id_to_idx(round_id: int):
		return round_id - 1

	def __init__(self, round_token_counts: Iterable[FilteredTokenCountDatum]):
		self.total_data = FilteredTokenCountDatum()
		self.round_data = tuple(
			self.__add_round(token_counts, self.total_data) for token_counts in round_token_counts)

	def __getitem__(self, round_id: int):
		return self.round_data[self.__round_id_to_idx(round_id)]

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	@property
	def round_count(self):
		return len(self.round_data)

	def round_counts_by_round_id(self):
		return enumerate(self.round_data, start=1)


class SessionRoundTokenCounter(object):
	def __init__(self, seg_utt_factory: utterances.SegmentUtteranceFactory,
				 filtering_token_counter: Callable[[Sequence[utterances.Utterance]], FilteredTokenCountDatum]):
		self.seg_utt_factory = seg_utt_factory
		self.filtering_token_counter = filtering_token_counter

	def __call__(self, named_sessions: Dict[T, SessionData]) -> Dict[T, Tuple[FilteredTokenCountDatum, ...]]:
		result = {}
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
			round_token_counts = tuple(self.__session_token_type_counts(session))
			result[dyad_id] = round_token_counts

		return result

	def __session_token_type_counts(self, session: SessionData) -> Iterator[FilteredTokenCountDatum]:
		round_start_end_times = tuple(session.read_round_start_end_times())
		round_count = len(round_start_end_times)
		print("Read {} game round(s).".format(round_count), file=sys.stderr)

		segments = utterances.read_segments(session.utts)
		utt_times = utterances.UtteranceTimes(self.seg_utt_factory(segments))
		round_utts = (tuple(utt_times.get(start_time, end_time)) for start_time, end_time in
					  round_start_end_times)
		# round_utts = (game_round_utterances(start_time, end_time, utts) for start_time, end_time in
		#			  round_start_end_times)
		return (self.filtering_token_counter(utts) for utts in round_utts)


class TokenTypeDataPrinter(object):
	@staticmethod
	def __ratio(tokens: int, types: int) -> Decimal:
		return Decimal(types) / Decimal(tokens)

	def __init__(self, strict: bool):
		self.strict = strict

	def __call__(self, token_type_data: Iterable[Tuple[Any, SessionTokenCountDatum]], outfile):
		print(COL_DELIM.join(("DYAD", "ROUND", "ROUND_TOKENS", "ROUND_TYPES", "ROUND_TYPE_RATIO", "TOTAL_TOKENS",
							  "TOTAL_TYPES", "TOTAL_TYPE_RATIO")), file=outfile)

		ordered_session_token_type_data = sorted(token_type_data, key=lambda item: item[0])
		for dyad_id, session_token_type_datum in ordered_session_token_type_data:
			for round_id, round_token_type_datum in session_token_type_datum.round_counts_by_round_id():
				round_data = round_token_type_datum.round_data.relevant_tokens
				round_token_count = round_data.total_token_count()
				round_type_count = len(round_data.token_types)
				cumulative_data = round_token_type_datum.cumulative_data.relevant_tokens
				cumulative_token_count = cumulative_data.total_token_count()
				cumulative_type_count = len(cumulative_data.token_types)
				try:
					round_ratio = self.__ratio(round_token_count, round_type_count)
					cumulative_ratio = self.__ratio(cumulative_token_count, cumulative_type_count)
				except ZeroDivisionError as e:
					if self.strict:
						raise ValueError("Round {} of session \"{}\" did not have any relevant tokens!".format(round_id,
																											   dyad_id)) from e
					else:
						print("WARNING: Round {} of session \"{}\" did not have any relevant tokens!".format(round_id,
																											 dyad_id),
							  file=sys.stderr)
						round_ratio = Decimal('NaN')
						cumulative_ratio = round_ratio

				row = (dyad_id, str(round_id), str(round_token_count), str(round_type_count), str(round_ratio),
					   str(cumulative_token_count), str(cumulative_type_count), str(cumulative_ratio))
				print(COL_DELIM.join(row), file=outfile)


class TokenCountDatum(object):
	def __init__(self, token_counts: Dict[str, int] = None):
		self.token_counts = Counter() if token_counts is None else token_counts

	@property
	def token_types(self):
		return self.token_counts.keys()

	def total_token_count(self) -> int:
		return sum(self.token_counts.values())

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def copy(self) -> "TokenCountDatum":
		return TokenCountDatum(Counter(self.token_counts))

	def update(self, other: "TokenCountDatum"):
		self.token_counts.update(other.token_counts)


def find_last_matching_elem_idx(elems: Sequence[T], predicate: Callable[[T], bool]):
	reversed_elems = reversed(elems)
	max_valid_idx = len(elems) - 1
	return next((max_valid_idx - i for i, elem in enumerate(reversed_elems) if predicate(elem)), -1)


def is_relevant_round(datum: FilteredTokenCountDatum):
	return len(datum.relevant_tokens.token_types) > 0


def session_token_type_data(named_sessions, relevant_tokens: Container[str]) -> Iterator[
	Tuple[str, SessionTokenCountDatum]]:
	seg_utt_factory = utterances.SegmentUtteranceFactory()
	session_token_counter = SessionRoundTokenCounter(seg_utt_factory,
													 FilteringTokenCounter(lambda token: token in relevant_tokens))
	session_round_token_counts = session_token_counter(named_sessions)
	for dyad_id, round_token_counts in session_round_token_counts.items():
		trimmed_round_token_counts = trim_empty_tail_rounds(dyad_id, round_token_counts)
		session_token_type_datum = SessionTokenCountDatum(trimmed_round_token_counts)
		yield dyad_id, session_token_type_datum


def trim_empty_tail_rounds(dyad_id: Any, round_token_counts: Sequence[FilteredTokenCountDatum]):
	last_relevant_elem_idx = find_last_matching_elem_idx(round_token_counts, is_relevant_round)
	max_valid_idx = len(round_token_counts) - 1
	if -1 < last_relevant_elem_idx < max_valid_idx:
		old_len = len(round_token_counts)
		result = round_token_counts[:last_relevant_elem_idx + 1]
		print("Trimmed {} empty round(s) from session \"{}\".".format(old_len - len(result), dyad_id),
			  file=sys.stderr)
	else:
		result = round_token_counts
	return result


def __create_argparser():
	result = argparse.ArgumentParser(description="Count token/type counts per round in each game session.")
	result.add_argument("token_group_file", metavar="TOKEN_GROUP_FILEPATH",
						help="The path to the token group mapping file to use.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The paths to search for sessions to process.")
	result.add_argument("-g", "--group", metavar="REGEX",
						help="A regular expression matching the token group(s) to calculate counts for.")
	result.add_argument("-s", "--strict", action="store_true",
						help="When this option is supplied, the script will throw an exception upon encountering a round with no relevant tokens.")
	return result


def __create_rounded_decimal_repr(value: Decimal):
	return str(value)


def __main(args):
	token_group_file_path = args.token_group_file
	print("Reading token groups from \"{}\".".format(token_group_file_path), file=sys.stderr)
	group_regex = args.group
	if group_regex:
		group_pattern = re.compile(group_regex)
		print("Calculating counts for token groups matching pattern \"{}\".".format(group_pattern), file=sys.stderr)
		token_groups = read_token_group_dict(token_group_file_path,
											 lambda group: group_pattern.match(group) is not None)
	else:
		token_groups = read_token_group_dict(token_group_file_path)
	print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

	named_sessions = walk_session_data(args.inpaths)
	outfile = sys.stdout
	token_type_data = session_token_type_data(named_sessions, token_groups.keys())
	printer = TokenTypeDataPrinter(args.strict)
	printer(token_type_data, outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
