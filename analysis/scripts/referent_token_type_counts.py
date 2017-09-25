#!/usr/bin/env python3

import argparse
import re
import sys
from collections import defaultdict
from decimal import Decimal
from typing import Any, Callable, Dict, Iterable, Iterator, List, Mapping, MutableMapping, Sequence, Tuple, TypeVar

import game_events
import re_token_type_counts
import utterances
from session_data import SessionData, walk_session_data
from token_groups import read_token_group_dict

COL_DELIM = '\t'

T = TypeVar('T')


class CoreferenceChainTokenCounter(object):
	def __init__(self, token_seq_factory: Callable[[Iterable[str]], Sequence[str]],
				 filtering_token_counter: Callable[
					 [Sequence[utterances.Utterance]], re_token_type_counts.FilteredTokenCountDatum]):
		self.token_seq_factory = token_seq_factory
		self.filtering_token_counter = filtering_token_counter

	def __call__(self, named_sessions: Mapping[T, SessionData]) -> Dict[T, Dict[int, "CoreferenceChainTokenCountDatum"]]:
		result = {}
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
			referent_token_counts = dict(
				(entity_id, CoreferenceChainTokenCountDatum(referent_counts)) for (entity_id, referent_counts) in
				self.__referent_token_counts(session).items())
			trim_empty_tail_rounds(dyad_id, referent_token_counts)
			result[dyad_id] = referent_token_counts

		return result

	def __referent_token_counts(self, session: SessionData) -> Dict[
		int, List[Tuple[int, re_token_type_counts.FilteredTokenCountDatum]]]:
		"""

		:param session: The SessionData instance representing the game session to process.
		:return: A dictionary mapping entity IDs to a respective list of pairs of round IDs and their corresponding token count instances.
		:rtype: Dict[int, List[Tuple[int, re_token_type_counts.FilteredTokenTypeDatum]]]
		"""
		result = defaultdict(list)
		event_data = game_events.read_events(session)
		source_participant_ids = event_data.source_participant_ids
		game_rounds = game_events.create_game_rounds(event_data.events)
		segments = utterances.read_segments(session.utts)
		seg_utt_factory = utterances.SegmentUtteranceFactory(self.token_seq_factory,
															 lambda source_id: source_participant_ids[source_id])
		utt_times = utterances.UtteranceTimes(seg_utt_factory(segments))
		game_round_utts = zip_game_round_utterances(game_rounds, utt_times)
		for (round_id, (game_round, utts)) in enumerate(game_round_utts, start=1):
			initial_event = game_round.initial_event
			utts_tuple = tuple(utts)
			round_referent_token_counts = self.filtering_token_counter(utts_tuple)
			for entity_id, _ in initial_event.referent_entities:
				referent_token_counts = result[entity_id]
				referent_token_counts.append((round_id, round_referent_token_counts))

		return result


class CoreferenceChainTokenCountDatum(object):
	"""
	This class represents token counts for a single referent entity throughout an entire game.
	"""

	@staticmethod
	def __add_round(round_datum: re_token_type_counts.FilteredTokenCountDatum,
					total_token_counts: re_token_type_counts.FilteredTokenCountDatum) -> re_token_type_counts.RoundTokenCountDatum:
		total_token_counts.update(round_datum)
		cumulative_data = total_token_counts.copy()
		return re_token_type_counts.RoundTokenCountDatum(round_datum, cumulative_data)

	def __init__(self, round_token_counts: Iterable[Tuple[Any, re_token_type_counts.FilteredTokenCountDatum]]):
		"""
		:param round_token_counts: An iterable object returning pairs of round IDs and their corresponding :class:`re_token_type_counts.FilteredTokenTypeDatum` instances representing token counts for each round in the game.
		:type round_token_counts: Iterable[Tuple[Any, re_token_type_counts.FilteredTokenTypeDatum]]
		"""
		self.total_data = re_token_type_counts.FilteredTokenCountDatum()
		"""The cumulative token counts for the entire game for the referent entity this object represents."""
		self.round_data = tuple(
			(round_id, self.__add_round(token_counts, self.total_data)) for round_id, token_counts in
			round_token_counts)
		"""A tuple of pairs of round IDs with their corresponding :class:`re_token_type_counts.RoundTokenTypeDatum` instances, each of which representing token counts for 
		a given round."""

	@property
	def __key(self):
		return self.total_data, self.round_data

	def __eq__(self, other):
		return (self is other or (isinstance(other, type(self))
								  and self.__key == other.__key))

	def __getitem__(self, round_id: Any):
		return next(
			game_round for (game_round_id, game_round) in self.round_counts_by_round_id() if game_round_id == round_id)

	def __ne__(self, other):
		return not (self == other)

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	@property
	def round_count(self):
		return len(self.round_data)

	def round_counts_by_round_id(self):
		return self.round_data


class TokenTypeDataPrinter(object):
	@staticmethod
	def __ratio(tokens: int, types: int) -> Decimal:
		return Decimal(types) / Decimal(tokens)

	def __init__(self, strict: bool):
		self.strict = strict

	def __call__(self, session_referent_token_counts: Iterable[Tuple[Any, Mapping[int, CoreferenceChainTokenCountDatum]]],
				 outfile):
		print(COL_DELIM.join(
			("DYAD", "ENTITY", "SEQUENCE_ORDER", "ROUND", "ROUND_TOKENS", "ROUND_TYPES", "ROUND_TYPE_RATIO",
			 "TOTAL_TOKENS",
			 "TOTAL_TYPES", "TOTAL_TYPE_RATIO")), file=outfile)

		ordered_session_referent_token_counts = sorted(session_referent_token_counts, key=lambda item: item[0])
		for dyad_id, referent_token_counts in ordered_session_referent_token_counts:
			for entity_id, entity_token_counts in sorted(referent_token_counts.items(), key=lambda item: item[0]):
				for (sequence_order, (round_id, round_token_counts)) in enumerate(
						entity_token_counts.round_counts_by_round_id(), start=1):
					round_data = round_token_counts.round_data.relevant_tokens
					round_token_count = sum(round_data.token_counts.values())
					round_type_count = len(round_data.token_types)
					cumulative_data = round_token_counts.cumulative_data.relevant_tokens
					cumulative_token_count = sum(cumulative_data.token_counts.values())
					cumulative_type_count = len(cumulative_data.token_types)
					try:
						round_ratio = self.__ratio(round_token_count, round_type_count)
						cumulative_ratio = self.__ratio(cumulative_token_count, cumulative_type_count)
					except ZeroDivisionError as e:
						if self.strict:
							raise ValueError(
								"Round {} of session \"{}\" did not have any relevant tokens!".format(round_id,
																									  dyad_id)) from e
						else:
							print(
								"WARNING: Round {} of session \"{}\" did not have any relevant tokens!".format(round_id,
																											   dyad_id),
								file=sys.stderr)
							round_ratio = Decimal('NaN')
							cumulative_ratio = round_ratio

					row = (dyad_id, str(entity_id), str(sequence_order), str(round_id), str(round_token_count),
						   str(round_type_count),
						   str(round_ratio),
						   str(cumulative_token_count), str(cumulative_type_count), str(cumulative_ratio))
					print(COL_DELIM.join(row), file=outfile)


def find_last_round_id(referent_token_counts: Mapping[T, CoreferenceChainTokenCountDatum]) -> Tuple[T, int]:
	return max(
		((entity_id, round_id) for (entity_id, counts) in referent_token_counts.items() for (round_id, round_counts) in
		 counts.round_counts_by_round_id()), key=lambda result: result[1])


def trim_empty_tail_rounds(dyad_id: Any, referent_token_counts: MutableMapping[int, CoreferenceChainTokenCountDatum]):
	is_last_round_relevant = False
	old_round_count = find_last_round_id(referent_token_counts)[1]
	while not is_last_round_relevant:
		last_referent_id, last_round_id = find_last_round_id(referent_token_counts)
		last_referent_counts = referent_token_counts[last_referent_id]
		last_round = last_referent_counts[last_round_id]
		if re_token_type_counts.is_relevant_round(last_round.round_data):
			is_last_round_relevant = True
		else:
			trimmed_round_counts = tuple((round_id, round_counts.round_data) for (round_id, round_counts) in
										 last_referent_counts.round_counts_by_round_id() if round_id != last_round_id)
			trimmed_referent_token_counts = CoreferenceChainTokenCountDatum(trimmed_round_counts)
			referent_token_counts[last_referent_id] = trimmed_referent_token_counts

	new_round_count = find_last_round_id(referent_token_counts)[1]
	if old_round_count > new_round_count:
		print("Trimmed {} empty round(s) from session \"{}\".".format(old_round_count - new_round_count, dyad_id),
			  file=sys.stderr)


def zip_game_round_utterances(game_rounds: Iterator[game_events.GameRound], utt_times: utterances.UtteranceTimes) -> \
		Iterator[Tuple[game_events.GameRound, Iterator[utterances.Utterance]]]:
	current_round = next(game_rounds)
	for next_round in game_rounds:
		current_round_start_time = current_round.start_time
		next_round_start_time = next_round.start_time
		current_round_utts = utt_times.get(current_round_start_time, next_round_start_time)
		yield current_round, current_round_utts
		current_round = next_round

	current_round_start_time = current_round.start_time
	current_round_utts = utt_times.get(current_round_start_time)
	yield current_round, current_round_utts


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(description="Count referent tokens and token types per round in each game session as well as the entity ID of the round's referent.")
	result.add_argument("token_group_file", metavar="TOKEN_GROUP_FILEPATH",
						help="The path to the token group mapping file to use.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The paths to search for sessions to process.")
	result.add_argument("-g", "--group", metavar="REGEX",
						help="A regular expression matching the token group(s) to calculate counts for.")
	result.add_argument("-s", "--strict", action="store_true",
						help="When this option is supplied, the script will throw an exception upon encountering a round with no relevant tokens.")
	return result


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
	referent_token_counter = CoreferenceChainTokenCounter(utterances.TokenSequenceFactory(),
														  re_token_type_counts.FilteringTokenCounter(
															  lambda token: token in token_groups.keys()))
	referent_token_counts = referent_token_counter(named_sessions)
	printer = TokenTypeDataPrinter(args.strict)
	printer(referent_token_counts.items(), outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
