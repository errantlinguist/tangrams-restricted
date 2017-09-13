#!/usr/bin/env python3

import argparse
import re
import statistics
import sys
from collections import defaultdict
from decimal import Decimal
from typing import Any, Callable, Dict, ItemsView, Iterable, Tuple, TypeVar

import game_events
import re_token_type_counts
import referent_token_type_counts
import session_data as sd
import token_groups as tg
import utterances

COL_DELIM = '\t'
NULL_VALUE_REPR = '?'

T = TypeVar('T')

_DECIMAL_INFINITY = Decimal("Infinity")


class RoundReferentCounts(object):
	def __init__(self, game_round: game_events.GameRound,
				 participant_counts: Dict[Any, re_token_type_counts.FilteredTokenCountDatum]):
		self.game_round = game_round
		self.participant_counts = participant_counts
		"""Token counts for utterances produced by a given participant in the round represented by this instance."""
		self.total_counts = re_token_type_counts.FilteredTokenCountDatum()
		for counts in participant_counts.values():
			self.total_counts.update(counts)
		"""Token counts for all utterances produced by all participants in the round represented by this instance."""

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)


class ReferentCounts(object):
	def __init__(self):
		self.round_counts = []
		"""Counts for each round in which the entity is referenced for utterances by a particular participant"""
		self.participant_total_counts = defaultdict(re_token_type_counts.FilteredTokenCountDatum)
		"""participant_id -> total counts for entity for the entire coreference chain for utterances by a particular participant"""
		self.total_counts = re_token_type_counts.FilteredTokenCountDatum()
		"""Total counts for entity for the entire coreference chain, for all utterances by all speakers"""

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def add_round_counts(self, round_id: Any, round_counts: RoundReferentCounts):
		self.round_counts.append((round_id, round_counts))
		for participant_id, participant_counts in round_counts.participant_counts.items():
			self.participant_total_counts[participant_id].update(participant_counts)
		self.total_counts.update(round_counts.total_counts)

	@property
	def coreference_chain_length(self):
		return len(self.round_counts)


class ParticipantCoreferenceChainTokenCounter(object):
	def __init__(self, seg_utt_factory: utterances.SegmentUtteranceFactory,
				 filtering_token_counter: Callable[
					 [Iterable[utterances.Utterance]], re_token_type_counts.FilteredTokenCountDatum]):
		self.seg_utt_factory = seg_utt_factory
		self.filtering_token_counter = filtering_token_counter

	def __call__(self, named_sessions: Iterable[Tuple[str, sd.SessionData]]) -> Dict[str, Dict[int, ReferentCounts]]:
		result = {}
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)

			events, source_participant_ids = game_events.read_events(session)
			game_rounds = iter(game_events.create_game_rounds(events))
			segments = utterances.read_segments(session.utts)
			utt_times = utterances.UtteranceTimes(self.seg_utt_factory(segments))
			game_round_utts = referent_token_type_counts.zip_game_round_utterances(game_rounds, utt_times)

			entity_referent_counts = defaultdict(ReferentCounts)
			for (round_id, (game_round, utts)) in enumerate(game_round_utts, start=1):
				initial_event = game_round.initial_event
				speaker_utts = utterances.create_speaker_dict(utts)
				participant_token_counts = {}
				for speaker_id, speaker_utts in speaker_utts.items():
					speaker_participant_id = source_participant_ids[speaker_id]
					participant_token_counts[speaker_participant_id] = self.filtering_token_counter(speaker_utts)

				round_counts = RoundReferentCounts(game_round, participant_token_counts)
				for entity_id, _ in initial_event.referent_entities:
					entity_counts = entity_referent_counts[entity_id]
					entity_counts.add_round_counts(round_id, round_counts)

			result[dyad_id] = entity_referent_counts

		return result


class TokenTypeDataPrinter(object):
	@staticmethod
	def __create_first_row(dyad_id, entity_id, sequence_order, round_id, current_round_total_tokens,
						   current_round_token_types, game_round: game_events.GameRound):
		mean_previous_token_count = NULL_VALUE_REPR
		token_type_overlap = 0
		overlap_ratio = NULL_VALUE_REPR
		current_round_length_drop = NULL_VALUE_REPR
		initial_event = game_round.initial_event
		return (dyad_id, str(entity_id), str(sequence_order), str(round_id), str(current_round_total_tokens),
				str(current_round_total_tokens), mean_previous_token_count,
				current_round_length_drop, str(len(current_round_token_types)), str(len(current_round_token_types)),
				str(token_type_overlap), overlap_ratio, str(initial_event.score), str(initial_event.event_time))

	def __init__(self, strict: bool):
		self.strict = strict

	def __call__(self, session_referent_token_counts: ItemsView[str, Dict[int, ReferentCounts]], outfile):
		print(COL_DELIM.join(
			("DYAD", "ENTITY", "SEQUENCE_ORDER", "ROUND", "ROUND_TOKENS", "CUMULATIVE_TOKENS", "PREVIOUS_MEAN_TOKENS",
			 "LENGTH_DROP",
			 "ROUND_TYPES", "CUMULATIVE_TYPES", "OVERLAPPING_TYPES",
			 "OVERLAPPING_TYPE_RATIO", "GAME_SCORE" "ROUND_START_TIME")), file=outfile)

		ordered_session_referent_token_counts = sorted(session_referent_token_counts, key=lambda item: item[0])
		for dyad_id, referent_token_counts in ordered_session_referent_token_counts:
			for entity_id, entity_token_counts in sorted(referent_token_counts.items(), key=lambda item: item[0]):
				# Counts for each round, ordered by their respective round IDs
				ordered_round_counts = sorted(entity_token_counts.round_counts, key=lambda item: item[0])
				# Round IDs and their corresponding counts, enumerated by their coreference chain sequence number
				enumerated_ordered_round_counts = enumerate(ordered_round_counts, start=1)
				sequence_order, (round_id, round_token_counts) = next(enumerated_ordered_round_counts)
				current_round_token_counts = round_token_counts.total_counts.relevant_tokens
				current_round_total_tokens = Decimal(current_round_token_counts.total_token_count())
				current_round_token_types = frozenset(current_round_token_counts.token_types)
				print(COL_DELIM.join(
					self.__create_first_row(dyad_id, entity_id, sequence_order, round_id, current_round_total_tokens,
											current_round_token_types, round_token_counts.game_round)),
					file=outfile)
				previous_round_token_types = current_round_token_types
				previous_round_total_tokens = [current_round_total_tokens]

				for (sequence_order, (round_id, round_token_counts)) in enumerated_ordered_round_counts:
					current_round_token_counts = round_token_counts.total_counts.relevant_tokens
					current_round_total_tokens = Decimal(current_round_token_counts.total_token_count())
					current_round_token_types = frozenset(current_round_token_counts.token_types)
					row = self.__create_row(dyad_id, entity_id, sequence_order, round_id, current_round_total_tokens,
											current_round_token_types, previous_round_token_types,
											previous_round_total_tokens, round_token_counts.game_round)
					print(COL_DELIM.join(row), file=outfile)

					previous_round_token_types = current_round_token_types
					previous_round_total_tokens.append(current_round_total_tokens)

	def __create_row(self, dyad_id, entity_id, sequence_order, round_id, current_round_total_tokens,
					 current_round_token_types, previous_round_token_types,
					 previous_round_total_tokens: Iterable[Decimal],
					 game_round: game_events.GameRound):
		unified_token_type_count = len(current_round_token_types.union(
			previous_round_token_types))
		overlapping_token_type_count = len(current_round_token_types.intersection(
			previous_round_token_types))
		cumulative_token_count = sum(previous_round_total_tokens) + current_round_total_tokens
		mean_previous_token_count = statistics.mean(previous_round_total_tokens)
		try:
			overlap_ratio = token_type_overlap_ratio(overlapping_token_type_count, unified_token_type_count)
			current_round_length_drop = length_drop(current_round_total_tokens, mean_previous_token_count)
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
				overlap_ratio = _DECIMAL_INFINITY
				current_round_length_drop = overlap_ratio

		initial_event = game_round.initial_event
		return (dyad_id, str(entity_id), str(sequence_order), str(round_id), str(current_round_total_tokens),
				str(cumulative_token_count), str(mean_previous_token_count),
				str(current_round_length_drop), str(len(current_round_token_types)), str(unified_token_type_count),
				str(overlapping_token_type_count),
				str(overlap_ratio), str(initial_event.score), str(initial_event.event_time))


def length_drop(current_total_tokens: Decimal, mean_previous_token_count: Decimal) -> Decimal:
	"""
	See Aina, L. et al (2017) "Referring Expressions and Communicative Success in Task-oriented Dialogues", p. 9.

	:param current_total_tokens: The total tokens uttered in the current unit under examination (e.g. either a single utterance or an entire sub-dialogue).
	:type current_total_tokens: Decimal
	:param mean_previous_token_count:  The mean tokens uttered of the previous units under examination (e.g. either a single utterance or an entire sub-dialogue).
	:type mean_previous_token_count: Decimal
	:return: A metric between -1 and 1.
	:rtype Decimal
	"""
	return (mean_previous_token_count - current_total_tokens) / (mean_previous_token_count + current_total_tokens)


def token_type_overlap_ratio(overlapping_token_type_count: int, unified_token_type_count: int) -> Decimal:
	return Decimal(overlapping_token_type_count) / Decimal(unified_token_type_count)


def __create_argparser():
	result = argparse.ArgumentParser(description="Count referent token/type counts per round in each game session.")
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
		token_groups = tg.read_token_group_dict(token_group_file_path,
												lambda group: group_pattern.match(group) is not None)
	else:
		token_groups = tg.read_token_group_dict(token_group_file_path)
	print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

	named_sessions = sd.walk_session_data(args.inpaths)
	outfile = sys.stdout
	referent_token_counter = ParticipantCoreferenceChainTokenCounter(
		utterances.SegmentUtteranceFactory(),
		re_token_type_counts.FilteringTokenCounter(
			lambda
				token: token in token_groups.keys()))
	session_entity_counts = referent_token_counter(named_sessions)
	printer = TokenTypeDataPrinter(args.strict)
	printer(session_entity_counts.items(), outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())