#!/usr/bin/env python3

import argparse
import itertools
import re
import statistics
import sys
from collections import defaultdict, namedtuple
from decimal import Decimal, InvalidOperation
from typing import Any, Callable, Dict, ItemsView, Iterable, List, Sequence, Set, Tuple, TypeVar

import game_events
import re_token_type_counts
import referent_token_type_counts
import session_data as sd
import token_groups as tg
import utterances

COL_DELIM = '\t'
COMBINED_PARTICIPANT_IDENTIFIER = "BOTH"

T = TypeVar('T')

_DECIMAL_INFINITY = Decimal("Infinity")
# _DECIMAL_NAN = Decimal("NaN")
_DECIMAL_ONE = Decimal("1")
_DECIMAL_ZERO = Decimal("0")

_EMPTY_SET = frozenset()

NULL_PREVIOUS_MEAN_COUNT_VALUE = None
NULL_TOKEN_TYPE_OVERLAP_VALUE = _DECIMAL_ZERO
NULL_TOKEN_LENGTH_DROP_VALUE = _DECIMAL_INFINITY


class EventParticipantIdFactory(object):
	"""
		This is a hack to map the non-anonymized usernames in the event logs to anonymized utterance speaker IDs.
		TODO: Remove this after anonymizing all data
	"""

	def __init__(self, initial_instructor_id: str):
		self.initial_instructor_id = initial_instructor_id

	def __call__(self, event: game_events.Event):
		"""
		:param event: The event to get the participant ID for
		:return: Either "A" or "B"
		"""
		return "A" if event.submitter == self.initial_instructor_id else "B"


class GameRoundMetrics(object):
	COL_NAMES = (
		"DYAD", "ENTITY", "SHAPE", "SEQUENCE_ORDER", "ROUND", "INSTRUCTOR", "GAME_SCORE", "ROUND_START_TIME", "TIME_SCORE_RATIO",
		"SCORE_ROUND_RATIO")

	@staticmethod
	def __create_referent_entity_shape_set_repr(event: game_events.Event) -> str:
		shapes = frozenset(referent_entity.shape for _, referent_entity in event.referent_entities)
		return ','.join(sorted(shapes))

	def __init__(self, dyad_id: str, entity_id: int, sequence_order: int, round_id: int,
				 initial_event: game_events.Event, instructor: str):
		self.dyad_id = dyad_id
		self.entity_id = entity_id
		self.sequence_order = sequence_order
		self.round_id = round_id
		self.instructor = instructor
		self.referent_shape = self.__create_referent_entity_shape_set_repr(initial_event)
		self.score = initial_event.score
		self.time = initial_event.event_time
		self.time_score_ratio = time_score_ratio(initial_event)
		self.score_round_ratio = score_round_ratio(initial_event)

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def row_cells(self):
		return (self.dyad_id, self.entity_id, self.referent_shape, self.sequence_order, self.round_id, self.instructor, self.score,
				self.time,
				self.time_score_ratio, self.score_round_ratio)


class LanguageMetrics(object):
	COL_NAMES = ("ROUND_TOKENS", "CUMULATIVE_TOKENS", "PREVIOUS_MEAN_TOKENS",
				 "LENGTH_DROP",
				 "ROUND_TYPES", "CUMULATIVE_TYPES", "OVERLAPPING_TYPES",
				 "OVERLAPPING_TYPE_RATIO", "UTTERANCES")

	def __set_initial_metrics(self):
		self.cumulative_token_count = self.current_round_total_tokens
		self.mean_previous_token_count = NULL_PREVIOUS_MEAN_COUNT_VALUE
		self.current_round_length_drop = NULL_TOKEN_LENGTH_DROP_VALUE
		self.overlap_ratio = NULL_TOKEN_TYPE_OVERLAP_VALUE

	def __set_history_sensitive_metrics(self, dyad_id: Any, round_id: Any, desc: Any,
										previous_round_total_tokens: Iterable[Decimal], strict: bool):
		self.cumulative_token_count = sum(previous_round_total_tokens) + self.current_round_total_tokens
		self.mean_previous_token_count = statistics.mean(previous_round_total_tokens)
		self.current_round_length_drop = try_length_drop(dyad_id, round_id, desc, self.current_round_total_tokens,
														 self.mean_previous_token_count, strict)
		self.overlap_ratio = try_token_type_overlap_ratio(dyad_id, round_id, desc, self.overlapping_token_type_count,
														  self.unified_token_type_count, strict)

	def __init__(self, dyad_id: Any, round_id: Any, desc: Any, utts: Sequence[utterances.Utterance],
				 token_counts: re_token_type_counts.TokenCountDatum,
				 previous_round_total_tokens: Iterable[Decimal] = None,
				 previous_round_token_types: Set[str] = _EMPTY_SET, strict: bool = False):
		self.utts = utts
		self.current_round_total_tokens = Decimal(token_counts.total_token_count())
		self.current_round_token_types = frozenset(token_counts.token_types)
		self.unified_token_type_count = len(self.current_round_token_types.union(
			previous_round_token_types))
		self.overlapping_token_type_count = len(self.current_round_token_types.intersection(
			previous_round_token_types))
		if previous_round_total_tokens is None:
			self.__set_initial_metrics()
		else:
			self.__set_history_sensitive_metrics(dyad_id, round_id, desc, previous_round_total_tokens, strict)

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def row_cells(self):
		return (self.current_round_total_tokens, self.cumulative_token_count, self.mean_previous_token_count,
				self.current_round_length_drop, len(self.current_round_token_types), self.unified_token_type_count,
				self.overlapping_token_type_count, self.overlap_ratio, utterances.dialogue_utt_str_repr(self.utts))


class ParticipantCoreferenceChainTokenCounter(object):
	def __init__(self, token_seq_factory: Callable[[Iterable[str]], Sequence[str]], filtering_token_counter: Callable[
		[Sequence[utterances.Utterance]], re_token_type_counts.FilteredTokenCountDatum]):
		self.token_seq_factory = token_seq_factory
		self.filtering_token_counter = filtering_token_counter

	def __call__(self, named_sessions: Iterable[Tuple[str, sd.SessionData]]) -> Dict[
		str, Tuple[Dict[int, "ReferentCounts"], Dict[str, str]]]:
		result = {}
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)

			event_data = game_events.read_events(session)
			source_participant_ids = event_data.source_participant_ids
			seg_utt_factory = utterances.SegmentUtteranceFactory(self.token_seq_factory,
																 lambda source_id: source_participant_ids[source_id])
			game_rounds = iter(game_events.create_game_rounds(event_data.events))
			segments = utterances.read_segments(session.utts)
			utt_times = utterances.UtteranceTimes(seg_utt_factory(segments))
			game_round_utts = referent_token_type_counts.zip_game_round_utterances(game_rounds, utt_times)

			entity_referent_counts = {}
			enumerated_game_round_utts = enumerate(game_round_utts, start=1)
			game_round_utts = next(enumerated_game_round_utts)
			event_participant_id_factory = EventParticipantIdFactory(event_data.initial_instructor_id)
			self.__put_entity_counts(game_round_utts, source_participant_ids, entity_referent_counts,
									 event_participant_id_factory)
			for game_round_utts in enumerated_game_round_utts:
				self.__put_entity_counts(game_round_utts, source_participant_ids, entity_referent_counts,
										 event_participant_id_factory)
			result[dyad_id] = (entity_referent_counts, source_participant_ids)

		return result

	def __put_entity_counts(self, enumerated_game_round_utts, source_participant_ids: Dict[str, str],
							entity_referent_counts: Dict[int, "ReferentCounts"],
							event_participant_id_factory: Callable[[game_events.Event], str]):
		round_id, game_round_utts = enumerated_game_round_utts
		game_round, utts = game_round_utts
		initial_event = game_round.initial_event
		speaker_utts = utterances.create_speaker_dict(utts)
		participant_token_counts = {}
		for speaker_id in source_participant_ids.keys():
			speaker_participant_id = source_participant_ids[speaker_id]
			participant_utts = speaker_utts.get(speaker_participant_id, _EMPTY_SET)
			participant_token_counts[speaker_participant_id] = self.filtering_token_counter(participant_utts)

		round_instructor_id = event_participant_id_factory(initial_event)
		round_counts = RoundCounts(game_round, participant_token_counts, round_instructor_id)
		for entity_id, _ in initial_event.referent_entities:
			try:
				entity_counts = entity_referent_counts[entity_id]
			except KeyError:
				entity_counts = ReferentCounts()
				entity_referent_counts[entity_id] = entity_counts
			entity_counts.add_round_counts(round_id, round_counts)


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

	def add_round_counts(self, round_id: Any, round_counts: "RoundCounts"):
		self.round_counts.append((round_id, round_counts))
		for participant_id, participant_counts in round_counts.participant_counts.items():
			self.participant_total_counts[participant_id].update(participant_counts)
		self.total_counts.update(round_counts.total_counts)

	@property
	def coreference_chain_length(self):
		return len(self.round_counts)

	def participant_ids(self):
		return self.participant_total_counts.keys()


class RoundCounts(object):
	def __init__(self, game_round: game_events.GameRound,
				 participant_counts: Dict[Any, re_token_type_counts.FilteredTokenCountDatum], instructor: str):
		self.game_round = game_round
		self.participant_counts = participant_counts
		"""Token counts for utterances produced by a given participant in the round represented by this instance."""
		self.total_counts = re_token_type_counts.FilteredTokenCountDatum()
		for counts in participant_counts.values():
			self.total_counts.update(counts)
		"""Token counts for all utterances produced by all participants in the round represented by this instance."""
		self.instructor = instructor

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)


_RowMetrics = namedtuple("_RowMetrics", ("round", "participant_lang", "total_lang"))


class TokenTypeDataPrinter(object):
	@staticmethod
	def __create_initial_metrics(dyad_id: str, entity_id: int, sequence_order: int, round_id: int,
								 round_token_counts: RoundCounts) -> _RowMetrics:
		round_metrics = GameRoundMetrics(dyad_id, entity_id, sequence_order, round_id,
										 round_token_counts.game_round.initial_event, round_token_counts.instructor)
		participant_lang_metrics = dict(
			(participant_id, LanguageMetrics(dyad_id, round_id, participant_id, counts.utts, counts.relevant_tokens))
			for
			participant_id, counts in
			round_token_counts.participant_counts.items())
		total_lang_metrics = create_total_lang_metrics(dyad_id, round_id, round_token_counts)
		return _RowMetrics(round=round_metrics, participant_lang=participant_lang_metrics,
						   total_lang=total_lang_metrics)

	@staticmethod
	def __create_metrics_row(round_metrics: GameRoundMetrics, participant_lang_metrics: Dict[str, LanguageMetrics],
							 total_lang_metrics: LanguageMetrics,
							 ordered_participant_ids: Iterable[str]) -> List[str]:
		result = []
		result.extend(str(cell) for cell in round_metrics.row_cells())
		for participant_id in ordered_participant_ids:
			metrics = participant_lang_metrics[participant_id]
			result.extend(str(cell) for cell in metrics.row_cells())
		result.extend(str(cell) for cell in total_lang_metrics.row_cells())
		return result

	@staticmethod
	def __participant_ids(
			session_referent_token_counts: ItemsView[str, Tuple[Dict[int, ReferentCounts], Dict[str, str]]]):
		result = set()
		for _, (referent_token_counts, _) in session_referent_token_counts:
			for counts in referent_token_counts.values():
				for participant_id in counts.participant_ids():
					result.add(participant_id)
		return result

	def __init__(self, strict: bool):
		self.strict = strict

	def __call__(self, session_referent_token_counts: ItemsView[str, Tuple[Dict[int, ReferentCounts], Dict[str, str]]],
				 outfile):
		all_participant_ids = tuple(self.__participant_ids(session_referent_token_counts))
		participant_metric_col_names = (
			"{col_name}_PARTICIPANT_{participant_id}".format(col_name=col_name, participant_id=participant_id) for
			participant_id in all_participant_ids for
			col_name in LanguageMetrics.COL_NAMES)
		combined_metric_col_name_format = "{col_name}_" + COMBINED_PARTICIPANT_IDENTIFIER
		total_metric_col_names = (combined_metric_col_name_format.format(col_name=col_name) for col_name in
								  LanguageMetrics.COL_NAMES)
		print(COL_DELIM.join(
			itertools.chain(GameRoundMetrics.COL_NAMES, participant_metric_col_names, total_metric_col_names)),
			file=outfile)

		ordered_session_referent_token_counts = sorted(session_referent_token_counts, key=lambda item: item[0])
		for dyad_id, (referent_token_counts, source_participant_ids) in ordered_session_referent_token_counts:
			self.__print_session(dyad_id, referent_token_counts, all_participant_ids, outfile)

	def __print_session(self, dyad_id: Any, referent_token_counts: Dict[int, ReferentCounts],
						ordered_participant_ids: Iterable[str], outfile):
		for entity_id, entity_token_counts in sorted(referent_token_counts.items(), key=lambda item: item[0]):
			# Counts for each round, ordered by their respective round IDs
			ordered_round_counts = sorted(entity_token_counts.round_counts, key=lambda item: item[0])
			# Round IDs and their corresponding counts, enumerated by their coreference chain sequence number
			enumerated_ordered_round_counts = enumerate(ordered_round_counts, start=1)
			sequence_order, (round_id, round_token_counts) = next(enumerated_ordered_round_counts)
			initial_metrics = self.__create_initial_metrics(dyad_id, entity_id, sequence_order, round_id,
															round_token_counts)
			print(COL_DELIM.join(
				self.__create_metrics_row(initial_metrics.round, initial_metrics.participant_lang,
										  initial_metrics.total_lang, ordered_participant_ids)),
				file=outfile)

			previous_round_participant_total_tokens = {}
			previous_round_participant_token_types = {}
			for participant_id in ordered_participant_ids:
				metrics = initial_metrics.participant_lang.get(participant_id)
				initial_token_count = metrics.current_round_total_tokens if metrics else _DECIMAL_ZERO
				previous_round_participant_total_tokens[participant_id] = [initial_token_count]
				initial_token_types = set(metrics.current_round_token_types) if metrics else set()
				previous_round_participant_token_types[participant_id] = initial_token_types

			previous_round_total_total_tokens = [initial_metrics.total_lang.current_round_total_tokens]
			previous_round_total_token_types = set(initial_metrics.total_lang.current_round_token_types)

			for (sequence_order, (round_id, round_token_counts)) in enumerated_ordered_round_counts:
				round_metrics = GameRoundMetrics(dyad_id, entity_id, sequence_order, round_id,
												 round_token_counts.game_round.initial_event,
												 round_token_counts.instructor)
				participant_lang_metrics = {}
				for participant_id, counts in round_token_counts.participant_counts.items():
					previous_round_total_tokens = previous_round_participant_total_tokens[participant_id]
					previous_round_token_types = previous_round_participant_token_types[participant_id]
					current_participant_metrics = LanguageMetrics(dyad_id, round_id, participant_id,
																  counts.utts,
																  counts.relevant_tokens,
																  previous_round_total_tokens,
																  previous_round_token_types)
					participant_lang_metrics[participant_id] = current_participant_metrics
					previous_round_total_tokens.append(current_participant_metrics.current_round_total_tokens)
					previous_round_token_types.update(current_participant_metrics.current_round_token_types)

				total_lang_metrics = create_total_lang_metrics(dyad_id, round_id,
															   round_token_counts,
															   previous_round_total_total_tokens,
															   previous_round_total_token_types)
				current_metrics = _RowMetrics(round_metrics, participant_lang_metrics, total_lang_metrics)
				print(COL_DELIM.join(
					self.__create_metrics_row(current_metrics.round, current_metrics.participant_lang,
											  current_metrics.total_lang, ordered_participant_ids)),
					file=outfile)


def create_total_lang_metrics(dyad_id: str, round_id: int, round_token_counts: RoundCounts,
							  previous_round_total_tokens: Iterable[Decimal] = None,
							  previous_round_token_types: Set[str] = _EMPTY_SET):
	total_counts = round_token_counts.total_counts
	return LanguageMetrics(dyad_id, round_id, COMBINED_PARTICIPANT_IDENTIFIER,
						   total_counts.utts,
						   total_counts.relevant_tokens, previous_round_total_tokens, previous_round_token_types)


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


def score_round_ratio(event: game_events.Event) -> Decimal:
	try:
		round_count = event.round_id - 1
		result = Decimal(event.score) / Decimal(round_count)
	except (InvalidOperation, ZeroDivisionError):
		result = _DECIMAL_ONE
	return result


def time_score_ratio(event: game_events.Event) -> Decimal:
	try:
		result = Decimal(event.event_time) / Decimal(event.score)
	except ZeroDivisionError:
		result = _DECIMAL_INFINITY
	return result


def token_type_overlap_ratio(overlapping_token_type_count: int, unified_token_type_count: int) -> Decimal:
	return Decimal(overlapping_token_type_count) / Decimal(unified_token_type_count)


def try_length_drop(dyad_id: Any, round_id: Any, desc: Any, current_round_total_tokens: Decimal,
					mean_previous_token_count: Decimal, strict: bool) -> Decimal:
	try:
		result = length_drop(current_round_total_tokens, mean_previous_token_count)
	except (InvalidOperation, ZeroDivisionError) as e:
		msg = "A(n) {etype} occurred while calculating the length drop for \"{desc}\" of round {round_id} of session \"{dyad_id}\" (current_round_total_tokens={current_round_total_tokens}; mean_previous_token_count={mean_previous_token_count})".format(
			etype=type(e).__name__, desc=desc, round_id=round_id, dyad_id=dyad_id,
			current_round_total_tokens=current_round_total_tokens, mean_previous_token_count=mean_previous_token_count)
		if strict:
			raise ValueError(msg + '.') from e
		else:
			print(
				"WARNING: {msg}: {erepr}".format(msg=msg, erepr=repr(e)),
				file=sys.stderr)
			result = NULL_TOKEN_LENGTH_DROP_VALUE
	return result


def try_token_type_overlap_ratio(dyad_id: Any, round_id: Any, desc: Any, overlapping_token_type_count: int,
								 unified_token_type_count: int, strict: bool) -> Decimal:
	try:
		result = token_type_overlap_ratio(overlapping_token_type_count, unified_token_type_count)
	except (InvalidOperation, ZeroDivisionError) as e:
		msg = "A(n) {etype} occurred while calculating the token overlap ratio for \"{desc}\" of round {round_id} of session \"{dyad_id}\" (overlapping_token_type_count={overlapping_token_type_count}; unified_token_type_count={unified_token_type_count})".format(
			etype=type(e).__name__, desc=desc, round_id=round_id, dyad_id=dyad_id,
			overlapping_token_type_count=overlapping_token_type_count,
			unified_token_type_count=unified_token_type_count)
		if strict:
			raise ValueError(msg + '.') from e
		else:
			print(
				"WARNING: {msg}: {erepr}".format(msg=msg, erepr=repr(e)),
				file=sys.stderr)
			result = NULL_TOKEN_TYPE_OVERLAP_VALUE

	return result


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
	referent_token_counter = ParticipantCoreferenceChainTokenCounter(utterances.TokenSequenceFactory(),
																	 re_token_type_counts.FilteringTokenCounter(
																		 lambda
																			 token: token in token_groups.keys()))
	session_entity_counts = referent_token_counter(named_sessions)
	printer = TokenTypeDataPrinter(args.strict)
	printer(session_entity_counts.items(), outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
