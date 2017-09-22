#!/usr/bin/env python3

import argparse
import itertools
import re
import sys
from collections import defaultdict
from decimal import Decimal
from enum import Enum, unique
from typing import Any, Callable, Dict, FrozenSet, Generic, Iterable, Iterator, ItemsView, Mapping, MutableSequence, \
	Optional, \
	Sequence, \
	Tuple, TypeVar

import game_events
import referent_token_type_counts
import session_data as sd
import token_groups as tg
import utterances

COL_DELIM = '\t'
NULL_VALUE_REPR = "N/A"

R = TypeVar('R')

_EMPTY_SET = frozenset()


class Coreference(object):
	"""
	This class represents a single reference in a coreference chain.
	"""

	def __init__(self, coref_id: int, tokens: FrozenSet[str], round_id: int,
				 antecedent: Optional["Coreference"]):
		self.coref_id = coref_id
		self.tokens = tokens
		self.round_id = round_id
		self.antecedent = antecedent

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	@property
	def antecedents(self) -> Iterator["Coreference"]:
		last_antecedent = self.antecedent
		while last_antecedent is not None:
			last_antecedent = last_antecedent.antecedent
			yield last_antecedent

	@property
	def seq_number(self):
		return sum(1 for _ in self.antecedents) + 1

	@property
	def token_types(self):
		return frozenset(self.tokens)

	def token_type_overlap_with_self(self) -> Optional[Decimal]:
		"""
		Calculates the number of token types (i.e. unique words) of this coreference which overlap with the types of the coreference preceding this one within the same coreference chain.
		:return: A ratio of the number of overlapping token types.
		:rtype: Decimal
		"""
		if self.antecedent is None:
			result = None
		else:
			token_types = self.token_types
			preceding_token_types = self.antecedent.token_types
			result = token_type_overlap_ratio(token_types.union(
				preceding_token_types), token_types.intersection(
				preceding_token_types))
		return result


class CoreferenceChainDataPrinter(object):
	def __init__(self, token_groups: Mapping[str, str]):
		self.token_groups = token_groups

	def __call__(self, session_data: ItemsView[str, "GameRoundUtterances"], outfile):
		token_grouping_col_names = ("{}_{}".format(col_name, grouping.value) for grouping in TokenGrouping for col_name
									in TokenMetrics.COL_NAMES)
		print(COL_DELIM.join(
			itertools.chain(GameRoundMetrics.COL_NAMES, DialogueMetrics.COL_NAMES, token_grouping_col_names)),
			file=outfile)
		ordered_session_data = sorted(session_data, key=lambda item: item[0])
		for dyad_id, session_data in ordered_session_data:
			self.__print_session(dyad_id, session_data, outfile)

	def __print_session(self, dyad_id: str, session_data: "GameRoundUtterances", outfile):
		grouped_corefs = defaultdict(SessionCoreferenceChainDatum)

		for round_id, (game_round, round_utts) in enumerate(session_data.game_round_utts,
															start=SessionGameRoundUtteranceFactory.ROUND_ID_OFFSET):
			round_instructor_id = session_data.round_instructor_ids[round_id]
			round_metrics = GameRoundMetrics(dyad_id, game_round, round_instructor_id)
			# NOTE: Only gets first referent entity (i.e. doesn't work if multiple entities are referents
			referent_id, referent_entity = next(game_round.initial_event.referent_entities)

			for (participant_id, participant_turn_utts) in utterances.group_utts_by_speaker_id(round_utts):
				lang_metrics = DialogueMetrics(participant_id,
											   utterances.join_utt_sentence_reprs(participant_turn_utts))
				grouped_referring_tokens = {}

				content = (token for utt in participant_turn_utts for token in utt.content)
				group_tokens = tg.create_group_token_set_dict(content, self.token_groups)
				all_grouped_tokens = frozenset(token for tokens in group_tokens.values() for token in tokens)
				grouped_referring_tokens[TokenGrouping.REFERENT] = (referent_id, all_grouped_tokens)

				shape_tokens = group_tokens.get("shape", _EMPTY_SET)
				grouped_referring_tokens[TokenGrouping.SHAPE] = (referent_entity.shape, shape_tokens)

				for grouping, (coref_chain_id, tokens) in grouped_referring_tokens.items():
					# If there are any semantically-relevant tokens at all, add a link in the coreference chain
					if tokens:
						entity_corefs = grouped_corefs[grouping]
						entity_corefs.add_entity_corefs(participant_id,
														coref_chain_id,
														round_id,
														tokens)

				# Calculate metric after adding all coreference chains so that the "baseline" metrics can be correctly calculated
				token_metric_row_cells = []
				for group in TokenGrouping:
					coref_chain_id, tokens = grouped_referring_tokens[group]
					corefs = grouped_corefs[group]
					token_metrics = TokenMetrics(tokens, participant_id, coref_chain_id, corefs)
					token_metric_row_cells.extend(token_metrics.row_cells())
				print(COL_DELIM.join(
					str(cell) for cell in
					itertools.chain(round_metrics.row_cells(), lang_metrics.row_cells(), token_metric_row_cells)),
					file=outfile)


class DialogueMetrics(object):
	COL_NAMES = (
		"SPEAKER", "UTTERANCE")

	def __init__(self, speaker_id: str, utt_repr: str):
		self.speaker_id = speaker_id
		self.utt_repr = utt_repr

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def row_cells(self) -> Tuple[str, ...]:
		return self.speaker_id, self.utt_repr


class GameRoundMetrics(object):
	COL_NAMES = (
		"DYAD", "ROUND", "INSTRUCTOR", "ROUND_START_TIME", "SCORE",
		"TIME_SCORE_RATIO",
		"SCORE_ROUND_RATIO", "REFERENT", "SHAPE", "HUE",)

	def __init__(self, dyad_id: str, game_round: game_events.GameRound, instructor: str):
		self.dyad_id = dyad_id
		self.round_id = game_round.round_id
		self.instructor = instructor

		initial_event = game_round.initial_event
		self.time = initial_event.event_time
		self.score = initial_event.score
		self.time_score_ratio = initial_event.time_score_ratio()
		self.score_round_ratio = initial_event.score_round_ratio()

		# NOTE: Only gets first referent entity (i.e. doesn't work if multiple entities are referents
		referent_id, referent_entity = next(initial_event.referent_entities)
		self.referent_id = referent_id
		self.referent_shape = referent_entity.shape
		self.referent_hue = referent_entity.hue

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def row_cells(self) -> Tuple[Any, ...]:
		return (
			self.dyad_id, self.round_id, self.instructor, self.time, self.score,
			self.time_score_ratio, self.score_round_ratio, self.referent_id, self.referent_shape,
			self.referent_hue)


class GameRoundUtterances(object):
	"""
	A class associating game rounds with the dialogues for each.
	"""

	def __init__(self, game_round_utts: Sequence[Tuple[game_events.GameRound, Sequence[utterances.Utterance]]],
				 round_instructor_ids: Mapping[int, str]):
		self.game_round_utts = game_round_utts
		self.round_instructor_ids = round_instructor_ids

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)


class SessionCoreferenceChainDatum(Generic[R]):
	@staticmethod
	def __add_to_chain(coref_id: int, tokens: FrozenSet[str], round_id: int,
					   coref_chain: MutableSequence[Coreference]) -> Coreference:
		if coref_chain:
			antecedent = coref_chain[len(coref_chain) - 1]
			result = Coreference(coref_id, tokens, round_id, antecedent)
		else:
			result = Coreference(coref_id, tokens, round_id, None)
		coref_chain.append(result)
		return result

	def __init__(self):
		self.participant_corefs = defaultdict(lambda: defaultdict(list))
		self.session_corefs = defaultdict(list)
		self.__last_coref_id = 0

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def add_entity_corefs(self, participant_id: str,
						  referent_id: R, round_id: int, tokens: FrozenSet[str]) -> Tuple[Coreference, Coreference]:
		participant_corefs = self.participant_corefs[participant_id][referent_id]
		participant_coref = self.__add_to_chain(self.__next_coref_id, tokens, round_id, participant_corefs)

		session_corefs = self.session_corefs[referent_id]
		session_coref = self.__add_to_chain(self.__next_coref_id, tokens, round_id, session_corefs)

		return participant_coref, session_coref

	def token_type_overlap_with_other(self,
									  participant_id: str, coref_chain_id: R) -> Tuple[
		Optional[Coreference], Optional[Tuple[Coreference, Decimal]]]:
		"""
		Calculates the number of token types (i.e. unique words) of the coreference from a given participant with that of the coreference preceding it which is not from the same participant but refers to the same referent, indicated by sharing the same coreference chain ID.

		:param participant_id: The ID of the participant to calculate overlap for.
		:param coref_chain_id: An identifier for the referent to search for coreference chains featuring it as a referent.
		:return: The ratio of overlap between the last coreference for the given participant and coreference chain ID and the preceding coreference with a different participant but the same coreference chain ID.
		"""
		own_participant_corefs = self.participant_corefs[participant_id][coref_chain_id]
		if own_participant_corefs:
			last_own_coref = own_participant_corefs[len(own_participant_corefs) - 1]
			last_own_coref_id = last_own_coref.coref_id
			last_own_coref_token_types = last_own_coref.token_types

			# Get all coreference chains for all participants with an ID not equal to the given one
			other_participant_corefs = ((other_participant_id, corefs) for (other_participant_id, corefs) in
										self.participant_corefs.items() if other_participant_id != participant_id)
			# Get all coreference chains with the same referent regardless of the participant ID
			other_corefs = (coref_chain[len(coref_chain) - 1] for (_, corefs) in other_participant_corefs for
							(other_coref_chain_id, coref_chain) in
							corefs.items() if
							other_coref_chain_id == coref_chain_id and len(coref_chain) > 0)
			other_previous_corefs = tuple(coref for coref in other_corefs if coref.coref_id < last_own_coref_id)
			if other_previous_corefs:
				preceding_coref = max(other_previous_corefs, key=lambda coref: coref.coref_id)
				preceding_token_types = preceding_coref.token_types
				overlap = token_type_overlap_ratio(last_own_coref_token_types, preceding_token_types)
				preceding_coref_overlap = preceding_coref, overlap
			else:
				preceding_coref_overlap = None
		else:
			last_own_coref = None
			preceding_coref_overlap = None
		return last_own_coref, preceding_coref_overlap

	@property
	def __next_coref_id(self) -> int:
		result = self.__last_coref_id + 1
		self.__last_coref_id = result
		return result


class SessionGameRoundUtteranceFactory(object):
	ROUND_ID_OFFSET = 1

	def __init__(self, token_seq_factory: Callable[[Iterable[str]], Sequence[str]]):
		self.token_seq_factory = token_seq_factory

	def __call__(self, named_sessions: Iterable[Tuple[str, sd.SessionData]]) -> Dict[
		str, GameRoundUtterances]:
		result = {}
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
			entity_coreference_chains = self.__create_game_round_utterances(session)
			result[dyad_id] = entity_coreference_chains

		return result

	def __create_game_round_utterances(self, session: sd.SessionData) -> GameRoundUtterances:
		event_data = game_events.read_events(session)
		source_participant_ids = event_data.source_participant_ids
		seg_utt_factory = utterances.SegmentUtteranceFactory(self.token_seq_factory,
															 lambda source_id: source_participant_ids[source_id])
		game_rounds = game_events.create_game_rounds(event_data.events)
		segments = utterances.read_segments(session.utts)
		utt_times = utterances.UtteranceTimes(seg_utt_factory(segments))
		game_round_utts = tuple((game_round, tuple(utt_iter)) for (game_round, utt_iter) in
								referent_token_type_counts.zip_game_round_utterances(game_rounds, utt_times))
		event_participant_id_factory = game_events.EventParticipantIdFactory(event_data.initial_instructor_id)

		round_instructor_ids = {}
		enumerated_game_round_utts = enumerate(game_round_utts, start=self.ROUND_ID_OFFSET)
		for round_id, round_utts in enumerated_game_round_utts:
			game_round, round_utts = round_utts
			initial_event = game_round.initial_event
			round_instructor_id = event_participant_id_factory(initial_event)
			existing_instructor_id = round_instructor_ids.get(round_id, None)
			if existing_instructor_id and existing_instructor_id != round_instructor_id:
				raise ValueError("Differing instructor ID for round {}.".format(round_id))
			else:
				round_instructor_ids[round_id] = round_instructor_id
		return GameRoundUtterances(game_round_utts, round_instructor_ids)


@unique
class TokenGrouping(Enum):
	REFERENT = "REFERENT"
	SHAPE = "SHAPE"


class TokenMetrics(Generic[R]):
	COL_NAMES = ("RELEVANT_TOKENS", "COREF_SEQ_SELF", "OVERLAP_SELF", "COREF_SEQ_OTHER", "OVERLAP_OTHER")

	@staticmethod
	def __create_other_metrics(preceding_other_coref_overlap: Optional[Tuple[Coreference, Decimal]]) -> Tuple[
		str, str]:
		if preceding_other_coref_overlap is None:
			coref_seq_no_repr = NULL_VALUE_REPR
			token_type_overlap_repr = NULL_VALUE_REPR
		else:
			prev_other_coref, entity_token_type_overlap = preceding_other_coref_overlap
			coref_seq_no_repr = str(prev_other_coref.seq_number)
			token_type_overlap_repr = str(
				entity_token_type_overlap)
		return coref_seq_no_repr, token_type_overlap_repr

	@staticmethod
	def __create_self_metrics(last_own_coref: Coreference) -> Tuple[str, str]:
		if last_own_coref is None:
			coref_seq_no_repr = NULL_VALUE_REPR
			token_type_overlap_repr = NULL_VALUE_REPR
		else:
			coref_seq_no_repr = str(last_own_coref.seq_number)
			entity_token_type_overlap = last_own_coref.token_type_overlap_with_self()
			token_type_overlap_repr = NULL_VALUE_REPR if entity_token_type_overlap is None else str(
				entity_token_type_overlap)
		return coref_seq_no_repr, token_type_overlap_repr

	def __init__(self, relevant_tokens: Iterable[str], participant_id: str, coref_chain_id: R,
				 session_corefs: SessionCoreferenceChainDatum):
		self.relevant_tokens_repr = ','.join(sorted(relevant_tokens))
		last_own_coref, preceding_other_coref_overlap = session_corefs.token_type_overlap_with_other(participant_id,
																									 coref_chain_id)
		self.coref_seq_no_self_repr, self.token_type_overlap_self_repr = self.__create_self_metrics(last_own_coref)
		self.coref_seq_no_other_repr, self.token_type_overlap_other_repr = self.__create_other_metrics(
			preceding_other_coref_overlap)

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def row_cells(self) -> Tuple[str, ...]:
		return (self.relevant_tokens_repr, self.coref_seq_no_self_repr,
				self.token_type_overlap_self_repr, self.coref_seq_no_other_repr, self.token_type_overlap_other_repr)


def token_type_overlap_ratio(token_types: FrozenSet[str], preceding_token_types: FrozenSet[str]) -> Decimal:
	unified_token_type_count = len(token_types.union(
		preceding_token_types))
	overlapping_token_type_count = len(token_types.intersection(
		preceding_token_types))
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
	session_game_round_utt_factory = SessionGameRoundUtteranceFactory(
		utterances.TokenSequenceFactory())
	session_entity_counts = session_game_round_utt_factory(named_sessions)
	printer = CoreferenceChainDataPrinter(token_groups)
	printer(session_entity_counts.items(), outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
