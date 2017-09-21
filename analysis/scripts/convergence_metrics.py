#!/usr/bin/env python3

import argparse
import itertools
import re
import sys
from collections import defaultdict
from decimal import Decimal
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

C = TypeVar('C')
R = TypeVar('R')

_EMPTY_SET = frozenset()


class CoreferenceChainDatum(object):
	"""
	A class representing all chains of coreference for a particular entity, e.g. the entity with the ID "1".
	"""

	def __init__(self):
		"""A description of the entity being referred to by the chain(s) represented by this object."""
		self.participant_chains = defaultdict(list)
		"""A dictionary of coreference chains for each individual dialogue participant."""
		self.dyad_chain = []
		"""A coreference chain for the entire session."""

	def add(self, round_id: int, utts: Sequence[utterances.Utterance]):
		round_participant_utts = utterances.create_speaker_dict(utts)
		for participant_id, participant_utts in round_participant_utts.items():
			participant_chain = self.participant_chains[participant_id]
			participant_chain.append((round_id, participant_utts))

		self.dyad_chain.append((round_id, utts))

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)


class Coreference(object):
	@staticmethod
	def token_type_overlap_with_other(participant_corefs: Mapping[str, Mapping[C, Sequence["Coreference"]]],
									  participant_id: str, coref_chain_id: C) -> Optional[Decimal]:
		own_participant_corefs = participant_corefs[participant_id][coref_chain_id]
		last_own_coref = own_participant_corefs[len(own_participant_corefs) - 1]
		last_own_coref_round_id = last_own_coref.round_id
		last_own_coref_token_types = last_own_coref.token_types

		# Get all coreference chains for all participants with an ID not equal to the given one
		other_participant_corefs = ((other_participant_id, corefs) for (other_participant_id, corefs) in
									participant_corefs.items() if other_participant_id != participant_id)
		# Get all coreference chains with the same referent regardless of the participant ID
		other_corefs = (coref for (_, corefs) in other_participant_corefs for (other_coref_chain_id, coref) in corefs if
						other_coref_chain_id == coref_chain_id)

	# other_preceding_corefs = (coref for coref in other_corefs if coref.round_id < )
	# TODO: Finish

	def __init__(self, coref_id: int, tokens: FrozenSet[str], round_id: int, antecedent: "Coreference" = None):
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
	def chain_head(self) -> Optional["Coreference"]:
		result = self.antecedent
		while result is not None:
			result = result.antecedent
		yield result

	@property
	def seq_number(self):
		return sum(1 for _ in self.antecedents) + 1

	@property
	def token_types(self):
		return frozenset(self.tokens)

	def token_type_overlap_with_self(self) -> Optional[Decimal]:
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
	LANG_COL_NAMES = (
		"SPEAKER", "UTTERANCE", "ALL_TOKENS", "COREF_CHAIN_SEQ_SELF_ENTITY", "OVERLAP_ENTITY_SELF", "SHAPE_TOKENS")

	def __init__(self, token_groups: Mapping[str, str]):
		self.token_groups = token_groups

	def __call__(self, session_data: ItemsView[str, "EntityCoreferenceChainDatum"], outfile):
		print(COL_DELIM.join(itertools.chain(GameRoundMetrics.COL_NAMES, self.LANG_COL_NAMES)), file=outfile)
		ordered_session_data = sorted(session_data, key=lambda item: item[0])
		for dyad_id, session_data in ordered_session_data:
			self.__print_session(dyad_id, session_data, outfile)

	def __print_session(self, dyad_id: str, session_data: "EntityCoreferenceChainDatum", outfile):
		entity_corefs = SessionCoreferenceChainDatum()
		shape_corefs = SessionCoreferenceChainDatum()

		for round_id, game_round_utts in enumerate(session_data.game_round_utts,
												   start=ParticipantCoreferenceChainTokenCounter.ROUND_ID_OFFSET):
			game_round, round_utts = game_round_utts
			round_instructor_id = session_data.round_instructor_ids[round_id]
			round_metrics = GameRoundMetrics(dyad_id, game_round, round_instructor_id)
			# NOTE: Only gets first referent entity (i.e. doesn't work if multiple entities are referents
			referent_id, referent_entity = next(game_round.initial_event.referent_entities)

			for utt in round_utts:
				participant_id = utt.speaker_id
				utt_repr = utterances.token_seq_repr(utt.content)

				group_tokens = tg.create_group_token_dict(utt.content, self.token_groups)
				all_grouped_tokens = frozenset(token for tokens in group_tokens.values() for token in tokens)
				entity_coref_seq_no_repr = NULL_VALUE_REPR
				entity_token_type_overlap_self_repr = NULL_VALUE_REPR

				shape_tokens = _EMPTY_SET
				shape_token_type_overlap_self_repr = NULL_VALUE_REPR
				# If there are any semantically-relevant tokens at all, add a link in the entity coreference chain
				if all_grouped_tokens:
					participant_entity_coref, session_entity_coref = entity_corefs.add_entity_corefs(participant_id,
																									 referent_id,
																									 round_id,
																									 all_grouped_tokens)

					shape_token_group_name = "shape"
					shape_tokens = group_tokens.get(shape_token_group_name, _EMPTY_SET)
					if shape_tokens:
						participant_shape_coref, session_shape_coref = shape_corefs.add_entity_corefs(participant_id,
																									  referent_entity.shape,
																									  round_id,
																									  all_grouped_tokens)
					else:
						print("No tokens for group \"{}\"; Skipping.".format(shape_token_group_name), file=sys.stderr)

					entity_coref_seq_no_repr = participant_entity_coref.seq_number
					entity_token_type_overlap_self = participant_entity_coref.token_type_overlap_with_self()
					entity_token_type_overlap_self_repr = NULL_VALUE_REPR if entity_token_type_overlap_self is None else str(
						entity_token_type_overlap_self)
				lang_row_cells = (
					participant_id, utt_repr,
					','.join(sorted(all_grouped_tokens)), entity_coref_seq_no_repr, entity_token_type_overlap_self_repr,
					','.join(sorted(shape_tokens)))
				print(COL_DELIM.join(str(cell) for cell in itertools.chain(round_metrics.row_cells(), lang_row_cells)),
					  file=outfile)


class EntityCoreferenceChainDatum(object):
	"""
	A class coreference information for all entities in a particular game.
	"""

	def __init__(self, game_round_utts: Sequence[Tuple[game_events.GameRound, Sequence[utterances.Utterance]]],
				 entity_coreference_chains: Dict[int, CoreferenceChainDatum], round_instructor_ids: Mapping[int, str]):
		self.game_round_utts = game_round_utts
		self.entity_coreference_chains = entity_coreference_chains
		self.round_instructor_ids = round_instructor_ids

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)


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


class ParticipantCoreferenceChainTokenCounter(object):
	ROUND_ID_OFFSET = 1

	def __init__(self, token_seq_factory: Callable[[Iterable[str]], Sequence[str]]):
		self.token_seq_factory = token_seq_factory

	def __call__(self, named_sessions: Iterable[Tuple[str, sd.SessionData]]) -> Dict[
		str, EntityCoreferenceChainDatum]:
		result = {}
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
			entity_coreference_chains = self.__create_session_counts(session)
			result[dyad_id] = entity_coreference_chains

		return result

	def __create_session_counts(self, session: sd.SessionData) -> EntityCoreferenceChainDatum:
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

		entity_coreference_chains = {}
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
				referent_entities = initial_event.referent_entities
				for referent_id, referent_entity in referent_entities:
					try:
						coreference_chains = entity_coreference_chains[referent_id]
					except KeyError:
						# print("No coreference chains have yet been built for entity {}.".format(referent_id),
						#	  file=sys.stderr)
						coreference_chains = CoreferenceChainDatum()
						entity_coreference_chains[referent_id] = coreference_chains
					coreference_chains.add(round_id, round_utts)
		return EntityCoreferenceChainDatum(game_round_utts, entity_coreference_chains, round_instructor_ids)


class SessionCoreferenceChainDatum(Generic[R]):
	@staticmethod
	def __add_to_chain(coref_id: int, tokens: FrozenSet[str], round_id: int,
					   coref_chain: MutableSequence["Coreference"]) -> "Coreference":
		if coref_chain:
			antecedent = coref_chain[len(coref_chain) - 1]
			result = Coreference(coref_id, tokens, round_id, antecedent)
		else:
			result = Coreference(coref_id, tokens, round_id)
		coref_chain.append(result)
		return result

	def __init__(self):
		self.participant_entity_corefs = defaultdict(lambda: defaultdict(list))
		self.session_entity_corefs = defaultdict(list)
		self.__last_coref_id = 0

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def add_entity_corefs(self, participant_id: str,
						  referent_id: R, round_id: int, tokens: FrozenSet[str]) -> Tuple[Coreference, Coreference]:
		participant_corefs = self.participant_entity_corefs[participant_id][referent_id]
		participant_coref = self.__add_to_chain(self.__next_coref_id, tokens, round_id, participant_corefs)

		session_corefs = self.session_entity_corefs[referent_id]
		session_coref = self.__add_to_chain(self.__next_coref_id, tokens, round_id, session_corefs)

		return participant_coref, session_coref

	@property
	def __next_coref_id(self) -> int:
		result = self.__last_coref_id + 1
		self.__last_coref_id = result
		return result


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
	referent_token_counter = ParticipantCoreferenceChainTokenCounter(
		utterances.TokenSequenceFactory())
	session_entity_counts = referent_token_counter(named_sessions)
	printer = CoreferenceChainDataPrinter(token_groups)
	printer(session_entity_counts.items(), outfile)


# printer = coreference_chain_overlap.TokenTypeDataPrinter(args.strict)
# printer(session_entity_counts.items(), outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
