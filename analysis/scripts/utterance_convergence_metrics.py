#!/usr/bin/env python3

import argparse
import itertools
import re
import sys
from collections import defaultdict
from decimal import Decimal
from enum import Enum, unique
from typing import Any, Callable, Generic, Iterable, ItemsView, Mapping, Optional, \
	Sequence, Tuple, TypeVar

import game_events
import session_data as sd
import token_groups as tg
import utterances
from coreference_chains import Coreference, DialogueCoreferenceChainDatum
from game_utterances import GameRoundUtterances, SessionGameRoundUtteranceFactory

COL_DELIM = '\t'
NULL_VALUE_REPR = "N/A"

R = TypeVar('R')

_EMPTY_SET = frozenset()


class CoreferenceChainDataPrinter(object):
	def __init__(self, token_groups: Mapping[str, str]):
		self.token_groups = token_groups

	def __call__(self, session_data: ItemsView[str, GameRoundUtterances], outfile):
		token_grouping_col_names = ("{}_{}".format(col_name, grouping.value.col_name) for grouping in TokenGrouping for
									col_name
									in TokenMetrics.COL_NAMES)
		print(COL_DELIM.join(
			itertools.chain(GameRoundMetrics.COL_NAMES, DialogueMetrics.COL_NAMES, token_grouping_col_names)),
			file=outfile)
		ordered_session_data = sorted(session_data, key=lambda item: item[0])
		for dyad_id, session_data in ordered_session_data:
			self.__print_session(dyad_id, session_data, outfile)

	def __print_session(self, dyad_id: str, session_data: GameRoundUtterances, outfile):
		grouped_coref_chains = defaultdict(DialogueCoreferenceChainDatum)

		for round_id, (game_round, round_utts) in enumerate(session_data.game_round_utts,
															start=SessionGameRoundUtteranceFactory.ROUND_ID_OFFSET):
			round_instructor_id = session_data.round_instructor_ids[round_id]
			round_metrics = GameRoundMetrics(dyad_id, game_round, round_instructor_id)
			# NOTE: Only gets first referent entity (i.e. doesn't work if multiple entities are referents
			referent_id, referent_entity = next(game_round.initial_event.referent_entities)

			for (participant_id, participant_turn_utts) in utterances.group_utts_by_speaker_id(round_utts):
				diag_metrics = DialogueMetrics(participant_id,
											   utterances.join_utt_sentence_reprs(participant_turn_utts))
				content = (token for utt in participant_turn_utts for token in utt.content)
				group_tokens = tg.create_group_token_list_dict(content, self.token_groups)
				grouped_referring_tokens = dict((grouping, (grouping.value.coref_chain_id_extractor(game_round),
															grouping.value.relevant_token_extractor(group_tokens))) for
												grouping in TokenGrouping)

				for grouping, (coref_chain_id, tokens) in grouped_referring_tokens.items():
					# If there are any semantically-relevant tokens at all, add a link in the coreference chain
					if tokens:
						entity_coref_chains = grouped_coref_chains[grouping]
						entity_coref_chains.add_entity_corefs(participant_id,
															  coref_chain_id,
															  round_id,
															  tokens)

				# Calculate metric after adding all coreference chains so that the "baseline" metrics can be correctly calculated
				token_metric_row_cells = []
				for group in TokenGrouping:
					coref_chain_id, tokens = grouped_referring_tokens[group]
					coref_chains = grouped_coref_chains[group]
					token_metrics = TokenMetrics(tokens, participant_id, coref_chain_id, coref_chains)
					token_metric_row_cells.extend(token_metrics.row_cells())
				print(COL_DELIM.join(
					str(cell) for cell in
					itertools.chain(round_metrics.row_cells(), diag_metrics.row_cells(), token_metric_row_cells)),
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


class TokenGroupingDataMappings(object):
	def __init__(self, col_name: str, coref_chain_id_extractor: Callable[[game_events.GameRound], Any],
				 relevant_token_extractor: Callable[[Mapping[str, Sequence[str]]], Sequence[str]]):
		self.col_name = col_name
		self.coref_chain_id_extractor = coref_chain_id_extractor
		self.relevant_token_extractor = relevant_token_extractor

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)


def _game_round_referent_id(game_round: game_events.GameRound) -> int:
	# NOTE: Only gets first referent entity (i.e. doesn't work if multiple entities are referents
	referent_id, referent_entity = next(game_round.initial_event.referent_entities)
	return referent_id


def _game_round_referent_shape(game_round: game_events.GameRound) -> int:
	# NOTE: Only gets first referent entity (i.e. doesn't work if multiple entities are referents
	referent_id, referent_entity = next(game_round.initial_event.referent_entities)
	return referent_entity.shape


@unique
class TokenGrouping(Enum):
	REFERENT = TokenGroupingDataMappings("REFERENT", _game_round_referent_id, lambda group_tokens: tuple(
		token for tokens in group_tokens.values() for token in tokens))
	SHAPE = TokenGroupingDataMappings("SHAPE", _game_round_referent_shape,
									  lambda group_tokens: group_tokens.get("shape", _EMPTY_SET))


class TokenMetrics(Generic[R]):
	COL_NAMES = ("RELEVANT_TOKEN_TYPES", "COREF_SEQ_SELF", "OVERLAP_SELF", "COREF_SEQ_OTHER", "OVERLAP_OTHER")

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
				 session_corefs: DialogueCoreferenceChainDatum):
		self.relevant_token_types_repr = ','.join(sorted(frozenset(relevant_tokens)))
		last_own_coref, preceding_other_coref_overlap = session_corefs.token_type_overlap_with_other(participant_id,
																									 coref_chain_id)
		self.coref_seq_no_self_repr, self.token_type_overlap_self_repr = self.__create_self_metrics(last_own_coref)
		self.coref_seq_no_other_repr, self.token_type_overlap_other_repr = self.__create_other_metrics(
			preceding_other_coref_overlap)

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def row_cells(self) -> Tuple[str, ...]:
		return (self.relevant_token_types_repr, self.coref_seq_no_self_repr,
				self.token_type_overlap_self_repr, self.coref_seq_no_other_repr, self.token_type_overlap_other_repr)


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
	session_game_round_utts = session_game_round_utt_factory(named_sessions)
	printer = CoreferenceChainDataPrinter(token_groups)
	printer(session_game_round_utts.items(), outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
