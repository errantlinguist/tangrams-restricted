#!/usr/bin/env python3

import argparse
import itertools
import re
import statistics
import sys
from collections import defaultdict
from decimal import Decimal
from enum import Enum, unique
from typing import Any, Callable, Generic, Iterable, Mapping, Optional, Sequence, \
	Tuple, TypeVar

import game_events
import session_data as sd
import token_groups as tg
import utterances
from coreference_chains import Coreference, DialogueCoreferenceChainDatum
from game_utterances import GameRoundUtterances, SessionGameRoundUtteranceFactory

C = TypeVar('C')
COL_DELIM = '\t'
NULL_VALUE_REPR = "N/A"

_EMPTY_SET = frozenset()


class GroupCoreferenceChainDatum(object):
	def __init__(self):
		self.group_coref_chains = defaultdict(DialogueCoreferenceChainDatum)
		self.round_group_corefs = []

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)


class CoreferenceChainDataGrouper(object):
	def __init__(self, token_groups: tg.TokenGroupMapping):
		self.token_groups = token_groups

	def __call__(self, session_round_utts: GameRoundUtterances) -> GroupCoreferenceChainDatum:
		result = GroupCoreferenceChainDatum()

		for round_id, (game_round, round_utts) in enumerate(session_round_utts.game_round_utts,
															start=SessionGameRoundUtteranceFactory.ROUND_ID_OFFSET):
			round_instructor_id = session_round_utts.round_instructor_ids[round_id]
			content = (token for utt in round_utts if utt.speaker_id == round_instructor_id for token in utt.content)
			group_tokens = tg.create_group_token_set_dict(content, self.token_groups)
			grouped_referring_tokens = dict((grouping, (grouping.value.coref_chain_id_extractor(game_round),
														grouping.value.relevant_token_extractor(group_tokens))) for
											grouping in TokenGrouping)

			group_round_corefs = {}
			for grouping, (coref_chain_id, tokens) in grouped_referring_tokens.items():
				# If there are any semantically-relevant tokens at all, add a link in the coreference chain
				if tokens:
					entity_coref_chains = result.group_coref_chains[grouping]
					round_instructor_coref, session_coref = entity_coref_chains.add_coref(round_instructor_id,
																						  coref_chain_id,
																						  round_id,
																						  tokens)
				else:
					round_instructor_coref = None
					session_coref = None
				round_corefs = GameRoundCoreferences(round_instructor_coref, session_coref, grouped_referring_tokens)
				group_round_corefs[grouping] = round_corefs
			result.round_group_corefs.append((game_round, round_instructor_id, round_utts, group_round_corefs))

		return result


class CoreferenceChainDataPrinter(object):
	@staticmethod
	def __print_session(dyad_id: str, coref_groups: GroupCoreferenceChainDatum,
						group_coref_chain_corpora: Mapping[str, Sequence[DialogueCoreferenceChainDatum]], outfile):
		for game_round, round_instructor_id, round_utts, group_round_corefs in coref_groups.round_group_corefs:
			round_metrics = GameRoundMetrics(dyad_id, game_round, round_instructor_id)
			diag_metrics = DialogueMetrics(round_utts)

			token_metric_row_cells = []
			for group in TokenGrouping:
				round_corefs = group_round_corefs[group]
				coref_chain_id, relevant_tokens = round_corefs.referring_tokens[group]
				group_session_coref_chains = coref_groups.group_coref_chains[group]
				coref_chain_corpus = group_coref_chain_corpora[group]
				baseline_coref_chain_id_filter = group.value.baseline_coref_chain_id_filter_factory(coref_chain_id)

				token_metrics = TokenMetrics(relevant_tokens, round_instructor_id, coref_chain_id,
											 group_session_coref_chains,
											 coref_chain_corpus, baseline_coref_chain_id_filter)
				token_metric_row_cells.extend(token_metrics.row_cells())
			print(COL_DELIM.join(
				str(cell) for cell in
				itertools.chain(round_metrics.row_cells(), diag_metrics.row_cells(), token_metric_row_cells)),
				file=outfile)

	def __init__(self, token_groups: Mapping[str, str]):
		self.token_groups = token_groups

	def __call__(self, session_coref_groups: Mapping[str, GroupCoreferenceChainDatum], outfile):
		token_grouping_col_names = ("{}_{}".format(col_name, grouping.value.col_name) for grouping in TokenGrouping for
									col_name
									in TokenMetrics.COL_NAMES)
		print(COL_DELIM.join(
			itertools.chain(GameRoundMetrics.COL_NAMES, DialogueMetrics.COL_NAMES, token_grouping_col_names)),
			file=outfile)

		group_coref_chain_corpora = defaultdict(list)
		for group_coref_chains in session_coref_groups.values():
			for group, coref_chains in group_coref_chains.group_coref_chains.items():
				group_coref_chain_corpora[group].append(coref_chains)

		ordered_session_coref_groups = sorted(session_coref_groups.items(), key=lambda item: item[0])
		for dyad_id, coref_groups in ordered_session_coref_groups:
			self.__print_session(dyad_id, coref_groups, group_coref_chain_corpora, outfile)


class DialogueMetrics(object):
	COL_NAMES = ("DIALOGUE",)

	def __init__(self, utts: Iterable[utterances.Utterance]):
		self.utt_repr = utterances.dialogue_utt_str_repr(utts)

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def row_cells(self) -> Tuple[str, ...]:
		return self.utt_repr,


class GameRoundCoreferences(object):
	def __init__(self, instructor_coref: Coreference, session_coref: Coreference,
				 referring_tokens: Sequence[str]):
		self.instructor_coref = instructor_coref
		self.session_coref = session_coref
		self.referring_tokens = referring_tokens

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


class TokenGroupingDataMappings(object):
	def __init__(self, col_name: str, coref_chain_id_extractor: Callable[[game_events.GameRound], C],
				 relevant_token_extractor: Callable[[Mapping[str, Sequence[str]]], Sequence[str]],
				 baseline_coref_chain_id_filter_factory: Callable[[C], Callable[[C], bool]]):
		self.col_name = col_name
		self.coref_chain_id_extractor = coref_chain_id_extractor
		self.relevant_token_extractor = relevant_token_extractor
		self.baseline_coref_chain_id_filter_factory = baseline_coref_chain_id_filter_factory

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)


def _game_round_referent_id(game_round: game_events.GameRound) -> int:
	# NOTE: Only gets first referent entity (i.e. doesn't work if multiple entities are referents
	referent_id, referent_entity = next(game_round.initial_event.referent_entities)
	return referent_id


def _game_round_referent_shape(game_round: game_events.GameRound) -> str:
	# NOTE: Only gets first referent entity (i.e. doesn't work if multiple entities are referents
	referent_id, referent_entity = next(game_round.initial_event.referent_entities)
	return referent_entity.shape


@unique
class TokenGrouping(Enum):
	REFERENT = TokenGroupingDataMappings("REFERENT", _game_round_referent_id, lambda group_tokens: tuple(
		token for tokens in group_tokens.values() for token in tokens),
										 lambda referent_id: lambda other_referend_id: True)
	SHAPE = TokenGroupingDataMappings("SHAPE", _game_round_referent_shape,
									  lambda group_tokens: group_tokens.get("shape", _EMPTY_SET),
									  lambda shape: lambda other_shape: other_shape == shape)


class TokenMetrics(Generic[C]):
	COL_NAMES = (
		"RELEVANT_TOKENS", "COREF_SEQ_SELF", "OVERLAP_SELF", "OVERLAP_SELF_BASELINE", "COREF_SEQ_OTHER",
		"OVERLAP_OTHER")

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
	def __create_self_metrics(last_own_coref: Coreference,
							  coref_chain_corpus: Iterable[DialogueCoreferenceChainDatum],
							  baseline_coref_chain_id_filter: Callable[[C], bool]) -> Tuple[str, str, str]:
		if last_own_coref is None:
			coref_seq_no_repr = NULL_VALUE_REPR
			token_type_overlap_repr = NULL_VALUE_REPR
			token_type_overlap_baseline_repr = NULL_VALUE_REPR
		else:
			coref_seq_no_repr = str(last_own_coref.seq_number)
			entity_token_type_overlap = last_own_coref.token_type_overlap_with_antecedent()
			if entity_token_type_overlap is None:
				token_type_overlap_repr = NULL_VALUE_REPR
				token_type_overlap_baseline_repr = NULL_VALUE_REPR
			else:
				token_type_overlap_repr = str(entity_token_type_overlap)
				token_type_overlap_baseline = token_type_overlap_with_antecedent_baseline(last_own_coref,
																						  coref_chain_corpus,
																						  baseline_coref_chain_id_filter)
				token_type_overlap_baseline_repr = NULL_VALUE_REPR if token_type_overlap_baseline is None else str(
					token_type_overlap_baseline)

		return coref_seq_no_repr, token_type_overlap_repr, token_type_overlap_baseline_repr

	def __init__(self, relevant_tokens: Iterable[str], participant_id: str, coref_chain_id: C,
				 session_coref_chains: DialogueCoreferenceChainDatum,
				 coref_chain_corpus: Iterable[DialogueCoreferenceChainDatum],
				 baseline_coref_chain_id_filter: Callable[[C], bool]):
		self.relevant_tokens_repr = ','.join(sorted(relevant_tokens))
		last_own_coref, preceding_other_coref_overlap = session_coref_chains.token_type_overlap_with_other(
			participant_id,
			coref_chain_id)
		self.coref_seq_no_self_repr, self.token_type_overlap_self_repr, self.token_type_overlap_baseline_repr = self.__create_self_metrics(
			last_own_coref, coref_chain_corpus, baseline_coref_chain_id_filter)
		self.coref_seq_no_other_repr, self.token_type_overlap_other_repr = self.__create_other_metrics(
			preceding_other_coref_overlap)

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def row_cells(self) -> Tuple[str, ...]:
		return (self.relevant_tokens_repr, self.coref_seq_no_self_repr,
				self.token_type_overlap_self_repr, self.token_type_overlap_baseline_repr, self.coref_seq_no_other_repr,
				self.token_type_overlap_other_repr)


def token_type_overlap_with_antecedent_baseline(coref: Coreference,
												coref_chain_corpus: Iterable["DialogueCoreferenceChainDatum"],
												coref_chain_id_filter: Callable[[C], bool]) -> Optional[Decimal]:
	"""

	:param coref: The coreference to compute the overlap for.
	:type coref: Coreference
	:param coref_chain_corpus: All DialogueCoreferenceChainDatum instances to use for calculation, each of which representing a single dyad in the entire corpus thereof.
	:param coref_chain_id_filter: A filter matching the identifier(s) for analogous coreference chains to compare against.
	:return: A ratio of the number of overlapping token types.
	:rtype: Optional[Decimal]
	"""
	antecedent_corefs = coref.analogous_antecedent_corefs(coref_chain_corpus, coref_chain_id_filter)
	if antecedent_corefs:
		result = statistics.mean(coref.token_type_overlap(coref) for coref in antecedent_corefs)
	else:
		result = None
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
	session_game_round_utt_factory = SessionGameRoundUtteranceFactory(
		utterances.TokenSequenceFactory())
	session_game_round_utts = session_game_round_utt_factory(named_sessions)

	grouper = CoreferenceChainDataGrouper(token_groups)
	session_coref_groups = dict(
		(dyad_id, grouper(game_round_utts)) for (dyad_id, game_round_utts) in session_game_round_utts.items())

	printer = CoreferenceChainDataPrinter(token_groups)
	printer(session_coref_groups, outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
