#!/usr/bin/env python3

import argparse
import re
import sys
from collections import defaultdict
from typing import Any, Callable, Dict, Iterable, Iterator, List, Sequence, Tuple, TypeVar

import game_events
import re_token_type_counts
import utterances
from referent_token_type_counts import ReferentTokenTypeDatum, TokenTypeDataPrinter
from session_data import SessionData, walk_session_data
from token_groups import read_token_group_dict

COL_DELIM = '\t'
NULL_VALUE_REPR = '?'

T = TypeVar('T')


class ReferentTokenCounter(object):
	def __init__(self, seg_utt_factory: utterances.SegmentUtteranceFactory,
				 filtering_token_counter: Callable[
					 [Iterable[utterances.Utterance]], re_token_type_counts.FilteredTokenTypeDatum]):
		self.seg_utt_factory = seg_utt_factory
		self.filtering_token_counter = filtering_token_counter

	def __call__(self, named_sessions: Dict[T, SessionData]) -> Dict[T, Dict[str, "ReferentTokenTypeDatum"]]:
		result = {}
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
			referent_token_counts = dict(
				(entity_id, ReferentTokenTypeDatum(referent_counts)) for (entity_id, referent_counts) in
				self.__referent_token_counts(session).items())
			trim_empty_tail_rounds(dyad_id, referent_token_counts)
			result[dyad_id] = referent_token_counts

		return result

	def __referent_token_counts(self, session: SessionData) -> Dict[
		str, List[Tuple[int, re_token_type_counts.FilteredTokenTypeDatum]]]:
		result = defaultdict(list)
		events = game_events.read_events(session)
		game_rounds = iter(game_events.create_game_rounds(events))
		segments = utterances.read_segments(session.utts)
		utts = tuple(self.seg_utt_factory(segments))
		game_round_utts = zip_game_round_utterances(game_rounds, utts)
		for (round_id, game_round_utts) in enumerate(game_round_utts, start=1):
			initial_event = next(iter(game_round_utts[0].events))
			for _, referent_entity in initial_event.referent_entities:
				round_referent_token_counts = self.filtering_token_counter(game_round_utts[1])
				referent_token_counts = result[referent_entity.shape]
				referent_token_counts.append((round_id, round_referent_token_counts))

		return result


def find_last_round_id(referent_token_counts: Dict[T, ReferentTokenTypeDatum]) -> Tuple[T, int]:
	return max(
		((entity_id, round_id) for (entity_id, counts) in referent_token_counts.items() for (round_id, round_counts) in
		 counts.round_counts_by_round_id()), key=lambda result: result[1])


def trim_empty_tail_rounds(dyad_id: Any, referent_token_counts: Dict[int, ReferentTokenTypeDatum]):
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
			trimmed_referent_token_counts = ReferentTokenTypeDatum(trimmed_round_counts)
			referent_token_counts[last_referent_id] = trimmed_referent_token_counts

	new_round_count = find_last_round_id(referent_token_counts)[1]
	if old_round_count > new_round_count:
		print("Trimmed {} empty round(s) from session \"{}\".".format(old_round_count - new_round_count, dyad_id),
			  file=sys.stderr)


def zip_game_round_utterances(game_rounds: Iterator[game_events.GameRound], utts: Sequence[utterances.Utterance]) -> \
		Iterator[Tuple[game_events.GameRound, Iterator[utterances.Utterance]]]:
	current_round = next(game_rounds)
	for next_round in game_rounds:
		current_round_start_time = current_round.start_time
		next_round_start_time = next_round.start_time
		current_round_utts = re_token_type_counts.game_round_utterances(current_round_start_time, next_round_start_time,
																		utts)
		yield current_round, current_round_utts
		current_round = next_round

	current_round_start_time = current_round.start_time
	current_round_utts = re_token_type_counts.game_round_utterances(current_round_start_time, float('inf'),
																	utts)
	yield current_round, current_round_utts


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
	referent_token_counter = ReferentTokenCounter(utterances.SegmentUtteranceFactory(),
												  re_token_type_counts.FilteringTokenTypeCounter(
													  lambda token: token in token_groups.keys()))
	referent_token_counts = referent_token_counter(named_sessions)
	printer = TokenTypeDataPrinter(args.strict)
	printer(referent_token_counts.items(), outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
