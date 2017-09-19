#!/usr/bin/env python3

import argparse
import re
import sys
from collections import defaultdict, deque
from decimal import Decimal
from typing import Callable, Generic, Iterable, Iterator, Sequence, Tuple, TypeVar

import coreference_chain_overlap
import game_events
import re_token_type_counts
import session_data
import session_data as sd
import token_groups as tg
import utterances

COL_DELIM = '\t'
DECIMAL_ONE = session_data.fetch_decimal_value("1")
DECIMAL_ZERO = session_data.fetch_decimal_value("0")

T = TypeVar('T')


class FeatureSpacePartitioner(Generic[T]):
	def __init__(self, partitions: int,
				 game_round_feature_value_extractor: Callable[[game_events.GameRound], Iterable[T]], min_value: T,
				 max_value: T):
		self.partitions = partitions
		self.game_round_feature_value_extractor = game_round_feature_value_extractor
		if min_value >= max_value:
			raise ValueError("Minimum value is {} but must be less than maximum value {}.".format(min_value, max_value))
		self.min_value = min_value
		self.max_value = max_value
		self.domain_size = self.max_value - self.min_value
		self.partition_size = self.domain_size / partitions

	def __call__(self,
				 referent_counts: Sequence[coreference_chain_overlap.ReferentCounts]):
		feature_derived_values = defaultdict(list)
		derived_features = self.__create_derived_features(referent_counts)
		for feature_value, derived_value in derived_features:
			feature_derived_values[feature_value].append(derived_value)

		ordered_feature_values = deque(feature_derived_values.keys())
		ordered_feature_values.sort()
		self.__ensure_in_ordered_domain(ordered_feature_values)

		for feature_value, derived_value in derived_features:
			print(COL_DELIM.join((str(feature_value), str(derived_value))))

	def __create_derived_features(self, referent_counts: Sequence[coreference_chain_overlap.ReferentCounts]) -> \
			Iterator[Tuple[T, Decimal]]:
		for referent_counts in referent_counts:
			round_relevant_token_type_overlap_ratios = coreference_chain_overlap.total_token_type_overlap_ratios(
				"AGGREGATED_SESSIONS", "RELEVANT_TOKENS", referent_counts,
				lambda round_counts: round_counts.total_counts.relevant_tokens, False)
			for round_id, game_round, relevant_token_type_overlap_ratio in round_relevant_token_type_overlap_ratios:
				game_round_key_feature_values = self.game_round_feature_value_extractor(game_round)
				for value in game_round_key_feature_values:
					yield value, relevant_token_type_overlap_ratio

	def __ensure_in_ordered_domain(self, ordered_feature_values: Sequence[T]):
		first_value = ordered_feature_values[0]
		if first_value < self.min_value:
			raise ValueError("First value is {} but minimum allowed is {}.".format(first_value, self.min_value))
		last_value = ordered_feature_values[len(ordered_feature_values) - 1]
		if last_value > self.max_value:
			raise ValueError("Last value is {} but maximum allowed is {}.".format(last_value, self.max_value))


def __create_argparser():
	result = argparse.ArgumentParser(
		description="Partition hue feature space into buckets which have the maximal average token overlap for each coreference chain involving entities with a hue inside the given feature space partition.")
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
	referent_token_counter = coreference_chain_overlap.ParticipantCoreferenceChainTokenCounter(
		utterances.TokenSequenceFactory(),
		re_token_type_counts.FilteringTokenCounter(
			lambda
				token: token in token_groups.keys()))
	session_entity_counts = referent_token_counter(named_sessions)

	all_referent_counts = tuple(round_counts for counts in session_entity_counts.values() for round_counts in
								counts.entity_referent_counts.values())
	print("Partitioning values for all {} coreference-chain count(s) from {} session(s).".format(
		len(all_referent_counts),
		len(session_entity_counts)), file=sys.stderr)
	partitioner = FeatureSpacePartitioner(4, __referent_hues, DECIMAL_ZERO, DECIMAL_ONE)
	partitioner(all_referent_counts)


def __referent_hues(game_round: game_events.GameRound) -> Iterator[session_data.DECIMAL_VALUE_TYPE]:
	return (entity.attr(session_data.DataColumn.HUE.name) for _, entity in game_round.initial_event.referent_entities)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
