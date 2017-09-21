#!/usr/bin/env python3

import argparse
import re
import statistics
import sys
from decimal import Decimal
from typing import Any, FrozenSet, Mapping, Iterable, Tuple

import coreference_chain_overlap
import re_token_type_counts
import referent_token_type_counts
import utterances
from session_data import walk_session_data
from token_groups import read_token_group_dict

COL_DELIM = '\t'


class TokenTypeDataPrinter(object):
	@staticmethod
	def __create_first_row(dyad_id, entity_id, sequence_order, round_id, current_round_total_tokens,
						   current_round_token_types):
		mean_previous_token_count = coreference_chain_overlap.NULL_PREVIOUS_MEAN_COUNT_VALUE
		token_type_overlap = coreference_chain_overlap.NULL_TOKEN_TYPE_OVERLAP_VALUE
		overlap_ratio = coreference_chain_overlap.NULL_TOKEN_TYPE_OVERLAP_VALUE
		current_round_length_drop = coreference_chain_overlap.NULL_TOKEN_LENGTH_DROP_VALUE
		return (dyad_id, str(entity_id), str(sequence_order), str(round_id), str(current_round_total_tokens),
				str(current_round_total_tokens), str(mean_previous_token_count),
				str(current_round_length_drop), str(len(current_round_token_types)),
				str(len(current_round_token_types)),
				str(token_type_overlap), str(overlap_ratio))

	def __init__(self, strict: bool):
		self.strict = strict

	def __call__(self, session_referent_token_counts: Iterable[
		Tuple[Any, Mapping[int, referent_token_type_counts.CoreferenceChainTokenCountDatum]]], outfile):
		print(COL_DELIM.join(
			("DYAD", "ENTITY", "SEQUENCE_ORDER", "ROUND", "ROUND_TOKENS", "CUMULATIVE_TOKENS", "PREVIOUS_MEAN_TOKENS",
			 "LENGTH_DROP",
			 "ROUND_TYPES", "CUMULATIVE_TYPES", "OVERLAPPING_TYPES",
			 "OVERLAPPING_TYPE_RATIO")), file=outfile)

		ordered_session_referent_token_counts = sorted(session_referent_token_counts, key=lambda item: item[0])
		for dyad_id, referent_token_counts in ordered_session_referent_token_counts:
			for entity_id, entity_token_counts in sorted(referent_token_counts.items(), key=lambda item: item[0]):
				enumerated_round_counts = enumerate(entity_token_counts.round_counts_by_round_id(), start=1)
				sequence_order, (round_id, round_token_counts) = next(enumerated_round_counts)
				current_round_token_counts = round_token_counts.round_data.relevant_tokens
				current_round_total_tokens = Decimal(current_round_token_counts.total_token_count())
				current_round_token_types = frozenset(current_round_token_counts.token_types)
				print(COL_DELIM.join(
					self.__create_first_row(dyad_id, entity_id, sequence_order, round_id, current_round_total_tokens,
											current_round_token_types)),
					file=outfile)
				previous_round_token_types = current_round_token_types
				previous_round_total_tokens = [current_round_total_tokens]

				for (sequence_order, (round_id, round_token_counts)) in enumerated_round_counts:
					current_round_token_counts = round_token_counts.round_data.relevant_tokens
					current_round_total_tokens = current_round_token_counts.total_token_count()
					current_round_token_types = frozenset(current_round_token_counts.token_types)
					row = self.__create_row(dyad_id, entity_id, sequence_order, round_id, current_round_total_tokens,
											current_round_token_types, previous_round_token_types,
											previous_round_total_tokens)
					print(COL_DELIM.join(row), file=outfile)

					previous_round_token_types = current_round_token_types
					previous_round_total_tokens.append(current_round_total_tokens)

	def __create_row(self, dyad_id: str, entity_id: int, sequence_order: int, round_id,
					 current_round_total_tokens: Decimal,
					 current_round_token_types: FrozenSet[str], previous_round_token_types: FrozenSet[str],
					 previous_round_total_tokens: Iterable[Decimal]) -> Tuple[str, ...]:
		unified_token_types = current_round_token_types.union(
			previous_round_token_types)
		overlapping_token_types = current_round_token_types.intersection(
			previous_round_token_types)
		cumulative_token_count = sum(previous_round_total_tokens) + current_round_total_tokens
		mean_previous_token_count = statistics.mean(previous_round_total_tokens)
		token_type_overlap = len(overlapping_token_types)
		try:
			overlap_ratio = coreference_chain_overlap.token_type_overlap_ratio(token_type_overlap,
																			   len(unified_token_types))
			current_round_length_drop = coreference_chain_overlap.length_drop(current_round_total_tokens,
																			  mean_previous_token_count)
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
				overlap_ratio = coreference_chain_overlap.NULL_TOKEN_TYPE_OVERLAP_VALUE
				current_round_length_drop = coreference_chain_overlap.NULL_TOKEN_LENGTH_DROP_VALUE

		return (dyad_id, str(entity_id), str(sequence_order), str(round_id), str(current_round_total_tokens),
				str(cumulative_token_count), str(mean_previous_token_count),
				str(current_round_length_drop), str(len(current_round_token_types)), str(len(unified_token_types)),
				str(token_type_overlap),
				str(overlap_ratio))


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
		token_groups = read_token_group_dict(token_group_file_path,
											 lambda group: group_pattern.match(group) is not None)
	else:
		token_groups = read_token_group_dict(token_group_file_path)
	print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

	named_sessions = walk_session_data(args.inpaths)
	outfile = sys.stdout
	referent_token_counter = referent_token_type_counts.CoreferenceChainTokenCounter(
		utterances.TokenSequenceFactory(),
		re_token_type_counts.FilteringTokenCounter(
			lambda
				token: token in token_groups.keys()))
	referent_token_counts = referent_token_counter(named_sessions)
	printer = TokenTypeDataPrinter(args.strict)
	printer(referent_token_counts.items(), outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
