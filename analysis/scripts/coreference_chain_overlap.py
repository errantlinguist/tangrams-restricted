#!/usr/bin/env python3

import argparse
import re
import statistics
import sys
from typing import Any, Dict, Iterable, Tuple

import re_token_type_counts
import referent_token_type_counts
import utterances
from session_data import walk_session_data
from token_groups import read_token_group_dict

COL_DELIM = '\t'
NULL_VALUE_REPR = '?'


class TokenTypeDataPrinter(object):
	@staticmethod
	def __create_first_row(dyad_id, entity_id, sequence_order, round_id, current_round_total_tokens,
						   current_round_token_types):
		mean_previous_token_count = NULL_VALUE_REPR
		token_type_overlap = 0
		overlap_ratio = NULL_VALUE_REPR
		current_round_length_drop = NULL_VALUE_REPR
		return (dyad_id, str(entity_id), str(sequence_order), str(round_id), str(current_round_total_tokens),
				str(current_round_total_tokens), mean_previous_token_count,
				current_round_length_drop, str(len(current_round_token_types)), str(len(current_round_token_types)),
				str(token_type_overlap), overlap_ratio)

	def __init__(self, strict: bool):
		self.strict = strict

	def __call__(self, session_referent_token_counts: Iterable[
		Tuple[Any, Dict[int, referent_token_type_counts.ReferentTokenTypeDatum]]], outfile):
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
				current_round_total_tokens = current_round_token_counts.total_token_count()
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

	def __create_row(self, dyad_id, entity_id, sequence_order, round_id, current_round_total_tokens,
					 current_round_token_types, previous_round_token_types, previous_round_total_tokens: Iterable[int]):
		unified_token_types = current_round_token_types.union(
			previous_round_token_types)
		overlapping_token_types = current_round_token_types.intersection(
			previous_round_token_types)
		cumulative_token_count = sum(previous_round_total_tokens) + current_round_total_tokens
		mean_previous_token_count = statistics.mean(previous_round_total_tokens)
		token_type_overlap = len(overlapping_token_types)
		try:
			overlap_ratio = token_type_overlap / len(unified_token_types)
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
				overlap_ratio = float('nan')
				current_round_length_drop = overlap_ratio

		return (dyad_id, str(entity_id), str(sequence_order), str(round_id), str(current_round_total_tokens),
				str(cumulative_token_count), str(mean_previous_token_count),
				str(current_round_length_drop), str(len(current_round_token_types)), str(len(unified_token_types)),
				str(token_type_overlap),
				str(overlap_ratio))


def length_drop(current_total_tokens: int, mean_previous_token_count: int) -> float:
	"""
	See Aina, L. et al (2017) "Referring Expressions and Communicative Success in Task-oriented Dialogues", p. 9.

	:param current_total_tokens: The total tokens uttered in the current unit under examination (e.g. either a single utterance or an entire sub-dialogue).
	:param mean_previous_token_count:  The mean tokens uttered of the previous units under examination (e.g. either a single utterance or an entire sub-dialogue).
	:return: A metric between -1 and 1.
	"""
	return (mean_previous_token_count - current_total_tokens) / (mean_previous_token_count + current_total_tokens)


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
	referent_token_counter = referent_token_type_counts.ReferentTokenCounter(utterances.SegmentUtteranceFactory(),
																			 re_token_type_counts.FilteringTokenTypeCounter(
																				 lambda
																					 token: token in token_groups.keys()))
	referent_token_counts = referent_token_counter(named_sessions)
	printer = TokenTypeDataPrinter(args.strict)
	printer(referent_token_counts.items(), outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
