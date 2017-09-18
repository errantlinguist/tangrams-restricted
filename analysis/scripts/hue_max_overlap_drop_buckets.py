#!/usr/bin/env python3

import argparse
import re
import sys
from typing import Sequence

import coreference_chain_overlap
import re_token_type_counts
import session_data as sd
import token_groups as tg
import utterances

COL_DELIM = '\t'


class FeatureSpacePartitioner(object):
	def __init__(self, partitions: int):
		self.partitions = partitions

	def __call__(self,
				 round_token_counts: Sequence[coreference_chain_overlap.RoundCounts]):
		for counts in round_token_counts:
			pass


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

	partitioner = FeatureSpacePartitioner(4)
	all_round_counts = tuple(round_counts for counts in session_entity_counts.values() for round_counts in
							 counts.entity_referent_counts.values())
	print("Partitioning all {} round(s) from {} session(s).".format(len(all_round_counts), len(session_entity_counts)),
		  file=sys.stderr)
	partitioner(all_round_counts)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
