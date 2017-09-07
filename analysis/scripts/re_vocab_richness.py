#!/usr/bin/env python3

import argparse
import sys
from decimal import Decimal
from typing import Dict, Iterable, Set
from collections import Counter

import utterances
from re_token_group_counts import TokenGroupDict, read_token_group_dict
from re_token_group_freqs import RoundTokenGroupCounter, game_round_start_end_times, read_round_start_times, game_round_utterances
from session_data import walk_session_data

COL_DELIM = '\t'
DYAD_ID_COL_NAME = "DYAD"
TOTAL_RESULTS_ROW_NAME = "TOTAL"


class SessionRoundTokenCounter(object):
	def __init__(self, seg_utt_factory : utterances.SegmentUtteranceFactory):
		self.seg_utt_factory = seg_utt_factory

	def __call__(self, named_sessions):
		result = {}
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
			round_start_end_times = tuple(game_round_start_end_times(iter(read_round_start_times(session))))
			round_count = len(round_start_end_times)
			print("Read {} game round(s).".format(round_count), file=sys.stderr)

			segments = utterances.read_segments(session.utts)
			utts = tuple(self.seg_utt_factory(segments))
			round_utts = (game_round_utterances(start_time, end_time, utts) for start_time, end_time in round_start_end_times)
			rount_token_counts = [_count_utt_tokens(utts) for utts in round_utts]
			result[dyad_id] = rount_token_counts

		return result

def _count_utt_tokens(utts : Iterable[utterances.Utterance]):
	result = Counter()
	for utt in utts:
		result.update(utt.content)
	return result

def __create_argparser():
	result = argparse.ArgumentParser(description="Count ratios of new token types occurring.")
	result.add_argument("token_group_file", metavar="TOKEN_GROUP_FILEPATH",
						help="The path to the token group mapping file to use.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The paths to search for sessions to process.")
	return result


def __get_item_key(item):
	return item[0]

class TokenTypeData(object):
	def __init__(self):
		self.group_tokens = {}


def new_token_ratios(round_token_group_counts : Iterable[Dict[str, int]]):
	total_group_counts = {}
	for round_token_group_count in round_token_group_counts:
		for group, count in round_token_group_count.items():
			try:
				total_count = total_group_counts[group]
			except KeyError:
				# The token has not been seen before
				pass



def __main(args):
	token_group_file_path = args.token_group_file
	print("Reading token groups from \"{}\".".format(token_group_file_path), file=sys.stderr)
	token_groups = read_token_group_dict(token_group_file_path)
	print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

	named_sessions = walk_session_data(args.inpaths)
	outfile = sys.stdout
	seg_utt_factory = utterances.SegmentUtteranceFactory(lambda token: token in token_groups.keys())
	token_counter = SessionRoundTokenCounter(seg_utt_factory)
	session_round_token_counts = token_counter(named_sessions)
	for dyad_id, round_token_counts in session_round_token_counts.items():
		print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
		print(round_token_counts)




if __name__ == "__main__":
	__main(__create_argparser().parse_args())
