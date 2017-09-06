#!/usr/bin/env python3

import argparse
import sys

from game_events import create_game_rounds, read_events
from re_token_group_counts import read_token_group_dict
from session_data import walk_session_data
from utterances import SegmentUtteranceFactory, UtteranceTimes, dialogue_utt_str_repr, read_segments

COL_DELIM = '\t'


def __create_argparser():
	result = argparse.ArgumentParser(description="Count frequencies of referring token groups.")
	result.add_argument("token_group_file", metavar="path",
						help="The path to the token group mapping file to use.")
	result.add_argument("inpaths", metavar="path", nargs='+',
						help="The paths to search for sessions to process.")
	result.add_argument("-r", "--round-split", metavar="count", type=int,
						help="When this option is supplied, each session is split into half, with the first half comprising this many game rounds.")
	return result


if __name__ == "__main__":
	args = __create_argparser().parse_args()
	print(args)
	token_group_file_path = args.token_group_file
	print("Reading token groups from \"{}\".".format(token_group_file_path), file=sys.stderr)
	token_groups = read_token_group_dict(token_group_file_path)
	print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

	seg_utt_factory = SegmentUtteranceFactory()

	for indir, session in walk_session_data(args.inpaths):
		print("Processing session directory \"{}\".".format(indir), file=sys.stderr)
		events = tuple(read_events(session))
		print("Read {} event(s).".format(len(events)), file=sys.stderr)

		segments = read_segments(session.utts)
		utts = seg_utt_factory(segments)
		utts_by_time = UtteranceTimes(utts)

		idxed_game_rounds = iter(enumerate(create_game_rounds(events)))
		round_idx, first_game_round = next(idxed_game_rounds)
		current_round_start_time = first_game_round.start_time
		for round_idx, next_round in idxed_game_rounds:
			next_round_start_time = next_round.start_time
			current_round_utts = utts_by_time.segments_between(current_round_start_time,
															   next_round_start_time)
			diag_utt_repr = dialogue_utt_str_repr(current_round_utts)
			print(COL_DELIM.join((str(round_idx), diag_utt_repr)))

			current_round_start_time = next_round_start_time
