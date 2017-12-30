#!/usr/bin/env python3

"""
Reads in event and utterance files for a directory of sessions and prints the utterance information alongside the target referent entity information on the same row to the standard output stream.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import csv
import logging
import sys
from typing import Callable

import pandas as pd

import session_data as sd
import utterances

DYAD_COL_NAME = "DYAD"


class SessionRoundTokenTypeSetDataFrameFactory(object):

	def __init__(self, utt_reader: utterances.UtteranceTabularDataReader,
				 postprocessor: Callable[[pd.DataFrame], pd.DataFrame]):
		self.utt_reader = utt_reader
		self.postprocessor = postprocessor

	def __call__(self, session_data: sd.SessionData) -> pd.DataFrame:
		session_name = session_data.name
		print("Reading events and utterances for \"{}\".".format(session_name), file=sys.stderr)
		events_df = session_data.read_events()
		events_df[DYAD_COL_NAME] = session_name

		orig_event_row_count = events_df.shape[0]
		events_df = events_df.loc[(events_df[sd.EventDataColumn.REFERENT_ENTITY.value] == True) & (
				events_df[sd.EventDataColumn.EVENT_NAME.value] == "nextturn.request")]
		events_df_shape = events_df.shape
		logging.debug("Removed %d non-referent, non new-turn-request entity rows; New shape is %s.",
					  orig_event_row_count - events_df_shape[0], events_df_shape)
		orig_utts_df = self.utt_reader(session_data.utts)
		logging.debug("Read utterances with shape %s.", orig_utts_df.shape)
		postprocessed_utts_df = self.postprocessor(orig_utts_df)
		logging.debug("Shape after post-processing is %s.", postprocessed_utts_df.shape)
		orig_utts_df_row_count = postprocessed_utts_df.shape[0]
		postprocessed_utts_df = postprocessed_utts_df.loc[
			postprocessed_utts_df[utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value].str.len() > 0]
		postprocessed_utts_df_shape = postprocessed_utts_df.shape
		logging.debug("Removed %s empty utterances; New shape is %s.",
					  orig_utts_df_row_count - postprocessed_utts_df_shape[0],
					  postprocessed_utts_df_shape)

		# Do a left-merge with the events dataframe on the left so that utterances without events (e.g. utterances in the pre-game round "0") are not included
		result = events_df.merge(postprocessed_utts_df, left_on=sd.EventDataColumn.ROUND_ID.value,
								 right_on=utterances.UtteranceTabularDataColumn.ROUND_ID.value)
		assert result.loc[result[sd.EventDataColumn.ROUND_ID.value] < 1].empty
		return result


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Reads in event and utterance files for a directory of sessions and prints the utterance information alongside the target referent entity information on the same row to the standard output stream.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The paths to search for sessions to process.")
	postprocessing = result.add_mutually_exclusive_group()
	postprocessing.add_argument("-c", "--merge-consecutive", dest="merge_consecutive", action='store_true',
								help="Concatenate consecutive utterances from the same speaker in a given round.")
	postprocessing.add_argument("-s", "--merge-speaker", dest="merge_speaker", action='store_true',
								help="Concatenate all utterances from the same speaker in a given round.")
	return result


def __main(args):
	if args.merge_consecutive:
		print("Will concatenate consecutive utterances.", file=sys.stderr)
		postprocessor = utterances.merge_consecutive_utts
	elif args.merge_speaker:
		print("Will concatenate speaker utterances.", file=sys.stderr)
		postprocessor = utterances.merge_speaker_utts
	else:
		postprocessor = lambda df: df
	df_factory = SessionRoundTokenTypeSetDataFrameFactory(utterances.UtteranceTabularDataReader(), postprocessor)

	inpaths = args.inpaths
	print("Looking for session data underneath {}.".format(inpaths), file=sys.stderr)
	session_utt_df = pd.concat(df_factory(session_data) for _, session_data in sd.walk_session_data(inpaths))
	print("DF shape is {}; {} unique dyad(s).".format(session_utt_df.shape,
													  session_utt_df[
														  DYAD_COL_NAME].nunique()),
		  file=sys.stderr)
	session_utt_df[utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value] = session_utt_df[
		utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value].transform(lambda token_seq: " ".join(token_seq))
	# Ensure that rows are sorted in order of which round they are for and their chronological ordering withing each round, sorting finally by dialogue role as a tiebreaker
	session_utt_df.sort_values(
		by=[DYAD_COL_NAME, sd.EventDataColumn.ROUND_ID.value, utterances.UtteranceTabularDataColumn.START_TIME.value,
			utterances.UtteranceTabularDataColumn.END_TIME.value, utterances.UtteranceTabularDataColumn.DIALOGUE_ROLE.value], inplace=True)
	session_utt_df.to_csv(sys.stdout, sep=csv.excel_tab.delimiter, encoding="utf-8", index=False)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
