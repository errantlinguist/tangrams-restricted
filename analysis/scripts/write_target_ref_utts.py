#!/usr/bin/env python3

"""
Reads in event and utterance files for a directory of sessions and prints the utterance information alongside the target referent entity information on the same row to the standard output stream.

Processes the utterances in the following order:
1. Removes empty utterances, i.e. those only containing metalanguage tokens
2. Removes non-instructor utterances
3. Merges all utterances from a single speaker in a given round into one
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import csv
import itertools
import logging
import sys
from typing import Any, Iterable, Sequence, Tuple, TypeVar

import pandas as pd

import session_data as sd
import utterances

DYAD_COL_NAME = "DYAD"
OUTPUT_FILE_DIALECT = csv.excel_tab
OUTPUT_FILE_ENCODING = "utf-8"

T = TypeVar("T")


class SessionRoundTokenTypeSetDataFrameFactory(object):

	@staticmethod
	def __create_summary_row(desc: str, datum: Tuple[int, ...]) -> Tuple[Any, ...]:
		return tuple(itertools.chain((desc,), datum))

	def __init__(self, utt_reader: utterances.UtteranceTabularDataReader):
		self.utt_reader = utt_reader
		self.orig_utt_df_shape_union = (0, 0)
		self.nonempty_utt_df_shape_union = (0, 0)
		self.instructor_utt_df_shape_union = (0, 0)
		self.speaker_utt_df_shape_union = (0, 0)

	def __call__(self, session_data: sd.SessionData) -> pd.DataFrame:
		session_name = session_data.name
		print("Reading events and utterances for \"{}\".".format(session_name), file=sys.stderr)
		events_df = session_data.read_events()
		events_df = remove_nonreferent_entity_utts(events_df)
		events_df[DYAD_COL_NAME] = session_name

		utts_df = self.__read_utts(session_data.utts)
		utts_df = self.__remove_empty_utts(utts_df)
		utts_df = self.__remove_noninstructor_utts(utts_df)
		utts_df = self.__merge_speaker_utts(utts_df)

		# Do a left-merge with the events dataframe on the left so that utterances without events (e.g. utterances in the pre-game round "0") are not included
		result = events_df.merge(utts_df, how="left", left_on=sd.EventDataColumn.ROUND_ID.value,
								 right_on=utterances.UtteranceTabularDataColumn.ROUND_ID.value)
		assert result.loc[result[sd.EventDataColumn.ROUND_ID.value] < 1].empty
		return result

	def create_summary_rows(self):
		result = (
			("DESC", "ROWS", "COLS"),
			self.__create_summary_row("Original DF", self.orig_utt_df_shape_union),
			self.__create_summary_row("After removing empty utterances", self.nonempty_utt_df_shape_union),
			self.__create_summary_row("After removing non-instructor utterances", self.instructor_utt_df_shape_union),
			self.__create_summary_row("After merging speaker utterances", self.speaker_utt_df_shape_union),
		)
		assert len(frozenset(len(row) for row in result)) == 1
		return result

	def __merge_speaker_utts(self, utts_df: pd.DataFrame) -> pd.DataFrame:
		logging.debug("Merging speaker utterances.")
		old_shape = utts_df.shape
		result = utterances.merge_speaker_utts(utts_df)
		new_shape = result.shape
		logging.debug("Row count after merging speaker utterances: %d; Diff: %d", new_shape[0],
					  old_shape[0] - new_shape[0])
		self.speaker_utt_df_shape_union = shape_union(self.speaker_utt_df_shape_union, new_shape)
		return result

	def __read_utts(self, infile_path: str):
		logging.debug("Reading utterance data from \"%s\".", infile_path)
		result = self.utt_reader(infile_path)
		shape = result.shape
		logging.debug("Read utterances with shape %s.", shape)
		self.orig_utt_df_shape_union = shape_union(self.orig_utt_df_shape_union, shape)
		return result

	def __remove_empty_utts(self, utts_df: pd.DataFrame) -> pd.DataFrame:
		logging.debug("Removing empty utterances from dataframe.")
		old_shape = utts_df.shape
		result = utts_df.loc[
			utts_df[utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value].str.len() > 0]
		new_shape = result.shape
		logging.debug("Row count after removing empty utterances: %d; Diff: %d", new_shape[0],
					  old_shape[0] - new_shape[0])
		self.nonempty_utt_df_shape_union = shape_union(self.nonempty_utt_df_shape_union, new_shape)
		return result

	def __remove_noninstructor_utts(self, utts_df: pd.DataFrame) -> pd.DataFrame:
		logging.debug("Removing non-instructor utterances from dataframe.")
		old_shape = utts_df.shape
		result = utts_df.loc[
			utts_df[utterances.UtteranceTabularDataColumn.DIALOGUE_ROLE.value] == "INSTRUCTOR"]
		new_shape = result.shape
		logging.debug("Row count after removing non-instructor utterances: %d; Diff: %d", new_shape[0],
					  old_shape[0] - new_shape[0])
		self.instructor_utt_df_shape_union = shape_union(self.instructor_utt_df_shape_union, new_shape)
		return result


def remove_nonreferent_entity_utts(events_df: pd.DataFrame) -> pd.DataFrame:
	logging.debug("Removing non-referent, non-new-turn-request rows.")
	orig_shape = events_df.shape
	result = events_df.loc[(events_df[sd.EventDataColumn.REFERENT_ENTITY.value] == True) & (
			events_df[sd.EventDataColumn.EVENT_NAME.value] == "nextturn.request")]
	new_shape = result.shape
	logging.debug("Removed %d non-referent, non new-turn-request entity rows; New shape is %s.",
				  orig_shape[0] - new_shape[0], new_shape)
	return result


def shape_union(s1: Tuple[int, int], s2: Tuple[int, int]) -> Tuple[int, int]:
	assert len(s1) == 2
	assert len(s2) == 2
	return s1[0] + s2[0], max(s1[1], s2[1])


def sort_cols(df: pd.DataFrame) -> pd.DataFrame:
	partial_ordering = (
		DYAD_COL_NAME, sd.EventDataColumn.ROUND_ID.value, sd.EventDataColumn.SCORE.value,
		sd.EventDataColumn.EVENT_ID.value,
		sd.EventDataColumn.EVENT_NAME.value, sd.EventDataColumn.EVENT_TIME.value, sd.EventDataColumn.SUBMITTER.value,
		sd.EventDataColumn.ENTITY_ID.value, utterances.UtteranceTabularDataColumn.SPEAKER_ID.value,
		utterances.UtteranceTabularDataColumn.DIALOGUE_ROLE.value,
		utterances.UtteranceTabularDataColumn.START_TIME.value,
		utterances.UtteranceTabularDataColumn.END_TIME.value,
		utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value)
	# NOTE: "reindex_axis(..)" should be used instead of "reindex(..)" for older versions of pandas
	return df.reindex(sorted(df.columns, key=lambda col_name: __element_order(col_name, partial_ordering)), axis=1,
						   copy=False)


def __create_str_repr(token_seq: Iterable[str]) -> str:
	return " ".join(token_seq)


def __element_order(elem: T, ordering: Sequence[T]) -> int:
	try:
		result = ordering.index(elem)
	except ValueError:
		result = len(ordering)
	return result


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Reads in event and utterance files for a directory of sessions and prints the utterance information alongside the target referent entity information on the same row to the standard output stream.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The paths to search for sessions to process.")
	return result


def __main(args):
	print("Will remove non-instructor utterances and concatenate speaker utterances.", file=sys.stderr)
	df_factory = SessionRoundTokenTypeSetDataFrameFactory(utterances.UtteranceTabularDataReader())

	inpaths = args.inpaths
	print("Looking for session data underneath {}.".format(inpaths), file=sys.stderr)
	session_utt_df = pd.concat(df_factory(session_data) for _, session_data in sd.walk_session_data(inpaths))
	print("DF shape is {}; {} unique dyad(s).".format(session_utt_df.shape,
													  session_utt_df[
														  DYAD_COL_NAME].nunique()),
		  file=sys.stderr)
	print("Utterance dataframe summary:", file=sys.stderr)
	summary_writer = csv.writer(sys.stderr, dialect=OUTPUT_FILE_DIALECT)
	summary_writer.writerows(df_factory.create_summary_rows())

	session_utt_df[utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value] = session_utt_df[
		utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value].transform(__create_str_repr)
	# Ensure that rows are sorted in order of which round they are for and their chronological ordering withing each round, sorting finally by dialogue role as a tiebreaker
	session_utt_df.sort_values(
		by=[DYAD_COL_NAME, sd.EventDataColumn.ROUND_ID.value, utterances.UtteranceTabularDataColumn.START_TIME.value,
			utterances.UtteranceTabularDataColumn.END_TIME.value,
			utterances.UtteranceTabularDataColumn.DIALOGUE_ROLE.value], inplace=True)
	session_utt_df = sort_cols(session_utt_df)
	session_utt_df.to_csv(sys.stdout, sep=OUTPUT_FILE_DIALECT.delimiter, encoding=OUTPUT_FILE_ENCODING, index=False)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
