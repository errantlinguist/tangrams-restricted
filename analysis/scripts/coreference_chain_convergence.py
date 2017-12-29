#!/usr/bin/env python3

"""
Calculates mean token-type overlap for each coreference chain sequence ordinality in each chain for a given referent.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import csv
import logging
import sys
from decimal import Decimal
from enum import Enum, unique
from typing import FrozenSet, Iterable, Optional

import numpy as np
import pandas as pd

import alignment_metrics
import session_data as sd
import utterances

ZERO_DECIMAL = Decimal("0")


@unique
class TokenTypeSetDataFrameColumn(Enum):
	DYAD = "DYAD"
	TOKEN_TYPES = "TOKEN_TYPES"


class SessionRoundTokenTypeSetDataFrameFactory(object):

	@staticmethod
	def __create_token_type_set(token_seq: Iterable[str]) -> FrozenSet[str]:
		result = frozenset(token_seq)
		assert result
		return result

	def __init__(self, utt_reader: Optional[utterances.UtteranceTabularDataReader] = None):
		self.utt_reader = utterances.UtteranceTabularDataReader() if utt_reader is None else utt_reader

	def __call__(self, session_data: sd.SessionData) -> pd.DataFrame:
		session_name = session_data.name
		print("Reading events and utterances for \"{}\".".format(session_name), file=sys.stderr)
		events_df = session_data.read_events()
		events_df[TokenTypeSetDataFrameColumn.DYAD.value] = session_name

		orig_event_row_count = events_df.shape[0]
		events_df = events_df.loc[(events_df[sd.EventDataColumn.REFERENT_ENTITY.value] == True) & (
				events_df[sd.EventDataColumn.EVENT_NAME.value] == "nextturn.request")]
		events_df_shape = events_df.shape
		logging.debug("Removed %d non-referent, non new-turn-request entity rows; New shape is %s.",
					  orig_event_row_count - events_df_shape[0], events_df_shape)
		utts_df = self.utt_reader(session_data.utts)
		orig_utts_df_row_count = utts_df.shape[0]
		utts_df = utts_df.loc[utts_df[utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value].str.len() > 0]
		utts_df_shape = utts_df.shape
		logging.debug("Removed %s empty utterances; New shape is %s.", orig_utts_df_row_count - utts_df_shape[0],
					  utts_df_shape)
		utts_df[TokenTypeSetDataFrameColumn.TOKEN_TYPES.value] = utts_df[
			utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value].transform(self.__create_token_type_set)

		# Do a left-merge with the events dataframe on the left so that utterances without events (e.g. utterances in the pre-game round "0") are not included
		result = events_df.merge(utts_df, left_on=sd.EventDataColumn.ROUND_ID.value,
								 right_on=utterances.UtteranceTabularDataColumn.ROUND_ID.value)
		assert result.loc[result[sd.EventDataColumn.ROUND_ID.value] < 1].empty
		return result


@unique
class TokenTypeOverlapColumn(Enum):
	COREF_SEQ_ORDER = "COREF_SEQ_ORDER"
	PRECEDING_UTT_START_TIME = "PRECEDING_UTT_START_TIME"
	PRECEDING_TOKEN_TYPES = "PRECEDING_TOKEN_TYPES"
	TOKEN_TYPE_OVERLAP = "TOKEN_TYPE_OVERLAP"


class ReferentIndividualTokenTypeOverlapCalculator(object):

	@staticmethod
	def __token_type_overlap(utt: pd.Series) -> Decimal:
		preceding_token_types = utt[TokenTypeOverlapColumn.PRECEDING_TOKEN_TYPES.value]
		if pd.isnull(preceding_token_types):
			result = ZERO_DECIMAL
		else:
			token_types = utt[TokenTypeSetDataFrameColumn.TOKEN_TYPES.value]
			result = alignment_metrics.token_type_overlap_ratio(token_types, preceding_token_types)
		return result

	def __call__(self, df: pd.DataFrame) -> pd.DataFrame:
		"""
		Calculates token-type overlaps over multiple sessions.

		:param df: The DataFrame including rows for all sessions.
		:return: A copy of the DataFrame with token-type data added.
		"""
		result = df.copy(deep=False)
		# Calculate token-type overlap for each chain of reference for each entity and each speaker in each session
		session_speaker_ref_utts = result.groupby((TokenTypeSetDataFrameColumn.DYAD.value,
												   sd.EventDataColumn.ENTITY_ID.value,
												   utterances.UtteranceTabularDataColumn.SPEAKER_ID.value),
												  as_index=False)
		result[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value] = session_speaker_ref_utts.cumcount() + 1
		result[TokenTypeOverlapColumn.PRECEDING_UTT_START_TIME.value] = session_speaker_ref_utts[
			utterances.UtteranceTabularDataColumn.START_TIME.value].shift()
		result[TokenTypeOverlapColumn.PRECEDING_TOKEN_TYPES.value] = session_speaker_ref_utts[
			TokenTypeSetDataFrameColumn.TOKEN_TYPES.value].shift()
		result[TokenTypeOverlapColumn.TOKEN_TYPE_OVERLAP.value] = result.apply(self.__token_type_overlap,
																			   axis=1).transform(np.longfloat)
		return result


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Calculates mean token-type overlap for each coreference chain sequence ordinality in each chain for a given referent.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The paths to search for sessions to process.")

	metric_types = result.add_mutually_exclusive_group(required=True)
	metric_types.add_argument("-i", "--individual", action='store_true',
							  help="Calculate token-type overlap for individual speakers with themselves.")
	metric_types.add_argument("-o", "--other", action='store_true',
							  help="Calculate token-type overlap for individual speakers with their interlocutors.")

	return result


def __main(args):
	inpaths = args.inpaths
	print("Looking for session data underneath {}.".format(inpaths), file=sys.stderr)
	df_factory = SessionRoundTokenTypeSetDataFrameFactory()
	session_utt_df = pd.concat(df_factory(session_data) for _, session_data in sd.walk_session_data(inpaths))
	print("DF shape is {}; {} unique dyad(s).".format(session_utt_df.shape,
													  session_utt_df[
														  TokenTypeSetDataFrameColumn.DYAD.value].nunique()),
		  file=sys.stderr)
	# Ensure that rows are sorted in order of which round they are for and their chronological ordering withing each round
	session_utt_df.sort_values(
		by=[sd.EventDataColumn.ROUND_ID.value, utterances.UtteranceTabularDataColumn.START_TIME.value,
			utterances.UtteranceTabularDataColumn.END_TIME.value], inplace=True)

	if args.individual:
		ref_overlap_calculator = ReferentIndividualTokenTypeOverlapCalculator()
	else:
		raise AssertionError("Logic error")

	session_utt_df = ref_overlap_calculator(session_utt_df)
	# session_utt_df.to_csv(sys.stdout, sep=csv.excel_tab.delimiter, encoding="utf-8")
	coref_seq_orders = session_utt_df[
		[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value, TokenTypeOverlapColumn.TOKEN_TYPE_OVERLAP.value]].groupby(
		by=TokenTypeOverlapColumn.COREF_SEQ_ORDER.value, as_index=False)
	aggs = coref_seq_orders.agg(["mean", "std", "sem"])
	aggs.columns = aggs.columns.droplevel(0)
	aggs.to_csv(sys.stdout, sep=csv.excel_tab.delimiter, encoding="utf-8", index_label="seq")


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
