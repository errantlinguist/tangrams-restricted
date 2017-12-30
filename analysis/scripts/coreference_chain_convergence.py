#!/usr/bin/env python3

"""
Calculates mean token type overlap for each coreference chain sequence ordinality in each chain for a given referent.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import csv
import re
import sys
from enum import Enum, unique
from typing import FrozenSet

import numpy as np
import pandas as pd

import alignment_metrics
import session_data as sd
import utterances
import write_target_ref_utts

INFILE_DTYPES = {**sd.EVENT_FILE_DTYPES, utterances.UtteranceTabularDataColumn.DIALOGUE_ROLE.value: "category",
				 utterances.UtteranceTabularDataColumn.SPEAKER_ID.value: "category"}
OUTFILE_CSV_DIALECT = csv.excel_tab
OUTFILE_ENCODING = "utf-8"


@unique
class TokenTypeOverlapColumn(Enum):
	COREF_SEQ_ORDER = "COREF_SEQ_ORDER"
	PRECEDING_UTT_START_TIME = "PRECEDING_UTT_START_TIME"
	PRECEDING_TOKEN_TYPES = "PRECEDING_TOKEN_TYPES"
	TOKEN_TYPE_OVERLAP = "TOKEN_TYPE_OVERLAP"


class ReferentIndividualTokenTypeOverlapCalculator(object):

	@staticmethod
	def __token_type_overlap(utt: pd.Series) -> np.longfloat:
		preceding_token_types = utt[TokenTypeOverlapColumn.PRECEDING_TOKEN_TYPES.value]
		if pd.isnull(preceding_token_types):
			result = np.longfloat(0.0)
		else:
			token_types = utt["TOKEN_TYPES"]
			result = alignment_metrics.token_type_overlap_ratio(token_types, preceding_token_types)
		return result

	# noinspection PyTypeChecker,PyUnresolvedReferences
	def __call__(self, df: pd.DataFrame) -> pd.DataFrame:
		"""
		Calculates token type overlaps over multiple sessions.

		:param df: The DataFrame including rows for all sessions.
		:return: A copy of the DataFrame with token type data added.
		"""
		result = df.copy(deep=False)
		# Calculate token type overlap for each chain of reference for each entity and each speaker in each session
		session_speaker_ref_utts = result.groupby(("DYAD",
												   sd.EventDataColumn.ENTITY_ID.value,
												   utterances.UtteranceTabularDataColumn.SPEAKER_ID.value),
												  as_index=False, sort=False)
		result[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value] = session_speaker_ref_utts.cumcount() + 1
		result[TokenTypeOverlapColumn.PRECEDING_UTT_START_TIME.value] = session_speaker_ref_utts[
			utterances.UtteranceTabularDataColumn.START_TIME.value].shift()
		result[TokenTypeOverlapColumn.PRECEDING_TOKEN_TYPES.value] = session_speaker_ref_utts[
			"TOKEN_TYPES"].shift()
		result[TokenTypeOverlapColumn.TOKEN_TYPE_OVERLAP.value] = result.apply(self.__token_type_overlap,
																			   axis=1)
		return result


class TokenTypeSetFactory(object):
	EMPTY_SET = frozenset()
	TOKEN_DELIMITER_PATTERN = re.compile("\\s+")

	def __init__(self):
		self.token_seq_singletons = {}

	def __call__(self, text: str) -> FrozenSet[str]:
		if text:
			try:
				result = self.token_seq_singletons[text]
			except KeyError:
				# NOTE: The tokens have already been properly tokenized using NLTK during creating of the event-utterance file
				tokens = self.TOKEN_DELIMITER_PATTERN.split(text)
				result = frozenset(sys.intern(token) for token in tokens)
				self.token_seq_singletons[result] = result
		else:
			result = self.EMPTY_SET

		return result


def read_event_utts(infile_path: str) -> pd.DataFrame:
	dialect = write_target_ref_utts.OUTPUT_FILE_DIALECT
	converters = {utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value: TokenTypeSetFactory()}
	result = pd.read_csv(infile_path, dialect=dialect, sep=dialect.delimiter,
						 float_precision="round_trip", converters=converters, dtype=INFILE_DTYPES)
	result.rename({utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value: "TOKEN_TYPES"}, axis=1, inplace=True)
	return result


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Calculates mean token type overlap for each coreference chain sequence ordinality in each chain for a given referent.")
	result.add_argument("infile", metavar="INFILE",
						help="The combined events and utterance data file to read.")
	result.add_argument("-d", "--dump", action='store_true',
						help="Dumps all the dataframe data to file rather than just the aggregates for the token type overlap.")
	result.add_argument("-ui", "--utts-instructor", dest="utts_instructor", action='store_true',
						help="Use only utterances from the instructor for each given round.")

	metric_types = result.add_mutually_exclusive_group(required=True)
	metric_types.add_argument("-ms", "--metric-self", dest="metric_self", action='store_true',
							  help="Calculate token type overlap for individual speakers with themselves.")
	metric_types.add_argument("-mo", "--metric-other", dest="metric_other", action='store_true',
							  help="Calculate token type overlap for individual speakers with their interlocutors.")
	return result


def __main(args):
	if args.metric_self:
		print("Calculating self overlap.", file=sys.stderr)
		ref_overlap_calculator = ReferentIndividualTokenTypeOverlapCalculator()
	else:
		raise AssertionError("Logic error")

	infile = args.infile
	print("Reading \"{}\".".format(infile), file=sys.stderr)
	session_utt_df = read_event_utts(infile)
	print("DF shape is {}; {} unique dyad(s).".format(session_utt_df.shape,
													  session_utt_df[
														  "DYAD"].nunique()),
		  file=sys.stderr)
	if args.utts_instructor:
		print("Removing non-instructor utterances from dataframe.", file=sys.stderr)
		session_utt_df = session_utt_df.loc[
			session_utt_df[utterances.UtteranceTabularDataColumn.DIALOGUE_ROLE.value] == "INSTRUCTOR"]

	# Ensure that rows are sorted in order of which round they are for and their chronological ordering withing each round, sorting finally by dialogue role as a tiebreaker
	session_utt_df.sort_values(
		by=[sd.EventDataColumn.ROUND_ID.value, utterances.UtteranceTabularDataColumn.START_TIME.value,
			utterances.UtteranceTabularDataColumn.END_TIME.value,
			utterances.UtteranceTabularDataColumn.DIALOGUE_ROLE.value], inplace=True)

	session_utt_df = ref_overlap_calculator(session_utt_df)
	session_utt_df.sort_values(
		["DYAD", sd.EventDataColumn.ENTITY_ID.value, utterances.UtteranceTabularDataColumn.SPEAKER_ID.value,
		 TokenTypeOverlapColumn.COREF_SEQ_ORDER.value,
		 sd.EventDataColumn.ROUND_ID.value], inplace=True)
	if args.dump:
		session_utt_df.to_csv(sys.stdout, sep=OUTFILE_CSV_DIALECT.delimiter, encoding=OUTFILE_ENCODING, index=False)
	else:
		coref_seq_orders = session_utt_df[
			[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value, TokenTypeOverlapColumn.TOKEN_TYPE_OVERLAP.value]].groupby(
			TokenTypeOverlapColumn.COREF_SEQ_ORDER.value, as_index=False, sort=False)
		aggs = coref_seq_orders.agg(["mean", "std", "sem"])
		aggs.columns = aggs.columns.droplevel(0)
		aggs.to_csv(sys.stdout, sep=OUTFILE_CSV_DIALECT.delimiter, encoding=OUTFILE_ENCODING, index_label="seq")


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
