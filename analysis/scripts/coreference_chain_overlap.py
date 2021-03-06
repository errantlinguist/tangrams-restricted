#!/usr/bin/env python3

"""
Calculates mean token type overlap for each coreference chain sequence ordinality in each chain for a given referent.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import csv
import logging
import re
import sys
from enum import Enum, unique
from typing import FrozenSet, Iterable, Mapping, Tuple

import numpy as np
import pandas as pd

from tangrams_analysis import alignment_metrics, session_data as sd, utterances
import write_target_ref_utts

INFILE_DTYPES = {**sd.EVENT_FILE_DTYPES, utterances.UtteranceTabularDataColumn.DIALOGUE_ROLE.value: "category",
				 utterances.UtteranceTabularDataColumn.SPEAKER_ID.value: "category"}
OUTFILE_CSV_DIALECT = csv.excel_tab
OUTFILE_ENCODING = "utf-8"

_EMPTY_SET = frozenset()
_EMPTY_SET_STR_REPR = ""


class BetweenSpeakerTokenTypeOverlapCalculator(object):

	@staticmethod
	def __prev_utts(utt_row: pd.Series, df: pd.DataFrame) -> pd.DataFrame:
		start_time_col_name = utterances.UtteranceTabularDataColumn.START_TIME.value
		start_time = utt_row[start_time_col_name]
		speaker_id_col_name = utterances.UtteranceTabularDataColumn.END_TIME.value
		speaker_id = utt_row[speaker_id_col_name]
		return df.loc[(df[speaker_id_col_name] != speaker_id) & (df[start_time_col_name] <= start_time)]

	@classmethod
	def __between_speaker_overlap(cls, entity_df: pd.DataFrame) -> pd.DataFrame:
		last_utt = entity_df.loc[entity_df[utterances.UtteranceTabularDataColumn.START_TIME.value].idxmax()]
		last_speaker = last_utt[utterances.UtteranceTabularDataColumn.SPEAKER_ID.value]
		cls.__speaker_other_overlap(entity_df, last_speaker)
		missed_utts = entity_df.loc[entity_df[TokenTypeOverlapColumn.TOKEN_TYPE_OVERLAP.value].isnull() | entity_df[
			TokenTypeOverlapColumn.COREF_SEQ_ORDER.value] < 1]
		if not missed_utts.empty:
			raise ValueError("Missed {} utterance rows.".format(missed_utts.shape[0]))

		return entity_df

	@classmethod
	def __create_coref_chain(cls, utt_row_idx: int, df: pd.DataFrame) -> Tuple[int, ...]:
		start_time_col_name = utterances.UtteranceTabularDataColumn.START_TIME.value
		result = []
		latest_prev_utt_idx = utt_row_idx
		prev_utts = cls.__prev_utts(df.loc[latest_prev_utt_idx], df)
		while not prev_utts.empty:
			result.append(latest_prev_utt_idx)
			latest_prev_utt_idx = prev_utts[start_time_col_name].idxmax()
			prev_utts = cls.__prev_utts(df.loc[latest_prev_utt_idx], df)

		# Append the last coreference to the list (in this case, the head of the chain)
		result.append(latest_prev_utt_idx)
		return tuple(reversed(result))

	@classmethod
	def __speaker_other_overlap(cls, entity_df: pd.DataFrame, speaker_id: str):
		speaker_utts = entity_df.loc[entity_df[utterances.UtteranceTabularDataColumn.SPEAKER_ID.value] == speaker_id]
		start_time_col_name = utterances.UtteranceTabularDataColumn.START_TIME.value
		last_utt_idx = speaker_utts[start_time_col_name].idxmax()
		coref_chain = cls.__create_coref_chain(last_utt_idx, entity_df)
		logging.debug("Created a coreference chain of length %d for the utt starting at %f by speaker \"%s\".",
					  len(coref_chain), entity_df.loc[last_utt_idx, start_time_col_name], speaker_id)

		preceding_coref_token_types = _EMPTY_SET
		preceding_coref_start_time = np.nan
		for coref_chain_idx, df_idx in enumerate(coref_chain):
			old_coref_seq_no = entity_df.loc[df_idx, TokenTypeOverlapColumn.COREF_SEQ_ORDER.value]
			if old_coref_seq_no < 1:
				entity_df.at[df_idx, TokenTypeOverlapColumn.COREF_SEQ_ORDER.value] = coref_chain_idx + 1
				# NOTE: Cannot assign iterable types as column values <https://github.com/pandas-dev/pandas/issues/7787>
				entity_df.at[
					df_idx, TokenTypeOverlapColumn.PRECEDING_TOKEN_TYPES.value] = create_tabular_collection_repr(
					preceding_coref_token_types)
				entity_df.at[df_idx, TokenTypeOverlapColumn.PRECEDING_UTT_START_TIME.value] = preceding_coref_start_time
				coref = entity_df.loc[df_idx]
				token_types = coref[TokenTypeOverlapColumn.TOKEN_TYPES.value]
				entity_df.at[
					df_idx, TokenTypeOverlapColumn.TOKEN_TYPE_OVERLAP.value] = alignment_metrics.token_type_overlap_ratio(
					token_types, preceding_coref_token_types)
				preceding_coref_token_types = token_types
				preceding_coref_start_time = coref[utterances.UtteranceTabularDataColumn.START_TIME.value]
			else:
				raise ValueError("Already set coref seq no!")

	def __init__(self, coreference_feature_col_name: str):
		self.coreference_feature_col_name = coreference_feature_col_name

	# noinspection PyTypeChecker,PyUnresolvedReferences
	def __call__(self, df: pd.DataFrame) -> pd.DataFrame:
		"""
		Calculates token type overlaps over multiple sessions.

		:param df: The DataFrame including rows for all sessions.
		:return: A copy of the DataFrame with token type data added.
		"""
		result = df.copy(deep=False)
		result[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value] = -1
		# NOTE: Cannot assign iterable types as column values <https://github.com/pandas-dev/pandas/issues/7787>
		result[TokenTypeOverlapColumn.PRECEDING_TOKEN_TYPES.value] = _EMPTY_SET_STR_REPR
		result[TokenTypeOverlapColumn.PRECEDING_UTT_START_TIME.value] = np.nan
		# Calculate token type overlap for each chain of reference for each entity in each session
		session_ref_utts = result.groupby((write_target_ref_utts.DYAD_COL_NAME, self.coreference_feature_col_name),
										  as_index=False, sort=False)
		return session_ref_utts.apply(self.__between_speaker_overlap)


class GeneralConvergenceTokenTypeOverlapCalculator(object):

	@staticmethod
	def __utt_mean_overlap(utt_row: pd.Series, prev_coref_utts: Mapping[int, pd.DataFrame]) -> float:
		coref_seq_ordinality = utt_row[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value]
		if coref_seq_ordinality < 2:
			result = np.longfloat(0.0)
		else:
			prev_utts = prev_coref_utts[coref_seq_ordinality]
			token_types = utt_row[TokenTypeOverlapColumn.TOKEN_TYPES.value]
			result = prev_utts.apply(lambda prev_utt: alignment_metrics.token_type_overlap_ratio(token_types, prev_utt[
				TokenTypeOverlapColumn.TOKEN_TYPES.value]), axis=1).mean()
		return result

	def __init__(self, coreference_feature_col_name: str):
		self.coreference_feature_col_name = coreference_feature_col_name

	# noinspection PyTypeChecker,PyUnresolvedReferences
	def __call__(self, df: pd.DataFrame) -> pd.DataFrame:
		"""
		Calculates token type overlaps over multiple sessions.

		:param df: The DataFrame including rows for all sessions.
		:return: A copy of the DataFrame with token type data added.
		"""
		scored_df = df.copy(deep=False)
		# Ensure that rows are sorted in order of which round they are for and their chronological ordering withing each round
		scored_df.sort_values(
			by=[sd.EventDataColumn.ROUND_ID.value, utterances.UtteranceTabularDataColumn.START_TIME.value,
				utterances.UtteranceTabularDataColumn.END_TIME.value], inplace=True)
		# Calculate token type overlap for each chain of reference for each entity/coreference feature and each speaker in each session
		session_ref_utts = scored_df.groupby((write_target_ref_utts.DYAD_COL_NAME,
											  self.coreference_feature_col_name),
											 as_index=False, sort=False)
		scored_df[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value] = session_ref_utts.cumcount() + 1
		prev_coref_utts = dict((coref_seq_ordinality, scored_df.loc[
			scored_df[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value] == coref_seq_ordinality - 1]) for
							   coref_seq_ordinality
							   in scored_df[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value].unique())
		# NOTE: This causes some loss of accuracy through rounding errors if the mean is taken of these means, but creating a bigger dataframe containing each individual comparison is extremely annoying with pandas
		scored_df[TokenTypeOverlapColumn.TOKEN_TYPE_OVERLAP.value] = scored_df.apply(
			lambda utt_row: self.__utt_mean_overlap(utt_row, prev_coref_utts), axis=1)
		scored_df[TokenTypeOverlapColumn.PRECEDING_TOKEN_TYPES.value] = "(not applicable for general convergence)"
		scored_df[TokenTypeOverlapColumn.PRECEDING_UTT_START_TIME.value] = "(not applicable for general convergence)"
		return scored_df


@unique
class TokenTypeOverlapColumn(Enum):
	COREF_SEQ_ORDER = "COREF_SEQ_ORDER"
	PRECEDING_UTT_START_TIME = "PRECEDING_UTT_START_TIME"
	PRECEDING_TOKEN_TYPES = "PRECEDING_TOKEN_TYPES"
	TOKEN_TYPES = "TOKEN_TYPES"
	TOKEN_TYPE_OVERLAP = "TOKEN_TYPE_OVERLAP"


class WithinSpeakerTokenTypeOverlapCalculator(object):

	@staticmethod
	def __token_type_overlap(utt: pd.Series) -> np.longfloat:
		preceding_token_types = utt[TokenTypeOverlapColumn.PRECEDING_TOKEN_TYPES.value]
		if pd.isnull(preceding_token_types):
			result = np.longfloat(0.0)
		else:
			token_types = utt[TokenTypeOverlapColumn.TOKEN_TYPES.value]
			result = alignment_metrics.token_type_overlap_ratio(token_types, preceding_token_types)
		return result

	def __init__(self, coreference_feature_col_name: str):
		self.coreference_feature_col_name = coreference_feature_col_name

	# noinspection PyTypeChecker,PyUnresolvedReferences
	def __call__(self, df: pd.DataFrame) -> pd.DataFrame:
		"""
		Calculates token type overlaps over multiple sessions.

		:param df: The DataFrame including rows for all sessions.
		:return: A copy of the DataFrame with token type data added.
		"""
		result = df.copy(deep=False)
		# Ensure that rows are sorted in order of which round they are for and their chronological ordering withing each round
		result.sort_values(
			by=[sd.EventDataColumn.ROUND_ID.value, utterances.UtteranceTabularDataColumn.START_TIME.value,
				utterances.UtteranceTabularDataColumn.END_TIME.value], inplace=True)
		# Calculate token type overlap for each chain of reference for each entity/coreference feature and each speaker in each session
		session_speaker_ref_utts = result.groupby((write_target_ref_utts.DYAD_COL_NAME,
												   self.coreference_feature_col_name,
												   utterances.UtteranceTabularDataColumn.SPEAKER_ID.value),
												  as_index=False, sort=False)
		result[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value] = session_speaker_ref_utts.cumcount() + 1
		result[TokenTypeOverlapColumn.PRECEDING_UTT_START_TIME.value] = session_speaker_ref_utts[
			utterances.UtteranceTabularDataColumn.START_TIME.value].shift()
		result[TokenTypeOverlapColumn.PRECEDING_TOKEN_TYPES.value] = session_speaker_ref_utts[
			TokenTypeOverlapColumn.TOKEN_TYPES.value].shift()
		result[TokenTypeOverlapColumn.TOKEN_TYPE_OVERLAP.value] = result.apply(self.__token_type_overlap,
																			   axis=1)
		# Convert the data rows into stable string representations here, after they have been used for calculating ovelaps and thus are no longer necessary for doing so
		result[TokenTypeOverlapColumn.TOKEN_TYPES.value] = result[TokenTypeOverlapColumn.TOKEN_TYPES.value].transform(
			create_tabular_collection_repr)
		result[TokenTypeOverlapColumn.PRECEDING_TOKEN_TYPES.value] = result[
			TokenTypeOverlapColumn.PRECEDING_TOKEN_TYPES.value].transform(
			lambda token_types: _EMPTY_SET_STR_REPR if pd.isnull(token_types) else create_tabular_collection_repr(
				token_types))
		return result


class TokenTypeSetFactory(object):
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
			result = _EMPTY_SET

		return result


def create_tabular_collection_repr(coll: Iterable[str]) -> str:
	"""
	 Creates a string from an iterable object because it is not possible to assign iterable types as pandas column values <https://github.com/pandas-dev/pandas/issues/7787>
	:param coll: The iterable object to create a string representation for.
	:return: A deterministic string representation for the given object.
	"""
	return " ".join(sorted(coll))


def read_event_utts(infile_path: str) -> pd.DataFrame:
	dialect = write_target_ref_utts.OUTPUT_FILE_DIALECT
	converters = {utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value: TokenTypeSetFactory()}
	result = pd.read_csv(infile_path, dialect=dialect, sep=dialect.delimiter,
						 float_precision="round_trip", converters=converters, dtype=INFILE_DTYPES)
	result.rename({utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value: TokenTypeOverlapColumn.TOKEN_TYPES.value},
				  axis=1, inplace=True)
	return result


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Calculates mean token type overlap for each coreference chain sequence ordinality in each chain for a given referent.")
	result.add_argument("infile", metavar="INFILE",
						help="The combined events and utterance data file to read.")
	result.add_argument("-d", "--dump", action='store_true',
						help="Dumps all the dataframe data to file rather than just the aggregates for the token type overlap.")

	metric_types = result.add_mutually_exclusive_group(required=True)
	metric_types.add_argument("-w", "--within-speaker", dest="within_speaker", action='store_true',
							  help="Calculate token type overlap for individual speakers with themselves.")
	metric_types.add_argument("-b", "--between-speaker", dest="between_speaker", action='store_true',
							  help="Calculate token type overlap for individual speakers with their interlocutors.")
	metric_types.add_argument("-g", "--general-convergence", dest="general_convergence", action='store_true',
							  help="Calculate general token type overlap for coreference chain steps.")

	feature_types = result.add_mutually_exclusive_group(required=True)
	feature_types.add_argument("-r", "--referent", action='store_true',
							   help="Calculate overlap for coreference of individual referent entities.")
	feature_types.add_argument("-s", "--shape", action='store_true',
							   help="Calculate overlap for coreference of shape features.")
	return result


def __main(args):
	if args.referent:
		coreference_feature_col_name = sd.EventDataColumn.ENTITY_ID.value
	elif args.shape:
		coreference_feature_col_name = sd.EventDataColumn.SHAPE.value
	else:
		raise AssertionError("Logic error")
	print("Using the dataframe column \"{}\" as the coreference feature to calculate overlaps for.".format(
		coreference_feature_col_name), file=sys.stderr)

	if args.within_speaker:
		print("Calculating within-speaker overlap.", file=sys.stderr)
		overlap_calculator = WithinSpeakerTokenTypeOverlapCalculator(coreference_feature_col_name)
	elif args.between_speaker:
		print("Calculating between-speaker overlap.", file=sys.stderr)
		overlap_calculator = BetweenSpeakerTokenTypeOverlapCalculator(coreference_feature_col_name)
	elif args.general_convergence:
		print("Calculating general overlap.", file=sys.stderr)
		overlap_calculator = GeneralConvergenceTokenTypeOverlapCalculator(coreference_feature_col_name)
	else:
		raise AssertionError("Logic error")

	infile = args.infile
	print("Reading \"{}\".".format(infile), file=sys.stderr)
	session_utt_df = read_event_utts(infile)
	print("DF shape is {}; {} unique dyad(s).".format(session_utt_df.shape,
													  session_utt_df[
														  write_target_ref_utts.DYAD_COL_NAME].nunique()),
		  file=sys.stderr)

	print("Calculating overlaps.", file=sys.stderr)
	session_utt_df = overlap_calculator(session_utt_df)
	if args.dump:
		session_utt_df.sort_values(
			[write_target_ref_utts.DYAD_COL_NAME, coreference_feature_col_name,
			 utterances.UtteranceTabularDataColumn.SPEAKER_ID.value,
			 TokenTypeOverlapColumn.COREF_SEQ_ORDER.value,
			 sd.EventDataColumn.ROUND_ID.value], inplace=True)
		session_utt_df.to_csv(sys.stdout, sep=OUTFILE_CSV_DIALECT.delimiter, encoding=OUTFILE_ENCODING, index=False)
	else:
		session_utt_df = session_utt_df[
			[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value, TokenTypeOverlapColumn.TOKEN_TYPE_OVERLAP.value]]
		session_utt_df = session_utt_df.groupby(
			TokenTypeOverlapColumn.COREF_SEQ_ORDER.value, as_index=False).filter(lambda group: group.name > 1)
		coref_seq_orders = session_utt_df[
			[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value, TokenTypeOverlapColumn.TOKEN_TYPE_OVERLAP.value]].groupby(
			TokenTypeOverlapColumn.COREF_SEQ_ORDER.value, as_index=False)
		aggs = coref_seq_orders.agg(["mean", "std", "sem"])
		aggs.columns = aggs.columns.droplevel(0)
		aggs.to_csv(sys.stdout, sep=OUTFILE_CSV_DIALECT.delimiter, encoding=OUTFILE_ENCODING, index_label="seq")


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
