#!/usr/bin/env python3

"""
Calculates mean token-type overlap for each coreference chain sequence ordinality in each chain for a given referent.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import csv
import sys
from collections import defaultdict
from decimal import Decimal
from enum import Enum, unique
from numbers import Integral
from typing import DefaultDict, FrozenSet, IO, Iterator, List, Mapping, Optional, Tuple

import numpy as np
import pandas as pd
import scipy.stats

import alignment_metrics
import session_data as sd
import utterances

ZERO_DECIMAL = Decimal("0")


@unique
class SessionRoundTokenTypeSetDataFrameColumn(Enum):
	DYAD_ID = "DYAD"
	TOKEN_SEQ = utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value


class SessionRoundTokenTypeSetDataFrameFactory(object):

	@staticmethod
	def __create_token_type_set(df: pd.DataFrame) -> FrozenSet[str]:
		# noinspection PyProtectedMember
		row_dicts = (row._asdict() for row in df.itertuples(index=False))
		token_seqs = (row_dict[utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value] for row_dict in row_dicts)
		return frozenset(token for token_seq in token_seqs for token in token_seq)

	def __init__(self, utt_reader: Optional[utterances.UtteranceTabularDataReader] = None):
		self.utt_reader = utterances.UtteranceTabularDataReader() if utt_reader is None else utt_reader

	def __call__(self, session_data: sd.SessionData) -> pd.DataFrame:
		session_name = session_data.name
		print("Reading events and utterances for \"{}\".".format(session_name), file=sys.stderr)
		utts_df = self.utt_reader(session_data.utts)
		round_utts = utts_df.groupby(utterances.UtteranceTabularDataColumn.ROUND_ID.value)
		round_token_bags = dict((round_id, self.__create_token_type_set(group)) for round_id, group in round_utts)

		result = session_data.read_events()
		result[SessionRoundTokenTypeSetDataFrameColumn.DYAD_ID.value] = session_name
		result[SessionRoundTokenTypeSetDataFrameColumn.TOKEN_SEQ.value] = result[
			sd.EventDataColumn.ROUND_ID.value].transform(
			lambda round_id: round_token_bags[round_id])
		return result


class ReferentTokenTypeOverlapCalculator(object):

	@staticmethod
	def __token_type_overlap(round_id: Integral, preceding_round_id: Integral, df: pd.DataFrame) -> Decimal:
		# Check first if there are preceding round rows in order to skip cases where there are none
		prev_round_rows = df.loc[df[sd.EventDataColumn.ROUND_ID.value] == preceding_round_id]
		prev_round_row_count = prev_round_rows.shape[0]
		# If there are no preceding rows, the given referent is a referent for the first time
		if prev_round_row_count < 1:
			result = ZERO_DECIMAL
		else:
			assert prev_round_row_count == 1
			prev_round_token_types = prev_round_rows.iloc[0][
				SessionRoundTokenTypeSetDataFrameColumn.TOKEN_SEQ.value]

			round_rows = df.loc[df[sd.EventDataColumn.ROUND_ID.value] == round_id]
			assert round_rows.shape[0] == 1
			round_token_types = round_rows.iloc[0][SessionRoundTokenTypeSetDataFrameColumn.TOKEN_SEQ.value]

			result = alignment_metrics.token_type_overlap_ratio(round_token_types, prev_round_token_types)
		return result

	@classmethod
	def __round_token_type_overlaps(cls, ref_df: pd.DataFrame) -> Iterator[Tuple[int, Decimal]]:
		"""
		Calculates token-type overlaps for each round in which a given entity is referred to.

		:param ref_df: A DataFrame containing data rows for the given referent.
		:return: A generator of pairs of "(coref_seq_ordinality, token_type_overlap_ratio)".
		"""

		all_round_ids = tuple(sorted(ref_df[sd.EventDataColumn.ROUND_ID.value].unique()))
		# Get rid of the first round because it cannot have any overlap
		round_ids = all_round_ids[1:]
		preceding_round_ids = all_round_ids[:len(all_round_ids) - 1]
		assert len(round_ids) == len(preceding_round_ids)
		coref_seq_round_id_pairs = zip(round_ids, preceding_round_ids)
		return ((coref_seq_ordinality, cls.__token_type_overlap(round_id, preceding_round_id, ref_df)) for
				(coref_seq_ordinality, (round_id, preceding_round_id)) in enumerate(coref_seq_round_id_pairs, start=2))

	@classmethod
	def __session_token_type_overlaps(cls, session_df: pd.DataFrame) -> Tuple[Tuple[int, Decimal], ...]:
		"""
		Calculates token-type overlaps for a single session, since entity IDs are not unique across sessions.

		:param session_df: The session DataFrame.
		:return: A tuple of pairs of "(coref_seq_ordinality, token_type_overlap_ratio)".
		"""
		ref_rows = session_df.groupby(sd.EventDataColumn.ENTITY_ID.value, as_index=False)
		return tuple((round_id, overlap) for _, rows in ref_rows for (round_id, overlap) in
					 cls.__round_token_type_overlaps(rows))

	def __call__(self, df: pd.DataFrame) -> DefaultDict[int, List[Decimal]]:
		"""
		Calculates token-type overlaps over multiple sessions.

		:param df: The DataFrame including rows for all sessions.
		:return: A dictionary of coreference sequence ordinalities ("1,2,3...") to lists of individual token-type overlap ratios calculated for each session.
		"""
		sessions = df.groupby(SessionRoundTokenTypeSetDataFrameColumn.DYAD_ID.value, as_index=False)
		session_coref_seq_overlaps = sessions.apply(self.__session_token_type_overlaps)

		result = defaultdict(list)
		for overlap_values in session_coref_seq_overlaps:
			for coref_seq_ordinality, overlap_value in overlap_values:
				result[coref_seq_ordinality].append(overlap_value)
		return result


def write_tabular_statistics(coref_chain_seq_overlaps: Mapping[int, List[Decimal]], outfile: IO[str]):
	writer = csv.writer(outfile, dialect=csv.excel_tab)
	writer.writerow(("seq", "mean", "std", "sem"))
	for coref_seq_ordinality, overlap_values in sorted(coref_chain_seq_overlaps.items(), key=lambda item: item[0]):
		if len(overlap_values) >= 2:
			float_values = np.asarray(overlap_values, np.longfloat)
			mean = np.mean(float_values)
			std = np.std(float_values)
			sem = scipy.stats.sem(float_values)
			writer.writerow((coref_seq_ordinality, mean, std, sem))


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Calculates mean token-type overlap for each coreference chain sequence ordinality in each chain for a given referent.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The paths to search for sessions to process.")
	return result


def __main(args):
	inpaths = args.inpaths
	print("Looking for session data underneath {}.".format(inpaths), file=sys.stderr)
	df_factory = SessionRoundTokenTypeSetDataFrameFactory()
	session_utt_df = pd.concat(df_factory(session_data) for _, session_data in sd.walk_session_data(inpaths))
	orig_session_utt_df_shape = session_utt_df.shape
	print("DF shape is {}; {} unique dyad(s).".format(orig_session_utt_df_shape,
													  session_utt_df[
														  SessionRoundTokenTypeSetDataFrameColumn.DYAD_ID.value].nunique()),
		  file=sys.stderr)
	session_utt_df = session_utt_df.loc[(session_utt_df[sd.EventDataColumn.REFERENT_ENTITY.value] == True) & (
			session_utt_df[sd.EventDataColumn.EVENT_NAME.value] == "nextturn.request")]
	session_utt_df_shape = session_utt_df.shape
	print("Removed {} non-referent, non new-turn-request entity rows; New shape is {}.".format(
		orig_session_utt_df_shape[0] - session_utt_df_shape[0], session_utt_df_shape),
		file=sys.stderr)
	# Ensure that rows are sorted in order of which round they are for
	session_utt_df.sort_values(by=sd.EventDataColumn.ROUND_ID.value, inplace=True)

	ref_overlap_calculator = ReferentTokenTypeOverlapCalculator()
	ref_coref_chain_seq_overlaps = ref_overlap_calculator(session_utt_df)
	write_tabular_statistics(ref_coref_chain_seq_overlaps, sys.stdout)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
