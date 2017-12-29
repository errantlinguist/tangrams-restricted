#!/usr/bin/env python3

"""
Calculates mean token-type overlap for each coreference chain sequence ordinality in each chain for a given referent.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import sys
from typing import Optional, Tuple

import pandas as pd

import session_data as sd
import utterances


class SessionRoundTokenBagDataFrameFactory(object):
	DYAD_ID_COL_NAME = "DYAD"
	TOKEN_SEQ_COL_NAME = utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value

	@staticmethod
	def __concatenate_token_seqs(df: pd.DataFrame) -> Tuple[str, ...]:
		# noinspection PyProtectedMember
		row_dicts = (row._asdict() for row in df.itertuples(index=False))
		token_seqs = (row_dict[utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value] for row_dict in row_dicts)
		return tuple(token for token_seq in token_seqs for token in token_seq)

	def __init__(self, utt_reader: Optional[utterances.UtteranceTabularDataReader] = None):
		self.utt_reader = utterances.UtteranceTabularDataReader() if utt_reader is None else utt_reader

	def __call__(self, session_data: sd.SessionData) -> pd.DataFrame:
		session_name = session_data.name
		print("Reading events and utterances for \"{}\".".format(session_name), file=sys.stderr)
		utts_df = self.utt_reader(session_data.utts)
		round_utts = utts_df.groupby(utterances.UtteranceTabularDataColumn.ROUND_ID.value)
		round_token_bags = dict((round_id, self.__concatenate_token_seqs(group)) for round_id, group in round_utts)

		result = session_data.read_events()
		result[self.DYAD_ID_COL_NAME] = session_name
		result[self.TOKEN_SEQ_COL_NAME] = result[sd.EventDataColumn.ROUND_ID.value].transform(
			lambda round_id: round_token_bags[round_id])
		return result


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Calculates mean token-type overlap for each coreference chain sequence ordinality in each chain for a given referent.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The paths to search for sessions to process.")
	return result


def __main(args):
	inpaths = args.inpaths
	print("Looking for session data underneath {}.".format(inpaths), file=sys.stderr)
	df_factory = SessionRoundTokenBagDataFrameFactory()
	session_utt_df = pd.concat(df_factory(session_data) for _, session_data in sd.walk_session_data(inpaths))
	orig_session_utt_df_shape = session_utt_df.shape
	print("DF shape is {}; {} unique dyad(s).".format(orig_session_utt_df_shape,
													  len(session_utt_df[
															  SessionRoundTokenBagDataFrameFactory.DYAD_ID_COL_NAME].unique())),
		  file=sys.stderr)
	session_utt_df = session_utt_df.loc[session_utt_df[sd.EventDataColumn.REFERENT_ENTITY.value] == True]
	print("Removed {} non-referent entity rows.".format(orig_session_utt_df_shape[0] - session_utt_df.shape[0]),
		  file=sys.stderr)

	session_rounds = session_utt_df.groupby(
		(SessionRoundTokenBagDataFrameFactory.DYAD_ID_COL_NAME, sd.EventDataColumn.ROUND_ID.value), as_index=False)
	print("Found {} rounds in all sessions.".format(len(session_rounds)), file=sys.stderr)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
