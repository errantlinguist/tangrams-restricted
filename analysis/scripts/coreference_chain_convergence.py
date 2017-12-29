#!/usr/bin/env python3

"""
Calculates mean token-type overlap for each coreference chain sequence ordinality in each chain for a given referent.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import sys
from typing import Tuple

import pandas as pd

import session_data as sd
import utterances

EVENT_DF_DYAD_ID_COL_NAME = "DYAD"
EVENT_DF_TOKEN_SEQ_COL_NAME = utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value


def concatenate_token_seqs(df: pd.DataFrame) -> Tuple[str, ...]:
	# noinspection PyProtectedMember
	row_dicts = (row._asdict() for row in df.itertuples(index=False))
	token_seqs = (row_dict[utterances.UtteranceTabularDataColumn.TOKEN_SEQ.value] for row_dict in row_dicts)
	return tuple(token for token_seq in token_seqs for token in token_seq)


def read_session_utterances(session_data: sd.SessionData,
							utt_reader: utterances.UtteranceTabularDataReader) -> pd.DataFrame:
	session_name = session_data.name
	print("Reading events and utterances for \"{}\".".format(session_name), file=sys.stderr)
	utts_df = utt_reader(session_data.utts)
	round_utts = utts_df.groupby(utterances.UtteranceTabularDataColumn.ROUND_ID.value)
	round_token_bags = dict((round_id, concatenate_token_seqs(group)) for round_id, group in round_utts)

	result = session_data.read_events()
	result[EVENT_DF_DYAD_ID_COL_NAME] = session_name
	result[EVENT_DF_TOKEN_SEQ_COL_NAME] = result[sd.DataColumn.ROUND_ID.value.name].transform(
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
	utt_reader = utterances.UtteranceTabularDataReader()
	session_utt_df = pd.concat(
		read_session_utterances(session_data, utt_reader) for _, session_data in sd.walk_session_data(inpaths))
	print("DF shape is {}; {} unique dyad(s).".format(session_utt_df.shape,
													  len(session_utt_df[EVENT_DF_DYAD_ID_COL_NAME].unique())), file=sys.stderr)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
