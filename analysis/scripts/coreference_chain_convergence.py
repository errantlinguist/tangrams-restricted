#!/usr/bin/env python3

"""
Calculates mean token-type overlap for each coreference chain sequence ordinality in each chain for a given referent.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import sys

import pandas as pd

import session_data as sd
import utterances

DYAD_ID_COL_NAME = "DYAD"


def read_session_utterances(session_data: sd.SessionData,
							utt_reader: utterances.UtteranceTabularDataReader) -> pd.DataFrame:
	result = utt_reader(session_data.utts)
	dyad_id = session_data.name
	result[DYAD_ID_COL_NAME] = dyad_id
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
													  len(session_utt_df[DYAD_ID_COL_NAME].unique())), file=sys.stderr)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
