#!/usr/bin/env python3

"""
Calculates general token type overlap for each coreference chain sequence ordinality in each chain for a given referent in the entire corpus.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import csv
import sys
from typing import Any, Dict, List, Mapping, Tuple

import numpy as np
import pandas as pd

from common import alignment_metrics, session_data as sd, utterances
import write_target_ref_utts
from coreference_chain_overlap import TokenTypeOverlapColumn, read_event_utts

OUTFILE_CSV_DIALECT = csv.excel_tab


class GeneralConvergenceTokenTypeOverlapCalculator(object):
	__NULL_ARRAY = np.full(1, np.longfloat(0.0))

	@classmethod
	def __utt_overlaps(cls, utt_row: Mapping[str, Any], prev_coref_utts: Mapping[int, pd.DataFrame]) -> Tuple[
		int, np.array]:
		coref_seq_ordinality = utt_row[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value]
		if coref_seq_ordinality < 2:
			overlaps = cls.__NULL_ARRAY
		else:
			prev_utts = prev_coref_utts[coref_seq_ordinality]
			token_types = utt_row[TokenTypeOverlapColumn.TOKEN_TYPES.value]
			overlaps = prev_utts.apply(
				lambda prev_utt: alignment_metrics.token_type_overlap_ratio(token_types, prev_utt[
					TokenTypeOverlapColumn.TOKEN_TYPES.value]), axis=1).values
		return coref_seq_ordinality, overlaps

	def __init__(self, coreference_feature_col_name: str):
		self.coreference_feature_col_name = coreference_feature_col_name

	# noinspection PyTypeChecker,PyUnresolvedReferences
	def __call__(self, df: pd.DataFrame) -> Dict[int, List[float]]:
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
		coref_seq_ordinalities = scored_df[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value].unique()
		prev_coref_utts = dict((coref_seq_ordinality, scored_df.loc[
			scored_df[TokenTypeOverlapColumn.COREF_SEQ_ORDER.value] == coref_seq_ordinality - 1]) for
							   coref_seq_ordinality
							   in coref_seq_ordinalities)
		result = dict((coref_seq_ordinality, []) for coref_seq_ordinality in coref_seq_ordinalities)
		for row in scored_df.itertuples():
			# noinspection PyProtectedMember
			coref_seq_ordinality, overlaps = self.__utt_overlaps(row._asdict(), prev_coref_utts)
			result[coref_seq_ordinality].extend(overlaps)
		return result


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Calculates general token type overlap for each coreference chain sequence ordinality in each chain for a given referent in the entire corpus.")
	result.add_argument("infile", metavar="INFILE",
						help="The combined events and utterance data file to read.")

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

	print("Calculating general overlap.", file=sys.stderr)
	overlap_calculator = GeneralConvergenceTokenTypeOverlapCalculator(coreference_feature_col_name)

	infile = args.infile
	print("Reading \"{}\".".format(infile), file=sys.stderr)
	session_utt_df = read_event_utts(infile)
	print("DF shape is {}; {} unique dyad(s).".format(session_utt_df.shape,
													  session_utt_df[
														  write_target_ref_utts.DYAD_COL_NAME].nunique()),
		  file=sys.stderr)

	print("Calculating overlaps.", file=sys.stderr)
	utt_overlaps = overlap_calculator(session_utt_df)
	overlaps_to_print = ((coref_seq_ordinality, overlaps) for (coref_seq_ordinality, overlaps) in utt_overlaps.items()
						 if
						 coref_seq_ordinality > 1)
	# https://pythonconquerstheuniverse.wordpress.com/2011/05/08/newline-conversion-in-python-3/
	writer = csv.writer(sys.stdout, dialect=OUTFILE_CSV_DIALECT, lineterminator="\n")
	writer.writerow(("seq", "overlap"))
	for coref_seq_ordinality, overlaps in sorted(overlaps_to_print, key=lambda item: item[0]):
		for overlap in overlaps:
			writer.writerow((coref_seq_ordinality, overlap))


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
