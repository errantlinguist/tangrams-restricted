#!/usr/bin/env python3

import argparse
import re
import sys
from typing import Iterable, TypeVar

import numpy as np

import instructor_relevant_tokens_metrics

CELL_MULTIVALUE_DELIM_PATTERN = re.compile("\\s*,\\s*")
COREF_SEQ_COL_NAME_SUFFIX = "_COREF_SEQ"
OVERLAP_COL_NAME_SUFFIX = "_OVERLAP"
OVERLAP_NULL_VALUE = np.NaN

T = TypeVar('T')

COL_NAME_PATTERN = re.compile(
	".*?((?:" + instructor_relevant_tokens_metrics.COREF_SEQ_COL_NAME_SUFFIX + ")|(?:" + instructor_relevant_tokens_metrics.OVERLAP_COL_NAME_SUFFIX + "))((?:" + instructor_relevant_tokens_metrics.SELF_METRIC_COL_NAME_SUFFIX + ")|(?:" + instructor_relevant_tokens_metrics.OTHER_METRIC_COL_NAME_SUFFIX + "))")


def __create_metric_data_colname_dict(col_names: Iterable[str]):
	result = {}
	for col_name in col_names:
		match = COL_NAME_PATTERN.match(col_name)
		if match:
			datum = match.group(1)
			metric = match.group(2)
			try:
				data_col_names = result[metric]
			except KeyError:
				data_col_names = {}
				result[metric] = data_col_names

			data_col_names[datum] = col_name

	return result


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Measure referent token type overlap baselines in coreference chains in each game session, using only instructor language to build coreference chains.")
	result.add_argument("inpath", metavar="INPATH",
						help="The file to process.")
	return result


def __main(args):
	inpath = args.inpath
	print("Reading \"{}\".".format(inpath),
		  file=sys.stderr)
	round_tokens = instructor_relevant_tokens_metrics.read_round_tokens(inpath)
	metric_data_col_names = __create_metric_data_colname_dict(df.columns.values)

	for metric, data in metric_data_col_names.items():
		coref_seq_col_name = data[instructor_relevant_tokens_metrics.COREF_SEQ_COL_NAME_SUFFIX]
		coref_seq_groups = round_tokens.groupby(coref_seq_col_name, as_index=False)
		overlap_col_name = data[instructor_relevant_tokens_metrics.OVERLAP_COL_NAME_SUFFIX]
		agg_df = coref_seq_groups[overlap_col_name].aggregate({"mean", "std", "sem"})
		print(agg_df, file=sys.stdout)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
