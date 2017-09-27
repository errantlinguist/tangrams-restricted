#!/usr/bin/env python3

import argparse
import re
import sys
from typing import Iterable

import numpy as np

import instructor_relevant_tokens_metrics


def __create_col_name_regex() -> str:
	col_disjunction = "((?:" + ")|(?:".join(col.value for col in instructor_relevant_tokens_metrics.DataColumn) + "))"
	metric_disjunction = "((?:" + ")|(?:".join(
		metric.value for metric in instructor_relevant_tokens_metrics.Metric) + "))"
	return ".*?" + col_disjunction + metric_disjunction


COL_NAME_PATTERN = re.compile(__create_col_name_regex())


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
		description="Measure referent token type overlap means in coreference chains in each game session, using only instructor language to build coreference chains.")
	result.add_argument("inpath", metavar="INPATH",
						help="The file to process.")
	return result


def __main(args):
	inpath = args.inpath
	print("Reading \"{}\".".format(inpath),
		  file=sys.stderr)
	round_tokens = instructor_relevant_tokens_metrics.read_round_tokens(inpath, na_values=(
		instructor_relevant_tokens_metrics.OUTPUT_NA_VALUE, np.NaN, None))

	metric_data_col_names = __create_metric_data_colname_dict(round_tokens.columns.values)
	for metric, data in metric_data_col_names.items():
		coref_seq_col_name = data[instructor_relevant_tokens_metrics.DataColumn.COREF_SEQ.value]
		coref_seq_groups = round_tokens.groupby(coref_seq_col_name, as_index=False)
		overlap_col_name = data[instructor_relevant_tokens_metrics.DataColumn.OVERLAP.value]
		agg_df = coref_seq_groups[overlap_col_name].aggregate(("mean", "std", "sem"))
		agg_df.fillna(instructor_relevant_tokens_metrics.OUTPUT_NA_VALUE)
		print(agg_df, file=sys.stdout)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
