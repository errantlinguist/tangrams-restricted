#!/usr/bin/env python3

import argparse
import re
import sys
from typing import Iterable

import instructor_relevant_tokens_metrics


def __create_col_name_regex() -> str:
	col_disjunction = "(?P<col_name>(?:" + ")|(?:".join(
		re.escape(col.value) for col in instructor_relevant_tokens_metrics.DataColumn) + "))"
	metric_disjunction = "(?P<metric>(?:" + ")|(?:".join(
		re.escape(metric.value) for metric in instructor_relevant_tokens_metrics.Metric) + "))"
	agg_disjunction = "(?P<agg>(?:" + ")|(?:".join(
		re.escape(agg.value) for agg in instructor_relevant_tokens_metrics.Aggregation) + "))"
	return ".*?" + col_disjunction + metric_disjunction + agg_disjunction


COL_NAME_PATTERN = re.compile(__create_col_name_regex())


def __create_col_name_dict(col_names: Iterable[str]):
	result = {}
	for col_name in col_names:
		match = COL_NAME_PATTERN.match(col_name)
		if match:
			datum = match.group("col_name")
			try:
				metric_aggs = result[datum]
			except KeyError:
				metric_aggs = {}
				result[datum] = metric_aggs

			metric = match.group("metric")
			try:
				agg_col_names = metric_aggs[metric]
			except KeyError:
				agg_col_names = {}
				metric_aggs[metric] = agg_col_names

			agg = match.group("agg")
			agg_col_names[agg] = col_name

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
	round_tokens = instructor_relevant_tokens_metrics.read_round_tokens(inpath, keep_default_na=True, na_filter=True,
																		na_values=(
																			instructor_relevant_tokens_metrics.OUTPUT_NA_VALUE,
																			None))

	metric_data_col_names = __create_col_name_dict(round_tokens.columns.values)
	for metric, data in metric_data_col_names.items():
		coref_seq_col_name = data[instructor_relevant_tokens_metrics.DataColumn.COREF_SEQ.value]
		print("Coreference chain sequence column \"{}\"; Is present? {}".format(coref_seq_col_name,
																				coref_seq_col_name in round_tokens.columns.values),
			  file=sys.stderr)
		coref_seq_groups = round_tokens.groupby(coref_seq_col_name, as_index=False)
		overlap_col_name = data[instructor_relevant_tokens_metrics.DataColumn.OVERLAP.value]
		print("Token overlap column \"{}\"; Is present? {}".format(overlap_col_name,
																   overlap_col_name in round_tokens.columns.values),
			  file=sys.stderr)
		agg_df = coref_seq_groups[overlap_col_name].aggregate(("mean", "std", "sem"))
		agg_df.fillna(instructor_relevant_tokens_metrics.OUTPUT_NA_VALUE)
		print(agg_df, file=sys.stdout)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
