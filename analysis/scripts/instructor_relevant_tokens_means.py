#!/usr/bin/env python3

import argparse
import sys
from typing import Dict, IO, Iterable, Mapping

import pandas as pd

import instructor_relevant_tokens_metrics


def __create_qualified_col_name_dict(input_col_names: Iterable[str]) -> Dict[
	instructor_relevant_tokens_metrics.Metric, Dict[
		instructor_relevant_tokens_metrics.Aggregation, Dict[instructor_relevant_tokens_metrics.Measurement, str]]]:
	result = {}
	for input_col_name in input_col_names:
		try:
			prefix, measurement, metric, agg = instructor_relevant_tokens_metrics.col_data(input_col_name)
			try:
				agg_col_names = result[metric]
			except KeyError:
				agg_col_names = {}
				result[metric] = agg_col_names

			try:
				measurement_col_names = agg_col_names[agg]
			except KeyError:
				measurement_col_names = {}
				agg_col_names[agg] = measurement_col_names

			measurement_col_names[measurement] = input_col_name

		except ValueError:
			print("Skipping column \"{}\".".format(input_col_name), file=sys.stderr)

	return result


def __print_non_aggregate_overlap_summary(df: pd.DataFrame, measurement_col_names: Mapping[
	instructor_relevant_tokens_metrics.Measurement, str], outfile: IO[str]):
	coref_seq_col_name = measurement_col_names[instructor_relevant_tokens_metrics.Measurement.COREF_SEQ]
	print("Coreference chain sequence column \"{}\"; Is present? {}".format(coref_seq_col_name,
																			coref_seq_col_name in df.columns.values),
		  file=sys.stderr)
	coref_seq_groups = df.groupby(coref_seq_col_name, as_index=False)
	overlap_col_name = measurement_col_names[instructor_relevant_tokens_metrics.Measurement.OVERLAP]
	print("Token overlap column \"{}\"; Is present? {}".format(overlap_col_name,
															   overlap_col_name in df.columns.values),
		  file=sys.stderr)
	agg_df = coref_seq_groups[overlap_col_name].aggregate(("mean", "std", "sem"))
	agg_df.fillna(instructor_relevant_tokens_metrics.OUTPUT_NA_VALUE)
	print(agg_df, file=outfile)


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
	overlaps = instructor_relevant_tokens_metrics.read_round_tokens(inpath, keep_default_na=True, na_filter=True,
																	na_values=(
																		instructor_relevant_tokens_metrics.OUTPUT_NA_VALUE,
																		None))

	outfile = sys.stdout
	col_names = __create_qualified_col_name_dict(overlaps.columns.values)
	for metric_name, metric_aggs in sorted(col_names.items(), key=lambda item: item[0].value):
		print("Processing metric \"{}\".".format(metric_name), file=sys.stderr)

		non_agg_measurement_col_names = metric_aggs[instructor_relevant_tokens_metrics.Aggregation.NONE]
		__print_non_aggregate_overlap_summary(overlaps, non_agg_measurement_col_names, outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
