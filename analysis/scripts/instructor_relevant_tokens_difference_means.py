#!/usr/bin/env python3

import argparse
import os.path
import sys
from typing import Dict, Iterable, Mapping

import pandas as pd

import instructor_relevant_tokens_metrics

COL_DELIM = "\t"

__TOKEN_SET_FILENAME_PREFIXES = {"RELEVANT_TOKENS_REFERENT": "referent", "RELEVANT_TOKENS_SHAPE": "shape"}


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
			# print("Skipping column \"{}\".".format(input_col_name), file=sys.stderr)
			pass

	return result


def __print_non_aggregate_overlap_summary(df: pd.DataFrame, minuend_measurement_col_names: Mapping[
	instructor_relevant_tokens_metrics.Measurement, str], subtrahend_measurement_col_names: Mapping[
	instructor_relevant_tokens_metrics.Measurement, str], coref_seq_col_name, outfile_path: str):
	coref_seq_groups = df.groupby(coref_seq_col_name, as_index=False)

	minuend_overlap_col_name = minuend_measurement_col_names[instructor_relevant_tokens_metrics.Measurement.OVERLAP]
	subtrahend_overlap_col_name = subtrahend_measurement_col_names[
		instructor_relevant_tokens_metrics.Measurement.OVERLAP]

	diffs = coref_seq_groups.apply(lambda group_df: (group_df[minuend_overlap_col_name] - group_df[subtrahend_overlap_col_name]).aggregate(("mean", "std", "sem", "median", "mad")))
	#group_aggregates = diffs.aggregate(("mean", "std", "sem", "median", "mad"))
	diffs.dropna(inplace=True)
	print(
		"Writing means calculated using coref seq. col. \"{}\" and the difference of token col. \"{}\" and \"{}\" to \"{}\".".format(
			coref_seq_col_name, minuend_overlap_col_name, subtrahend_overlap_col_name, outfile_path), file=sys.stderr)
	with open(outfile_path, 'w', encoding="utf-8") as outfile:
		# print("\n\n", file=outfile)
		# print(COL_DELIM.join((coref_seq_col_name, overlap_col_name)), file=outfile)
		diffs.to_csv(outfile, index_label="seq", sep=COL_DELIM)


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Measure referent token type overlap means in coreference chains in each game session, using only instructor language to build coreference chains.")
	result.add_argument("inpath", metavar="INPATH",
						help="The file to process.")
	result.add_argument("-o", "--outdir", metavar="OUTPATH", required=True,
						help="The directory to write the aggregate file(s) to.")
	result.add_argument("-t", "--tokens", metavar="COL_NAME", required=True,
						help="The column to use as relevant tokens.")
	result.add_argument("-a", "--aggregation", metavar="COL_NAME", required=True,
						help="The aggregation type to subtract from the token overlap.")
	return result


def __create_outfile_name(token_set_col_name: str, metric: instructor_relevant_tokens_metrics.Metric,
						  agg: instructor_relevant_tokens_metrics.Aggregation) -> str:
	prefix = __TOKEN_SET_FILENAME_PREFIXES[token_set_col_name]
	metric_infix = metric.value.lower()
	agg_infix = agg.value.lower()
	suffix = "mean-diffs.tsv"
	return '_'.join((prefix, metric_infix, agg_infix, suffix))


def __main(args):
	inpath = args.inpath
	outdir = args.outdir
	token_set_col_name = args.tokens
	agg = instructor_relevant_tokens_metrics.Aggregation[args.aggregation]
	print(
		"Reading \"{}\" using tokens from column \"{}\" and substract {} from it as a baseline; Will write to \"{}\".".format(
			inpath,
			token_set_col_name, agg, outdir),
		file=sys.stderr)
	overlaps = instructor_relevant_tokens_metrics.read_round_tokens(inpath, keep_default_na=True, na_filter=True,
																	na_values=(
																		instructor_relevant_tokens_metrics.OUTPUT_NA_VALUE,
																		None))

	col_names = __create_qualified_col_name_dict(overlaps.columns.values)
	for metric, metric_aggs in sorted(col_names.items(), key=lambda item: item[0].value):
		print("Processing metric \"{}\".".format(metric), file=sys.stderr)
		minuend_measurement_col_names = metric_aggs[instructor_relevant_tokens_metrics.Aggregation.NONE]
		subtrahend_measurement_col_names = metric_aggs[agg]

		coref_seq_no_col_name = instructor_relevant_tokens_metrics.create_qualified_col_name(token_set_col_name,
																							 instructor_relevant_tokens_metrics.Measurement.COREF_SEQ,
																							 metric,
																							 instructor_relevant_tokens_metrics.Aggregation.NONE)
		outfile_name = __create_outfile_name(token_set_col_name, metric, agg)
		outfile_path = os.path.join(outdir, outfile_name)
		__print_non_aggregate_overlap_summary(overlaps, minuend_measurement_col_names, subtrahend_measurement_col_names,
											  coref_seq_no_col_name, outfile_path)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
