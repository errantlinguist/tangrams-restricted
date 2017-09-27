#!/usr/bin/env python3

import argparse
import sys
from typing import Dict, Iterable

import instructor_relevant_tokens_metrics
import pandas as pd


def __create_qualified_col_name_dict(input_col_names: Iterable[str]) -> Dict[str, Dict[str, Dict[str, str]]]:
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


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Measure referent token type overlap means in coreference chains in each game session, using only instructor language to build coreference chains.")
	result.add_argument("inpath", metavar="INPATH",
						help="The file to process.")
	return result

#def __baseline_overlap(df: pd.DataFrame):

def __main(args):
	inpath = args.inpath
	print("Reading \"{}\".".format(inpath),
		  file=sys.stderr)
	round_tokens = instructor_relevant_tokens_metrics.read_round_tokens(inpath, keep_default_na=True, na_filter=True,
																		na_values=(
																			instructor_relevant_tokens_metrics.OUTPUT_NA_VALUE,
																			None))

	col_names = __create_qualified_col_name_dict(round_tokens.columns.values)
	for metric_name, metric_aggs in sorted(col_names.items(), key=lambda item: item[0].value):
		print("Processing metric \"{}\".".format(metric_name), file=sys.stderr)

		non_agg_measurement_col_names = metric_aggs[instructor_relevant_tokens_metrics.Aggregation.NONE]
		coref_seq_col_name = non_agg_measurement_col_names[instructor_relevant_tokens_metrics.Measurement.COREF_SEQ]
		print("Coreference chain sequence column \"{}\"; Is present? {}".format(coref_seq_col_name,
																				coref_seq_col_name in round_tokens.columns.values),
			  file=sys.stderr)
		coref_seq_groups = round_tokens.groupby(coref_seq_col_name, as_index=False)
		overlap_col_name = non_agg_measurement_col_names[instructor_relevant_tokens_metrics.Measurement.OVERLAP]
		print("Token overlap column \"{}\"; Is present? {}".format(overlap_col_name,
																   overlap_col_name in round_tokens.columns.values),
			  file=sys.stderr)
		agg_df = coref_seq_groups[overlap_col_name].aggregate(("mean", "std", "sem"))
		agg_df.fillna(instructor_relevant_tokens_metrics.OUTPUT_NA_VALUE)
		print(agg_df, file=sys.stdout)




if __name__ == "__main__":
	__main(__create_argparser().parse_args())
