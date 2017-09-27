#!/usr/bin/env python3

import argparse
import csv
import re
import sys
from enum import Enum, unique
from typing import Optional, FrozenSet, Iterable, Iterator, Tuple, TypeVar

import numpy as np
import pandas as pd

CELL_MULTIVALUE_DELIM_PATTERN = re.compile("\\s*,\\s*")
OUTPUT_NA_VALUE = "N/A"
OVERLAP_NULL_VALUE = np.NaN

T = TypeVar('T')

_AGGREGATION_PREFIX = '@'


@unique
class Aggregation(Enum):
	NONE = "NONE"
	BASELINE = "BASELINE"
	REFERENT_BASELINE = "REFERENT_BASELINE"


_MEASUREMENT_PREFIX = '$'


@unique
class Measurement(Enum):
	COREF_SEQ = "COREF_SEQ"
	OVERLAP = "OVERLAP"


_METRIC_PREFIX = '#'


@unique
class Metric(Enum):
	SELF = "SELF"
	OTHER = "OTHER"
	EITHER = "EITHER"
	BASELINE = "BASELINE"


def qualified_col_name(prefix: str, measurement: Measurement, metric: Metric, agg: Aggregation) -> str:
	return prefix + _MEASUREMENT_PREFIX + measurement.value + _METRIC_PREFIX + metric.value + _AGGREGATION_PREFIX + agg.value


def col_data(qualified_col_name: str) -> Optional[Tuple[str, Measurement, Metric, Aggregation]]:
	rest, agg_str = qualified_col_name.rsplit(_AGGREGATION_PREFIX, 1)
	agg = Aggregation[agg_str]
	rest, metric_str = rest.rsplit(_METRIC_PREFIX, 1)
	metric = Metric[metric_str]

	prefix, measurement_str = rest.split(_MEASUREMENT_PREFIX, 1)
	measurement = Measurement[measurement_str]
	return prefix, measurement, metric, agg


def complement_coref_chain_seq_no(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
								  token_set_col_name: str) -> int:
	prev_coref_rows = tuple(__prev_complement_rows(row, df, referent_id_col_name, token_set_col_name))
	return len(prev_coref_rows) + 1


def complement_token_overlap(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
							 token_set_col_name: str) -> int:
	prev_coref_rows = tuple(__prev_complement_rows(row, df, referent_id_col_name, token_set_col_name))
	if prev_coref_rows:
		prev_coref_row = max(prev_coref_rows, key=lambda r: r["ROUND"])
		current_tokens = row[token_set_col_name]
		prev_tokens = prev_coref_row[token_set_col_name]
		result = set_overlap(current_tokens, prev_tokens)
	else:
		result = OVERLAP_NULL_VALUE
	return result


def either_coref_chain_seq_no(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
							  token_set_col_name: str) -> int:
	prev_coref_rows = __prev_either_coref_rows(row, df, referent_id_col_name, token_set_col_name)
	return prev_coref_rows.shape[0] + 1


def either_token_overlap(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
						 token_set_col_name: str) -> int:
	prev_coref_rows = __prev_either_coref_rows(row, df, referent_id_col_name, token_set_col_name)
	try:
		prev_coref_row = prev_coref_rows.loc[prev_coref_rows["ROUND"].argmax()]
		current_tokens = row[token_set_col_name]
		prev_tokens = prev_coref_row[token_set_col_name]
		result = set_overlap(current_tokens, prev_tokens)
	except ValueError:
		result = OVERLAP_NULL_VALUE
	return result


def parse_set(cell_value: str) -> FrozenSet[str]:
	return frozenset(CELL_MULTIVALUE_DELIM_PATTERN.split(cell_value))


def prettify_token_set_series(df: pd.DataFrame):
	df["RELEVANT_TOKENS_REFERENT"] = df["RELEVANT_TOKENS_REFERENT"].map(
		__token_set_repr)
	df["RELEVANT_TOKENS_SHAPE"] = df["RELEVANT_TOKENS_SHAPE"].map(
		__token_set_repr)


def read_round_tokens(inpath: str, **kwargs) -> pd.DataFrame:
	unified_kwargs = {"converters": {"RELEVANT_TOKENS_REFERENT": parse_set, "RELEVANT_TOKENS_SHAPE": parse_set},
					  "dialect": csv.excel_tab, "encoding": "utf-8", "float_precision": "high",
					  "memory_map": True, "na_filter": False, "sep": "\t"}
	# Override any defaults with those from explicitly-supplied kwargs
	for param_name, param_value in kwargs.items():
		unified_kwargs[param_name] = param_value
	return pd.read_csv(inpath, **unified_kwargs)


def self_coref_chain_seq_no(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
							token_set_col_name: str) -> int:
	prev_coref_rows = __prev_self_coref_rows(row, df, referent_id_col_name, token_set_col_name)
	return prev_coref_rows.shape[0] + 1


def self_token_overlap(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
					   token_set_col_name: str) -> int:
	prev_coref_rows = __prev_self_coref_rows(row, df, referent_id_col_name, token_set_col_name)
	try:
		prev_coref_row = prev_coref_rows.loc[prev_coref_rows["ROUND"].argmax()]
		current_tokens = row[token_set_col_name]
		prev_tokens = prev_coref_row[token_set_col_name]
		result = set_overlap(current_tokens, prev_tokens)
	except ValueError:
		result = OVERLAP_NULL_VALUE
	return result


def set_overlap(first: FrozenSet[T], second: FrozenSet[T]) -> float:
	# print("First: " + ",".join(sorted(first)), file=sys.stderr)
	# print("Complement: " + ",".join(sorted(complement)), file=sys.stderr)
	# if first and second:
	intersection = first.intersection(second)
	union = first.union(second)
	result = len(intersection) / len(union)
	# else:
	#	# Don't compute overlap for utterances which don't have any relevant tokens
	#	result = OVERLAP_NULL_VALUE
	return result


def baseline_token_overlap(row: pd.Series, df: pd.DataFrame, coref_chain_seq_no_col_name: str,
						   token_set_col_name: str) -> int:
	prev_coref_rows = __analogous_coref_rows(row, df, coref_chain_seq_no_col_name, token_set_col_name)
	prev_token_sets = prev_coref_rows[token_set_col_name]

	current_tokens = row[token_set_col_name]
	overlaps = prev_token_sets.transform(lambda prev_tokens: set_overlap(current_tokens, prev_tokens))
	return overlaps.mean()


def baseline_referent_token_overlap(row: pd.Series, df: pd.DataFrame, coref_chain_seq_no_col_name: str,
									referent_id_col_name: str,
									token_set_col_name: str) -> int:
	prev_coref_rows = __analogous_referent_coref_rows(row, df, coref_chain_seq_no_col_name, referent_id_col_name,
													  token_set_col_name)
	prev_token_sets = prev_coref_rows[token_set_col_name]

	current_tokens = row[token_set_col_name]
	overlaps = prev_token_sets.transform(lambda prev_tokens: set_overlap(current_tokens, prev_tokens))
	return overlaps.mean()


def __analogous_coref_rows(row: pd.Series, df: pd.DataFrame, coref_chain_seq_no_col_name: str,
						   token_set_col_name: str) -> pd.DataFrame:
	dyad = row["DYAD"]
	coref_chain_seq_no = row[coref_chain_seq_no_col_name]
	return df.loc[
		(df["DYAD"] == dyad) & (df[coref_chain_seq_no_col_name] == coref_chain_seq_no) & (
			df[token_set_col_name].str.len() > 0)]


def __analogous_referent_coref_rows(row: pd.Series, df: pd.DataFrame, coref_chain_seq_no_col_name: str,
									referent_id_col_name: str,
									token_set_col_name: str) -> pd.DataFrame:
	dyad = row["DYAD"]
	referent_id = row[referent_id_col_name]
	coref_chain_seq_no = row[coref_chain_seq_no_col_name]
	return df.loc[
		(df["DYAD"] == dyad) & (df[referent_id_col_name] == referent_id) & (
			df[coref_chain_seq_no_col_name] == coref_chain_seq_no) & (
			df[token_set_col_name].str.len() > 0)]


def __prev_complement_rows(current_row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
						   token_set_col_name: str) -> Iterator[
	pd.Series]:
	dyad = current_row["DYAD"]
	referent_id = current_row[referent_id_col_name]
	while True:
		current_round = current_row["ROUND"]
		# print("Current round: {}".format(current_round), file=sys.stderr)
		current_instructor = current_row["INSTRUCTOR"]
		# print("Current instructor to find complement of: {}".format(current_instructor), file=sys.stderr)
		# https://stackoverflow.com/a/37335656/1391325
		prev_complement_rows = df.loc[
			(df["DYAD"] == dyad) & (df[referent_id_col_name] == referent_id) & (df["ROUND"] < current_round) & (
				df["INSTRUCTOR"] != current_instructor) & (df[token_set_col_name].str.len() > 0)]
		# print("prev_complement_rows" + prev_complement_rows)
		try:
			last_prev_complement_row = prev_complement_rows.loc[prev_complement_rows["ROUND"].argmax()]
			yield last_prev_complement_row
			current_row = last_prev_complement_row
		except ValueError:
			break


def __prev_either_coref_rows(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
							 token_set_col_name: str) -> pd.DataFrame:
	dyad = row["DYAD"]
	referent_id = row[referent_id_col_name]
	current_round = row["ROUND"]
	return df.loc[
		(df["DYAD"] == dyad) & (df[referent_id_col_name] == referent_id) & (df["ROUND"] < current_round) & (
			df[token_set_col_name].str.len() > 0)]


def __prev_self_coref_rows(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
						   token_set_col_name: str) -> pd.DataFrame:
	dyad = row["DYAD"]
	referent_id = row[referent_id_col_name]
	current_round = row["ROUND"]
	current_instructor = row["INSTRUCTOR"]
	return df.loc[
		(df["DYAD"] == dyad) & (df[referent_id_col_name] == referent_id) & (df["ROUND"] < current_round) & (
			df["INSTRUCTOR"] == current_instructor) & (df[token_set_col_name].str.len() > 0)]


def __token_type_overlaps_either(df: pd.DataFrame, referent_id_col_name: str, token_set_col_name: str):
	print("Calculating either overlap for \"{}\".".format(token_set_col_name), file=sys.stderr)
	metric = Metric.EITHER

	coref_chain_seq_no_col_name = qualified_col_name(token_set_col_name, Measurement.COREF_SEQ, metric,
													 Aggregation.NONE)
	df[coref_chain_seq_no_col_name] = df.apply(
		lambda row: either_coref_chain_seq_no(row, df, referent_id_col_name, token_set_col_name), axis=1)

	overlap_col_name = qualified_col_name(token_set_col_name, Measurement.OVERLAP, metric, Aggregation.NONE)
	df[overlap_col_name] = df.apply(
		lambda row: either_token_overlap(row, df, referent_id_col_name, token_set_col_name), axis=1)
	# NOTE: This logic is dependent on the previous ones already setting coreference chain sequence numbers
	print("Calculating either baseline overlap for \"{}\".".format(token_set_col_name), file=sys.stderr)
	baseline_overlap_col_name = qualified_col_name(token_set_col_name, Measurement.OVERLAP, metric,
												   Aggregation.BASELINE)
	df[baseline_overlap_col_name] = df.apply(
		lambda row: baseline_token_overlap(row, df, coref_chain_seq_no_col_name, token_set_col_name), axis=1)
	print("Calculating either referent baseline overlap for \"{}\".".format(token_set_col_name), file=sys.stderr)
	referent_baseline_overlap_col_name = qualified_col_name(token_set_col_name, Measurement.OVERLAP, metric,
															Aggregation.REFERENT_BASELINE)
	df[referent_baseline_overlap_col_name] = df.apply(
		lambda row: baseline_referent_token_overlap(row, df, coref_chain_seq_no_col_name, referent_id_col_name,
													token_set_col_name), axis=1)


def __token_type_overlaps_other(df: pd.DataFrame, referent_id_col_name: str, token_set_col_name: str):
	print("Calculating other overlap for \"{}\".".format(token_set_col_name), file=sys.stderr)
	metric = Metric.SELF

	coref_chain_seq_no_col_name = qualified_col_name(token_set_col_name, Measurement.COREF_SEQ, metric,
													 Aggregation.NONE)
	df[coref_chain_seq_no_col_name] = df.apply(
		lambda row: complement_coref_chain_seq_no(row, df, referent_id_col_name, token_set_col_name), axis=1)

	overlap_col_name = qualified_col_name(token_set_col_name, Measurement.OVERLAP, metric, Aggregation.NONE)
	df[overlap_col_name] = df.apply(
		lambda row: complement_token_overlap(row, df, referent_id_col_name, token_set_col_name), axis=1)
	# NOTE: This logic is dependent on the previous ones already setting coreference chain sequence numbers
	print("Calculating other baseline overlap for \"{}\".".format(token_set_col_name), file=sys.stderr)
	baseline_overlap_col_name = qualified_col_name(token_set_col_name, Measurement.OVERLAP, metric,
												   Aggregation.BASELINE)
	df[baseline_overlap_col_name] = df.apply(
		lambda row: baseline_token_overlap(row, df, coref_chain_seq_no_col_name, token_set_col_name), axis=1)
	print("Calculating other referent baseline overlap for \"{}\".".format(token_set_col_name), file=sys.stderr)
	referent_baseline_overlap_col_name = qualified_col_name(token_set_col_name, Measurement.OVERLAP, metric,
															Aggregation.REFERENT_BASELINE)
	df[referent_baseline_overlap_col_name] = df.apply(
		lambda row: baseline_referent_token_overlap(row, df, coref_chain_seq_no_col_name, referent_id_col_name,
													token_set_col_name), axis=1)


def __token_type_overlaps_self(df: pd.DataFrame, referent_id_col_name: str, token_set_col_name: str):
	print("Calculating self overlap for \"{}\".".format(token_set_col_name), file=sys.stderr)
	metric = Metric.SELF

	coref_chain_seq_no_col_name = qualified_col_name(token_set_col_name, Measurement.COREF_SEQ, metric,
													 Aggregation.NONE)
	df[coref_chain_seq_no_col_name] = df.apply(
		lambda row: self_coref_chain_seq_no(row, df, referent_id_col_name, token_set_col_name), axis=1)

	overlap_col_name = qualified_col_name(token_set_col_name, Measurement.OVERLAP, metric, Aggregation.NONE)
	df[overlap_col_name] = df.apply(
		lambda row: self_token_overlap(row, df, referent_id_col_name, token_set_col_name), axis=1)
	# NOTE: This logic is dependent on the previous ones already setting coreference chain sequence numbers
	print("Calculating self baseline overlap for \"{}\".".format(token_set_col_name), file=sys.stderr)
	baseline_overlap_col_name = qualified_col_name(token_set_col_name, Measurement.OVERLAP, metric,
												   Aggregation.BASELINE)
	df[baseline_overlap_col_name] = df.apply(
		lambda row: baseline_token_overlap(row, df, coref_chain_seq_no_col_name, token_set_col_name), axis=1)
	print("Calculating self referent baseline overlap for \"{}\".".format(token_set_col_name), file=sys.stderr)
	referent_baseline_overlap_col_name = qualified_col_name(token_set_col_name, Measurement.OVERLAP, metric,
															Aggregation.REFERENT_BASELINE)
	df[referent_baseline_overlap_col_name] = df.apply(
		lambda row: baseline_referent_token_overlap(row, df, coref_chain_seq_no_col_name, referent_id_col_name,
													token_set_col_name), axis=1)


def __token_type_overlap(df: pd.DataFrame, referent_id_col_name: str, token_set_col_name: str):
	"""
	See <https://stackoverflow.com/a/46402641/1391325>
	:param df: The DataFrame instance to process.
	:param referent_id_col_name: The name of the column to use as an identifier for each referent entity.
	:param token_set_col_name: The name of the column to use as the relevant tokens to process.
	"""
	__token_type_overlaps_self(df, referent_id_col_name, token_set_col_name)
	__token_type_overlaps_other(df, referent_id_col_name, token_set_col_name)
	__token_type_overlaps_either(df, referent_id_col_name, token_set_col_name)


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Measure referent token type overlap in coreference chains in each game session, using only instructor language to build coreference chains.")
	result.add_argument("inpath", metavar="INPATH",
						help="The file to process.")
	result.add_argument("-r", "--referent", metavar="COL_NAME", required=True,
						help="The column to use as a referent ID.")
	result.add_argument("-t", "--tokens", metavar="COL_NAME", required=True,
						help="The column to use as relevant tokens.")
	return result


def __main(args):
	inpath = args.inpath
	token_set_col_name = args.tokens
	referent_id_col_name = args.referent
	print("Reading \"{}\" using tokens from column \"{}\" and values from column \"{}\" as referent IDs.".format(inpath,
																												 token_set_col_name,
																												 referent_id_col_name),
		  file=sys.stderr)
	round_tokens = read_round_tokens(inpath)

	__token_type_overlap(round_tokens, referent_id_col_name, token_set_col_name)

	round_tokens.sort_values(["DYAD", "REFERENT", "ROUND", "INSTRUCTOR"], inplace=True)
	prettify_token_set_series(round_tokens)
	round_tokens.reset_index(drop=True, inplace=True)
	round_tokens.to_csv(sys.stdout, index_label="INDEX", sep="\t", na_rep=OUTPUT_NA_VALUE)


def __token_set_repr(tokens: Iterable[str]) -> str:
	return ','.join(sorted(tokens))


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
