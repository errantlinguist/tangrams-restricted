#!/usr/bin/env python3

import argparse
import csv
import re
import sys
from enum import Enum, unique
from typing import FrozenSet, Iterable, Iterator, Tuple, TypeVar

import numpy as np
import pandas as pd

CELL_MULTIVALUE_DELIM_PATTERN = re.compile("\\s*,\\s*")
OUTPUT_NA_VALUE = "N/A"
OVERLAP_NULL_VALUE = np.NaN

T = TypeVar('T')


@unique
class DataColumn(Enum):
	COREF_SEQ = "_COREF_SEQ"
	OVERLAP = "_OVERLAP"


@unique
class Metric(Enum):
	SELF = "_SELF"
	OTHER = "_OTHER"
	EITHER = "_EITHER"


def complement_coref_chain_seq_no(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
								  token_set_col_name: str) -> int:
	prev_coref_chain_rows = tuple(__prev_complement_rows(row, df, referent_id_col_name, token_set_col_name))
	return len(prev_coref_chain_rows) + 1


def complement_token_overlap(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
							 token_set_col_name: str) -> int:
	prev_coref_chain_rows = tuple(__prev_complement_rows(row, df, referent_id_col_name, token_set_col_name))
	if prev_coref_chain_rows:
		prev_coref_chain_row = max(prev_coref_chain_rows, key=lambda r: r["ROUND"])
		current_tokens = row[token_set_col_name]
		prev_tokens = prev_coref_chain_row[token_set_col_name]
		result = set_overlap(current_tokens, prev_tokens)
	else:
		result = OVERLAP_NULL_VALUE
	return result


def either_coref_chain_seq_no(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
							  token_set_col_name: str) -> int:
	prev_coref_chain_rows = __prev_either_coref_chain_rows(row, df, referent_id_col_name, token_set_col_name)
	return prev_coref_chain_rows.shape[0] + 1


def either_token_overlap(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
						 token_set_col_name: str) -> int:
	prev_coref_chain_rows = __prev_either_coref_chain_rows(row, df, referent_id_col_name, token_set_col_name)
	if prev_coref_chain_rows.shape[0] > 0:
		prev_coref_chain_row = prev_coref_chain_rows.loc[prev_coref_chain_rows["ROUND"].argmax()]
		current_tokens = row[token_set_col_name]
		prev_tokens = prev_coref_chain_row[token_set_col_name]
		result = set_overlap(current_tokens, prev_tokens)
	else:
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
	return pd.read_csv(inpath, sep="\t", dialect=csv.excel_tab, encoding="utf-8", float_precision="high",
					   memory_map=True, na_filter=False,
					   converters={"RELEVANT_TOKENS_REFERENT": parse_set, "RELEVANT_TOKENS_SHAPE": parse_set}, **kwargs)


def self_coref_chain_seq_no(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
							token_set_col_name: str) -> int:
	prev_coref_chain_rows = __prev_self_coref_chain_rows(row, df, referent_id_col_name, token_set_col_name)
	return prev_coref_chain_rows.shape[0] + 1


def self_token_overlap(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
					   token_set_col_name: str) -> int:
	prev_coref_chain_rows = __prev_self_coref_chain_rows(row, df, referent_id_col_name, token_set_col_name)
	if prev_coref_chain_rows.shape[0] > 0:
		prev_coref_chain_row = prev_coref_chain_rows.loc[prev_coref_chain_rows["ROUND"].argmax()]
		current_tokens = row[token_set_col_name]
		prev_tokens = prev_coref_chain_row[token_set_col_name]
		result = set_overlap(current_tokens, prev_tokens)
	else:
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


def zip_previous_row_values(df: pd.DataFrame, col_name: str) -> Iterator[Tuple[T, T]]:
	return zip(df[col_name], df[col_name].shift(-1))


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


def __prev_either_coref_chain_rows(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
								   token_set_col_name: str) -> pd.DataFrame:
	dyad = row["DYAD"]
	referent_id = row[referent_id_col_name]
	current_round = row["ROUND"]
	return df.loc[
		(df["DYAD"] == dyad) & (df[referent_id_col_name] == referent_id) & (df["ROUND"] < current_round) & (
			df[token_set_col_name].str.len() > 0)]


def __prev_self_coref_chain_rows(row: pd.Series, df: pd.DataFrame, referent_id_col_name: str,
								 token_set_col_name: str) -> pd.DataFrame:
	dyad = row["DYAD"]
	referent_id = row[referent_id_col_name]
	current_round = row["ROUND"]
	current_instructor = row["INSTRUCTOR"]
	return df.loc[
		(df["DYAD"] == dyad) & (df[referent_id_col_name] == referent_id) & (df["ROUND"] < current_round) & (
			df["INSTRUCTOR"] == current_instructor) & (df[token_set_col_name].str.len() > 0)]


def __token_type_overlap(df: pd.DataFrame, referent_id_col_name: str, token_set_col_name: str):
	"""
	See <https://stackoverflow.com/a/46402641/1391325>
	:param df: The DataFrame instance to process.
	:param referent_id_col_name: The name of the column to use as an identifier for each referent entity.
	:param token_set_col_name: The name of the column to use as the relevant tokens to process.
	"""
	print("Calculating self overlap for \"{}\".".format(token_set_col_name), file=sys.stderr)
	self_coref_chain_seq_no_col_name = token_set_col_name + DataColumn.COREF_SEQ.value + Metric.SELF.value
	df[self_coref_chain_seq_no_col_name] = df.apply(
		lambda row: self_coref_chain_seq_no(row, df, referent_id_col_name, token_set_col_name), axis=1)
	self_overlap_col_name = token_set_col_name + DataColumn.OVERLAP.value + Metric.SELF.value
	df[self_overlap_col_name] = df.apply(
		lambda row: self_token_overlap(row, df, referent_id_col_name, token_set_col_name), axis=1)

	print("Calculating other overlap for \"{}\".".format(token_set_col_name), file=sys.stderr)
	other_coref_chain_seq_no_col_name = token_set_col_name + DataColumn.COREF_SEQ.value + Metric.OTHER.value
	df[other_coref_chain_seq_no_col_name] = df.apply(
		lambda row: complement_coref_chain_seq_no(row, df, referent_id_col_name, token_set_col_name), axis=1)
	other_overlap_col_name = token_set_col_name + DataColumn.OVERLAP.value + Metric.OTHER.value
	df[other_overlap_col_name] = df.apply(
		lambda row: complement_token_overlap(row, df, referent_id_col_name, token_set_col_name), axis=1)

	print("Calculating either overlap for \"{}\".".format(token_set_col_name), file=sys.stderr)
	either_coref_chain_seq_no_col_name = token_set_col_name + DataColumn.COREF_SEQ.value + Metric.EITHER.value
	df[either_coref_chain_seq_no_col_name] = df.apply(
		lambda row: either_coref_chain_seq_no(row, df, referent_id_col_name, token_set_col_name), axis=1)
	either_overlap_col_name = token_set_col_name + DataColumn.OVERLAP.value + Metric.EITHER.value
	df[either_overlap_col_name] = df.apply(
		lambda row: either_token_overlap(row, df, referent_id_col_name, token_set_col_name), axis=1)


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
	round_tokens.to_csv(sys.stdout, index_label="INDEX", sep="\t", na_rep=OUTPUT_NA_VALUE)


def __token_set_repr(tokens: Iterable[str]) -> str:
	return ','.join(sorted(tokens))


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
