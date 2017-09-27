#!/usr/bin/env python3

import argparse
import csv
import sys
from collections import defaultdict
from decimal import Decimal
from typing import Callable, Dict, FrozenSet, Iterable, List, TypeVar

import numpy as np
from statsmodels import robust

import instructor_relevant_tokens_metrics

COL_DELIM = "\t"
T = TypeVar('T')


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Measure referent token type overlap in coreference chains in each game session, using only instructor language to build coreference chains.")
	result.add_argument("inpath", metavar="INPATH",
						help="The file to process.")
	result.add_argument("-t", "--tokens", metavar="COL_NAME", required=True,
						help="The column to use as relevant tokens.")
	return result


def read_nonempty_coref_seq_token_sets(inpath: str, self_coref_seq_no_col_name: str, token_set_col_name: str) -> Dict[
	int, List[FrozenSet[str]]]:
	result = defaultdict(list)
	with open(inpath, 'r') as infile:
		rows = csv.reader(infile, dialect="excel-tab")
		col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
		token_set_col_idx = col_idxs[token_set_col_name]
		self_coref_seq_no_col_idx = col_idxs[self_coref_seq_no_col_name]
		for row in rows:
			token_str = row[token_set_col_idx]
			tokens = instructor_relevant_tokens_metrics.parse_set(token_str)
			if tokens:
				coref_seq_no = int(row[self_coref_seq_no_col_idx])
				result[coref_seq_no].append(tokens)
	return result


def set_overlap_high_precision(first: FrozenSet[T], second: FrozenSet[T],
							   decimal_factory: Callable[[int], T]) -> T:
	intersection = first.intersection(second)
	union = first.union(second)
	return decimal_factory(len(intersection)) / decimal_factory(len(union))


def __main(args):
	inpath = args.inpath
	token_set_col_name = args.tokens
	print("Reading \"{}\" using tokens from column \"{}\".".format(inpath,
																   token_set_col_name),
		  file=sys.stderr)

	self_coref_seq_no_col_name = instructor_relevant_tokens_metrics.qualified_col_name(token_set_col_name,
																					   instructor_relevant_tokens_metrics.Measurement.COREF_SEQ,
																					   instructor_relevant_tokens_metrics.Metric.SELF,
																					   instructor_relevant_tokens_metrics.Aggregation.NONE)

	coref_seq_token_sets = read_nonempty_coref_seq_token_sets(inpath, self_coref_seq_no_col_name, token_set_col_name)
	print("Read token sets for {} coreference sequence step(s).".format(len(coref_seq_token_sets)), file=sys.stderr)

	outfile = sys.stdout

	decimal_cache = {}

	def fetch_decimal(value: int) -> np.dtype:
		try:
			result = decimal_cache[value]
		except KeyError:
			result = np.longfloat(value)
			decimal_cache[value] = result
		return result

	overlap_cache = {}

	def fetch_overlap(first: FrozenSet[T], second: FrozenSet[T]) -> Decimal:
		key = (first, second)
		try:
			result = overlap_cache[key]
		except KeyError:
			result = set_overlap_high_precision(first, second, fetch_decimal)
			overlap_cache[key] = result
		return result

	print("Calculating aggregates.", file=sys.stderr)
	print(COL_DELIM.join(("seq", "mean", "stdev", "sem", "median", "mad")), file=outfile)
	for coref_seq_no, token_sets in sorted(coref_seq_token_sets.items()):
		overlaps = tuple(fetch_overlap(first, other) for first in token_sets for other in
						 token_sets)
		mean = np.mean(overlaps)
		stdev = np.std(overlaps)
		sem = np.std(overlaps)
		median = np.median(overlaps)
		mad = robust.mad(overlaps)
		row = (coref_seq_no, mean, stdev, sem, median, mad)
		print(COL_DELIM.join(str(cell) for cell in row), file=outfile)


def __token_set_repr(tokens: Iterable[str]) -> str:
	return ','.join(sorted(tokens))


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
