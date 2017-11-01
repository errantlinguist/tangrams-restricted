#!/usr/bin/env python3

import argparse
import csv
import itertools
import statistics
import sys
from collections import defaultdict
from decimal import Decimal
from typing import Callable, Dict, FrozenSet, Generic, Iterable, Iterator, List, Mapping, Sequence, Tuple, TypeVar

import numpy as np

import instructor_relevant_tokens_metrics

# from statsmodels import robust

COL_DELIM = "\t"
K = TypeVar('K')
V = TypeVar('V')


class CachingFactory(Generic[K, V]):
	def __init__(self, factory: Callable[[K], V]):
		self.factory = factory
		self.cache = {}

	def __call__(self, key: K) -> V:
		try:
			result = self.cache[key]
		except KeyError:
			result = self.factory(key)
			self.cache[key] = result
		return result


class CachingSetOverlapFactory(Generic[K, V]):
	def __init__(self, decimal_factory: Callable[[int], V]):
		self.decimal_factory = decimal_factory
		self.overlap_cache = {}

	def __call__(self, first: FrozenSet[K], second: FrozenSet[K]):
		key = (first, second)
		try:
			result = self.overlap_cache[key]
		except KeyError:
			result = set_overlap_high_precision(first, second, self.decimal_factory)
			self.overlap_cache[key] = result
		return result


class SetOverlapAggregatorDecimal(Generic[K, V]):
	def __init__(self, decimal_constructor: Callable[[int], V]):
		self.decimal_factory = CachingFactory(decimal_constructor)
		self.overlap_factory = CachingSetOverlapFactory(self.decimal_factory)

	def __call__(self, current_token_sets: Sequence[FrozenSet[str]],
				 prev_token_sets: Sequence[FrozenSet[str]]):
		overlaps = tuple(self.overlaps(current_token_sets, prev_token_sets))
		mean = statistics.mean(overlaps)
		stdev = statistics.stdev(overlaps)
		sample_size = self.decimal_factory(len(overlaps))
		sem = stdev / sample_size.sqrt()
		# median = np.median(overlaps)
		# mad = robust.mad(overlaps)
		return len(current_token_sets), len(overlaps), mean, stdev, sem

	def overlaps(self, current_token_sets: Sequence[FrozenSet[str]],
				 prev_token_sets: Sequence[FrozenSet[str]]) -> Iterator[V]:
		return (self.overlap_factory(token_set, prev_token_set) for token_set in current_token_sets for prev_token_set
				in
				prev_token_sets)


class SetOverlapAggregatorNumpy(Generic[K, V]):
	def __init__(self, decimal_constructor: Callable[[int], V]):
		self.decimal_factory = CachingFactory(decimal_constructor)
		self.overlap_factory = CachingSetOverlapFactory(self.decimal_factory)

	def __call__(self, current_token_sets: Sequence[FrozenSet[K]], prev_token_sets: Sequence[FrozenSet[K]]):
		overlaps = np.array(
			tuple(self.overlaps(current_token_sets, prev_token_sets)))
		mean = np.mean(overlaps)
		stdev = np.std(overlaps)
		sample_size = self.decimal_factory(len(overlaps))
		sem = stdev / np.sqrt(sample_size)
		# median = np.median(overlaps)
		# mad = robust.mad(overlaps)
		return len(current_token_sets), len(overlaps), mean, stdev, sem

	def overlaps(self, current_token_sets: Sequence[FrozenSet[str]],
				 prev_token_sets: Sequence[FrozenSet[str]]) -> Iterator[V]:
		return (self.overlap_factory(token_set, prev_token_set) for token_set in current_token_sets for prev_token_set
				in
				prev_token_sets)


def coref_seq_no_overlaps(coref_seq_token_sets: Mapping[int, Sequence[FrozenSet[str]]],
						  aggregator: Callable[[FrozenSet[K], FrozenSet[K]], V]) -> Tuple[int, Tuple[V, ...]]:
	sorted_coref_seq_token_sets = tuple(sorted(coref_seq_token_sets.items()))
	for coref_token_sets_to_calculate, prev_coref_token_sets in zip(sorted_coref_seq_token_sets[1:],
																	sorted_coref_seq_token_sets):
		current_coref_seq_no, current_token_sets = coref_token_sets_to_calculate
		prev_coref_seq_no, prev_token_sets = prev_coref_token_sets
		assert prev_coref_seq_no < current_coref_seq_no
		print("Calculating overlaps of coref seq. no {} with coref seq. no {}.".format(current_coref_seq_no,
																					   prev_coref_seq_no),
			  file=sys.stderr)

		yield current_coref_seq_no, aggregator(current_token_sets, prev_token_sets)


def read_nonempty_coref_seq_token_sets(inpath: str, self_coref_seq_no_col_name: str, token_set_col_name: str) -> Dict[
	int, List[FrozenSet[str]]]:
	result = defaultdict(list)
	with open(inpath, 'r') as infile:
		rows = csv.reader(infile, dialect=csv.excel_tab)
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


def set_overlap_high_precision(first: FrozenSet[K], second: FrozenSet[K],
							   decimal_factory: Callable[[int], V]) -> V:
	intersection = first.intersection(second)
	union = first.union(second)
	return decimal_factory(len(intersection)) / decimal_factory(len(union))


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Measure referent token type overlap in coreference chains in each game session, using only instructor language to build coreference chains.")
	result.add_argument("inpath", metavar="INPATH",
						help="The file to process.")
	result.add_argument("-p", "--high-precision", action="store_true",
						help="If this flag is set, values are calculated using arbitrary precision, which is much slower than using hardware floating-point arithmetic.")
	result.add_argument("-t", "--tokens", metavar="COL_NAME", required=True,
						help="The column to use as relevant tokens.")
	return result


def __main(args):
	inpath = args.inpath
	token_set_col_name = args.tokens
	print("Reading \"{}\" using tokens from column \"{}\".".format(inpath,
																   token_set_col_name),
		  file=sys.stderr)

	self_coref_seq_no_col_name = instructor_relevant_tokens_metrics.create_qualified_col_name(token_set_col_name,
																							  instructor_relevant_tokens_metrics.Measurement.COREF_SEQ,
																							  instructor_relevant_tokens_metrics.Metric.SELF,
																							  instructor_relevant_tokens_metrics.Aggregation.NONE)

	coref_seq_token_sets = read_nonempty_coref_seq_token_sets(inpath, self_coref_seq_no_col_name, token_set_col_name)
	print("Read token sets for {} coreference sequence step(s).".format(len(coref_seq_token_sets)), file=sys.stderr)

	outfile = sys.stdout
	if args.high_precision:
		print("Using arbitrary-precision decimal arithmetic.", file=sys.stderr)
		aggregator = SetOverlapAggregatorDecimal(Decimal)
	else:
		print("Using numpy for hardware floating-point arithmetic.", file=sys.stderr)
		aggregator = SetOverlapAggregatorNumpy(np.longfloat)

	print("Calculating aggregates.", file=sys.stderr)
	print(COL_DELIM.join(("seq", "count", "comparisons", "mean", "std", "sem")), file=outfile)
	# print(COL_DELIM.join(("seq", "count", "comparisons", "mean", "std", "sem", "median", "mad")), file=outfile)
	for current_coref_seq_no, aggs in coref_seq_no_overlaps(coref_seq_token_sets, aggregator):
		row = itertools.chain((current_coref_seq_no,), aggs)
		print(COL_DELIM.join(str(cell) for cell in row), file=outfile)


def __token_set_repr(tokens: Iterable[str]) -> str:
	return ','.join(sorted(tokens))


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
