#!/usr/bin/env python3

import argparse
import csv
import statistics
import sys
from collections import defaultdict
from decimal import Decimal
from typing import Callable, Dict, FrozenSet, Iterable, List, Mapping, Sequence, Tuple, TypeVar

import numpy as np

import instructor_relevant_tokens_dialogic_convergence
import instructor_relevant_tokens_metrics

COL_DELIM = "\t"
K = TypeVar('K')
V = TypeVar('V')

_NUMPY_DECIMAL_VALUE_TYPE = np.longfloat


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Measure referent token type overlap in coreference chains in each game session, using only instructor language to build coreference chains.")
	result.add_argument("inpath", metavar="INPATH",
						help="The file to process.")
	result.add_argument("-p", "--high-precision", action="store_true",
						help="If this flag is set, values are calculated using arbitrary precision, which is much slower than using hardware floating-point arithmetic.")
	result.add_argument("-r", "--referent", metavar="COL_NAME", required=True,
						help="The column to use as a referent ID.")
	result.add_argument("-t", "--tokens", metavar="COL_NAME", required=True,
						help="The column to use as relevant tokens.")
	return result


def coref_seq_no_overlaps(coref_seq_token_sets: Mapping[int, Mapping[str, Sequence[FrozenSet[str]]]], overlap_factory:
Callable[[FrozenSet[str], FrozenSet[str]], Iterable[V]]) -> Dict[int, Tuple[V]]:
	sorted_coref_seq_token_sets = tuple(sorted(coref_seq_token_sets.items()))

	result = {}
	for coref_token_sets_to_calculate, prev_coref_token_sets in zip(sorted_coref_seq_token_sets[1:],
																	sorted_coref_seq_token_sets):
		current_coref_seq_no, current_token_sets = coref_token_sets_to_calculate
		prev_coref_seq_no, prev_token_sets = prev_coref_token_sets
		assert prev_coref_seq_no < current_coref_seq_no
		for referent_id, current_referent_token_sets in current_token_sets.items():
			overlaps = []
			try:
				print("Calculating overlaps of coref seq. no {} with coref seq. no {} for referent ID \"{}\"".format(
					current_coref_seq_no,
					prev_coref_seq_no, referent_id),
					file=sys.stderr)
				prev_referent_token_sets = prev_token_sets[referent_id]
				overlaps.extend(overlap_factory(current_referent_token_sets, prev_referent_token_sets))
			except KeyError:
				print("No token sets found for for referent ID \"{}\" and coref seq. no {}".format(referent_id,
																								   current_coref_seq_no),
					  file=sys.stderr)
			result[current_coref_seq_no] = overlaps
	return result


class _ArithmeticDecimal(object):
	@staticmethod
	def create_array(all_overlaps: Iterable[Decimal]) -> Tuple[Decimal, ...]:
		return tuple(all_overlaps)

	@staticmethod
	def mean(all_overlaps: Iterable[Decimal]) -> Decimal:
		return statistics.mean(all_overlaps)

	@staticmethod
	def stdev(all_overlaps: Iterable[Decimal]) -> Decimal:
		return statistics.stdev(all_overlaps)

	@staticmethod
	def sqrt(value: Decimal) -> Decimal:
		return value.sqrt()


class _ArithmeticNumpy(object):
	@staticmethod
	def create_array(all_overlaps) -> np.array:
		return np.array(tuple(all_overlaps))

	@staticmethod
	def mean(all_overlaps: np.array):
		return all_overlaps.mean()

	@staticmethod
	def stdev(all_overlaps: np.array):
		return all_overlaps.std()

	@staticmethod
	def sqrt(value: _NUMPY_DECIMAL_VALUE_TYPE) -> Decimal:
		return np.sqrt(value)


def read_nonempty_coref_seq_referent_token_sets(inpath: str, self_coref_seq_no_col_name: str, token_set_col_name: str,
												referent_id_col_name: str) -> Dict[
	int, Dict[str, List[FrozenSet[str]]]]:
	result = defaultdict(lambda: defaultdict(list))
	with open(inpath, 'r') as infile:
		rows = csv.reader(infile, dialect="excel-tab")
		col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
		token_set_col_idx = col_idxs[token_set_col_name]
		self_coref_seq_no_col_idx = col_idxs[self_coref_seq_no_col_name]
		referent_id_col_name_idx = col_idxs[referent_id_col_name]
		for row in rows:
			token_str = row[token_set_col_idx]
			tokens = instructor_relevant_tokens_metrics.parse_set(token_str)
			if tokens:
				coref_seq_no = int(row[self_coref_seq_no_col_idx])
				referent_id = row[referent_id_col_name_idx]
				result[coref_seq_no][referent_id].append(tokens)
	return result


def __main(args):
	inpath = args.inpath
	token_set_col_name = args.tokens
	referent_id_col_name = args.referent
	print("Reading \"{}\" using tokens from column \"{}\" and values from \"{}\" as referent IDs.".format(inpath,
																										  token_set_col_name,
																										  referent_id_col_name),
		  file=sys.stderr)

	self_coref_seq_no_col_name = instructor_relevant_tokens_metrics.qualified_col_name(token_set_col_name,
																					   instructor_relevant_tokens_metrics.Measurement.COREF_SEQ,
																					   instructor_relevant_tokens_metrics.Metric.SELF,
																					   instructor_relevant_tokens_metrics.Aggregation.NONE)

	coref_seq_token_sets = read_nonempty_coref_seq_referent_token_sets(inpath,
																	   self_coref_seq_no_col_name,
																	   token_set_col_name, referent_id_col_name)
	print("Read token sets for {} coreference sequence step(s).".format(len(coref_seq_token_sets)), file=sys.stderr)

	outfile = sys.stdout
	if args.high_precision:
		print("Using arbitrary-precision decimal arithmetic.", file=sys.stderr)
		arithmetic = _ArithmeticDecimal
		decimal_constructor = Decimal
		aggregator = instructor_relevant_tokens_dialogic_convergence.SetOverlapAggregatorDecimal(decimal_constructor)
	else:
		print("Using numpy for hardware floating-point arithmetic.", file=sys.stderr)
		arithmetic = _ArithmeticNumpy
		decimal_constructor = _NUMPY_DECIMAL_VALUE_TYPE
		aggregator = instructor_relevant_tokens_dialogic_convergence.SetOverlapAggregatorNumpy(decimal_constructor)

	print("Calculating aggregates.", file=sys.stderr)
	print(COL_DELIM.join(("desc", "value")), file=outfile)
	overlaps = coref_seq_no_overlaps(coref_seq_token_sets,
									 lambda current_token_sets, prev_token_sets: aggregator.overlaps(current_token_sets,
																									 prev_token_sets))
	all_overlaps = arithmetic.create_array(
		overlap for seq_no_overlaps in overlaps.values() for overlap in seq_no_overlaps)
	print(COL_DELIM.join(("comparisons", str(len(all_overlaps)))), file=outfile)
	mean = arithmetic.mean(all_overlaps)
	print(COL_DELIM.join(("mean", str(mean))), file=outfile)
	stdev = arithmetic.stdev(all_overlaps)
	print(COL_DELIM.join(("std", str(stdev))), file=outfile)
	total_sample_size = decimal_constructor(len(all_overlaps))
	sem = stdev / arithmetic.sqrt(total_sample_size)
	print(COL_DELIM.join(("sem", str(sem))), file=outfile)

	max_coref_seq_no = max(overlaps.keys())
	print(COL_DELIM.join(("max_seq_len", str(max_coref_seq_no))), file=outfile)
	seq_overlaps = tuple(((seq_no, arithmetic.create_array(seq_overlaps)) for seq_no, seq_overlaps in overlaps.items()))

	coref_seq_stdevs = tuple((seq_no, arithmetic.stdev(seq_overlaps)) for seq_no, seq_overlaps in seq_overlaps)

	mean_coref_seq_stdev = arithmetic.mean(arithmetic.create_array(stdev for seq_no, stdev in coref_seq_stdevs))
	print(COL_DELIM.join(("mean_coref_seq_std", str(mean_coref_seq_stdev))), file=outfile)

	seq_means = ((seq_no, arithmetic.mean(overlaps)) for seq_no, overlaps in seq_overlaps)

	for seq_no, stdev in sorted(coref_seq_stdevs):
		print(COL_DELIM.join(("mean_stdev_{}".format(seq_no), str(mean))), file=outfile)

	print("\n\n", file=outfile)
	print(COL_DELIM.join(("seq", "mean", "std", "sem")), file=outfile)
	for seq_no, mean in sorted(seq_means, key=lambda item: item[0]):
		stdev = next(stdev for stdev_seq_no, stdev in coref_seq_stdevs if stdev_seq_no == seq_no)
		seq_overlaps = overlaps[seq_no]
		sem = stdev / arithmetic.sqrt(decimal_constructor(len(seq_overlaps)))
		row = (seq_no, mean, stdev, sem)
		print(COL_DELIM.join(str(col) for col in row), file=outfile)


def __token_set_repr(tokens: Iterable[str]) -> str:
	return ','.join(sorted(tokens))


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
