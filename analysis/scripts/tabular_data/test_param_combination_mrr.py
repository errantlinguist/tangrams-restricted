#!/usr/bin/env python3


from collections import defaultdict
from typing import DefaultDict, IO, Iterable, List

import numpy

from common import COL_DELIM
from test_param_combination_ranks import TestParameterCombinationRankFileReader, P, R
from test_param_combinations import create_param_whitelisting_filter


class TestParameterCombinationRankListFileReader(TestParameterCombinationRankFileReader):
	def _create_param_combination_rank_coll(self) -> DefaultDict[P, List[R]]:
		return defaultdict(list)

	def _process_param_combination_rank(self, param_combination_ranks:  DefaultDict[P, List[R]], param_vals: P, rank: R):
		param_combination_rank_counts = param_combination_ranks[param_vals]
		param_combination_rank_counts.append(rank)


def __main(infile_paths, input_param_name_regexes: Iterable[str], outfile: IO[str]):
	param_whitelisting_filter = create_param_whitelisting_filter(input_param_name_regexes)
	reader = TestParameterCombinationRankListFileReader(param_whitelisting_filter, __rr)
	param_combination_ranks, param_names = reader(infile_paths)
	param_name_ordering = tuple(sorted(param_names))
	col_names = []
	col_names.extend(param_name_ordering)
	col_names.append("Mean RR")
	col_names.append("RR SD")
	print(COL_DELIM.join(col_names), file=outfile)
	for param_combination, ranks in param_combination_ranks.items():
		param_vals = dict(param_combination)
		ordered_param_vals = tuple((param_vals[param_name] for param_name in param_name_ordering))
		row_vals = []
		row_vals.extend(ordered_param_vals)

		rank_arr = numpy.array(ranks, copy=False)
		mean = numpy.mean(rank_arr)
		row_vals.append(mean)
		std = numpy.std(rank_arr)
		row_vals.append(std)
		print(COL_DELIM.join(str(row_val) for row_val in row_vals), file=outfile)


def __rr(value):
	return numpy.longfloat_(1.0 / value)


if __name__ == "__main__":
	import sys

	if len(sys.argv) < 2:
		raise ValueError("Usage: %s INFILE [PARAM_NAME_REGEXES...] > OUTFILE" % sys.argv[0])
	else:
		__main(sys.argv[1:2], sys.argv[2:], sys.stdout)
