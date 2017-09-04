#!/usr/bin/env python3

import sys
from collections import defaultdict

import numpy
from statsmodels import robust

from common import COL_DELIM
from test_param_combination_ranks import TestParameterCombinationRankFileReader
from test_param_combinations import create_param_whitelisting_filter


class TestParameterCombinationRankListFileReader(TestParameterCombinationRankFileReader):

	def _create_param_combination_rank_coll(self):
		return defaultdict(list)
	
	def _process_param_combination_rank(self, param_combination_ranks, param_vals, rank):
		param_combination_rank_counts = param_combination_ranks[param_vals]
		param_combination_rank_counts.append(rank)

	
	
if __name__ == "__main__":
	if len(sys.argv) < 2:
		raise ValueError("Usage: %s INFILE [PARAM_NAME_REGEXES...] > OUTFILE" % sys.argv[0])
	else:
		infile_paths = sys.argv[1:2]
		input_param_name_regexes = sys.argv[2:]
		param_whitelisting_filter = create_param_whitelisting_filter(input_param_name_regexes)
		reader = TestParameterCombinationRankListFileReader(param_whitelisting_filter)
		param_combination_ranks, param_names = reader(infile_paths)
		param_name_ordering = tuple(sorted(param_names))
		col_names = []
		col_names.extend(param_name_ordering)
		col_names.append("Median Rank")
		col_names.append("Rank MAD")
		print(COL_DELIM.join(col_names))
		for param_combination, ranks in param_combination_ranks.items():
			param_vals = dict(param_combination)
			ordered_param_vals = tuple((param_vals[param_name] for param_name in param_name_ordering))
			row_vals = []
			row_vals.extend(ordered_param_vals)
			
			rank_arr = numpy.array(ranks, copy=False)
			median = numpy.median(rank_arr)
			row_vals.append(median)
			mad = robust.mad(rank_arr)
			row_vals.append(mad)
			print(COL_DELIM.join(str(row_val) for row_val in row_vals))	
