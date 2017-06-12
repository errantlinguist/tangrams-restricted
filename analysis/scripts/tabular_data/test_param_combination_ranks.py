#!/usr/bin/env python3

from collections import Counter, defaultdict
import sys

from common import COL_DELIM, RANK_COL_NAME, TEST_PARAM_COL_NAMES, create_subcol_name_idx_map, parse_row_cells, parse_test_param_subtype_value, unify_regexes


__DEFAULT_PARAM_NAME_WHITELIST = TEST_PARAM_COL_NAMES

_DICT_ENTRY_KEY_SORT_KEY = lambda item: item[0]

class TestParameterCombinationValueMappings(object):
	
	def _create_nested_counter_dict():
		return defaultdict(Counter)

	def __init__(self):
		self.param_subtypes = {}
		
	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)
	
	def iter_param_combination_rank_counts(self):
		for param, subtypes in sorted(self.param_subtypes.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
			for subtype, vals in sorted(subtypes.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
				for val, rank_counts in sorted(vals.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
					for rank, count in sorted(rank_counts.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
						yield (param, subtype, val, rank, count)
		
	def add(self, param, param_subtype, param_value, param_value_mapping):
		try:
			subtypes = self.param_subtypes[param]
		except KeyError:
			subtypes = defaultdict(TestParameterCombinationValueMappings._create_nested_counter_dict)
			self.param_subtypes[param] = subtypes
		subtype_vals = subtypes[param_subtype]
		subtype_val_mappings = subtype_vals[param_value]
		subtype_val_mappings[param_value_mapping] += 1
		
def read_test_param_combination_ranks(infile_paths, test_param_whitelisting_filter):
	result = TestParameterCombinationValueMappings()
	for infile_path in infile_paths:
		print("Reading test parameters from \"%s\"." % infile_path, file=sys.stderr)
		with open(infile_path, 'r') as infile:
			col_names = parse_row_cells(next(infile))
			subcol_name_idxs = create_subcol_name_idx_map(col_names, test_param_whitelisting_filter)
			rank_idx = col_names.index(RANK_COL_NAME)
			
			rows = (parse_row_cells(line) for line in infile)
			for row in rows:
				rank = float(row[rank_idx])
				for subcol_names, idx in subcol_name_idxs.items():
					test_param_name = subcol_names[0]
					test_param_subtype = subcol_names[1]
					param_val = parse_test_param_subtype_value(row[idx])
					result.add(test_param_name, test_param_subtype, param_val, rank)
					
	return result
	
if __name__ == "__main__":
	import re
	if len(sys.argv) < 2:
		raise ValueError("Usage: %s INFILE [PARAM_NAME_REGEXES...] > OUTFILE" % sys.argv[0])
	else:
		infile_paths = sys.argv[1:2]
		input_param_name_regexes = sys.argv[2:]
		if input_param_name_regexes:
			param_name_regexes = frozenset(input_param_name_regexes)
		else:
			param_name_regexes = __DEFAULT_PARAM_NAME_WHITELIST
		print("Will print median ranks for combinations of parameters matching following regexes: %s" % sorted(param_name_regexes), file=sys.stderr)
		whitelisted_param_pattern = re.compile(unify_regexes(param_name_regexes))
		param_whitelisting_filter = lambda param_name: whitelisted_param_pattern.match(param_name) is not None
		param_combination_ranks = read_test_param_combination_ranks(infile_paths, param_whitelisting_filter)
		col_names = ("Parameter", "Subtype", "Value", "Rank", "Count")
		print(COL_DELIM.join(col_names))
		for iter_param_combination_rank_count in param_combination_ranks.iter_param_combination_rank_counts():
			row = COL_DELIM.join(str(cell) for cell in iter_param_combination_rank_count)	
			print(row)					
	
	