#!/usr/bin/env python3

from collections import Counter, defaultdict
from decimal import Decimal, InvalidOperation
import sys

from common import COL_DELIM, create_subcol_name_idx_map, parse_row_cells, parse_test_param_subtype_value


__DEFAULT_PARAM_NAME_WHITELIST = frozenset(("UtteranceFiltering", "Cleaning", "Tokenization", "TokenType", "TokenFilter", "Training"))

_DICT_ENTRY_KEY_SORT_KEY = lambda item: item[0]


class TestParameterCombinationCounts(object):

	def __init__(self):
		self.param_subtypes = {}
		
	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)
	
	def iter_param_val_counts(self):
		for param, subtypes in sorted(self.param_subtypes.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
			for subtype, vals in sorted(subtypes.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
				for val, count in sorted(vals.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
					yield (param, subtype, val, count)
		
	def add(self, param, param_subtype, param_value):
		try:
			subtypes = self.param_subtypes[param]
		except KeyError:
			subtypes = defaultdict(Counter)
			self.param_subtypes[param] = subtypes
		subtype_vals = subtypes[param_subtype]
		subtype_vals[param_value] += 1
		
def read_test_param_values(infile_paths, test_param_whitelisting_filter):
	result = TestParameterCombinationCounts()
	for infile_path in infile_paths:
		print("Reading test parameters from \"%s\"." % infile_path, file=sys.stderr)
		with open(infile_path, 'r') as infile:
			subcol_name_idxs = create_subcol_name_idx_map(next(infile), test_param_whitelisting_filter)
			rows = (parse_row_cells(line) for line in infile)
			for row in rows:
				for subcol_names, idx in subcol_name_idxs.items():
					test_param_name = subcol_names[0]
					test_param_subtype = subcol_names[1]
					param_val = parse_test_param_subtype_value(row[idx])
					result.add(test_param_name, test_param_subtype, param_val)
					
	return result

def __unify_regexes(regexes):
	if len(regexes) < 2:
		result = regexes
	else:		
		group_start = "(?:"
		group_end = ")"
		union_delim = group_end + "|" + group_start
		result = group_start + union_delim.join(regexes) + group_end
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
		print("Will print only parameters which match at least one of the following regexes: %s" % sorted(param_name_regexes), file=sys.stderr)
		whitelisted_param_pattern = re.compile(__unify_regexes(param_name_regexes))
		param_whitelisting_filter = lambda param_name: whitelisted_param_pattern.match(param_name) is not None
		param_vals = read_test_param_values(infile_paths, param_whitelisting_filter)
		col_names = ("Parameter", "Subtype", "Value", "Count")
		print(COL_DELIM.join(col_names))
		for param_val_row_count in param_vals.iter_param_val_counts():
			row = COL_DELIM.join(str(cell) for cell in param_val_row_count)	
			print(row)		
	
	