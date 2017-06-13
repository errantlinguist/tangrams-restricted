#!/usr/bin/env python3

from collections import defaultdict
import sys

from common import COL_DELIM, RANK_COL_NAME, SUBCOL_NAME_DELIM, parse_row_cells
from decimal import Decimal
from test_param_combinations import create_col_name_idx_map, create_param_whitelisting_filter, parse_test_param_subtype_value

WITHIN_SUBJECTS_FACTOR_COL_NAMES = frozenset(("UtteranceFiltering", "Cleaning", "Tokenization", "TokenType", "TokenFiltering", "KEY", "SESSION_ORDER", "TEST_ITER"))

BETWEEN_SUBJECTS_FACTOR_COL_NAMES = frozenset("RANK")

COVARIATE_COL_NAMES = frozenset(("TESTED_UTT_COUNT", "TOTAL_UTT_COUNT", "TOKEN_COUNT"))
	
TRAINING_COL_NAME = "Training"

_DICT_ENTRY_KEY_SORT_KEY = lambda item: item[0]

def parse_test_param_training_param_ranks(lines):
	test_param_training_param_ranks = defaultdict(dict)
	param_names = set()
	unique_training_param_values = set()
	
	rows = (parse_row_cells(line) for line in lines)
	col_names = next(rows)
	param_col_name_idxs = create_col_name_idx_map(col_names, param_whitelisting_filter)	
	param_names.update(param_col_name_idxs.keys())
	training_param_idx = col_names.index(TRAINING_COL_NAME)
	rank_idx = col_names.index(RANK_COL_NAME)
	
	for row in rows:
		param_vals = tuple((col_name, parse_test_param_subtype_value(row[idx])) for (col_name, idx) in param_col_name_idxs.items())
		training_param_ranks = test_param_training_param_ranks[param_vals]
		training_param_val = row[training_param_idx]
		unique_training_param_values.add(training_param_val)
		rank = Decimal(row[rank_idx])
		training_param_ranks[training_param_val] = rank
		
	return test_param_training_param_ranks, param_names, unique_training_param_values
	
if __name__ == "__main__":
	if len(sys.argv) < 2:
		raise ValueError("Usage: %s INFILE > OUTFILE" % sys.argv[0])
	else:
		infile_path = sys.argv[1]
		input_param_name_regexes = sys.argv[2:]
		param_whitelisting_filter = create_param_whitelisting_filter(WITHIN_SUBJECTS_FACTOR_COL_NAMES)
		
		print("Reading test results from \"%s\"." % infile_path, file=sys.stderr)
		with open(infile_path, 'r') as infile:
			test_param_training_param_ranks, param_names, unique_training_param_values = parse_test_param_training_param_ranks(infile)
				
		param_name_ordering = tuple(sorted(param_names))
		training_param_value_ordering = tuple(sorted(unique_training_param_values))
		col_names = []
		col_names.extend(param_name_ordering)
		training_factor_col_names = ((TRAINING_COL_NAME + SUBCOL_NAME_DELIM + param_value) for param_value in training_param_value_ordering)
		col_names.extend(training_factor_col_names)
		print(COL_DELIM.join(col_names))

		for param_combination, training_param_ranks in sorted(test_param_training_param_ranks.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
			param_vals = dict(param_combination)
			row = []
			ordered_param_vals = tuple((param_vals[param_name] for param_name in param_name_ordering))
			row.extend(ordered_param_vals)
			try: 
				ordered_training_param_val_ranks = (training_param_ranks[training_param_val] for training_param_val in training_param_value_ordering)
				row.extend(ordered_training_param_val_ranks)
				print(COL_DELIM.join(str(cell) for cell in row))	
			except KeyError as e:
				print("No training value parameter value \"%s\" used for other param values \"%s\"; Omitting entire row from output." % (e, param_vals), file=sys.stderr)
