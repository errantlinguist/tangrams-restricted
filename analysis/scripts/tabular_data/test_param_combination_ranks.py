#!/usr/bin/env python3

from collections import Counter, defaultdict
import sys

from common import COL_DELIM, RANK_COL_NAME, parse_row_cells
from test_param_combinations import create_param_whitelisting_filter, create_col_name_idx_map, parse_test_param_subtype_value


def read_test_param_combination_ranks(infile_paths, test_param_whitelisting_filter, rank_cell_val_transformer=None):
	if not rank_cell_val_transformer:
		rank_cell_val_transformer = lambda rank_cell_value : float(rank_cell_value)
		
	param_combination_ranks = defaultdict(Counter)
	param_names = set()
	for infile_path in infile_paths:
		print("Reading test parameters from \"%s\"." % infile_path, file=sys.stderr)
		with open(infile_path, 'r') as infile:
			rows = (parse_row_cells(line) for line in infile)
			col_names = next(rows)
			param_col_name_idxs = create_col_name_idx_map(col_names, test_param_whitelisting_filter)
			param_names.update(param_col_name_idxs.keys())
			rank_idx = col_names.index(RANK_COL_NAME)
			
			for row in rows:
				param_vals = tuple((col_name, parse_test_param_subtype_value(row[idx])) for (col_name, idx) in param_col_name_idxs.items())
				param_combination_rank_counts = param_combination_ranks[param_vals]
				rank = rank_cell_val_transformer(row[rank_idx])
				param_combination_rank_counts[rank] += 1
		
	return param_combination_ranks, param_names
	
if __name__ == "__main__":
	if len(sys.argv) < 2:
		raise ValueError("Usage: %s INFILE [PARAM_NAME_REGEXES...] > OUTFILE" % sys.argv[0])
	else:
		infile_paths = sys.argv[1:2]
		input_param_name_regexes = sys.argv[2:]
		param_whitelisting_filter = create_param_whitelisting_filter(input_param_name_regexes)
		param_combination_ranks, param_names = read_test_param_combination_ranks(infile_paths, param_whitelisting_filter)
		param_name_ordering = tuple(sorted(param_names))
		col_names = []
		col_names.extend(param_name_ordering)
		col_names.append("Rank")
		col_names.append("Count")
		print(COL_DELIM.join(col_names))
		for param_combination, rank_counts in param_combination_ranks.items():
			param_vals = dict(param_combination)
			ordered_param_vals = tuple((param_vals[param_name] for param_name in param_name_ordering))
			for rank, count in sorted(rank_counts.items(), key=lambda item: item[0]):
				row = []	
				row.extend(ordered_param_vals)
				row.append(rank)
				row.append(count)
				print(COL_DELIM.join(str(cell) for cell in row))				
	
	
