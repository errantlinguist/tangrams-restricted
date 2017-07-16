#!/usr/bin/env python3

from collections import defaultdict
import re
import sys

from common import COL_DELIM, RANK_COL_NAME, SUBCOL_NAME_DELIM
from decimal import Decimal

COVARIATE_COL_NAMES = frozenset(("DIALOGIC_INFO", "TESTED_UTT_COUNT", "TOTAL_UTT_COUNT", "TOKEN_COUNT"))
RANK_COL_NAME = "RANK"
TRAINING_COL_NAME = "Training"
TRAINING_METHODS_TO_COMPARE = frozenset(("DIALOGIC", "ALL_NEG"))
UNKNOWN_VAL_REPR = "?"

_DICT_ENTRY_KEY_SORT_KEY = lambda item: item[0]
__DIGITS_PATTERN = re.compile('(\d+)')

class TrainingParamDatum(object):
	def __init__(self, rank, covariates):
		self.rank = rank
		self.covariates = covariates
		
	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)
		
def create_training_method_rank_comparison(rows):
	result = defaultdict(__create_nested_defaultdict)
	
	col_name_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
	covariate_col_name_idxs = [(col_name, idx) for (col_name, idx) in col_name_idxs.items() if col_name in COVARIATE_COL_NAMES]
	
	training_method_idx = col_name_idxs[TRAINING_COL_NAME]
	key_idx = col_name_idxs["KEY"]
	session_order_idx = col_name_idxs["SESSION_ORDER"]
	test_iter_idx = col_name_idxs["TEST_ITER"]
	rank_idx = col_name_idxs[RANK_COL_NAME]
	for row in rows:
		training_method = row[training_method_idx]
		if training_method in TRAINING_METHODS_TO_COMPARE:
			session_diags = result[row[key_idx]]
			diag_test_iters = session_diags[row[session_order_idx]]
			test_iter = row[test_iter_idx]
			try:
				diag_test_iter = diag_test_iters[test_iter]
			except KeyError:
				diag_test_iter = {}
				diag_test_iters[test_iter] = diag_test_iter
				
			training_results = diag_test_iter.get(training_method)
			if training_results is None:
				rank = float(row[rank_idx])
				covariates = dict((col_name, row[idx]) for (col_name, idx) in covariate_col_name_idxs)
				training_results = TrainingParamDatum(rank, covariates)
				diag_test_iter[training_method] = training_results
			else:
				raise ValueError("Value already present for training method \"%s\"." % training_method)
				
	return result
	
def natural_keys(text):
	'''
	alist.sort(key=natural_keys) sorts in human order
	
	:see: http://nedbatchelder.com/blog/200712/human_sorting.html
	:see: http://stackoverflow.com/a/5967539/1391325
	'''
	return [__atoi(c) for c in __DIGITS_PATTERN.split(text)]
	
def __atoi(text):
	'''
	:see: http://stackoverflow.com/a/5967539/1391325
	'''
	return int(text) if text.isdigit() else text
	
def print_training_method_rank_comparison(training_method_rank_comparison):
	col_names = ["KEY", "SESSION_ORDER", "TEST_ITER"]
	sorted_training_method_names = tuple(sorted(TRAINING_METHODS_TO_COMPARE))
	sorted_covariate_names = tuple(sorted(COVARIATE_COL_NAMES))
	for training_method in sorted_training_method_names:
		col_names.append(RANK_COL_NAME + SUBCOL_NAME_DELIM + training_method)
	for covariate_name in sorted_covariate_names:
		for training_method in sorted_training_method_names:
			col_names.append(covariate_name + SUBCOL_NAME_DELIM + training_method)
		
	print(COL_DELIM.join(col_names))
	natural_dict_sorting_key = lambda item: natural_keys(item[0])
	for session_key, session_diags in sorted(training_method_rank_comparison.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
		for session_diag_order, test_iters in sorted(session_diags.items(), key=natural_dict_sorting_key):
			for test_iter, test_results in sorted(test_iters.items(), key=natural_dict_sorting_key):
				row_cells = [session_key, session_diag_order, test_iter]
				
				training_results = []
				for training_method in sorted_training_method_names:
					rank = UNKNOWN_VAL_REPR
					covariate_vals = dict((covariate_name, UNKNOWN_VAL_REPR) for covariate_name in sorted_covariate_names)
					try:
						training_method_results = test_results[training_method]
						rank = training_method_results.rank
						covariate_vals = training_method_results.covariates
					except KeyError:
						print("No results for training method \"%s\" found for key \"%s\", session order %s, test iteration %s." % (training_method, session_key, session_diag_order, test_iter), file=sys.stderr)
					training_results.append((rank, covariate_vals))
				
				for datum in training_results:
					row_cells.append(datum[0])
				for covariate_name in sorted_covariate_names:
					for datum in training_results:
						row_cells.append(datum[1][covariate_name])
				print(COL_DELIM.join(str(cell) for cell in row_cells))	
					
		

def __create_nested_defaultdict():
	return defaultdict(dict)

if __name__ == "__main__":
	if len(sys.argv) < 2:
		raise ValueError("Usage: %s INFILE > OUTFILE" % sys.argv[0])
	else:
		infile_path = sys.argv[1]
		print("Reading test results from \"%s\"." % infile_path, file=sys.stderr)
		with open(infile_path, 'r') as lines:
			rows = (line.strip().split(COL_DELIM) for line in lines)
			training_method_rank_comparison = create_training_method_rank_comparison(rows)
			print_training_method_rank_comparison(training_method_rank_comparison)					
