#!/usr/bin/env python3

from collections import defaultdict
import sys

from common import COL_DELIM, RANK_COL_NAME, SUBCOL_NAME_DELIM, parse_row_cells
from decimal import Decimal
from test_param_combinations import create_col_name_idx_map, create_param_whitelisting_filter, parse_test_param_subtype_value

WITHIN_SUBJECTS_FACTOR_COL_NAMES = frozenset(("UtteranceFiltering", "Cleaning", "Tokenization", "TokenType", "TokenFiltering", "DYAD", "SESSION_ORDER"))

COVARIATE_COL_NAMES = frozenset(("TESTED_UTT_COUNT", "TOTAL_UTT_COUNT", "TOKEN_COUNT"))
	
TEST_ITER_COL_NAME = "TEST_ITER"
TRAINING_COL_NAME = "Training"

_DICT_ENTRY_KEY_SORT_KEY = lambda item: item[0]

class TrainingParamDatum(object):
	def __init__(self, rank, covariates):
		self.rank = rank
		self.covariates = covariates
		
	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

class TestIterDatum(object):
	
	def __init__(self):
		self.training_param_data = {}
		
	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

class SubjectGroup(object):
	
	def __init__(self):
		self.iter_data = {}
		
	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)
		
	def get_iter_data(self, test_iter):
		return self.iter_data[test_iter]
	
	def fetch_iter_data(self, test_iter):
		try:
			result = self.get_iter_data(test_iter)
		except KeyError:
			result = TestIterDatum()
			self.iter_data[test_iter] = result
		return result

def parse_test_param_training_param_ranks(lines):
	test_param_subject_groups = defaultdict(SubjectGroup)
	param_names = set()
	unique_training_param_values = set()
	
	rows = (parse_row_cells(line) for line in lines)
	col_names = next(rows)
	param_col_name_idxs = create_col_name_idx_map(col_names, create_param_whitelisting_filter(WITHIN_SUBJECTS_FACTOR_COL_NAMES))	
	param_names.update(param_col_name_idxs.keys())
	training_param_idx = col_names.index(TRAINING_COL_NAME)
	test_iter_param_idx = col_names.index(TEST_ITER_COL_NAME)
	rank_idx = col_names.index(RANK_COL_NAME)
	covariate_val_col_name_idxs = create_col_name_idx_map(col_names, create_param_whitelisting_filter(COVARIATE_COL_NAMES))
	
	for row in rows:
		param_vals = tuple((col_name, parse_test_param_subtype_value(row[idx])) for (col_name, idx) in param_col_name_idxs.items())
		subject_group = test_param_subject_groups[param_vals]
		test_iter = int(row[test_iter_param_idx])
		iter_data = subject_group.fetch_iter_data(test_iter)
		training_param_val = row[training_param_idx]
		unique_training_param_values.add(training_param_val)
		rank = Decimal(row[rank_idx])
		param_covariates = dict((covariate_col_name, parse_test_param_subtype_value(row[idx])) for covariate_col_name, idx in covariate_val_col_name_idxs.items())
		iter_data.training_param_data[training_param_val] = TrainingParamDatum(rank, param_covariates)
		
	return test_param_subject_groups, param_names, unique_training_param_values

def __create_nested_defaultdict():
	return defaultdict(dict)

if __name__ == "__main__":
	if len(sys.argv) < 2:
		raise ValueError("Usage: %s INFILE > OUTFILE" % sys.argv[0])
	else:
		infile_path = sys.argv[1]
		input_param_name_regexes = sys.argv[2:]
		print("Reading test results from \"%s\"." % infile_path, file=sys.stderr)
		with open(infile_path, 'r') as infile:
			test_param_subject_groups, param_names, unique_training_param_values = parse_test_param_training_param_ranks(infile)
				
		param_name_ordering = tuple(sorted(param_names))
		training_param_value_ordering = tuple(sorted(unique_training_param_values))
		col_names = []
		col_names.extend(param_name_ordering)
		col_names.append(TEST_ITER_COL_NAME)
		training_factor_col_names = ((TRAINING_COL_NAME + SUBCOL_NAME_DELIM + param_value) for param_value in training_param_value_ordering)
		col_names.extend(training_factor_col_names)
		covariate_subcol_name_ordering = tuple(sorted(COVARIATE_COL_NAMES))
		for training_param_value in training_param_value_ordering:
			param_covariate_col_names = ((col_name + SUBCOL_NAME_DELIM + training_param_value) for col_name in covariate_subcol_name_ordering)
			col_names.extend(param_covariate_col_names)
		print(COL_DELIM.join(col_names))

		for param_combination, subj_group in sorted(test_param_subject_groups.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
			param_vals = dict(param_combination)
			ordered_param_vals = tuple((param_vals[param_name] for param_name in param_name_ordering))
			
			for test_iter, iter_datum in subj_group.iter_data.items():
				row = []
				row.extend(ordered_param_vals)
				row.append(test_iter)
				
				ranks_to_append = []
				covariates_to_append = []
				for training_param_val in training_param_value_ordering:
					try: 
						training_param_datum = iter_datum.training_param_data[training_param_val]
					except KeyError as e:
						print("No training param data %s used for other param values \"%s\" for test iter %d; Substituting values from first test iter." % (e, param_vals, test_iter), file=sys.stderr)
						first_iter_datum = subj_group.iter_data[1]
						training_param_datum = first_iter_datum.training_param_data[training_param_val]
					
					ranks_to_append.append(training_param_datum.rank)
					covariates_to_append.extend((training_param_datum.covariates[col_name] for col_name in covariate_subcol_name_ordering))

				row.extend(ranks_to_append)
				row.extend(covariates_to_append)
				print(COL_DELIM.join(str(cell) for cell in row))		
