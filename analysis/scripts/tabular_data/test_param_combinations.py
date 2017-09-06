import re
import sys
from decimal import Decimal, InvalidOperation

from common import split_subcol_names, unify_regexes

TEST_PARAM_COL_NAMES = frozenset(("UtteranceFiltering", "Cleaning", "Tokenization", "TokenType", "TokenFilter", "Training"))

def create_col_name_idx_map(col_names, col_name_whitelisting_filter=None):
	if not col_name_whitelisting_filter:
		col_name_whitelisting_filter = lambda _ : True
		
	result = {}
	for idx, col_name in enumerate(col_names):
		subcol_names = split_subcol_names(col_name)
		if col_name_whitelisting_filter(subcol_names[0]):
			result[col_name] = idx
	return result

def create_param_whitelisting_filter(input_param_name_regexes):
	if input_param_name_regexes:
		param_name_regexes = frozenset(input_param_name_regexes)
	else:
		param_name_regexes = TEST_PARAM_COL_NAMES
	print("Will print only parameters which match at least one of the following regexes: %s" % sorted(param_name_regexes), file=sys.stderr)
	whitelisted_param_pattern = re.compile(unify_regexes(param_name_regexes))
	return lambda param_name: whitelisted_param_pattern.match(param_name) is not None
	
def create_subcol_name_idx_map(col_names, col_name_whitelisting_filter):
	result = {}
	for idx, col_name in enumerate(col_names):
		subcol_names = split_subcol_names(col_name)
		if col_name_whitelisting_filter(subcol_names[0]):
			result[subcol_names] = idx
	return result
	
def parse_test_param_subtype_value(row_cell_val):
	result = row_cell_val
	try:
		result = int(result)
	except ValueError:
		try:
			result = Decimal(result)
		except InvalidOperation:
			pass
	return result
