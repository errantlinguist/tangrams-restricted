from collections import defaultdict
from decimal import Decimal, InvalidOperation


COL_DELIM = "\t"
'''
NOTE: This is for SPSS compatibility, which does not allow e.g."-" as part of a variable name.
 
@see https://www.ibm.com/support/knowledgecenter/en/SSLVMB_21.0.0/com.ibm.spss.statistics.help/syn_variables_variable_names.htm
'''
SUBCOL_NAME_DELIM = "#"

RANK_COL_NAME = "RANK"
TOKEN_COUNT_COL_NAME = "TOKEN_COUNT"
TEST_PARAM_COL_NAMES = frozenset(("UtteranceFiltering", "Cleaning", "Tokenization", "TokenType", "TokenFilter", "Training"))


def create_subcol_name_idx_map(header, col_name_whitelisting_filter):
	col_names = parse_row_cells(header)
	result = {}
	for idx, col_name in enumerate(col_names):
		subcol_names = split_subcol_names(col_name)
		if col_name_whitelisting_filter(subcol_names[0]):
			result[subcol_names] = idx
	return result

def parse_row_cells(line):
	line = line.strip()
	return line.split(COL_DELIM)

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

def parse_token_count_ranks(lines, rank_datatype=float):
	result = defaultdict(list)
	token_count_idx, rank_idx = __token_count_rank_idxs(next(lines))
	for line in lines:
		line = line.strip()
		row_vals = line.split(COL_DELIM)
		token_count = int(row_vals[token_count_idx])
		rank = rank_datatype(row_vals[rank_idx])
		result[token_count].append(rank)
		
	return result

def split_subcol_names(col_name):
	sub_col_names = col_name.split(SUBCOL_NAME_DELIM, 2)
	col_name = sub_col_names[0]
	subcol_name = sub_col_names[1] if len(sub_col_names) > 1 else ""
	return col_name, subcol_name

def unify_regexes(regexes):
	if len(regexes) < 2:
		result = regexes
	else:		
		group_start = "(?:"
		group_end = ")"
		union_delim = group_end + "|" + group_start
		result = group_start + union_delim.join(regexes) + group_end
	return result

def __token_count_rank_idxs(header):
	header = header.strip()
	col_names = header.split(COL_DELIM)
	rank_idx = col_names.index(RANK_COL_NAME)
	token_count_idx = col_names.index(TOKEN_COUNT_COL_NAME)
	return token_count_idx, rank_idx
