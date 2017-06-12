from collections import defaultdict

COL_DELIM = "\t"
'''
NOTE: This is for SPSS compatibility, which does not allow e.g."-" as part of a variable name.
 
@see https://www.ibm.com/support/knowledgecenter/en/SSLVMB_21.0.0/com.ibm.spss.statistics.help/syn_variables_variable_names.htm
'''
SUBCOL_NAME_DELIM = "#"

RANK_COL_NAME = "RANK"
TOKEN_COUNT_COL_NAME = "TOKEN_COUNT"


def create_col_name_list(header):
	header = header.strip()
	return header.split(COL_DELIM)

def parse_token_count_ranks(lines, rank_datatype=float):
	result = defaultdict(list)
	token_count_idx, rank_idx = __token_count_rank_idxs(next(lines))
	for line in lines:
		line = line.strip()
		row_vals = line.split(COL_DELIM)
		token_count = int(row_vals[token_count_idx])
		rank = rank_datatype=(row_vals[rank_idx])
		result[token_count].append(rank)
		
	return result

def split_subcol_names(col_name):
	return col_name.split(SUBCOL_NAME_DELIM, 2)

def __token_count_rank_idxs(header):
	header = header.strip()
	col_names = header.split(COL_DELIM)
	rank_idx = col_names.index(RANK_COL_NAME)
	token_count_idx = col_names.index(TOKEN_COUNT_COL_NAME)
	return token_count_idx, rank_idx