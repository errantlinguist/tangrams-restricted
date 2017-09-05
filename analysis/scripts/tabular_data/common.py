from collections import defaultdict


COL_DELIM = "\t"
"""
NOTE: This is for SPSS compatibility, which does not allow e.g."-" as part of a variable name.
 
@see https://www.ibm.com/support/knowledgecenter/en/SSLVMB_21.0.0/com.ibm.spss.statistics.help/syn_variables_variable_names.htm
"""
SUBCOL_NAME_DELIM = "."

RANK_COL_NAME = "RANK"
TOKEN_COUNT_COL_NAME = "TOKEN_COUNT"

def parse_row_cells(line):
	line = line.strip()
	return line.split(COL_DELIM)

def parse_token_count_ranks(lines, rank_cell_val_transformer=None):
	if not rank_cell_val_transformer:
		rank_cell_val_transformer = lambda rank_cell_value : float(rank_cell_value)	
	
	result = defaultdict(list)
	token_count_idx, rank_idx = __token_count_rank_idxs(next(lines))
	for line in lines:
		row_vals = parse_row_cells(line)
		token_count = int(row_vals[token_count_idx])
		rank = rank_cell_val_transformer(row_vals[rank_idx])
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
	col_names = parse_row_cells(header)
	rank_idx = col_names.index(RANK_COL_NAME)
	token_count_idx = col_names.index(TOKEN_COUNT_COL_NAME)
	return token_count_idx, rank_idx
