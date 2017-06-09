from collections import defaultdict

COL_DELIM = "\t"
RANK_COL_NAME = "RANK"
TOKEN_COUNT_COL_NAME = "TOKEN_COUNT"

def __token_count_rank_idxs(header):
	header = header.strip()
	col_names = header.split(COL_DELIM)
	rank_idx = col_names.index(RANK_COL_NAME)
	token_count_idx = col_names.index(TOKEN_COUNT_COL_NAME)
	return token_count_idx, rank_idx

def parse_token_count_ranks(lines):
	result = defaultdict(list)
	token_count_idx, rank_idx = __token_count_rank_idxs(next(lines))
	for line in lines:
		line = line.strip()
		row_vals = line.split(COL_DELIM)
		token_count = int(row_vals[token_count_idx])
		rank = float(row_vals[rank_idx])
		result[token_count].append(rank)
		
	return result