import csv
import sys
from enum import Enum, unique
from typing import Callable, Dict, FrozenSet, Iterator, Tuple

GROUP_LIST_DELIM = ","

TokenGroupDict = Dict[str, FrozenSet[str]]


@unique
class TokenGroupDataColumn(Enum):
	GROUP = "GROUP"
	TOKEN = "TOKEN"


def __default_group_filter(group) -> bool:
	return group


def read_token_group_dict(infile_path: str,
						  group_filter: Callable[[str], bool] = __default_group_filter) -> TokenGroupDict:
	with open(infile_path, 'r') as inf:
		token_groups = read_token_groups(inf, group_filter)
		return dict(token_groups)


def read_token_groups(infile, group_filter: Callable[[str], bool] = __default_group_filter) -> Iterator[
	Tuple[str, FrozenSet[str]]]:
	rows = csv.reader(infile, dialect="excel-tab")
	col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
	token_col_idx = col_idxs[TokenGroupDataColumn.TOKEN.value]
	group_col_idx = col_idxs[TokenGroupDataColumn.GROUP.value]
	token_group_strs = ((row[token_col_idx], row[group_col_idx]) for row in rows)
	for token, group_str in token_group_strs:
		groups = group_str.split(GROUP_LIST_DELIM)
		filtered_groups = (sys.intern(group) for group in groups if group_filter(group))
		group_set = frozenset(filtered_groups)
		if group_set:
			yield token, group_set
