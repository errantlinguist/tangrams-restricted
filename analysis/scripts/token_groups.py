import csv
import sys
from collections import defaultdict
from enum import Enum, unique
from typing import Callable, Dict, FrozenSet, Iterable, Iterator, List, Mapping, Set, Tuple

ENCODING = "utf-8"
GROUP_LIST_DELIM = ","

TokenGroupMapping = Mapping[str, FrozenSet[str]]
_TokenGroupMappingImpl = Dict[str, FrozenSet[str]]


@unique
class TokenGroupDataColumn(Enum):
	GROUP = "GROUP"
	TOKEN = "TOKEN"


def __default_group_filter(group: str) -> bool:
	"""

	:param group: The token group string
	:type group: str
	:return: True iff the group is neither empty nor composed only of whitespaces
	:rtype: bool
	"""
	return bool(group)


def create_group_token_list_dict(tokens: Iterable[str], token_groups: TokenGroupMapping) -> Dict[
	str, List[str]]:
	result = defaultdict(list)
	for token in tokens:
		try:
			groups = token_groups[token]
			for group in groups:
				result[group].append(token)
		except KeyError:
			# Do nothing with tokens which don't have a semantic group
			pass

	return result


def create_group_token_set_dict(tokens: Iterable[str], token_groups: TokenGroupMapping) -> Dict[
	str, Set[str]]:
	result = defaultdict(set)
	for token in tokens:
		try:
			groups = token_groups[token]
			for group in groups:
				result[group].add(token)
		except KeyError:
			# Do nothing with tokens which don't have a semantic group
			pass

	return result


def read_token_group_dict(infile_path: str,
						  group_filter: Callable[[str], bool] = __default_group_filter) -> _TokenGroupMappingImpl:
	with open(infile_path, 'r', encoding=ENCODING) as inf:
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
			yield sys.intern(token), group_set
