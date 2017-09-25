#!/usr/bin/env python3


from collections import Counter, defaultdict
from typing import Any, Callable, Generic, IO, Iterable, Iterator, Tuple, TypeVar

from common import COL_DELIM, parse_row_cells
from test_param_combinations import create_param_whitelisting_filter, create_subcol_name_idx_map, \
	parse_test_param_subtype_value

T = TypeVar('T')
ST = TypeVar('ST')
V = TypeVar('C')


class TestParameterCombinationCounts(Generic[T, ST, V]):
	def __init__(self):
		self.param_subtypes = {}

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def add(self, param: T, param_subtype: ST, param_value: V):
		try:
			subtypes = self.param_subtypes[param]
		except KeyError:
			subtypes = defaultdict(Counter)
			self.param_subtypes[param] = subtypes
		subtype_vals = subtypes[param_subtype]
		subtype_vals[param_value] += 1

	@property
	def param_combination_counts(self) -> Iterator[Tuple[T, ST, V, int]]:
		for param, subtypes in sorted(self.param_subtypes.items(), key=_get_item_key):
			for subtype, vals in sorted(subtypes.items(), key=_get_item_key):
				for val, count in sorted(vals.items(), key=_get_item_key):
					yield (param, subtype, val, count)


def read_test_param_combinations(infile_paths: Iterator[str], test_param_whitelisting_filter: Callable[[str], bool],
								 err_outfile: IO[str]) -> TestParameterCombinationCounts:
	result = TestParameterCombinationCounts()
	for infile_path in infile_paths:
		print("Reading test parameters from \"%s\"." % infile_path, file=err_outfile)
		with open(infile_path, 'r', encoding='utf-8') as infile:
			rows = (parse_row_cells(line) for line in infile)
			subcol_name_idxs = create_subcol_name_idx_map(next(rows), test_param_whitelisting_filter)
			for row in rows:
				for subcol_names, idx in subcol_name_idxs.items():
					test_param_name = subcol_names[0]
					test_param_subtype = subcol_names[1]
					param_val = parse_test_param_subtype_value(row[idx])
					result.add(test_param_name, test_param_subtype, param_val)

	return result


def _get_item_key(item: Tuple[T, Any]) -> T:
	return item[0]


def __main(infile_paths: Iterator[str], input_param_name_regexes: Iterable[str], outfile: IO[str],
		   err_outfile: IO[str]):
	param_whitelisting_filter = create_param_whitelisting_filter(input_param_name_regexes)
	param_combinations = read_test_param_combinations(infile_paths, param_whitelisting_filter, err_outfile)

	print(COL_DELIM.join(("Parameter", "Subtype", "Value", "Count")), file=outfile)
	for param_combination_count in param_combinations.param_combination_counts:
		row = COL_DELIM.join(str(cell) for cell in param_combination_count)
		print(row, file=outfile)


if __name__ == "__main__":
	import sys

	if len(sys.argv) < 2:
		raise ValueError("Usage: %s INFILE [PARAM_NAME_REGEXES...] > OUTFILE" % sys.argv[0])
	else:
		__main(sys.argv[1:2], sys.argv[2:], sys.stdout, sys.stderr)
