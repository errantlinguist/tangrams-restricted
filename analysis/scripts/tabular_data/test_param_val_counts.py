#!/usr/bin/env python3

from collections import Counter, defaultdict
from statistics import mean
import sys
from pip._vendor.pyparsing import White

__COL_DELIM = "\t"
'''
NOTE: This is for SPSS compatibility, which does not allow e.g."-" as part of a variable name.
 
@see https://www.ibm.com/support/knowledgecenter/en/SSLVMB_21.0.0/com.ibm.spss.statistics.help/syn_variables_variable_names.htm
'''
__SUBCOL_NAME_DELIM = "#";

_DICT_ENTRY_KEY_SORT_KEY = lambda x: x[0]

class ParameterValues(object):

	def __init__(self):
		self.param_subtypes = {}
		
	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)
	
	def iter_param_val_counts(self):
		for param, subtypes in sorted(self.param_subtypes.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
			for subtype, vals in sorted(subtypes.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
				for val, count in sorted(vals.items(), key=_DICT_ENTRY_KEY_SORT_KEY):
					yield (param, subtype, val, count)
		
	def put(self, param, param_subtype, value):
		subtypes = self.param_subtypes.get(param)
		if subtypes is None:
			subtypes = defaultdict(Counter)
			self.param_subtypes[param] = subtypes
			
		subtype_vals = subtypes[param_subtype]
		subtype_vals[value] += 1
		
def read_param_values(infile_paths, param_whitelisting_filter):
	result = ParameterValues()
	for infile_path in infile_paths:
		print("Reading test parameters from \"%s\"." % infile_path, file=sys.stderr)
		with open(infile_path) as infile:
			col_names = __create_col_name_list(next(infile))
			for line in infile:
				line = line.strip()
				row_vals = line.split(__COL_DELIM)
				col_row_values = zip(col_names, row_vals)
				for col_name, row_val in col_row_values:
					sub_col_names = col_name.split(__SUBCOL_NAME_DELIM, 2)
					param = sub_col_names[0]
					if param_whitelisting_filter(param):
						param_subtype = sub_col_names[1] if len(sub_col_names) > 1 else ""
						try:
							row_val = int(row_val)
						except ValueError:
							try:
								row_val = float(row_val)
							except ValueError:
								pass
						result.put(param, param_subtype, row_val)
					
	return result

def __create_col_idx_map(header):
	header = header.strip()
	col_names = header.split(__COL_DELIM)
	return dict((col_name, idx) for idx, col_name in enumerate(col_names))

def __create_col_name_list(header):
	header = header.strip()
	return header.split(__COL_DELIM)
	
if __name__ == "__main__":
	if len(sys.argv) < 2:
		raise ValueError("Usage: %s INFILE WHITELISTED_PARAMS... > OUTFILE" % sys.argv[0])
	else:
		infile_paths = sys.argv[1:2]
		whitelisted_param_names = sys.argv[2:]
		if len(whitelisted_param_names) < 1:
			param_whitelisting_filter = (lambda param_name: True)
		else:
			whitelisted_param_names = set(whitelisted_param_names)
			print("Will print only the following parameters: %s" % sorted(whitelisted_param_names), file=sys.stderr)
			param_whitelisting_filter = lambda param_name: param_name in whitelisted_param_names
		param_vals = read_param_values(infile_paths, param_whitelisting_filter)
		col_names = ("Parameter", "Subtype", "Value", "Count")
		print(__COL_DELIM.join(col_names))
		for param_val_row_count in param_vals.iter_param_val_counts():
			row = __COL_DELIM.join(str(cell) for cell in param_val_row_count)	
			print(row)		
	
	