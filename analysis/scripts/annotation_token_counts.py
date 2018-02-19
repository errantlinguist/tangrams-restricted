#!/usr/bin/env python3

"""
Reads Higgins Annotation Tool (HAT) XML annotation files <http://www.speech.kth.se/hat/> and prints out each token type (i.e. unique word) found and the number of times each thereof was found.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import collections
import csv
import sys
import xml.etree.ElementTree
from typing import DefaultDict, Dict, Iterable, Set

from tangrams_analysis.annotations import ANNOTATION_NAMESPACES
from tangrams_analysis.xml_files import walk_xml_files


def count_tokens(infile_paths: Iterable[str]) -> Dict[str, int]:
	result = collections.Counter()
	for infile_path in infile_paths:
		print("Reading XML file \"{}\".".format(infile_path), file=sys.stderr)
		doc_tree = xml.etree.ElementTree.parse(infile_path)
		token_annots = doc_tree.iterfind(".//hat:t", ANNOTATION_NAMESPACES)
		tokens = (annot.text for annot in token_annots)
		result.update(tokens)

	return result


def equal_casings(tokens: Iterable[str]) -> DefaultDict[str, Set[str]]:
	result = collections.defaultdict(set)
	token_tuple = tuple(tokens)
	for i, t1 in enumerate(token_tuple):
		t1_casefolded = t1.casefold()
		for j, t2 in enumerate(token_tuple):
			if i != j and t2 not in result:
				t2_casefolded = t2.casefold()
				if t1_casefolded == t2_casefolded:
					result[t1].add(t2)
	return result


def __main(inpaths: Iterable[str]):
	infiles = walk_xml_files(*inpaths)
	token_counts = count_tokens(infiles)
	print("Found {} unique token(s).".format(len(token_counts)), file=sys.stderr)
	casing_dict = equal_casings(token_counts.keys())
	if casing_dict:
		print("Found {} tokens with multiple casings.".format(len(casing_dict)), file=sys.stderr)
		# https://pythonconquerstheuniverse.wordpress.com/2011/05/08/newline-conversion-in-python-3/
		err_writer = csv.writer(sys.stderr, dialect=csv.excel_tab, lineterminator="\n")
		err_writer.writerow(("TOKEN", "OTHER_CASINGS"))
		for token, other_casings in sorted(casing_dict.items(), key=lambda item: item[0]):
			casing_str = ", ".join(sorted(other_casings))
			err_writer.writerow((token, casing_str))

	# https://pythonconquerstheuniverse.wordpress.com/2011/05/08/newline-conversion-in-python-3/
	writer = csv.writer(sys.stdout, dialect=csv.excel_tab, lineterminator="\n")
	writer.writerow(("TOKEN", "COUNT"))
	alphabetic_token_counts = sorted(token_counts.items(), key=lambda item: item[0])
	alphabetic_count_desc_token_counts = sorted(alphabetic_token_counts, key=lambda item: item[1], reverse=True)
	writer.writerows(alphabetic_count_desc_token_counts)


if __name__ == "__main__":
	if len(sys.argv) < 2:
		sys.exit("Usage: {} INPUT_PATHS... > OUTFILE".format(sys.argv[0]))
	else:
		__main(sys.argv[1:])
