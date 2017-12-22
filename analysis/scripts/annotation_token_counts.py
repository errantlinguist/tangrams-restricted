#!/usr/bin/env python3

"""
Reads Higgins Annotation Tool (HAT) XML annotation files <http://www.speech.kth.se/hat/> and prints out each token type (i.e. unique word) found and the number of times each thereof was found.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

from collections import Counter
import csv
import sys
import xml.etree.ElementTree

from annotations import ANNOTATION_NAMESPACES
from xml_files import walk_xml_files


def count_tokens(infile_paths):
	result = Counter()
	for infile_path in infile_paths:
		print("Reading XML file \"{}\".".format(infile_path), file=sys.stderr)
		doc_tree = xml.etree.ElementTree.parse(infile_path)
		token_annots = doc_tree.iterfind(".//hat:t", ANNOTATION_NAMESPACES)
		tokens = (annot.text for annot in token_annots)
		result.update(tokens)

	return result


if __name__ == "__main__":
	if len(sys.argv) < 2:
		raise ValueError("Usage: {} INPUT_PATHS... > OUTFILE".format(sys.argv[0]))
	else:
		inpaths = sys.argv[1:]
		infiles = walk_xml_files(*inpaths)
		token_counts = count_tokens(infiles)
		print("Found {} unique token(s).".format(len(token_counts)), file=sys.stderr)
		writer = csv.writer(sys.stdout, dialect=csv.excel_tab)
		writer.writerow(("TOKEN", "COUNT"))
		alphabetic_token_counts = sorted(token_counts.items(), key=lambda item: item[0])
		alphabetic_count_desc_token_counts = sorted(alphabetic_token_counts, key=lambda item: item[1], reverse=True)
		writer.writerows(alphabetic_count_desc_token_counts)