#!/usr/bin/env python3

"""
Reads Higgins Annotation Tool (HAT) XML annotation files <http://www.speech.kth.se/hat/> and prints out "segment" annotations sorted by length.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import sys
import xml.etree.ElementTree
from collections import Counter
from typing import List, Tuple

from annotations import ANNOTATION_NAMESPACES
from xml_files import walk_xml_files

COL_DELIM = '\t'


def read_segments(infile_paths: str) -> List[Tuple[str, str, Tuple[str, ...]]]:
	result = []
	for infile_path in infile_paths:
		print("Reading XML file \"{}\".".format(infile_path), file=sys.stderr)
		doc_tree = xml.etree.ElementTree.parse(infile_path)
		segments = doc_tree.iterfind(".//hat:segment", ANNOTATION_NAMESPACES)
		for seg in segments:
			tokens = seg.iterfind(".//hat:t", ANNOTATION_NAMESPACES)
			tokens = tuple(t.text for t in tokens)
			result.append((infile_path, seg.attrib.get("id"), tokens))
	return result


def __token_length(row: Tuple[str, str, Tuple[str, ...]]) -> int:
	tokens = row[2]
	return len(tokens)


if __name__ == "__main__":
	if len(sys.argv) < 2:
		raise ValueError("Usage: {} INPUT_PATHS... > OUTFILE".format(sys.argv[0]))
	else:
		inpaths = sys.argv[1:]
		infiles = walk_xml_files(*inpaths)
		segments = read_segments(infiles)
		print("Found {} segment(s).".format(len(segments)), file=sys.stderr)
		segments.sort(key=__token_length)
		col_headers = ("FILE", "SEGMENT_ID", "TOKENS")
		print(COL_DELIM.join(col_headers))
		for seg in segments:
			print(seg)
			filename = seg[0]
			seg_id = seg[1]
			tokens = seg[2]
			row = (filename, seg_id, " ".join(tokens))
			print(COL_DELIM.join(row))
