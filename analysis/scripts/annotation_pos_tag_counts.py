#!/usr/bin/env python3

"""
A script which reads Higgins Annotation Tool (HAT) XML annotation files <http://www.speech.kth.se/hat/>, extracts the individual "segment" elements contained therein and creates POS tags for the segment tokens using NLTK.
The counts of each unique token-tag pair are then printed to the standard output stream.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import collections
import csv
import sys
import xml.etree.ElementTree
from typing import Dict, Iterable, List, Tuple

import nltk

from annotations import ANNOTATION_NAMESPACES
from xml_files import walk_xml_files


class CachingPosTagger(object):
	def __init__(self):
		self.cache = {}

	def __call__(self, text: str) -> List[Tuple[str, str]]:
		try:
			result = self.cache[text]
		except KeyError:
			tokens = nltk.tokenize.word_tokenize(text)
			result = nltk.pos_tag(tokens)
			self.cache[text] = result
		return result


def count_pos_tags(infile_paths: Iterable[str], pos_tagger: CachingPosTagger) -> Dict[Tuple[str, str], int]:
	result = collections.Counter()
	for infile_path in infile_paths:
		print("Reading XML file \"{}\".".format(infile_path), file=sys.stderr)
		doc_tree = xml.etree.ElementTree.parse(infile_path)
		segments = doc_tree.iterfind(".//hat:segment", ANNOTATION_NAMESPACES)
		for segment in segments:
			token_annots = segment.iterfind(".//hat:t", ANNOTATION_NAMESPACES)
			utt_str_repr = ' '.join((token_annot.text.strip() for token_annot in token_annots))
			tokens = pos_tagger(utt_str_repr)
			result.update(tokens)

	return result


def __main(inpaths: Iterable[str]):
	infiles = walk_xml_files(*inpaths)
	token_pos_tag_counts = count_pos_tags(infiles, CachingPosTagger())
	print("Found {} unique token(s).".format(len(token_pos_tag_counts)), file=sys.stderr)
	# https://pythonconquerstheuniverse.wordpress.com/2011/05/08/newline-conversion-in-python-3/
	writer = csv.writer(sys.stdout, dialect=csv.excel_tab, lineterminator="\n")
	writer.writerow(("TOKEN", "POS_TAG", "COUNT"))
	alphabetic_token_counts = sorted(token_pos_tag_counts.items(), key=lambda item: item[0])
	alphabetic_count_desc_token_counts = sorted(alphabetic_token_counts, key=lambda item: item[1], reverse=True)
	for tagged_token, count in alphabetic_count_desc_token_counts:
		row = []
		row.extend(tagged_token)
		row.append(count)
		writer.writerow(row)


if __name__ == "__main__":
	if len(sys.argv) < 2:
		sys.exit("Usage: {} INPUT_PATHS... > OUTFILE".format(sys.argv[0]))
	else:
		__main(sys.argv[1:])
