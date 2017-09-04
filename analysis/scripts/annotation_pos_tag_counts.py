#!/usr/bin/env python3

import sys
import xml.etree.ElementTree
from collections import Counter

import nltk

from annotations import ANNOTATION_NAMESPACES
from xml_files import walk_xml_files

COL_DELIM = '\t'


class CachingPosTagger(object):
	def __init__(self):
		self.cache = {}

	def __call__(self, text):
		try:
			result = self.cache[text]
		except KeyError:
			tokens = nltk.tokenize.word_tokenize(text)
			result = nltk.pos_tag(tokens)
			self.cache[text] = result
		return result


def count_pos_tags(infile_paths, pos_tagger):
	result = Counter()
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


if __name__ == "__main__":
	if len(sys.argv) < 2:
		raise ValueError("Usage: {} INPUT_PATHS... > OUTFILE".format(sys.argv[0]))
	else:
		inpaths = sys.argv[1:]
		infiles = walk_xml_files(inpaths)
		token_pos_tag_counts = count_pos_tags(infiles, CachingPosTagger())
		print("Found {} unique token(s).".format(len(token_pos_tag_counts)), file=sys.stderr)
		col_headers = ("TOKEN", "POS_TAG", "COUNT")
		print(COL_DELIM.join(col_headers))
		alphabetic_token_counts = sorted(token_pos_tag_counts.items(), key=lambda item: item[0])
		alphabetic_count_desc_token_counts = sorted(alphabetic_token_counts, key=lambda item: item[1], reverse=True)
		for tagged_token, count in alphabetic_count_desc_token_counts:
			row = []
			row.extend(tagged_token)
			row.append(count)
			print(COL_DELIM.join(str(cell) for cell in row))
