#!/usr/bin/env python3

from collections import Counter
import mimetypes
import os
import os.path
import re
import sys
import xml.etree.ElementTree

COL_DELIM = '\t'
XML_CONTENT_TYPE_PATTERN = re.compile(".*?/xml")

					
def count_tokens(infile_paths):
	result = Counter()
	for infile_path in infile_paths:
		print("Reading XML file \"{}\".".format(infile_path), file=sys.stderr)
		e = xml.etree.ElementTree.parse(infile_path)
		token_annots = e.findall('.//{http://www.speech.kth.se/higgins/2005/annotation/}t')
		tokens = (annot.text for annot in token_annots)
		result.update(tokens)
				
	return result
	
def walk_xml_files(inpaths):
	for inpath in inpaths:
		for dirpath, dirnames, filenames in os.walk(inpath, followlinks=True):
			for filename in filenames:
				resolved_path = os.path.join(dirpath, filename)
				mimetype = mimetypes.guess_type(resolved_path)[0]
				if mimetype is not None and XML_CONTENT_TYPE_PATTERN.match(mimetype):
					yield resolved_path
					

if __name__ == "__main__":
	if len(sys.argv) < 2:
		raise ValueError("Usage: %s INPUT_PATHS... > OUTFILE" % sys.argv[0])
	else:
		inpaths = sys.argv[1:]
		infiles = walk_xml_files(inpaths)
		token_counts = count_tokens(infiles)
		print("Found {} unique token(s).".format(len(token_counts)), file=sys.stderr)
		col_headers = ("TOKEN", "COUNT")
		print(COL_DELIM.join(col_headers))
		alphabetic_token_counts = sorted(token_counts.items(), key=lambda item: item[0])
		alphabetic_count_desc_token_counts = sorted(alphabetic_token_counts, key=lambda item: item[1], reverse=True)
		for token_count in alphabetic_count_desc_token_counts:
			print(COL_DELIM.join(str(cell) for cell in token_count))
