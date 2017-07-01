#!/usr/bin/env python3
'''
Created on Apr 3, 2017

@author: tshore
'''

from decimal import Decimal

from lxml import etree

from annotations import AnnotationParser, HAT_DATA_NAMESPACE, QNameStringFactory


def shift_segment_times(segments, addend):	
	for segment in segments: 
		orig_start = Decimal(segment.get("start"))
		# print("Original start: %s" % orig_start, file=sys.stderr)
		new_start = orig_start + addend
		# print("New start: %s" % new_start, file=sys.stderr)
		segment.set("start", str(new_start))
		orig_end = Decimal(segment.get("end"))
		# print("Original end: %s" % orig_end, file=sys.stderr)
		new_end = orig_end + addend
		# print("New end: %s" % new_end, file=sys.stderr)
		segment.set("end", str(new_end))

if __name__ == '__main__':
	import argparse
	import re
	import sys
	from etree_printing import print_etree_to_file
	
	parser = argparse.ArgumentParser(description=sys.argv[0])
	parser.add_argument("infile", metavar='PATH', help='The file to read.')
	parser.add_argument("-p", '--source-id-pattern', metavar='REGEX', type=re.compile, required=True, help='A regular expression matching the source ID of the segments to change.')
	parser.add_argument("-a", '--addend', type=Decimal, required=True, help='The amount to shift the times by.')
	parser.add_argument("-o", '--outfile', metavar='PATH', help='The path to write the output to.')
	
	args = parser.parse_args()
	
	default_namespace = HAT_DATA_NAMESPACE
	
	inpath = args.infile
	print("Reading \"%s\"." % inpath, file=sys.stderr)
	parser = AnnotationParser(QNameStringFactory(default_namespace), {None: default_namespace})
	doc_tree = etree.parse(inpath)
	infile_datum = parser(doc_tree)

	source_id_pattern = args.source_id_pattern
	segments = [segment for segment in infile_datum.segment_data.segments_by_id.values() if source_id_pattern.match(segment.get("source")) is not None]
	if segments:
		print("%d segment(s) matching source ID pattern \"%s\"." % (len(segments), source_id_pattern.pattern), file=sys.stderr) 
		
		addend = args.addend
		print("Shifting times by a value of %f." % addend, file=sys.stderr)
		
		shift_segment_times(segments, addend)
		encoding = doc_tree.docinfo.encoding
		outpath = args.outfile
		if outpath:
			print("Writing transformed data to \"%s\"." % outpath, file=sys.stderr)
			doc_tree.write(outpath, encoding=encoding, xml_declaration=True, pretty_print=True)
		else:
			print_etree_to_file(doc_tree, encoding, sys.stdout)
	else: 
		raise ValueError("No segments matching source ID pattern \"%s\"." % source_id_pattern.pattern)
