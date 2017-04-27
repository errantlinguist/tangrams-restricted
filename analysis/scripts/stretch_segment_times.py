#!/usr/bin/env python3
'''
Created on Apr 3, 2017

@author: tshore
'''

from decimal import Decimal

from lxml import etree

from annotations import AnnotationParser, HAT_DATA_NAMESPACE, QNameStringFactory
from etree_printing import print_etree_to_file


def stretch_segment_times(segments, factor):
	max_start_time = max((Decimal(segment.get("start")) for segment in segments))
	print("Max start: %f" % max_start_time)
	max_end_time = max((Decimal(segment.get("end")) for segment in segments))
	print("Max end: %f" % max_end_time)
	
	for segment in segments: 
		orig_start = Decimal(segment.get("start"))
		print("Original start: %s" % orig_start, file=sys.stderr)
		new_start = orig_start * factor
		print("New start: %s" % new_start, file=sys.stderr)
		orig_end = Decimal(segment.get("end"))
		print("Original end: %s" % orig_end, file=sys.stderr)
		new_end = orig_end * factor
		print("New end: %s" % new_end, file=sys.stderr)

if __name__ == '__main__':
	import sys
	if len(sys.argv) < 3:
		print("Usage: %s INFILE TIME_COMPRESSION_FACTOR > OUTFILE" % sys.argv[0], file=sys.stderr)
		sys.exit(64);
	else:
		default_namespace = HAT_DATA_NAMESPACE
		
		inpath = sys.argv[1]
		print("Reading \"%s\"." % inpath, file=sys.stderr)
		parser = AnnotationParser(QNameStringFactory(default_namespace), {None: default_namespace})
		doc_tree = etree.parse(inpath)
		infile_datum = parser(doc_tree)
	
		factor = Decimal(sys.argv[2])
		print("Stretching times by a factor of %f." % factor, file=sys.stderr)
		stretch_segment_times(infile_datum.segment_data.segments_by_id.values(), factor)
		encoding = doc_tree.docinfo.encoding
		print_etree_to_file(doc_tree, encoding, sys.stdout)
