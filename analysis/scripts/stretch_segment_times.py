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
	for segment in segments: 
		orig_start = Decimal(segment.get("start"))
		#print("Original start: %s" % orig_start, file=sys.stderr)
		new_start = orig_start * factor
		#print("New start: %s" % new_start, file=sys.stderr)
		segment.set("start", str(new_start))
		orig_end = Decimal(segment.get("end"))
		#print("Original end: %s" % orig_end, file=sys.stderr)
		new_end = orig_end * factor
		#print("New end: %s" % new_end, file=sys.stderr)
		segment.set("end", str(new_end))

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
