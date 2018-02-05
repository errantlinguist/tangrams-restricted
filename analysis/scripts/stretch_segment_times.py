#!/usr/bin/env python3
"""
A script for stretching the time of each segment found for the given parameters in a given Higgins Annotation Tool (HAT) XML annotation file <http://www.speech.kth.se/hat/>.

Created on Apr 3, 2017.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import re
from decimal import Decimal
from numbers import Number
from typing import IO, Iterable

from lxml import etree

from common.annotations import AnnotationParser, HAT_DATA_NAMESPACE, QNameStringFactory
from common.etree_printing import print_etree_to_file


def stretch_segment_times(segments: Iterable[etree.Element], factor: Number):
	for segment in segments:
		orig_start = Decimal(segment.get("start"))
		# print("Original start: %s" % orig_start, file=sys.stderr)
		new_start = orig_start * factor
		# print("New start: %s" % new_start, file=sys.stderr)
		segment.set("start", str(new_start))
		orig_end = Decimal(segment.get("end"))
		# print("Original end: %s" % orig_end, file=sys.stderr)
		new_end = orig_end * factor
		# print("New end: %s" % new_end, file=sys.stderr)
		segment.set("end", str(new_end))


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Stretches the time of each segment found for the given parameters in a given Hat Annotation Tool XML annotation file.")
	result.add_argument("infile", metavar='PATH', help='The file to read.')
	result.add_argument("-p", '--source-id-pattern', metavar='REGEX', type=re.compile, required=True,
						help='A regular expression matching the source ID of the segments to change.')
	result.add_argument("-f", '--factor', type=Decimal, required=True, help='The amount to stretch the times by.')
	result.add_argument("-o", '--outfile', metavar='PATH', help='The path to write the output to.')
	return result


def __main(args, outfile: IO[str], err_outfile: IO[str]):
	default_namespace = HAT_DATA_NAMESPACE

	inpath = args.infile
	print("Reading \"%s\"." % inpath, file=err_outfile)
	parser = AnnotationParser(QNameStringFactory(default_namespace), {None: default_namespace})
	doc_tree = etree.parse(inpath)
	infile_datum = parser(doc_tree)

	source_id_pattern = args.source_id_pattern
	segments = [segment for segment in infile_datum.segment_data.segments_by_id.values() if
				source_id_pattern.match(segment.get("source")) is not None]
	if segments:
		print("%d segment(s) matching source ID pattern \"%s\"." % (len(segments), source_id_pattern.pattern),
			  file=err_outfile)

		factor = args.factor
		print("Stretching times by a factor of %f." % factor, file=err_outfile)

		stretch_segment_times(segments, factor)
		encoding = doc_tree.docinfo.encoding
		outpath = args.outfile
		if outpath:
			print("Writing transformed data to \"%s\"." % outpath, file=err_outfile)
			doc_tree.write(outpath, encoding=encoding, xml_declaration=True, pretty_print=True)
		else:
			print_etree_to_file(doc_tree, encoding, outfile)
	else:
		raise ValueError("No segments matching source ID pattern \"%s\"." % source_id_pattern.pattern)


if __name__ == '__main__':
	import sys

	__main(__create_argparser().parse_args(), sys.stdout, sys.stderr)
