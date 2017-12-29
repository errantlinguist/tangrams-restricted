"""
Utilities for printing ElementTree instances to file-like objects in a manner agnostic to the actual ElementTree implementation used, e.g. standard Python's xml.etree.ElementTree.ElementTree vs. lxml's lxml.etree.

see https://docs.python.org/3/library/xml.etree.elementtree.html
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import os
import tempfile

from typing import IO


def print_etree_to_file(etree, encoding: str, outfile: IO[str]):
	tmpfile, tmpfile_path = tempfile.mkstemp(text=True)
	"""
	Prints an ElementTree-like object to file in a manner which is agnostic towards the XML implementation used.
	
	:param etree: An ElementTree-like object, e.g. either standard Python's xml.etree.ElementTree.ElementTree or lxml's lxml.etree.
	:param encoding: The output file encoding.
	:param outfile: The file-like object to write to.
	"""
	try:
		etree.write(tmpfile_path, encoding=encoding, xml_declaration=True, pretty_print=True)
		with open(tmpfile_path, 'r', encoding=encoding) as inf:
			print(inf.read(), file=outfile)
	finally:
		os.close(tmpfile)
		os.remove(tmpfile_path)
