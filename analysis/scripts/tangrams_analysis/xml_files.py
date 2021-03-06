"""
Utilities for manipulating XML files.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import mimetypes
import os
import re
import xml.etree.ElementTree
from typing import Iterator

XML_CONTENT_TYPE_PATTERN = re.compile(".*?/xml")
XSI = "http://www.w3.org/2001/XMLSchema-instance"


def add_xml_schema_location(root: xml.etree.ElementTree.Element, location: str):
	# https://stackoverflow.com/a/15370357/1391325
	root.attrib['{{{pre}}}schemaLocation'.format(pre=XSI)] = location


def walk_xml_files(*inpaths: str) -> Iterator[str]:
	for inpath in inpaths:
		for dirpath, _, filenames in os.walk(inpath, followlinks=True):
			for filename in filenames:
				resolved_path = os.path.join(dirpath, filename)
				mimetype = mimetypes.guess_type(resolved_path)[0]
				if mimetype is not None and XML_CONTENT_TYPE_PATTERN.match(mimetype):
					yield resolved_path
