#!/usr/bin/env python3
'''
Created on Apr 3, 2017

@author: tshore
'''

import os.path

from lxml import etree

from annotations import AnnotationParser, HAT_DATA_NAMESPACE, QNameStringFactory, sanitize_dom_id


def merge_annotations(inpaths, namespace):
	annot_data = []
	qname_factory = QNameStringFactory(namespace)
	for inpath in inpaths:
		print("Reading \"%s\"." % inpath, file=sys.stderr)
		id_prefix = sanitize_dom_id(os.path.splitext(os.path.basename(inpath))[0]) + "-"
		nsmap = {None: namespace}
		parser = AnnotationParser(id_prefix, qname_factory, nsmap)
		doc_tree = etree.parse(inpath)
		infile_datum = parser(doc_tree)
		annot_data.append(infile_datum)
			
	result = annot_data[0]
	if len(annot_data) > 1:
		next_data = annot_data[1:]
		for next_datum in next_data:
			result.add(next_datum)
	return result

if __name__ == '__main__':
	import sys
	if len(sys.argv) < 2:
		print("Usage: %s INPUT_PATHS... > OUTFILE" % sys.argv[0], file=sys.stderr)
		sys.exit(64);
	else:
		import os
		import tempfile

		default_namespace = HAT_DATA_NAMESPACE
		# http://stackoverflow.com/a/18340978/1391325
		etree.register_namespace("hat", default_namespace)
		
		inpaths = sys.argv[1:]
		result = merge_annotations(inpaths, default_namespace)
		annot_elem = result.create_xml_element()
		annot_tree = etree.ElementTree(annot_elem)
	
		tmpfile = tempfile.mkstemp(text=True)
		tmpfile_path = tmpfile[1]
		try:
			encoding = result.encoding
			annot_tree.write(tmpfile_path, encoding=encoding, xml_declaration=True, pretty_print=True)
			with open(tmpfile_path, 'r', encoding=encoding) as inf:
				print(inf.read())
		finally:
			os.close(tmpfile[0])
			os.remove(tmpfile_path)
				
		