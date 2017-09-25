import os
import tempfile

from typing import IO


def print_etree_to_file(etree, encoding: str, outfile: IO[str]):
	tmpfile = tempfile.mkstemp(text=True)
	tmpfile_path = tmpfile[1]
	try:
		etree.write(tmpfile_path, encoding=encoding, xml_declaration=True, pretty_print=True)
		with open(tmpfile_path, 'r', encoding=encoding) as inf:
			print(inf.read(), file=outfile)
	finally:
		os.close(tmpfile[0])
		os.remove(tmpfile_path)
