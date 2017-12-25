#!/usr/bin/env python3
"""
Creates content for a LaTeX figure displaying multiple images in a grid.

Use with e.g. "find tangram-imgs/ -iname "*.pdf" -exec ./latex-image-table.py {} +"
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import csv
import sys
from typing import Sequence


def __main(infiles: Sequence[str]):
	print("Creating grid content for {} image file(s).".format(len(infiles)))


if __name__ == "__main__":
	if len(sys.argv) < 2:
		raise ValueError("Usage: {} INFILES... > OUTFILE".format(sys.argv[0]))
	else:
		__main(sys.argv[1:])
