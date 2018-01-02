#!/usr/bin/env python3

"""
Reads tabular utterance files and creates a single map of unique utterances to the canonical referring tokens in that utterance.

Use with e.g. "find ~/Documents/Projects/Tangrams/Data/Derived/ -iname "*utt-referring-tokens-basic-inflected.tsv" -exec ./write_utterance_referring_tokens.py {} +"
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import csv
import itertools
import sys
from typing import Dict

INPUT_CSV_DIALECT = csv.excel_tab
INPUT_ENCODING = "utf-8"
OUTPUT_CSV_DIALECT = csv.excel_tab
REFERRING_LANGUAGE_COL_NAME = "REFERRING_TOKENS"
UTTERANCE_COL_NAME = "TOKENS"


def create_utterance_referring_token_map(*inpaths: str) -> Dict[str, str]:
	result = {}
	for inpath in inpaths:
		with open(inpath, "r", encoding=INPUT_ENCODING) as inf:
			rows = csv.reader(inf, dialect=INPUT_CSV_DIALECT)
			header_col_idxs = dict((col, idx) for (idx, col) in enumerate(next(rows)))
			utt_col_idx = header_col_idxs[UTTERANCE_COL_NAME]
			ref_lang_col_idx = header_col_idxs[REFERRING_LANGUAGE_COL_NAME]

			for row in rows:
				utt = row[utt_col_idx]
				ref_lang = row[ref_lang_col_idx]
				old_ref_lang = result.get(utt)
				if old_ref_lang is None or old_ref_lang == ref_lang:
					result[utt] = ref_lang
				else:
					raise ValueError("Differing referring language for utterance \"{}\" in file \"{}\": Existing: \"{}\"; New: \"{}\"".format(utt, inpath, old_ref_lang, ref_lang))
	return result


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Reads tabular utterance files and creates a single map of unique utterances to the canonical referring tokens in that utterance.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The files to process.")

	return result


def __main(args):
	inpaths = args.inpaths
	print("Will read {} file(s).".format(len(inpaths)), file=sys.stderr)
	utt_ref_lang = create_utterance_referring_token_map(*inpaths)
	# https://pythonconquerstheuniverse.wordpress.com/2011/05/08/newline-conversion-in-python-3/
	writer = csv.writer(sys.stdout, dialect=OUTPUT_CSV_DIALECT, lineterminator="\n")
	header = (UTTERANCE_COL_NAME, REFERRING_LANGUAGE_COL_NAME)
	rows = itertools.chain((header,), (sorted(utt_ref_lang.items(), key=lambda item: item[0])))
	writer.writerows(rows)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
