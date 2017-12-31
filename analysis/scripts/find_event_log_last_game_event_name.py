#!/usr/bin/env python3

"""
Finds the last game event name in IrisTK event logs.

Use with e.g. "find ~/Documents/Projects/Tangrams/Data/Ready/ -iname "events*.txt" -exec ./find_event_log_last_game_event_name.py {} +"
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import csv
import json
import sys

import iristk

OUTPUT_CSV_DIALECT = csv.excel_tab


def read_last_game_event_name(infile_path: str):
	result = None
	with open(infile_path, encoding=iristk.LOGFILE_ENCODING) as inf:
		for line in inf:
			if line:
				json_struct = json.loads(line)
				event_name = json_struct["event_name"]
				if event_name.startswith("tangrams."):
					result = event_name
	return result


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Calculates mean token type overlap for each coreference chain sequence ordinality in each chain for a given referent.")
	result.add_argument("infiles", metavar="INFILE", nargs="+",
						help="The event log file(s) to read.")
	return result


def __main(args):
	infiles = args.infiles
	writer = csv.writer(sys.stderr, dialect=OUTPUT_CSV_DIALECT)
	writer.writerow(("FILE", "LAST_EVENT_NAME"))
	for infile in sorted(infiles):
		last_event_name = read_last_game_event_name(infile)
		writer.writerow((infile, last_event_name))


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
