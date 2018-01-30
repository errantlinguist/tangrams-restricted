#!/usr/bin/env python3

"""
Finds the screenshot which most closely matches the given time for a recording session.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2018 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import argparse
import bisect
import datetime
import os
import re
import sys
from collections import defaultdict
from typing import Optional

import dateutil.parser

import session_data as sd

GAME_ROUND_SCREENSHOT_FILENAME_PATTERN = re.compile("round-([^-]+)-([^-]+)-([^.]+?)\\.png")
SELECTION_SCREENSHOT_FILENAME_PATTERN = re.compile("selection-entity-id([^-]+)-([^-]+)-([^.]+?)\\.png")

"""
A strptime format corresponding to \"HHmmssSSS\".
"""
SCREENSHOT_TIMESTAMP_FORMAT = "%H%M%S%f"


class ScreenshotData(object):

	def __init__(self, dirpath: str):
		self.dirpath = dirpath
		self.files_by_time = defaultdict(list)
		self.ordered_times = []
		for root_dirpath, _, filenames in os.walk(dirpath, followlinks=True):
			for filename in filenames:
				screenshot_timestamp = parse_screenshot_timestamp(filename)
				if screenshot_timestamp is not None:
					screenshot_time = parse_screenshot_time(screenshot_timestamp)
					self.files_by_time[screenshot_time].append(filename)
					self.ordered_times.append(screenshot_time)

		self.ordered_times.sort()


def offset_time(augend: datetime.datetime, addend_secs: float) -> datetime.datetime:
	time_micros = addend_secs * 1000000
	offset = datetime.timedelta(microseconds=time_micros)
	return augend + offset


def parse_screenshot_time(timestamp: str) -> datetime.time:
	d = datetime.datetime.strptime(timestamp, SCREENSHOT_TIMESTAMP_FORMAT)
	return d.time()


def parse_screenshot_timestamp(filename: str) -> Optional[str]:
	match = GAME_ROUND_SCREENSHOT_FILENAME_PATTERN.match(filename)
	if match:
		result = match.group(2)
	else:
		match = SELECTION_SCREENSHOT_FILENAME_PATTERN.match(filename)
		if match:
			result = match.group(2)
		else:
			result = None
	return result


def __create_argparser() -> argparse.ArgumentParser:
	result = argparse.ArgumentParser(
		description="Finds the screenshot which most closely matches the given time for a recording session.")
	result.add_argument("indir", metavar="INDIR",
						help="The directory of the session to find a corresponding screenshot for.")
	result.add_argument("time", metavar="TIME", type=float,
						help="The time of the screenshot to find in seconds.")
	return result


def __main(args):
	indir = args.indir
	print("Processing session directory \"{}\".".format(indir), file=sys.stderr)
	session = sd.SessionData(indir)
	screenshot_data = ScreenshotData(session.screenshot_dir)
	time_secs = args.time
	print("Finding screenshot at {} seconds.".format(time_secs), file=sys.stderr)
	start_timestamp = session.read_session_metadata()["START_TIME"]
	start_time = dateutil.parser.parse(start_timestamp)
	offset_screenshot_time = offset_time(start_time, time_secs).time()
	ordered_times = screenshot_data.ordered_times
	i = bisect.bisect_left(ordered_times, offset_screenshot_time)
	nearest_time = ordered_times[i]
	print("Found screenshot at {}.".format(nearest_time), file=sys.stderr)
	filenames = screenshot_data.files_by_time[nearest_time]
	screenshot_dir = session.screenshot_dir
	for filename in sorted(filenames):
		path = os.path.join(screenshot_dir, filename)
		print(path)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
