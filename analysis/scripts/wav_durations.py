#!/usr/bin/env python3

"""
A script for aggregating statistics on WAV file duration underneath a given directory or directories.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import mimetypes
import os.path
import re
import statistics
import wave
from decimal import Decimal
from typing import Iterable, Iterator, Tuple

COL_DELIM = "\t"
WAV_CONTENT_TYPE_PATTERN = re.compile(".*?/(?:x-)?wav")


def parent_dir(infile_path: str) -> str:
	infile_path_parts = os.path.split(infile_path)
	if len(infile_path_parts) < 2:
		result = ""
	else:
		result = os.path.join(*infile_path_parts[:len(infile_path_parts) - 1])
	return result


def print_statistics(file_durations: Iterable[Tuple[str, Decimal]], outfile):
	sorted_durations = tuple(sorted(file_durations, key=lambda item: item[0]))
	print(COL_DELIM.join(("PATH", "DURATION")), file=outfile)
	for (audio_filepath, duration) in sorted_durations:
		print(COL_DELIM.join((audio_filepath, str(duration))), file=outfile)

	min_duration = min(duration for (_, duration) in sorted_durations)
	print(COL_DELIM.join(("MIN", str(min_duration))))
	max_duration = max(duration for (_, duration) in sorted_durations)
	print(COL_DELIM.join(("MAX", str(max_duration))))
	mean_duration = statistics.mean(duration for (_, duration) in sorted_durations)
	print(COL_DELIM.join(("MEAN", str(mean_duration))))


def read_audio_file_duration(infile_path: str) -> Decimal:
	with wave.open(infile_path, 'r') as f:
		frames = f.getnframes()
		rate = f.getframerate()
		return Decimal(frames) / Decimal(rate)


def walk_wav_files(*inpaths: str) -> Iterator[str]:
	for inpath in inpaths:
		for dirpath, _, filenames in os.walk(inpath, followlinks=True):
			for filename in filenames:
				resolved_path = os.path.join(dirpath, filename)
				mimetype = mimetypes.guess_type(resolved_path)[0]
				if mimetype is not None and WAV_CONTENT_TYPE_PATTERN.match(mimetype):
					yield resolved_path


def __main(inpaths: Iterable[str]):
	audio_infiles = tuple(walk_wav_files(*inpaths))
	print("Found {} audio file(s).".format(len(audio_infiles)), file=sys.stderr)
	file_durations = ((audio_infile, read_audio_file_duration(audio_infile)) for audio_infile in audio_infiles)
	print_statistics(file_durations, sys.stdout)


if __name__ == "__main__":
	import sys

	if len(sys.argv) < 2:
		raise ValueError("Usage: {} INPATHS...".format(sys.argv[0]))
	else:
		__main(sys.argv[1:])
