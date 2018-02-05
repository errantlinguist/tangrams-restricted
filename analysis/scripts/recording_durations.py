#!/usr/bin/env python3

"""
A script for aggregating statistics on the duration of audio files which have corresponding Higgins Annotation Tool (HAT) <http://www.speech.kth.se/hat/> XML annotation files found underneath a given directory or directories.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import csv
import os.path
import statistics
import wave
from decimal import Decimal
from typing import IO, Iterable, Tuple, Set
from xml.etree.ElementTree import parse as parse_etree

from tangrams_analysis import annotations
from tangrams_analysis.xml_files import walk_xml_files


def parent_dir(infile_path: str) -> str:
	infile_path_parts = os.path.split(infile_path)
	if len(infile_path_parts) < 2:
		result = ""
	else:
		result = os.path.join(*infile_path_parts[:len(infile_path_parts) - 1])
	return result


def print_statistics(file_durations: Iterable[Tuple[str, Decimal]], outfile: IO[str]):
	sorted_durations = tuple(sorted(file_durations))
	# https://pythonconquerstheuniverse.wordpress.com/2011/05/08/newline-conversion-in-python-3/
	writer = csv.writer(outfile, dialect=csv.excel_tab, lineterminator="\n")
	writer.writerow(("PATH", "DURATION"))
	writer.writerows(sorted_durations)

	min_duration = min(duration for (_, duration) in sorted_durations)
	writer.writerow(("MIN", min_duration))
	max_duration = max(duration for (_, duration) in sorted_durations)
	writer.writerow(("MAX", max_duration))
	mean_duration = statistics.mean(duration for (_, duration) in sorted_durations)
	writer.writerow(("MEAN", mean_duration))
	summed_duration = sum(duration for (_, duration) in sorted_durations)
	writer.writerow(("SUM", summed_duration))


def read_audio_file_duration(infile_path: str) -> Decimal:
	with wave.open(infile_path, 'r') as f:
		frames = f.getnframes()
		rate = f.getframerate()
		return Decimal(frames) / Decimal(rate)


def walk_audio_files(inpaths: Iterable[str]) -> Set[str]:
	infile_paths = walk_xml_files(*inpaths)
	result = set()
	for infile_path in infile_paths:
		print("Reading XML file \"{}\".".format(infile_path), file=sys.stderr)
		doc_tree = parse_etree(infile_path)
		sources = doc_tree.iterfind(".//hat:source", annotations.ANNOTATION_NAMESPACES)
		source_file_urls = frozenset(source.get("href") for source in sources)
		indir = parent_dir(infile_path)
		resolved_source_file_paths = (os.path.join(indir, url) for url in source_file_urls)
		result.update(resolved_source_file_paths)

	return result


def __main(inpaths: Iterable[str]):
	audio_infiles = walk_audio_files(inpaths)
	print("Found {} audio file(s).".format(len(audio_infiles)), file=sys.stderr)
	file_durations = ((audio_infile, read_audio_file_duration(audio_infile)) for audio_infile in audio_infiles)
	print_statistics(file_durations, sys.stdout)


if __name__ == "__main__":
	import sys

	if len(sys.argv) < 2:
		sys.exit("Usage: {} INPATHS...".format(sys.argv[0]))
	else:
		__main(sys.argv[1:])
