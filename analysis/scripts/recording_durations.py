#!/usr/bin/env python3

import os.path
import statistics
import wave
from decimal import Decimal
from typing import Iterable, Tuple, Set
from xml.etree.ElementTree import parse as parse_etree

import utterances
from xml_files import walk_xml_files

COL_DELIM = "\t"


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


def walk_audio_files(inpaths: Iterable[str]) -> Set[str]:
	infile_paths = walk_xml_files(*inpaths)
	result = set()
	for infile_path in infile_paths:
		print("Reading XML file \"{}\".".format(infile_path), file=sys.stderr)
		doc_tree = parse_etree(infile_path)
		sources = doc_tree.iterfind(".//hat:source", utterances.ANNOTATION_NAMESPACES)
		source_file_urls = frozenset(source.get("href") for source in sources)
		indir = parent_dir(infile_path)
		resolved_source_file_paths = (os.path.join(indir, url) for url in source_file_urls)
		result.update(resolved_source_file_paths)

	return result


def __main(inpaths):
	audio_infiles = walk_audio_files(inpaths)
	print(f"Found {len(audio_infiles)} audio file(s).", file=sys.stderr)
	file_durations = ((audio_infile, read_audio_file_duration(audio_infile)) for audio_infile in audio_infiles)
	print_statistics(file_durations, sys.stdout)


if __name__ == "__main__":
	import sys

	if len(sys.argv) < 2:
		raise ValueError("Usage: {} INPATHS...".format(sys.argv[0]))
	else:
		__main(sys.argv[1:])
