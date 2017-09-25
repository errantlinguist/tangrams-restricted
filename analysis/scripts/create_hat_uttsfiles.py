#!/usr/bin/env python3


import mimetypes
import os
import re
import sys
from typing import Iterable

from lxml import etree

import session_data
from annotations import HAT_DATA_NAMESPACE, HAT_DATA_NAMESPACE_NAME, create_skeleton_annnotation_elem

WAV_CONTENT_TYPE_PATTERN = re.compile(".*?/wav")
UTTERANCE_FILE_NAME = session_data.SessionDatum.UTTS.value


def create_session_dir_uttfiles(*inpaths: str):
	for inpath in inpaths:
		for dirpath, _, filenames in os.walk(inpath, followlinks=True):
			longest_wav_filename = find_longest_wav_filename(dirpath, filenames)
			if longest_wav_filename:
				is_session_transcribed = UTTERANCE_FILE_NAME in filenames
				if not is_session_transcribed:
					utt_annots_elem = create_skeleton_annnotation_elem(longest_wav_filename)
					utt_annots_etree = etree.ElementTree(utt_annots_elem)
					outpath = os.path.join(dirpath, UTTERANCE_FILE_NAME)
					print("Writing new XML file to \"{}\" for audio file \"{}\".".format(outpath, os.path.join(outpath,
																											   longest_wav_filename)),
						  file=sys.stderr)
					utt_annots_etree.write(outpath, encoding=session_data.ENCODING, xml_declaration=True,
										   pretty_print=True)


def find_longest_wav_filename(dirpath: str, filenames: Iterable[str]):
	null_filename_value = ""
	longest_filename = null_filename_value
	for filename in filenames:
		resolved_path = os.path.join(dirpath, filename)
		mimetype = mimetypes.guess_type(resolved_path)[0]
		if mimetype is not None and WAV_CONTENT_TYPE_PATTERN.match(mimetype):
			longest_filename = max((longest_filename, filename), key=len)

	return None if longest_filename == null_filename_value else longest_filename


def is_untranscribed_session_dir(dirpath: str, filenames: Iterable[str]) -> bool:
	found_uttsfile = False
	found_wavfile = True
	for filename in filenames:
		if filename == UTTERANCE_FILE_NAME:
			# There is already an utterance annotation file; The directory represents a "complete" session
			found_uttsfile = True
			break
		else:
			resolved_path = os.path.join(dirpath, filename)
			mimetype = mimetypes.guess_type(resolved_path)[0]
			if mimetype is not None and WAV_CONTENT_TYPE_PATTERN.match(mimetype):
				found_wavfile = True
				break

	return found_wavfile and not found_uttsfile


def __main(inpaths: Iterable[str]):
	default_namespace = HAT_DATA_NAMESPACE
	# http://stackoverflow.com/a/18340978/1391325
	etree.register_namespace(HAT_DATA_NAMESPACE_NAME, default_namespace)
	create_session_dir_uttfiles(*inpaths)


if __name__ == "__main__":
	if len(sys.argv) < 2:
		raise ValueError("Usage: {} INPATHS...".format(sys.argv[0]))
	else:
		__main(sys.argv[1:])
