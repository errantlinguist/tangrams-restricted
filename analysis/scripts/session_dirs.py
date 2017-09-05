from os import walk


class SessionFileName(object):
	EVENTS = "events.tsv"
	EVENTS_METADATA = "events-metadata.tsv"
	UTTS = "utts.xml"

	ALL = frozenset((EVENTS, EVENTS_METADATA, UTTS))

	def __init__(self):
		raise RuntimeError("Class should not be instantiated.")


def is_session_dir(filenames):
	result = False

	filenames_to_find = set(SessionFileName.ALL)
	for filename in filenames:
		filenames_to_find.discard(filename)
		if not filenames_to_find:
			result = True
			break

	return result


def walk_session_dirs(inpaths):
	for inpath in inpaths:
		for dirpath, _, filenames in walk(inpath, followlinks=True):
			if is_session_dir(filenames):
				yield dirpath
