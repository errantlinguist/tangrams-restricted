import csv
import os
from enum import Enum, unique
from typing import Iterator, Iterable, Tuple


@unique
class SessionDatum(Enum):
	EVENTS = "events.tsv"
	EVENTS_METADATA = "events-metadata.tsv"
	UTTS = "utts.xml"

	@property
	def canonical_filename(self):
		return self.value


__SESSION_DATA_FILENAMES = frozenset(datum.canonical_filename for datum in SessionDatum)


class SessionData(object):
	def __init__(self, session_file_prefix):
		self.events = os.path.join(session_file_prefix, SessionDatum.EVENTS.canonical_filename)
		self.events_metadata = os.path.join(session_file_prefix, SessionDatum.EVENTS_METADATA.canonical_filename)
		self.utts = os.path.join(session_file_prefix, SessionDatum.UTTS.canonical_filename)

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)


def is_session_dir(filenames: Iterable[str]) -> bool:
	result = False

	filenames_to_find = set(__SESSION_DATA_FILENAMES)
	for filename in filenames:
		filenames_to_find.discard(filename)
		if not filenames_to_find:
			result = True
			break

	return result


def read_events_metadata(infile_path: str) -> dict:
	with open(infile_path, 'r') as infile:
		rows = csv.reader(infile, dialect="excel-tab")
		return dict(rows)


def walk_session_data(inpaths) -> Iterator[Tuple[str, SessionData]]:
	session_dirs = walk_session_dirs(inpaths)
	return ((session_dir, SessionData(session_dir)) for session_dir in session_dirs)


def walk_session_dirs(inpaths: Iterable[str]) -> Iterator[str]:
	for inpath in inpaths:
		for dirpath, _, filenames in os.walk(inpath, followlinks=True):
			if is_session_dir(filenames):
				yield dirpath
