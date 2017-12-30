import csv
import os
from enum import Enum, unique
from typing import Dict, Iterator, Iterable, Tuple

import pandas as pd

ENCODING = 'utf-8'
SESSION_METADATA_CSV_DIALECT = csv.excel_tab
EVENT_FILE_DTYPES = {"NAME": "category", "SHAPE": "category", "SUBMITTER": "category"}

_PARTICIPANT_METADATA_HEADER_ROW_NAME = "PARTICIPANT_ID"


@unique
class EventDataColumn(Enum):
	ALPHA = "ALPHA"
	BLUE = "BLUE"
	BRIGHTNESS = "BRIGHTNESS"
	EDGE_COUNT = "EDGE_COUNT"
	ENTITY_ID = "ENTITY"
	EVENT_ID = "EVENT"
	EVENT_NAME = "NAME"
	EVENT_TIME = "TIME"
	GREEN = "GREEN"
	HUE = "HUE"
	POSITION_X = "POSITION_X"
	POSITION_Y = "POSITION_Y"
	REFERENT_ENTITY = "REFERENT"
	RED = "RED"
	ROUND_ID = "ROUND"
	SATURATION = "SATURATION"
	SCORE = "SCORE"
	SELECTED_ENTITY = "SELECTED"
	SIZE = "SIZE"
	SHAPE = "SHAPE"
	SUBMITTER = "SUBMITTER"


@unique
class EventMetadataRow(Enum):
	ENTITY_COUNT = "ENTITY_COUNT"
	EVENT_COUNT = "EVENT_COUNT"
	INITIAL_INSTRUCTOR_ID = "INITIAL_INSTRUCTOR_ID"
	ROUND_COUNT = "ROUND_COUNT"


@unique
class ParticipantMetadataRow(Enum):
	SOURCE_ID = "SOURCE_ID"


@unique
class SessionDatum(Enum):
	EVENTS = "events.tsv"
	SESSION_METADATA = "session-metadata.tsv"
	PARTICIPANT_METADATA = "participant-metadata.tsv"
	UTTERANCES = "utts.tsv"

	@property
	def canonical_filename(self):
		return self.value


__SESSION_DATA_FILENAMES = frozenset(datum.canonical_filename for datum in SessionDatum)


class SessionData(object):
	def __init__(self, name: str, session_file_prefix: str):
		self.name = name
		self.events = os.path.join(session_file_prefix, SessionDatum.EVENTS.canonical_filename)
		self.session_metadata = os.path.join(session_file_prefix, SessionDatum.SESSION_METADATA.canonical_filename)
		self.participant_metadata = os.path.join(session_file_prefix,
												 SessionDatum.PARTICIPANT_METADATA.canonical_filename)
		self.utts = os.path.join(session_file_prefix, SessionDatum.UTTERANCES.canonical_filename)

	@property
	def __key(self):
		return self.name, self.events, self.session_metadata, self.utts

	def __eq__(self, other):
		return (self is other or (isinstance(other, type(self))
								  and self.__key == other.__key))

	def __hash__(self):
		return hash(self.__key)

	def __ne__(self, other):
		return not (self == other)

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def read_events(self) -> pd.DataFrame:
		return pd.read_csv(self.events, sep=csv.excel_tab.delimiter, dialect=csv.excel_tab,
						   float_precision="round_trip",
						   encoding=ENCODING, memory_map=True, dtype=EVENT_FILE_DTYPES)

	def read_session_metadata(self) -> Dict[str, str]:
		with open(self.session_metadata, 'r', encoding=ENCODING) as infile:
			rows = csv.reader(infile, dialect=SESSION_METADATA_CSV_DIALECT)
			return dict(rows)

	def read_participant_metadata(self) -> Dict[str, Dict[str, str]]:
		result = {}
		with open(self.participant_metadata, 'r', encoding=ENCODING) as infile:
			rows = csv.reader(infile, dialect=csv.excel_tab)
			headed_rows = dict((row[0], row[1:]) for row in rows)
		participant_ids = headed_rows[_PARTICIPANT_METADATA_HEADER_ROW_NAME]
		participant_id_idxs = tuple((participant_id, idx) for (idx, participant_id) in enumerate(participant_ids))
		non_header_rows = ((metadatum_name, participant_values) for (metadatum_name, participant_values) in
						   headed_rows.items() if metadatum_name != _PARTICIPANT_METADATA_HEADER_ROW_NAME)
		for metadatum_name, participant_values in non_header_rows:
			participant_value_dict = dict(
				(participant_id, participant_values[idx]) for (participant_id, idx) in participant_id_idxs)
			result[metadatum_name] = participant_value_dict

		return result


def is_session_dir(filenames: Iterable[str]) -> bool:
	result = False

	filenames_to_find = set(__SESSION_DATA_FILENAMES)
	for filename in filenames:
		filenames_to_find.discard(filename)
		if not filenames_to_find:
			result = True
			break

	return result


def walk_session_data(inpaths: Iterable[str]) -> Iterator[Tuple[str, SessionData]]:
	session_dirs = walk_session_dirs(inpaths)
	# Use the basename of the directory (i.e. the last directory name in the path) as the session name
	return ((session_dir, SessionData(os.path.basename(session_dir), session_dir)) for session_dir in session_dirs)


def walk_session_dirs(inpaths: Iterable[str]) -> Iterator[str]:
	for inpath in inpaths:
		for dirpath, _, filenames in os.walk(inpath, followlinks=True):
			if is_session_dir(filenames):
				yield dirpath
