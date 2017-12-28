import csv
import os
from decimal import Decimal
from enum import Enum, unique
from typing import Dict, Iterator, List, Iterable, Tuple

import pandas as pd

DECIMAL_VALUE_TYPE = Decimal
ENCODING = 'utf-8'
SESSION_METADATA_CSV_DIALECT = csv.excel_tab

_DECIMAL_INFINITY = DECIMAL_VALUE_TYPE('Infinity')
_EVENT_FILE_DTYPES = {"NAME": "category", "SHAPE": "category", "SUBMITTER": "category"}
_PARTICIPANT_METADATA_HEADER_ROW_NAME = "PARTICIPANT_ID"

__DECIMAL_VALUE_POOL = {}


class DataColumnProperties(object):
	def __init__(self, name: str, value_transformer):
		self.name = name
		self.value_transformer = value_transformer

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)


def fetch_decimal_value(cell_value: str) -> DECIMAL_VALUE_TYPE:
	try:
		result = __DECIMAL_VALUE_POOL[cell_value]
	except KeyError:
		result = DECIMAL_VALUE_TYPE(cell_value)
		__DECIMAL_VALUE_POOL[cell_value] = result
	return result


def _is_truth_cell_value(val: str) -> bool:
	return val == "true"


@unique
class DataColumn(Enum):
	BLUE = DataColumnProperties("BLUE", int)
	EDGE_COUNT = DataColumnProperties("EDGE_COUNT", int)
	ENTITY_ID = DataColumnProperties("ENTITY", int)
	EVENT_ID = DataColumnProperties("EVENT", int)
	EVENT_NAME = DataColumnProperties("NAME", str)
	EVENT_TIME = DataColumnProperties("TIME", fetch_decimal_value)
	GREEN = DataColumnProperties("BLUE", int)
	HUE = DataColumnProperties("HUE", fetch_decimal_value)
	POSITION_X = DataColumnProperties("POSITION_X", fetch_decimal_value)
	POSITION_Y = DataColumnProperties("POSITION_Y", fetch_decimal_value)
	REFERENT_ENTITY = DataColumnProperties("REFERENT", _is_truth_cell_value)
	RED = DataColumnProperties("RED", int)
	ROUND_ID = DataColumnProperties("ROUND", int)
	SCORE = DataColumnProperties("SCORE", int)
	SELECTED_ENTITY = DataColumnProperties("SELECTED", _is_truth_cell_value)
	SIZE = DataColumnProperties("SIZE", fetch_decimal_value)
	SHAPE = DataColumnProperties("SHAPE", str)
	SUBMITTER = DataColumnProperties("SUBMITTER", str)


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
	UTTERANCES = "utts.xml"

	@property
	def canonical_filename(self):
		return self.value


__SESSION_DATA_FILENAMES = frozenset(datum.canonical_filename for datum in SessionDatum)


class SessionData(object):
	def __init__(self, session_file_prefix: str):
		self.events = os.path.join(session_file_prefix, SessionDatum.EVENTS.canonical_filename)
		self.events_metadata = os.path.join(session_file_prefix, SessionDatum.SESSION_METADATA.canonical_filename)
		self.participant_metadata = os.path.join(session_file_prefix,
												 SessionDatum.PARTICIPANT_METADATA.canonical_filename)
		self.utts = os.path.join(session_file_prefix, SessionDatum.UTTERANCES.canonical_filename)

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
						   encoding=ENCODING, memory_map=True, dtype=_EVENT_FILE_DTYPES)

	def read_events_metadata(self) -> Dict[str, str]:
		with open(self.events_metadata, 'r', encoding=ENCODING) as infile:
			rows = csv.reader(infile, dialect=SESSION_METADATA_CSV_DIALECT)
			return dict(rows)

	def read_metadata_entity_count(self) -> int:
		return int(self.read_metadatum_value(EventMetadataRow.ENTITY_COUNT))

	def read_metadata_event_count(self) -> int:
		return int(self.read_metadatum_value(EventMetadataRow.EVENT_COUNT))

	def read_metadata_round_count(self) -> int:
		return int(self.read_metadatum_value(EventMetadataRow.ROUND_COUNT))

	def read_metadatum_value(self, metadatum: EventMetadataRow):
		events_metadata = self.read_events_metadata()
		return events_metadata[metadatum.value]

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

	def read_round_start_end_times(self) -> Iterator[Tuple[DECIMAL_VALUE_TYPE, DECIMAL_VALUE_TYPE]]:
		return session_round_start_end_times(iter(self.read_round_start_times()))

	def read_round_start_times(self) -> List[DECIMAL_VALUE_TYPE]:
		round_count = self.read_metadata_round_count()

		result = [_DECIMAL_INFINITY] * round_count
		with open(self.events, 'r', encoding=ENCODING) as infile:
			rows = csv.reader(infile, dialect=csv.excel_tab)
			col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
			for row in rows:
				event_time_col_idx = col_idxs[DataColumn.EVENT_TIME.value]
				event_time = fetch_decimal_value(row[event_time_col_idx])
				round_id_col_idx = col_idxs[DataColumn.ROUND_ID.value]
				round_idx = int(row[round_id_col_idx]) - 1
				result[round_idx] = min(result[round_idx], event_time)

		return result

	@property
	def __key(self):
		return self.events, self.events_metadata, self.utts


def is_session_dir(filenames: Iterable[str]) -> bool:
	result = False

	filenames_to_find = set(__SESSION_DATA_FILENAMES)
	for filename in filenames:
		filenames_to_find.discard(filename)
		if not filenames_to_find:
			result = True
			break

	return result


def session_round_start_end_times(round_start_times: Iterator[DECIMAL_VALUE_TYPE]) -> Iterator[
	Tuple[DECIMAL_VALUE_TYPE, DECIMAL_VALUE_TYPE]]:
	current_start_time = next(round_start_times)
	for next_start_time in round_start_times:
		yield current_start_time, next_start_time
		current_start_time = next_start_time

	end_reached = False
	while not end_reached:
		try:
			next_start_time = next(round_start_times)
		except StopIteration:
			next_start_time = _DECIMAL_INFINITY
			end_reached = True

		yield current_start_time, next_start_time
		current_start_time = next_start_time


def walk_session_data(inpaths: Iterable[str]) -> Iterator[Tuple[str, SessionData]]:
	session_dirs = walk_session_dirs(inpaths)
	return ((session_dir, SessionData(session_dir)) for session_dir in session_dirs)


def walk_session_dirs(inpaths: Iterable[str]) -> Iterator[str]:
	for inpath in inpaths:
		for dirpath, _, filenames in os.walk(inpath, followlinks=True):
			if is_session_dir(filenames):
				yield dirpath
