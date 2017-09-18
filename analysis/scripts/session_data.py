import csv
import os
from decimal import Decimal
from enum import Enum, unique
from typing import Any, Callable, Dict, Iterator, List, Iterable, Tuple

DECIMAL_VALUE_TYPE = Decimal
ENCODING = 'utf-8'


class DataColumnProperties(object):
	def __init__(self, name: str, value_transformer: Callable[[str], Any]):
		self.name = name
		self.value_transformer = value_transformer

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)


def _is_truth_cell_value(val: str) -> bool:
	return val == "true"


@unique
class DataColumn(Enum):
	BLUE = DataColumnProperties("BLUE", int)
	EDGE_COUNT = DataColumnProperties("EDGE_COUNT", int)
	ENTITY_ID = DataColumnProperties("ENTITY", int)
	EVENT_ID = DataColumnProperties("EVENT", int)
	EVENT_NAME = DataColumnProperties("NAME", str)
	EVENT_TIME = DataColumnProperties("TIME", DECIMAL_VALUE_TYPE)
	GREEN = DataColumnProperties("BLUE", int)
	HUE = DataColumnProperties("HUE", DECIMAL_VALUE_TYPE)
	POSITION_X = DataColumnProperties("POSITION_X", DECIMAL_VALUE_TYPE)
	POSITION_Y = DataColumnProperties("POSITION_Y", DECIMAL_VALUE_TYPE)
	REFERENT_ENTITY = DataColumnProperties("REFERENT", _is_truth_cell_value)
	RED = DataColumnProperties("RED", int)
	ROUND_ID = DataColumnProperties("ROUND", int)
	SCORE = DataColumnProperties("SCORE", int)
	SELECTED_ENTITY = DataColumnProperties("SELECTED", _is_truth_cell_value)
	SIZE = DataColumnProperties("SIZE", DECIMAL_VALUE_TYPE)
	SHAPE = DataColumnProperties("SHAPE", str)
	SUBMITTER = DataColumnProperties("SUBMITTER", str)


@unique
class MetadataColumn(Enum):
	ENTITY_COUNT = "ENTITY_COUNT"
	EVENT_COUNT = "EVENT_COUNT"
	INITIAL_INSTRUCTOR_ID = "INITIAL_INSTRUCTOR_ID"
	SOURCE_PARTICIPANT_IDS = "SOURCE_PARTICIPANT_IDS"
	ROUND_COUNT = "ROUND_COUNT"


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
	def __init__(self, session_file_prefix: str):
		self.events = os.path.join(session_file_prefix, SessionDatum.EVENTS.canonical_filename)
		self.events_metadata = os.path.join(session_file_prefix, SessionDatum.EVENTS_METADATA.canonical_filename)
		self.utts = os.path.join(session_file_prefix, SessionDatum.UTTS.canonical_filename)

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def read_events_metadata(self, ) -> Dict[str, str]:
		with open(self.events_metadata, 'r', encoding=ENCODING) as infile:
			rows = csv.reader(infile, dialect="excel-tab")
			return dict(rows)

	def read_metadata_entity_count(self) -> int:
		return int(self.read_metadatum_value(MetadataColumn.ENTITY_COUNT))

	def read_metadata_event_count(self) -> int:
		return int(self.read_metadatum_value(MetadataColumn.EVENT_COUNT))

	def read_metadata_round_count(self) -> int:
		return int(self.read_metadatum_value(MetadataColumn.ROUND_COUNT))

	def read_metadatum_value(self, metadatum: MetadataColumn):
		events_metadata = self.read_events_metadata()
		return events_metadata[metadatum.value]

	def read_round_start_end_times(self) -> Iterator[Tuple[DECIMAL_VALUE_TYPE, DECIMAL_VALUE_TYPE]]:
		return session_round_start_end_times(iter(self.read_round_start_times()))

	def read_round_start_times(self) -> List[DECIMAL_VALUE_TYPE]:
		round_count = self.read_metadata_round_count()

		result = [DECIMAL_VALUE_TYPE('inf')] * round_count
		with open(self.events, 'r', encoding=ENCODING) as infile:
			rows = csv.reader(infile, dialect="excel-tab")
			col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
			for row in rows:
				event_time_col_idx = col_idxs[DataColumn.EVENT_TIME.value]
				event_time = DECIMAL_VALUE_TYPE(row[event_time_col_idx])
				round_id_col_idx = col_idxs[DataColumn.ROUND_ID.value]
				round_idx = int(row[round_id_col_idx]) - 1
				result[round_idx] = min(result[round_idx], event_time)

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
			next_start_time = DECIMAL_VALUE_TYPE('Infinity')
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
