import csv
import os
from decimal import Decimal
from enum import Enum, unique
from typing import Any, Callable, Dict, Iterator, List, Iterable, Tuple

DECIMAL_VALUE_TYPE = Decimal
ENCODING = 'utf-8'

_DECIMAL_INFINITY = DECIMAL_VALUE_TYPE('Infinity')

__DECIMAL_VALUE_POOL = {}


class DataColumnProperties(object):
	def __init__(self, name: str, value_transformer: Callable[[str], Any]):
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

	def __eq__(self, other):
		return (self is other or (isinstance(other, type(self))
								  and self.__key == other.__key))

	def __hash__(self):
		return hash(self.__key)

	def __ne__(self, other):
		return not (self == other)

	def __repr__(self):
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

		result = [_DECIMAL_INFINITY] * round_count
		with open(self.events, 'r', encoding=ENCODING) as infile:
			rows = csv.reader(infile, dialect="excel-tab")
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
