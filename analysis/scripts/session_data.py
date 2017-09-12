import csv
import os
from enum import Enum, unique
from typing import Dict, Iterator, List, Iterable, Tuple

ENCODING = 'utf-8'

@unique
class DataColumn(Enum):
	ENTITY_ID = "ENTITY"
	EVENT_ID = "EVENT"
	EVENT_NAME = "NAME"
	EVENT_TIME = "TIME"
	REFERENT_ENTITY = "REFERENT"
	ROUND_ID = "ROUND"
	SELECTED_ENTITY = "SELECTED"
	SUBMITTER = "SUBMITTER"
	SHAPE = "SHAPE"


@unique
class MetadataColumn(Enum):
	ENTITY_COUNT = "ENTITY_COUNT"
	EVENT_COUNT = "EVENT_COUNT"
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

	def read_round_start_end_times(self) -> Iterator[Tuple[float, float]]:
		return session_round_start_end_times(iter(self.read_round_start_times()))

	def read_round_start_times(self) -> List[float]:
		round_count = self.read_metadata_round_count()

		result = [float('inf')] * round_count
		with open(self.events, 'r', encoding=ENCODING) as infile:
			rows = csv.reader(infile, dialect="excel-tab")
			col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
			for row in rows:
				event_time_col_idx = col_idxs[DataColumn.EVENT_TIME.value]
				event_time = float(row[event_time_col_idx])
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


def walk_session_data(inpaths: Iterable[str]) -> Iterator[Tuple[str, SessionData]]:
	session_dirs = walk_session_dirs(inpaths)
	return ((session_dir, SessionData(session_dir)) for session_dir in session_dirs)


def walk_session_dirs(inpaths: Iterable[str]) -> Iterator[str]:
	for inpath in inpaths:
		for dirpath, _, filenames in os.walk(inpath, followlinks=True):
			if is_session_dir(filenames):
				yield dirpath


def session_round_start_end_times(round_start_times: Iterator[float]) -> Iterator[Tuple[float, float]]:
	current_start_time = next(round_start_times)
	for next_start_time in round_start_times:
		yield current_start_time, next_start_time
		current_start_time = next_start_time

	end_reached = False
	while not end_reached:
		try:
			next_start_time = next(round_start_times)
		except StopIteration:
			next_start_time = float('inf')
			end_reached = True

		yield current_start_time, next_start_time
		current_start_time = next_start_time
