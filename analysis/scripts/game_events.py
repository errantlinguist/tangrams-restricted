import csv
import decimal
import json
import sys
from collections import defaultdict
from enum import Enum, unique
from typing import Any, Dict, Iterable, Iterator, List, MutableSequence, Sequence, Tuple, Union

import session_data

_DECIMAL_INFINITY = decimal.Decimal("Infinity")
_DECIMAL_ONE = decimal.Decimal("1")


class EntityData(object):
	def __init__(self, col_idxs: Dict[str, int], row: Sequence[Any]):
		self.__col_idxs = col_idxs
		self.__row = row

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def attr(self, attr_name: str) -> Any:
		attr_value_idx = self.__col_idxs[attr_name]
		return self.__row[attr_value_idx]

	@property
	def hue(self) -> session_data.DECIMAL_VALUE_TYPE:
		return self.__data_col_attr(session_data.DataColumn.HUE)

	@property
	def is_referent(self) -> bool:
		return self.__data_col_attr(session_data.DataColumn.REFERENT_ENTITY)

	@property
	def is_selected(self) -> bool:
		return self.__data_col_attr(session_data.DataColumn.SELECTED_ENTITY)

	@property
	def shape(self) -> str:
		return self.__data_col_attr(session_data.DataColumn.SHAPE)

	def __data_col_attr(self, col: session_data.DataColumn) -> Any:
		return self.attr(col.value.name)


class Event(object):
	@unique
	class Attribute(Enum):
		ID = session_data.DataColumn.EVENT_ID.value
		NAME = session_data.DataColumn.EVENT_NAME.value
		SUBMITTER = session_data.DataColumn.SUBMITTER.value
		TIME = session_data.DataColumn.EVENT_TIME.value
		SCORE = session_data.DataColumn.SCORE.value

	def __init__(self, entities: Sequence[EntityData], attrs: Dict[Attribute, Any] = None):
		if attrs is None:
			first_entity_desc = next(iter(entities))
			attrs = dict((attr, first_entity_desc.attr(attr.value.name)) for attr in Event.Attribute)
		self.entities = entities
		self.attrs = attrs

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def entity(self, entity_id: int) -> EntityData:
		return self.entities[entity_id - 1]

	def entities_by_id(self):
		return enumerate(self.entities, start=1)

	@property
	def event_id(self) -> int:
		return self.attrs[Event.Attribute.ID]

	@property
	def event_time(self) -> session_data.DECIMAL_VALUE_TYPE:
		return self.attrs[Event.Attribute.TIME]

	@property
	def referent_entities(self) -> Iterator[Tuple[int, EntityData]]:
		return ((entity_id, entity) for (entity_id, entity) in self.entities_by_id() if entity.is_referent)

	@property
	def round_id(self) -> int:
		first_entity_desc = next(iter(self.entities))
		return first_entity_desc.attr(GameRound.Attribute.ID.value.name)

	@property
	def score(self) -> int:
		return self.attrs[Event.Attribute.SCORE]

	@property
	def selected_entities(self) -> Iterator[Tuple[int, EntityData]]:
		return ((entity_id, entity) for (entity_id, entity) in self.entities_by_id() if entity.is_selected)

	@property
	def submitter(self) -> str:
		return self.attrs[Event.Attribute.SUBMITTER]

	def score_round_ratio(self) -> decimal.Decimal:
		try:
			round_count = self.round_id - 1
			result = decimal.Decimal(self.score) / decimal.Decimal(round_count)
		except (decimal.InvalidOperation, ZeroDivisionError):
			result = _DECIMAL_ONE
		return result

	def time_score_ratio(self) -> decimal.Decimal:
		try:
			result = decimal.Decimal(self.event_time) / decimal.Decimal(self.score)
		except ZeroDivisionError:
			result = _DECIMAL_INFINITY
		return result


class EventData(object):
	def __init__(self, events: Iterable[Event], source_participant_ids: Dict[str, str], initial_instructor_id: str):
		self.events = events
		self.source_participant_ids = source_participant_ids
		self.initial_instructor_id = initial_instructor_id

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)


class EventParticipantIdFactory(object):
	"""
		This is a hack to map the non-anonymized usernames in the event logs to anonymized utterance speaker IDs.
		TODO: Remove this after anonymizing all data
	"""

	def __init__(self, initial_instructor_id: str):
		self.initial_instructor_id = initial_instructor_id

	def __call__(self, event: Event):
		"""
		:param event: The event to get the participant ID for
		:return: Either "A" or "B"
		"""
		return "A" if event.submitter == self.initial_instructor_id else "B"


class GameRound(object):
	@unique
	class Attribute(Enum):
		ID = session_data.DataColumn.ROUND_ID.value

	def __init__(self, start_time: float, end_time: Union[float, None], events: Sequence[Event]):
		self.start_time = start_time
		self.end_time = end_time
		self.events = events

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def event(self, event_id: int):
		return self.events[event_id - 1]

	def events_by_id(self):
		return enumerate(self.events, start=1)

	@property
	def initial_event(self):
		return next(iter(self.events))


def create_game_rounds(events: Iterable[Event]) -> Iterator[GameRound]:
	round_events = defaultdict(list)
	for event in events:
		round_events[event.round_id].append(event)

	for event_list in round_events.values():
		event_list.sort(key=lambda e: e.event_id)

	ordered_event_lists = iter(sorted(round_events.items(), key=lambda item: item[0]))
	current_event_list = next(ordered_event_lists)
	current_events = current_event_list[1]
	current_round_start_time = current_events[0].event_time
	for next_event_list in ordered_event_lists:
		next_events = next_event_list[1]
		next_round_start_time = next_events[0].event_time
		yield GameRound(current_round_start_time, next_round_start_time, current_events)

		current_events = next_events
		current_round_start_time = next_round_start_time

	yield GameRound(current_round_start_time, None, current_events)


def read_events(session: session_data.SessionData) -> EventData:
	events_metadata = session.read_events_metadata()

	event_count = int(events_metadata[session_data.MetadataColumn.EVENT_COUNT.value])
	entity_count = int(events_metadata[session_data.MetadataColumn.ENTITY_COUNT.value])
	event_entity_descs = read_event_entity_desc_matrix(session.events, event_count, entity_count)
	events = (Event(entity_descs) for entity_descs in event_entity_descs)

	source_participant_id_json_str = events_metadata[session_data.MetadataColumn.SOURCE_PARTICIPANT_IDS.value]
	source_participant_ids = json.loads(source_participant_id_json_str, encoding=session_data.ENCODING)
	interned_source_participant_ids = dict(
		(sys.intern(key), sys.intern(value)) for (key, value) in source_participant_ids.items())
	initial_instructor_id = sys.intern(events_metadata[session_data.MetadataColumn.INITIAL_INSTRUCTOR_ID.value])
	return EventData(events, interned_source_participant_ids, initial_instructor_id)


def read_event_entity_desc_matrix(infile_path: str, event_count: int, entity_count: int) -> Tuple[
	List[EntityData], ...]:
	result = tuple([None] * entity_count for _ in range(event_count))

	with open(infile_path, 'r', encoding=session_data.ENCODING) as infile:
		rows = csv.reader(infile, dialect="excel-tab")
		col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
		entity_id_col_idx = col_idxs[session_data.DataColumn.ENTITY_ID.value.name]
		event_id_col_idx = col_idxs[session_data.DataColumn.EVENT_ID.value.name]

		for row in rows:
			__transform_row_cell_values(row, col_idxs)
			event_id = row[event_id_col_idx]
			entity_descs = result[event_id - 1]
			entity_id = row[entity_id_col_idx]
			row[entity_id_col_idx] = entity_id
			entity_idx = entity_id - 1
			if entity_descs[entity_idx]:
				raise ValueError("Duplicate rows for event {}, entity {}.", event_id, entity_id)
			else:
				entity_descs[entity_idx] = EntityData(col_idxs, row)

	return result


def __transform_row_cell_value(row: MutableSequence[str], col_idxs: Dict[str, int], data_col: session_data.DataColumn):
	col_props = data_col.value
	idx = col_idxs[col_props.name]
	transformed_val = col_props.value_transformer(row[idx])
	row[idx] = transformed_val


def __transform_row_cell_values(row: MutableSequence[str], col_idxs: Dict[str, int]):
	for data_col in session_data.DataColumn:
		__transform_row_cell_value(row, col_idxs, data_col)
