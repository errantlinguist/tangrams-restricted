import csv
from collections import defaultdict
from enum import Enum, unique
from typing import Dict, Iterable, List, Sequence, Tuple, Union

from session_data import read_events_metadata


@unique
class _DataColumn(Enum):
	ENTITY_ID = "ENTITY"
	EVENT_ID = "EVENT"
	EVENT_NAME = "NAME"
	EVENT_TIME = "TIME"
	REFERENT_ENTITY = "REFERENT"
	ROUND_ID = "ROUND"
	SELECTED_ENTITY = "SELECTED"
	SHAPE = "SHAPE"


def __is_truth_cell_value(val):
	return val == "true"


class EntityData(object):
	@unique
	class Attribute(Enum):
		REFERENT = _DataColumn.REFERENT_ENTITY.value
		SELECTED = _DataColumn.SELECTED_ENTITY.value
		SHAPE = _DataColumn.SHAPE.value

	def __init__(self, col_idxs, row):
		self.__col_idxs = col_idxs
		self.__row = row

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def attr(self, attr_name):
		attr_value_idx = self.__col_idxs[attr_name]
		return self.__row[attr_value_idx]

	@property
	def is_referent(self):
		return self.attr(EntityData.Attribute.REFERENT.value)

	@property
	def is_selected(self):
		return self.attr(EntityData.Attribute.SELECTED.value)


class Event(object):
	@unique
	class Attribute(Enum):
		ID = _DataColumn.EVENT_ID.value
		NAME = _DataColumn.EVENT_NAME.value
		TIME = _DataColumn.EVENT_TIME.value

	def __init__(self, entities: Sequence[EntityData], attrs: Dict[Attribute, str] = None):
		if attrs is None:
			first_entity_desc = next(iter(entities))
			attrs = dict((attr, first_entity_desc.attr(attr.value)) for attr in Event.Attribute)
		self.entities = entities
		self.attrs = attrs

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	@property
	def event_id(self):
		return self.attrs[Event.Attribute.ID]

	@property
	def event_time(self):
		return self.attrs[Event.Attribute.TIME]

	@property
	def referent_entities(self):
		return (entity for entity in self.entities if entity.is_referent)

	@property
	def round_id(self):
		first_entity_desc = next(iter(self.entities))
		return _entity_round_id(first_entity_desc)

	@property
	def selected_entities(self):
		return (entity for entity in self.entities if entity.is_selected)


class GameRound(object):
	@unique
	class Attribute(Enum):
		ID = _DataColumn.ROUND_ID.value

	def __init__(self, start_time: float, end_time: Union[float, None], events: Sequence[Event]):
		self.start_time = start_time
		self.end_time = end_time
		self.events = events

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)


def create_game_rounds(events: Iterable[Event]) -> Iterable[GameRound]:
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


def read_events(session) -> Iterable[Event]:
	events_metadata = read_events_metadata(session.events_metadata)

	event_count = int(events_metadata["EVENT_COUNT"])
	entity_count = int(events_metadata["ENTITY_COUNT"])

	event_entity_descs = read_event_entity_desc_matrix(session.events, event_count, entity_count)
	return (Event(entity_descs) for entity_descs in event_entity_descs)


def read_event_entity_desc_matrix(infile_path, event_count, entity_count) -> Tuple[List[EntityData], ...]:
	result = tuple([None] * entity_count for _ in range(0, event_count))

	with open(infile_path, 'r') as infile:
		rows = csv.reader(infile, dialect="excel-tab")
		col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
		entity_id_col_idx = col_idxs[_DataColumn.ENTITY_ID.value]
		event_id_col_idx = col_idxs[_DataColumn.EVENT_ID.value]

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


def __entity_event_time(entity_desc: EntityData) -> float:
	return float(entity_desc.attr(Event.Attribute.TIME.value))


def _entity_round_id(entity_desc: EntityData) -> str:
	return entity_desc.attr(GameRound.Attribute.ID.value)


def __round_id_and_time(event: Event):
	initial_entity_desc = next(iter(event.entities))
	round_id = _entity_round_id(initial_entity_desc)
	round_event_time = __entity_event_time(initial_entity_desc)
	return round_id, round_event_time


def __transform_row_cell_value(row, col_idxs, data_col: _DataColumn, transformer):
	idx = col_idxs[data_col.value]
	transformed_val = transformer(row[idx])
	row[idx] = transformed_val


def __transform_row_cell_values(row, col_idxs):
	__transform_row_cell_value(row, col_idxs, _DataColumn.ENTITY_ID, int)
	__transform_row_cell_value(row, col_idxs, _DataColumn.EVENT_ID, int)
	__transform_row_cell_value(row, col_idxs, _DataColumn.EVENT_TIME, float)
	__transform_row_cell_value(row, col_idxs, _DataColumn.REFERENT_ENTITY, __is_truth_cell_value)
	__transform_row_cell_value(row, col_idxs, _DataColumn.ROUND_ID, int)
	__transform_row_cell_value(row, col_idxs, _DataColumn.SELECTED_ENTITY, __is_truth_cell_value)
