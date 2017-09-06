import csv
from enum import Enum, unique
from typing import Dict, Iterable, List, Tuple, Sequence

from session_data import read_events_metadata

_TRUTH_CELL_VALUE = "true"


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
		return self.attr(EntityData.Attribute.REFERENT.value) == _TRUTH_CELL_VALUE

	@property
	def is_selected(self):
		return self.attr(EntityData.Attribute.SELECTED.value) == _TRUTH_CELL_VALUE


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
	def referent_entities(self):
		return (entity for entity in self.entities if entity.is_referent)

	@property
	def selected_entities(self):
		return (entity for entity in self.entities if entity.is_selected)


class GameRound(object):
	@unique
	class Attribute(Enum):
		ID = _DataColumn.ROUND_ID.value

	def __init__(self, start_time, events: Sequence[Event]):
		self.start_time = start_time
		self.events = events

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)


def create_game_rounds(events: Sequence[Event]) -> Iterable[GameRound]:
	enumerated_events = enumerate(events)
	enumerated_event_iter = iter(enumerated_events)
	_, initial_event = next(enumerated_event_iter)
	current_round_id, current_round_event_time = __round_id_and_time(initial_event)
	current_round_event_list_start_idx = 0

	for event_idx, event in enumerated_events:
		first_entity_desc = next(iter(event.entities))
		event_round_id = __entity_round_id(first_entity_desc)
		if event_round_id != current_round_id:
			# print("Finishing round {}.".format(current_round_id), file=sys.stderr)
			completed_round = GameRound(current_round_event_time,
										events[current_round_event_list_start_idx:event_idx])

			current_round_id = event_round_id
			current_round_event_time = __entity_event_time(first_entity_desc)
			current_round_event_list_start_idx = event_idx
			yield completed_round


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
			event_id = int(row[event_id_col_idx])
			entity_descs = result[event_id - 1]
			entity_id = int(row[entity_id_col_idx])
			entity_idx = entity_id - 1
			if entity_descs[entity_idx]:
				raise ValueError("Duplicate rows for event {}, entity {}.", event_id, entity_id)
			else:
				entity_descs[entity_idx] = EntityData(col_idxs, row)

	return result


def __entity_event_time(entity_desc: EntityData) -> float:
	return float(entity_desc.attr(Event.Attribute.TIME.value))


def __entity_round_id(entity_desc: EntityData) -> str:
	return entity_desc.attr(GameRound.Attribute.ID.value)


def __round_id_and_time(event: Event):
	initial_entity_desc = next(iter(event.entities))
	round_id = __entity_round_id(initial_entity_desc)
	round_event_time = __entity_event_time(initial_entity_desc)
	return round_id, round_event_time
