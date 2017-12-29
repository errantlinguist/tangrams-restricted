from enum import Enum, unique


@unique
class EventColumn(Enum):
	DYAD_ID = "DYAD"
	ENTITY_ID = "ENTITY"
	ENTITY_SHAPE = "SHAPE"
	EVENT_ID = "EVENT"
	EVENT_NAME = "NAME"
	EVENT_SUBMITTER = "SUBMITTER"
	EVENT_TIME = "TIME"
	ROUND_ID = "ROUND"
