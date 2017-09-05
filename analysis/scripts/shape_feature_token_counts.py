#!/usr/bin/env python3

import bisect
import csv
import itertools
import os.path
import sys
import xml.etree.ElementTree
from collections import Counter, defaultdict

from nltk import ngrams

from annotations import ANNOTATION_NAMESPACES
from session_dirs import SessionFileName, walk_session_dirs

TRUTH_CELL_VALUE = "true"


class CachingNgramFactory(object):
	def __init__(self, max_ngram_length):
		self.max_ngram_length_stop = max_ngram_length + 1
		self.cache = {}

	def __call__(self, tokens):
		try:
			result = self.cache[tokens]
		except KeyError:
			result = self.__create_ngrams(tokens)
			self.cache[tokens] = result

		return result

	def __create_ngrams(self, tokens):
		ngram_lens = range(1, min(self.max_ngram_length_stop, len(tokens)))
		return tuple(len_ngram for ngram_len in ngram_lens for len_ngram in ngrams(tokens, ngram_len))


class DataColumn(object):
	ENTITY_ID = "ENTITY"
	EVENT_ID = "EVENT"
	EVENT_TIME = "TIME"
	ROUND = "ROUND"
	SHAPE = "SHAPE"


class EntityData(object):
	def __init__(self, col_idxs, row):
		self.col_idxs = col_idxs
		self.row = row

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def attr(self, attr_name):
		attr_value_idx = self.col_idxs[attr_name]
		return self.row[attr_value_idx]


class Event(object):
	def __init__(self, entities, attrs):
		self.entities = entities
		self.attrs = attrs

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	@property
	def referent_entities(self):
		return (entity for entity in self.entities if entity.attr("REFERENT") == TRUTH_CELL_VALUE)

	@property
	def selected_entities(self):
		return (entity for entity in self.entities if entity.attr("SELECTED") == TRUTH_CELL_VALUE)


class EventDataColumn(object):
	ID = DataColumn.EVENT_ID
	NAME = "NAME"
	TIME = DataColumn.EVENT_TIME

	ALL = frozenset((ID, NAME, TIME,))


class GameRound(object):
	def __init__(self, start_time, events):
		self.start_time = start_time
		self.events = events

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)


class SegmentUtteranceFactory(object):
	def __init__(self):
		self.token_seq_singletons = {}

	def __call__(self, segments):
		for segment in segments:
			utt = self.__create(segment)
			if utt:
				yield utt

	def __create(self, segment):
		tokens = segment.iterfind(".//hat:t", ANNOTATION_NAMESPACES)
		content = tuple(stripped_token for stripped_token in (token.text.strip() for token in tokens) if stripped_token)
		if content:
			try:
				singleton_content = self.token_seq_singletons[content]
			except KeyError:
				singleton_content = tuple(sys.intern(token) for token in content)
				self.token_seq_singletons[singleton_content] = singleton_content
			result = Utterance(segment.get("id"), sys.intern(segment.get("source")), float(segment.get("start")),
							   float(segment.get("end")),
							   singleton_content)
		else:
			result = None

		return result


class SortedList(list):
	"""
	see <https://docs.python.org/2/library/bisect.html#searching-sorted-lists>
	"""

	def __init__(self, *args) -> None:
		super().__init__(*args)

	@staticmethod
	def __index_lt(sublist, elem):
		result = bisect.bisect_left(sublist, elem)
		if result:
			return result
		else:
			raise ValueError

	@staticmethod
	def __slice_lt(sublist, elem):
		"""Find values less than elem"""
		idx = SortedList.__index_lt(sublist, elem)
		return sublist[0:idx - 1]

	def index(self, elem, start: int = 0, stop: int = ...) -> int:
		result = bisect.bisect_left(self, elem, start, stop)
		end_idx = min(stop, len(self))
		if result < end_idx and self[result] == elem:
			return result
		else:
			raise ValueError

	def _index_ge(self, x):
		"""Find the index of the first value greater than or equal to x"""
		result = bisect.bisect_left(self, x)
		if result >= len(self):
			raise ValueError
		else:
			return result

	def iter_between(self, start, end):
		# Find the index of the first element equal to or greater than the start element
		start_idx = self._index_ge(start)
		end_idx = SortedList.__index_lt(self, end)
		return itertools.islice(self, start_idx, end_idx)

	def slice_between(self, start, end):
		elems_after_start = self.slice_ge(start)
		return SortedList.__slice_lt(elems_after_start, end)

	def slice_le(self, x):
		"""Find values less than or equal to x"""
		idx = bisect.bisect_right(self, x)
		if idx:
			return self[0: idx - 1]
		else:
			raise ValueError

	def slice_lt(self, x):
		"""Find values less than x"""
		return SortedList.__slice_lt(self, x)

	def slice_ge(self, x):
		"""Find values greater than or equal to x"""
		idx = self._index_ge(x)
		return self[idx:]

	def slice_gt(self, x):
		"""Find values greater than x"""
		idx = bisect.bisect_right(self, x)
		length = len(self)
		if idx == length:
			raise ValueError
		else:
			return self[idx: length]


class Utterance(object):
	def __init__(self, segment_id, speaker_id, start_time, end_time, content):
		self.segment_id = segment_id
		self.speaker_id = speaker_id
		self.start_time = start_time
		self.end_time = end_time
		self.content = content

	@property
	def __key(self):
		return self.segment_id, self.speaker_id, self.start_time, self.end_time, self.content

	def __hash__(self):
		return hash(self.__key)

	def __eq__(self, other):
		return self.__key == other.__key

	def __ne__(self, other):
		return not (self == other)

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)


class UtteranceTimes(object):
	def __init__(self, utts):
		self.utts_by_start_time = defaultdict(SortedList)
		for utt in utts:
			self.utts_by_start_time[utt.start_time].append(utt)
		for utt_list in self.utts_by_start_time.values():
			utt_list.sort(key=lambda utt: utt.end_time)

		self.ascending_start_times = SortedList(self.utts_by_start_time.keys())
		self.ascending_start_times.sort()
		print("Processed utterances with {} unique start time(s).".format(len(self.ascending_start_times)),
			  file=sys.stderr)

	def segments_between(self, start_time, end_time):
		seg_start_times = self.ascending_start_times.iter_between(start_time, end_time)
		started_segs = itertools.chain.from_iterable(
			self.utts_by_start_time[start_time] for start_time in seg_start_times)
		return (seg for seg in started_segs if seg.end_time < end_time)


def create_game_rounds(events):
	enumerated_events = enumerate(event_entity_descs)
	current_round_id, first_event = next(enumerated_events)
	first_entity_desc = next(iter(first_event))
	current_round_event_time = float(first_entity_desc.attr(DataColumn.EVENT_TIME))
	current_round_event_list_start_idx = 0
	for event_idx, entity_descs in enumerated_events:
		first_entity_desc = entity_descs[0]

		event_round_id = int(first_entity_desc.attr(DataColumn.ROUND))
		if event_round_id != current_round_id:
			# print("Finishing round {}.".format(current_round_id), file=sys.stderr)
			completed_round = GameRound(current_round_event_time,
										events[current_round_event_list_start_idx:event_idx])

			current_round_id = event_round_id
			current_round_event_time = float(first_entity_desc.attr(DataColumn.EVENT_TIME))
			current_round_event_list_start_idx = event_idx
			yield completed_round


def create_event(entity_descs):
	first_entity_desc = next(iter(entity_descs))
	event_attrs = dict((name, first_entity_desc.attr(name)) for name in EventDataColumn.ALL)
	return Event(entity_descs, event_attrs)


def print_tabular_data(feature_value_ngram_counts, file):
	header_cells = []
	subheader_cells = []
	for feature_value in sorted(feature_value_ngram_counts.keys()):
		header_cells.append(feature_value)
		subheader_cells.append("NGRAM")
		header_cells.append("")
		subheader_cells.append("COUNT")
	print('\t'.join(header_cells), file=file)
	print('\t'.join(subheader_cells), file=file)

	ngram_count_iters = []
	for _, ngram_counts in sorted(feature_value_ngram_counts.items(), key=lambda item: item[0]):
		alpha_ngram_counts = sorted(ngram_counts.items(), key=lambda item: ''.join(item[0]))
		lensorted_desc_alpha_ngram_counts = sorted(alpha_ngram_counts, key=lambda item: len(item[0]), reverse=True)
		desc_ngram_counts = sorted(lensorted_desc_alpha_ngram_counts, key=lambda item: item[1], reverse=True)
		ngram_count_iter = iter(desc_ngram_counts)
		ngram_count_iters.append(ngram_count_iter)

	finished_iters = set()
	while len(finished_iters) < len(ngram_count_iters):
		row = []
		for ngram_count_iter in ngram_count_iters:
			try:
				ngram_count = next(ngram_count_iter)
				ngram = ' '.join(ngram_count[0])
				count = str(ngram_count[1])
			except StopIteration:
				ngram = ""
				count = ""
				finished_iters.add(ngram_count_iter)
			row.append(ngram)
			row.append(count)
		print('\t'.join(row))


def read_events(infile_path, event_count, entity_count):
	result = tuple([None] * entity_count for _ in range(0, event_count))

	with open(infile_path, 'r') as infile:
		rows = csv.reader(infile, dialect="excel-tab")
		col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
		entity_id_col_idx = col_idxs[DataColumn.ENTITY_ID]
		event_id_col_idx = col_idxs[DataColumn.EVENT_ID]

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


def read_events_metadata(infile_path):
	with open(infile_path, 'r') as infile:
		rows = csv.reader(infile, dialect="excel-tab")
		return dict(rows)


if __name__ == "__main__":
	if len(sys.argv) < 2:
		sys.exit("Usage: {} INPATHS...".format(sys.argv[0]))
	else:

		feature_value_ngram_counts = defaultdict(Counter)
		seg_utt_factory = SegmentUtteranceFactory()
		ngram_factory = CachingNgramFactory(2)

		inpaths = sys.argv[1:]
		for session_dir in walk_session_dirs(inpaths):
			print("Processing session directory \"{}\".".format(session_dir), file=sys.stderr)
			events_metadata_filepath = os.path.join(session_dir, SessionFileName.EVENTS_METADATA)
			events_metadata = read_events_metadata(events_metadata_filepath)

			event_count = int(events_metadata["EVENT_COUNT"])
			entity_count = int(events_metadata["ENTITY_COUNT"])

			events_filepath = os.path.join(session_dir, SessionFileName.EVENTS)
			event_entity_descs = read_events(events_filepath, event_count, entity_count)
			events = [create_event(entity_descs) for entity_descs in event_entity_descs]
			print("Read {} event(s).".format(len(events)), file=sys.stderr)

			utts_filepath = os.path.join(session_dir, SessionFileName.UTTS)
			doc_tree = xml.etree.ElementTree.parse(utts_filepath)
			segments = doc_tree.iterfind('.//hat:segment', ANNOTATION_NAMESPACES)
			utts = seg_utt_factory(segments)
			utts_by_time = UtteranceTimes(utts)

			idxed_game_rounds = iter(enumerate(create_game_rounds(events)))
			round_idx, first_game_round = next(idxed_game_rounds)
			current_round_start_time = first_game_round.start_time
			for round_idx, next_round in idxed_game_rounds:
				# print("Processing round index {}.".format(round_idx), file=sys.stderr)
				initial_event = next(iter(next_round.events))
				referent_entity = next(iter(initial_event.referent_entities))
				# print(referent_entity)
				shape = referent_entity.attr(DataColumn.SHAPE)
				shape_ngram_counts = feature_value_ngram_counts[shape]

				next_round_start_time = next_round.start_time
				current_round_utts = (utts_by_time.segments_between(current_round_start_time,
																	next_round_start_time))
				current_round_ngrams = itertools.chain.from_iterable(
					(ngram_factory(utt.content) for utt in current_round_utts))
				shape_ngram_counts.update(current_round_ngrams)

		print_tabular_data(feature_value_ngram_counts, sys.stdout)
