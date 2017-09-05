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
from game_events import EntityData, create_game_rounds, read_events
from session_data import read_events_metadata, walk_session_data


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


if __name__ == "__main__":
	if len(sys.argv) < 2:
		sys.exit("Usage: {} INPATHS...".format(sys.argv[0]))
	else:

		feature_value_ngram_counts = defaultdict(Counter)
		seg_utt_factory = SegmentUtteranceFactory()
		ngram_factory = CachingNgramFactory(2)

		inpaths = sys.argv[1:]
		for _, session in walk_session_data(inpaths):
			events = tuple(read_events(session))
			print("Read {} event(s).".format(len(events)), file=sys.stderr)

			doc_tree = xml.etree.ElementTree.parse(session.utts)
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
				shape = referent_entity.attr(EntityData.Attribute.SHAPE.value)
				shape_ngram_counts = feature_value_ngram_counts[shape]

				next_round_start_time = next_round.start_time
				current_round_utts = (utts_by_time.segments_between(current_round_start_time,
																	next_round_start_time))
				current_round_ngrams = itertools.chain.from_iterable(
					(ngram_factory(utt.content) for utt in current_round_utts))
				shape_ngram_counts.update(current_round_ngrams)

		print_tabular_data(feature_value_ngram_counts, sys.stdout)
