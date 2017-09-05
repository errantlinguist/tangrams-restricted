import sys
from typing import Iterable, Iterator

import itertools
from collections import defaultdict
from sorted_lists import SortedList

from annotations import ANNOTATION_NAMESPACES


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


class SegmentUtteranceFactory(object):
	def __init__(self):
		self.token_seq_singletons = {}

	def __call__(self, segments) -> Iterator[Utterance]:
		for segment in segments:
			utt = self.__create(segment)
			if utt:
				yield utt

	def __create(self, segment) -> Utterance:
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


def dialogue_utt_str_repr(utts: Iterator[Utterance]) -> str:
	initial_utt = next(utts)
	current_speaker_id = initial_utt.speaker_id
	current_sentences = [__token_seq_repr(initial_utt.content)]

	repr_list = []
	try:
		next_utt = next(utts)
		next_utt_speaker_id = next_utt.speaker_id
		if next_utt_speaker_id == current_speaker_id:
			current_sentences.append(__token_seq_repr(next_utt.content))
		else:
			repr_list.append(__speaker_id_repr(current_speaker_id))
			repr_list.append('"' + ' '.join(current_sentences) + '"')

			current_speaker_id = next_utt_speaker_id
			current_sentences = [__token_seq_repr(next_utt.content)]
	except StopIteration:
		repr_list.append(__speaker_id_repr(current_speaker_id))
		repr_list.append('"' + ' '.join(current_sentences) + '"')
		pass

	return ' '.join(repr_list)


def __capitalize_first_char(string: str) -> str:
	if len(string) < 2:
		return string.upper()
	else:
		first_char = string[0]
		return first_char.upper() + string[1:]


def __speaker_id_repr(speaker_id) -> str:
	return "**{}:**".format(speaker_id)


def __token_seq_repr(tokens: Iterable[str]) -> str:
	token_iter = iter(tokens)
	formatted_tokens = []

	next_token = __capitalize_first_char(next(token_iter))
	end_reached = False
	while not end_reached:
		current_token = next_token
		try:
			next_token = next(token_iter)
		except StopIteration:
			current_token = current_token + '.'
			end_reached = True
		formatted_tokens.append(current_token)

	return ' '.join(formatted_tokens)
