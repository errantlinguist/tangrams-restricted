import itertools
import sys
from collections import defaultdict
from typing import Callable, Iterable, Iterator, List
from xml.etree.ElementTree import parse as parse_etree

from annotations import ANNOTATION_NAMESPACES
from sorted_lists import SortedList

"""
NOTE: See "../src/main/resources/se/kth/speech/coin/tangrams/analysis/SegmentUtteranceFactory.properties"
"""
METALANGUAGE_TOKENS = frozenset(("ARTIFACT", "BREATH", "CLICK", "COUGH", "GROAN", "GRUNT", "LAUGHTER", "META", "MOAN",
								 "NOISE", "SIGH", "SNIFF", "UNKNOWN",))


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
	def __init__(self, token_filter: Callable[[str], bool] = lambda token: token not in METALANGUAGE_TOKENS):
		self.token_filter = token_filter
		self.token_seq_singletons = {}

	def __call__(self, segments) -> Iterator[Utterance]:
		for segment in segments:
			utt = self.__create(segment)
			if utt:
				yield utt

	def __create(self, segment) -> Utterance:
		tokens = segment.iterfind(".//hat:t", ANNOTATION_NAMESPACES)
		content = tuple(stripped_token for stripped_token in (token.text.strip() for token in tokens) if
						stripped_token and self.token_filter(stripped_token))
		if content:
			try:
				singleton_content = self.token_seq_singletons[content]
			except KeyError:
				singleton_content = tuple(sys.intern(token) for token in content)
				self.token_seq_singletons[singleton_content] = singleton_content
			result = Utterance(segment.get("id"), sys.intern(segment.get("source")), float(segment.get("start")),
							   float(segment.get("end")), singleton_content)
		else:
			result = None

		return result


class UtteranceTimes(object):
	def __init__(self, utts : Iterable[Utterance]):
		self.utts_by_start_time = defaultdict(SortedList)
		for utt in utts:
			self.utts_by_start_time[utt.start_time].append(utt)
		for utt_list in self.utts_by_start_time.values():
			utt_list.sort(key=lambda utt: utt.end_time)

		self.ascending_start_times = SortedList(self.utts_by_start_time.keys())
		self.ascending_start_times.sort()

	def between(self, start_time : float, end_time : float) -> Iterator[Utterance]:
		utt_start_times = self.ascending_start_times.iter_between(start_time, end_time)
		started_utts = itertools.chain.from_iterable(
			self.utts_by_start_time[start_time] for start_time in utt_start_times)
		return (utt for utt in started_utts if utt.start_time < end_time)


def dialogue_utt_str_repr(utts: Iterator[Utterance]) -> str:
	repr_list = []

	grouped_utts = group_utts_by_speaker_id(utts)
	for utt_group in grouped_utts:
		speaker_repr = __speaker_id_repr(utt_group[0].speaker_id)
		repr_list.append(speaker_repr)
		sentence_repr = '"' + ' '.join(__token_seq_repr(utt.content) for utt in utt_group) + '"'
		repr_list.append(sentence_repr)

	return ' '.join(repr_list)


def group_utts_by_speaker_id(utts: Iterable[Utterance]) -> List[List[Utterance]]:
	result = []

	current_speaker_id = None
	current_speaker_utts = []
	for utt in utts:
		utt_speaker_id = utt.speaker_id
		if utt_speaker_id == current_speaker_id:
			current_speaker_utts.append(utt)
		else:
			if current_speaker_utts:
				result.append(current_speaker_utts)
			current_speaker_id = utt_speaker_id
			current_speaker_utts = [utt]

	if current_speaker_utts:
		result.append(current_speaker_utts)

	return result


def read_segments(infile_path):
	print("Reading XML file \"{}\".".format(infile_path), file=sys.stderr)
	doc_tree = parse_etree(infile_path)
	return doc_tree.iterfind(".//hat:segment", ANNOTATION_NAMESPACES)


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
