"""
Functionalities for creating Utterance objects representing an individual utterance in dialogue from Higgins Annotation Tool (HAT) annotations <http://www.speech.kth.se/hat/>.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import csv
import sys
from enum import Enum, unique
from typing import Any, Callable, Iterable, Iterator, List, Optional, Sequence, Tuple
from xml.etree.ElementTree import Element

import nltk
import pandas as pd

import annotations

"""
NOTE: See "../src/main/resources/se/kth/speech/nlp/fillers.txt"
"""
FILLER_TOKENS = frozenset(("eh", "ehm", "em", "er", "err", "eugh," "uff", "uh", "uhm", "um", "umm",))
"""
NOTE: See "../src/main/resources/se/kth/speech/coin/tangrams/analysis/SegmentUtteranceFactory.properties"
"""
METALANGUAGE_TOKENS = frozenset(
	("ARTIFACT", "BREATH", "CLICK", "COUGH", "FOREIGN_WORD", "GASP", "GROAN", "GRUNT", "LAUGHTER", "META", "MOAN",
	 "NOISE", "PUFF", "SIGH", "SNIFF", "START_SIGNAL", "UNKNOWN",))
__TOKEN_TRUNCATION_MARKER = '-'


class SegmentUtteranceFactory(object):
	def __init__(self, token_seq_factory: Callable[[Iterable[str]], Sequence[str]],
				 source_speaker_id_factory: Callable[[str], str] = lambda source_id: source_id):
		"""

		:param token_seq_factory: A function for converting a sequence of bare token strings into normalized forms for use as utterance content.
		:param source_speaker_id_factory: A function for converting segment source IDs (e.g. "source234") to utterances speaker IDs (e.g. "superplayer" or "A" or "B")
		"""
		self.token_seq_factory = token_seq_factory
		"""A function for converting a sequence of bare token strings into normalized forms for use as utterance content."""
		self.source_speaker_id_factory = source_speaker_id_factory
		"""A function for converting segment source IDs (e.g. "source234") to utterances speaker IDs (e.g. "superplayer" or "A" or "B")."""

	def __call__(self, segments: Iterable[Element]) -> Iterator["Utterance"]:
		for segment in segments:
			utt = self.__create(segment)
			if utt:
				yield utt

	def __create(self, segment: Element) -> Optional["Utterance"]:
		token_elems = segment.iterfind(".//hat:t", annotations.ANNOTATION_NAMESPACES)
		token_text = (elem.text for elem in token_elems)
		content = self.token_seq_factory(token_text)
		if content:
			speaker_id = self.source_speaker_id_factory(segment.get("source"))
			result = Utterance(segment.get("id"), speaker_id, float(segment.get("start")),
							   float(segment.get("end")), content)
		else:
			result = None

		return result


class Utterance(object):
	@staticmethod
	def between(utts: Iterable["Utterance"], start_time: float, end_time: float):
		return (utt for utt in utts if (utt.start_time >= start_time) and (utt.start_time < end_time))

	def __init__(self, segment_id: str, speaker_id: str, start_time: float, end_time: float, content: Sequence[str]):
		self.segment_id = segment_id
		self.speaker_id = speaker_id
		self.start_time = start_time
		self.end_time = end_time
		self.content = content

	@property
	def __key(self):
		return self.segment_id, self.speaker_id, self.start_time, self.end_time, self.content

	def __eq__(self, other):
		return (self is other or (isinstance(other, type(self))
								  and self.__key == other.__key))

	def __hash__(self):
		return hash(self.__key)

	def __ne__(self, other):
		return not (self == other)

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)


@unique
class UtteranceTabularDataColumn(Enum):
	ROUND_ID = "ROUND"
	SPEAKER_ID = "SPEAKER"
	DIALOGUE_ROLE = "DIALOGUE_ROLE"
	START_TIME = "START_TIME"
	END_TIME = "END_TIME"
	TOKEN_SEQ = "TOKENS"


class UtteranceTabularDataReader(object):
	FILE_ENCODING = "utf-8"
	FILE_CSV_DIALECT = csv.excel_tab
	DTYPES = {UtteranceTabularDataColumn.DIALOGUE_ROLE.value: "category",
			  UtteranceTabularDataColumn.SPEAKER_ID.value: "category"}

	def __init__(self, token_seq_factory: Optional[Callable[[Iterable[str]], Sequence[str]]] = None):
		self.token_seq_factory = TokenSequenceFactory() if token_seq_factory is None else token_seq_factory
		self.converters = {UtteranceTabularDataColumn.TOKEN_SEQ.value: self.__parse_utt_token_seq}

	def __call__(self, infile_path: str) -> pd.DataFrame:
		return pd.read_csv(infile_path, dialect=self.FILE_CSV_DIALECT, sep=self.FILE_CSV_DIALECT.delimiter,
						   float_precision="round_trip", converters=self.converters, dtype=self.DTYPES)

	def __parse_utt_token_seq(self, text: str) -> Sequence[str]:
		return self.token_seq_factory(text)


def is_semantically_relevant_token(token: str) -> bool:
	return token not in METALANGUAGE_TOKENS and token not in FILLER_TOKENS and not is_disfluency(token)


class TokenSequenceFactory(object):
	EMPTY_SEQ = ()

	def __init__(self, token_filter: Callable[[str], bool] = is_semantically_relevant_token):
		self.token_filter = token_filter
		self.token_seq_singletons = {}

	def __call__(self, text: str) -> Tuple[str, ...]:
		if text:
			try:
				result = self.token_seq_singletons[text]
			except KeyError:
				tokens = nltk.tokenize.word_tokenize(text)
				filtered_tokens = (token for token in tokens if self.token_filter(token))
				result = tuple(sys.intern(token) for token in filtered_tokens)
				self.token_seq_singletons[result] = result
		else:
			result = self.EMPTY_SEQ

		return result


def dialogue_utt_str_repr(utts: Iterable[Utterance]) -> str:
	repr_list = []
	grouped_utts = group_utts_by_speaker_id(utts)
	for speaker_id, utt_group in grouped_utts:
		speaker_repr = __speaker_id_repr(speaker_id)
		repr_list.append(speaker_repr)
		sentence_repr = '"' + join_utt_sentence_reprs(utt_group) + '"'
		repr_list.append(sentence_repr)

	return ' '.join(repr_list)


def group_utts_by_speaker_id(utts: Iterable[Utterance]) -> List[Tuple[List[Utterance], ...]]:
	result = []

	current_speaker_id = None
	current_speaker_utts = []
	for utt in utts:
		utt_speaker_id = utt.speaker_id
		if utt_speaker_id == current_speaker_id:
			current_speaker_utts.append(utt)
		else:
			if current_speaker_utts:
				result.append((current_speaker_id, current_speaker_utts))
			current_speaker_id = utt_speaker_id
			current_speaker_utts = [utt]

	if current_speaker_utts:
		result.append((current_speaker_id, current_speaker_utts))

	return result


def is_disfluency(token: str) -> bool:
	return token.startswith(__TOKEN_TRUNCATION_MARKER) or token.endswith(__TOKEN_TRUNCATION_MARKER)


def join_utt_sentence_reprs(utts: Iterable[Utterance]) -> str:
	return ' '.join(token_seq_repr(utt.content) for utt in utts)


def token_seq_repr(tokens: Iterable[str]) -> str:
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


def __capitalize_first_char(string: str) -> str:
	if len(string) < 2:
		return string.upper()
	else:
		first_char = string[0]
		return first_char.upper() + string[1:]


def __speaker_id_repr(speaker_id: Any) -> str:
	return "**{}:**".format(speaker_id)
