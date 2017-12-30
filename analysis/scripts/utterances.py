"""
Functionalities for creating Utterance objects representing an individual utterance in dialogue from Higgins Annotation Tool (HAT) annotations <http://www.speech.kth.se/hat/>.
"""

__author__ = "Todd Shore <errantlinguist+github@gmail.com>"
__copyright__ = "Copyright (C) 2016-2017 Todd Shore"
__license__ = "GNU General Public License, Version 3"

import csv
import sys
from enum import Enum, unique
from typing import Any, Callable, Iterable, List, Optional, Sequence, Tuple

import nltk
import pandas as pd

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

	@staticmethod
	def __merge_consecutive_utts(round_utts: pd.DataFrame) -> pd.DataFrame:
		# noinspection PyProtectedMember
		row_dicts = (row._asdict() for row in round_utts.itertuples(index=False))
		first_row = next(row_dicts)
		last_round_id = first_row[UtteranceTabularDataColumn.ROUND_ID.value]
		last_speaker_id = first_row[UtteranceTabularDataColumn.SPEAKER_ID.value]
		last_diag_role = first_row[UtteranceTabularDataColumn.DIALOGUE_ROLE.value]
		last_start_time = first_row[UtteranceTabularDataColumn.START_TIME.value]
		last_end_time = first_row[UtteranceTabularDataColumn.END_TIME.value]
		last_tokens = list(first_row[UtteranceTabularDataColumn.TOKEN_SEQ.value])

		col_data = dict((col.value, []) for col in UtteranceTabularDataColumn)
		for row_dict in row_dicts:
			tokens = row_dict[UtteranceTabularDataColumn.TOKEN_SEQ.value]
			if tokens:
				round_id = row_dict[UtteranceTabularDataColumn.ROUND_ID.value]
				speaker_id = row_dict[UtteranceTabularDataColumn.SPEAKER_ID.value]
				diag_role = row_dict[UtteranceTabularDataColumn.DIALOGUE_ROLE.value]
				start_time = row_dict[UtteranceTabularDataColumn.START_TIME.value]
				end_time = row_dict[UtteranceTabularDataColumn.END_TIME.value]
				if round_id == last_round_id and speaker_id == last_speaker_id and diag_role == last_diag_role:
					last_start_time = min(last_start_time, start_time)
					last_end_time = max(last_end_time,
										end_time)
					last_tokens.extend(tokens)
				else:
					col_data[UtteranceTabularDataColumn.ROUND_ID.value].append(last_round_id)
					last_round_id = round_id
					col_data[UtteranceTabularDataColumn.SPEAKER_ID.value].append(last_speaker_id)
					last_speaker_id = speaker_id
					col_data[UtteranceTabularDataColumn.DIALOGUE_ROLE.value].append(last_diag_role)
					last_diag_role = diag_role
					col_data[UtteranceTabularDataColumn.START_TIME.value].append(last_start_time)
					last_start_time = start_time
					col_data[UtteranceTabularDataColumn.END_TIME.value].append(last_end_time)
					last_end_time = end_time
					col_data[UtteranceTabularDataColumn.TOKEN_SEQ.value].append(tuple(last_tokens))
					last_tokens = list(tokens)

		# Add the final row
		col_data[UtteranceTabularDataColumn.ROUND_ID.value].append(last_round_id)
		col_data[UtteranceTabularDataColumn.SPEAKER_ID.value].append(last_speaker_id)
		col_data[UtteranceTabularDataColumn.DIALOGUE_ROLE.value].append(last_diag_role)
		col_data[UtteranceTabularDataColumn.START_TIME.value].append(last_start_time)
		col_data[UtteranceTabularDataColumn.END_TIME.value].append(last_end_time)
		col_data[UtteranceTabularDataColumn.TOKEN_SEQ.value].append(tuple(last_tokens))

		return pd.DataFrame(data=col_data, copy=False)

	@classmethod
	def __create_merged_df(cls, result: pd.DataFrame) -> pd.DataFrame:
		round_utts = result.groupby(UtteranceTabularDataColumn.ROUND_ID.value, as_index=False, sort=False)
		return round_utts.apply(cls.__merge_consecutive_utts)

	def __init__(self, merge_consecutive_utts: bool,
				 token_seq_factory: Optional[Callable[[Iterable[str]], Sequence[str]]] = None):
		self.df_postprocessor = self.__create_merged_df if merge_consecutive_utts else lambda df: df
		self.token_seq_factory = TokenSequenceFactory() if token_seq_factory is None else token_seq_factory
		self.converters = {UtteranceTabularDataColumn.TOKEN_SEQ.value: self.__parse_utt_token_seq}

	def __call__(self, infile_path: str) -> pd.DataFrame:
		result = pd.read_csv(infile_path, dialect=self.FILE_CSV_DIALECT, sep=self.FILE_CSV_DIALECT.delimiter,
							 float_precision="round_trip", converters=self.converters, dtype=self.DTYPES)
		# Ensure that rows are sorted according to their chronological ordering withing each round
		result.sort_values(
			by=[UtteranceTabularDataColumn.START_TIME.value, UtteranceTabularDataColumn.END_TIME.value], inplace=True)
		return self.df_postprocessor(result)

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
