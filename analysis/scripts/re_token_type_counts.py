from collections import Counter
from typing import Callable, Dict, MutableSequence, Sequence

import utterances


class FilteredTokenCountDatum(object):
	"""
	This class keeps counts of all tokens in a set of utterances in addition to counts for "relevant" tokens out of the set of all tokens.
	"""

	def __init__(self, all_tokens: "TokenCountDatum" = None, relevant_tokens: "TokenCountDatum" = None,
				 utts: MutableSequence[utterances.Utterance] = None):
		"""

		:param all_tokens: Counts for all tokens observed.
		:type all_tokens: TokenCountDatum
		:param relevant_tokens: Counts only for the relevant tokens which were observed.
		:type relevant_tokens: TokenCountDatum
		:param utts: The utterances used for deriving counts.
		:type utts: MutableSequence[utterances.Utterance]
		"""
		self.all_tokens = TokenCountDatum() if all_tokens is None else all_tokens
		"""Counts for all tokens observed."""
		self.relevant_tokens = TokenCountDatum() if relevant_tokens is None else relevant_tokens
		"""Counts only for the relevant tokens which were observed."""
		self.utts = [] if utts is None else utts
		"""The utterances used for deriving counts."""

	@property
	def __key(self):
		return self.all_tokens, self.relevant_tokens, self.utts

	def __eq__(self, other):
		return (self is other or (isinstance(other, type(self))
								  and self.__key == other.__key))

	def __ne__(self, other):
		return not (self == other)

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def add(self, utts: Sequence[utterances.Utterance], token_filter: Callable[[str], bool] = lambda _: True):
		for utt in utts:
			self.all_tokens.token_counts.update(utt.content)
			self.relevant_tokens.token_counts.update(token for token in utt.content if token_filter(token))
			self.utts.append(utt)

	def copy(self):
		return FilteredTokenCountDatum(self.all_tokens.copy(), self.relevant_tokens.copy(), self.utts.copy())

	def update(self, other: "FilteredTokenCountDatum"):
		self.all_tokens.update(other.all_tokens)
		self.relevant_tokens.update(other.relevant_tokens)
		self.utts.extend(other.utts)


class FilteringTokenCounter(object):
	def __init__(self, token_filter: Callable[[str], bool] = lambda _: True):
		self.token_filter = token_filter

	def __call__(self, utts: Sequence[utterances.Utterance]) -> FilteredTokenCountDatum:
		result = FilteredTokenCountDatum()
		result.add(utts, self.token_filter)
		return result


class RoundTokenCountDatum(object):
	"""
	This class represents token counts for a single round in a game.
	"""

	def __init__(self, round_data: FilteredTokenCountDatum, cumulative_data: FilteredTokenCountDatum):
		"""

		:param round_data: Token counts for the individual round this object represents.
		:type round_data: FilteredTokenCountDatum
		:param cumulative_data: Cumulative counts for the entire game up to and including the individual round this object represents.
		:type cumulative_data: FilteredTokenCountDatum
		"""
		self.round_data = round_data
		"""Token counts for the individual round this object represents."""
		self.cumulative_data = cumulative_data
		"""Cumulative counts for the entire game up to and including the individual round this object represents."""

	@property
	def __key(self):
		return self.round_data, self.cumulative_data

	def __eq__(self, other):
		return (self is other or (isinstance(other, type(self))
								  and self.__key == other.__key))

	def __ne__(self, other):
		return not (self == other)

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)


class TokenCountDatum(object):
	"""
	This class represents counts of individual tokens observed.
	"""

	def __init__(self, token_counts: Dict[str, int] = None):
		"""

		:param token_counts: Counts for each token type (i.e. unique word) observed.
		:type token_counts: Dict[str, int]
		"""
		self.token_counts = Counter() if token_counts is None else token_counts
		"""Counts for each token type (i.e. unique word) observed."""

	@property
	def __key(self):
		return self.token_counts

	def __eq__(self, other):
		return (self is other or (isinstance(other, type(self))
								  and self.__key == other.__key))

	def __ne__(self, other):
		return not (self == other)

	@property
	def token_types(self):
		return self.token_counts.keys()

	def total_token_count(self) -> int:
		return sum(self.token_counts.values())

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def copy(self) -> "TokenCountDatum":
		return TokenCountDatum(Counter(self.token_counts))

	def update(self, other: "TokenCountDatum"):
		self.token_counts.update(other.token_counts)


def is_relevant_round(datum: FilteredTokenCountDatum) -> bool:
	return len(datum.relevant_tokens.token_types) > 0
