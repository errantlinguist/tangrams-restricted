from collections import defaultdict
from decimal import Decimal
from typing import FrozenSet, Generic, Iterator, MutableSequence, \
	Optional, \
	Tuple, TypeVar

import alignment_metrics

R = TypeVar('R')


class Coreference(object):
	"""
	This class represents a single reference in a coreference chain.
	"""

	def __init__(self, coref_id: int, tokens: FrozenSet[str], round_id: int,
				 antecedent: Optional["Coreference"]):
		self.coref_id = coref_id
		self.tokens = tokens
		self.round_id = round_id
		self.antecedent = antecedent

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	@property
	def antecedents(self) -> Iterator["Coreference"]:
		last_antecedent = self.antecedent
		while last_antecedent is not None:
			last_antecedent = last_antecedent.antecedent
			yield last_antecedent

	@property
	def seq_number(self):
		return sum(1 for _ in self.antecedents) + 1

	@property
	def token_types(self):
		return frozenset(self.tokens)

	def token_type_overlap_with_self(self) -> Optional[Decimal]:
		"""
		Calculates the number of token types (i.e. unique words) of this coreference which overlap with the types of the coreference preceding this one within the same coreference chain.
		:return: A ratio of the number of overlapping token types.
		:rtype: Decimal
		"""
		if self.antecedent is None:
			result = None
		else:
			token_types = self.token_types
			preceding_token_types = self.antecedent.token_types
			result = alignment_metrics.token_type_overlap_ratio(token_types.union(
				preceding_token_types), token_types.intersection(
				preceding_token_types))
		return result


class DialogueCoreferenceChainDatum(Generic[R]):
	@staticmethod
	def __add_to_chain(coref_id: int, tokens: FrozenSet[str], round_id: int,
					   coref_chain: MutableSequence[Coreference]) -> Coreference:
		if coref_chain:
			antecedent = coref_chain[len(coref_chain) - 1]
			result = Coreference(coref_id, tokens, round_id, antecedent)
		else:
			result = Coreference(coref_id, tokens, round_id, None)
		coref_chain.append(result)
		return result

	def __init__(self):
		self.participant_corefs = defaultdict(lambda: defaultdict(list))
		"""A dictionary of entity coreference chains for each dialogue participant."""
		self.session_corefs = defaultdict(list)
		"""A dictionary for coreference chains for each entity, mapped by the respective entity's ID."""
		self.__last_coref_id = 0

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def add_entity_corefs(self, participant_id: str,
						  referent_id: R, round_id: int, tokens: FrozenSet[str]) -> Tuple[Coreference, Coreference]:
		participant_corefs = self.participant_corefs[participant_id][referent_id]
		participant_coref = self.__add_to_chain(self.__next_coref_id, tokens, round_id, participant_corefs)

		session_corefs = self.session_corefs[referent_id]
		session_coref = self.__add_to_chain(self.__next_coref_id, tokens, round_id, session_corefs)

		return participant_coref, session_coref

	def token_type_overlap_with_other(self,
									  participant_id: str, coref_chain_id: R) -> Tuple[
		Optional[Coreference], Optional[Tuple[Coreference, Decimal]]]:
		"""
		Calculates the number of token types (i.e. unique words) of the coreference from a given participant with that of the coreference preceding it which is not from the same participant but refers to the same referent, indicated by sharing the same coreference chain ID.

		:param participant_id: The ID of the participant to calculate overlap for.
		:param coref_chain_id: An identifier for the referent to search for coreference chains featuring it as a referent.
		:return: The ratio of overlap between the last coreference for the given participant and coreference chain ID and the preceding coreference with a different participant but the same coreference chain ID.
		"""
		own_participant_corefs = self.participant_corefs[participant_id][coref_chain_id]
		if own_participant_corefs:
			last_own_coref = own_participant_corefs[len(own_participant_corefs) - 1]
			last_own_coref_id = last_own_coref.coref_id
			last_own_coref_token_types = last_own_coref.token_types

			# Get all coreference chains for all participants with an ID not equal to the given one
			other_participant_corefs = ((other_participant_id, corefs) for (other_participant_id, corefs) in
										self.participant_corefs.items() if other_participant_id != participant_id)
			# Get all coreference chains with the same referent regardless of the participant ID
			other_corefs = (coref_chain[len(coref_chain) - 1] for (_, corefs) in other_participant_corefs for
							(other_coref_chain_id, coref_chain) in
							corefs.items() if
							other_coref_chain_id == coref_chain_id and len(coref_chain) > 0)
			other_previous_corefs = tuple(coref for coref in other_corefs if coref.coref_id < last_own_coref_id)
			if other_previous_corefs:
				preceding_coref = max(other_previous_corefs, key=lambda coref: coref.coref_id)
				preceding_token_types = preceding_coref.token_types
				overlap = alignment_metrics.token_type_overlap_ratio(last_own_coref_token_types, preceding_token_types)
				preceding_coref_overlap = preceding_coref, overlap
			else:
				preceding_coref_overlap = None
		else:
			last_own_coref = None
			preceding_coref_overlap = None
		return last_own_coref, preceding_coref_overlap

	@property
	def __next_coref_id(self) -> int:
		result = self.__last_coref_id + 1
		self.__last_coref_id = result
		return result
