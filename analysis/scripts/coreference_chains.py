from collections import defaultdict
from decimal import Decimal
from typing import Callable, FrozenSet, Generic, Iterable, Iterator, MutableSequence, \
	Optional, \
	Tuple, TypeVar

import alignment_metrics

C = TypeVar('C')


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

	@property
	def __key(self):
		return self.coref_id, self.round_id, self.tokens, self.antecedent

	def __eq__(self, other):
		return (self is other or (isinstance(other, type(self))
								  and self.__key == other.__key))

	def __ne__(self, other):
		return not (self == other)

	def __hash__(self):
		return hash(self.__key)

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

	def analogous_antecedent_corefs(self, coref_chain_corpus: Iterable["DialogueCoreferenceChainDatum"],
									coref_chain_id_filter: Callable[[C], bool]) -> Optional[Iterator["Coreference"]]:
		"""

		:param coref_chain_corpus: All DialogueCoreferenceChainDatum instances to use for calculation, each of which representing a single dyad in the entire corpus thereof.
		:param coref_chain_id_filter: A filter matching the identifier(s) for analogous coreference chains to compare against.
		:return: The coreferences in coreference chains preceding the coreference in that chain analogous to this one.
		:rtype: Optional[Iterator[Coreference]]
		"""
		seq_no = self.seq_number
		if seq_no < 2:
			result = None
		else:
			for dyad_coref_chains in coref_chain_corpus:
				for coref_chain_id, coref_chain in dyad_coref_chains.session_coref_chains.items():
					if coref_chain_id_filter(coref_chain_id) and len(coref_chain) >= seq_no:
						analogous_preceding_coref = coref_chain[seq_no - 2]
						# print("Original seq. no {}; Analogous coref seq. no {}.".format(seq_no,
						#																analogous_preceding_coref.seq_number))
						assert analogous_preceding_coref.seq_number == seq_no - 1
						yield analogous_preceding_coref

	def token_type_overlap(self, other: "Coreference") -> Optional[Decimal]:
		"""
		Calculates the number of token types (i.e. unique words) of this coreference which overlap with the types of another, given coreference.
		:param other: The coreference to compare against.
		:type other: Coreference
		:return: A ratio of the number of overlapping token types.
		:rtype: Decimal
		"""
		token_types = self.token_types
		other_token_types = other.token_types
		return alignment_metrics.token_type_overlap_ratio(token_types.union(
			other_token_types), token_types.intersection(
			other_token_types))

	def token_type_overlap_with_antecedent(self) -> Optional[Decimal]:
		"""
		Calculates the number of token types (i.e. unique words) of this coreference which overlap with the types of the coreference preceding this one within the same coreference chain.
		:return: A ratio of the number of overlapping token types.
		:rtype: Optional[Decimal]
		"""
		return None if self.antecedent is None else self.token_type_overlap(self.antecedent)


class DialogueCoreferenceChainDatum(Generic[C]):
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
		self.participant_coref_chains = defaultdict(lambda: defaultdict(list))
		"""A dictionary of entity coreference chains for each dialogue participant."""
		# self.participant_round_corefs = defaultdict(lambda: defaultdict(set))
		# """A dictionary mapping participant IDs to a respective dictionary of round IDs mapped to all coreferences by that participant which occurred in that round."""
		self.session_coref_chains = defaultdict(list)
		"""A dictionary for coreference chains for each entity, mapped by the respective entity's ID."""
		# self.session_round_corefs = defaultdict(set)
		# """A dictionary of round IDs mapped to all coreferences occurring in that round."""
		self.__last_coref_id = 0

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)

	def add_coref(self, participant_id: str,
				  referent_id: C, round_id: int, tokens: FrozenSet[str]) -> Tuple[Coreference, Coreference]:
		participant_coref_chain = self.participant_coref_chains[participant_id][referent_id]
		participant_coref = self.__add_to_chain(self.__next_coref_id, tokens, round_id, participant_coref_chain)
		# self.participant_round_corefs[participant_id][round_id].add(participant_coref)

		session_coref_chain = self.session_coref_chains[referent_id]
		session_coref = self.__add_to_chain(self.__next_coref_id, tokens, round_id, session_coref_chain)
		# self.session_round_corefs[round_id].add(session_coref)

		return participant_coref, session_coref

	def token_type_overlap_with_other(self,
									  participant_id: str, coref_chain_id: C) -> Tuple[
		Optional[Coreference], Optional[Tuple[Coreference, Decimal]]]:
		"""
		Calculates the number of token types (i.e. unique words) of the coreference from a given participant with that of the coreference preceding it which is not from the same participant but refers to the same referent, indicated by sharing the same coreference chain ID.

		:param participant_id: The ID of the participant to calculate overlap for.
		:param coref_chain_id: An identifier for the referent to search for coreference chains featuring it as a referent.
		:return: The ratio of overlap between the last coreference for the given participant and coreference chain ID and the preceding coreference with a different participant but the same coreference chain ID.
		"""
		own_participant_coref_chain = self.participant_coref_chains[participant_id][coref_chain_id]
		if own_participant_coref_chain:
			last_own_coref = own_participant_coref_chain[len(own_participant_coref_chain) - 1]
			last_own_coref_id = last_own_coref.coref_id
			last_own_coref_round_id = last_own_coref.round_id
			last_own_coref_token_types = last_own_coref.token_types

			# Get all coreference chains for all participants with an ID not equal to the given one
			other_participant_coref_chains = ((other_participant_id, coref_chains) for
											  (other_participant_id, coref_chains) in
											  self.participant_coref_chains.items() if
											  other_participant_id != participant_id)
			# Get all coreference chains with the same referent regardless of the participant ID
			other_corefs = (coref_chain[len(coref_chain) - 1] for (_, corefs) in other_participant_coref_chains for
							(other_coref_chain_id, coref_chain) in
							corefs.items() if
							other_coref_chain_id == coref_chain_id and len(coref_chain) > 0)
			other_previous_corefs = tuple(coref for coref in other_corefs if
										  coref.coref_id < last_own_coref_id and coref.round_id <= last_own_coref_round_id)
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
