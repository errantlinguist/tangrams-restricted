import sys
from typing import Callable, Dict, Iterable, Iterator, List, Mapping, Sequence, \
	Tuple

import game_events
import session_data as sd
import utterances


class GameRoundUtterances(object):
	"""
	A class associating game rounds with the dialogues for each.
	"""

	def __init__(self, game_round_utts: Sequence[Tuple[game_events.GameRound, Sequence[utterances.Utterance]]],
				 round_instructor_ids: Mapping[int, str]):
		self.game_round_utts = game_round_utts
		self.round_instructor_ids = round_instructor_ids

	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)


class SessionGameRoundUtteranceFactory(object):
	ROUND_ID_OFFSET = 1

	def __init__(self, token_seq_factory: Callable[[Iterable[str]], Sequence[str]]):
		self.token_seq_factory = token_seq_factory

	def __call__(self, named_sessions: Iterable[Tuple[str, sd.SessionData]]) -> Dict[
		str, GameRoundUtterances]:
		result = {}
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
			entity_coreference_chains = self.__create_game_round_utterances(session)
			result[dyad_id] = entity_coreference_chains

		return result

	def __create_game_round_utterances(self, session: sd.SessionData) -> GameRoundUtterances:
		event_data = game_events.read_events(session)
		source_participant_ids = event_data.source_participant_ids
		seg_utt_factory = utterances.SegmentUtteranceFactory(self.token_seq_factory,
															 lambda source_id: source_participant_ids[source_id])
		game_rounds = game_events.create_game_rounds(event_data.events)
		segments = utterances.read_segments(session.utts)
		utts = seg_utt_factory(segments)

		game_round_utts = zip_game_round_utterances(game_rounds, tuple(utts))[1]
		event_participant_id_factory = game_events.EventParticipantIdFactory(event_data.initial_instructor_id)

		round_instructor_ids = {}
		enumerated_game_round_utts = enumerate(game_round_utts, start=self.ROUND_ID_OFFSET)
		for round_id, round_utts in enumerated_game_round_utts:
			game_round, round_utts = round_utts
			initial_event = game_round.initial_event
			round_instructor_id = event_participant_id_factory(initial_event)
			existing_instructor_id = round_instructor_ids.get(round_id, None)
			if existing_instructor_id and existing_instructor_id != round_instructor_id:
				raise ValueError("Differing instructor ID for round {}.".format(round_id))
			else:
				round_instructor_ids[round_id] = round_instructor_id
		return GameRoundUtterances(game_round_utts, round_instructor_ids)


def zip_game_round_utterances(game_round_iter: Iterator[game_events.GameRound],
							  utts: Iterable[utterances.Utterance]) -> \
		Tuple[Tuple[utterances.Utterance, ...], List[Tuple[game_events.GameRound, Tuple[utterances.Utterance, ...]]]]:
	first_round = next(game_round_iter)
	first_round_start_time = first_round.start_time
	# Get utterances preceding the first round
	pre_game_utts = tuple(utt for utt in utts if utt.start_time < first_round_start_time)

	current_round = first_round
	game_round_utts = []
	for next_round in game_round_iter:
		next_round_start_time = next_round.start_time
		# TODO: optimize
		next_round_utts = tuple(utt for utt in utts if utt.start_time < next_round_start_time)
		game_round_utts.append((current_round, next_round_utts))
		current_round = next_round

	# Get utterances for last round
	last_round_start_time = current_round.start_time
	last_round_utts = tuple(utt for utt in utts if utt.start_time >= last_round_start_time)
	game_round_utts.append((current_round, last_round_utts))

	return pre_game_utts, game_round_utts
