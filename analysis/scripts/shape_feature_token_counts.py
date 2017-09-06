#!/usr/bin/env python3

import itertools
import sys
from collections import Counter, defaultdict
from typing import Any, Dict, TextIO

from game_events import EntityData, create_game_rounds, read_events
from ngrams import CachingNgramFactory
from session_data import walk_session_data
from utterances import SegmentUtteranceFactory, UtteranceTimes, read_segments

COL_DELIM = "\t"


def print_tabular_data(feature_value_ngram_counts: Dict[Any, Dict[str, int]], file: TextIO):
	header_cells = []
	subheader_cells = []
	for feature_value in sorted(feature_value_ngram_counts.keys()):
		header_cells.append(feature_value)
		subheader_cells.append("NGRAM")
		header_cells.append("")
		subheader_cells.append("COUNT")
	print(COL_DELIM.join(header_cells), file=file)
	print(COL_DELIM.join(subheader_cells), file=file)

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
		print(COL_DELIM.join(row))


if __name__ == "__main__":
	if len(sys.argv) < 2:
		sys.exit("Usage: {} INPATHS...".format(sys.argv[0]))
	else:

		feature_value_ngram_counts = defaultdict(Counter)
		seg_utt_factory = SegmentUtteranceFactory()
		ngram_factory = CachingNgramFactory(2)

		inpaths = sys.argv[1:]
		for indir, session in walk_session_data(inpaths):
			print("Processing session directory \"{}\".".format(indir), file=sys.stderr)
			events = tuple(read_events(session))
			print("Read {} event(s).".format(len(events)), file=sys.stderr)

			segments = read_segments(session.utts)
			utts = seg_utt_factory(segments)
			utts_by_time = UtteranceTimes(utts)

			idxed_game_rounds = iter(enumerate(create_game_rounds(events)))
			round_idx, first_game_round = next(idxed_game_rounds)
			current_round_start_time = first_game_round.start_time
			for round_idx, next_round in idxed_game_rounds:
				initial_event = next(iter(next_round.events))
				referent_entity = next(iter(initial_event.referent_entities))
				shape = referent_entity.attr(EntityData.Attribute.SHAPE.value)
				shape_ngram_counts = feature_value_ngram_counts[shape]

				next_round_start_time = next_round.start_time
				current_round_utts = (utts_by_time.segments_between(current_round_start_time,
																	next_round_start_time))
				current_round_ngrams = itertools.chain.from_iterable(
					(ngram_factory(utt.content) for utt in current_round_utts))
				shape_ngram_counts.update(current_round_ngrams)

		print_tabular_data(feature_value_ngram_counts, sys.stdout)
