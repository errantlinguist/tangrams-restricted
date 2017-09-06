#!/usr/bin/env python3

import itertools
import sys
import xml.etree.ElementTree
from decimal import Decimal, localcontext

from annotations import ANNOTATION_NAMESPACES
from game_events import create_game_rounds, read_events
from re_token_group_counts import read_token_group_dict
from session_data import walk_session_data
from utterances import SegmentUtteranceFactory, UtteranceTimes, dialogue_utt_str_repr

COL_DELIM = '\t'


def print_tabular_freqs(infile_token_group_counts, group_count_sums, decimal_printing_ctx, file):
	item_key_getter = lambda item: item[0]
	ordered_group_counts = tuple(
		(group, Decimal(count)) for group, count in sorted(group_count_sums.items(), key=item_key_getter))
	ordered_groups = tuple(group for (group, _) in ordered_group_counts)
	header_cells = itertools.chain(("DYAD",), (group for (group, _) in ordered_group_counts))
	print(COL_DELIM.join(header_cells), file=file)

	for infile, token_group_counts in sorted(infile_token_group_counts.items(), key=item_key_getter):
		counts = tuple(Decimal(token_group_counts.get(group, 0)) for group in ordered_groups)
		dyad_total_count = Decimal(sum(counts))
		freqs = (count / dyad_total_count for count in counts)
		with localcontext(decimal_printing_ctx) as _:
			print(COL_DELIM.join(itertools.chain((infile,), (str(freq) for freq in freqs))),
				  file=file)

	summary_counts = tuple(count for (_, count) in ordered_group_counts)
	summary_total_count = sum(summary_counts)
	summary_freqs = (count / summary_total_count for count in summary_counts)
	summary_row_cells = itertools.chain(("TOTAL",), (str(freq) for freq in summary_freqs))
	with localcontext(decimal_printing_ctx) as _:
		print(COL_DELIM.join(summary_row_cells))


def read_segments(infile_path):
	print("Reading XML file \"{}\".".format(infile_path), file=sys.stderr)
	doc_tree = xml.etree.ElementTree.parse(infile_path)
	return doc_tree.iterfind(".//hat:segment", ANNOTATION_NAMESPACES)


if __name__ == "__main__":
	if len(sys.argv) < 3:
		raise ValueError("Usage: {} TOKEN_GROUP_FILE INPATHS... > OUTFILE".format(sys.argv[0]))
	else:
		token_group_file = sys.argv[1]
		print("Reading token groups from \"{}\".".format(token_group_file), file=sys.stderr)
		token_groups = read_token_group_dict(token_group_file)
		print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

		seg_utt_factory = SegmentUtteranceFactory()

		inpaths = sys.argv[2:]
		for indir, session in walk_session_data(inpaths):
			print("Processing session directory \"{}\".".format(indir), file=sys.stderr)
			events = tuple(read_events(session))
			print("Read {} event(s).".format(len(events)), file=sys.stderr)

			doc_tree = xml.etree.ElementTree.parse(session.utts)
			segments = doc_tree.iterfind('.//hat:segment', ANNOTATION_NAMESPACES)
			utts = list(seg_utt_factory(segments))
			utts_by_time = UtteranceTimes(utts)

			idxed_game_rounds = iter(enumerate(create_game_rounds(events)))
			round_idx, first_game_round = next(idxed_game_rounds)
			current_round_start_time = first_game_round.start_time
			for round_idx, next_round in idxed_game_rounds:

				next_round_start_time = next_round.start_time
				current_round_utts = tuple(utts_by_time.segments_between(current_round_start_time,
																	next_round_start_time))
				diag_utt_repr = dialogue_utt_str_repr(iter(current_round_utts))
				print(COL_DELIM.join((str(round_idx), diag_utt_repr)))

				current_round_start_time = next_round_start_time
