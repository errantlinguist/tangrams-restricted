#!/usr/bin/env python3

import argparse
import itertools
import sys
from collections import Counter
from decimal import Decimal, ROUND_HALF_UP, Context, localcontext
from typing import Counter, Dict, Iterable, Iterator, TextIO, Tuple

import game_events
import utterances
from re_token_group_counts import read_token_group_dict
from session_data import walk_session_data
from xml_files import walk_xml_files

COL_DELIM = '\t'


def create_utt_token_group_counts(utts: Iterable[utterances.Utterance], token_groups: Dict[str, Iterable[str]]) -> \
		Counter[str]:
	result = Counter()

	tokens = semantically_relevant_tokens(utts)
	group_sets = (token_groups.get(token) for token in tokens)
	for group_set in group_sets:
		if group_set:
			result.update(group_set)

	return result


def print_tabular_freqs(infile_token_group_counts: Dict[str, Dict[str, int]], group_count_sums: Dict[str, int],
						decimal_printing_ctx: Context, outfile: TextIO):
	item_key_getter = lambda item: item[0]
	ordered_group_counts = tuple(
		(group, Decimal(count)) for group, count in sorted(group_count_sums.items(), key=item_key_getter))
	ordered_groups = tuple(group for (group, _) in ordered_group_counts)
	header_cells = itertools.chain(("DYAD",), (group for (group, _) in ordered_group_counts))
	print(COL_DELIM.join(header_cells), file=outfile)

	for infile, token_group_counts in sorted(infile_token_group_counts.items(), key=item_key_getter):
		counts = tuple(Decimal(token_group_counts.get(group, 0)) for group in ordered_groups)
		dyad_total_count = Decimal(sum(counts))
		freqs = (count / dyad_total_count for count in counts)
		with localcontext(decimal_printing_ctx) as _:
			print(COL_DELIM.join(itertools.chain((infile,), (str(freq) for freq in freqs))),
				  file=outfile)

	summary_counts = tuple(count for (_, count) in ordered_group_counts)
	summary_total_count = sum(summary_counts)
	summary_freqs = (count / summary_total_count for count in summary_counts)
	summary_row_cells = itertools.chain(("TOTAL",), (str(freq) for freq in summary_freqs))
	with localcontext(decimal_printing_ctx) as _:
		print(COL_DELIM.join(summary_row_cells))


def read_utt_token_group_counts(infile: str, token_groups: Dict[str, Iterable[str]],
								seg_utt_factory: utterances.SegmentUtteranceFactory):
	segments = utterances.read_segments(infile)
	utts = seg_utt_factory(segments)
	return create_utt_token_group_counts(utts, token_groups)


def semantically_relevant_tokens(utts: Iterable[utterances.Utterance]) -> Iterator[str]:
	# https://stackoverflow.com/a/18551476/1391325
	all_tokens = (token for utt in utts for token in utt.content)
	non_fillers = (token for token in all_tokens if token not in utterances.FILLER_TOKENS)
	return (token for token in non_fillers if not utterances.is_disfluency(token))


def __count_token_group_freqs(idxed_game_rounds: Iterator[Tuple[int, game_events.GameRound]],
							  utts_by_time: utterances.UtteranceTimes):
	round_idx, first_game_round = next(idxed_game_rounds)
	current_round_start_time = first_game_round.start_time
	for round_idx, next_round in idxed_game_rounds:
		next_round_start_time = next_round.start_time
		current_round_utts = utts_by_time.between(current_round_start_time,
												  next_round_start_time)

		diag_utt_repr = utterances.dialogue_utt_str_repr(current_round_utts)
		print(COL_DELIM.join((str(round_idx), diag_utt_repr)), file=outfile)

		current_round_start_time = next_round_start_time


def __create_argparser():
	result = argparse.ArgumentParser(description="Count frequencies of referring token groups.")
	result.add_argument("token_group_file", metavar="path",
						help="The path to the token group mapping file to use.")
	result.add_argument("inpaths", metavar="path", nargs='+',
						help="The paths to search for sessions to process.")
	result.add_argument("-r", "--round-split", metavar="count", type=int,
						help="When this option is supplied, each session is split into half, with the first half comprising this many game rounds.")
	return result


def __process_all_tokens(inpaths: Iterable[str], outfile: TextIO):
	infiles = walk_xml_files(*inpaths)
	seg_utt_factory = utterances.SegmentUtteranceFactory()
	infile_token_group_counts = dict(
		(infile, read_utt_token_group_counts(infile, token_groups, seg_utt_factory)) for infile in infiles)
	print("Read token counts for {} file(s).".format(len(infile_token_group_counts)), file=sys.stderr)

	group_count_sums = Counter()
	for group_counts in infile_token_group_counts.values():
		for group, count in group_counts.items():
			group_count_sums[group] += count

	printing_ctx = Context(prec=3, rounding=ROUND_HALF_UP)
	print_tabular_freqs(infile_token_group_counts, group_count_sums, printing_ctx, outfile)


def __process_split_sessions(inpaths: Iterable[str], session_round_split_count: int, outfile: TextIO):
	seg_utt_factory = utterances.SegmentUtteranceFactory()
	for indir, session in walk_session_data(inpaths):
		print("Processing session directory \"{}\".".format(indir), file=sys.stderr)
		events = tuple(game_events.read_events(session))
		print("Read {} event(s).".format(len(events)), file=sys.stderr)

		segments = utterances.read_segments(session.utts)
		utts = seg_utt_factory(segments)
		utts_by_time = utterances.UtteranceTimes(utts)

		idxed_game_rounds = tuple(enumerate(game_events.create_game_rounds(events)))
		if len(idxed_game_rounds) <= session_round_split_count:
			raise ValueError(
				"Cannot split at {} rounds because the session read from \"{}\" has only {} round(s).".format(
					session_round_split_count, indir, len(idxed_game_rounds)))
		else:
			first_idxed_game_rounds = idxed_game_rounds[:session_round_split_count]
			print("First half of session \"{}\" has {} round(s).".format(indir, len(first_idxed_game_rounds)),
				  file=sys.stderr)
			__count_token_group_freqs(iter(first_idxed_game_rounds), utts_by_time)
			next_idxed_game_rounds = idxed_game_rounds[session_round_split_count:]
			print("Second half of session \"{}\" has {} round(s).".format(indir, len(next_idxed_game_rounds)),
				  file=sys.stderr)
			__count_token_group_freqs(iter(next_idxed_game_rounds), utts_by_time)


if __name__ == "__main__":
	args = __create_argparser().parse_args()
	print(args)
	token_group_file_path = args.token_group_file
	print("Reading token groups from \"{}\".".format(token_group_file_path), file=sys.stderr)
	token_groups = read_token_group_dict(token_group_file_path)
	print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

	inpaths = args.inpaths
	outfile = sys.stdout
	session_round_split_count = args.round_split
	if session_round_split_count:
		print("Splitting sessions after {} round(s).".format(session_round_split_count), file=sys.stderr)
		__process_split_sessions(inpaths, session_round_split_count, outfile)
	else:
		__process_all_tokens(inpaths, outfile)
