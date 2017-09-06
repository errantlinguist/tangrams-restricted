#!/usr/bin/env python3

import argparse
import csv
import itertools
import sys
from collections import Counter
from decimal import Decimal, ROUND_HALF_UP, Context, localcontext
from typing import Counter, Dict, Iterable, List, Sequence, Iterator, TextIO, Tuple

import game_events
import utterances
from re_token_group_counts import read_token_group_dict
from session_data import SessionData, walk_session_data
from xml_files import walk_xml_files

COL_DELIM = '\t'
EVENT_TIME_COL_NAME = "TIME"
ROUND_ID_COL_NAME = "ROUND"

__TOKEN_GROUP_DICT_TYPE = Dict[str, Iterable[str]]


def count_split_session_token_groups(session: SessionData, token_groups: __TOKEN_GROUP_DICT_TYPE,
									 session_round_split_count: int,
									 seg_utt_factory: utterances.SegmentUtteranceFactory) -> Tuple[
	Counter[str], Counter[str]]:
	round_start_end_times = tuple(game_round_start_end_times(iter(read_round_start_times(session))))
	round_count = len(round_start_end_times)
	print("Read {} game round(s).".format(round_count), file=sys.stderr)

	segments = utterances.read_segments(session.utts)
	utts = tuple(seg_utt_factory(segments))

	if round_count <= session_round_split_count:
		raise ValueError(
			"Cannot split at {} rounds because the session has only {} round(s).".format(
				session_round_split_count, round_count))
	else:
		first_round_start_end_times = round_start_end_times[:session_round_split_count]
		print("First half of session has {} round(s).".format(len(first_round_start_end_times)),
			  file=sys.stderr)
		first_half_counts = __count_token_groups(first_round_start_end_times, token_groups, utts)

		next_round_start_end_times = round_start_end_times[session_round_split_count:]
		print("Second half of session has {} round(s).".format(len(next_round_start_end_times)),
			  file=sys.stderr)
		next_half_counts = __count_token_groups(next_round_start_end_times, token_groups, utts)

		return first_half_counts, next_half_counts


def create_utt_token_group_counts(utts: Iterable[utterances.Utterance], token_groups: __TOKEN_GROUP_DICT_TYPE) -> \
		Counter[str]:
	result = Counter()

	tokens = semantically_relevant_tokens(utts)
	group_sets = (token_groups.get(token) for token in tokens)
	for group_set in group_sets:
		if group_set:
			result.update(group_set)

	return result


def game_round_start_end_times(round_start_times: Iterator[float]) -> Iterator[Tuple[float, float]]:
	end_delimited_round_start_times = itertools.chain(round_start_times, (float('inf'),))
	current_start_time = next(end_delimited_round_start_times)
	for next_start_time in end_delimited_round_start_times:
		yield current_start_time, next_start_time
		current_start_time = next_start_time

	end_reached = False
	while not end_reached:
		try:
			next_start_time = next(round_start_times)
		except StopIteration:
			next_start_time = float('inf')
			end_reached = True

		yield current_start_time, next_start_time
		current_start_time = next_start_time


def group_freqs(group_counts: Dict[str, int]) -> Iterator[Tuple[str, Decimal]]:
	decimal_counts = tuple((group, Decimal(count)) for group, count in group_counts.items())
	total = sum(count for (_, count) in decimal_counts)
	return ((group, count / total) for group, count in decimal_counts)


def game_round_utterances(start_time: float, end_time: float, utts: Iterable[utterances.Utterance]):
	return (utt for utt in utts if (utt.start_time >= start_time) and (utt.start_time < end_time))


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


def read_round_start_times(session: SessionData) -> List[float]:
	events_metadata = game_events.read_events_metadata(session.events_metadata)
	round_count = int(events_metadata["ROUND_COUNT"])

	result = [float('inf')] * round_count
	with open(session.events, 'r') as infile:
		rows = csv.reader(infile, dialect="excel-tab")
		col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
		for row in rows:
			event_time_col_idx = col_idxs[EVENT_TIME_COL_NAME]
			event_time = float(row[event_time_col_idx])
			round_id_col_idx = col_idxs[ROUND_ID_COL_NAME]
			round_idx = int(row[round_id_col_idx]) - 1
			result[round_idx] = min(result[round_idx], event_time)

	return result


def read_utt_token_group_counts(infile: str, token_groups: __TOKEN_GROUP_DICT_TYPE,
								seg_utt_factory: utterances.SegmentUtteranceFactory):
	segments = utterances.read_segments(infile)
	utts = seg_utt_factory(segments)
	return create_utt_token_group_counts(utts, token_groups)


def semantically_relevant_tokens(utts: Iterable[utterances.Utterance]) -> Iterator[str]:
	# https://stackoverflow.com/a/18551476/1391325
	all_tokens = (token for utt in utts for token in utt.content)
	non_fillers = (token for token in all_tokens if token not in utterances.FILLER_TOKENS)
	return (token for token in non_fillers if not utterances.is_disfluency(token))


def __count_token_groups(start_end_times: Iterable[Tuple[int, int]],
						 token_groups: __TOKEN_GROUP_DICT_TYPE,
						 utts: Sequence[utterances.Utterance]) -> Counter[str]:
	result = Counter()
	for start_time, end_time in start_end_times:
		round_utts = game_round_utterances(start_time, end_time, utts)
		for utt in round_utts:
			group_sets = (token_groups.get(token) for token in utt.content)
			for group_set in group_sets:
				if group_set:
					result.update(group_set)
	return result


def __create_argparser():
	result = argparse.ArgumentParser(description="Count frequencies of referring token groups.")
	result.add_argument("token_group_file", metavar="path",
						help="The path to the token group mapping file to use.")
	result.add_argument("inpaths", metavar="path", nargs='+',
						help="The paths to search for sessions to process.")
	result.add_argument("-r", "--round-split", metavar="count", type=int,
						help="When this option is supplied, each session is split into half, with the first half comprising this many game rounds.")
	return result


def __process_all_tokens(inpaths: Iterable[str], token_groups: __TOKEN_GROUP_DICT_TYPE, outfile: TextIO):
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


class Distribution(object):
	def __init__(self, counts):
		self.counts = counts
		self.freqs = group_freqs(counts)


class GroupDistributions(object):
	def __init__(self, first_counts, next_counts):
		self.first = Distribution(first_counts)
		self.next = Distribution(next_counts)

		total_counts = Counter(first_counts)
		total_counts.update(next_counts)
		self.total = Distribution(total_counts)


def __process_split_sessions(inpaths: Iterable[str], token_groups: __TOKEN_GROUP_DICT_TYPE,
							 session_round_split_count: int, outfile: TextIO):
	seg_utt_factory = utterances.SegmentUtteranceFactory()

	for indir, session in walk_session_data(inpaths):
		print("Processing session directory \"{}\".".format(indir), file=sys.stderr)
		first_half_counts, next_half_counts = count_split_session_token_groups(session, token_groups,
																			   session_round_split_count,
																			   seg_utt_factory)
		first_half_freqs = group_freqs(first_half_counts)
		next_half_freqs = group_freqs(next_half_counts)

		for group, count in first_half_freqs:
			print(COL_DELIM.join((group, str(count),)))
		for group, count in next_half_freqs:
			print(COL_DELIM.join((group, str(count),)))


if __name__ == "__main__":
	args = __create_argparser().parse_args()
	token_group_file_path = args.token_group_file
	print("Reading token groups from \"{}\".".format(token_group_file_path), file=sys.stderr)
	token_groups = read_token_group_dict(token_group_file_path)
	print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

	inpaths = args.inpaths
	outfile = sys.stdout
	session_round_split_count = args.round_split
	if session_round_split_count:
		print("Splitting sessions after {} round(s).".format(session_round_split_count), file=sys.stderr)
		__process_split_sessions(inpaths, token_groups, session_round_split_count, outfile)
	else:
		__process_all_tokens(inpaths, token_groups, outfile)
