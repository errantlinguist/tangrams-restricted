#!/usr/bin/env python3

import argparse
import csv
import itertools
import sys
import statistics
from collections import Counter
from decimal import Decimal, ROUND_HALF_UP
from enum import Enum, unique
from typing import Dict, Iterable, List, Sequence, Iterator, Tuple

import game_events
import utterances
from re_token_group_counts import read_token_group_dict, read_utt_token_group_counts
from session_data import SessionData, walk_session_data
from xml_files import walk_xml_files

COL_DELIM = '\t'
DYAD_ID_COL_NAME = "DYAD"
TOTAL_RESULTS_ROW_NAME = "TOTAL"

_ZERO_DECIMAL = Decimal('0.000')

__DECIMAL_REPR_ROUNDING_EXP = Decimal('1.000')
__TOKEN_GROUP_DICT_TYPE = Dict[str, Iterable[str]]


class Distribution(object):
	def __init__(self, counts):
		self.counts = counts
		self.freqs = dict(group_freqs(counts))

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def keys(self):
		return self.counts.keys()


@unique
class EventDataColumn(Enum):
	EVENT_TIME = "TIME"
	ROUND_ID = "ROUND"


class GroupDistributions(object):
	def __init__(self, first_counts, next_counts):
		self.first = Distribution(first_counts)
		self.next = Distribution(next_counts)

		total_counts = Counter(first_counts)
		total_counts.update(next_counts)
		self.total = Distribution(total_counts)

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def group_freq_differences(self):
		for group in self.total.freqs.keys():
			first_freq = self.first.freqs.get(group, _ZERO_DECIMAL)
			next_freq = self.next.freqs.get(group, _ZERO_DECIMAL)
			yield group, next_freq - first_freq


def count_partitioned_session_token_groups(session: SessionData, token_groups: __TOKEN_GROUP_DICT_TYPE,
										   first_partition_round_count: int,
										   seg_utt_factory: utterances.SegmentUtteranceFactory) -> Tuple[
	Dict[str, int], Dict[str, int]]:
	round_start_end_times = tuple(game_round_start_end_times(iter(read_round_start_times(session))))
	round_count = len(round_start_end_times)
	print("Read {} game round(s).".format(round_count), file=sys.stderr)

	segments = utterances.read_segments(session.utts)
	utts = tuple(seg_utt_factory(segments))

	if round_count <= first_partition_round_count:
		raise ValueError(
			"Cannot split at {} rounds because the session has only {} round(s).".format(
				first_partition_round_count, round_count))
	else:
		first_round_start_end_times = round_start_end_times[:first_partition_round_count]
		print("First half of session has {} round(s).".format(len(first_round_start_end_times)),
			  file=sys.stderr)
		first_half_counts = count_token_groups(first_round_start_end_times, token_groups, utts)

		next_round_start_end_times = round_start_end_times[first_partition_round_count:]
		print("Second half of session has {} round(s).".format(len(next_round_start_end_times)),
			  file=sys.stderr)
		next_half_counts = count_token_groups(next_round_start_end_times, token_groups, utts)

		return first_half_counts, next_half_counts


def count_token_groups(start_end_times: Iterable[Tuple[int, int]],
					   token_groups: __TOKEN_GROUP_DICT_TYPE,
					   utts: Sequence[utterances.Utterance]) -> Dict[str, int]:
	result = Counter()
	for start_time, end_time in start_end_times:
		round_utts = game_round_utterances(start_time, end_time, utts)
		for utt in round_utts:
			group_sets = (token_groups.get(token) for token in utt.content)
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


def print_partitioned_session_group_dists(session_group_dists: Dict[str, GroupDistributions], total_group_dists,
										  first_partition_round_count,
										  outfile):
	referring_groups = tuple(
		sorted(frozenset(group for dists in session_group_dists.values() for group in dists.total.keys())))
	firstn_col_name = "FIRST_{}".format(first_partition_round_count)
	header_cells = [DYAD_ID_COL_NAME]
	subheader_cells = [""]
	for referring_group in referring_groups:
		header_cells.append(referring_group)
		subheader_cells.append(firstn_col_name)
		header_cells.append("")
		subheader_cells.append("REST")
		header_cells.append("")
		subheader_cells.append("TOTAL")
	print(COL_DELIM.join(header_cells), file=outfile)
	print(COL_DELIM.join(subheader_cells), file=outfile)

	for session, group_dists in sorted(session_group_dists.items(), key=__get_item_key):
		row = [session]
		__append_group_freqs(row, group_dists, referring_groups)
		print(COL_DELIM.join(row), file=outfile)

	summary_row = [TOTAL_RESULTS_ROW_NAME]
	__append_group_freqs(summary_row, total_group_dists, referring_groups)
	print(COL_DELIM.join(summary_row), file=outfile)


def print_whole_session_group_freqs(infile_token_group_counts: Dict[str, Dict[str, int]],
									group_count_sums: Dict[str, int],
									outfile):
	ordered_group_counts = tuple(
		(group, Decimal(count)) for group, count in sorted(group_count_sums.items(), key=__get_item_key))
	ordered_groups = tuple(group for (group, _) in ordered_group_counts)
	header_cells = itertools.chain((DYAD_ID_COL_NAME,), (group for (group, _) in ordered_group_counts))
	print(COL_DELIM.join(header_cells), file=outfile)

	for infile, token_group_counts in sorted(infile_token_group_counts.items(), key=__get_item_key):
		counts = tuple(Decimal(token_group_counts.get(group, 0)) for group in ordered_groups)
		dyad_total_count = Decimal(sum(counts))
		freqs = (count / dyad_total_count for count in counts)
		print(COL_DELIM.join(itertools.chain((infile,), (_create_rounded_decimal_repr(freq) for freq in freqs))),
			  file=outfile)

	summary_counts = tuple(count for (_, count) in ordered_group_counts)
	summary_total_count = sum(summary_counts)
	summary_freqs = (count / summary_total_count for count in summary_counts)
	summary_row_cells = itertools.chain((TOTAL_RESULTS_ROW_NAME,),
										(_create_rounded_decimal_repr(freq) for freq in summary_freqs))
	print(COL_DELIM.join(summary_row_cells))


def read_metadata_round_count(session: SessionData) -> int:
	events_metadata = game_events.read_events_metadata(session.events_metadata)
	return int(events_metadata["ROUND_COUNT"])


def read_round_start_times(session: SessionData) -> List[float]:
	round_count = read_metadata_round_count(session)

	result = [float('inf')] * round_count
	with open(session.events, 'r') as infile:
		rows = csv.reader(infile, dialect="excel-tab")
		col_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
		for row in rows:
			event_time_col_idx = col_idxs[EventDataColumn.EVENT_TIME.value]
			event_time = float(row[event_time_col_idx])
			round_id_col_idx = col_idxs[EventDataColumn.ROUND_ID.value]
			round_idx = int(row[round_id_col_idx]) - 1
			result[round_idx] = min(result[round_idx], event_time)

	return result


def _create_rounded_decimal_repr(value: Decimal):
	return str(value.quantize(__DECIMAL_REPR_ROUNDING_EXP, ROUND_HALF_UP))


def __append_group_freqs(row, group_dists: GroupDistributions, referring_groups: Iterable[str]):
	for referring_group in referring_groups:
		row.append(_create_rounded_decimal_repr(group_dists.first.freqs.get(referring_group, _ZERO_DECIMAL)))
		row.append(_create_rounded_decimal_repr(group_dists.next.freqs.get(referring_group, _ZERO_DECIMAL)))
		row.append(_create_rounded_decimal_repr(group_dists.total.freqs.get(referring_group, _ZERO_DECIMAL)))


def __count_session_group_dists(named_sessions: Iterable[Tuple[str, SessionData]],
								token_groups: __TOKEN_GROUP_DICT_TYPE, first_partition_round_count,
								seg_utt_factory: utterances.SegmentUtteranceFactory):
	session_group_dists = {}
	total_first_half_group_counts = Counter()
	total_next_half_group_counts = Counter()
	for dyad_id, session in named_sessions:
		print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
		first_half_counts, next_half_counts = count_partitioned_session_token_groups(session, token_groups,
																					 first_partition_round_count,
																					 seg_utt_factory)
		total_first_half_group_counts.update(first_half_counts)
		total_next_half_group_counts.update(next_half_counts)
		group_dist = GroupDistributions(first_half_counts, next_half_counts)
		session_group_dists[dyad_id] = group_dist

	return session_group_dists, GroupDistributions(total_first_half_group_counts, total_next_half_group_counts)


def __create_argparser():
	result = argparse.ArgumentParser(description="Count frequencies of referring token groups.")
	result.add_argument("token_group_file", metavar="TOKEN_GROUP_FILEPATH",
						help="The path to the token group mapping file to use.")
	result.add_argument("inpaths", metavar="INPATH", nargs='+',
						help="The paths to search for sessions to process.")
	group = result.add_mutually_exclusive_group(required=False)
	group.add_argument("-r", "--round-split", metavar="count", type=int,
					   help="When this option is supplied, each session is split into half, with the first partition comprising this many game rounds.")
	group.add_argument("-o", "--optimize-partition-diff", action="store_true",
					   help="When this option is supplied, the sessions are partitioned at the point which maximizes the mean difference between the partitions over all sessions.")
	return result


def __get_item_key(item):
	return item[0]


def __main(args):
	token_group_file_path = args.token_group_file
	print("Reading token groups from \"{}\".".format(token_group_file_path), file=sys.stderr)
	token_groups = read_token_group_dict(token_group_file_path)
	print("Read group info for {} token type(s).".format(len(token_groups)), file=sys.stderr)

	inpaths = args.inpaths
	outfile = sys.stdout
	first_partition_round_count = args.round_split
	if first_partition_round_count:
		print("Splitting sessions after {} round(s).".format(first_partition_round_count), file=sys.stderr)
		__process_partitioned_sessions(inpaths, token_groups, first_partition_round_count,
									   outfile)
	elif args.optimize_partition_diff:
		__process_optimally_partitioned_sessions(inpaths, token_groups, outfile)
	else:
		__process_whole_sessions(inpaths, token_groups, outfile)


def __process_optimally_partitioned_sessions(inpaths: Iterable[str], token_groups: __TOKEN_GROUP_DICT_TYPE,
											 outfile):
	seg_utt_factory = utterances.SegmentUtteranceFactory()

	named_sessions = tuple(walk_session_data(inpaths))
	partition_size_limit = min(read_metadata_round_count(session) for dyad_id, session in named_sessions)
	print("The maximum possible partition size is {}.".format(partition_size_limit), file=sys.stderr)

	optimal_freq_diff = Decimal('-Infinity')
	optimal_session_group_dists = None
	optimal_total_group_dists = None
	optimal_partition_round_count = None
	for first_partition_round_count in range(1, partition_size_limit):
		# print("Testing partition size {}.".format(first_partition_round_count), file=sys.stderr)
		session_group_dists, total_group_dists = __count_session_group_dists(named_sessions, token_groups,
																			 first_partition_round_count,
																			 seg_utt_factory)
		mean_freq_diff = statistics.mean(freq for (group, freq) in total_group_dists.group_freq_differences())
		if optimal_freq_diff < mean_freq_diff:
			optimal_freq_diff = mean_freq_diff
			optimal_session_group_dists = session_group_dists
			optimal_total_group_dists = total_group_dists
			optimal_partition_round_count = first_partition_round_count

	print("Printing maximally-different partitioning, at {} rounds with a difference of {}.".format(
		optimal_partition_round_count, optimal_freq_diff), file=sys.stderr)
	print_partitioned_session_group_dists(optimal_session_group_dists, optimal_total_group_dists,
										  optimal_partition_round_count, outfile)


def __process_whole_sessions(inpaths: Iterable[str], token_groups: __TOKEN_GROUP_DICT_TYPE,
							 outfile):
	infiles = walk_xml_files(*inpaths)
	seg_utt_factory = utterances.SegmentUtteranceFactory()
	infile_token_group_counts = dict(
		(infile, read_utt_token_group_counts(infile, token_groups, seg_utt_factory)) for infile in infiles)
	print("Read token counts for {} file(s).".format(len(infile_token_group_counts)), file=sys.stderr)

	group_count_sums = Counter()
	for group_counts in infile_token_group_counts.values():
		for group, count in group_counts.items():
			group_count_sums[group] += count

	print_whole_session_group_freqs(infile_token_group_counts, group_count_sums, outfile)


def __process_partitioned_sessions(inpaths: Iterable[str], token_groups: __TOKEN_GROUP_DICT_TYPE,
								   first_partition_round_count: int, outfile):
	seg_utt_factory = utterances.SegmentUtteranceFactory()

	named_sessions = walk_session_data(inpaths)
	session_group_dists, total_group_dists = __count_session_group_dists(named_sessions, token_groups,
																		 first_partition_round_count, seg_utt_factory)
	print_partitioned_session_group_dists(session_group_dists, total_group_dists, first_partition_round_count, outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
