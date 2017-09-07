#!/usr/bin/env python3

import argparse
import csv
import itertools
import statistics
import sys
from collections import Counter
from decimal import Decimal, ROUND_HALF_UP
from enum import Enum, unique
from typing import Dict, Iterable, List, Sequence, Iterator, Tuple

import game_events
import utterances
from re_token_group_counts import TokenGroupDict, read_token_group_dict
from session_data import SessionData, walk_session_data

COL_DELIM = '\t'
DYAD_ID_COL_NAME = "DYAD"
TOTAL_RESULTS_ROW_NAME = "TOTAL"

_ZERO_DECIMAL = Decimal('0.000')

__DECIMAL_REPR_ROUNDING_EXP = Decimal('1.000')


class Distribution(object):
	def __init__(self, counts):
		self.counts = counts
		self.__freqs = dict(group_freqs(counts))

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def freq(self, group):
		return self.__freqs.get(group, _ZERO_DECIMAL)

	@property
	def freqs(self):
		return self.__freqs

	def keys(self):
		return self.counts.keys()


@unique
class EventDataColumn(Enum):
	EVENT_TIME = "TIME"
	ROUND_ID = "ROUND"


class OptimalSessionPartitioner(object):
	def __init__(self, session_dist_collector):
		self.session_dist_collector = session_dist_collector

	def __call__(self, named_sessions: Sequence[Tuple[str, SessionData]]):
		partition_size_limit = min(read_metadata_round_count(session) for dyad_id, session in named_sessions)
		print("The partition size limit is {}.".format(partition_size_limit), file=sys.stderr)

		optimal_freq_diff = Decimal('-Infinity')
		optimal_session_group_dists = None
		optimal_total_group_dists = None
		optimal_partition_round_count = None
		for partition_round_count in range(1, partition_size_limit):
			# print("Testing partition size {}.".format(partition_round_count), file=sys.stderr)
			session_group_dists, total_group_dists = self.session_dist_collector(named_sessions,
																				 partition_round_count)
			mean_freq_diff = statistics.mean(freq for (group, freq) in total_group_dists.group_freq_differences())
			if optimal_freq_diff < mean_freq_diff:
				optimal_freq_diff = mean_freq_diff
				optimal_session_group_dists = session_group_dists
				optimal_total_group_dists = total_group_dists
				optimal_partition_round_count = partition_round_count

		print("Found maximally-different partitioning at {} rounds with a difference of {}.".format(
			optimal_partition_round_count, optimal_freq_diff), file=sys.stderr)
		return optimal_session_group_dists, optimal_total_group_dists, optimal_partition_round_count


class PartitionDistributions(object):
	def __init__(self, first_counts, next_counts):
		self.first = Distribution(first_counts)
		self.next = Distribution(next_counts)

		total_counts = Counter(first_counts)
		total_counts.update(next_counts)
		self.total = Distribution(total_counts)

	def __repr__(self, *args, **kwargs):
		return self.__class__.__name__ + str(self.__dict__)

	def group_freq_differences(self):
		for group in self.total.keys():
			first_freq = self.first.freq(group)
			next_freq = self.next.freq(group)
			yield group, next_freq - first_freq


class PartitionedSessionGroupDistributionCollector(object):
	def __init__(self, token_groups: TokenGroupDict, seg_utt_factory: utterances.SegmentUtteranceFactory):
		self.token_groups = token_groups
		self.seg_utt_factory = seg_utt_factory

	def __call__(self, named_sessions: Iterable[Tuple[str, SessionData]], partition_round_count: int):
		session_group_dists = {}
		total_first_half_group_counts = Counter()
		total_next_half_group_counts = Counter()
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
			first_half_counts, next_half_counts = self.__count_partitioned_session_token_groups(session,
																								partition_round_count)
			total_first_half_group_counts.update(first_half_counts)
			total_next_half_group_counts.update(next_half_counts)
			group_dist = PartitionDistributions(first_half_counts, next_half_counts)
			session_group_dists[dyad_id] = group_dist

		return session_group_dists, PartitionDistributions(total_first_half_group_counts, total_next_half_group_counts)

	def __count_partitioned_session_token_groups(self, session: SessionData,
												 partition_round_count: int) -> Tuple[
		Dict[str, int], Dict[str, int]]:
		round_start_end_times = tuple(game_round_start_end_times(iter(read_round_start_times(session))))
		round_count = len(round_start_end_times)
		print("Read {} game round(s).".format(round_count), file=sys.stderr)
		if round_count <= partition_round_count:
			raise ValueError(
				"Cannot split at {} rounds because the session has only {} round(s).".format(
					partition_round_count, round_count))
		else:
			segments = utterances.read_segments(session.utts)
			token_group_counter = RoundTokenGroupCounter(self.token_groups, tuple(self.seg_utt_factory(segments)))

			first_round_start_end_times = round_start_end_times[:partition_round_count]
			print("First half of session has {} round(s).".format(len(first_round_start_end_times)),
				  file=sys.stderr)
			first_half_counts = token_group_counter(*first_round_start_end_times)

			next_round_start_end_times = round_start_end_times[partition_round_count:]
			print("Second half of session has {} round(s).".format(len(next_round_start_end_times)),
				  file=sys.stderr)
			next_half_counts = token_group_counter(*next_round_start_end_times)

			return first_half_counts, next_half_counts


class RoundTokenGroupCounter(object):
	def __init__(self, token_groups: TokenGroupDict,
				 utts: Sequence[utterances.Utterance]):
		self.token_groups = token_groups
		self.utts = utts

	def __call__(self, *start_end_times: Tuple[float, float]) -> Dict[str, int]:
		result = Counter()
		for start_end_time in start_end_times:
			round_utts = game_round_utterances(start_end_time[0], start_end_time[1], self.utts)
			for utt in round_utts:
				group_sets = (self.token_groups.get(token) for token in utt.content)
				for group_set in group_sets:
					if group_set:
						result.update(group_set)
		return result


class WholeSessionGroupDistributionCollector(object):
	def __init__(self, token_groups: TokenGroupDict, seg_utt_factory: utterances.SegmentUtteranceFactory):
		self.token_groups = token_groups
		self.seg_utt_factory = seg_utt_factory

	def __call__(self, named_sessions: Iterable[Tuple[str, SessionData]]):
		session_group_dists = {}
		total_group_counts = Counter()
		for dyad_id, session in named_sessions:
			print("Processing session \"{}\".".format(dyad_id), file=sys.stderr)
			group_counts = self.__count_session_token_groups(session)
			total_group_counts.update(group_counts)
			session_group_dists[dyad_id] = Distribution(group_counts)

		return session_group_dists, Distribution(total_group_counts)

	def __count_session_token_groups(self, session: SessionData) -> Dict[str, int]:
		round_start_end_times = tuple(game_round_start_end_times(iter(read_round_start_times(session))))
		round_count = len(round_start_end_times)
		print("Read {} game round(s).".format(round_count), file=sys.stderr)

		segments = utterances.read_segments(session.utts)
		token_group_counter = RoundTokenGroupCounter(self.token_groups, tuple(self.seg_utt_factory(segments)))
		return token_group_counter(*round_start_end_times)


def game_round_start_end_times(round_start_times: Iterator[float]) -> Iterator[Tuple[float, float]]:
	current_start_time = next(round_start_times)
	for next_start_time in round_start_times:
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


def print_partitioned_session_group_dists(session_group_dists: Dict[str, PartitionDistributions],
										  total_group_dists: PartitionDistributions,
										  partition_round_count,
										  outfile):
	referring_groups = tuple(
		sorted(frozenset(group for dists in session_group_dists.values() for group in dists.total.keys())))
	firstn_col_name = "FIRST_{}".format(partition_round_count)
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

	for dyad_id, group_dists in sorted(session_group_dists.items(), key=__get_item_key):
		row = [dyad_id]
		__append_group_freqs(group_dists, row, referring_groups)
		print(COL_DELIM.join(row), file=outfile)

	summary_row = [TOTAL_RESULTS_ROW_NAME]
	__append_group_freqs(total_group_dists, summary_row, referring_groups)
	print(COL_DELIM.join(summary_row), file=outfile)


def print_whole_session_group_freqs(session_group_dists: Dict[str, Distribution], total_group_dists: Distribution,
									outfile):
	ordered_total_group_freqs = tuple(
		(group, freq) for group, freq in sorted(total_group_dists.freqs.items(), key=__get_item_key))
	ordered_groups = tuple(group for (group, _) in ordered_total_group_freqs)
	header_cells = itertools.chain((DYAD_ID_COL_NAME,), ordered_groups)
	print(COL_DELIM.join(header_cells), file=outfile)

	for dyad_id, group_dists in sorted(session_group_dists.items(), key=__get_item_key):
		freqs = (group_dists.freq(group) for group in ordered_groups)
		print(COL_DELIM.join(itertools.chain((dyad_id,), (_create_rounded_decimal_repr(freq) for freq in freqs))),
			  file=outfile)

	summary_freqs = (freq for (_, freq) in ordered_total_group_freqs)
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


def __append_group_freqs(group_dists: PartitionDistributions, row, referring_groups: Iterable[str]):
	for referring_group in referring_groups:
		row.append(_create_rounded_decimal_repr(group_dists.first.freq(referring_group)))
		row.append(_create_rounded_decimal_repr(group_dists.next.freq(referring_group)))
		row.append(_create_rounded_decimal_repr(group_dists.total.freq(referring_group)))


def _create_rounded_decimal_repr(value: Decimal):
	return str(value.quantize(__DECIMAL_REPR_ROUNDING_EXP, ROUND_HALF_UP))


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

	named_sessions = walk_session_data(args.inpaths)
	outfile = sys.stdout
	seg_utt_factory = utterances.SegmentUtteranceFactory()
	partition_round_count = args.round_split
	if partition_round_count:
		dist_collector = PartitionedSessionGroupDistributionCollector(token_groups, seg_utt_factory)
		print("Splitting sessions after {} round(s).".format(partition_round_count), file=sys.stderr)
		session_group_dists, total_group_dists = dist_collector(named_sessions, partition_round_count)
		print_partitioned_session_group_dists(session_group_dists, total_group_dists,
											  partition_round_count, outfile)
	elif args.optimize_partition_diff:
		partitioner = OptimalSessionPartitioner(
			PartitionedSessionGroupDistributionCollector(token_groups, seg_utt_factory))
		session_group_dists, total_group_dists, partition_round_count = partitioner(tuple(named_sessions))
		print_partitioned_session_group_dists(session_group_dists, total_group_dists,
											  partition_round_count, outfile)
	else:
		dist_collector = WholeSessionGroupDistributionCollector(token_groups, seg_utt_factory)
		session_group_dists, total_group_dists = dist_collector(named_sessions)
		print_whole_session_group_freqs(session_group_dists, total_group_dists, outfile)


if __name__ == "__main__":
	__main(__create_argparser().parse_args())
