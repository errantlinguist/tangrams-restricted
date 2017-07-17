#!/usr/bin/env python3

import statistics
from collections import defaultdict


COL_DELIM = "\t"
DIALOGIC_TRAINING_METHOD_PARAM_NAME = "DIALOGIC"
UTT_REL_LOG_FILE_SUFFIX = ".uttrels.tsv"


class UtteranceRelation(object):

	def __init__(self, instr_utt, sent_val, prev_utts, prev_utt_total_tokens):
		self.instr_utt = instr_utt
		self.sent_val = sent_val
		self.prev_utts = prev_utts
		self.prev_utt_total_tokens = prev_utt_total_tokens
		
	def __repr__(self):
		return self.__class__.__name__ + str(self.__dict__)
		
	@property
	def prev_utt_count(self):
		return len(self.prev_utts)

def create_event_time_utt_rel_dict(inpath):
	result = defaultdict(dict)
	with open(inpath, 'r') as lines:
		rows = (line.strip().split(COL_DELIM) for line in lines)
		col_name_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(next(rows)))
		evt_time_idx = col_name_idxs["EVENT_TIME"]
		diag_rel_no_idx = col_name_idxs["DIAG_REL_NO"]
		instr_utt_idx = col_name_idxs["INSTR_UTT"]
		sent_val_idx = col_name_idxs["SENT_VALUE"]
		prev_utts_idx = col_name_idxs["PREV_UTTS"]
		prev_utt_total_tokens_idx = col_name_idxs["PREV_UTT_TOTAL_TOKENS"]
		for row in rows:
			evt_time = row[evt_time_idx]
			evt_diags = result[evt_time]
			diag_rel_no = row[diag_rel_no_idx]
			old_rel = evt_diags.get(diag_rel_no)
			
			instr_utt = row[instr_utt_idx]
			if old_rel is None:
				sent_value_str = row[sent_val_idx]
				try:
					prev_utts_str = row[prev_utts_idx]
					prev_utts = prev_utts_str[1:len(prev_utts_str) - 1].split("\", \"")
				except IndexError:
					prev_utts = ()
				
				utt_rel = UtteranceRelation(instr_utt, float(sent_value_str), prev_utts, int(row[prev_utt_total_tokens_idx]))
				evt_diags[diag_rel_no] = utt_rel
			else:
				if instr_utt != old_rel.instr_utt:
					raise ValueError("Duplicate row found for timestamp \"%s\" and relation no. \"%s\" but with non-equal values.", (evt_time, diag_rel_no))
				
			
			
			
	return result
	

	
def print_utt_rel_data(inpath, utt_rels):
	with open(inpath, 'r') as lines:
		rows = (line.strip().split(COL_DELIM) for line in lines)
		header = next(rows)
		col_name_idxs = dict((col_name, idx) for (idx, col_name) in enumerate(header))
		header.append("PREV_UTT_COUNT")
		header.append("PREV_UTT_TOTAL_TOKENS")
		header.append("MEAN_SENT_VALUE")
		header.append("DIALOGIC_INFO")
		header.append("PREV_UTT_COUNT_USED_DIALOGICALLY")
		header.append("PREV_TOKEN_COUNT_USED_DIALOGICALLY")
		print(COL_DELIM.join(header))
		
		evt_time_idx = col_name_idxs["EVENT_TIME"]
		training_idx = col_name_idxs["Training"]
		for row in rows:
			evt_time = row[evt_time_idx]
			evt_utt_rels = utt_rels[evt_time]
			row.append(sum(len(utt_rel.prev_utts) for utt_rel in evt_utt_rels.values()))
			row.append(sum(utt_rel.prev_utt_total_tokens for utt_rel in evt_utt_rels.values()))
			
			mean_sent_val = 0.0
			utt_count_used_dialogically = 0
			token_count_used_dialogically = 0
			if DIALOGIC_TRAINING_METHOD_PARAM_NAME == row[training_idx]:
				mean_sent_val = statistics.mean(utt_rel.sent_val for utt_rel in evt_utt_rels.values())
				for evt_utt_rel in evt_utt_rels.values():
					used_dialogically = evt_utt_rel.sent_val != 0 and evt_utt_rel.prev_utt_total_tokens > 0
					if used_dialogically:
						utt_count_used_dialogically += len(evt_utt_rel.prev_utts)
						token_count_used_dialogically += evt_utt_rel.prev_utt_total_tokens
		
			dialogic_info = 1 if token_count_used_dialogically > 0 else 0
			row.append(mean_sent_val)
			row.append(dialogic_info)
			row.append(utt_count_used_dialogically)
			row.append(token_count_used_dialogically)
			print (COL_DELIM.join(str(cell) for cell in row))

if __name__ == "__main__":
	import sys
	if len(sys.argv) < 2:
		raise ValueError("Usage: %s INPUT_PATHS... > OUTFILE" % sys.argv[0])
	else:
		inpath = sys.argv[1]
		uttrel_inpath = inpath + UTT_REL_LOG_FILE_SUFFIX
		print("Reading utterance relations from \"%s\"." % uttrel_inpath, file=sys.stderr)
		utt_rels = create_event_time_utt_rel_dict(uttrel_inpath)
		print("Read values for %d unique event(s)." % len(utt_rels), file=sys.stderr)		
		
		print("Reading test results from \"%s\"." % inpath, file=sys.stderr)
		print_utt_rel_data(inpath, utt_rels)