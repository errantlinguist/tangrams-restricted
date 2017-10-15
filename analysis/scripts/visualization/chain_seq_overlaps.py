#!/usr/bin/env python3

import csv
import sys

import pandas as pd
import seaborn as sns

import matplotlib.pyplot as plt

def __main(infile_path: str, outfile_path: str):
	sns.set()
	#sns.set_style("white")

	coref_seq_no_overlaps = pd.read_csv(infile_path, sep='\t', dialect=csv.excel_tab, float_precision="round_trip",
									encoding="utf-8", memory_map=True)

	fig, ax = plt.subplots()
	#ax.legend()
	# Create line plot of dataframe
	axis_self = sns.pointplot("RELEVANT_TOKENS_REFERENT_COREF_SEQ_SELF",  # Horizontal axis
						 "RELEVANT_TOKENS_REFERENT_OVERLAP_SELF",  # Vertical axis
						 data=coref_seq_no_overlaps, ax=ax, color="blue", label="self", legend=True, legend_out=True)
	#axis_self.set(label="Self")
	#axis_self.legend()
	axis_other = sns.pointplot("RELEVANT_TOKENS_REFERENT_COREF_SEQ_OTHER",  # Horizontal axis
							  "RELEVANT_TOKENS_REFERENT_OVERLAP_OTHER",  # Vertical axis
							  data=coref_seq_no_overlaps, ax=ax, color="green", label="other", legend=True, legend_out=True)
	#axis_other.set(label="Other")
	#axis_other.legend()
	ax.set(xlabel='Sequence length', xlim=(0, 8.5), ylabel='Overlap', ylim=(0, None))
	#ax.legend()
	#axis_self.legend(handles=axis_self.lines[::len(coref_seq_no_overlaps) + 1], labels=["Self"])
	#axis_other.legend(handles=axis_other.lines[::len(coref_seq_no_overlaps) + 1], labels=["Other"])
	plt.legend()

	fig.savefig(outfile_path)


if __name__ == "__main__":

	if len(sys.argv) < 3:
		raise ValueError("Usage: {} INFILE OUTFILE".format(sys.argv[0]))
	else:
		__main(sys.argv[1], sys.argv[2])
