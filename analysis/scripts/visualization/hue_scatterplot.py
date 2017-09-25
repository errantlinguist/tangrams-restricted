#!/usr/bin/env python3

import csv
import sys

import pandas as pd
import seaborn as sns


def __main(infile_path: str, outfile_path: str):
	sns.set()

	hue_token_overlap = pd.read_csv(infile_path, sep='\t', dialect=csv.excel_tab, float_precision="high",
									encoding="utf-8", memory_map=True)
	xaxis_name = hue_token_overlap.columns[1]
	yaxis_name = hue_token_overlap.columns[0]
	print("X axis: {}; Y axis: {}".format(xaxis_name, yaxis_name))

	# Create scatterplot of dataframe
	hue_plot = sns.lmplot(xaxis_name,  # Horizontal axis
						  yaxis_name,  # Vertical axis
						  data=hue_token_overlap,  # Data source
						  fit_reg=False,  # Don't fix a regression line
						  # hue="z",  # Set color
						  scatter_kws={"marker": "D",  # Set marker style
									   "s": 10})  # S marker size
	# fig = hue_plot.get_figure()
	hue_plot.savefig(outfile_path)


if __name__ == "__main__":

	if len(sys.argv) < 3:
		raise ValueError("Usage: {} INFILE OUTFILE".format(sys.argv[0]))
	else:
		__main(sys.argv[1], sys.argv[2])
