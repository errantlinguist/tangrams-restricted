#!/usr/bin/env python3

import csv

import pandas as pd
import seaborn as sns


def __main(infile_path: str, outfile_path: str):
	sns.set()
	hue_token_overlap = pd.read_csv(infile_path, sep='\t', dialect=csv.excel_tab,
									float_precision="high", encoding="utf-8", memory_map=True)

	# Select a subset of the networks
	# used_networks = [1, 5, 6, 7, 8, 12, 13, 17]
	# used_columns = (df.columns.get_level_values("network")
	#                  .astype(int)
	#                  .isin(used_networks))
	# df = df.loc[:, used_columns]

	# Create a categorical palette to identify the networks
	# network_pal = sns.husl_palette(8, s=.45)
	# network_lut = dict(zip(map(str, used_networks), network_pal))

	# Convert the palette to vectors that will be drawn on the side of the matrix
	# networks = df.columns.get_level_values("network")
	# network_colors = pd.Series(networks, index=df.columns).map(network_lut)

	# Draw the full plot
	# sns.clustermap(df.corr(), center=0, cmap="vlag",
	#       row_colors=network_colors, col_colors=network_colors,
	#       linewidths=.75, figsize=(13, 13))
	hue_plot = sns.heatmap(hue_token_overlap)
	fig = hue_plot.get_figure()
	fig.savefig(outfile_path)


if __name__ == "__main__":
	import sys

	if len(sys.argv) < 3:
		raise ValueError("Usage: {} INFILE OUTFILE".format(sys.argv[0]))
	else:
		__main(sys.argv[1], sys.argv[2])
