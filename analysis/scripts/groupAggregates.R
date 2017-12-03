#!/usr/bin/env Rscript

# Prints aggregate statistics for the results of cross-validation on reference resolution for the game "tangrams-restricted", comparing the training methods "ONE_NEG", "ALL_NEG" and "DIALOGIC".
#
# See Shore, T. Skantze, G. (2017) "Enhancing Reference Resolution in Dialogue Using Participant Feedback" <http://dx.doi.org/10.21437/GLU.2017-16> in Salvi, G. and Dupont, S. (eds.) Proceedings of the GLU2017 International Workshop on Grounding Language Understanding <http://dx.doi.org/10.21437/GLU.2017>, pp. 78--82.
#
#
# This file is part of tangrams-restricted.
#
# Tangrams-restricted is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


args <- commandArgs(trailingOnly=TRUE)
if(length(args) < 1)
{
	stop("Usage: <scriptname> INFILE")
}

infile <-args[1]
if (!file_test("-f", infile)) 
{
	stop(sprintf("No file found at \"%s\".", infile));
}

cvResults <- read.table(infile, sep="\t", header=TRUE)
origSampleSize <- nrow(cvResults)
print(sprintf("Read %d cross-validation sample(s).", origSampleSize), quote=FALSE)

#Take out the observation(s) with token count over 200 (in this data: one data point)
cvResults[!cvResults$TOKEN_COUNT>200,] -> cvResults
sampleSizeWithoutOutliers <- nrow(cvResults)
print(sprintf("Removed %d outlier(s).", origSampleSize - sampleSizeWithoutOutliers), quote=FALSE)

rank_avgs <- function(x) c(mean = mean(x), sd = sd(x), median = median(x), mad = mad(x))

writeLines("\nRank averages for each training method:")
aggregate(RANK ~ Training, cvResults, FUN=rank_avgs)

writeLines("\nSample counts for each training method:")
table(cvResults$Training)

writeLines("\nRank counts for each training method:")
table(cvResults$RANK,cvResults$Training)

writeLines("\nRank averages for each token count given each training method:")
aggregate(cvResults$RANK, list(TokenCount = factor(cvResults$TOKEN_COUNT), Training = factor(cvResults$Training)), FUN=rank_avgs)

writeLines("\nRank averages for each dyad given each training method:")
aggregate(cvResults$RANK, list(Dyad = factor(cvResults$DYAD), Training = factor(cvResults$Training)), FUN=rank_avgs)
