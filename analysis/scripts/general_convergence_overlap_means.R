#!/usr/bin/env Rscript

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

# https://www.statmethods.net/stats/correlations.html
# Correlations with significance levels
library(Hmisc)

coref_overlaps <- read.table(infile, sep="\t", header=TRUE)
print(sprintf("Read %d coreference overlap value(s).", nrow(coref_overlaps)), quote=FALSE)


# https://stackoverflow.com/a/2677859/1391325
se <- function(x) sqrt(var(x)/length(x))

print("Coference overlap means:", quote=FALSE)
aggs <- aggregate(coref_overlaps$overlap, list(seq=coref_overlaps$seq), function(x) c(mean = mean(x), sd = sd(x), sem = se(x)))
# https://stackoverflow.com/a/21509371/1391325
round(aggs, 3)