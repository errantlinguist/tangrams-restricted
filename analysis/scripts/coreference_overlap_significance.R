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
  stop("Usage: <scriptname> INFILE [MAX_COREF_SEQ_LENGTH]")
}

infile <-args[1]
if (!file_test("-f", infile)) 
{
  stop(sprintf("No file found at \"%s\".", infile));
}

maxCorefChainLength <- ifelse(length(args) < 2, 6, strtoi(args[2]))
print(sprintf("Using a maximum coref sequence ordinality of %d.", maxCorefChainLength), quote=FALSE)

library(lmerTest)
library(MASS)
library(MuMIn)

filterInvalidCorefs <- function(corefOverlaps, minCorefChainLength, maxCorefChainLength) {
  origSampleSize <- nrow(corefOverlaps)
  corefOverlaps[!corefOverlaps$COREF_SEQ_ORDER<minCorefChainLength,] -> result
  sizeAfterMinPrune <- nrow(result)
  print(sprintf("Removed %d observation(s) lesser than coref sequence ordinality %d.", origSampleSize - sizeAfterMinPrune, minCorefChainLength), quote=FALSE)
  result[!result$COREF_SEQ_ORDER>maxCorefChainLength,] -> result
  sizeAfterMaxPrune <- nrow(result)
  print(sprintf("Removed %d observation(s) greater than coref sequence ordinality %d.", sizeAfterMinPrune - sizeAfterMaxPrune, maxCorefChainLength), quote=FALSE)
  return(result)
}

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/options.html
options(na.action=na.fail)

print(sprintf("Reading data from \"%s\".", infile), quote=FALSE)
corefOverlaps <- read.csv(infile, sep="\t", header=TRUE)
print(sprintf("Read %d coreference overlap value(s).", nrow(corefOverlaps)), quote=FALSE)
corefOverlaps$DYAD <- as.factor(corefOverlaps$DYAD)
corefOverlaps <- filterInvalidCorefs(corefOverlaps, 2, maxCorefChainLength)



#Model m.additive is an additive model (only main effects)
m.additive <- lmer(TOKEN_TYPE_OVERLAP ~ COREF_SEQ_ORDER +  (1 + SHAPE|DYAD), data=corefOverlaps, REML=FALSE)

summary(m.additive)
