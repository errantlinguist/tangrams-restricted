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

library(psych)

filterInvalidCorefs <- function(corefOverlaps, minCorefChainLength, maxCorefChainLength) {
  origSampleSize <- nrow(corefOverlaps)
  corefOverlaps[!corefOverlaps$seq<minCorefChainLength,] -> result
  sizeAfterMinPrune <- nrow(result)
  print(sprintf("Removed %d observation(s) lesser than coref sequence ordinality %d.", origSampleSize - sizeAfterMinPrune, minCorefChainLength), quote=FALSE)
  result[!result$seq>maxCorefChainLength,] -> result
  sizeAfterMaxPrune <- nrow(result)
  print(sprintf("Removed %d observation(s) greater than coref sequence ordinality %d.", sizeAfterMinPrune - sizeAfterMaxPrune, maxCorefChainLength), quote=FALSE)
  return(result)
}

testIntervalCorefSeqs <- function(corefOverlaps) {
  print("Testing correlation with coreference sequence ordinality considered as an interval value.", quote=FALSE)
  #print("Pearson correlation coefficient:", quote=FALSE)
  #print(rcorr(corefOverlaps$seq, corefOverlaps$overlap, type="pearson"))
  
  # https://www.rdocumentation.org/packages/stats/versions/3.4.3/topics/cor.test
  #cor.test(corefOverlaps$seq, corefOverlaps$overlap, alternative="two.sided", method="pearson", conf.level=0.999, exact=TRUE)
  testResults <- corr.test(corefOverlaps[,c("seq", "overlap")], method="pearson", alpha=0.02)
  print(testResults, short=FALSE, digits=4)
}

testOrdinalCorefSeqs <- function(corefOverlaps) {
  print("Testing correlation with coreference sequence ordinality considered as an ordinal value.", quote=FALSE)
  #print("Converting coreference sequence ordinalities to ordinal values.", quote=FALSE)
  #corefOverlaps <- cbind(corefOverlaps)
  #corefOverlaps$seq <- as.ordered(corefOverlaps$seq)
  
  #print("Spearman's rank correlation coefficient:", quote=FALSE)
  #print(rcorr(corefOverlaps$seq, corefOverlaps$overlap, type="spearman"))
  
  # https://www.rdocumentation.org/packages/stats/versions/3.4.3/topics/cor.test
  #cor.test(corefOverlaps$seq, corefOverlaps$overlap, alternative="two.sided", method = "spearman", conf.level=0.999, exact=FALSE)
  testResults <- corr.test(corefOverlaps[,c("seq", "overlap")], method="spearman", alpha=0.02)
  print(testResults, short=FALSE, digits=4)
}

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/options.html
options(na.action=na.fail)

print(sprintf("Reading data from \"%s\".", infile), quote=FALSE)
corefOverlaps <- read.table(infile, sep="\t", header=TRUE)
print(sprintf("Read %d coreference overlap value(s).", nrow(corefOverlaps)), quote=FALSE)


# https://stackoverflow.com/a/2677859/1391325
se <- function(x) sqrt(var(x)/length(x))

print("Coference overlap means:", quote=FALSE)
aggs <- aggregate(corefOverlaps$overlap, list(seq=corefOverlaps$seq), function(x) c(mean = mean(x), std = sd(x), sem = se(x)))
# https://stackoverflow.com/a/21509371/1391325
format(aggs, scientific=FALSE)
#round(aggs, 4)


corefOverlaps <- filterInvalidCorefs(corefOverlaps, 2, maxCorefChainLength)
testIntervalCorefSeqs(corefOverlaps)
testOrdinalCorefSeqs(corefOverlaps)
