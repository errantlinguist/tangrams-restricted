#!/usr/bin/env Rscript

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
