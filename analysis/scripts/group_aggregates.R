#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if(length(args) < 1)
{
	stop("Usage: <scriptname> INFILE")
}

infile <-args[1]
if (!file_test("-f", infile)) 
{
	stop(sprintf("No file found at \"%s\".", infile));
}

cv_results <- read.table(infile, sep="\t", header=TRUE)

#Take out the observation(s) with token count over 200 (in this data: one data point)
cv_results[!cv_results$TOKEN_COUNT>200,] -> cv_results

#summary(cv_results)

rank_avgs <- function(x) c(mean = mean(x), sd = sd(x), median = median(x), mad = mad(x))

aggregate(RANK ~ Training, cv_results, FUN = rank_avgs)
table(cv_results$Training)
table(cv_results$RANK,cv_results$Training)

aggregate(cv_results$RANK, list(TokenCount = factor(cv_results$TOKEN_COUNT), Training = factor(cv_results$Training)), FUN = rank_avgs)
aggregate(cv_results$RANK, list(Dyad = factor(cv_results$DYAD), Training = factor(cv_results$Training)), FUN = rank_avgs)
