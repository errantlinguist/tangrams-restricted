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
orig_sample_size <- nrow(cv_results)
print(sprintf("Read %d cross-validation samples.", orig_sample_size), quote = FALSE)

#Take out the observation(s) with token count over 200 (in this data: one data point)
cv_results[!cv_results$TOKEN_COUNT>200,] -> cv_results
sample_size_without_outliers <- nrow(cv_results)
print(sprintf("Removed %d outlier.", orig_sample_size - sample_size_without_outliers), quote = FALSE)

rank_avgs <- function(x) c(mean = mean(x), sd = sd(x), median = median(x), mad = mad(x))

writeLines("\nRank averages for each training method:")
aggregate(RANK ~ Training, cv_results, FUN = rank_avgs)

writeLines("\nSample counts for each training method:")
table(cv_results$Training)

writeLines("\nRank counts for each training method:")
table(cv_results$RANK,cv_results$Training)

writeLines("\nRank averages for each token count given each training method:")
aggregate(cv_results$RANK, list(TokenCount = factor(cv_results$TOKEN_COUNT), Training = factor(cv_results$Training)), FUN = rank_avgs)

writeLines("\nRank averages for each dyad given each training method:")
aggregate(cv_results$RANK, list(Dyad = factor(cv_results$DYAD), Training = factor(cv_results$Training)), FUN = rank_avgs)
