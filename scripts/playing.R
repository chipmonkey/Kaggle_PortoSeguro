#
# Chip Lynch - Kaggle Porto Seguro Safe Driver Competition
#

options(scipen = 999) # Mostly disable scientific notation in display and file writes
options(stringsAsFactors=F)   # Disable conversion of strings to factors on read.  Sometimes useful.

rm(list=ls());  # WARNING!  this is effectively rm -r /*  !  Destroy everything!  Use with care!

gc() ; Sys.time() ; start_time <- Sys.time()  # Run this at the beginning

file_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # Generate a timestamp with 1-second accuracy
submission_filename <- paste0('../submission_', file_timestamp, '.csv')  # Create a timestamp filename



setwd("C:/Kaggle/Kaggle Porto Seguro Safe Driver/scripts")

train <- read.csv('../input/train.csv')
test <- read.csv('../input/test.csv')
ss <- read.csv('../input/sample_submission.csv')

gc() ; Sys.time() - start_time  # And run this at every useful checkpoint afterwards to do garbage collection and track time

#  No nulls or NAs!
which(is.na(train))
which(is.null(train))
which(is.na(test))
which(is.null(test))

