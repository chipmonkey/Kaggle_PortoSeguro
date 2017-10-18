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

CompareSets <- function(test, train) {
  
  test <- as.data.frame(test)  # Hack a fix if the input is data.table or something
  train <- as.data.frame(test) # it works, but it could probably be better... fix later
  
  comprows <- ifelse(nrow(train) < nrow(test), nrow(train), nrow(test))
  fields <- intersect(names(train), names(test))
  fields <- setdiff(fields, 'id')
  # fields <- setdiff(fields, names(which(sapply(train, class) =='factor')))
  
  tt_compare <- data.frame(NULL)
  for (name in sort(fields)) {
    print(paste("field: ", name, " of class ", class(train[[name]])))
    
    if (class(train[[name]]) %in% c('numeric', 'integer')) {
      plot(density(na.omit(train[1:comprows,name])), col=rgb(1,0,0,0.5), main=name)
      lines(density(na.omit(test[1:comprows,name])), col=rgb(0,0,1,0.5))
      tt_compare <- rbind(tt_compare, 
                          cbind(name, ks.test(train[,name], test[,name])$stati))
    } else if(length(unique(train[,name])) < 50 && class(train[[name]]) == 'factor') {
      plot(train[,name], col=rgb(1,0,0,0.5), main=name)
      par(new=TRUE)
      plot(test[,name], col=rgb(0,0,1,0.5))
    }
  }
  tt_compare$V2 <- as.numeric(as.character(tt_compare$V2))
  print(tt_compare)
  return(tt_compare)
}

par( mfcol=c(3,3))
tt_compare <- CompareSets(test, train)
par( mfcol=c(1,1))

gc() ; Sys.time() - start_time  # And run this at every useful checkpoint afterwards to do garbage collection and track time


