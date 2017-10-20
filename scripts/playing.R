#
# Chip Lynch - Kaggle Porto Seguro Safe Driver Competition
#
  
# Initial setup and library loading

    library(reshape2)
    
    options(scipen = 999) # Mostly disable scientific notation in display and file writes
    options(stringsAsFactors=F)   # Disable conversion of strings to factors on read.  Sometimes useful.
    
    rm(list=ls());  # WARNING!  this is effectively rm -r /*  !  Destroy everything!  Use with care!
    
    gc() ; Sys.time() ; start_time <- Sys.time()  # Run this at the beginning
    
    file_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # Generate a timestamp with 1-second accuracy
    submission_filename <- paste0('../submission_', file_timestamp, '.csv')  # Create a timestamp filename
    
    if(file.exists('./scripts')) {
      setwd('./scripts/')
    } else if(file.exists('C:/Kaggle/Kaggle Porto Seguro Safe Driver/scripts')) {
      setwd("C:/Kaggle/Kaggle Porto Seguro Safe Driver/scripts/")
    }

# Read the data
  train <- read.csv('../input/train.csv')
  test <- read.csv('../input/test.csv')
  ss <- read.csv('../input/sample_submission.csv')
  
  gc() ; Sys.time() - start_time  # And run this at every useful checkpoint afterwards to do garbage collection and track time


#  No nulls or NAs!
which(is.na(train))
which(is.null(train))
which(is.na(test))
which(is.null(test))

ttNormalize <- function(train, test, badnames) {
  # Input: train and test sets with theoretically identical domains
  #     No we don't test for this but we'll fail if bad happens
  # Output: normalized train and test sets, normalized against train stats
  
  trainHold <- train[,intersect(names(train),badnames)]
  testHold <- test[,intersect(names(test), badnames)]
  train <- train[,setdiff(names(train), badnames)]
  test <- test[,setdiff(names(test), badnames)]
  myMeans <- lapply(train, mean)
  mySDs <- lapply(train, sd)
  
  nrow(train)
  ncol(train)
  length(myMeans)
  length(mySDs)
  
  
  
  for(i in 1:ncol(train)) {
    train[,i] <- (train[,i] - myMeans[[i]]) / mySDs[[i]]
    test[,i] <- (test[,i] - myMeans[[i]]) / mySDs[[i]]
  }
  
  train <- cbind(train, trainHold)
  test <- cbind(test, testHold)
  
  
  return(list(train=train, test=test))
}

x <- ttNormalize(train,test, c('id', 'target'))
trainNorm <- x[[1]]
testNorm <- x[[2]]
rm(x)


sort(unlist(lapply(train, function(x) length(unique(x)))))
table(train$ps_car_11_cat)




###  Build some variable information:

# Badnames stores the primary key, the output variable,
# And any other variables we wish to categorically exclude:
  badNames <- c('id', 'target')

# Determine natural classes:
  classes <- lapply(train, class)
  factorNames <-  names(which(classes == 'factor'))
  integerNames <- names(which(classes == 'integer'))
  numericNames <- names(which(classes == 'numeric'))
  characterNames <- names(which(classes == 'character'))
  dateNames <- names(which(classes == 'Date'))
  binaryNames <- setdiff(names(which(lapply(train, function(x) length(unique(x))) == 2 )), badNames)
  
  variantThreshold = 30  # Tweak this usually based on actual data
  myCorrelations <- cor(train[,setdiff(names(train), badNames)])
    x <- myCorrelations
    x[!upper.tri(myCorrelations, diag=FALSE)] <- NA
  myCorrPairs <- na.omit(melt(x, value.name='correlation'))
    rm(x)
  myCorrPairs[which(abs(myCorrPairs$correlation) > 0.5),]

stop('unstop me to run the pretty')
# Pretty

summary(train)
summary(trainNorm)
par(mfrow=c(3,3))
lapply(train, function(x) plot(density(x)))
lapply(trainNorm, function(x) plot(density(x), col="blue"))
par(mfrow=c(1,1))



heatmap(myCorrelations)
plot(density(myCorrPairs$correlation))

myBig <- which(myCorrelations < 1 & myCorrelations > 0.5)
myBig
myCorrelations[myBig]
plot(density(myCorrelations[which(myCorrelations > 0.01)]))
hist(myCorrelations[which(myCorrelations > 0.01)])
