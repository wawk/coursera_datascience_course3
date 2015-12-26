## read X_data from test and train into respective data frames
xtest <- read.table("test/x_test.txt",na.strings = "NA",)
xtrain <- read.table("train/x_train.txt",na.strings = "NA")

## read Subject, Activity and Y data from files into data frames
testsub <- read.table("test/subject_test.txt", col.names = c("Subject"), na.strings = "NA")
trainsub <- read.table("train/subject_train.txt", col.names = c("Subject"), na.strings = "NA")
ytest <- read.table("test/y_test.txt", col.names = c("Activity"), na.strings = "NA")
ytrain <- read.table("train/y_train.txt", col.names = c("Activity"), na.strings = "NA")

## Create data frames combing xtest & xtrain using cbind then rbind
testDF <- cbind(xtest,testsub,ytest)
trainDF <- cbind(xtrain,trainsub,ytrain)
combinedDF <- rbind(testDF,trainDF)

## Get the list of features from the 2nd column of the data frame read in from data
featuresList <- read.table("features.txt", na.strings = "NA")[,2]

featureNames <- c(as.character(unlist(featuresList)),c("Subject","Activity"))

## rename the columns using feature names
colnames(combinedDF) <- featureNames

## Change the names in each row for the Activity to something more meaningful
combinedDF$Activity <- as.character(combinedDF$Activity)
combinedDF$Activity <- sub("1","WALKING", combinedDF$Activity)
combinedDF$Activity <- sub("2","WALKING_UPSTAIRS", combinedDF$Activity)
combinedDF$Activity <- sub("3","WALKING_DOWNSTAIRS", combinedDF$Activity)
combinedDF$Activity <- sub("4","SITTING", combinedDF$Activity)
combinedDF$Activity <- sub("5","STANDING", combinedDF$Activity)
combinedDF$Activity <- sub("6","LAYING", combinedDF$Activity)

## split out the data referencing the mean() and std() variables
mean_regex <- grep("mean()-", featureNames, value=T, fixed=T)
std_regex <- grep("std()-", featureNames, value=T, fixed=T)

## Create a subset of all the mean rows and std rows
meanDF <- subset(combinedDF, select=c(mean_regex,c("Subject","Activity")))
stdDF <- subset(combinedDF, select=c(std_regex, c("Subject", "Activity")))

## create a simple vector of just the sensor measurement names
meanDFnames <- as.vector(names(meanDF)[1:24])
td <- function(x){strsplit(x,"-")[[1]][1]}
str <- sapply(meanDFnames,td)

strvec <- unique(as.vector(str[1:24]))

## Create an empty Data Frame
df <- data.frame(
  sensor=rep(NA,1),
  mean_std=rep(NA,1),
  subject_id=rep(NA,1),
  activity=rep(NA,1),
  x_measure=rep(NA,1),
  y_measure=rep(NA,1),
  z_measure=rep(NA,1),
  stringsAsFactors = F)

## Internal functions for getting the mean and std data
t <- function(x,y) {
  subMeanDF <- subset(meanDF, meanDF["Subject"] == y)
  a <- split(subMeanDF[x],subMeanDF["Activity"])
  a
}

tstd <- function(x,y) {
  subStdDF <- subset(stdDF, stdDF["Subject"] == y)
  a <- split(subStdDF[x],subStdDF["Activity"])
}

dfrows <- nrow(df)

mean_std <- ""

r <- function(x) {
  for (sensr in x) {
    valx <- paste0(sensr,"-",mean_std,"()-X")
    valy <- paste0(sensr,"-",mean_std,"()-Y")
    valz <- paste0(sensr,"-",mean_std,"()-Z")
    for(subj in 1:30) {
      subject <- as.character(subj)
      if(mean_std == "mean") {
        a <- t(valx,subject)
        b <- t(valy,subject)
        c <- t(valz,subject)
      } else {
        a <- tstd(valx,as.character(subj))
        b <- tstd(valy,as.character(subj))
        c <- tstd(valz,as.character(subj))
      }
      sensor <- sensr
      for(index in 1:6) {
        activity <- names(a[index])
        xaxis <- mean(unlist(a[index]))
        yaxis <- mean(unlist(b[index]))
        zaxis <- mean(unlist(c[index]))
        df[dfrows,] <- c(sensor,mean_std,subject,activity,xaxis,yaxis,zaxis)
        dfrows <- dfrows + 1
      }
    }
  }
  df
}  

mean_std <- "mean"
df3 <- r(strvec)
mean_std <- "std"
df4 <- r(strvec)
finalDF <- rbind(df3,df4)

## Uncomment out the line below to create a txt file of the final tidy data set.
## write.table(finalDF,"tidy_human_activity_recognition.txt", row.names = F)
