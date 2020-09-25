#First extract the train and test data from folders
library(readr)
Xtrain <- read.table("train/X_train.txt",header=FALSE,sep = "",dec = ".")
ytrain <- read.table("train/y_train.txt",header=FALSE,sep = "",dec = ".")
Xtest <- read.table("test/X_test.txt",header=FALSE,sep = "",dec = ".")
ytest <- read.table("test/y_test.txt",header=FALSE,sep = "",dec = ".")

#Next bind the train tables together and test tables together
train <- cbind(Xtrain,ytrain)
test <- cbind(Xtest,ytest)

#Finally, bind the train and test sets together
data <- rbind(train,test)

#Read in the feature names
features <- read.table("features.txt")

#Now find the indices of features that have "mean()" or "std()" in them
inds <- grep("mean\\(\\)|std\\(\\)",features[,2])
newData <- data[,meanInds]

#Now lets extract the actual names of the features
indVals <- grep("mean\\(\\)|std\\(\\)",features[,2],value=TRUE)

#Make a function to make descriptive titles based on current titles
makeNewTitle <- function(oldTitle) {
  newTitle = ""
  if(grepl("mean\\(\\)",oldTitle)) {
    newTitle <- paste(newTitle,"Mean of",sep="")
  } else if (grepl("std\\(\\)",oldTitle)) {
    newTitle <- paste(newTitle,"Standard Deviation of",sep="")
  }
  if (grepl("Body",oldTitle)) {
    newTitle <- paste(newTitle, "Body")
  } else if (grepl("Gravity",oldTitle)) {
    newTitle <- paste(newTitle, "Gravity")
  }
  if (grepl("Acc",oldTitle)) {
    newTitle <- paste(newTitle, "Accelerometer")
  } else if (grepl("Gyro",oldTitle)) {
    newTitle <- paste(newTitle, "Gyroscope")
  }
  if (grepl("Jerk",oldTitle)) {
    newTitle <- paste(newTitle, "Jerk Signals")
  } 
  if (grepl("Mag",oldTitle)) {
    newTitle <- paste(newTitle, "Magnitude Calculation")
  }
  if (grepl("X",oldTitle)) {
    newTitle <- paste(newTitle, "in X Direction")
  } else if (grepl("Y",oldTitle)) {
    newTitle <- paste(newTitle, "in Y Direction")
  } else if (grepl("Z",oldTitle)) {
    newTitle <- paste(newTitle, "in Z Direction")
  }
  if (grepl("^t",oldTitle)) {
    newTitle <- paste(newTitle,"(Time Domain)")
  } else if (grepl("^f",oldTitle)) {
    newTitle <- paste(newTitle, "(Frequency Domain)")
  }
  newTitle
}

newTitles <- sapply(indVals,makeNewTitle)

#Assign new names to the names of the dataset
names(newData) <- newTitles
avgSet <- sapply(newData,function(x){mean(x)})
write.table(avgSet,file="table2.txt",row.names = FALSE)