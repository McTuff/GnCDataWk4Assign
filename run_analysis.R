#1 - Merges the training and the test sets to create one data set.
#2 - Extracts only the measurements on the mean and standard deviation for each measurement.
#3 - Uses descriptive activity names to name the activities in the data set
#4 - Appropriately labels the data set with descriptive variable names.
#5 - From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.

setwd("~/UCIHARDataset")
require(dplyr)
require(stringr)
require(qdapTools)
#read in required data
SubjectTest <- read.table("./test/subject_test.txt",sep="",stringsAsFactors=F,header=F,col.names="Subject")
Xtest <- read.table("./test/X_test.txt",sep="",stringsAsFactors=F,header=F)
Ytest <- read.table("./test/Y_test.txt",sep="",stringsAsFactors=F,header=F,col.names="ActivityLabel")
SubjectTrain <- read.table("./train/subject_train.txt",sep="",stringsAsFactors=F,header=F,col.names="Subject")
Xtrain <- read.table("./train/X_train.txt",sep="",stringsAsFactors=F,header=F)
Ytrain <- read.table("./train/Y_train.txt",sep="",stringsAsFactors=F,header=F,col.names="ActivityLabel")
ActivityLabels <- read.table("activity_labels.txt")

#combine into one Dataframe
DFtest <- cbind(SubjectTest,Ytest,Xtest)
DFtrain <- cbind(SubjectTrain,Ytrain,Xtrain)
DF <- rbind(DFtest,DFtrain)
#Extract only required columns based on feature names incl. "mean()" and "std()"
VNames <-read.table("features.txt",sep="",stringsAsFactors=F,header=F)
ExtractIndex <- which(str_detect(VNames$V2,fixed("mean()")) | str_detect(VNames$V2,fixed("std()")))
Extract <- ExtractIndex+2
Extract <- c(1,2,Extract)
DF <- DF[,Extract]
#rename activity label with activity name
DF$ActivityLabel<-lookup(DF$ActivityLabel,ActivityLabels[,1:2])
#rename Variable columns based on feature names
VNames <-read.table("features.txt",sep="",stringsAsFactors=F,header=F)
ExtractIndex <- which(str_detect(VNames$V2,fixed("mean()")) | str_detect(VNames$V2,fixed("std()")))
VNames <- VNames[ExtractIndex,2]
VNames <- gsub("-","",VNames)
VNames <- gsub("\\(|\\)","",VNames)
VNames <- c("subject","activity",VNames)
colnames(DF) <- VNames

NewDF <- DF %>% group_by(activity,subject)
TidyDF <- NewDF %>% summarize_each(funs(mean),-activity,-subject)

