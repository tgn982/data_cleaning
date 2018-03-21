## Getting and Cleaning Data Peer-graded Assignment 

## 1.Merges the training and the test sets to create one data set.
## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3.Uses descriptive activity names to name the activities in the data set
## 4.Appropriately labels the data set with descriptive variable names. 
## 5.From the data set in step 4, creates a second, independent tidy data set 
##   with the average of each variable for each activity and each subject.


#### Step 0 : Library Import
##          Preparing data - reading raw data to make basic data frame 

## Library import
library(dplyr)
library(tidyr)

## Set Path of Project path (activity_labels, features)
## Set Path of Test data path and Train data path
projectPath <- "./UCI HAR Dataset"
testPath <-"./UCI HAR Dataset/test"
trainPath <-"./UCI HAR Dataset/train"

## read data from files under project folder (activity_labels, features)
## "features" have duplicated names. 
## To make the names identical, uniting V1,V2 is conducted.  
activeLabels <- read.table(file.path(projectPath,"activity_labels.txt"))
features <- read.table(file.path(projectPath,"features.txt"))
noFeatures <- unite(features,"no_features",V1,V2)

## read data from files under test/train folder (subject, X, y)
subjectTest <- read.table(file.path(testPath,"subject_test.txt"))
xTest <- read.table(file.path(testPath,"X_test.txt"))
yTest <- read.table(file.path(testPath,"y_test.txt"))
subjectTrain <- read.table(file.path(trainPath,"subject_train.txt"))
xTrain <- read.table(file.path(trainPath,"X_train.txt"))
yTrain <- read.table(file.path(trainPath,"y_train.txt"))



#### Step 1 : Merges the training and the test sets to create one data set
## labeling the x-data, y-data, subject
names(xTest) <- unlist(noFeatures)
names(xTrain) <- unlist(noFeatures)
names(yTest) <- "activity"
names(yTrain) <- "activity"
names(subjectTest) <- "subject"
names(subjectTrain) <- "subject"


## Data column bind each Test and Train data set
dataTest <- cbind(subjectTest,yTest,xTest)
dataTrain <- cbind(subjectTrain,yTrain,xTrain)


## merge test and Train data set
mergedData <- rbind(dataTest, dataTrain)


#### Step 2 : Merges the training and the test sets to create one data set
mData <- tbl_df(mergedData)
selected <- select(mData, contains("activity"), contains("subject"),contains("mean()"),contains("std()"))


#### Step 3 : Uses descriptive activity names to name the activities in the data set
selected$activity <- activeLabels[selected$activity,2]


#### Step 4 : Appropriately labels the data set with descriptive variable names. 
## delete number & _ which is added before. 
names(selected) <- gsub("[0-9]","", names(selected))
names(selected) <- gsub("_","", names(selected))

names(selected) <- gsub("^f","Frequency.Domain.", names(selected))
names(selected) <- gsub("^t","Time.Domain.", names(selected))
names(selected) <- gsub("mean\\(\\)","Mean", names(selected))
names(selected) <- gsub("std\\(\\)","Standard.Deviation", names(selected))
names(selected) <- gsub("-",".", names(selected))

#### Step 5 :From the data set in step 4, creates a second, independent tidy data set 
##           with the average of each variable for each activity and each subject.
second <- summarize_all(group_by(selected,activity,subject),funs(mean))




