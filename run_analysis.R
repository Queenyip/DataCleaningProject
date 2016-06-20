getwd()
setwd ("E:/Coursera/Data Science/Getting and Cleaning Data/Week 4")

#Load packages
rm(list = ls(all = TRUE))
library(plyr)  
library(data.table) 
library(dplyr) 

#Download zip files
temp <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)

#List variables
unzip(temp, list = TRUE)
#Load tables y_test, x_test, y_train, x_train, y_subject, x_subject
YTest <- read.table(unzip(temp, "UCI HAR Dataset/test/y_test.txt"))
XTest <- read.table(unzip(temp, "UCI HAR Dataset/test/X_test.txt"))
SubjectTest <- read.table(unzip(temp, "UCI HAR Dataset/test/subject_test.txt"))
YTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/y_train.txt"))
XTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/X_train.txt"))
SubjectTrain <- read.table(unzip(temp, "UCI HAR Dataset/train/subject_train.txt"))
Features <- read.table(unzip(temp, "UCI HAR Dataset/features.txt"))

# Remove the temp file
unlink(temp)

# Check out data tables
str(Features)
str(SubjectTest)
str(XTest)
str(XTrain)
str(YTest)
str(YTrain)
# Change variable names
colnames(XTrain) <- t(Features[2])
colnames(XTest) <- t(Features[2])

str(SubjectTest)
#Task 1:  Merge subjects/actions to a test file and a training file
XTrain$actions <- YTrain[, 1]
XTrain$subjects <- SubjectTrain[, 1]
XTest$actions <- YTest[, 1]
XTest$subjects <- SubjectTest[, 1]

str(XTrain)
str(XTest)
#Merge test and traning files
group <- c("test")
XTest1 <- cbind(XTest, group)
group <- c("training")
XTrain1 <- cbind(XTrain, group)
combined <- rbind(XTrain1, XTest1)

#Task 2:  Extract only the measurements on the mean and sD for each measurement
str(combined)
Mean <- combined[,grep("mean()", names(combined), value = TRUE)]
STD <- combined[,grep("std()", names(combined), value = TRUE)]
subjects <- combined[,grep("subjects", names(combined), value = TRUE)]
actions <- combined[,grep("actions", names(combined), value = TRUE)]
group <- combined[,grep("group", names(combined), value = TRUE)]
masterdata <- cbind(Mean, STD, subjects, actions, group)
#Check if correct variables are included
str(masterdata)


#Task 3:  Use descriptive activity names to name activities in the dataset
masterdata$actions <- as.character(masterdata$actions)
masterdata$actions[masterdata$actions == 1] <- "Walking"
masterdata$actions[masterdata$actions == 2] <- "Walking Upstairs"
masterdata$actions[masterdata$actions == 3] <- "Walking Downstairs"
masterdata$actions[masterdata$actions == 4] <- "Sitting"
masterdata$actions[masterdata$actions == 5] <- "Standing"
masterdata$actions[masterdata$actions == 6] <- "Laying"
masterdata$actions <- as.factor(masterdata$actions)

#Task 4:  Appropriately label the dataset
names(masterdata) 

names(masterdata) <- gsub("Acc", "Accelerator", names(masterdata))
names(masterdata) <- gsub("Mag", "Magnitude", names(masterdata))
names(masterdata) <- gsub("Gyro", "Gyroscope", names(masterdata))
names(masterdata) <- gsub("^t", "time", names(masterdata))
names(masterdata) <- gsub("^f", "frequency", names(masterdata))

masterdata$subjects <- as.character(masterdata$subjects)
masterdata$subjects[combined$subjects == 01] <- "subject 01"
masterdata$subjects[combined$subjects == 02] <- "subject 02"
masterdata$subjects[combined$subjects == 03] <- "subject 03"
masterdata$subjects[combined$subjects == 04] <- "subject 04"
masterdata$subjects[combined$subjects == 05] <- "subject 05"
masterdata$subjects[combined$subjects == 06] <- "subject 06"
masterdata$subjects[combined$subjects == 07] <- "subject 07"
masterdata$subjects[combined$subjects == 08] <- "subject 08"
masterdata$subjects[combined$subjects == 09] <- "subject 09"
masterdata$subjects[combined$subjects == 10] <- "subject 10"
masterdata$subjects[combined$subjects == 11] <- "subject 11"
masterdata$subjects[combined$subjects == 12] <- "subject 12"
masterdata$subjects[combined$subjects == 13] <- "subject 13"
masterdata$subjects[combined$subjects == 14] <- "subject 14"
masterdata$subjects[combined$subjects == 15] <- "subject 15"
masterdata$subjects[combined$subjects == 16] <- "subject 16"
masterdata$subjects[combined$subjects == 17] <- "subject 17"
masterdata$subjects[combined$subjects == 18] <- "subject 18"
masterdata$subjects[combined$subjects == 19] <- "subject 19"
masterdata$subjects[combined$subjects == 20] <- "subject 20"
masterdata$subjects[combined$subjects == 21] <- "subject 21"
masterdata$subjects[combined$subjects == 22] <- "subject 22"
masterdata$subjects[combined$subjects == 23] <- "subject 23"
masterdata$subjects[combined$subjects == 24] <- "subject 24"
masterdata$subjects[combined$subjects == 25] <- "subject 25"
masterdata$subjects[combined$subjects == 26] <- "subject 26"
masterdata$subjects[combined$subjects == 27] <- "subject 27"
masterdata$subjects[combined$subjects == 28] <- "subject 28"
masterdata$subjects[combined$subjects == 29] <- "subject 29"
masterdata$subjects[combined$subjects == 30] <- "subject 30"
masterdata$subjects <- as.factor(masterdata$subjects)

#Task 5:  From the data in step 4, create a second, independent tidy dataset
#with the average of each variable for each activity and each subject

tidydataset<-aggregate(. ~subjects + actions, masterdata, mean)
write.table(tidydataset, file = "tidydata.txt",row.name=FALSE)

