##Getting and Cleaning Data Course Project
# The purpose of this project is to demonstrate your ability to collect, work with, and clean
# a data set. The goal is to prepare tidy data that can be used for later analysis. 
# You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
#   
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following.
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


## Download and unzip the dataset
# UNCOMMENT IF NEEDED
# Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# filename <- "getdata_dataset.zip"
# if (!file.exists(filename)){
#   download.file(Url, filename, method="curl")
# }
# if (!file.exists("UCI HAR Dataset")) { 
#   unzip(filename) 
# }

#read metadata files
labels <- read.table("UCI HAR Dataset/activity_labels.txt", row.names = 1)
features_labe <- read.table("UCI HAR Dataset/features.txt", row.names = 1)

#train
train_feat <- read.table("UCI HAR Dataset/train/X_train.txt")
train_activity<- read.table("UCI HAR Dataset/train/Y_train.txt")
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")
#test
test_feat <- read.table("UCI HAR Dataset/test/X_test.txt")
test_activity<- read.table("UCI HAR Dataset/test/Y_test.txt")
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")

#1.- Merge the training and the test sets to create one data set.
# Load the datasets
subject <- rbind(train_subject, test_subject)
activity <- rbind(train_activity, test_activity)
features <- rbind(train_feat, test_feat)
colnames(activity) <- "activity"
colnames(subject) <- "subject"

#I can change the name of the features to the features labels
colnames(features)<- t(features_labe)

#merge all
colnames(features) <- tolower(colnames(features))#to normalize names to lower
all <- cbind(features, subject, activity)

#2. Extract only the measurements on the mean and standard deviation for 
# each measurement

#First thing is detecting the columns that fulfill our requeriment
index <- grep(".*mean.*|.*std.*",names(all))
index
#
all_2 <- all[,index]
all_2 <- cbind(all_2, subject= all$subject, activity = all$activity)
  
#3. Uses descriptive activity names to name the activities in the data set
all_2$activity <- as.character(all_2$activity)
for (i in 1:6){
  all_2$activity[all_2$activity == i] <- as.character(labels[i,1])
}

#4. Appropriately labels the data set with descriptive variable names.
# acc = accelerometer
# gyro = gyroscope
# bodybody = body
# mag = magnitude
# f = frequency
# t = time

names(all_2) <- gsub("acc", "accelerometer", names(all_2))
names(all_2) <- gsub("gyro", "gyroscope", names(all_2))
names(all_2) <- gsub("body", "body", names(all_2))
names(all_2) <- gsub("mag", "magnitude", names(all_2))
#
names(all_2) <- gsub("^f", "frequency", names(all_2))
names(all_2) <- gsub("^t", "time", names(all_2))
names(all_2)

#5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
library(dplyr)
tidy_data <- all_2 %>% aggregate(c(activity, subject), mean)
write.table(tidy_data, "tidy_data.csv", row.name=FALSE)

