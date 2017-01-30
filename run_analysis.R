# Coursera Data Science Specialization Course 3: Getting and Cleaning Data
# Course Project: Prepare a tidy data set that can be used for later analysis
# Set working directory to UCI HAR Dataset folder

# Clean up workspace
rm(list=ls())

# Load libraries
library("dplyr")
library("data.table")

# Download data if necessary
# fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
# download.file(fileURL, destfile = "Course3ProjectDataSet.zip")
# unzip("Course3ProjectDataSet.zip")

# Get and merge data
subject_train <- read.table('./train/subject_train.txt', header = FALSE)
x_train <- read.table('./train/X_train.txt', header = FALSE)
y_train <- read.table('./train/y_train.txt', header = FALSE)

subject_test <- read.table('./test/subject_test.txt', header = FALSE)
x_test <- read.table('./test/X_test.txt', header = FALSE)
y_test <- read.table('./test/y_test.txt', header = FALSE)

subjectDataSet <- rbind(subject_train, subject_test)
xDataSet <- rbind(x_train, x_test)
yDataSet <- rbind(y_train, y_test)

# dim(subjectDataSet)
# dim(xDataSet)
# dim(yDataSet)

# Extract only the measurements on the mean and standard deviation for each measurement
xDataSet_mean_stdev <- xDataSet[, grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2])]
names(xDataSet_mean_stdev) <- read.table("features.txt")[grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2]), 2]
View(xDataSet_mean_stdev)
# dim(xDataSet_mean_stdev)

# Use descriptive activity names to name the activities in the data set 
yDataSet[, 1] <- read.table("activity_labels.txt")[yDataSet[, 1], 2]
names(yDataSet) <- "Activity"
View(yDataSet)

# Appropriately label the data set with descriptive variable names
names(subjectDataSet) <- "Subject"
summary(subjectDataSet)

# Combine into single data set
TotalDataSet <- cbind(xDataSet_mean_stdev, yDataSet, subjectDataSet)

# Make some names more descriptive
names(TotalDataSet) <- make.names(names(TotalDataSet))
names(TotalDataSet) <- gsub('Acc',"Accel",names(TotalDataSet))
names(TotalDataSet) <- gsub('GyroJerk',"AngAccel",names(TotalDataSet))
names(TotalDataSet) <- gsub('Gyro',"AngSpeed",names(TotalDataSet))
names(TotalDataSet) <- gsub('Mag',"Magnitude",names(TotalDataSet))
names(TotalDataSet) <- gsub('^t',"TimeDomain.",names(TotalDataSet))
names(TotalDataSet) <- gsub('^f',"FreqDomain.",names(TotalDataSet))
names(TotalDataSet) <- gsub('\\.mean',".Mean",names(TotalDataSet))
names(TotalDataSet) <- gsub('\\.std',".StdDev",names(TotalDataSet))
names(TotalDataSet) <- gsub('Freq\\.',"Frequency.",names(TotalDataSet))
names(TotalDataSet) <- gsub('Freq$',"Frequency",names(TotalDataSet))

# View data
View(TotalDataSet)

# Make tidy data 
TidyData <-aggregate(. ~Subject + Activity, TotalDataSet, mean)
TidyData<-TidyData[order(TidyData$Subject,TidyData$Activity),]

# Produce tidy data output file
write.table(TidyData, file = "TidyData.txt",row.name=FALSE)

