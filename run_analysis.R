# ----------------------------------------------------------
# Getting and Cleaning Data Course Project
# Purpose: The script performs data cleaning and summarization on the
# "Human Activity Recognition Using Smartphones" (UCI HAR) dataset.
# ----------------------------------------------------------
library(dplyr)

# Download and unzip the dataset (if it doesn't exist)
filename <- "UCI_HAR_Dataset.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# Read features and activity labels
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
# Read test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
# Read training data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# --------------------------------------------------------------------
# Step 1: Merge the training and the test sets to create one data set.
# --------------------------------------------------------------------
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

# ----------------------------------------------------------------------------------------------
# Step 2: Extract only the measurements on the mean and standard deviation for each measurement. 
# ----------------------------------------------------------------------------------------------
# %>%  pipe operator from dplyr:pass the output of one function as the input to the next function
TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

# ------------------------------------------------------------------------------
# Step 3: Use descriptive activity names to name the activities in the data set.
# ------------------------------------------------------------------------------
# Replace activity codes with activity names
TidyData$code <- activities[TidyData$code, 2]

# -------------------------------------------------------
# Step 4: Label data set with descriptive variable names.
# -------------------------------------------------------
# gsub():GLOBAL string substitution,it finds and replaces ALL text patterns inside character strings.
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))

# --------------------------------------------------------------------------------------------------------
# Step 5: Create second tidy dataset with the average of each variable for each activity and each subject.
# --------------------------------------------------------------------------------------------------------
FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(mean)

# output table
write.table(FinalData, "TidyData.txt", row.name=FALSE)
cat("TidyData.txt has been created successfully.\n")
