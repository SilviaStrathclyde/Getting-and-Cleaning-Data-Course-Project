## Getting and Cleaning Data Project

# 1. Merges the training and the test sets to create one data set.

# Loading packages
install.packages("data.table", "dplyr")
library(data.table)
library(dplyr)

# Reading training data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

# Reading test data
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

# Loading activity labels and features
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

# Binding the data and naming the columns
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)
colnames(features) <- t(featureNames[2])

# Merging the data sets
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
MergedData <- cbind(features,activity,subject)

# MergedData now contains both the Training and the Test data set.

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
columnsMeanSD <- grep(".*Mean.*|.*SD.*", names(MergedData), ignore.case=TRUE)
requiredColumns <- c(columnsMeanSD, 562, 563)
dim(MergedData)
extractData <- MergedData[,requiredColumns]
dim(extractData)

# 3. Uses descriptive activity names to name the activities in the data set.
extractData$Activity <- as.character(extractData$Activity)
for (i in 1:6){
  extractData$Activity[extractData$Activity == i] <- as.character(activityLabels[i,2])
}
extractData$Activity <- as.factor(extractData$Activity)

# 4. Appropriately labels the data set with descriptive variable names. 
# Viewing names to retrieve abbreviations
names(extractData)
names(extractData)<-gsub("Acc", "Accelerometer", names(extractData))
names(extractData)<-gsub("Gyro", "Gyroscope", names(extractData))
names(extractData)<-gsub("BodyBody", "Body", names(extractData))
names(extractData)<-gsub("Mag", "Magnitude", names(extractData))
names(extractData)<-gsub("^t", "Time", names(extractData))
names(extractData)<-gsub("^f", "Frequency", names(extractData))
names(extractData)<-gsub("tBody", "TimeBody", names(extractData))
names(extractData)<-gsub("-mean()", "Mean", names(extractData), ignore.case = TRUE)
names(extractData)<-gsub("-std()", "STD", names(extractData), ignore.case = TRUE)
names(extractData)<-gsub("-freq()", "Frequency", names(extractData), ignore.case = TRUE)
names(extractData)<-gsub("angle", "Angle", names(extractData))
names(extractData)<-gsub("gravity", "Gravity", names(extractData))
# Viewing names to see change of labels
names(extractData)

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
extractData$Subject <- as.factor(extractData$Subject)
extractData <- data.table(extractData)
tidyData <- aggregate(. ~Subject + Activity, extractData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

# Retrieving tidy data set (tidyData) as .txt and .csv files
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
write.table(tidyData, file = "TidyData.csv", row.names = FALSE)