# run_analysis.R
## Getting and Cleaning Data, Course Project
## Mitra Soleimani 
## 2015- 06- 20

# runAnalysis.r File Description:
# The script will perform the following steps on the UCI HAR Dataset downloaded from:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
# 3. Use descriptive activity names to name the activities in the data set.
# 4. Appropriately label the data set with descriptive activity names.
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# Step 1. Merge the training and the test sets to create one data set
#set working directory to the location of  the unzipped UCI HAR Dataset 
setwd('/Users/Soleimani/Desktop/UCI HAR Dataset/')
# Read in the data from files
features = read.table('./features.txt',header=FALSE) #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE) #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE) #imports subject_train.txt
xTrain = read.table('./train/x_train.txt',header=FALSE) #imports x_train.txt
yTrain = read.table('./train/y_train.txt',header=FALSE) #imports y_train.txt
# Assigin column names to the imported data
colnames(activityType) = c('activityId','activityType')
colnames(subjectTrain) = "subjectId"
colnames(xTrain) = features[,2]
colnames(yTrain) = "activityId"
# Merge  yTrain, subjectTrain, and xTrain to create final train set

trainData = cbind(yTrain,subjectTrain,xTrain)

# Read in the test data


subjectTest = read.table('./test/subject_test.txt',header=FALSE) #imports subject_test.txt
xTest = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt
# Assign column names to the test data 
colnames(subjectTest) = "subjectId"
colnames(xTest) = features[,2]
colnames(yTest) = "activityId"
# Merge the xTest, yTest and subjectTest data to create final test set
testData = cbind(yTest,subjectTest,xTest)
# Combine training and test data to create a final data set
combinedData = rbind(trainData,testData)
featureNames <- readData("features.txt")[, 2]
names(combinedData) <- featureNames

# Step 2, Extract only the measurements on the mean and standard deviation for each
# measurement.
# Limit to columns with feature names matching mean() or std():
matches <- grep("(mean|std)\\(\\)", names(combinedData))
limited <- combinedData[, matches]
# Use descriptive activity names to name the activities in the data set.
# get the activity data and remap again
yMerged <- rbind(yTrain, yTest)[, 1]
activityNames <- c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
activities <- activityNames[yMerged]
# Step 4, Appropriately label the data set with descriptive variable names.
# Change t to Time, f to Frequency, mean() to Mean and std() to StdDev
names(limited) <- gsub("^t", "Time", names(limited))
names(limited) <- gsub("^f", "Frequency", names(limited))
names(limited) <- gsub("-mean\\(\\)", "Mean", names(limited))
names(limited) <- gsub("-std\\(\\)", "StdDev", names(limited))
# Remove extra dashes and BodyBody naming error from original feature names
names(limited) <- gsub("-", "", names(limited))
names(limited) <- gsub("BodyBody", "Body", names(limited))
# Add activities and subject with nice names
subjects <- rbind(subjectTrain, subjectTest)[, 1]
tidy <- cbind(Subject = subjects, Activity = activities, limited)
#  From the data set in step 4, create a second, independent tidy data set with the
# average of each  variable for each activity and each subject.
# Column means for all but the subject and activity columns
library(plyr)
limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans)
names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])
write.table(tidyMeans, "tidyMeans.txt", row.names = FALSE)





