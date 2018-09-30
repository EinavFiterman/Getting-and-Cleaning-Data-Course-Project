library(dplyr)
library(data.table)
library(textclean)
library(reshape2)


features <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/features.txt")
activityLables <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")

trainSubjects <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
trainMeaurements <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
trainActivity <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/train/Y_train.txt") 

trainSubjects <- rename(trainSubjects, subjectID = V1)
trainActivity <- rename(trainActivity, activity = V1)

trainData <- cbind(trainSubjects, trainMeaurements, trainActivity)


testSubjects <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
testMeaurements  <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
testActivity <- read.table("./getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/test/Y_test.txt") 

testSubjects <- rename(testSubjects, subjectID = V1)
testActivity <- rename(testActivity, activity = V1)

testData <- cbind(testSubjects, testMeaurements, testActivity)

# Merges the training and the test sets to create one data set
mergedTrainTest <- rbind(trainData, testData)
# Replace standart variable names V1, V2... with the feature's names from the features.txt file
setnames(mergedTrainTest, names(trainMeaurements), as.character(features$V2))
# Extracts only the measurements on the mean and standard deviation for each measurement

meanStdFeaturesCols <- grep("mean|std", names(mergedTrainTest))

meanStdMeaurements <- mergedTrainTest[,meanStdFeaturesCols]
# Add the subjectID and activity cols to the new data set (with only the mean and std measurements)
meanStdMeaurements<- cbind(subjectID = mergedTrainTest$subjectID, meanStdMeaurements, activity = mergedTrainTest$activity)

#Gives descriptive activity names to name the activities in the data set
meanStdMeaurements$activity <- levels(activityLables$V2)[meanStdMeaurements$activity]
#labels the data set with descriptive variable names
oldVariablesLables <- c("^t", "Acc", "Gyro", "Mag", "^f")
newVariablesLables <- c("time", "Accelerometer", "Gyroscope", "Magnitude", "fastFourierTransform")
names(meanStdMeaurements) <- mgsub(names(meanStdMeaurements), oldVariablesLables, newVariablesLables, fixed = FALSE)

#Independent tidy data set with the average of each variable for each activity and each subject.
meltedMeaurements <- melt(meanStdMeaurements, c("subjectID", "activity"), names(meanStdMeaurements)[2:80])
newDataSet <- dcast(meltedMeaurements, subjectID +  activity ~ variable, mean)

# Write the tidy data set created in step 5 of the instructions
write.table(newDataSet, "tidyData.txt", row.name=FALSE)

