# Getting-and-Cleaning-Data-Course-Project
The Getting and Cleaning Data Course Project

# Problem Summary
One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data in this project represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.


# Repository Contents
File Name | Description
------------ | -------------
README.md | Documentation explaining the project and how to use files contained in the repository.
codebook.md |  Describes the variables 
run_analysis.R | R script used to create tidy data

# Key Concepts in the Analysis
1. Use of read.table() to read the relevant txt files from the UCI HAR Dataset folder
2. Use of cbind() and rename() to combine the records of the following files : subject_test.txt, X_test and Y_test (same for the 'train'      files)
3. Use of rbind() in order to merge the training and the test sets to create one data set 
4. Use of setnames() to replace standart variable names V1, V2... with the feature's names from the features.txt file
5. Use of grep() to extracts only the measurements on the mean and standard deviation for each measurement
6. Use of levels() to give descriptive activity names the activities in the data set
7. Use of mgsub() to label the data set with descriptive variable names
8. Use of melt() and dcast() to create an independent tidy data set with the average of each variable for each activity and each subject
