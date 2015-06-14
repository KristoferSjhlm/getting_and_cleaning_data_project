# The Code Book: Getting and Cleaning Data Course Project 

###Overview
This code book describes the variables, the data, and any transformations made by the R-script "run_analysis.R".

###Hardware and software used
- OS X Yosemite 10.10.3 on a Mac Book Pro.
- R Studio Version 0.99.441.
- R version 3.2.0 (2015-04-16)

###Data Set Description
The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 

A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

The data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

The data contains two sets: the training data and the test data which are placed in different folders. 
- 'features.txt': all features list.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'train/subject_train.txt': Identifies the person who performed the training activity.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.
- 'test/subject_test.txt': Identifies the person who performed the test activity.

###Variables in a resulting file tidy_data.txt
- "Type": The first six rows is the different activities (from LAYING to WALKING_UPSTAIRS) that the test persons performed. The rest is all the subjects from 1-30 and how the they individually performed.
- List of features where:
  - 'Acc': the accelerometer signals, 'Gyro' - the gyroscope signals
  - 'X','Y','Z': 3-axial signals in the X, Y and Z directions
  - 't': denote time
  - 'f': indicate frequency domain signals
  - Mean: Mean value
  - Std: Standard deviation
  - Jerk: Jerk signals
  - Mag: magnitude

###Transformations

#### Step 0. Getting the data
Download data into and unzip in to the working directory. All data are then available in the 'UCI HAR Dataset' directory and it's sub directorys.

#### Step 1. Merge the training and the test sets to create one data set. 
Reading first the test and then the train data 'x' and combined with their, 'y' and 'subject' data. Add variable name for all columns.  Then make the 'full_dataset' using the 'rbind' function. 
Then remove the big old datasets which are no longer required from the memory.

#### Step 2. Extract only the measurements on the mean and standard deviation for each measurement.
Extracting numbers of names of features which contain '-mean()' and '-std()' text using the 'grep' function. The new dataset with only the 'means' and 'std' are stored in 'data_mean_std'.

#### Step 3. Use descriptive activity names to name the activities in the data set.
Replace numbers of activities in 'y' using activities labels using a 'for loop' that checks and replaces the labels in every row. 

#### Step 4. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Using  a for loop that goes through every column and counts the mean with the help of the 'sapply' function' and stores in to an new data frame called 'full_means'.

#### Step 5. Export the tidy data to a text file
Create a text file called 'tidy_data.txt' in the working directory with the 'write.table' function.
