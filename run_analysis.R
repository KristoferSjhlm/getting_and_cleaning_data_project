## download and unzip the data
print("Downloading and unzipping the data")
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data.zip", method="curl")
unzip("data.zip", files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir ="./", unzip = "internal", setTimes = FALSE)

print("Merging test and training data into one dataset")
## Generate "features" list, which will provide labels to our measures
features<-read.table("./UCI HAR Dataset/features.txt")

## Take in "test" subjects set
test_subjects<-read.table("./UCI HAR Dataset/test/subject_test.txt",col.names="Subject")

## Take in "test" activities set
test_activities<-read.table("./UCI HAR Dataset/test/Y_test.txt",col.names="Activity")

## Take in "test" measure values and assign "features" list to column names
test_set<-read.table("./UCI HAR Dataset/test/X_test.txt")
colnames(test_set)<-features[,2]
## Create "Original" label for data imported from "test" folder
test_labels<-as.data.frame(rep("Test",nrow(test_set)))
colnames(test_labels)<-"Original"

## Combine "test" data above into a dataset called "test_full"
test_full<-cbind(test_labels,test_subjects,test_activities,test_set)

## Take in "train" subjects set
train_subjects<-read.table("./UCI HAR Dataset/train/subject_train.txt",col.names="Subject")

## Take in "train" activities set
train_activities<-read.table("./UCI HAR Dataset/train/Y_train.txt",col.names="Activity")

## Take in "train" measure values and assign "features" list to column names
train_set<-read.table("./UCI HAR Dataset/train/X_train.txt")
colnames(train_set)<-features[,2]

## Create "Original" label for data imported from "train" folder
train_labels<-as.data.frame(rep("Train",nrow(train_set)))
colnames(train_labels)<-"Original"

## Combine "train" data above into a dataset called "train_full"
train_full<-cbind(train_labels,train_subjects,train_activities,train_set)

## Combine "test_full" and "train_full" datasets
full_dataset<-rbind(test_full,train_full)

## Remove previously created dataset components
rm(list=c("features","test_activities","test_full","test_labels","test_set","test_subjects","train_activities","train_full","train_labels","train_set","train_subjects"))

# Create a smaller dataset containing only the mean and std variables
print("Extracting mean and std observations from the rest of the dataset")
search <- grep("-mean|-std", colnames(full_dataset))
data_mean_std <- full_dataset[,c(1,2,3,search)]

## Take in activity labels to assign to "Activity" column
print("Replacing the activity numeric value with labels")
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")[2]

## Convert Activity column to character so that it can take in the new labels
data_mean_std$Activity<-as.character(data_mean_std$Activity)

## For each activity type found in "data_mean_std", replace the numeric value with its associated label
for (x in 1:nrow(activity_labels)) {
  label<-as.character(activity_labels[x,1])
  data_mean_std[data_mean_std$Activity==x,]$Activity<-label
}

print("Computing the means for each 'Activity' and 'Subject' and make a new dataset with the values")

## Create a new "full_means" dataset 
full_means<-data.frame(Type="")
full_means<-full_means[-1,]

## Apply mean function across each measure in “data_mean_std” for each activity and then each subject
## Save to "full_means" and apply associated column name
for (x in 4:ncol(data_mean_std)) {
  full_means<-cbind(full_means,c(sapply(split(data_mean_std[,x],data_mean_std$Activity),mean),
                                 sapply(split(data_mean_std[,x],data_mean_std$Subject),mean)))
  colnames(full_means)[x-3]<-colnames(data_mean_std)[x]
}

## Capture row names and assign to first column
Type<-rownames(full_means)
full_means<-cbind(Type,full_means)

# Save the resulting dataset
print("Writing the new tidy dataset to file 'tidy_data.txt' to the working directory")
write.table(full_means, file="tidy_data.txt", row.name=FALSE)

print("Done!")
