#create one R script called run_analysis.R that does the following. 
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
run_analysis<-function()
{
  #Check for the plyr package and data.table package
  #install the package and load it
  if(!require(dplR))
  install.packages(dplR)

  
  if(!require(data.table))
    install.packages(data.table)
  
  library(dplR)
  library(data.table)
  

# Load: activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
# Load: data column names
features <- read.table("./UCI HAR Dataset/features.txt")[,2]
##print(features)
# Extract only the measurements on the mean and standard deviation for each measurement.
extract_features <- grepl("mean|std", features)
#print(extract_features)


# Load test data
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

#Assign the names of the columns
names(X_test)<-features

#Extract mean and std features only
ex_test<-X_test[,extract_features]

# Load activity labels
y_test[,2] = activity_labels[y_test[,1]]
names(y_test) = c("Activity_ID", "Activity_Label")
names(subject_test) = "subject"
message("Loaded Test Data")

# Bind data together to get test_data
test_data <- cbind(subject_test, y_test, ex_test)


# Load training data from the X_train.txt and y_train.txt files
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
names(X_train) = features


# Load activity labels for train dataset
y_train[,2] = activity_labels[y_train[,1]]
names(y_train) = c("Activity_ID", "Activity_Label")
names(subject_train) = "subject"

# Extract only the measurements on the mean and standard deviation for each measurement.
ex_train = X_train[,extract_features]

# Bind data to get train_data set
train_data <- cbind(subject_train, y_train, ex_train)
message("Loaded Train Data")

# Merge the Test and training data together by rbind()
all_data<-rbind(test_data,train_data)
message("Combined Test and Train Data")

#create independent tidy data set with the average of each variable for each activity and each subject.
tidy <- ddply(melt(all_data, id.vars=c("subject", "Activity_ID","Activity_Label")), .(subject, Activity_ID, Activity_Label), summarise, MeanSamples=mean(value))
message("Generated Tidy Data")

write.csv(tidy, file = "tidy.txt",row.names = FALSE)
message("tidy.txt file has been created.")
}