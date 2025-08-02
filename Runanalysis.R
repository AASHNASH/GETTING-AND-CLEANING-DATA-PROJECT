# Load required packages
library(dplyr)
library(reshape2)
library(data.table)

# Step 1: Download and unzip the dataset if not already present
filename <- "getdata_projectfiles_UCI HAR Dataset.zip"
dataset_folder <- "UCI HAR Dataset"

if (!file.exists(dataset_folder)) {
  if (!file.exists(filename)) {
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, destfile = filename, method = "curl")
  }
  unzip(filename)
}

# Step 2: Read data files
activityLabels <- read.table(file.path(dataset_folder, "activity_labels.txt"), col.names = c("classLabels", "activityName"))
features <- read.table(file.path(dataset_folder, "features.txt"), col.names = c("index", "featureName"))

# Extract only mean and std features
featuresWanted <- grep("mean\\(\\)|std\\(\\)", features$featureName)
measurements <- features[featuresWanted, "featureName"]

# Clean feature names
measurements <- gsub("[()]", "", measurements)
measurements <- gsub("-mean", "Mean", measurements)
measurements <- gsub("-std", "STD", measurements)
measurements <- gsub("-", "", measurements)

# Load train datasets
x_train <- read.table(file.path(dataset_folder, "train", "X_train.txt"))[, featuresWanted]
colnames(x_train) <- measurements
y_train <- read.table(file.path(dataset_folder, "train", "y_train.txt"), col.names = "Activity")
subject_train <- read.table(file.path(dataset_folder, "train", "subject_train.txt"), col.names = "SubjectNum")
train <- cbind(subject_train, y_train, x_train)

# Load test datasets
x_test <- read.table(file.path(dataset_folder, "test", "X_test.txt"))[, featuresWanted]
colnames(x_test) <- measurements
y_test <- read.table(file.path(dataset_folder, "test", "y_test.txt"), col.names = "Activity")
subject_test <- read.table(file.path(dataset_folder, "test", "subject_test.txt"), col.names = "SubjectNum")
test <- cbind(subject_test, y_test, x_test)

# Merge train and test data
combined <- rbind(train, test)

# Apply descriptive activity names
combined[["Activity"]] <- factor(combined[["Activity"]],
                                 levels = activityLabels[["classLabels"]],
                                 labels = activityLabels[["activityName"]])

# Convert SubjectNum to factor
combined[["SubjectNum"]] <- as.factor(combined[["SubjectNum"]])

# Melt and cast to tidy format
combined <- melt(data = combined, id = c("SubjectNum", "Activity"))
combined <- dcast(data = combined, SubjectNum + Activity ~ variable, fun.aggregate = mean)

# Write tidy dataset to file
write.table(combined, "tidyData.txt", row. Names = FALSE)
