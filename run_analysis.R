# verify if the required libraries are present
# and if not, install them
if (!require("data.table")) {
        install.packages("data.table")
}

if (!require("dplyr")) {
        install.packages("dplyr")
}

if (!require("plyr")) {
        install.packages("plyr")
}

# load libraries
library(data.table)
library(plyr)
library(dplyr)

# load global data labels
labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

# rename the columns of activity df
names(labels) <- c("id_activity", "activity")

# add a last column name "analisys", that contains the origin of data (test or train)
features <- rbind(features, data.frame(V1 = 562, V2 = "analisys"))

# opening data files
train_sets <- read.table("UCI HAR Dataset/train/X_train.txt")
test_sets <- read.table("UCI HAR Dataset/test/X_test.txt")

# opening label files
train_labels <- read.table("UCI HAR Dataset/train/y_train.txt")
test_labels <- read.table("UCI HAR Dataset/test/y_test.txt")

# opening subject data files
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")

# join the labels of train and test
unif_labels <- rbind(train_labels, test_labels)

# rename the column name of train_labels
names(unif_labels) <- c("id_activity")

# join the subjects from train and test
unif_subjects <- rbind(train_subject, test_subject)

# rename the column name subjects
names(unif_subjects) <- c("subject")

# add a column id of data origin
train_sets$analisys <- "train"
test_sets$analisys <- "test"

# join the sets data
unif_sets <- rbind(train_sets, test_sets)

# rename the column name subjects
names(unif_subjects) <- c("subject")

# set the columns labels
names(unif_sets) <- features$V2

# set columns names to lower case
names(unif_sets) <- tolower(names(unif_sets))


## 1 - Merges the training and the test sets to create one data set.
df <- cbind(unif_subjects, unif_labels, unif_sets)


# filter all fields that have mean and std (analisys is my own decision)
filterStdMean <- features$V2[grep("mean\\(|std\\(|*analisys*", features$V2)]

# conver filter to lower case
filterStdMean <- tolower(filterStdMean)


## 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
df <- subset(df, select = c("subject", "id_activity", filterStdMean))

## 3 - Uses descriptive activity names to name the activities in the data set.
df <- merge(labels, df, by = "id_activity", all.x = TRUE)
df$activity <- as.character(df$activity)

# delete old data (id_activity) from df
df$id_activity <- NULL

## 4 - Appropriately labels the data set with descriptive variable names.
names(df) <- gsub("^t", "time", names(df))
names(df) <- gsub("^f", "frequency", names(df))
names(df) <- gsub("-std\\(\\)", "std", names(df))
names(df) <- gsub("-mean\\(\\)", "mean", names(df))
names(df) <- gsub("bodybody", "body", names(df))
names(df) <- gsub("acc", "accelerometer", names(df))
names(df) <- gsub("gyro", "gyroscope", names(df))
names(df) <- gsub("mag", "magnitude", names(df))
names(df) <- gsub("bodybody", "body", names(df))

## 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
df1 = ddply(df, c("subject", "activity"), numcolwise(mean))

# Write the tidy data exit file
write.table(df1, file = "tidy_data.txt", sep = " ", col.names = FALSE)