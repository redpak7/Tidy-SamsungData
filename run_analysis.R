## loading reshape2 package

library(reshape2)


## 1st Task: Merges the training and the test sets to create one data set

# firstly we need to read the training data to respective dataframes

subject_train <- read.table("train/subject_train.txt")
xtrain<-read.table("train/X_train.txt")
ytrain<-read.table("train/y_train.txt")

# secondly we need to read the test data to respective dataframes

subject_test <- read.table("test/subject_test.txt")
xtest<-read.table("test/X_test.txt")
ytest<-read.table("test/y_test.txt")

# thirdly we combine the training and test tests

xmerged<-rbind(xtrain,xtest)
ymerged<-rbind(ytrain,ytest)
subject_merged<-rbind(subject_train,subject_test)
names(subject_merged)<-"ID"

# we also read the feature file onto a dataframe

data_features<-read.table("features.txt")

#apply the features to the merged x files

names(xmerged)<-data_features$V2

#also name the activities in merged y file

names(ymerged)<-"Activity"

# finally we merge all data sets to one file

combineddata<-cbind(subject_merged,ymerged,xmerged)


## 2nd Task: Extracts only the measurements on the mean and standard deviation for each measurement.

# firstly we have to find out which columns contain "mean()" or "std()"

relevantcols <- grepl("mean\\(\\)", names(combineddata)) |
  grepl("std\\(\\)", names(combineddata))

# Secondly we need to remove the columns that dont have the mean or std data

mean_std_data <- combineddata[, relevantcols]


# Thirdly we need to make sure that the ID of subject and Activity data is being recorded

colstokeep<-combineddata[,1:2]

# Fourthly we combined the subject and activity with the mean and standard deviation data

final_data<-cbind(colstokeep,mean_std_data)

## 3rd Task: Uses descriptive activity names to name the activities in the data set.

# i read the activiity names into a table and convert them into a factor

activity_names<-read.table("activity_labels.txt")
anames<-as.factor(activity_names$V2)

# then i assign those factors to the data 

final_data$Activity<-factor(final_data$Activity, levels = 1:6, labels = anames)

## 4th Task:Appropriately labels the data set with descriptive variable names

# Change t to Time, f to Frequency, mean() to Mean and std() to StdDev
# Remove extra dashes and BodyBody naming error from original feature names

names(final_data) <- gsub("-mean\\(\\)", "-Mean", names(final_data))
names(final_data) <- gsub("-std\\(\\)", "-StdDev", names(final_data))
names(final_data) <- gsub("^t", "Time-", names(final_data))
names(final_data) <- gsub("^f", "Frequency-", names(final_data))
names(final_data) <- gsub("BodyBody", "Body", names(final_data))


## 5th Task: Create a second, independent tidy data set with the average of each variable for each activity and each subject.

# create the tidy data set
tidymelted<- melt(final_data, id=c("ID","Activity"))
tidydata <- dcast(tidymelted, ID + Activity ~ variable, mean)

# Finally write the data to a CSV file
write.table(tidydata, "Tidy-Data.txt", row.names=FALSE)