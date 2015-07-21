

## set work dir & load library
setwd("C:/Users/masak_000/SkyDrive/Documents/cousera/getdata/Project")
library(dplyr)

##### 1.Merges the training and the test sets to create one data set.

##Read test

## features data
features<-read.table("features.txt", header=FALSE) 


## test data
x_test<-read.table("./test/X_test.txt", header=FALSE) 
y_test<-read.table("./test/y_test.txt", header=FALSE)
subject_test<-read.table("./test/subject_test.txt", header=FALSE)
names(y_test) <- c("no")
names(x_test)<-features$V2
names(subject_test) <- c("subject")


## train data
x_train<-read.table("./train/X_train.txt", header=FALSE) 
y_train<-read.table("./train/y_train.txt", header=FALSE)
subject_train<-read.table("./train/subject_train.txt", header=FALSE)
names(y_train) <- c("no")
names(x_train)<-features$V2
names(subject_train) <- c("subject")

## marge
test<-cbind(subject_test, y_test, x_test)
train<-cbind(subject_train, y_train, x_train)


# normalize for data
#Error in fix.by(by.x, x) : 'by' must specify uniquely valid columns

train <- train[, unique(colnames(train))]
test  <- test[, unique(colnames(test))]
#train <- mutate(train, orgdata="train")
#test  <- mutate(test, orgdata="test")

data <- rbind(test, train)


## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 

mean_sd<-select(data, subject, no, contains("mean"), contains("std"))


## 3.Uses descriptive activity names to name the activities in the data set

activity_labels<-read.table("activity_labels.txt", header=FALSE) 
mean_sd$no<-factor(mean_sd$no, levels=activity_labels$V1,labels=activity_labels$V2)

## 4.Appropriately labels the data set with descriptive variable names. 

# Subject
# Original
# Activity

# According to "features_info.txt"....
# tBody -> time-Body
# tGravity ->	time-Gravity
# acc -> acceleration
# fBody -> FastFourierTransform-Body
# Mag	  ->    Magnitude
# Gyro	->    Gyroscope

namelist<- sub("subject", "Subject", names(mean_sd), ignore.case = FALSE )
#namelist<- sub("orgdata", "Original", namelist, ignore.case = FALSE )
namelist<- sub("no", "Activity", namelist, ignore.case = FALSE )

namelist<- sub("tBody", "time-Body", namelist, ignore.case = FALSE )
namelist<- gsub("tGravity", "time-Gravity", namelist, ignore.case = FALSE )
namelist<- gsub("acc", "Acceleration", namelist, ignore.case = FALSE )
namelist<- gsub("fBody", "FastFourierTransform-Body", namelist, ignore.case = FALSE )
namelist<- gsub("Mag", "Magnitude", namelist, ignore.case = FALSE )
namelist<- gsub("Gyro", "Gyroscope", namelist, ignore.case = FALSE )
namelist<- gsub("tGravity", "time-Gravity", namelist, ignore.case = FALSE )

names(mean_sd) <- namelist


## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

average<-mean_sd  %>%
  group_by(Subject, Activity) %>%
  summarise_each(funs(mean))

## Write data to file

write.table(average, file="average.txt", row.names=FALSE, col.names=TRUE)

