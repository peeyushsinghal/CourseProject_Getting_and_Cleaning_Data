
## Coursera Getting and Cleaning Data Course Project
## Peeyush Singhal
## run_analysis.R  does the following. 

## 1.Merges the training and the test sets to create one data set.
## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3.Uses descriptive activity names to name the activities in the data set
## 4.Appropriately labels the data set with descriptive variable names. 
## 5.From the data set in step 4, it creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#########################

## including library plyr for the functions used later
library(plyr)

## 1.Merges the training and the test sets to create one data set.

## from training data set (y= activity, x = feature)
xtrain <- read.table("./data/Dataset/train/X_train.txt")
ytrain <- read.table("./data/Dataset/train/y_train.txt")

## from test data set (y= activity, x = feature)
xtest <- read.table("./data/Dataset/test/X_test.txt")
ytest <- read.table("./data/Dataset/test/y_test.txt")

## subject data 
subjecttest<-read.table("./data/Dataset/test/subject_test.txt")
subjecttrain<-read.table("./data/Dataset/train/subject_train.txt")


## joining training and test data (y = activity, x = feature)
xdata <- rbind(xtrain,xtest)
ydata <- rbind(ytrain,ytest)
subjectdata<-rbind(subjecttrain,subjecttest)

## getting variable (column) names
names (ydata) <- c("activity")
names (subjectdata) <- c("subject")
xdatanames <- read.table("./data/Dataset/features.txt")
names(xdata)<- xdatanames$V2

## combining all data
Data <- cbind(ydata,subjectdata,xdata)


##-------------------##
## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 

## getting the columns which contain the mean and std deviation for each measurements
subset_xdatanames<-xdatanames$V2[grep("mean\\(\\)|std\\(\\)", xdatanames$V2)]


## subsetting Data for desired columns
subset_Colnames <- c(as.character(subset_xdatanames), "subject", "activity" )
subset_Data <- Data[,subset_Colnames]

##-------------------##
## 3.Uses descriptive activity names to name the activities in the data set

## reading activity labels
activity_labels <-read.table("./data/Dataset/activity_labels.txt")

## assigning to the y data (y=activity) & to subset data
ydata[, 1] <- activity_labels[ydata[, 1], 2]
subset_Data$activity <- activity_labels[subset_Data$activity,2]
##-------------------##

## 4.Appropriately labels the data set with descriptive variable names. 


for (i in 1:length(subset_Colnames)) 
{
  subset_Colnames[i] = gsub("\\()","",subset_Colnames[i])
  subset_Colnames[i] = gsub("-std$","StdDev",subset_Colnames[i])
  subset_Colnames[i] = gsub("-mean","Mean",subset_Colnames[i])
  subset_Colnames[i] = gsub("^(t)","time",subset_Colnames[i])
  subset_Colnames[i] = gsub("^(f)","freq",subset_Colnames[i])
  subset_Colnames[i] = gsub("([Gg]ravity)","Gravity",subset_Colnames[i])
  subset_Colnames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",subset_Colnames[i])
  subset_Colnames[i] = gsub("[Gg]yro","Gyro",subset_Colnames[i])
  subset_Colnames[i] = gsub("AccMag","AccMagnitude",subset_Colnames[i])
  subset_Colnames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",subset_Colnames[i])
  subset_Colnames[i] = gsub("JerkMag","JerkMagnitude",subset_Colnames[i])
  subset_Colnames[i] = gsub("GyroMag","GyroMagnitude",subset_Colnames[i])
};

colnames(subset_Data) <- subset_Colnames;
##-------------------##
## 5.From the data set in step 4, it creates a second, independent tidy data set with the average of each variable for each activity and each subject.

data_average <-  ddply(subset_Data, .(subject, activity), function(x) colMeans(x[, 1:66]))
## 67 = subject, 68 = activity, we intend  to remove those hence columeans is for first 66 columns

write.table(data_average, './data/Dataset/data_average.txt',row.names=FALSE,sep='\t')
##-------------------##

