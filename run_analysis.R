# run_analysis.R
# Cousera - Getting and Cleaning Data - Assignment
# Rainer Koelle - version 1.0 - 18.12.2014 (course retake)

######################################################################################
# configuration
## path to project - adapt to your local settings
projectPath <- "~/__PERFORMANCE/Courses/GettingAndCleaningData/assignment/"


#######################################################################################
# 1. Merge the training and the test sets to create one data set.

## read in x, y, and subject id from training and test sets
xTrain <- read.table(file.path(projectPath, "UCIHARDataset/train/X_train.txt"))
xTest <- read.table(file.path(projectPath, "UCIHARDataset/test/X_test.txt"))

yTrain <- read.table(file.path(projectPath, "UCIHARDataset/train/y_train.txt"))
yTest <- read.table(file.path(projectPath, "UCIHARDataset/test/y_test.txt"))

subjectTrain <- read.table(file.path(projectPath, "UCIHARDataset/train/subject_train.txt"))
subjectTest <- read.table(file.path(projectPath, "UCIHARDataset/test/subject_test.txt"))


## merge subsets - perform row-bind
## x/y/subject train set: row 1 .. 7352 - x/y/subject test: rows 7353 .. 10299
xData <- rbind(xTrain, xTest)
yData <- rbind(yTrain, yTest)
sData <- rbind(subjectTrain, subjectTest)


#######################################################################################
# 2. Extract only the measurements on the mean and standard deviation
# for each measurement. 

## read-in features.txt 
feature <- read.table(file.path(projectPath, "UCIHARDataset/features.txt"))

## regular expression filter to extract mean and standard deviation
## terms from 2nd column of feature (= feature names)
filter <- grep("-mean\\(\\)|-std\\(\\)", feature[,2])

## subset feature dataframe
feature <- feature[filter,]

## subset x data frame to include mean and std measurements
## first column of feature contains xData variable indices
xData <- xData[,feature[,1]]


######################################################################################
# 3. descriptive activity names to name the activities in the data set

## read in activity labels
activityLabels <- read.table(file.path(projectPath,"UCIHARDataset/activity_labels.txt"))

## pretty names; lower case for activity labels and remove "_"
activityLabels[,2] <- tolower(activityLabels[,2])
activityLabels[,2] <- gsub("_", " ", activityLabels[,2])

## label activities - assign activity label per row value of yData
yData$activity <- activityLabels[yData[,1], 2]


#####################################################################################
# 4. Appropriately labels the data set with descriptive activity (??) names.
# ?? some confustion about naming / assignment text
# the following renames the colums and binds the 'tidy' data set

## label columns in xData with appropriate name
names(xData) <- feature[,2]

## label colum of sData with "subject"
names(sData) <- "subject"

## subset yData (label already assigned)
yData <- data.frame( yData[,"activity"] )
names(yData) <- "activity"

## create tidy data set, colum bind 
tidy <- cbind(sData, yData, xData )

## check tidy set completeness
if(! all(colSums(is.na(tidy))==0)) {print("data set: not well formed")}

## write tidy data set
write.table(tidy, file.path(projectPath, "tidyUCHARDataset.txt"))


###################################################################################
# 5. Create a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 

## aggregate tidy data frame into summary per subject and activity
## aggregation function --> mean
tidy2 <- aggregate(tidy[3:68], by = tidy[1:2], mean)

## check tidy2 set completeness
if(! all(colSums(is.na(tidy2))==0)) {print("data set: not well formed")} 

## write tidy2
write.table(tidy2, file.path(projectPath, "tidyUCHARDatasetMeans.txt"), row.names=FALSE)
