###    Getting and Cleaning Data Course Project

setwd("/Users/christophergadek/Desktop/Data Science/Coursera/Getting_and_Cleaning_Data/UCI_HAR_Dataset/CourseProject")
library(plyr)

# Assign files and their respective directories

uciDirectory <- "/Users/christophergadek/Desktop/Data Science/Coursera/Getting_and_Cleaning_Data/UCI_HAR_Dataset/CourseProject"
xTestFile <- paste(uciDirectory, "/test/X_test.txt", sep = "")
yTestFile <- paste(uciDirectory, "/test/Y_test.txt", sep = "")
xTrainFile <- paste(uciDirectory, "/train/X_train.txt", sep = "")
yTrainFile <- paste(uciDirectory, "/train/Y_train.txt", sep = "")
subjectTestFile <- paste(uciDirectory, "/test/subject_test.txt", sep = "")
subjectTrainFile <- paste(uciDirectory, "/train/subject_train.txt", sep = "")
featureDataFile <- paste(uciDirectory, "/features.txt", sep = "")
activityLabelDataFile <- paste(uciDirectory, "/activity_labels.txt", sep = "")

# Read in the raw data

xTest <- read.table(xTestFile)
yTest <- read.table(yTestFile)
xTrain <- read.table(xTrainFile)
yTrain <- read.table(yTrainFile)
subjectTest <- read.table(subjectTestFile)
subjectTrain <- read.table(subjectTrainFile)
featureData <- read.table(featureDataFile, colClasses = c("character"))
activityLabelData <- read.table(activityLabelDataFile, col.names = c("ActivityId","ActivityName"))

### Project Script Parameters (should do the following:)
                                
## 1 - Merges the training and the test sets to create one data set.

# Merge into one dataset
trainingSensorData <- cbind(cbind(xTrain, subjectTrain), yTrain)
testSensorData <- cbind(cbind(xTest, subjectTest), yTest)
mergedSensorData <- rbind(trainingSensorData, testSensorData)

# Assign labels to the merged dataset

sensorLabels <- rbind(rbind(featureData, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(mergedSensorData) <- sensorLabels

## 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 

measurements <- mergedSensorData[,grepl("mean|std|Subject|ActivityId", names(mergedSensorData))]


## 3 - Uses descriptive activity names to name the activities in the data set

measurements <- join(measurements, activityLabelData, by = "ActivityId", match = "first")
measurements <- measurements[,-1]

head(measurements)

## 4 - Appropriately labels the data set with descriptive variable names. 

#     Clean up labels

names(measurements) <- gsub('\\(|\\)',"",names(measurements))
names(measurements) <- make.names(names(measurements))

#     Expand abbreviated labels

names(measurements) <- gsub('\\.mean',".Mean",names(measurements))
names(measurements) <- gsub('\\.std',".StandardDeviation",names(measurements))
names(measurements) <- gsub('Freq\\.',"Frequency.",names(measurements))
names(measurements) <- gsub('Freq$',"Frequency",names(measurements))
names(measurements) <- gsub('^t',"TimeDomain.",names(measurements))
names(measurements) <- gsub('^f',"FrequencyDomain.",names(measurements))
names(measurements) <- gsub('Mag',"Magnitude",names(measurements))
names(measurements) <- gsub('GyroJerk',"AngularAcceleration",names(measurements))
names(measurements) <- gsub('Gyro',"AngularSpeed",names(measurements))
names(measurements) <- gsub('Acc',"Acceleration",names(measurements))

#     Validate changes

names(measurements)


## 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidyDataSet <- ddply(measurements, c("Subject","ActivityName"), numcolwise(mean))
write.table(tidyDataSet, row.names = FALSE, file = "tidyDataSet.txt")
