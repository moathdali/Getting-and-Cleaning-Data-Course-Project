
# 1. Merge the training and the test sets to create one data set.


# Read in the data from files
features = read.table("features.txt",header=FALSE) 
activityLabels = read.table("activity_labels.txt",header=FALSE) 
subjectTrain = read.table("train/subject_train.txt",header=FALSE) 
xTrain = read.table("train/x_train.txt",header=FALSE) 
yTrain = read.table("train/y_train.txt",header=FALSE)

subjectTest = read.table("./test/subject_test.txt",header=FALSE) 
xTest       = read.table("./test/x_test.txt",header=FALSE)
yTest       = read.table("./test/y_test.txt",header=FALSE)


#assigning column names

colnames(activityLabels) = c("activityId","activityType")
colnames(subjectTrain) = "subjectId"
colnames(xTrain) = features[,2] 
colnames(yTrain) = "activityId"
colnames(subjectTest) = "subjectId"
colnames(xTest) = features[,2] 
colnames(yTest) = "activityId"

#binding each train and test data files into 2 files

trainingData = cbind(yTrain,subjectTrain,xTrain)
testData = cbind(yTest,subjectTest,xTest)

#binding train and test data in one file

finalData = rbind(trainingData,testData)

#assigning column names to final data set

colNames  = colnames(finalData) 


# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

#select features that include mean or std and then subset from binded file

extractedFeatures <- grep("-(mean|std|activity|subject)\\(\\)", features[, 2])
finalData <- finalData[, extractedFeatures]

#colname correction
names(finalData) <- features[extractedFeatures, 2]
newcolNames <- colnames(finalData)

# 3. Use descriptive activity names to name the activities in the data set

finalData = merge(activityLabels,finalData)


# 4. Appropriately label the data set with descriptive activity names. 

for(i in 1: length(finalData$activityType))
{
  if(finalData$activityId[i]==1)
  {finalData$activityType[i]<- "walking"}
  if(finalData$activityId[i]==2)
  {finalData$activityType[i]<- "Walking Upstairs"}
  if(finalData$activityId[i]==3)
  {finalData$activityType[i]<- "Walking Downstairs"}
  if(finalData$activityId[i]==4)
  {finalData$activityType[i]<- "Sitting"}
  if(finalData$activityId[i]==5)
  {finalData$activityType[i]<- "Standing"}
  if(finalData$activityId[i]==6)
  {finalData$activityType[i]<- "Laying"}
}


# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
library(data.table)
tableData <- data.table(finalData)
tidyData <- tableData[, lapply(.SD, mean), by ="activityLabels"]
write.table(tidyData, file = "tidyData.txt", row.names = FALSE)