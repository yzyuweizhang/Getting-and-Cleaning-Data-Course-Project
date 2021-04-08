#Read in Data 
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")  

#Merge Training and Test Set 
X_test_train <- rbind(X_test, X_train) #features 
Y_test_train <- rbind(y_test, y_train) # activity 
Subject_test_train <- rbind(subject_test, subject_train) # subject 

#Naming the dataset 
colnames(X_test_train) <- t(features[2])
colnames(Y_test_train) <- "Activity"
colnames(Subject_test_train) <- "Subject"

Data <- cbind(X_test_train, Y_test_train, Subject_test_train) 

MeanSTD <- grep("[Mm]ean|[Ss]td", names(Data), ignore.case=TRUE) #Column no. with Std or Mean 
AddSubjectAcivity<- c(MeanSTD, 562, 563)
MeanSTDTable <- Data[,AddSubjectAcivity]

#Assinging Name to Activity 
MeanSTDTable$Activity <- activity_labels[MeanSTDTable$Activity, 2]

#Reformating Names 
names(MeanSTDTable) <- gsub("Acc", "Accelerometer", names(MeanSTDTable), ignore.case = TRUE)
names(MeanSTDTable) <- gsub("Gyro", "Gyroscope", names(MeanSTDTable), ignore.case = TRUE)
names(MeanSTDTable) <- gsub("BodyBody", "Body", names(MeanSTDTable), ignore.case = TRUE)
names(MeanSTDTable) <- gsub("Mag", "Magnitude", names(MeanSTDTable), ignore.case = TRUE)
names(MeanSTDTable) <- gsub("^tBody", "TimeBody", names(MeanSTDTable), ignore.case = TRUE)
names(MeanSTDTable) <- gsub("-mean()", "_Mean", names(MeanSTDTable), ignore.case = TRUE)
names(MeanSTDTable) <- gsub("-std()", "_STD", names(MeanSTDTable), ignore.case = TRUE)
names(MeanSTDTable) <- gsub("-freq()", "Frequency", names(MeanSTDTable), ignore.case = TRUE)
names(MeanSTDTable) <- gsub("angle", "Angle", names(MeanSTDTable), ignore.case = TRUE)
names(MeanSTDTable) <- gsub("gravity", "Gravity", names(MeanSTDTable), ignore.case = TRUE)

#Average of Subjects and Activities 
FinalData <- MeanSTDTable %>%
  group_by(Subject, Activity)%>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)
