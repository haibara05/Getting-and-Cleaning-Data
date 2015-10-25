loadData <- function (type) {
        folder <- "UCI HAR Dataset/"
        filetype <- ".txt"
        
        subjects <- read.table(paste(folder, type, "/subject_", type, filetype, sep= ""))
        y <- read.table(paste(folder, type, "/y_", type, filetype, sep= ""))
        
        featuresName <- read.table(paste(folder, "features", filetype, sep= ""))
        x <- read.table(paste(folder, type, "/X_", type, filetype, sep= ""))
        
        #put header of the feature value with features name
        names(x) <- featuresName[, 2]
        
        #put header of the activity value and subject with proper name
        colnames(subjects) <- "subject"
        colnames(y) <- "activity"
        
        #combine the y, subject and x together
        df <- cbind(cbind(subjects, y), x)
        return(df)
}
test <- loadData("test")
train <- loadData("train")

fulData <- rbind(train, test)

##replace the activity with corresponding activity name
activityNames <- read.table("UCI HAR Dataset/activity_labels.txt")
fulData$activity <- sapply (fulData$activity, function(x)activityNames[x, 2])

##keep features that represents for mean and standard deviation (std)
featuresNames <- read.table("UCI HAR Dataset/features.txt")
colnames(featuresNames) <- c("fId", "featurename")
keepFeatures <- subset(featuresNames, grepl("mean|std", featurename))[, 2]

##select the features 
preData <- subset(fulData, select = c(cbind("subject", "activity"), 
                                      as.character(keepFeatures)))

##Create dataframe with identity vaible "subject" and "activity"
library(reshape)
tidyMelted <- melt(preData, id = c("subject", "activity"))

##create tidy data with measurements with respect to each subject and activities
tidy <- cast(tidyMelted, subject + activity ~ variable, mean)
dim(tidy)

summary(tidy)

##output tidy as tidy.txt and tidy.csv
write.table(tidy, "tidy.txt", row.names = FALSE)
write.csv(tidy, "tidy.csv")
