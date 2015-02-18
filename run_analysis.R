# loads tidy train or test data set
loadDataSet <- function(xFile, yFile) {
    # load list of all features
    featureNames <- as.character(read.table("features.txt", stringsAsFactors = F)[, 2])
    
    # we want to load only std and mean features
    featuresToLoad <- grepl("std", featureNames) | grepl("mean", featureNames)
    colClassesToLoad <- rep("NULL", times = length(featureNames))
    colClassesToLoad[featuresToLoad] <- "numeric"
    
    # load data set
    colNames <- gsub("[\\(|)]", featureNames, replacement = "")
    colNames <- gsub("-", featureNames, replacement = "_")
    
    data <- read.table(xFile, colClasses = colClassesToLoad, 
                        col.names = colNames)
    names(data) <- featureNames[featuresToLoad]
    
    # load activity labels
    activityLabels <- read.table("activity_labels.txt", col.names = c("ActivityLabelIndex", "ActivityLabel"))
    
    # load activities
    trainActivities <- read.table(yFile, stringsAsFactors = T
                                  , colClasses = c("numeric"), col.names = c("ActivityLabelIndex"))
    trainActivities <- merge(trainActivities, activityLabels, by = "ActivityLabelIndex")[, 2]
        
    # append activities to data set
    data$Activity <- trainActivities
    data
}

# load and merge tidy train and test data sets
data <- rbind(loadDataSet("train\\X_train.txt", "train\\y_train.txt"), 
               loadDataSet("test\\X_test.txt", "test\\y_test.txt"))

# save tidy data set to tidy.txt
write.table(data, "tidy.txt", row.names = F)