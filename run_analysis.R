# loads tidy train or test data set
loadDataSet <- function(xFile, yFile) {
    # load list of all features from features.txt
    featureNames <- as.character(read.table("features.txt", stringsAsFactors = F)[, 2])
    
    # we want to load only std and mean features so filter out other
    featuresToLoad <- grepl("std", featureNames) | grepl("mean", featureNames)
    colClassesToLoad <- rep("NULL", times = length(featureNames))
    colClassesToLoad[featuresToLoad] <- "numeric"
    
    # load measurements data
    data <- read.table(xFile, colClasses = colClassesToLoad)
    # set column names
    names(data) <- featureNames[featuresToLoad]
    
    # load activity labels from activity_labels.txt
    activityLabels <- read.table("activity_labels.txt", col.names = c("ActivityLabelIndex", "ActivityLabel"))
    
    # load activities
    activities <- read.table(yFile, stringsAsFactors = T
                                  , colClasses = c("numeric"), col.names = c("ActivityLabelIndex"))
        
    # append activities to data set
    activities <- merge(activities, activityLabels, by = "ActivityLabelIndex")[, 2]
    data$Activity <- activities
    
    # return tidt data set
    data
}

# load and merge tidy train and tidy test data sets
data <- rbind(loadDataSet("train\\X_train.txt", "train\\y_train.txt"), 
               loadDataSet("test\\X_test.txt", "test\\y_test.txt"))

# save tidy data set to tidy.txt
write.table(data, "tidy.txt", row.names = F, quote = F)