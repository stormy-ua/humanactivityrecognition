# loads tidy train or test data set
loadDataSet <- function(xTrainFile, yTrainFile) {
    # load list of all features
    featureNames <- as.character(read.table("features.txt", stringsAsFactors = F)[, 2])
    featureNames <- gsub("[\\(|)]", featureNames, replacement = "")
    featureNames <- gsub("-", featureNames, replacement = "_")
    
    # we want to load only std and mean features
    featuresToLoad <- grepl("std", featureNames) | grepl("mean", featureNames)
    colClassesToLoad <- rep("NULL", times = length(featureNames))
    colClassesToLoad[featuresToLoad] <- "numeric"
    
    # load train data set
    train <- read.table(xTrainFile, colClasses = colClassesToLoad, 
                        col.names = featureNames)
    
    # load activity labels
    activityLabels <- read.table("activity_labels.txt", col.names = c("ActivityLabelIndex", "ActivityLabel"))
    
    # load train activities
    trainActivities <- read.table(yTrainFile, stringsAsFactors = T
                                  , colClasses = c("numeric"), col.names = c("ActivityLabelIndex"))
    trainActivities <- merge(trainActivities, activityLabels, by = "ActivityLabelIndex")[, 2]
    
    # append activities to train data set
    train$Activity <- trainActivities
    train
}

# load and merge tidy train and test data sets
data <- rbind(loadDataSet("train\\X_train.txt", "train\\y_train.txt"), 
               loadDataSet("test\\X_test.txt", "test\\y_test.txt"))
write.table(data, "tidy.txt", row.names = F)

# todo: include readme.md