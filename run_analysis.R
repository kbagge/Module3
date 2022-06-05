# Load some packages
library(tidyr)
library(dplyr)
setwd("~/Projects/R/R_harward/Modul 3/Assignment Modul3/")

## Define the files we want to load
## What information are hidden in what files are not very well documented.
## The label files are in the main folder.
## In the train and test folders there are the following:
## - subject file :: contains personal ID for the individuals
## - y file :: contains activity code for the record
## - X file :: contains the rest of the data
# Label files
NamesFile <- "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt"
ActivitiesFile <- "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt"

# Test files
TestFile <- "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt"
TestIDFile <- "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt"
TestActivityFile <- "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt"

# Training files
TrainFile <- "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train//X_train.txt"
TrainIDFile <- "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt"
TrainActivityFile <- "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt"

## Load the files, be aware that they have no header
# Label files
names <- read.table(file = NamesFile, header = FALSE)
activities <- read.table(file = ActivitiesFile, header = FALSE)

# Test files - we load the appropiate variable names directly here with the col.names argument
TestSet <- read.table(file = TestFile, col.names = names$V2, header = FALSE)
TestID <- read.table(file = TestIDFile, header = FALSE)
TestActivity <- read.table(file = TestActivityFile, header = FALSE)

# Training files - we load the appropiate variable names directly here with the col.names argument
TrainSet <- read.table(file = TrainFile, col.names = names$V2, header = FALSE)
TrainID <- read.table(file = TrainIDFile, header = FALSE)
TrainActivity <- read.table(file = TrainActivityFile, header = FALSE)

# Find the appropiate coloumns for the test and training data sets containing information on mean or standard deviation
LogicalVector <- names$V2 %>% grepl(pattern = "(mean|std)")

# Merge the data, ID and activity for Test persons
TestSet <- TestSet[, LogicalVector] # Only keep the coloumns with mean or std
TestID <- TestID %>% rename(ID = V1) # Give proper name to variable
TestActivity <- rename(TestActivity, Activity = V1) # Give proper name to variable
TestMerged <- cbind(TestSet, TestID, TestActivity)

# Merge the data, ID and activity for Train persons
TrainSet <- TrainSet[, LogicalVector] # Only keep the coloumns with mean or std
TrainID <- TrainID %>% rename(ID = V1) # Give proper name to variable
TrainActivity <- rename(TrainActivity, Activity = V1) # Give proper name to variable
TrainMerged <- cbind(TrainSet, TrainID, TrainActivity)

# Bind together the training and test dataset
TotalSet <- rbind(TrainMerged,TestMerged)

# Make a function that changes activity value to text for all observations
activity_func <- function(df) {
  count <- c(1)
  l <- length(df$Activity)
  for(i in df$Activity){ # Loop over all obervations from the activity coloumn
    j <- df$Activity[count]
    k <- activities[j,2] # For each of them find the corresponding text value
    df$Activity[count] <- k  # And insert it instead of the number value
    count <- count+1
  }
  return(df)
}
# Run the function on our dataset
TotalSet <- activity_func(TotalSet)

# We have now:
# 1. Merged the training and test dataset to create one.
# 2. Extracted the measuremnt on mean and standard deviation
# 3. Changed numbers to descriptive names in the activity coloumn
# 4. Labeled the variables appropiatle, this was done when we read the data.
# Now we just needs to:
# 5. Create a second tidy dataset with the average of each variable for each activity and person
NewDF <- TotalSet %>% group_by(ID, Activity) %>% summarise_all(mean)
# Print it to the console to confirm that it has 180 rows corresponding to 6 activites for 30 persons

# And save it to a file
write.table(NewDF, file = "./MyTidyDataSet.csv")
