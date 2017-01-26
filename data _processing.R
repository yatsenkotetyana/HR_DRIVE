# Install packages if we need it 
if (!require("corrplot")) install.packages("corrplot")
if (!require("ggplot2"))  install.packages("ggplot2")
if (!require("dplyr"))    install.packages("dplyr")
if (!require("stringr"))  install.packages("stringr")
if (!require("tidyr"))    install.packages("tidyr")
if (!require("gridExtra"))install.packages("gridExtra")
if (!require("caret"))    install.packages("caret")

# Load librarys
library (corrplot)
library (ggplot2)
library (dplyr)
library (stringr)
library (tidyr)
library (gridExtra)
library (caret)

# Clear global environment
rm(list = ls())

# Set up current dir
setwd("data_Q3_2016")
# Receving names of file in dir "data_Q3_2016"
data_files_Names <- list.files(pattern="*.csv")
# Clear data
dataset <- NULL
# Download only first 50 raws from each file for preliminary analysis because
for(file_Name in data_files_Names)  # Read next file
  dataset <- rbind(dataset, read.csv(file_Name, nrows = 50))

# function from cater package gives information and recommendation about each raws 
# Help: nearZeroVar diagnoses predictors that have one unique value 
# (i.e. are zero variance predictors)
# or predictors that are have both of the following characteristics: 
# they have very few unique values relative to the number of samples and the ratio of the 
# frequency of the most common value to the frequency of the second most common value is large.
nearZeroVar(dataset, saveMetrics = TRUE)

# Based on information from previous funtion and wiki article https://ru.wikipedia.org/wiki/S.M.A.R.T. following set is chosen
chosenRawNames <- c("date", "serial_number",
                    "model", "capacity_bytes", "failure", 
                    "smart_1_normalized",         # Raw Read Error Rate
                    "smart_3_normalized",         # Spin-Up Time
                    "smart_7_normalized",         # Seek Error Rate
                    "smart_9_raw",                # Power-on Time Count (Power-On Hours)
                    "smart_12_raw",               # Device Power Cycle Count
                    "smart_190_raw"               # Airflow Temperature (WDC)
)

# Merge all needed data
dataset <- NULL
for(file_Name in data_files_Names){
  dataTmp <- read.csv(file_Name)
  dataset <- rbind(dataset, dataTmp[,chosenRawNames])
}
# Remove dataTmp
remove(dataTmp)

# Check nearZeroVar for whole table
nearZeroVar(dataset, saveMetrics = TRUE)

setwd("data_Q3_2016")
# Save processed dataset for easy access 
write.csv(dataset,"hard_drive_choose.csv",quote=FALSE,row.names=FALSE)
# Remove dataset
remove(dataset)

setwd("data_Q3_2016")
# Load processed dataset for easy access 
dataset <- read.csv("hard_drive_choose.csv")  #
View(dataset)
summary(dataset)
str(dataset)

# Spliting firm and model number
# Inserted blank between firm name and model number where it neede
# str_detect detect pattern in a string
# str_replace replace pattern in a string by given string
# paste0 merge strings without symbol brtwenn them
# str_extract extract symbol from a string after pattern
dataset$FMN<-ifelse( (str_detect(dataset$model, pattern=" ")==FALSE),
                     str_replace(dataset$model,  pattern = "[0-9]", 
                                 paste0(" ",str_extract(dataset$model, pattern = "[0-9]"))),
                     str_replace(dataset$model,  pattern = " "," ") )

# Separate column on several colums (data in column separated by pattern)
dataset <- dataset %>% 
  separate(col=FMN,into =c("firm","model_number"),sep=" ",remove = TRUE)


setwd("data_Q3_2016")
# Save splitted dataset for easy access 
write.csv(dataset,"hard_drive_choose_split.csv",quote=FALSE,row.names=FALSE)
# Remove dataset
remove(dataset)
