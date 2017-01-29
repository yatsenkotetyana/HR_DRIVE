# Install packages if we need it 
if (!require("corrplot"))   install.packages("corrplot")
if (!require("ggplot2"))    install.packages("ggplot2")
if (!require("dplyr"))      install.packages("dplyr")
if (!require("stringr"))    install.packages("stringr")
if (!require("tidyr"))      install.packages("tidyr")
if (!require("gridExtra"))  install.packages("gridExtra")
if (!require("caret"))      install.packages("caret")
if (!require("data.table")) install.packages("data.table")

# Load librarys
library (corrplot)
library (ggplot2)
library (dplyr)
library (stringr)
library (tidyr)
library (gridExtra)
library (caret)
library(data.table)

# Clear global environment
rm(list = ls())

setwd("data_Q3_2016")
# Load splitted dataset
dataset <- read.csv("hard_drive_choose_split.csv") #,nrow=100000
View(dataset)
summary(dataset)
str(dataset)

# choose throw from databasecolumns 3,4
dataset <-dataset[c(T,T,F,F,T,T,T,T,T,T,T,T,T)]

# make column date as date type
dataset$date             <- as.Date  (dataset$date)
# make column serial_number as character type
dataset$serial_number    <- as.character(dataset$serial_number)

# find the earlest data
min_date         <- as.Date(min(dataset$date))
# find time differents
dataset$time_dif <- dataset$date - min_date

# make column time_dif as integer type
dataset$time_dif <- as.integer(dataset$time_dif)

# View database
View(dataset)

# 1 - failure; 0 - work
type_wf <- 0

# get only serial number for disks which were broken
# 1 step - filter only broken; 2 step - stay only unique
serial       <- dataset %>%
                 filter(failure==type_wf) %>%
                 distinct(serial_number) 

View(serial)

#-------------------------------------------------------

# dataset with all records for  working (broken) disks 
datasetSerial_firm <- dataset  %>%
                      filter(serial_number %in% serial$serial_number)
# View database                      
View(datasetSerial_firm)

# Convert datasetSerial_firm to data table
datasetSerial_firm <- data.table(datasetSerial_firm)

# data.table to aggregate more than one field. This example will calculate 
# the mean ability score and number of attendance days grouped by grade. 
# This is done by adding j=list() with the names of the fields you want 
# to aggregate listed within the parentheses. 
# Mean for each firm and time moment
Agg_data <- as.data.frame(datasetSerial_firm[,
            j=list(smart_1_normalized = mean(smart_1_normalized, na.rm = TRUE), 
                   smart_3_normalized = mean(smart_3_normalized, na.rm = TRUE),
                   smart_7_normalized = mean(smart_7_normalized,na.rm = TRUE),  
                   smart_9_raw        = mean(smart_9_raw,na.rm = TRUE),
                   smart_12_raw       = mean(smart_12_raw,na.rm = TRUE),
                   smart_190_raw      = mean(smart_190_raw,na.rm = TRUE)
                   ),
                   by                 = list(time_dif,firm)]) 

# sd for each firm and time moment            
Agg_data <- as.data.frame(datasetSerial_firm[,
            j=list(smart_1_normalized = sd(smart_1_normalized, na.rm = TRUE), 
                   smart_3_normalized = sd(smart_3_normalized, na.rm = TRUE),
                   smart_7_normalized = sd(smart_7_normalized,na.rm = TRUE),  
                   smart_9_raw        = sd(smart_9_raw,na.rm = TRUE),
                   smart_12_raw       = sd(smart_12_raw,na.rm = TRUE),
                   smart_190_raw      = sd(smart_190_raw,na.rm = TRUE)
                   ),
                   by                 = list(time_dif,firm)]) 

View(Agg_data)

# Convert datasetSerial_firm to data frame
datasetSerial_firm <- data.frame(datasetSerial_firm)

# Form plots
agg1 <-   Agg_data %>%
          ggplot(aes(x=time_dif,y=smart_1_normalized,color=firm))+geom_point(position = "jitter")+geom_smooth()
agg3 <-   Agg_data %>%
          ggplot(aes(x=time_dif,y=smart_3_normalized,color=firm))+geom_point(position = "jitter")+geom_smooth()
agg7 <-   Agg_data %>%
          ggplot(aes(x=time_dif,y=smart_7_normalized,color=firm))+geom_point(position = "jitter")+geom_smooth()
agg9 <-   Agg_data %>%
          ggplot(aes(x=time_dif,y=smart_9_raw,color=firm))+geom_point(position = "jitter")+geom_smooth()
agg12 <-  Agg_data %>%
          ggplot(aes(x=time_dif,y=smart_12_raw,color=firm))+geom_point(position = "jitter")+geom_smooth()
agg190 <- Agg_data %>%
          ggplot(aes(x=time_dif,y=smart_190_raw,color=firm))+geom_point(position = "jitter")+geom_smooth()

# Show plots
grid.arrange(agg1,agg3,agg7,agg9,agg12,agg190, nrow=3, ncol=2)  

#----------------------------------------------------------------------------------------------------

# Firm name
firm_name <-  c("HGST") # c("HGST","Hitachi","ST","TOSHIBA","WDC")
# dataset with all records for broken disks of some firm
datasetSerial_firm <- dataset  %>%
                      filter(serial_number %in% serial$serial_number,firm %in% firm_name)

View(datasetSerial_firm)

# Count how many records are for some firm for all model
# Arrange result
datasetSerial_N_firm <- datasetSerial_firm   %>% 
                        count(model_number) %>% 
                        arrange(desc(n))
View(datasetSerial_N_firm)

# Number of broken disk to look
N_disk_firm <- 5

# Take fist N_disk_rirm model (have more records)
datasetSerial_N_part_firm <- datasetSerial_N_firm[1:N_disk_firm,]

# Convert datasetSerial_firm to data table
datasetSerial_firm <- data.table(datasetSerial_firm)

# data.table to aggregate more than one field. This example will calculate 
# the mean ability score and number of attendance days grouped by grade. 
# This is done by adding j=list() with the names of the fields you want 
# to aggregate listed within the parentheses. 
# Mean for N_disk_firm=5 models (specifical firm) and time moment
Agg_data <- as.data.frame(datasetSerial_firm[,
                          j=list(smart_1_normalized = mean(smart_1_normalized, na.rm = TRUE), 
                                 smart_3_normalized = mean(smart_3_normalized, na.rm = TRUE),
                                 smart_7_normalized = mean(smart_7_normalized,na.rm = TRUE),  
                                 smart_9_raw        = mean(smart_9_raw,na.rm = TRUE),
                                 smart_12_raw       = mean(smart_12_raw,na.rm = TRUE),
                                 smart_190_raw      = mean(smart_190_raw,na.rm = TRUE)
                                 ),
                                 by                 = list(time_dif,model_number)]) 


View(Agg_data)

# Convert datasetSerial_firm to data frame
datasetSerial_firm <- data.frame(datasetSerial_firm)

# Form plots
agg1 <-   Agg_data %>%
          filter(model_number %in% datasetSerial_N_part_firm$model_number) %>%
          ggplot(aes(x=time_dif,y=smart_1_normalized,color=model_number))+geom_point(position = "jitter")+geom_smooth()
agg3 <-   Agg_data %>%
          filter(model_number %in% datasetSerial_N_part_firm$model_number) %>%
          ggplot(aes(x=time_dif,y=smart_3_normalized,color=model_number))+geom_point(position = "jitter")+geom_smooth()
agg7 <-   Agg_data %>%
          filter(model_number %in% datasetSerial_N_part_firm$model_number) %>%
          ggplot(aes(x=time_dif,y=smart_7_normalized,color=model_number))+geom_point(position = "jitter")+geom_smooth()
agg9 <-   Agg_data %>%
          filter(model_number %in% datasetSerial_N_part_firm$model_number) %>%
          ggplot(aes(x=time_dif,y=smart_9_raw,color=model_number))+geom_point(position = "jitter")+geom_smooth()
agg12 <-  Agg_data %>%
          filter(model_number %in% datasetSerial_N_part_firm$model_number) %>%
          ggplot(aes(x=time_dif,y=smart_12_raw,color=model_number))+geom_point(position = "jitter")+geom_smooth()
agg190 <- Agg_data %>%
          filter(model_number %in% datasetSerial_N_part_firm$model_number) %>%
          ggplot(aes(x=time_dif,y=smart_190_raw,color=model_number))+geom_point(position = "jitter")+geom_smooth()

# Show plots
grid.arrange(agg1,agg3,agg7,agg9,agg12,agg190, nrow=3, ncol=2)  
