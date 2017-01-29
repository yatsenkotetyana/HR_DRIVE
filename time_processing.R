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

setwd("data_Q3_2016")
# Load splitted dataset
dataset <- read.csv("hard_drive_choose_split.csv")
View(dataset)
summary(dataset)
str(dataset)

# Normalized capacity (TB)
dataset$T_capacity_bytes <- dataset$capacity_bytes/ 1e12
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

# Firm name
firm_name <-  c("WDC") # c("HGST","Hitachi","ST","TOSHIBA","WDC")
# dataset with all records for broken disks of some firm
datasetSerial_firm <- dataset  %>%
                      filter(serial_number %in% serial$serial_number,firm %in% firm_name)  

# Form factor from serial_number
datasetSerial_firm$serial_number   <- as.character(datasetSerial_firm$serial_number)

# Cound and arraned by this number all records for failure (working) disk
# (all recoreds before failure)
datasetSerial_N_firm <- datasetSerial_firm   %>% 
                        count(serial_number) %>% 
                        arrange(desc(n)) 
str(datasetSerial)
View(datasetSerial)

# Number of broken disk to look
N_disk_firm <- 5

# Take fist N_disk disk (have more records)
datasetSerial_N_part_firm <- datasetSerial_N_firm[1:N_disk_firm,]
# Form factor from serial_number
datasetSerial_firm $serial_number  <- as.factor(datasetSerial_firm$serial_number)

f1 <- datasetSerial_firm %>%
      filter(serial_number %in% datasetSerial_N_part_firm$serial_number) %>%
      ggplot(aes(x=time_dif,y=smart_1_normalized,color=serial_number))+geom_point(position = "jitter")+geom_smooth()     
f3 <- datasetSerial_firm %>%
      filter(serial_number %in% datasetSerial_N_part_firm$serial_number) %>%
      ggplot(aes(x=time_dif,y=smart_3_normalized,color=serial_number))+geom_point(position = "jitter")+geom_smooth()     
f7 <- datasetSerial_firm %>%
      filter(serial_number %in% datasetSerial_N_part_firm$serial_number) %>%
      ggplot(aes(x=time_dif,y=smart_7_normalized,color=serial_number))+geom_point(position = "jitter")+geom_smooth()     
f9 <- datasetSerial_firm %>%
      filter(serial_number %in% datasetSerial_N_part_firm$serial_number) %>%
      ggplot(aes(x=time_dif,y=smart_9_raw,color=serial_number))+geom_point(position = "jitter")+geom_smooth()     
f12 <- datasetSerial_firm %>%
       filter(serial_number %in% datasetSerial_N_part_firm$serial_number) %>%
       ggplot(aes(x=time_dif,y=smart_12_raw,color=serial_number))+geom_point(position = "jitter")+geom_smooth()
f190 <- datasetSerial_firm %>%
        filter(serial_number %in% datasetSerial_N_part_firm$serial_number) %>%
        ggplot(aes(x=time_dif,y=smart_190_raw,color=serial_number))+geom_point(position = "jitter")+geom_smooth()     

grid.arrange(f1,f3,f7,f9,f12,f190, nrow=3, ncol=2)  

