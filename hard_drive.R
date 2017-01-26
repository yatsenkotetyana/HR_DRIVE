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
dataset <- read.csv("hard_drive_choose_split.csv")  #,nrows=100000
# View database
View(dataset)
# Summarry
summary(dataset)
#Strusture
str(dataset)

# Normalized capacity (TB)
dataset$T_capacity_bytes <- dataset$capacity_bytes/ 1e12


# Arrange by failure & date
# For each serial_number unique number
# You receive all serial number just one time
Unique_firm <- dataset %>%
  arrange(desc(failure),desc(date)) %>%
  distinct(serial_number,.keep_all = TRUE)
View(Unique_firm)  

# Count work & failure for each firm
table(Unique_firm$firm,Unique_firm$failure)
# The same in other way
# Number of record for each firm (table)
firms <- count (Unique_firm,firm,failure)
View(firms)

# Count work & failure for whole firm
table(Unique_firm$failure)

# Form factor "firm" and "failure"
Unique_firm$firm    <- as.factor(Unique_firm$firm)
Unique_firm$failure <- as.factor(Unique_firm$failure)
# Structure
str(dataset)


# Dependence number of record on each firm + 
# Facet on failure (0 - work, 1 - not) (Picture)
ggplot(Unique_firm, aes(firm, fill = firm ) ) + geom_bar()+facet_wrap(~failure,nrow=2,scales = "free")

# Dependence capacity (in TB) on firm
# Facet on failure (0 - work, 1 - not) (Picture)
ggplot(Unique_firm, aes(x=firm,y=T_capacity_bytes,color=firm))+geom_jitter()+
  facet_wrap(~failure,nrow=2,scales = "free")

# Smart_1_normalized on firm
# Facet on failure (0 - work, 1 - not) (Picture)
ggplot(Unique_firm, aes(x=firm,y=smart_1_normalized,color=firm))+geom_jitter()+
  facet_wrap(~failure,nrow=2,scales = "free")

# Smart_3_normalized on firm
# Facet on failure (0 - work, 1 - not) (Picture)
ggplot(Unique_firm, aes(x=firm,y=smart_3_normalized,color=firm))+geom_jitter()+
  facet_wrap(~failure,nrow=2,scales = "free")

# Smart_7_normalized on firm
# Facet on failure (0 - work, 1 - not) (Picture)
ggplot(Unique_firm, aes(x=firm,y=smart_7_normalized,color=firm))+geom_jitter()+
  facet_wrap(~failure,nrow=2,scales = "free")

# Smart_9_raw on firm
# Facet on failure (0 - work, 1 - not) (Picture)
ggplot(Unique_firm, aes(x=firm,y=smart_9_raw,color=firm))+geom_jitter()+
  facet_wrap(~failure,nrow=2,scales = "free")

# Smart_12_raw on firm
# Facet on failure (0 - work, 1 - not) (Picture)
ggplot(Unique_firm, aes(x=firm,y=smart_12_raw,color=firm))+geom_jitter()+
  facet_wrap(~failure,nrow=2,scales = "free")

# Smart_190_raw on firm
# Facet on failure (0 - work, 1 - not) (Picture)
ggplot(Unique_firm, aes(x=firm,y=smart_190_raw,color=firm))+geom_jitter()+
  facet_wrap(~failure,nrow=2,scales = "free")

# Information for working disk (last time moment)
w1 <- Unique_firm %>%
  filter(failure==0) %>%
  ggplot(aes(x=firm,y=smart_1_normalized,color=firm))+geom_jitter()
w3 <- Unique_firm %>%
  filter(failure==0) %>%
  ggplot(aes(x=firm,y=smart_3_normalized,color=firm))+geom_jitter()
w7 <- Unique_firm %>%
  filter(failure==0) %>%
  ggplot(aes(x=firm,y=smart_7_normalized,color=firm))+geom_jitter()
w9 <- Unique_firm %>%
  filter(failure==0) %>%
  ggplot(aes(x=firm,y=smart_9_raw,color=firm))+geom_jitter()
w12 <- Unique_firm %>%
  filter(failure==0) %>%
  ggplot(aes(x=firm,y=smart_12_raw,color=firm))+geom_jitter()
w190 <- Unique_firm %>%
  filter(failure==0) %>%
  ggplot(aes(x=firm,y=smart_190_raw,color=firm))+geom_jitter()

grid.arrange(w1,w3,w7,w9,w12,w190, nrow=3, ncol=2) 

f1 <- Unique_firm %>%
  filter(failure==1) %>%
  ggplot(aes(x=firm,y=smart_1_normalized,color=firm))+geom_jitter()
f3 <- Unique_firm %>%
  filter(failure==1) %>%
  ggplot(aes(x=firm,y=smart_3_normalized,color=firm))+geom_jitter()
f7 <- Unique_firm %>%
  filter(failure==1) %>%
  ggplot(aes(x=firm,y=smart_7_normalized,color=firm))+geom_jitter()
f9 <- Unique_firm %>%
  filter(failure==1) %>%
  ggplot(aes(x=firm,y=smart_9_raw,color=firm))+geom_jitter()
f12 <- Unique_firm %>%
  filter(failure==1) %>%
  ggplot(aes(x=firm,y=smart_12_raw,color=firm))+geom_jitter()
f190 <- Unique_firm %>%
  filter(failure==1) %>%
  ggplot(aes(x=firm,y=smart_190_raw,color=firm))+geom_jitter()

grid.arrange(f1,f3,f7,f9,f12,f190, nrow=3, ncol=2)

