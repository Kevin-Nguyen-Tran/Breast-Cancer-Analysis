#=============================================================================================================================================================================================================================
# PREREQUISITES, READING IN DATA SET, AND TIDYING DATA
#=============================================================================================================================================================================================================================

rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values
library(forcats) # transform integers to factors

# https://www.kaggle.com/uciml/breast-cancer-wisconsin-data

#read the data set into RStudio and stored into object
data <- read.csv("C:/Users/Kevin/Desktop/Breast-Cancer-Analysis/Breast Cancer Wisconsin Data Set.csv")

bc_data <- as_tibble(data)

# POTENTIAL RESEARCH AND ANALYSIS OPPORTUNITIES:
# looking at all variables in comparison to having breast cancer or not.
# Not interested in texture since there is no reference image to get full understanding
# Can compare the average to the worst for each key feature of the cell nuclei of the fine needle aspirate (FNA) of a breast mass.








