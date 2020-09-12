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

bc_data$diagnosis <- as.factor(bc_data$diagnosis)


# POTENTIAL RESEARCH AND ANALYSIS OPPORTUNITIES:
# looking at all variables in comparison to having breast cancer or not.
# can run logistic model to see which variables are good predictors of having breast cancer or not (will need to change diagnosis to a factor of zero (benign) and one(malignant))
# Not interested in texture since there is no reference image to get full understanding
# Can compare the average to the worst for each key feature of the cell nuclei of the fine needle aspirate (FNA) of a breast mass.

#=============================================================================================================================================================================================================================
# COMPARISON OF AVERAGE VARIABLE TO IF THE PATIENT HAS MALIGNANT OR BENIGN CELL NUCLEI OF BREAST MASS
#=============================================================================================================================================================================================================================

#=============================================================================================================================================================================================================================
# SECTION 1: RADIUS, PERIMETER, AND AREA TO CELL NUCLEI DIAGNOSIS (BENIGN VS MALIGNANT)
#=============================================================================================================================================================================================================================

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3678677/ COULD BE AN ARTICLE THAT WE CAN USE AS SUPPORTING DETAILS!

mean_bc_data <- bc_data %>%
  select(id:radius_mean, perimeter_mean:fractal_dimension_mean) #select columns of focus

# We can do a multiple linear regression model for the radius, perimeter, and area to diagnosis.
# let's start off with doing individual comparisons. radius -> perimeter -> area.
# I hypothesize that with increasing averages, there is a higher likelihood of malignancy diagnosis: let's look at boxplots first

ggplot(mean_bc_data, aes(x = diagnosis, y = radius_mean)) +
  geom_boxplot()

mean_bc_data %>%
  group_by(diagnosis) %>%
  summarise(mean(radius_mean)) # average radius Benign = 12.1 and Malignant = 17.5

#-------------------------------------------------------------------------------------

ggplot(mean_bc_data, aes(x = diagnosis, y = perimeter_mean)) +
  geom_boxplot()

mean_bc_data %>%
  group_by(diagnosis) %>%
  summarise(mean(perimeter_mean)) # average radius Benign = 78.1 and Malignant = 115

#-------------------------------------------------------------------------------------

ggplot(mean_bc_data, aes(x = diagnosis, y = area_mean)) +
  geom_boxplot()

mean_bc_data %>%
  group_by(diagnosis) %>%
  summarise(mean(area_mean)) # average radius Benign = 463 and Malignant = 978

#For all three boxplots, they show a dramatic difference between average size. There is a positive correlation between radius, perimeter, and area to categorization of malignancy.

#-------------------------------------------------------------------------------------

# what would it look like if we created a scatter plot between the average radius, perimeter and area (color to reflect diagnosis)
# All sharing a positive correlation

ggplot(mean_bc_data, aes(x = radius_mean, y = perimeter_mean, color = diagnosis)) +
  geom_point()

ggplot(mean_bc_data, aes(x = radius_mean, y = area_mean, color = diagnosis)) +
  geom_point()

ggplot(mean_bc_data, aes(x = perimeter_mean, y = area_mean, color = diagnosis)) +
  geom_point()

#------------------------------------------------------------------------------------

# since we know that there is a linear relationship, let's see if all 3 are good predictors of malignancy
# Logistic Linear Regression - Binomial

predicted <- glm(diagnosis ~ radius_mean + perimeter_mean + area_mean, family = "binomial", data = mean_bc_data)
summary(predicted)

#Why is radius negative?!

probability_data <- data.frame(fitted.values = predicted$fitted.values, status = mean_bc_data$diagnosis)

probability_data <- probability_data %>%
  arrange(fitted.values)

probability_data <- probability_data %>%
  mutate(rank = 1:nrow(probability_data))

ggplot(probability_data, aes(x = rank, y = fitted.values, color = status)) +
  geom_point(alpha = 1, shape = 1, stroke = 2)

#As seen by the graph, our glm has accurately captured the relationship of the 3 variables to diagnosis
# The 3 values are all good predictors of malignancy as shown by the small p-values

t.test(mean_bc_data$radius_mean ~ mean_bc_data$diagnosis, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)

# Curious to see why the radius has a negative relationship to diagnosis. but the other 2 have positive relationships.
# Will have to look into this a bit more*********************






