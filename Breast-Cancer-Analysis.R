#=============================================================================================================================================================================================================================
# PREREQUISITES, READING IN DATA SET, AND TIDYING DATA
#=============================================================================================================================================================================================================================

rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(Rmisc) # transform integers to factors, must be first or will mask other packages. We only need multiplot!
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values
library(dplyr)
library(wesanderson)
library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")

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

#------------------------------------------------------------------------------------- [BOX PLOTS]

# I hypothesize that with increasing averages, there is a higher likelihood of malignancy diagnosis: let's look at boxplots first

radius_bp <- ggplot(mean_bc_data, aes(x = diagnosis, y = radius_mean)) +
  geom_boxplot(fill = wes_palette("Moonrise3", n = 2)) +
  labs(x = "Diagnosis (Benign vs Malignant)", 
       y = "Average Radius",
       title = "Average Radius of Malignant Cell Nuclei are Larger",
       subtitle = "Breast Cancer UCI Data",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

data %>%
  group_by(diagnosis) %>%
  summarise(mean(radius_mean)) # average radius Benign = 12.1 and Malignant = 17.5

t.test(mean_bc_data$radius_mean ~ mean_bc_data$diagnosis, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)


#-------------------------------------------------------------------------------------

peri_bp <- ggplot(mean_bc_data, aes(x = diagnosis, y = perimeter_mean)) +
  geom_boxplot(fill = wes_palette("Moonrise3", n = 2)) +
  labs(x = "Diagnosis (Benign vs Malignant)", 
       y = "Average Perimeter",
       title = "Average Perimeter of Malignant Cell Nuclei are Larger",
       subtitle = "Breast Cancer UCI Data",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


mean_bc_data %>%
  group_by(diagnosis) %>%
  summarise(mean(perimeter_mean)) # average radius Benign = 78.1 and Malignant = 115

#-------------------------------------------------------------------------------------

area_bp <- ggplot(mean_bc_data, aes(x = diagnosis, y = area_mean)) +
  geom_boxplot(fill = wes_palette("Moonrise3", n = 2)) +
  labs(x = "Diagnosis (Benign vs Malignant)", 
       y = "Average Area",
       title = "Average Area of Malignant Cell Nuclei are Larger",
       subtitle = "Breast Cancer UCI Data",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

mean_bc_data %>%
  group_by(diagnosis) %>%
  summarise(mean(area_mean)) # average radius Benign = 463 and Malignant = 978

#For all three boxplots, they show a dramatic difference between average size. There is a positive correlation between radius, perimeter, and area to categorization of malignancy.
multiplot(radius_bp, peri_bp, area_bp, cols = 2) #the above 3 plots plotted on a single graph for visibility purposes

#------------------------------------------------------------------------------------- [DENSITY PLOTS]

radius_dp <- ggplot(mean_bc_data, aes(x = radius_mean, fill = diagnosis)) +
  geom_density(size = 1, alpha = .5) +
  labs(x = "Average Radius", 
       y = "Density",
       title = "Majority of Malignant Cells have an",
       subtitle = "Average Radius of between 15 and 20",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

peri_dp <- ggplot(mean_bc_data, aes(x = perimeter_mean, fill = diagnosis)) +
  geom_density(size = 1, alpha = .5) +
  labs(x = "Average Perimeter", 
       y = "Density",
       title = "Majority of Malignant Cells have an",
       subtitle = "Average Perimeter of between 100 and 130",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


area_dp <- ggplot(mean_bc_data, aes(x = area_mean, fill = diagnosis)) +
  geom_density(size = 1, alpha = .5) +
  labs(x = "Average Area", 
       y = "Density",
       title = "Majority of Malignant Cells have an",
       subtitle = "Average Area of between 700 and 1250",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

multiplot(radius_dp, peri_dp, area_dp, cols = 2) #the above 3 plots plotted on a single graph for visibility purposes

# It is interesting to see that there is a shared relationship between proprotions of those with a partifuclar diagnosis and size of cell nuclei.
# It is not surprising since a greater radius would signifiy a greater perimeter, which would signify a greater area.
# A density plot is great to see how the patients are distributed by their diagnosis and to see how the peaks signify at a particular x value, where the most patients will fall into that bin.


#------------------------------------------------------------------------------------- [SCATTER PLOTS]

# what would it look like if we created a scatter plot between the average radius, perimeter and area (color to reflect diagnosis)
# All sharing a positive correlation

rp_sp <- ggplot(mean_bc_data, aes(x = radius_mean, y = perimeter_mean, color = diagnosis)) +
  geom_point()+
  labs(x = "Average Radius", 
       y = "Average Perimeter",
       title = "Positive Linear Correlation",
       subtitle = "Between the Radius and Perimeter",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ra_sp <- ggplot(mean_bc_data, aes(x = radius_mean, y = area_mean, color = diagnosis)) +
  geom_point() +
  labs(x = "Average Radius", 
       y = "Average Area",
       title = "Positive Correlation",
       subtitle = "Between the Radius and Area",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

pa_sp <- ggplot(mean_bc_data, aes(x = perimeter_mean, y = area_mean, color = diagnosis)) +
  geom_point() +
  labs(x = "Average Perimeter", 
       y = "Average Area",
       title = "Positive Correlation",
       subtitle = "Between the Perimeter and Area",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

multiplot(rp_sp, ra_sp, pa_sp, cols = 2)


#=============================================================================================================================================================================================================================
# SECTION 2: SMOOTHNESS & COMPACTNESS TO CELL NUCLEI DIAGNOSIS (BENIGN VS MALIGNANT)
#=============================================================================================================================================================================================================================

# Since smoothness is calculated from variability it will always be less than 1 but greater than zero
# Would zero mean 100% smoothness and the closer you are the less variation in radius lengths.

ggplot(mean_bc_data, aes(x = diagnosis, y = smoothness_mean)) +
  geom_boxplot(fill = wes_palette("GrandBudapest1", n = 2)) +
  labs(x = "Diagnosis", 
       y = "Average Smoothness",
       title = "Malignant Cell Nuclei are Less Smooth",
       subtitle = "Closer to Zero, Less Variation in Radius Lengths",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# We see that the malignant cell nuclei have more average variability (less smooth) than the benign cell nuclei of breast mass.

ggplot(mean_bc_data, aes(x = diagnosis, y = compactness_mean)) +
  geom_boxplot(fill = wes_palette("GrandBudapest1", n = 2)) +
  labs(x = "Diagnosis", 
       y = "Average Compactness",
       title = "Malignant Cell Nuclei are Less Compact",
       subtitle = "Closer to Zero, the More Compact",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Compactness is defined by the cells ability to be "closely and packed together." <a href="https://medical-dictionary.thefreedictionary.com/compactness">compact</a>
# In this case, the smaller the number, the more compact it is and the larger the number the less compact it is
# Can we assume that???

# Abiding by that assumption, Malignant cell nuclei are less compact on average than benign cancer cells

# Both the smoothness and compactness seem to be categorize the closer you are to zero the higher likelihood you have a benign cell nuclei
# and the further you are from zero the higher the likelihood you have a malignant cell nuclei

# Let's look at their relationship:

ggplot(mean_bc_data, aes(x = smoothness_mean, y = compactness_mean, color = diagnosis)) +
  geom_point() +
  geom_smooth(se = FALSE, size = 2) +
  labs(x = "Average Smoothness", 
       y = "Average Compactness",
       title = "Positive Correlation Between Smoothness and Compactness",
       subtitle = "Seen in Both Benign and Malignant Cell Nuclei",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# This graph effectively tells us that both of the diagnoses, are positively correlated with smoothness and compactness.

#=============================================================================================================================================================================================================================
# SECTION 3: CONCAVE SEVERITY & AMOUNT TO CELL NUCLEI DIAGNOSIS (BENIGN VS MALIGNANT)
#=============================================================================================================================================================================================================================

# Can we assume that the higher the number the higher the severity of the concave portions within the cell nuclei
# Can we assume that the higher the total number of concave portions, the higher the likelihood of malignancy?

mean_bc_data %>%
  summarise(
    mean(concavity_mean),
    min(concavity_mean),
    max(concavity_mean)
  )
# The range of the data within concavity mean is between 0 and 0.427. Zero indicating no severity in concave portions (relatively flat)
# let's see if we can see the relationship between concavity (severity of concave portions) to diagnosis

ggplot(mean_bc_data, aes(x = diagnosis, y = concavity_mean)) +
  geom_boxplot(fill = wes_palette("GrandBudapest2", n = 2)) +
  labs(x = "Diagnosis", 
       y = "Average Concavity",
       title = "Malignant Cell Nuclei are More Severe in Concavity",
       subtitle = "Closer to Zero, Less Severity in Concave Portions",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# The higher the average concavity, the higher the likelihood of malignancy

#now let's explore the number of concave portions in the same regard as severity.
mean_bc_data %>%
  summarise(
    mean(concave.points_mean),
    min(concave.points_mean),
    max(concave.points_mean)
  )
# The range of the data within concavity mean is between 0 and 0.201. Zero indicating no concave portions.
# since these numbers are less than 1, can we assume this is in reference to a proportion of the cell nuclei that has concave points??

ggplot(mean_bc_data, aes(x = diagnosis, y = concave.points_mean)) +
  geom_boxplot(fill = wes_palette("GrandBudapest2", n = 2)) +
  labs(x = "Diagnosis", 
       y = "Average Number of Concave Portions",
       title = "Malignant Cell Nuclei Have More Concave Portions",
       subtitle = "Closer to Zero, Less Concave Portions",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# The higher the number of concave points (or the higher the percent of the cell nuclei with concave portions), the higher the liklihood of malignancy

# When plotting the two variables above, we expect to see a positive correlation between the two in regards to diagnosis

ggplot(mean_bc_data, aes(x = concave.points_mean, y = concavity_mean, color = diagnosis)) +
  geom_point(position = "jitter", alha = 1/5) +
  geom_smooth(se = FALSE, size = 2) +
  geom_vline(xintercept = .0853, linetype = "dashed", color = "blue", size = 1) +
  labs(x = "Average Number of Concave Portions", 
       y = "Average Concavity",
       title = "Benign Cell Nuclei has a Sudden Breakpoint",
       subtitle = "Number of Concave Points can be a Good Predictor of Malignancy",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Makes sense that the higher the number or proportion of concave points within a cell nuclei, the higher the severity.
# If there is no concave points, there should be virtually no severity as the concavity is not there.

# There seems to be a drop off in benign cancer cells (in terms of concave points), lets explore the max of benign diagnosis in these two variables!
# 100% of cancer cells with a number > 0.0853 of concave Portions are Malignant!

mean_bc_data %>%
  filter(diagnosis == "B") %>%
  summarise(
    mean(concave.points_mean),
    max(concave.points_mean) # max number of concave points is .0853. Let's add a vertical line to ggplot above at .09 to show cut off of concave points.
  )
# With that being said, can we use number of concave points as an indicator for malignancy within a cell nuclei?
# such as that > than .0853 is a good indicator of malignancy? Can this be applied to the population? Great questions!


#=============================================================================================================================================================================================================================
# COMPARISON OF AVERAGE VARIABLE TO WORST (LARGEST) VARIABLE SEPARATED BY DIAGNOSIS XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#=============================================================================================================================================================================================================================

#=============================================================================================================================================================================================================================
# SECTION 1: RADIUS, PERIMETER, AND AREA XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#=============================================================================================================================================================================================================================

# cc_data <- bc_data %>%
  #select(id:radius_mean, perimeter_mean:concave.points_mean, radius_worst, perimeter_worst:concave.points_worst)

# radius_cc <- ggplot(data = cc_data, mapping = aes(x = radius_mean, y = radius_worst, color = diagnosis)) +
 # geom_point()

# ggplot(cc_data, aes(x = diagnosis, y = radius_worst)) +
  #geom_boxplot()




#=============================================================================================================================================================================================================================
# LOGISTIC REGRESSION - BINOMIAL - FOR ALL ANALYZED VARIABLES IN RELATION TO DIAGNOSIS
#=============================================================================================================================================================================================================================


# I don't feel that adding the worst in comparison to the average adds anymore too the analysis. Everything is just shifted upward.
mean_bc_data$diagnosis <- relevel(mean_bc_data$diagnosis, ref = "B") # changed the reference to be M and finding good predictors for it

predicted <- glm(diagnosis ~ radius_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + concave.points_mean, family = "binomial", data = mean_bc_data)
summary(predicted)

# as notated by the small p-values, out of all of the variables we analyzed, it seems that area_mean and concave.points_mean 
# are the most useful predictors of diagnosis!

probability_data <- data.frame(fitted.values = predicted$fitted.values, status = mean_bc_data$diagnosis)

probability_data <- probability_data %>%
  arrange(fitted.values)

probability_data <- probability_data %>%
  mutate(rank = 1:nrow(probability_data))

ggplot(probability_data, aes(x = rank, y = fitted.values, color = status)) +
  geom_point(alpha = 1, shape = 1, stroke = 2) +
  labs(x = "Rank", 
       y = "Predicted Probability of Malignancy",
       title = "Predicted Probability of Malignant Cell Nuclei",
       subtitle = "Closer to One, the Higher the Probability of Malignancy",
       caption = "Source: https://archive.ics.uci.edu") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
#As seen by the graph, our glm has accurately captured the relationship of the 3 variables to diagnosis
# The 3 values are all good predictors of malignancy as shown by the small p-values












