#=======================================================================
#
# Practical Data Analysis - Cold storage Problem
#
#=======================================================================

# Environment Set up and Data Import

# Invoking Libraries
library(tidyverse) # contains ggplot2,dplyr,forcats,lubridate etc
library(gridExtra) # Needed for plotting multiple ggplot graphs side-by-side
library(knitr) # Necessary to generate sourcecodes from a .Rmd File

# Setup Working Directory
setwd("C:/Users/USER/Documents/El-PaDJo/R programming language/4-Project 2")

# Import and Read Input File
cold_storage_temp_data = read.csv("Cold_Storage_Temp_Data.csv")
cold_storage_mar2018_data = read.csv("Cold_Storage_Mar2018.csv")


# Global options settings
options(scipen=999)  # turn off scientific notation like 1e+06

# Function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# PROBLEM 1: 
# Cold Storage started its operations in Jan 2016. They are in the business of 
# storing Pasteurized Fresh Whole or Skimmed Milk, Sweet Cream, Flavoured Milk Drinks. 
# To ensure that there is no change of texture, body appearance, separation of fats the 
# optimal temperature to be maintained is between 2 - 4 C.
# In the first year of business, they outsourced the plant maintenance work to a 
# professional company with stiff penalty clauses. It was agreed that if it was 
# statistically proven that the probability of temperature going outside the 2 - 4 C 
# during the one-year contract was above 2.5% and less than 5% then the penalty would be 
# 10% of AMC (annual maintenance contract). In case it exceeded 5% then the penalty 
# would be 25% of the AMC fee. The average temperature data at date level is given in 
# the file "Cold_Storage_Temp_Data.csv"


# Preliminary analysis for Problem 1
# check dimension of dataset
dim(cold_storage_temp_data)

#see first 6 rows(observations) of dataset
head(cold_storage_temp_data)

#see last 6 rows(observations) of dataset
tail(cold_storage_temp_data)

# check structure of dataset
str(cold_storage_temp_data)

# get summary of dataset
summary(cold_storage_temp_data)

# get standard deviation of concerned column: Temperature
sd_temp_1yr_data = sd(cold_storage_temp_data$Temperature)
sd_temp_1yr_data

# get variance of concerned column: Temperature
var_temp_1yr_data = var(cold_storage_temp_data$Temperature)
var_temp_1yr_data

# get mode of concerned column: Temperature
getmode(cold_storage_temp_data$Temperature)

# Histogram for Temperature Variable (1Yr Data)
temperature_histogram = 
  ggplot(cold_storage_temp_data, aes(x = cold_storage_temp_data$Temperature)) +
  geom_histogram(fill = "cornflowerblue", bins = 30) + 
  labs(title="Histogram for Temperature (1Yr Data)", x="Temperature", y="Count") + 
  scale_x_continuous(breaks=seq(1.4,max(cold_storage_temp_data$Temperature)+0.3,by=0.3))+
  scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))

# Box Plot for temperature Variable (1Yr Data)
temperature_boxplot = 
  ggplot(cold_storage_temp_data, aes(x = 0, y = cold_storage_temp_data$Temperature)) +
  geom_boxplot(size=1, outlier.shape = 1, outlier.color="black", fill="cornflowerblue")+
  labs(title = "Box Plot for Temperature (1Yr Data) ", x = "", y="Temperature") +
  scale_y_continuous(breaks=seq(1.4,max(cold_storage_temp_data$Temperature)+0.3,by=0.3))+
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.text.y = element_blank()) +
  coord_flip()

# Print charts side-by-side
grid.arrange(temperature_histogram,temperature_boxplot,nrow=1,ncol= 2)

# Q1: calculate Temperature mean value for Summer, Winter and Rainy Season
by( cold_storage_temp_data$Temperature,INDICES = cold_storage_temp_data$Season,FUN=mean)

# Q2: calculate Temperature mean value for the year
mean_temp_1yr_data = mean(cold_storage_temp_data$Temperature)
mean_temp_1yr_data

# Q3: calculate Temperature Standard deviation for the year
sd_temp_1yr_data = sd(cold_storage_temp_data$Temperature)
sd_temp_1yr_data

# Q4: On assumption of Normal Distribution, calculate probability of temperature < 2 C
norm_prob_below_2 = pnorm(2,mean=mean_temp_1yr_data,sd=sd_temp_1yr_data)
norm_prob_below_2

# Q5: On assumption of Normal Distribution, calculate probability of temperature > 4 C
norm_prob_above_4 = 1 - pnorm(4,mean=mean_temp_1yr_data,sd=sd_temp_1yr_data)
norm_prob_above_4

# Q6: On assumption of Normal Distribution, calculate probability 
# of temperature > 4 C OR temperature < 2 C
norm_prob_outside_2_4 = norm_prob_below_2 + norm_prob_above_4 
norm_prob_outside_2_4

# Q7: calculate a one-way ANOVA
aov_temp_1yr_data=aov(cold_storage_temp_data$Temperature~cold_storage_temp_data$Season,
                      data=cold_storage_temp_data)

# test for differences between rainy, summer and winter Seasons
TukeyHSD(aov_temp_1yr_data)

# PROBLEM 2: 
# In Mar 2018, Cold Storage started getting complaints from their clients 
# that they have been getting complaints from end consumers of the dairy products 
# going sour and often smelling. On getting these complaints, the supervisor pulls out 
# data of the last 35 days' temperatures. As a safety measure, the Supervisor decides 
# to be vigilant to maintain the temperature at 3.9 C or below.
# Assume 3.9 C as the upper acceptable value for mean temperature and at alpha = 0.1. 
# Do you feel that there is a need for some corrective action in the Cold Storage 
# Plant or is it that the problem is from the procurement side from where Cold 
# Storage is getting the Dairy Products? The data of the last 35 days is in 
# "Cold_Storage_Mar2018.csv"


# Preliminary analysis for problem 2
# check dimension of dataset
dim(cold_storage_mar2018_data)

#see first 6 rows(observations) of dataset
head(cold_storage_mar2018_data)

#see last 6 rows(observations) of dataset
tail(cold_storage_mar2018_data)

# check structure of dataset
str(cold_storage_mar2018_data)

# get summary of dataset
summary(cold_storage_mar2018_data)

# get standard deviation of concerned column: Temperature
sd_temp_mar_data = sd(cold_storage_mar2018_data$Temperature)
sd_temp_mar_data

# get variance of concerned column: Temperature
var_temp_mar_data = var(cold_storage_mar2018_data$Temperature)
var_temp_mar_data

# get mode of concerned column: Temperature
getmode(cold_storage_mar2018_data$Temperature)

# Histogram for Temperature Variable (mar Data)
temperature_histogram2 = 
  ggplot(cold_storage_mar2018_data, aes(x = cold_storage_mar2018_data$Temperature)) +
  geom_histogram(fill = "cornflowerblue", bins = 10) + 
  labs(title="Histogram for Temperature (mar Data)", x="Temperature", y="Count") + 
  scale_x_continuous(breaks=seq(3.7,max(cold_storage_mar2018_data$Temperature)+0.1,by=0.1))+
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))

# Box Plot for temperature Variable (mar Data)
temperature_boxplot2 = 
  ggplot(cold_storage_mar2018_data, aes(x = 0, y = cold_storage_mar2018_data$Temperature)) +
  geom_boxplot(size=1, outlier.shape = 1, outlier.color="black", fill="cornflowerblue")+
  labs(title = "Box Plot for Temperature (mar Data) ", x = "", y="Temperature") +
  scale_y_continuous(breaks=seq(3.7,max(cold_storage_mar2018_data$Temperature)+0.1,by=0.1))+
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.text.y = element_blank()) +
  coord_flip()

# Print charts side-by-side
grid.arrange(temperature_histogram2,temperature_boxplot2,nrow=1,ncol= 2)

# Q1: Which Hypothesis test shall be performed to check if corrective action is 
# needed at the cold storage plant?
# ANS: A one Sample upper tailed T-test


# Q2: State the Hypothesis, perform hypothesis test and determine p-value
#   H0: E[Temperature] = 3.9 C
#   HA: E[Temperature] > 3.9 C


# perform upper tailed t.test on mar2018 data with true mean provided
t.test(cold_storage_mar2018_data$Temperature,mu=3.9,alternative='greater')


# Since pvalue is less than level of significance, we reject the hypothesis.
# There is need for some corrective action in the Cold Storage Plant. 
# The problem is NOT from the procurement side from where Cold Storage is 
# getting the Dairy Products.


#=======================================================================
#
# T H E - E N D
#
#=======================================================================

#Generate the .R file to hold the source code
purl("project-2.Rmd", documentation = 0) 


## #=======================================================================
## #
## # Practical Data Analysis - Cold storage Problem
## #
## #=======================================================================
## 
## # Environment Set up and Data Import
## 
## # Invoking Libraries
## library(tidyverse) # contains ggplot2,dplyr,forcats,lubridate etc
## library(gridExtra) # Needed for plotting multiple ggplot graphs side-by-side
## library(knitr) # Necessary to generate sourcecodes from a .Rmd File
## 
## # Setup Working Directory
## setwd("C:/Users/USER/Documents/El-PaDJo/R programming language/4-Project 2")
## 
## # Import and Read Input File
## cold_storage_temp_data = read.csv("Cold_Storage_Temp_Data.csv")
## cold_storage_mar2018_data = read.csv("Cold_Storage_Mar2018.csv")
## 
## 
## # Global options settings
## options(scipen=999)  # turn off scientific notation like 1e+06
## 
## # Function to calculate mode
## getmode <- function(v) {
##   uniqv <- unique(v)
##   uniqv[which.max(tabulate(match(v, uniqv)))]
## }
## 
## # PROBLEM 1:
## # Cold Storage started its operations in Jan 2016. They are in the business of
## # storing Pasteurized Fresh Whole or Skimmed Milk, Sweet Cream, Flavoured Milk Drinks.
## # To ensure that there is no change of texture, body appearance, separation of fats the
## # optimal temperature to be maintained is between 2 - 4 C.
## # In the first year of business, they outsourced the plant maintenance work to a
## # professional company with stiff penalty clauses. It was agreed that if it was
## # statistically proven that the probability of temperature going outside the 2 - 4 C
## # during the one-year contract was above 2.5% and less than 5% then the penalty would be
## # 10% of AMC (annual maintenance contract). In case it exceeded 5% then the penalty
## # would be 25% of the AMC fee. The average temperature data at date level is given in
## # the file "Cold_Storage_Temp_Data.csv"
## 
## 
## # Preliminary analysis for Problem 1
## # check dimension of dataset
## dim(cold_storage_temp_data)
## 
## #see first 6 rows(observations) of dataset
## head(cold_storage_temp_data)
## 
## #see last 6 rows(observations) of dataset
## tail(cold_storage_temp_data)
## 
## # check structure of dataset
## str(cold_storage_temp_data)
## 
## # get summary of dataset
## summary(cold_storage_temp_data)
## 
## # get standard deviation of concerned column: Temperature
## sd_temp_1yr_data = sd(cold_storage_temp_data$Temperature)
## sd_temp_1yr_data
## 
## # get variance of concerned column: Temperature
## var_temp_1yr_data = var(cold_storage_temp_data$Temperature)
## var_temp_1yr_data
## 
## # get mode of concerned column: Temperature
## getmode(cold_storage_temp_data$Temperature)
## 
## # Histogram for Temperature Variable (1Yr Data)
## temperature_histogram =
##   ggplot(cold_storage_temp_data, aes(x = cold_storage_temp_data$Temperature)) +
##   geom_histogram(fill = "cornflowerblue", bins = 30) +
##   labs(title="Histogram for Temperature (1Yr Data)", x="Temperature", y="Count") +
##   scale_x_continuous(breaks=seq(1.4,max(cold_storage_temp_data$Temperature)+0.3,by=0.3))+
##   scale_y_continuous(breaks = seq(0, 40, by = 5)) +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Box Plot for temperature Variable (1Yr Data)
## temperature_boxplot =
##   ggplot(cold_storage_temp_data, aes(x = 0, y = cold_storage_temp_data$Temperature)) +
##   geom_boxplot(size=1, outlier.shape = 1, outlier.color="black", fill="cornflowerblue")+
##   labs(title = "Box Plot for Temperature (1Yr Data) ", x = "", y="Temperature") +
##   scale_y_continuous(breaks=seq(1.4,max(cold_storage_temp_data$Temperature)+0.3,by=0.3))+
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5)) +
##   theme(axis.text.y = element_blank()) +
##   coord_flip()
## 
## # Print charts side-by-side
## grid.arrange(temperature_histogram,temperature_boxplot,nrow=1,ncol= 2)
## 
## # Q1: calculate Temperature mean value for Summer, Winter and Rainy Season
## by( cold_storage_temp_data$Temperature,INDICES = cold_storage_temp_data$Season,FUN=mean)
## 
## # Q2: calculate Temperature mean value for the year
## mean_temp_1yr_data = mean(cold_storage_temp_data$Temperature)
## mean_temp_1yr_data
## 
## # Q3: calculate Temperature Standard deviation for the year
## sd_temp_1yr_data = sd(cold_storage_temp_data$Temperature)
## sd_temp_1yr_data
## 
## # Q4: On assumption of Normal Distribution, calculate probability of temperature < 2 C
## norm_prob_below_2 = pnorm(2,mean=mean_temp_1yr_data,sd=sd_temp_1yr_data)
## norm_prob_below_2
## 
## # Q5: On assumption of Normal Distribution, calculate probability of temperature > 4 C
## norm_prob_above_4 = 1 - pnorm(4,mean=mean_temp_1yr_data,sd=sd_temp_1yr_data)
## norm_prob_above_4
## 
## # Q6: On assumption of Normal Distribution, calculate probability
## # of temperature > 4 C OR temperature < 2 C
## norm_prob_outside_2_4 = norm_prob_below_2 + norm_prob_above_4
## norm_prob_outside_2_4
## 
## # Q7: calculate a one-way ANOVA
## aov_temp_1yr_data=aov(cold_storage_temp_data$Temperature~cold_storage_temp_data$Season,
##                       data=cold_storage_temp_data)
## 
## # test for differences between rainy, summer and winter Seasons
## TukeyHSD(aov_temp_1yr_data)
## 
## # PROBLEM 2:
## # In Mar 2018, Cold Storage started getting complaints from their clients
## # that they have been getting complaints from end consumers of the dairy products
## # going sour and often smelling. On getting these complaints, the supervisor pulls out
## # data of the last 35 days' temperatures. As a safety measure, the Supervisor decides
## # to be vigilant to maintain the temperature at 3.9 C or below.
## # Assume 3.9 C as the upper acceptable value for mean temperature and at alpha = 0.1.
## # Do you feel that there is a need for some corrective action in the Cold Storage
## # Plant or is it that the problem is from the procurement side from where Cold
## # Storage is getting the Dairy Products? The data of the last 35 days is in
## # "Cold_Storage_Mar2018.csv"
## 
## 
## # Preliminary analysis for problem 2
## # check dimension of dataset
## dim(cold_storage_mar2018_data)
## 
## #see first 6 rows(observations) of dataset
## head(cold_storage_mar2018_data)
## 
## #see last 6 rows(observations) of dataset
## tail(cold_storage_mar2018_data)
## 
## # check structure of dataset
## str(cold_storage_mar2018_data)
## 
## # get summary of dataset
## summary(cold_storage_mar2018_data)
## 
## # get standard deviation of concerned column: Temperature
## sd_temp_mar_data = sd(cold_storage_mar2018_data$Temperature)
## sd_temp_mar_data
## 
## # get variance of concerned column: Temperature
## var_temp_mar_data = var(cold_storage_mar2018_data$Temperature)
## var_temp_mar_data
## 
## # get mode of concerned column: Temperature
## getmode(cold_storage_mar2018_data$Temperature)
## 
## # Histogram for Temperature Variable (mar Data)
## temperature_histogram2 =
##   ggplot(cold_storage_mar2018_data, aes(x = cold_storage_mar2018_data$Temperature)) +
##   geom_histogram(fill = "cornflowerblue", bins = 10) +
##   labs(title="Histogram for Temperature (mar Data)", x="Temperature", y="Count") +
##   scale_x_continuous(breaks=seq(3.7,max(cold_storage_mar2018_data$Temperature)+0.1,by=0.1))+
##   scale_y_continuous(breaks = seq(0, 20, by = 2)) +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Box Plot for temperature Variable (mar Data)
## temperature_boxplot2 =
##   ggplot(cold_storage_mar2018_data, aes(x = 0, y = cold_storage_mar2018_data$Temperature)) +
##   geom_boxplot(size=1, outlier.shape = 1, outlier.color="black", fill="cornflowerblue")+
##   labs(title = "Box Plot for Temperature (mar Data) ", x = "", y="Temperature") +
##   scale_y_continuous(breaks=seq(3.7,max(cold_storage_mar2018_data$Temperature)+0.1,by=0.1))+
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5)) +
##   theme(axis.text.y = element_blank()) +
##   coord_flip()
## 
## # Print charts side-by-side
## grid.arrange(temperature_histogram2,temperature_boxplot2,nrow=1,ncol= 2)
## 
## # Q1: Which Hypothesis test shall be performed to check if corrective action is
## # needed at the cold storage plant?
## # ANS: A one Sample upper tailed T-test
## 
## 
## # Q2: State the Hypothesis, perform hypothesis test and determine p-value
## #   H0: E[Temperature] = 3.9 C
## #   HA: E[Temperature] > 3.9 C
## 
## 
## # perform upper tailed t.test on mar2018 data with true mean provided
## t.test(cold_storage_mar2018_data$Temperature,mu=3.9,alternative='greater')
## 
## 
## # Since pvalue is less than level of significance, we reject the hypothesis.
## # There is need for some corrective action in the Cold Storage Plant.
## # The problem is NOT from the procurement side from where Cold Storage is
## # getting the Dairy Products.
## 
## 
## #=======================================================================
## #
## # T H E - E N D
## #
## #=======================================================================
