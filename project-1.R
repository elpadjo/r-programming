#=======================================================================
#
# Exploratory Data Analysis - CardioGoodFitness
#
#=======================================================================

# Environment Set up and Data Import

# Invoking Libraries
library(tidyverse) # contains ggplot2,dplyr,forcats,lubridate etc
library(gridExtra) # Needed for plotting multiple ggplot graphs side-by-side
library(corrplot) # Needed to plot correlation plots
library(scales) # For Big Number formating
library(knitr) # Necessary to generate sourcecodes from a .Rmd File

# Setup Working Directory
setwd("C:/Users/USER/Documents/El-PaDJo/R programming language/1-Introduction to R & Statistics/week_3")

# Read Input File
cardio_data = read.csv("CardioGoodFitness.csv")

#making all columns accessible without '$' sign usage
attach(cardio_data) 

# Global options settings
options(scipen=999)  # turn off scientific notation like 1e+06

# Function to calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Variable Identification
# check dimension of dataset
dim(cardio_data)

#see first 6 rows(observations) of dataset
head(cardio_data)

#see last 6 rows(observations) of dataset
tail(cardio_data)

# check structure of dataset
str(cardio_data)

#Change fitness variable type to factor
cardio_data$Fitness = as.factor(Fitness)

# get summary of dataset
summary(cardio_data)

# Bar Plot for Product Variable
ggplot(cardio_data, aes(x = Product)) + 
  geom_bar(fill = "cornflowerblue", color="black") +
  labs(title="Bar Chart for Product", x="Products", y="Count") + 
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))

# Histogram for Age Variable
age_histogram = ggplot(cardio_data, aes(x = Age)) +
  geom_histogram(fill = "cornflowerblue", bins = 33) + 
  labs(title="Histogram for Age", x="Age", y="Count") + 
  scale_x_continuous(breaks = seq(18, max(Age)+5, by = 5)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))

# Box Plot for Age Variable
age_boxplot = ggplot(cardio_data, aes(x = 0, y = Age)) +
  geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", fill="cornflowerblue") +
  labs(title = "Box Plot for Age ", x = "", y="Age") +
  scale_y_continuous(breaks = seq(18, max(Age)+5, by = 5)) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.text.y = element_blank()) +
  coord_flip()

# Print charts side-by-side
grid.arrange(age_histogram,age_boxplot,nrow=1,ncol= 2)

# Bar Plot for Gender Variable
ggplot(cardio_data, aes(x = Gender)) + 
  geom_bar(fill = "cornflowerblue", color="black") +
  labs(title="Bar Chart for Gender", x="Gender", y="Count") + 
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))

# Histogram for Education Variable
edu_histogram = ggplot(cardio_data, aes(x = Education)) +
  geom_histogram(fill = "cornflowerblue", bins = 10) + 
   labs(title="Histogram for Education", x="Education Levels", y="Count") + 
  scale_x_continuous(breaks = seq(min(Education), max(Education), by = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 15)) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))

# Box for Education Variable
edu_boxplot = ggplot(cardio_data, aes(x = 0, y = Education)) +
  geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", fill="cornflowerblue") +
  labs(title = "Box Plot for Education ", x = "", y="Education") +
  scale_y_continuous(breaks = seq(min(Education), max(Education), by = 1)) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.text.y = element_blank()) +
  coord_flip()

# Print charts side-by-side
grid.arrange(edu_histogram,edu_boxplot,nrow=1,ncol= 2)

# Bar Plot for Marital Status Variable
ggplot(cardio_data, aes(x = MaritalStatus)) + 
  geom_bar(fill = "cornflowerblue", color="black") +
  labs(title="Bar Chart for Marital Status", x="Marital Status", y="Count") + 
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))

# Histogram for Usage Variable
usage_histogram = ggplot(cardio_data, aes(x = Usage)) +
  geom_histogram(fill = "cornflowerblue", bins = 6) + 
   labs(title="Histogram for Usage", x="Usage Levels", y="Count") + 
  scale_x_continuous(breaks = seq(min(Usage), max(Usage), by = 1)) +
  scale_y_continuous(breaks = seq(0, 80, by = 10)) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))

# Box for Usage Variable
usage_boxplot = ggplot(cardio_data, aes(x = 0, y = Usage)) +
  geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", fill="cornflowerblue") +
  labs(title = "Box Plot for Usage ", x = "", y="Usage") +
  scale_y_continuous(breaks = seq(min(Usage), max(Usage), by = 1)) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.text.y = element_blank()) +
  coord_flip()

# Print charts side-by-side
grid.arrange(usage_histogram,usage_boxplot,nrow=1,ncol= 2)

# Bar Plot for Fitness Variable
ggplot(cardio_data, aes(x = factor(Fitness))) + 
  geom_bar(fill = "cornflowerblue", color="black") +
  labs(title="Bar Chart for Fitness", x="Fitness", y="Count") + 
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))

# Histogram for Income Variable
income_histogram = ggplot(cardio_data, aes(x = Income)) +
  geom_histogram(fill = "cornflowerblue", bins = 50) + 
  labs(title="Histogram for Income", x="Income Levels", y="Count") + 
  scale_x_continuous(breaks = seq(20000, 120000, by = 10000), labels = comma) +
  scale_y_continuous(breaks = seq(0, 25, by = 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title=element_text(hjust=0.5))

# Box for Income Variable
income_boxplot = ggplot(cardio_data, aes(x = 0, y = Income)) +
  geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", fill="cornflowerblue") +
  labs(title = "Box Plot for Income ", x = "", y="Income") +
  scale_y_continuous(breaks = seq(20000, 120000, by = 10000), labels = comma) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text.y = element_blank()) +
  coord_flip()

# Print charts side-by-side
grid.arrange(income_histogram,income_boxplot,nrow=1,ncol= 2)

# Histogram for Miles Variable
miles_histogram = ggplot(cardio_data, aes(x = Miles)) +
  geom_histogram(fill = "cornflowerblue", bins = 35) + 
  labs(title="Histogram for Miles", x="Miles", y="Count") + 
  scale_x_continuous(breaks = seq(0, max(Miles)+10, by = 50)) +
  scale_y_continuous(breaks = seq(0, 40, by = 5))+
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))

# Box for Miles Variable
miles_boxplot = ggplot(cardio_data, aes(x = 0, y = Miles)) +
  geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", fill="cornflowerblue") +
  labs(title = "Box Plot for Miles ", x = "", y="Miles") +
  scale_y_continuous(breaks = seq(0, max(Miles)+10, by = 50)) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.text.y = element_blank()) +
  coord_flip()

# Print charts side-by-side
grid.arrange(miles_histogram,miles_boxplot,nrow=1,ncol= 2)

# Jitter box Plot for Product vs Age
ggplot(cardio_data, aes(x = Product, y = Age, color = Product)) +
  geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", outlier.size  = 3) +
  geom_jitter(alpha = 0.5, width=.2) + 
  labs(title = "Jitter Box Plot for Product Vs Age", x = "Products", y = "Age") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  coord_flip()

# Grouped Bar Chart For Product vs Gender
ggplot(cardio_data, aes(x = Product, fill = Gender)) + 
  geom_bar(position = position_dodge(preserve = "single")) +
  labs(title = "Grouped Bar Chart For Product vs Gender", x = "Products", y = "Count") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) 


# Jitter box Plot for Product vs Education
ggplot(cardio_data, aes(x = Product, y = Education, color = Product)) +
  geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", outlier.size  = 3) +
  geom_jitter(alpha = 0.5, width=.2) + 
  labs(title = "Jitter Box Plot for Product vs Education", x = "Products", y = "Education") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  coord_flip()

# Grouped Bar Chart For Product vs Marital Status
ggplot(cardio_data, aes(x = Product, fill = factor(MaritalStatus))) + 
  geom_bar(position = position_dodge(preserve = "single")) +
  labs(title = "Grouped Bar Chart For Product vs Marital Status", x = "Products", y = "Count") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) 

# Grouped Bar Chart For Product vs Usage Status
ggplot(cardio_data, aes(x = Product, y = Usage, color = Product)) +
  geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", outlier.size  = 3) +
  geom_jitter(alpha = 0.5, width=.2) + 
  labs(title = "Jitter Box Plot for Product vs Usage", x = "Products", y = "Usage") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  coord_flip()

# Grouped Bar Chart For Product vs Marital Status
ggplot(cardio_data, aes(x = Product, fill = factor(Fitness))) + 
  geom_bar(position = position_dodge(preserve = "single")) +
  labs(title = "Grouped Bar Chart For Product vs Fitness", x = "Products", y = "Count") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) 

# Jitter box Plot for Product vs Income
ggplot(cardio_data, aes(x = Product, y = Income, color = Product)) +
  geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", outlier.size  = 3) +
  geom_jitter(alpha = 0.5, width=.2) + 
  labs(title = "Jitter Box Plot for Product Vs Income", x = "Products", y = "Income") +
  scale_y_continuous(breaks = seq(20000, 120000, by = 5000), labels = comma) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# Jitter box Plot for Product vs Age
ggplot(cardio_data, aes(x = Product, y = Miles, color = Product)) +
  geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", outlier.size  = 3) +
  geom_jitter(alpha = 0.5, width=.2) + 
  labs(title = "Jitter Box Plot for Product Vs Miles", x = "Products", y = "Miles") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(legend.position = "none") +
  coord_flip()

#Make a Correllation plot of AGE, Education, Usage, Income and miles
corrplot(cor(cardio_data[,c(2,4,6,8,9)]))

# scatterplot with loess smoothed line 
# and better labeling and color
ggplot(cardio_data, aes(x = Education, y = Income)) +
  geom_point(color="cornflowerblue", size = 2, alpha = .6) +
  geom_smooth(size = 1.5, color = "tomato") +
  scale_y_continuous(labels = comma, limits = c(10000, 130000)) +
  scale_x_continuous(breaks = seq(min(Education), max(Education), by = 1), 
                     limits = c(11, 22)) +
  labs(title = "Scatterplot with Loess Smoothed Line for Education Vs Income", 
       x = "Education", y = "Income") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))

# check if NA field exists in the dataset
anyNA(cardio_data)

#=======================================================================
#
# T H E - E N D
#
#=======================================================================

#Generate the .R file to hold the source code
## 4. Conclusion And Recommendations
purl("project-1.Rmd", documentation = 0) 

## #=======================================================================
## #
## # Exploratory Data Analysis - CardioGoodFitness
## #
## #=======================================================================
## 
## # Environment Set up and Data Import
## 
## # Invoking Libraries
## library(tidyverse) # contains ggplot2,dplyr,forcats,lubridate etc
## library(gridExtra) # Needed for plotting multiple ggplot graphs side-by-side
## library(corrplot) # Needed to plot correlation plots
## library(scales) # For Big Number formating
## library(knitr) # Necessary to generate sourcecodes from a .Rmd File
## 
## # Setup Working Directory
## setwd("C:/Users/USER/Documents/El-PaDJo/R programming language/1-Introduction to R & Statistics/week_3")
## 
## # Read Input File
## cardio_data = read.csv("CardioGoodFitness.csv")
## 
## #making all columns accessible without '$' sign usage
## attach(cardio_data)
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
## # Variable Identification
## # check dimension of dataset
## dim(cardio_data)
## 
## #see first 6 rows(observations) of dataset
## head(cardio_data)
## 
## #see last 6 rows(observations) of dataset
## tail(cardio_data)
## 
## # check structure of dataset
## str(cardio_data)
## 
## #Change fitness variable type to factor
## cardio_data$Fitness = as.factor(Fitness)
## 
## # get summary of dataset
## summary(cardio_data)
## 
## # Bar Plot for Product Variable
## ggplot(cardio_data, aes(x = Product)) +
##   geom_bar(fill = "cornflowerblue", color="black") +
##   labs(title="Bar Chart for Product", x="Products", y="Count") +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Histogram for Age Variable
## age_histogram = ggplot(cardio_data, aes(x = Age)) +
##   geom_histogram(fill = "cornflowerblue", bins = 33) +
##   labs(title="Histogram for Age", x="Age", y="Count") +
##   scale_x_continuous(breaks = seq(18, max(Age)+5, by = 5)) +
##   scale_y_continuous(breaks = seq(0, 30, by = 5)) +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Box Plot for Age Variable
## age_boxplot = ggplot(cardio_data, aes(x = 0, y = Age)) +
##   geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", fill="cornflowerblue") +
##   labs(title = "Box Plot for Age ", x = "", y="Age") +
##   scale_y_continuous(breaks = seq(18, max(Age)+5, by = 5)) +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5)) +
##   theme(axis.text.y = element_blank()) +
##   coord_flip()
## 
## # Print charts side-by-side
## grid.arrange(age_histogram,age_boxplot,nrow=1,ncol= 2)
## 
## # Bar Plot for Gender Variable
## ggplot(cardio_data, aes(x = Gender)) +
##   geom_bar(fill = "cornflowerblue", color="black") +
##   labs(title="Bar Chart for Gender", x="Gender", y="Count") +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Histogram for Education Variable
## edu_histogram = ggplot(cardio_data, aes(x = Education)) +
##   geom_histogram(fill = "cornflowerblue", bins = 10) +
##    labs(title="Histogram for Education", x="Education Levels", y="Count") +
##   scale_x_continuous(breaks = seq(min(Education), max(Education), by = 1)) +
##   scale_y_continuous(breaks = seq(0, 100, by = 15)) +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Box for Education Variable
## edu_boxplot = ggplot(cardio_data, aes(x = 0, y = Education)) +
##   geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", fill="cornflowerblue") +
##   labs(title = "Box Plot for Education ", x = "", y="Education") +
##   scale_y_continuous(breaks = seq(min(Education), max(Education), by = 1)) +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5)) +
##   theme(axis.text.y = element_blank()) +
##   coord_flip()
## 
## # Print charts side-by-side
## grid.arrange(edu_histogram,edu_boxplot,nrow=1,ncol= 2)
## 
## # Bar Plot for Marital Status Variable
## ggplot(cardio_data, aes(x = MaritalStatus)) +
##   geom_bar(fill = "cornflowerblue", color="black") +
##   labs(title="Bar Chart for Marital Status", x="Marital Status", y="Count") +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Histogram for Usage Variable
## usage_histogram = ggplot(cardio_data, aes(x = Usage)) +
##   geom_histogram(fill = "cornflowerblue", bins = 6) +
##    labs(title="Histogram for Usage", x="Usage Levels", y="Count") +
##   scale_x_continuous(breaks = seq(min(Usage), max(Usage), by = 1)) +
##   scale_y_continuous(breaks = seq(0, 80, by = 10)) +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Box for Usage Variable
## usage_boxplot = ggplot(cardio_data, aes(x = 0, y = Usage)) +
##   geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", fill="cornflowerblue") +
##   labs(title = "Box Plot for Usage ", x = "", y="Usage") +
##   scale_y_continuous(breaks = seq(min(Usage), max(Usage), by = 1)) +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5)) +
##   theme(axis.text.y = element_blank()) +
##   coord_flip()
## 
## # Print charts side-by-side
## grid.arrange(usage_histogram,usage_boxplot,nrow=1,ncol= 2)
## 
## # Bar Plot for Fitness Variable
## ggplot(cardio_data, aes(x = factor(Fitness))) +
##   geom_bar(fill = "cornflowerblue", color="black") +
##   labs(title="Bar Chart for Fitness", x="Fitness", y="Count") +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Histogram for Income Variable
## income_histogram = ggplot(cardio_data, aes(x = Income)) +
##   geom_histogram(fill = "cornflowerblue", bins = 50) +
##   labs(title="Histogram for Income", x="Income Levels", y="Count") +
##   scale_x_continuous(breaks = seq(20000, 120000, by = 10000), labels = comma) +
##   scale_y_continuous(breaks = seq(0, 25, by = 5)) +
##   theme_minimal() +
##   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Box for Income Variable
## income_boxplot = ggplot(cardio_data, aes(x = 0, y = Income)) +
##   geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", fill="cornflowerblue") +
##   labs(title = "Box Plot for Income ", x = "", y="Income") +
##   scale_y_continuous(breaks = seq(20000, 120000, by = 10000), labels = comma) +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5)) +
##   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
##   theme(axis.text.y = element_blank()) +
##   coord_flip()
## 
## # Print charts side-by-side
## grid.arrange(income_histogram,income_boxplot,nrow=1,ncol= 2)
## 
## # Histogram for Miles Variable
## miles_histogram = ggplot(cardio_data, aes(x = Miles)) +
##   geom_histogram(fill = "cornflowerblue", bins = 35) +
##   labs(title="Histogram for Miles", x="Miles", y="Count") +
##   scale_x_continuous(breaks = seq(0, max(Miles)+10, by = 50)) +
##   scale_y_continuous(breaks = seq(0, 40, by = 5))+
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Box for Miles Variable
## miles_boxplot = ggplot(cardio_data, aes(x = 0, y = Miles)) +
##   geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", fill="cornflowerblue") +
##   labs(title = "Box Plot for Miles ", x = "", y="Miles") +
##   scale_y_continuous(breaks = seq(0, max(Miles)+10, by = 50)) +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5)) +
##   theme(axis.text.y = element_blank()) +
##   coord_flip()
## 
## # Print charts side-by-side
## grid.arrange(miles_histogram,miles_boxplot,nrow=1,ncol= 2)
## 
## # Jitter box Plot for Product vs Age
## ggplot(cardio_data, aes(x = Product, y = Age, color = Product)) +
##   geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", outlier.size  = 3) +
##   geom_jitter(alpha = 0.5, width=.2) +
##   labs(title = "Jitter Box Plot for Product Vs Age", x = "Products", y = "Age") +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5)) +
##   theme(legend.position = "none") +
##   coord_flip()
## 
## # Grouped Bar Chart For Product vs Gender
## ggplot(cardio_data, aes(x = Product, fill = Gender)) +
##   geom_bar(position = position_dodge(preserve = "single")) +
##   labs(title = "Grouped Bar Chart For Product vs Gender", x = "Products", y = "Count") +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## 
## # Jitter box Plot for Product vs Education
## ggplot(cardio_data, aes(x = Product, y = Education, color = Product)) +
##   geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", outlier.size  = 3) +
##   geom_jitter(alpha = 0.5, width=.2) +
##   labs(title = "Jitter Box Plot for Product vs Education", x = "Products", y = "Education") +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5)) +
##   theme(legend.position = "none") +
##   coord_flip()
## 
## # Grouped Bar Chart For Product vs Marital Status
## ggplot(cardio_data, aes(x = Product, fill = factor(MaritalStatus))) +
##   geom_bar(position = position_dodge(preserve = "single")) +
##   labs(title = "Grouped Bar Chart For Product vs Marital Status", x = "Products", y = "Count") +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Grouped Bar Chart For Product vs Usage Status
## ggplot(cardio_data, aes(x = Product, y = Usage, color = Product)) +
##   geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", outlier.size  = 3) +
##   geom_jitter(alpha = 0.5, width=.2) +
##   labs(title = "Jitter Box Plot for Product vs Usage", x = "Products", y = "Usage") +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5)) +
##   theme(legend.position = "none") +
##   coord_flip()
## 
## # Grouped Bar Chart For Product vs Marital Status
## ggplot(cardio_data, aes(x = Product, fill = factor(Fitness))) +
##   geom_bar(position = position_dodge(preserve = "single")) +
##   labs(title = "Grouped Bar Chart For Product vs Fitness", x = "Products", y = "Count") +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # Jitter box Plot for Product vs Income
## ggplot(cardio_data, aes(x = Product, y = Income, color = Product)) +
##   geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", outlier.size  = 3) +
##   geom_jitter(alpha = 0.5, width=.2) +
##   labs(title = "Jitter Box Plot for Product Vs Income", x = "Products", y = "Income") +
##   scale_y_continuous(breaks = seq(20000, 120000, by = 5000), labels = comma) +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5)) +
##   theme(legend.position = "none") +
##   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
##   coord_flip()
## 
## # Jitter box Plot for Product vs Age
## ggplot(cardio_data, aes(x = Product, y = Miles, color = Product)) +
##   geom_boxplot(size=1, outlier.shape = 1, outlier.color = "black", outlier.size  = 3) +
##   geom_jitter(alpha = 0.5, width=.2) +
##   labs(title = "Jitter Box Plot for Product Vs Miles", x = "Products", y = "Miles") +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5)) +
##   theme(legend.position = "none") +
##   coord_flip()
## 
## #Make a Correllation plot of AGE, Education, Usage, Income and miles
## corrplot(cor(cardio_data[,c(2,4,6,8,9)]))
## 
## # scatterplot with loess smoothed line
## # and better labeling and color
## ggplot(cardio_data, aes(x = Education, y = Income)) +
##   geom_point(color="cornflowerblue", size = 2, alpha = .6) +
##   geom_smooth(size = 1.5, color = "tomato") +
##   scale_y_continuous(labels = comma, limits = c(10000, 130000)) +
##   scale_x_continuous(breaks = seq(min(Education), max(Education), by = 1),
##                      limits = c(11, 22)) +
##   labs(title = "Scatterplot with Loess Smoothed Line for Education Vs Income",
##        x = "Education", y = "Income") +
##   theme_minimal() +
##   theme(plot.title=element_text(hjust=0.5))
## 
## # check if NA field exists in the dataset
## anyNA(cardio_data)
## 
## #Generate the .R file to hold the source code
## #Save all Sourcecodes in a corresponding .R file
## purl("project-1.Rmd", documentation = 0)
## 
## #=======================================================================
## #
## # T H E - E N D
## #
## #=======================================================================
## 
## ##
## ##
## 
