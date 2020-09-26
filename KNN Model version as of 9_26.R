#KNN Model
library(dplyr)
library(ggplot2)
LED=Life.Expectancy.Data
str(LED)

library(ggplot2)
library(ggthemes)
library(gridExtra)
library(tidyverse)
library(plyr)
library(dplyr) 
library(naniar) #keep
library(zoo)
# create not in operator
`%notin%` <- Negate(`%in%`)

##CLEANING DATA by Neil Benson
########### Loading the data
life_ex_DF <- Life.Expectancy.Data
# cleaning up column names
colnames(life_ex_DF) <- tolower(colnames(life_ex_DF))
colnames(life_ex_DF) <- gsub("\\.\\.",".",colnames(life_ex_DF))
colnames(life_ex_DF) <- gsub("\\.","_",colnames(life_ex_DF))
names(life_ex_DF)[19] <- "thinness_10_19_years"
# reviewing the structure of the data
str(life_ex_DF)
# converting status to factor column
life_ex_DF$status <- as.factor(life_ex_DF$status)


## checking for missing data
# checking for missing values in the data
vis_miss(life_ex_DF) + xlab("Data Columns")


## fix missing data
# sorting the columns so that life_expectancy column is first
life_ex_DF <- life_ex_DF %>% select("life_expectancy", everything())
factorcols <- c("status","country","year")
allcols <- colnames(life_ex_DF)
# selecting only numerical columns for correlation
corvars <- allcols[allcols %notin% factorcols]
# impute with median missing values per country
life_ex_median_DF <- as.data.frame(life_ex_DF %>%
                                     group_by(country) %>%
                                     mutate_at(corvars, na.aggregate, FUN=median))
# dropping rows where the country has a single entry/record in the dataset and their records are mostly incomplete
life_ex_median_DF <- life_ex_median_DF[!is.na(life_ex_median_DF$life_expectancy), ]
# imputing the median by country still left a few nulls throughout. We will then impute the rest of the missing values as the mean of stats (developpoing, developed, by year)
life_ex_cleaned_DF <- as.data.frame(life_ex_median_DF %>%
                                      group_by(status,year) %>%
                                      mutate_at(corvars, na.aggregate, FUN=median))
# checking for missing values in the data
vis_miss(life_ex_cleaned_DF) + xlab("Data Columns")

# summary of cleaned up data
summary(life_ex_cleaned_DF)

#changing variable name to what I was using in my code from Neil's
LED = life_ex_cleaned_DF

##factorizing status into 0-1
x = LED$status
x = factor(x)
LED$status <- sapply(as.integer(x), switch, "Developing" = 0, "Developed" = 1)

#removes unecessary variables and non-numeric values to avoid KNN NAbyCoercion
LED = subset(LED, select = -c(thinness_5_9_years, gdp, under_five_deaths,population, country))

#Data Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

LED.df <- as.data.frame(lapply(LED.subset.clean[,-1], normalize)) #normalizes data and removes life expectancy as response variable to be predicted

#splicing into test/train set
set.seed(123)
dat.d <- sample(1:nrow(LED.df), size=nrow(LED.df)*0.7, replace = FALSE) #random selection for 70% of data

train.LED <- LED[dat.d,] # 70% of data
test.LED <- LED[-dat.d,] # 30% of data

#creates separate data frame for 'life expectancy' feature
train.LED_labels <- LED[dat.d,1] #1 for life expectency
test.LED_labels <- LED[-dat.d,1]


library(class)
#NROW(train.LED_labels) # number of observations indicates knn(38) sqrt of 1458

ACC.model = data.frame(matrix(ncol = 1, nrow = 0))

for(i in 1:10){
  knn.model <- knn(train=train.LED, test=test.LED, cl=train.LED_labels, k=i) #gave errors due to non-numeric variables, attempting to remove non-numeric values (country & status)
  
  #accuracy test
  ACC.model[i,] = 100* sum(test.LED_labels == knn.model)/NROW(test.LED_labels)
}

#convert knn and test set to dataframes for analysis
knn.df <- as.data.frame(knn.model)
test.LED_labels.df <- as.data.frame(test.LED_labels)

max(ACC.model) #retrieves maximum accuracy obtained
which(ACC.model == max(ACC.model)) #finds the k value that gives the maximum accuracy

table(knn.model , test.LED_labels)    

library(caret)
confusionMatrix(table(knn.model ,test.LED_labels))