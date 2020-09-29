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
#str(life_ex_DF)
# converting status to factor column
life_ex_DF$status <- as.factor(life_ex_DF$status)


## checking for missing data
# checking for missing values in the data
#vis_miss(life_ex_DF) + xlab("Data Columns")


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
#vis_miss(life_ex_cleaned_DF) + xlab("Data Columns")

# summary of cleaned up data
#summary(life_ex_cleaned_DF)

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

LED_outcome <- LED %>% select(life_expectancy)
LED.df <- as.data.frame(lapply(LED[,-1], normalize)) #normalizes data and removes life expectancy as response variable to be predicted

str(LED_outcome)
str(LED.df)

##knn.reg model
install.packages("FNN")
library(FNN)

set.seed(1234) # set the seed to make the partition reproducible

# 75% of the sample size
smp_size <- floor(0.75 * nrow(LED.df))

train_ind <- sample(seq_len(nrow(LED.df)), size = smp_size)

# creating test and training sets that contain all of the predictors
reg_pred_train <- LED.df[train_ind, ]
reg_pred_test <- LED.df[-train_ind, ]

LED_outcome_train <- LED_outcome[train_ind, ]
LED_outcome_test <- LED_outcome[-train_ind, ]

model_error = data.frame(matrix(ncol = 2, nrow = 100))
colnames(model_error) <- c('MSE','MAE')

#cycles through k = 1 to 100 storing all mean error values
for(i in 1:100)
{
  
#knn predictions
reg_results <- knn.reg(reg_pred_train, reg_pred_test, LED_outcome_train, k = i)

#mean square prediction error
model_error[i,1] = mean((LED_outcome_test - reg_results$pred) ^ 2)

#mean absolute error
model_error[i,2] = mean(abs(LED_outcome_test - reg_results$pred))
}

min(model_error$MSE) #retrieves minimum errorobtained
MSE_val <- which(model_error$MSE == min(model_error$MSE)) #finds the k value that gives the min MSE
min(model_error$MAE) #retrieves minimum errorobtained
MAE_val <- which(model_error$MAE == min(model_error$MAE)) #finds the k value that gives the min MAE

model_error[MSE_val,]
model_error[MAE_val,]

#Results of K that produced lowest MSE
reg_results <- knn.reg(reg_pred_train, reg_pred_test, LED_outcome_train, k = 3)

plot(LED_outcome_test~reg_results$pred, cex = .8, col = "dodgerblue", main = "k = 3", xlab="Predicted Values", ylab="Actual Values")
reg_line = lm(LED_outcome_test~reg_results$pred)
abline(reg_line, col = "red")
