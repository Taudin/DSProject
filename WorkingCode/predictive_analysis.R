
#We start by loading the necessary packages:
library(ggplot2)
library(scales)
library(plyr)
library(vcd)
library(ggthemes)
library(caret)
library(GoodmanKruskal)
library(ResourceSelection)
library(randomForest)
library(e1071)
library(nnet)
library(DMwR)


# Reading in the Preprocessed Datasets ------------------------------------

##Set the working directory
setwd("CensusData")
#We read the train and test datasets into the adult_train and adult_test dataframes respectively:
adult_train <- read.csv("adult_df.csv")
adult_test <- read.csv("test_df.csv")


# Logistic Regression -----------------------------------------------------

#We are interested in predicting the values of the variable income.  Our response variable, income, assumes only
#two values: less than 50K/year and more than 50k/year.  We consider this a classification problem.  If Y(i) is 
#the random variable (income of the ith subject), then Y~binom(n,p).

names(adult_train)
covariates <- paste("age", "workclass", "education", "education_num", "marital_status", "occupation", "relationship",
                    "race", "sex", "native_region", "hours_worked", "cap_gain", "cap_loss", sep = "+")
form <- as.formula(paste("income ~ ", covariates))

start_time <- proc.time()
glm.model <- glm(formula = form, data = adult_train, family = binomial(link = "logit"), x = TRUE)
#The option "x = TRUE" returns the design matrix.
time.logistic <- proc.time() - start_time
time.logistic
