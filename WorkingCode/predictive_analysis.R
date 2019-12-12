
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
glm_model <- glm(formula = form, data = adult_train, family = binomial(link = "logit"), x = TRUE)
#The option "x = TRUE" returns the design matrix.
time_logistic <- proc.time() - start_time
time_logistic

##Collinearity of the Predictor Variables

#We investigate whether there are collinear predictor variables.
summary(glm_model)$coefficients[, 1:2]

#We notice the following warning message: "Coefficients:(1 not defined because of singularities)". In the 
#coefficients table the coefficient for the variable education_num is NA.  This is an indication that the 
#covariate education_num is collinear with some other predictors.  We have to exclude education_num from the
#list of predictor variables and fit the model again.  For that, we use the R function findLinearCombos() from
#the package caret to test whether the covariate education_num is collinear with some of the other predictors.
#findLinearCombos() determines linear combinations in a numeric matrix and returns a list that enumerates these
#dependencies and a vector of column positions that can be removed to eliminate the linear dependencies:
findLinearCombos(glm_model$x)
findLinearCombos(glm_model$x)$remove

#R found linear dependencies between the covariates and recommends to remove column 24 from the design matrix.
#We identify which predictor corresponds to column 24:
colnames(glm_model$x)[findLinearCombos(glm_model$x)$remove]

#It turns out that the problematic variable is education_num.  The variable education_num provides redundant
#information, equivalent to that contained in the variable education.  We can also see that the two variables
#are linearly dependent, i.e., collinear.  We list the unique combination of values for the variables education 
#and education_num.  The variable education has a total of 16 factor levels and we see that for each level of 
#education there corresponds a number from 1 to 16 from the variable education_num
unique_combinations <- unique(adult_train[, c("education", "education_num")])
unique_combinations[order(unique_combinations$education_num),]

#we remove the covariate education_num and fit a new model-- glm_model_wld ("wld" for "without linear dependencies"),
#thereby resolving the problem with linearly dependent predictors:
new_covariates <- paste("age", "workclass", "education", "marital_status", "occupation", "relationship", "race",
                        "sex", "native_region", "hours_worked", "cap_gain", "cap_loss", sep = "+")
new_form <- as.formula(paste("income ~ ", new_covariates))

start_time <- proc.time()
glm_model_wld <- glm(formula = new_form, data = adult_train, family = binomial(link = "logit"),
                     x = TRUE, y = TRUE)
time_logistic <- proc.time() - start_time
time_logistic

#There are no linear dependencies between the covariates anymore:
findLinearCombos(glm_model_wld$x)

##Goodman and Kruskal's tau measure

#Another way to see  if there exist any correlations between the covariates is to calculate the Goodman and
#Kruskal's tau measure for all pairs of covariates.  The Goodman and Kruskal's tau measure will give us the 
#strength of the association between predictors two-by-two.  If there are three or more mutually dependent
#predictors, this coefficient will not help us identify them.  The tau measures from 0 to 1 and values closer
#to zero indicate weak association, whereas values closer to 1 indicate strong association.  The function
#GKtauDataframe() returns an object of class GKtauMatrix and the plot() method can be applied for this object
#class, visualizing in a convenient way the Goodman and Kruskal's tau for all pairs of predictors.  The plot 
#is in the form of a matrix.  The numbers on the diagonal are equal to the number of levels for each categorical
#variable, while the off diagonal elements give the Goodman Kruskal tau values.  Each tau measure is represented
#graphically by an ellipse, which is a circle for tau = 0 and degenerates into a straight line for tau = 1.
GKmatrix <- GKtauDataframe(adult_train[, c("age", "workclass", "education", "education_num", "marital_status",
                                           "occupation", "relationship", "race", "sex", "hours_worked",
                                           "native_region", "cap_gain", "cap_loss")])
plot(GKmatrix)

#From the resulting plot above, we see that education is perfectly predictable from education_num and vice-versa.
#This confirms our conclusion that the two variables are collinear.  All other tau values are close to zero, 
#except for tau(relationship, marital_status), tau(relationship, sex), and tau(marital_status, relationship).
#These numbers make sense, because relationshp can be predicted by marital_status and the other way around.
#The only association which is not intuitive is that between relationship and sex.  The tau value of 0.42 
#suggests that being a female or male can determine the type of relationship that an individual is in.  We see
#that 36% of women are Not-in-family compared to 20% of men, and 25% of women are Unmarried in contrast to only
#4% of men.
tab <- xtabs(~sex + relationship, data = adult_train)
ptab <- prop.table(tab, 1)
print(format(round(ptab, 2), scientific = FALSE))

#Now we compute the Cramer's V value for the above pairs of variables.  The values we obtain for Cramer's V indicate
#strength of association which is similar to the one predicted by Goodman and Kruskal's tau:
assocstats(tab)$cramer
tab1 <- xtabs(~marital_status + relationship, data = adult_train)
assocstats(tab1)$cramer


# Performance of the Fitted Model -----------------------------------------

#Performance on the training data

#We calculate the percentage of correctly guessed response variables using the training dataset adult_train.
attributes(adult_train$income)

#The response variable modeled is >50K, i.e., when fittling the logistic regression model, the probability of
#the income being greater than 50K is calculated.  We denote the vector with the predicted income values as 
#predicted_income_train:
predicted_probs <- predict(glm_model_wld, type = "response")
predicted_income_train <- ifelse(predicted_probs > 0.5, " >50K", " <=50K")

#There is an 84.9% match between observed and predicted values of income:
mean(predicted_income_train == adult_train$income)

