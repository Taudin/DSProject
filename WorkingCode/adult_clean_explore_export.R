#Load necessary packages.
library(ggplot2)
library(plyr)
library(gridExtra)
library(gmodels)
library(grid)
library(vcd)
library(scales)
library(ggthemes)
library(knitr)

#Downloading and Reading the Data.
#First we download the census dataset.  In order to do this we set the working directory
#setwd("/home/taudin/MiscFiles/Fall19/CSCI385/DSProject/CensusData")

#Then we download the file containing the training data.
#if (!file.exists("./adult.data")){
  #fileUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
 # download.file(fileUrl, destfile = "./adult.data")
#}

#Then we download the file that contains the test data.
#if (!file.exists("./adult.test")){
  #fileUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test"
  #download.file(fileUrl, destfile = "./adult.test")
#}

adult_train <- read.table("CensusData/adult.data", sep = ",", header = FALSE)

#Preliminary look at the data.
#The number of observations in the adult_train dataframe is:
dim(adult_train)[1]

#The number of variables are:
dim(adult_train)[2]

#The column names are such that each column is labeled ambiguously as "V1, V2, V3,..., etc. So we wil get the names from the attributes
#list available at https://archive.ics.uci.edu/ml/datasets/Census+Income .
colnames(adult_train) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship",
                          "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

#Lets look at the first few observations of the dataset.
head(adult_train, 10)

#Its structure:
str(adult_train, vec.len = 2, strict.width = "no", width = 30)

#We can see that the variables "age", "fnlwgt", "education_num", "capital_gain", "capital_loss", and "hours_per_week" are integer types.
#Other variables are factor variables with differing levels.
#In order to see what the levels of each factor variable are, we will provide a function "get_factor_levels()", which takes a dataframe as
#an argument, identifies the factor variables and prints the levels of each categorical variable.
get_factor_levels <- function(mydata){
  col_names <- names(mydata)
  for (i in 1:length(col_names)){
    if (is.factor(mydata[, col_names[i]])){
      message(noquote(paste("Covariate ", "*",
                            col_names[i], "*",
                            " with factor levels: ",
                            sep = "")))
      print(levels(mydata[, col_names[i]]))
    }
  }
}

get_factor_levels(adult_train)

#From the output above, we noticed that some of the factor variables have a level denoted by "?".  Those are missing values according to 
#documentation provided for the census dataset.

#Cleaning Missing Values

#Before we can proceed with the exploratory data analysis and predictive analysis, we have to get rid of the missing values.
#We read in the data file "adult.data" again, but with the additional option na.strings = "?". This makes sure all ?'s in the data will 
#be replaced with NA's.
adult_train <- read.table("CensusData/adult.data", sep = ",", header = FALSE, na.strings = " ?")

colnames(adult_train) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship",
                          "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

#After marking the missing values we can clean them with the function "na.omit()".
adult_train <- na.omit(adult_train)

#Re-enumerate the rows of the data.
row.names(adult_train) <- 1:nrow(adult_train)

#Transformations on the Data.

#From the boxplot and the summary of the variable "hours_per_week", we see that the mean number of working hours per week is 41, and at 
#least 50% of the people taking part in the survey work between 40 and 45 hours per week.
summary(adult_train$hours_per_week)

#The boxplot tells us about the many outliers:
ggplot(aes(x = factor(0), y = hours_per_week), data = adult_train) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", shape = 19, color = "red", cex = 2) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  xlab(label = "") +
  ylab(label = "Working Hours per Week") +
  ggtitle("Box Plot of Working Hours per Week")

#We  will group the working hours in 5 categories considered relevant. We will also create a new factor variable called "hours_worked" with 
#5 levels corresponding to these 5 categories.
adult_train$hours_worked[adult_train$hours_per_week < 40] <- " less_than_40"
adult_train$hours_worked[adult_train$hours_per_week >= 40 & adult_train$hours_per_week <= 45] <- " between_40_and_45"
adult_train$hours_worked[adult_train$hours_per_week > 45 & adult_train$hours_per_week <= 60] <- " between_45_and_60"
adult_train$hours_worked[adult_train$hours_per_week > 60 & adult_train$hours_per_week <= 80] <- " between_60_and_80"
adult_train$hours_worked[adult_train$hours_per_week > 80] <- " more_than_80"

#Note that we created a category " between_40_and_45" with greater than or equal and less than or equal, since we want the range of this 
#category to correspong to the span of the data points between the first and third quartiles.

#We make the new variable "hours_worked" a factor variable, we will use the "factor()" function to do so.
adult_train$hours_worked <- factor(adult_train$hours_worked, ordered = FALSE, levels = c(" less_than_40", " between_40_and_45", " between_45_and_60",
                                                                             " between_60_and_80", " more_than_80"))

#We can see how many people belong to each category of the factor variable "hours_worked":
summary(adult_train$hours_worked)

#Mentioned already was that a majority of people work between 40 and 45 hours a week, but there is also a considerable amount of folks
#working less than 40 and between 45 and 60 hours per week.
for (i in 1:length(summary(adult_train$hours_worked))){
  print(round(100 * summary(adult_train$hours_worked)[i] / sum(!is.na(adult_train$hours_worked)), 2))
}

#These percentages give us a proportion of how many fall into each category.

#The Variable "native_country".

#The factor variable "native_country" has 41 levels. When building a predictive model with "native_country" as a covariate, it will give 
#us 41 degrees of freedom and unnecessarily complicate the analysis and possibly lead to overfitting.  We will coarsen the data by using
#global regions instead.
levels(adult_train$native_country)

#First, we'll define the regions.
Asia_East <- c(" Cambodia", " China", " Hong", " Laos", " Thailand", " Japan", " Taiwan", " Vietnam")
Asia_Central <- c(" India", " Iran")
Central_America <- c(" Cuba", " Guatemala", " Jamaica", " Nicaragua", " Puerto-Rico", " Dominican-Republic", " El-Salvador", " Haiti",
                     " Honduras", " Mexico", " Trinidad&Tobago")
South_America <- c(" Ecuador", " Peru", " Columbia")
Europe_West <- c(" England", " Germany", " Holand-Netherlands", " Ireland", " France", " Greece", " Italy", " Portugal", " Scotland")
Europe_East <- c(" Poland", " Yugoslavia", " Hungary")

#Modify the dataframe by adding an additional column named "native_region".
adult_train <- mutate(adult_train, native_region = ifelse(native_country %in% Asia_East, " East-Asia",
                                                        ifelse(native_country %in% Asia_Central, " Central-Asia",
                                                        ifelse(native_country %in% Central_America, " Central-America",
                                                        ifelse(native_country %in% South_America, " South-America",
                                                        ifelse(native_country %in% Europe_West, " Europe-West",
                                                        ifelse(native_country %in% Europe_East, " Europe-East",
                                                        ifelse(native_country == " United-States", " United-States", "Outlying-US")))))
                                                                                                           )))

#Transform the new variable, "native_region" into a factor.
adult_train$native_region <- factor(adult_train$native_region, ordered = FALSE)

#The Variables "capital_gain" and "capital_loss"

#From the summary below we see that at least 50% of the variables "capital_gain" and "capital_loss" are zeros.
summary(adult_train$capital_gain)
summary(adult_train$capital_loss)

#The percentage of zeros in "capital_gain is 91.59%!
(nrow(subset(adult_train, adult_train$capital_gain == 0)) / nrow(adult_train)) * 100

#The percentage of zeros in "capital_loss" is also very high...
(nrow(subset(adult_train, adult_train$capital_loss == 0)) / nrow(adult_train)) * 100

#As we also saw from the summary above, the mean values of "capital_gain" and "capital_loss" with zero values included are, respectively:
mean_gain <- mean(adult_train$capital_gain)
mean_loss <- mean(adult_train$capital_loss)
kable(data.frame(Mean_Capital_Gain = mean_gain, Mean_Capital_Loss = mean_loss), caption = "Mean Capital with Zero Values Included")

#We give the mean capital gain and loss also in the case of all the zero values removed:
mean_gain <- mean(subset(adult_train$capital_gain, adult_train$capital_gain > 0))
mean_loss <- mean(subset(adult_train$capital_loss, adult_train$capital_loss > 0))
kable(data.frame(Mean_Capital_Gain = mean_gain, Mean_Capital_Loss = mean_loss), caption = "Mean Capital Only for Nonzero Values")

#We show a summary of the nonzero values of "capital_loss", and "capital_gain", as well as their respective interquartile ranges.
iqr_gain <- IQR(subset(adult_train$capital_gain, adult_train$capital_gain > 0))
iqr_loss <- IQR(subset(adult_train$capital_loss, adult_train$capital_loss > 0))
quantile_gain <- quantile(x = subset(adult_train$capital_gain, adult_train$capital_gain > 0), probs = seq(0, 1, 0.25))
quantile_loss <- quantile(x = subset(adult_train$capital_loss, adult_train$capital_loss > 0), probs = seq(0, 1, 0.25))
kable(x = data.frame(Capital_Gain = quantile_gain, Capital_Loss = quantile_loss), caption = "Quantile of the Nonzero Capital")
kable(x = data.frame(IQR_Capital_Gain = iqr_gain, IQR_Capital_Loss = iqr_loss), caption = "IQR of the Nonzero Capital")

#Notice that the IQR of the nonzero capital gain is much larger than that of the capital loss. We display a boxplot of the nonzero
#capital gain.
ggplot(aes(x = factor(0), y = capital_gain),
       data = subset(adult_train, adult_train$capital_gain > 0)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               shape = 19,
               color = "red",
               cex = 2) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 100000, 5000)) +
  ylab("Capital Gain") +
  xlab("") +
  ggtitle("Box Plot of Nonzero Capital Gain")

#From the boxplot we see that the bulk of the data is between 3,000 and 15,000 dollars and there are a few outliers. Next, we'll show
#a histogram of the nonzero capital gain:
df <- adult_train[adult_train$capital_gain > 0,]
ggplot(data = df, aes(x = df$capital_gain)) +
  geom_histogram(binwidth = 5000,
                 color = "black",
                 fill = "lightblue",
                 alpha = 0.4) +
  scale_y_continuous(breaks = seq(0, 4000, 100)) +
  labs(x = "Capital Gain", y = "Count") +
  ggtitle("Histogram of Nonzero Capital Gain")

#The histogram confirms what we've already observed, the majority of people with positive capital gain have a captital gain between
#0 ad $25,000 and there are about 150 people with capital gain of around $100,000. The biggest number of people with positive capital gain
#are those with about $5,000.
#Below is the box plot of the nonzero capital loss values.
ggplot(aes(x = factor(0), y = capital_loss), data = subset(adult_train, adult_train$capital_loss > 0)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", shape = 19, color = "red", cex = 2) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 5000, 500)) +
  ylab("Capital Loss") +
  xlab("") +
  ggtitle("Box Plot of Nonzero Capital Loss")

#Display a histogram of the nonzero capital loss:
df <- adult_train[adult_train$capital_loss > 0,]
ggplot(data = df, aes(x = df$capital_loss)) +
  geom_histogram(color = "black", fill = "lightblue", alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 5000, 250)) +
  scale_y_continuous(breaks = seq(0, 450, 50)) +
  labs(x = "Capital Loss", y = "Count") +
  ggtitle("Histogram of Nonzero Capital Loss")

#The box plot tells us that most values are between $1,700 and $2,000 and there are many outliers. The largest number of people have a
#capital loss of about $1,875.

#Based on these results, we will group the values of the variables "capital_loss", and "capital_gain" into categories and we will create
#two new factor variables called "cap_gain" and "cap_loss".

#We will mark all values of "capital_gain" which are less than the first quartile of the nonzero capital gain as "Low"; all values that
#are between the first and third quartile as "Medium"; and all values greater than or equal to the third quartile are marked "High".

#We mark alll values of "capital_loss" which are less than the first quartile of the nonzero capital gain as "Low", all values that are
#between the first and third quartile as "Medium", and all values greater than or equal to the third quartile are marked "High".
adult_train <- mutate(adult_train, cap_gain = ifelse(adult_train$capital_gain < 3464, "Low",
                                            ifelse(adult_train$capital_gain >= 3464 & adult_train$capital_gain <= 14080, "Medium", "High")))
adult_train$cap_gain <- factor(adult_train$cap_gain, ordered = TRUE, levels = c("Low", "Medium", "High"))

adult_train <- mutate(adult_train, cap_loss = ifelse(adult_train$capital_loss< 1672, "Low",
                                            ifelse(adult_train$capital_loss >= 1672 & adult_train$capital_loss <= 1977, "Medium","High")))
adult_train$cap_loss <- factor(adult_train$cap_loss, ordered = TRUE, levels = c("Low", "Medium", "High"))

#The Variable "workclass".

#We notice that there is one unused factor level in the variable "workclass", the level "Never-worked".
summary(adult_train$workclass)

#We will remove the facotr level "Never-worked" from the categorical variable "workclass".
adult_train$workclass <- droplevels(adult_train$workclass)
levels(adult_train$workclass)

#Preprocessing the Test Dataset

#The census data comes with a separate test data set, which we use to test out-of-sample accuracy of the constructed predictive models.
#We repeat the same steps as in the transformation of the training dataframe "adult_train".
adult_test <- read.table("CensusData/adult.test", sep = ",", header = FALSE, skip = 1, na.strings = " ?")
colnames(adult_test) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship",
                          "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

#Cleaning missing values from the test data.
adult_test <- na.omit(adult_test)
row.names(adult_test) <- 1:nrow(adult_test)

#Taking a look at what we have.
head(adult_test, 10)

#From the display of the first 5 observations of the data, we notice that the names of the levels of the factor variable "income" differ
#from the respective names in the training data "adult_train" by the symbol ".". We remove the "." from the names of the factor levels
#of "income" in the test data.
levels(adult_test$income)[1] <- "<=50K"
levels(adult_test$income)[2] <- ">50K"
levels(adult_test$income)

#Just like the training data we create a new variable called "hours_worked".
adult_test$hours_worked[adult_test$hours_per_week < 40] <- "less_than_40"
adult_test$hours_worked[adult_test$hours_per_week >= 40 & adult_test$hours_per_week <= 45] <- "between_40_and_45"
adult_test$hours_worked[adult_test$hours_per_week > 45 & adult_test$hours_per_week <= 60] <- "between_45_and_60"
adult_test$hours_worked[adult_test$hours_per_week > 60 & adult_test$hours_per_week <= 80] <- "between_60_and_80"
adult_test$hours_worked[adult_test$hours_per_week > 80] <- "more_than_80"

adult_test$hour_w <- factor(adult_test$hours_worked, ordered = FALSE,
                            levels = c("less_than_40", "between_40_and_45", "between_45_and_60", "between_60_and_80", "more_than_80"))

#We also have to create the variable "native_region".
adult_test <- mutate(adult_test, native_region = ifelse(native_country %in% Asia_East, "East-Asia",
                                                 ifelse(native_country %in% Asia_Central, "Central-Asia",
                                                 ifelse(native_country %in% Central_America, "Central-America",
                                                 ifelse(native_country %in% South_America, "South-America",
                                                 ifelse(native_country %in% Europe_West, "Europe-West",
                                                 ifelse(native_country %in% Europe_East, "Europe-East",
                                                 ifelse(native_country == "United-States", "United-States", "Outlying-US"))))))))
adult_test$native_region <- factor(adult_test$native_region, ordered = FALSE)

#Create the variables "cap_gain" and "cap_loss" .
adult_test <- mutate(adult_test, cap_gain = ifelse(adult_test$capital_gain < 3464, "Low",
                                            ifelse(adult_test$capital_gain >= 3464 & adult_test$capital_gain <= 14080, "Medium", "High")))
adult_test$cap_gain <- factor(adult_test$cap_gain, ordered = FALSE, levels = c("Low", "Medium", "High"))

adult_test <- mutate(adult_test, cap_loss = ifelse(adult_test$capital_loss < 1672, "Low",
                                            ifelse(adult_test$capital_loss >= 1672 & adult_test$capital_loss <= 1977, "Medium", "High")))
adult_test$cap_loss <- factor(adult_test$cap_loss, ordered = FALSE, levels = c("Low", "Medium", "High"))

#We drop the unused level "Never-worked" from the factor variable "workclass".
adult_test$workclass <- droplevels(adult_test$workclass)

#Exporting the Transformed Datasets
 
#We export the cleaned and preprocessed train and test datasets into the csv files "adult_df.csv" and "test_df.csv" respectively.
#Change the directory so that the csv's go where they should.
setwd("CensusData")
write.csv(adult_train, "adult_df.csv", row.names = FALSE)
write.csv(adult_test, "test_df.csv", row.names = FALSE)
  