#Load the necessary libraries.
library(ggplot2)
library(plyr)
library(gridExtra)
library(gmodels)
library(grid)
library(vcd)
library(scales)
library(ggthemes)

#Set the working directory and then read the preprocessed training data into the adult_data dataframe.
setwd("CensusData")
adult_data <- read.csv("adult_df.csv")


# The Variable "income" ---------------------------------------------------

#income is a factor variable with two levels:
class(adult_data$income)
levels(adult_data$income)

#Show a summary statistic:
summary(adult_data$income)

#Visualize the above results with a bar plot.
ggplot(data = adult_data,
       mapping = aes(x = adult_data$income, fill = adult_data$income)) +
  geom_bar(mapping = aes(y = (..count..) / sum(..count..))) +
  geom_text(mapping = aes(label = scales::percent((..count..) / sum(..count..)),
                          y = (..count..) / sum(..count..)),
            stat = "count",
            vjust = -.1) +
  labs(x = "Income",
       y = "",
       fill = "Income") +
  scale_y_continuous(labels = percent)

#The graph shows us the percentage of people earning less than 50k a year and more than 50K.  75.1% of the 
#participants are paid less than 50K and 24.9 are paid more than 50K.


# The Variables capital_gain/cap_gain and capital_loss/cap_loss -----------

#For people earning more than 50K a year, the bulk of the values (50% of the data points) as well as the median, 
#and the mean value of the capital gain are significantly greater than those of people earning less than 50K:
ggplot(mapping = aes(x = income, y = capital_gain),
       data = subset(adult_data, adult_data$capital_gain > 0)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               shape = 19,
               color = "red",
               cex = 2) +
  coord_cartesian(ylim = c(0, 30000)) +
  scale_y_continuous(breaks = seq(0, 30000, 1500)) +
  labs(x = "Income",
       y = "Capital Gain") +
  ggtitle("Boxplot of Nonzero Capital Gain by Income")

#Next, we give the boxplot of nonzero capital loss grouped by income.  We observe the same tendency for 
#capital gain. Possibly due to the fact that people with higher income are more prone to invest more often,
#leading to higher chances of not only good, but bad investments that result in bigger losses.
ggplot(mapping = aes(x = income, y = capital_loss),
       data = subset(adult_data, adult_data$capital_loss > 0)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               shape = 19,
               color = "red", 
               cex = 2) +
  coord_cartesian(ylim = c(0, 3000)) +
  scale_y_continuous(breaks = seq(0, 3000, 200)) +
  labs(x = "Income",
       y= "Capital Loss") +
  ggtitle("Boxplot of Nonzero Capital Loss by Income")

#We see the strong evidence for a relationship between the nonzero values of capital_gain and capital_loss with income.
#However, we will not include these variables in the predictive model because of the extremely high number of
#zeros among these variables.
#For the predictive analysis, we will include the categorical variables cap_gain and cap_loss as covariates,
#but we will also fit a predictive model without them.  If cap_gain and cap_loss do not contribute to the
#prediction accuracy of the model, we will drop them.


# cap_gain and cap_loss ---------------------------------------------------

#We will examine the relationship between the factor variables cap_gain and cap_loss and the categorical
#variable income.
#We will show bar plots of the two variables grouped by income.

lg_cap_gain <- lapply(X = levels(adult_data$income), FUN = function(v){
  df <- subset(adult_data, adult_data$income == v)
  df <- within(df, cap_gain <- factor(cap_gain,
                                      levels = names(sort(table(cap_gain),
                                                          decreasing = TRUE))))
  ggplot(data = df,
         aes(x = cap_gain,
             fill = cap_gain)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                  y = (..count..) / sum(..count..)),
              stat = "count",
              vjust = -.1) +
    labs(x = "Capital Gain",
         y = "",
         fill = "Capital Gain") +
    theme(legend.position = "none") +
    ggtitle(paste("Income", v, sep = "")) +
    scale_y_continuous(labels = percent)})

grid.arrange(grobs = lg_cap_gain, ncol = 2)

#We see that 0% of people who earn less than 50K a year have high capital gain.
#In order to check if this result is due to some rounding error, we display the number of individuals with
#income of less than 50K and high capital gain:
nrow(subset(adult_data, adult_data$cap_gain == "High" &
              adult_data$income == " <=50K"))

#Indeed there are people with low annual income and high capital gain.
#The bar plot also shows that the proportion of people who have medium and high capital gains is much bigger
#within the group of people with income of more than 50K a year compared to the respective proportion within
#the group of people with income of less than 50K.
#We can conclude that there is a relationship between cap_gain and income.

#Consider the variable cap_loss:
lg_cap_loss <- lapply(levels(adult_data$income), function(v){
  df <- subset(adult_data, adult_data$income == v)
  df <- within(df, cap_loss <- factor(cap_loss,
                                      levels = names(sort(table(cap_loss),
                                                          decreasing = TRUE))))
  ggplot(data = df,
         aes(x = cap_loss,
             fill = cap_loss)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                  y = (..count..) / sum(..count..)),
              stat = "count",
              vjust = -.1) +
    labs(x = "Capital Loss",
         y = "",
         fill = "Capital Loss") +
    theme(legend.position = "none") +
    ggtitle(paste("Income", v, sep = "")) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = lg_cap_loss, ncol = 2)

#We observe the same trend as in the case of the variable cap_gain.


# The variable age --------------------------------------------------------

#Display a summary of the variable age as well as its IQR.
summary(adult_data$age)
IQR(adult_data$age)

#To visualize the summary statistics we show a boxplot of the variable age:
ggplot(mapping = aes(x = factor(0), y = age),
       data = adult_data) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               shape = 19,
               color = "red",
               cex = 2) +
  coord_cartesian(ylim = c(10, 100)) +
  scale_y_continuous(breaks = seq(10, 100, 5)) +
  ylab("Age") +
  xlab("") +
  ggtitle("Boxplot of Age") +
  scale_x_discrete(breaks = NULL)

#From the histogram below, we can see that the bulk of individuals are between 20 and 50 years old:
qplot(x = adult_data$age,
      data = adult_data,
      binwidth = 5,
      color = I("black"),
      fill = I("#F29025"),
      xlab = "Age",
      ylab = "Count",
      main = "Histogram of Age") +
  scale_x_continuous(breaks = seq(0, 95, 5)) +
  scale_y_continuous(breaks = seq(0, 4500, 500))

#We display the empirical density of age grouped by income and see that a majority of people earning more than
#50K a year are between 33 and 55 years old, whereas the greater number of people who earn less than 50K a year 
#are between 18 and 45:
ggplot(data = adult_data, aes(age, fill = income)) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 95, 5))

#The density plot above clearly shows that age and income are correlated -- people of greater age have higher income.
#This can also be seen from the following histograms of age by income:
ggplot(data = adult_data, mapping = aes(x = age)) +
  geom_histogram(binwidth = 5,
                 color = "black",
                 fill = "lightblue",
                 alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 95, 5)) +
  facet_wrap(~income) +
  ggtitle("Histogram of Age by Income")

#We give the boxplot of age grouped by income:
ggplot(aes(x = income, y = age),
       data = adult_data) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               shape = 16,
               cex = 2,
               color = "red") +
  coord_cartesian(ylim = c(10, 90)) +
  scale_y_continuous(breaks = seq(10, 90, 5)) +
  ylab("Age") +
  xlab("Income") +
  ggtitle("Boxplot of Age by Income")

#Once again we see the relationship between age and income which can also be seen by the summary statistic below:
summary(subset(adult_data$age, adult_data$income == " <=50K"))
summary(subset(adult_data$age, adult_data$income == " >50K"))

#From the above output and the boxplots we also note that the first quartiles for both groups differ significantly.
#The elder an individual is, the bigger their chance of haing a higher income.

#We show one more plot to demonstrate the correlation between age and income.
cd_plot(x = adult_data$age,
        y = adult_data$income,
        xlab = "Age",
        ylab = "Income",
        main = "Conditional Density Plot of Income versus Age")

#We show a plot of mean working hours per week versus age, grouped by gender.  We see that on average, men work more
#hours per week than women at almost all ages.
#We also notice that the mean working hours per week fro people between 25 and 60 years old is 40 hours for women
#and 45 hours for men.
ggplot(aes(x = age, y = hours_per_week),
       data = adult_data) +
  geom_line(mapping = aes(color = sex),
            stat = "summary",
            fun.y = mean) +
  geom_smooth(mapping = aes(color = sex)) +
  scale_x_continuous(breaks = seq(10, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 55, 5)) +
  labs(x = "Age", y = "Mean Hours per Week") +
  ggtitle("Age vs. Mean Hours per Week by Gender")


# The Variables hours_per_week and hours_worked --------------------------------

#hours_per_week

#Below is a summary statistic for the variable hours_per_week:
summary(adult_data$hours_per_week)
IQR(adult_data$hours_per_week)

#We show the boxplot of hours_per_week which visualizes the summary statistic and see that there are many outliers
#i.e., there are many who work less than 40 hours a week or more than 40 hours a week:
ggplot(aes(x = factor(0), y = hours_per_week),
       data = adult_data) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               shape = 19,
               color = "red",
               cex = 2) +
  coord_cartesian(ylim = c(10, 100)) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(10, 100, 5)) +
  ylab("Hours per Week") +
  xlab("") +
  ggtitle("Boxplot of Hours per Week")

#We display the boxplot of hours per week grouped by income since we are interested in the relationship between
#income and working hours.
ggplot(aes(x = income, y = hours_per_week),
       data = adult_data) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               shape = 19,
               color = "red",
               cex = 2) +
  coord_cartesian(ylim = c(10, 100)) +
  scale_y_continuous(breaks = seq(10, 100, 10)) +
  ylab("Hours per Week") +
  xlab("Income") +
  ggtitle("Boxplot of Hours per Week by Income")

#The mean number of working hours per week is significantly greater for people who earn more than 50K a year compared
#to people earning less than 50K a year.  The two medians are equal, but for income <=50K the median coincides
#with the third quartile, whereas for the data corresponding to income >50K, the respective median coincides with the 
#first quartile.
#The exact numbers can be observed better with the help of the summary() function:
summary(subset(adult_data$hours_per_week, adult_data$income == " <=50K"))
summary(subset(adult_data$hours_per_week, adult_data$income == " >50K"))

#The mean value for income <=50K is approximately 39 compared to 46 for income >50K and at least 50% of the people
#with income less than 50K work between 38 and 40 hours per week.  At least 50% of the individuals earning more
#than 50K a year work between 40 and 50 hours a week, which is considerably more than the group of people with
#income of less than 50K.  There is a pronounced correlation between income and working hours per week.

#We show a graph of the mean working hours per wek versus age, grouped by income:
ggplot(mapping = aes(x = age, y = hours_per_week),
       data = adult_data) +
  geom_line(mapping = aes(color = income),
            stat = "summary",
            fun.y = mean) +
  geom_smooth(mapping = aes(color = income)) +
  scale_x_continuous(breaks = seq(10, 100, 5)) +
  labs(x = "Age",
       y = "Mean Hours per Week") +
  ggtitle("Mean Hours per Week vs Age")

#We see that for all age groups the mean number of working hours per week is greater for people with income
#>50K than for people with income <=50K.

#hours_worked

#We see that the bulk of people work between 40 and 45 hours a week which we've already determined:
summary(adult_data$hours_worked)

#We give a bar plot of hours_worked, which shows the percentage of people belonging to each category of the 
#factor variable.
adult_data <- within(adult_data, hours_worked <- factor(hours_worked, levels =
                                                          names(sort(table(hours_worked),
                                                                     decreasing = TRUE))))

ggplot(adult_data,
       aes(x = adult_data$hours_worked, fill = adult_data$hours_worked)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                y = (..count..) / sum(..count..)),
            stat = "count",
            vjust = 0.3) +
  labs(x = "Hours per Week",
       y = "",
       fill = "Hours per Week") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Bar Plot of Hours per Week") +
  scale_y_continuous(labels = percent)

#We show a bar plot of hours_worked grouped by income:
lg_hpw <- lapply(levels(adult_data$income), function(v){
  df <- subset(adult_data, adult_data$income == v)
  df <- within(df, hours_worked <- factor(hours_worked,
                                          levels = names(sort(table(hours_worked),
                                                              decreasing = TRUE))))
  ggplot(data = df,
         aes(x = hours_worked,
             fill = hours_worked)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                  y = (..count..) / sum(..count..)),
              stat = "count",
              vjust = -.1,
              size = 3) +
    labs(x = "Hours per Week",
         y = "",
         fill = "Hours per Week") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste("Income", v, sep = "")) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = lg_hpw, ncol = 2)

#The proportion of people with income >50K who work between 45 and 60 hours a week is 33.2% compared to 14.6% for
#that of people with income <=50K.


# The variable native_region ----------------------------------------------

#We start with a summary statistic:
summary(adult_data$native_region)

#The majority of people come from the US and Central America.

#We show a bar plot with the percentage of people belonging to each native region:
adult_data$native_region <- factor(adult_data$native_region, 
                                   levels = 
                                     names(sort(table(adult_data$native_region),
                                                decreasing = TRUE)))

ggplot(adult_data,
       aes(x = adult_data$native_region, fill = adult_data$native_region)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                y = (..count..) / sum(..count..)),
            stat = "count",
            vjust = -.1) +
  labs(x = "Region",
       y = "",
       fill = "Regions") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent)

#91.2% of the participants in the study come from the USA.  We have a small number of people from each of the 
#other native regions leading to random samples which may not be representative for the respective population.
#Further analysis must be carried out with caution.

#We display the percentage of people earning less than 50K and more than 50K among all individuals belonging
#to a given native region:
lp_region <- lapply(levels(adult_data$native_region), function(v){
  df <- subset(adult_data, adult_data$native_region == v)
  ggplot(data = df,
         aes(x = income,
             fill = income)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                  y = (..count..) / sum(..count..)),
              stat = "count",
              vjust = c(2, -0.1),
              size = 4) +
    labs(x = "Income",
         y = "",
         fill = "Income" ) +
    ggtitle(v) +
    theme(legend.position = "none",
          plot.title = element_text(size = 11, face = "bold")) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = lp_region[1:4], ncol = 2)
grid.arrange(grobs = lp_region[5:8], ncol = 2)

#40.8% of the people with Central-Asian origin earn more than 50K a year, which is the highest percentage among
#all native regions.  We cannot rely completely on this inference because there are only 142 participants with
#Central-Asian origin compared to 27,504 people with US roots.  If we ignore the small number of individuals 
#which comprise the samples for the rest of the native regions, we can make similar conclusions. If we disregard
#the small number of observations from the non US native regions, the latter results indicate that income is
#dependent on native_region.


# The Variable workclass --------------------------------------------------

table(adult_data$workclass)

#The majority of people are employed in the private sector.

#We show a graph with the percentage of of people belonging to each category of workclass.
adult_data$workclass <- factor(adult_data$workclass, levels = names(sort(table(adult_data$workclass),
                                                                         decreasing = TRUE)))

ggplot(adult_data, aes(x = adult_data$workclass, fill = adult_data$workclass)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                y = (..count..) / sum(..count..)),
            stat = "count",
            vjust = -.1,
            size = 3.5) +
  labs(x = "Employment type",
       y = "",
       fill = "Employment type") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent)

#Based on the summary statistic, we observed that there are no people in the category Never worked.
nrow(subset(adult_data, adult_data$workclass == "Never-worked"))

#We also notice that there are no people in the category Without pay who earn more than 50K a year, which seems
#obvious and logical.
nrow(subset(adult_data, adult_data$workclass == "Without-pay" & adult_data$income == " >50K"))

#In order to make the plots more readable and meaningful, we exclude the factor levels Never-worked and Without pay.
modified_work <- levels(adult_data$workclass)
modified_work
modified_work <- modified_work[!is.element(modified_work, c("Never-worked", "Without-pay"))]
modified_work

#We plot the percentage of people earning less than 50K and more than 50K based on their employment status.
lg_workclass_mod <- lapply(modified_work, function(v){
  ggplot(data = subset(adult_data, adult_data$workclass == v),
         aes(x = subset(adult_data, adult_data$workclass == v)$income,
             fill = subset(adult_data, adult_data$workclass == v)$income)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                  y = (..count..) / sum(..count..)),
              stat = "count",
              vjust = c(2, 1.5)) +
    labs(x = "Income",
         y = "",
         fill = "Income") +
    ggtitle(v) +
    theme(legend.position = "none",
          plot.title = element_text(size = 11, face = "bold")) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = lg_workclass_mod[1:3], ncol = 2)
grid.arrange(grobs = lg_workclass_mod[4:6], ncol = 2)

#We can see that the percentage of individuals having an income of more than 50K is biggest for the category
#Self-emp-inc (self-employed with income) - 55.9%.  The group with the second highest percentage of people 
#earning more than 50K is that of federal government employees.  There is no significant difference between the
#categrories Local gov (local government), Self-emp-not-inc (self-employed without income), and State-gov (state
#government jobs) - about 27-28% of the workers in these branches have an annual pay of more than 50K.  In the 
#private sector, only about 22% of the people earn more than 50K a year.  There is a relationship between the
#variables income and workclass.

# The Variable education --------------------------------------------------

#We start with a summary of education:
summary(adult_data$education)

#The bar plot below shows the percentage of people belonging to each category of education:
adult_data$education <- factor(adult_data$education,
                               levels = names(sort(table(adult_data$education), decreasing = TRUE)))

ggplot(adult_data, aes(x = adult_data$education, fill = adult_data$education)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                y = (..count..) / sum(..count..)),
            stat = "count",
            vjust = -.1,
            size = 3.5) +
  labs(x = "Education",
       y = "",
       fill = "Education") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent)

#There are no people with education Preschool who earn more than 50K as we can see below:
nrow(subset(adult_data, adult_data$education == " Preschool" & adult_data$income == " >50K"))

#We will remove the factor level Preschool before we continue further with the analysis.
modified_edu <- levels(adult_data$education)
modified_edu
modified_edu <- modified_edu[!is.element(modified_edu, " Preschool")]
modified_edu

#We display the bar plot of each education category grouped by income:
lg_mod_edu <- lapply(modified_edu, function(v){
  ggplot(data = subset(adult_data, adult_data$education == v),
         aes(x = subset(adult_data, adult_data$education == v)$income,
             fill = subset(adult_data, adult_data$education == v)$income)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                  y = (..count..) / sum(..count..)),
              stat = "count",
              vjust = c(2, 0.5),
              size = 3) +
    labs(x = "Income",
         y = "",
         fill = "Income") +
    ggtitle(v) +
    theme(legend.position = "none",
          plot.title = element_text(size = 11, face = "bold")) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = lg_mod_edu[1:4], ncol = 2)
grid.arrange(grobs = lg_mod_edu[5:8], ncol = 2)
grid.arrange(grobs = lg_mod_edu[9:12], ncol = 2)
grid.arrange(grobs = lg_mod_edu[13:15], ncol = 2)

#The categories 1st-4th, 5th-6th, 7th-8th, 9th, 10th, 11th and 12th have a very small percentage of people with income
#greater than 50K a year.  the percentage of people with a high school degree who earn more than 50K is also
#relatively small-- 16.4%.  20% of the individuals in the category Some-college earn more than 50K.  The biggest
#percentage of employees (74.9%), who have an annual income higher than 50K, belongs to the category
#Prof-school.  The Doctorate group is next with 74.7% followed by the categories Masters (56.4%), and Bachelors
#(42.1%).  There is a relationship between education and income.  In order to demonstrate this correlation
#visually, we show one more graph-- a conditional density plot of income versus education number.
cd_plot(x = adult_data$education_num,
        y = adult_data$income,
        xlab = "Education Number",
        ylab = "Income",
        main = "Conditional Density Plot of Income vs. Education Number")

#Each number (from 1 to 16) in this integer variable corresponds to an education level from the factor variable
#education, starting from the lowest level (Preschool) and reaching the highest education level-- Doctorate.
#The higher the education level, the greater the chances of earning more than 50K a year.


# The Variable marital_status ---------------------------------------------

#We see how many people belong to each category of the variable marital_status:
summary(adult_data$marital_status)

#We visualize the percentage of people belonging to each category:
adult_data$marital_status <- factor(adult_data$marital_status,
                                    levels = names(sort(table(adult_data$marital_status), decreasing = TRUE)))

ggplot(adult_data, aes(x = adult_data$marital_status, fill = adult_data$marital_status)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                y = (..count..) / sum(..count..)),
            stat = "count",
            vjust = -.1,
            size = 3.5) +
  labs(x = "Marital Status",
       y = "",
       fill = "Marital Status") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent)

#We give bar plots of income grouped by marital status:
lp_marital <- lapply(levels(adult_data$marital_status), function(v){
  ggplot(data = subset(adult_data, adult_data$marital_status == v),
         aes(x = subset(adult_data, adult_data$marital_status == v)$income,
             fill = subset(adult_data, adult_data$marital_status == v)$income)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                  y = (..count..) / sum(..count..)),
              stat = "count",
              vjust = c(2, -0.1)) +
    labs(x = "Income",
         y = "",
         fill = "Income") +
    ggtitle(v) +
    theme(legend.position = "none",
          plot.title = element_text(size = 11, face = "bold")) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = lp_marital[1:3], ncol = 2)
grid.arrange(grobs = lp_marital[4:7], ncol = 2)

#The biggest percentage of employees with income higher than 50K are those from the category Married-AF-spouse.
#Since there are only 25 observations in this category, we cannot draw trustworthy conclusions regarding the
#income of the individuals belonging to this group.  On the other hand, the random sample for the category
#Married-civ-spouse amounts to 14,605 individuals and can be considered representative.  For this category, the
#percentage of people with income of more than 50K is very high-- 45.5%.  The same cannot be said for the groups
#Divorced, Never-married, Married-spouse-absent, Separated, and Widowed.  One explanation as to why people who
#never got married earn less than married people is that the former group probably contains mostly young individuals
#who work part-time, as well as younger people as a whole, who are in the beginning of their professional career.
#this conclusion is also in agreement with the variable age, where we noticed that the greater the age of an
#individual, the higher the income.  There is a correlation between income and marital status, which cannot be
#explained only with the confounding age variable.


# The Variable occupation -------------------------------------------------

#First we show the summary statistic of occupation:
summary(adult_data$occupation)

#We visualize the percentage of people belonging to each category of the factor variable occupation:
adult_data$occupation <- factor(adult_data$occupation, levels = names(sort(table(adult_data$occupation),
                                                                           decreasing = TRUE)))

ggplot(adult_data, aes(x = adult_data$occupation, fill = adult_data$occupation)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                y = (..count..) / sum(..count..)),
            stat = "count",
            vjust = -.1,
            size = 3.5) +
  labs(x = "Occupation",
       y = "Percentage",
       fill = "Occupation") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent)

#There are no women working in the Armed-Forces and there are no men working in the Priv-house-serv sector who
#are earning more than 50K a year.
nrow(subset(adult_data, adult_data$sex == " Female" & adult_data$occupation == " Armed-Forces"))
nrow(subset(adult_data, adult_data$sex == " Male" & adult_data$occupation == " Priv-house-serv" &
              adult_data$income == " >50K"))

#For the following bar plots with percentages of women earning less than 50K and more than 50K for each type
#oc occupation, we exclude Armed-Forces.
modified_occup_f <- levels(adult_data$occupation)
modified_occup_f
modified_occup_f <- modified_occup_f[!is.element(modified_occup_f, c(" Armed-Forces"))]
modified_occup_f

#We display bar plots of women's income grouped by occupation:
lp_occupation_f <- lapply(modified_occup_f, function(v){
  ggplot(data = subset(adult_data, adult_data$occupation == v & adult_data$sex == " Female"),
         aes(x = subset(adult_data, adult_data$occupation == v & adult_data$sex == " Female")$income,
             fill = subset(adult_data, adult_data$occupation == v & adult_data$sex == " Female")$income)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                  y = (..count..) / sum(..count..)),
              stat = "count",
              vjust = c(2, -0.1)) +
    labs(x = "Income",
         y = "",
         fill = "Income") +
    ggtitle(paste("Women in \n", v, sep = "")) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, face = "bold")) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = lp_occupation_f[1:4], ncol = 2)
grid.arrange(grobs = lp_occupation_f[5:9], ncol = 3)
grid.arrange(grobs = lp_occupation_f[10:13], ncol = 3)

#The biggest percentage of women with income greater than 50K is in the category Prof-specialty- 25.5% followed
#by 24.2% in the category Exec-managerial.  In the rest of the categories, this percentage is less than 10%
#with the exception of Protective-serv-- 13.2%, and Tech-support-- 12.9%. We give a summary statistic for the 
#number of women belonging to each category of occupation:
summary(adult_data[adult_data$sex == " Female",]$occupation)

#The groups Admn-clerical, Exec-managerial, Machine-op-inspct, Other-service, Prof-specialty, Sales and Tech-support
#can be considered as representative random samples, whereas the statistical inferences for the rest of the
#categories should be viewed with caution.

#Since no men who are working in the Priv-house-serv sector are earning more than 50K a year, we leave out 
#Armed-Forces and Priv-house-serv when displaying the bar plots of income for men grouped by occupation.
modified_occup_m <- levels(adult_data$occupation)
modified_occup_m
modified_occup_m <- modified_occup_m[!is.element(modified_occup_m, " Priv-house-serv")]
modified_occup_m

lp_occupation_m <- lapply(modified_occup_m, function(v){
  ggplot(data = subset(adult_data, adult_data$occupation == v & adult_data$sex == " Male"),
         aes(x = subset(adult_data, adult_data$occupation == v & adult_data$sex == " Male")$income,
             fill = subset(adult_data, adult_data$occupation == v & adult_data$sex == " Male")$income)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                  y = (..count..) / sum(..count..)),
              stat = "count",
              vjust = c(2, 1),
              size = 3) +
    labs(x = "Income",
         y = "",
         fill = "Income") +
    ggtitle(paste("Men in", v, sep = "")) +
    theme(legend.position = "none",
          plot.title = element_text(size = 11, face = "bold")) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = lp_occupation_m[1:4], ncol = 2)
grid.arrange(grobs = lp_occupation_m[5:8], ncol = 2)
grid.arrange(grobs = lp_occupation_m[9:10], ncol = 2)
grid.arrange(grobs = lp_occupation_m[11:13], ncol = 2)

#From the summary statistic below we see the number of men in each category of the variable occupation.  All
#groups except Armed-Forces and Priv-house-serv are representative random samples with enough number of observations.
summary(adult_data[adult_data$sex == " Male",]$occupation)

#We observe the tendency that jobs requiring highly qualified specialists with college or university degrees are
#paid better than jobs that don't.  This conclusion seems reasonable and, indeed, reflects the real world job
#market.


# The Variable relationship -----------------------------------------------

#This variable is closely related to the variable marital_status and they should be considered together.  From
#the summary below we notice that the majority of people are married because they identified themselves as Husband
#and Wife.  This is in accordance with the summary statistic of marital_status.
summary(adult_data$relationship)

#We show the percentage of people belonging to each category of relationship:
adult_data$relationship <- factor(adult_data$relationship, levels = names(sort(table(adult_data$relationship),
                                                                               decreasing = TRUE)))

ggplot(adult_data, aes(x = adult_data$relationship, fill = adult_data$relationship)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..) / sum(..count..)),
                y = (..count..) / sum(..count..)),
            stat = "count",
            vjust = -.1) +
  labs(x = "Relationship", y = "", fill = "Relationship") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent)

#The distribution of people in each category of relationship is connected to that of marital_status given below:
ggplot(adult_data, aes(x = adult_data$marital_status, fill = adult_data$marital_status)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..) / sum(..count..)), y = (..count..) / sum(..count..)),
            stat = "count", vjust = -.1, size = 3.5) +
  labs(x = "Marital Status", y = "", fill = "Marital Status") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent)

#We give a summary statistic of the marital status of each level of the factor variable relationship:
summary(adult_data[adult_data$relationship == " Not-in-family",]$marital_status)
summary(adult_data[adult_data$relationship == " Husband",]$marital_status)
summary(adult_data[adult_data$relationship == " Other-relative",]$marital_status)
summary(adult_data[adult_data$relationship == " Own-child",]$marital_status)
summary(adult_data[adult_data$relationship == " Unmarried",]$marital_status)
summary(adult_data[adult_data$relationship == " Wife",]$marital_status)

#Most of the results are in agreement with the variable marital_status.

#We show bar plots of income by relationship status:
ggplot(adult_data, aes(x = adult_data$relationship, fill = adult_data$income)) +
  geom_bar(position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Income", y = "Number of People", fill = "Income") +
  ggtitle("Income Grouped by Relationship") +
  scale_y_continuous(breaks = seq(0, 7000, 500))

#We give bar plots with percentages of individuals having an income lower and higher than 50K for each relationship
#level:
lg_relationship <- lapply(levels(adult_data$relationship), function(v){
  ggplot(data = subset(adult_data, adult_data$relationship == v), aes(x = subset(adult_data, adult_data$relationship == v)$income,
                                                                      fill = subset(adult_data, adult_data$relationship == v)$income)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)), y = (..count..) / sum(..count..)),
              stat = "count", vjust = c(2, -0.1), size = 3) +
    labs(x = "Income", y = "", fill = "Income") +
    ggtitle(paste(v)) +
    theme(legend.position = "none", plot.title = element_text(size = 11, face = "bold")) +
    scale_y_continuous(labels = percent)})

grid.arrange(grobs = lg_relationship[1:3], ncol = 2)
grid.arrange(grobs = lg_relationship[4:6], ncol = 2)

#The results above are in agreement with the results for income grouped by marital status.  As was the case with
#marital_status, we observe a relationship between income and relationship.


# The Variable race -------------------------------------------------------

#We start with a summary statistic:
summary(adult_data$race)

#The major part of individuals belong to the category White, followed by the category Black.  86% of the participants
#in the study are White, 9.3%- Black, 3%- Asian-Pac-Islander, 0.9%- Amer-Indian-Eskimo and 0.8%- Other:
adult_data$race <- factor(adult_data$race, levels = names(sort(table(adult_data$race), decreasing = TRUE)))

ggplot(adult_data, aes(x = adult_data$race, fill = adult_data$race)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..) / sum(..count..)), y = (..count..) / sum(..count..)),
            stat = "count", vjust = c(-0.2, -0.2, -0.2, -0.2, 3)) +
  labs(x = "Race", y = "", fill = "Race") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent)

#We show bar plots of income by race:
lg_race <- lapply(levels(adult_data$race), function(v){
  ggplot(data = subset(adult_data, adult_data$race == v), aes(x = subset(adult_data, adult_data$race == v)$income,
                                                              fill = subset(adult_data, adult_data$race == v)$income)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)), y = (..count..) / sum(..count..)),
              stat = "count", vjust = c(2, -0.1)) +
    labs(x = "Income", y = "", fill = "Income") +
    ggtitle(paste(v)) +
    theme(legend.position = "none", plot.title = element_text(size = 11, face = "bold")) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = lg_race, ncol = 3)


# The Variable sex --------------------------------------------------------

#We see that there are 20,380 men and 9,782 women who took part in the survey:
summary(adult_data$sex)

#In percentages, this makes 67.6% male participants and more than two times less female participants:
ggplot(adult_data, aes(x = adult_data$sex, fill = adult_data$sex)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..) / sum(..count..)), y = (..count..) / sum(..count..)),
            stat = "count", vjust = -.1) +
  labs(x = "Gender", y = "Percentage", fill = "Gender") +
  scale_y_continuous(labels = percent)

#We show in one graph two bar plots with the number of men and women who earn less than 50K and more than 50K a year:
ggplot(adult_data, aes(x = adult_data$sex, fill = adult_data$income)) +
  geom_bar(position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Income", y = "Number of People", fill = "Income") +
  ggtitle("Income Grouped by Gender") +
  scale_y_continuous(breaks = seq(0, 14500, 1000))

#We find it easier to interpret bar plots with percentages rather than raw counts:
gender_income <- lapply(levels(adult_data$sex), function(v){
  ggplot(data = subset(adult_data, adult_data$sex == v), aes(x = subset(adult_data, adult_data$sex == v)$income,
                                                             fill = subset(adult_data, adult_data$sex == v)$income)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)), y = (..count..) / sum(..count..)),
              stat = "count", vjust = -0.1, size = 3) +
    labs(x = "Income", y = "", fill = "Income") +
    ggtitle(paste(v)) +
    theme(legend.position = "none", plot.title = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(hjust = 1)) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = gender_income, ncol = 2)

#We show several ways to obtain the same information as that shown above without plotting graphs:

##Number of women earning less than 50K and more than 50K:
summary(adult_data$income[adult_data$sex == " Female"])

##Number of men earning less than 50K and more than 50K:
summary(adult_data$income[adult_data$sex == " Male"])

##Number of men and women earning less than 50K and more than 50K:
by(data = adult_data$income, INDICES = adult_data$sex, FUN = summary)

##Proportion of men and women with income <50K and >=50K:
prop.table(table(adult_data$sex, adult_data$income), margin = 1)

#We give the bar plots of workclass grouped by gender:
lg_gender_workclass <- lapply(levels(adult_data$sex), function(v){
  df <- subset(adult_data, adult_data$sex == v)
  df <- within(df, workclass <- factor(workclass, levels = names(sort(table(workclass), decreasing = TRUE))))
  ggplot(data = df, aes(x = df$workclass, fill = df$workclass)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)), y = (..count..) / sum(..count..)),
              stat = "count", vjust = -0.1, size = 3) +
    labs(x = "Workclass", y = "", fill = "Workclass") +
    ggtitle(paste(v)) +
    theme(legend.position = "none", plot.title = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = lg_gender_workclass, ncol = 2)

#The biggest percentage of both female and male individuals are employed in the private sector. 
#Bar plots of occupation by gender are shown next:
lg_gender_occupation <- lapply(levels(adult_data$sex), function(v){
  df <- subset(adult_data, adult_data$sex == v)
  df <- within(df, occupation <- factor(occupation, levels = names(sort(table(occupation), decreasing = TRUE))))
  ggplot(data = df, aes(x = df$occupation, fill = subset(adult_data, adult_data$sex == v)$occupation)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)), y = (..count..) / sum(..count..)),
              stat = "count", vjust = -0.1, hjust = 0.3, size = 3) +
    labs(x = "Occupation", y = "", fill = "Occupation") +
    ggtitle(paste(v)) +
    theme(legend.position = "none", plot.title = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = lg_gender_occupation, ncol = 2)

#We notice that there is an overlap between the first five most popular occupation categories among men and 
#women-- these are the categories Prof-specialty, Exec-managerial, and Sales.
#We continue with bar plots of education grouped by gender:
lg_gender_education <- lapply(levels(adult_data$sex), function(v){
  df <- subset(adult_data, adult_data$sex == v)
  df <- within(df, education <- factor(education, levels = names(sort(table(education), decreasing = TRUE))))
  ggplot(data = df, aes(x = df$education, fill = subset(adult_data, adult_data$sex == v)$education)) +
    geom_bar(aes(y = (..count..) / sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..) / sum(..count..)), y = (..count..) / sum(..count..)),
              stat = "count", vjust = -0.1, size = 2.5) +
    labs(x = "Education", y = "", fill = "Education") +
    ggtitle(paste(v)) +
    theme(legend.position = "none", plot.title = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = percent)
})

grid.arrange(grobs = lg_gender_education, ncol = 2)

#We see that the percentages of men and women belonging to each level of education are very similar.  An exception
#makes the category Doctorate, in which ther are almost two times more men than women.


# Tests for Independence of the Variables ---------------------------------

#We will test the independence of the categorical variables two by two with the Pearson's Chi-Square Test of 
#independence.  The test checks for the following null hypothesis: H0: The two categorical variables are 
#independent in the considered population, HA: the two categorical variables are dependent (related) in the 
#considered population.


# The Variables sex and income --------------------------------------------

##Two-way contingency table with Pearson's chi-square test of independence for the variables sex and income:
CrossTable(adult_data$sex, adult_data$income, prop.chisq = TRUE, chisq = TRUE)

#Pearson's chi-square test of independence for the variables sex and income:
chisq.test(adult_data$sex, adult_data$income)

#The p-value is less than 0.05, so we fail to accept the null hypothesis that the two categorical variables
#are independent.


# The Variables race and income -------------------------------------------

chisq.test(adult_data$race, adult_data$income)
CrossTable(adult_data$race, adult_data$income, prop.chisq = TRUE, chisq = TRUE)

#We reject the null hypothesis at the 0.05 significance level.  There is a strong indication that race and
#income are correlated.


# The Variables workclass and income --------------------------------------

chisq.test(table(adult_data$workclass, adult_data$income))
chisq.test(table(adult_data$workclass, adult_data$income))$expected

#We notice that there are two cells with expected cell counts equal to 0 and one cell with expected cell count
#equal to 3.5<5.  If we look at the observed counts of the levels of workclass:
table(adult_data$workclass)

#There are no participants in the study who identify themselves as belonging to the category Never-worked.
#We remove this unused factor level from the categorical variable workclass.
adult_data$workclass <- droplevels(adult_data$workclass)
levels(adult_data$workclass)

summary(adult_data$workclass)

#We again perform Pearson's chi-square test:
CrossTable(adult_data$workclass, adult_data$income, prop.chisq = TRUE, chisq = TRUE)

#There is still one cell with expected cell count less than 5, we'll ignore the warning and interpret the results
#with caution. The p-value is very small, which means that we reject the null hypothesis at the 0.05 
#significance level.


# The Variables occupation and income: ------------------------------------

chisq.test(adult_data$occupation, adult_data$income)

#We check if there are any cells with expected count less than 5:
chisq.test(adult_data$occupation, adult_data$income)$expected

#Since there is only one problematic cell, we'll consider the Pearson's test trustworthy.  The calculated p-value
#is very small and hence we fail to accept the null hypothesis, which means that there is a strong indication
#that the categorical variables occupation and income are dependent.


# The Variables education and income: -------------------------------------

chisq.test(adult_data$education, adult_data$income)


# The Variables marital_status and income: --------------------------------

chisq.test(adult_data$marital_status, adult_data$income)


# The Variables relationship and income: ----------------------------------

chisq.test(adult_data$relationship, adult_data$income)


# The Variables native_region and income: ---------------------------------

chisq.test(adult_data$native_region, adult_data$income)


# The Variables hours_worked and income: ----------------------------------

chisq.test(adult_data$hours_worked, adult_data$income)

#All of the rest of the Pearson's chi-square tests give very small p-values, which means that it is very
#unlikely for the considered categorical variables no to be related with income.  