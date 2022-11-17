#################################################################################################### 
##
## Project: Predictive Modelling with the Malawi 2019 LSMS data
## Script purpose: A primer on Machine Learning and Supervised Predictive Modeling
## Encoding: UTF-8
## Packages: tidyverse, data.table, caret, AppliedPredictiveModeling, corrplot, Hmisc
## Date: 15 November 2022
## Author: Michelle González Amador
## Version: R ‘3.6.2’ // Rstudio ‘2022.2.3.492’
## Notes v00:
##
####################################################################################################
rm(list = ls())
setwd("~/Desktop/MachineLearning4PP/Machine-Learning-for-Public-Policy")
getRversion()
RStudio.Version()$version

##### 1. Libraries ####

# install.packages("caret", "AppliedPredictiveModeling", "corrplot", "Hmisc")
# Mac M1 Chip (and above) users will get an error installing the last package, please check with Michelle to fix it.

#library(usethis) 
#usethis::edit_r_environ() #use this if you need to increase memory use
library(tidyverse)
library(data.table)
library(caret) # To learn more about the caret package: https://topepo.github.io/caret/
library(AppliedPredictiveModeling)
library(corrplot)
library(Hmisc)


### 2. Upload the data ####
malawi <- read_rds("Malawi_2019.rds")

### 3. Get to know your data: visualisation ####

str(malawi)

# The dataframe contains 50476 observations (households) and 514 features (variables).
# This is a very large dataset, and I want to work with only a subset of it using all the observations but only some features
# You can find the label descriptions here: https://microdata.worldbank.org/index.php/catalog/3818/data-dictionary

# Let's begin the "critical" process of feature selection (or variable selection)
# Some notes on this: From a practical point of view, a model with less predictors may be more interpretable and less costly especially if there is a cost to measuring the predictors
# Also, some models may be negatively affected by non-informative predictors. Hence: 

# Target variable (aka Dependent Variable (DV)): Total Food Consumption in the Past Week

#1 $ food consumption in the past week: sumConsumption
which( colnames(malawi)=="sumConsumption") # 514
#2 How old is NAME (year): hh_b05a
which( colnames(malawi)=="hh_b05a") # 27
#3 reside: reside
which( colnames(malawi)=="reside") # 6
#4 household size: hhsize
which( colnames(malawi)=="hhsize") #21 
#5 did you or anyone else in this HH borrow on credit?: hh_s01
which( colnames(malawi)=="hh_s01") # 19
#6 gender [1vMALE, 2 FEMALE]: hh_b03 
which( colnames(malawi)=="hh_b03") # 22
#7 What is the highest educational qualification [NAME] has acquired: hh_c09
which( colnames(malawi)=="hh_c09") # 88
#8 $ Tuition, including any extra tuition fees (hh_c22a)
which( colnames(malawi)=="hh_c22a") # 105
#9 $ Expenditures on after school programs & tutoring (hh_c22b)
which( colnames(malawi)=="hh_c22b") # 106
#10 $ School books and other materials (hh_c22c)
which( colnames(malawi)=="hh_c22c") # 107
#11 $ School uniform clothing (hh_c22d)
which( colnames(malawi)=="hh_c22d") #108
#12 $ TOTAL (hh_c22j)
which( colnames(malawi)=="hh_c22j") #114
#13 How much was spent in TOTAL in the last 12 months? (hh_c22l)
which( colnames(malawi)=="hh_c22l") #116
#14 Have you used a computer? (hh_c24)
which( colnames(malawi)=="hh_c24") # 124
#15 Amt [NAME] spent in the past 4 weeks for all illnesses and injuries? (hh_d10)
which( colnames(malawi)=="hh_d10") # 142
#16 Amt in total did [NAME]spent...for medical care not related to an illness? (hh_d11)
which( colnames(malawi)=="hh_d11") #143 
#17 How much in total did [NAME] spend..for medical insurance? (hh_d12_1)
which( colnames(malawi)=="hh_d12_1") #145
#18 Does [NAME] want to change his/her current employment situation? (hh_e70)
which( colnames(malawi)=="hh_e70") #337
#19 What was the total amout paid in the form of land rent during the p... (hh_f04_4)
which( colnames(malawi)=="hh_f04_4") #355
#20 hh_f04a How much do you pay to rent this property?
which( colnames(malawi)=="hh_f04a") #358
#21 Do you have electricity working in your dwelling? (hh_f19)
which( colnames(malawi)=="hh_f19") #381
#22 How many working cell phones in total does your household own? (hh_f34)
which( colnames(malawi)=="hh_f34") #404
#23 Which of the following is true? Your current income . . . (hh_t08)
which( colnames(malawi)=="hh_t08") #499
#24 Concerning your HH's food consumption, over the past months which is true (hh_t01)
which( colnames(malawi)=="hh_t01") #492
#25 HH were unable to eat healthy & nutritious food b'se of a lack of money/other (hh_t14)
which( colnames(malawi)=="hh_t14") #507
#26 ... What other features do you consider important? 

# subsetting our dataframe
cols   <- c(6,21,27,514,19,22,88,105,106,107,108,114,116,124,142,143,145,337,355,358,381,404,499,492,507)
malawi <- malawi[,cols] 
# The malawi dataframe should only have 25 (selected) features now

### Let's do a quick data clean-up (please be sure to go over this step yourself and analyse whether it was implemented appropiately)
 # gettind rid of missing data
colSums(is.na(malawi))

#Interestingly, a lot of the features from module c have more than half of missing data (>30,000 hhs).
# All of the variables refer to $ spent on education. For the sake of simplicity, I will simply remove those features. 
# Another alternative would be to only work with the remaining households for which we have full data.
# Multiple imputation is not feasible due to the large percentage of missingness

#hh_c22a-l
which( colnames(malawi)=="hh_c22a") # 8
which( colnames(malawi)=="hh_c22b") # 9
which( colnames(malawi)=="hh_c22c") # 10
which( colnames(malawi)=="hh_c22d") #11
which( colnames(malawi)=="hh_c22j") #12
which( colnames(malawi)=="hh_c22l") #13

cols_cModule <- c(8,9,10,11,12,13)
malawi <- malawi[,-cols_cModule]
# we have 19 features left in the dataframe

# Are there any remaining missing values? double-checking is always a good idea!
colSums(is.na(malawi))
# There seem to be quite a lot of missing values in a couple of vectors from the F module
which( colnames(malawi)=="hh_f04_4") # 13
which( colnames(malawi)=="hh_f04a") # 14

malawi <- malawi[,-c(13,14)]

# The Food Consumption in the past week variable, sumConsumption, has 9046 missing values. 
# Let's get rid of them
rows_consumption <- which(is.na(malawi$sumConsumption))
head(rows_consumption)
tail(rows_consumption)
length(rows_consumption) # we've creted an indicator with all the missing rows

malawi <- malawi[-rows_consumption,]
colSums(is.na(malawi)) # all clear!

str(malawi) # 41,430 obs. and  17 features

# We want to quickly plot histograms of ALL the variables in our dataset.

x <- malawi %>% select_if(~is.integer(.) | is.numeric(.)) # this line selects all variables which are integer or numeric, and can therefore be plotted as a histogram
hist.data.frame(x) # from the Hmisc package, quick and painless.


# alternatively: (please note that factors, aka categorical data, will be coerced into numeric)
#df <- as.data.frame(lapply(malawi,as.numeric))


# now let's create a correlation matrix out of our dataset! (would be great to see all possible correlates, remember the >.5 rule of thumb!)

# correlation matrix
M <- cor(x) # create a correlation matrix of the whole dataset, cor() uses Pearson's correlation coefficient as default
corrplot(M, method="circle", addCoef.col ='black', number.cex = 0.8) # visualise it in a nice way

#OH NO! In some situations, the data generating mechanism can create predictors that only have a single unique value (i.e. a “zero-variance predictor”). For many models (excluding tree-based models), this may cause the model to crash or the fit to be unstable.

#This correlation matrix does not take into account the Spearman correlation btween factor and numeric variables!

#### 4. Partition data into training and test data ####
# First, set a seed to guarantee replicability of the process
set.seed(1234777) # use any number you want

# We could split the data manually, but the caret package includes a function

train_idx <- createDataPartition(malawi$sumConsumption, p = .8, list = FALSE, times = 1) # this creates an 80/20 split
# why 80/20? a) it's standard (based on empirical studies, though!); b) it has its origins in the Pareto Principle: "in most cases, 80% of effects come from 20% of causes".
head(train_idx)

Train_df <- malawi[ train_idx,]
Test_df  <- malawi[-train_idx,]

# we have succesfully created a training and a test dataset based on the 80/20 rule!
# Now that we have split our data, we can use the lm() function to fit a model to your training set, rather than the entire dataset.

##### 5. Creating our first predictive model and evaluating its performance: RMSE and R^2 ######

model1 <- lm(sumConsumption ~ .,Train_df) # the dot asks the lm() function tu use all other variables in the df as predictors
summary(model1)
cat(sprintf("The R^2 value of our model, or total variance explained is: %.4f",0.1013))
#What do you make of this number? Is it a good model?

# You can use the predict() function to make predictions from that model on the test data
# we are testing how well the model extrapolates to a different population
p <- predict(model1, Test_df)

# We got a warning. What does it mean? 
# This warning does not stop the program from running or producing results. It does, however, indicate that those results may be meaningless.

# Now that you have predictions on the test set, you can use these predictions to calculate an error metric (in this case RMSE) on the test set and see how the model performs out-of-sample
# We'll use two steps: 
#1: calculate distance between the observed values minus de predicted values.

error <- p - Test_df[["sumConsumption"]] # predicted values minus actual values
#2: estimate the Root Mean Squared Error (you've got the Error part already!)

RMSE_test <- sqrt(mean(error^2))
print(RMSE_test) # this is known as the out-of-sample RMSE

# Now we need to calculate the RMSE for the training dataset
# this is known as the in-sample RMSE!
p0 <- predict(model1, Train_df)

error0 <- p0 - Train_df[["sumConsumption"]]
# In-sample RMSE
RMSE_og <- sqrt(mean(error0 ^ 2))

# Comparing out of sample RMSE to in-sample RMSE
cat(sprintf("RMSE by dataset: training[%.2f], test[%.2f]",RMSE_og,RMSE_test))

# First: what is a good RMSE value?
# recall that RMSE has the same unit as the dependent variable (DV). It means that there is no absolute good or bad threshold, however you can define it based on your DV. For a datum which ranges from 0 to 1000, an RMSE of 0.7 is small, but if the range goes from 0 to 1, it is not that small anymore. However, although the smaller the RMSE, the better, you can make theoretical claims on levels of the RMSE by knowing what is expected from your DV in your field of research. 

#Second: how to compare models with different datasets using RMSE?
# the smaller the better but remember that small differences between those RMSE may not be relevant or even significant.
# what conclusions can we draw from comparing our two RMSEs?
# The RMSE for your training and your test sets should be very similar if you have built a good model. If the RMSE for the test set is much higher than that of the training set, it is likely that you've badly over fit the data, i.e. you've created a model that tests well in sample, but has little predictive value when tested out of sample.


### 5. Cross-validation ####
set.seed(1607) #yes, again

# You can also choose to make your life easier by doing all these steps in one go with the caret package:

# 10-fold cross-validation

# A way to improve the cross-validation of your models is to use multiple systematic test sets, rather than a single train/test split.

tenfoldcv <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)


mod1 <- train(sumConsumption ~ ., data = Train_df, 
                 method = "lm", #linear model
                 trControl = tenfoldcv, # we previously defined this guy
                 )
print(mod1)
# The warnings are, again, indicating us that our model is not very good. Or rather, not good at all!
# The performance metrics (close to what we estimated in a less elegant way without the package) also tell us our model is less than good
# From the caret package: By default, the train function chooses the model with the largest performance value (or smallest, for mean squared error in regression models).

# Excercise: use this data and select a continious DV. Can you build a good predictive model?
