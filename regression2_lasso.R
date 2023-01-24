#################################################################################################### 
##
## Project: Lasso and Ridge regression models
## Script purpose: Practical excercise using the Malawi 2019 dataset to run a lasso regression
## Encoding: UTF-8
## Packages: tidyverse, plyr, data.table, caret, Hmisc, elasticnet, corrplot
## Date: 6 December 2022
## Author: Michelle González Amador
## Version: R ‘3.6.2’ // Rstudio ‘2022.7.2.576’
## Notes v00:
##
####################################################################################################
rm(list=ls())
setwd("~/Users/michellegonzalez/Documents/GitHub/Machine-Learning-for-Public-Policy")

### 0. Libraries ####
#install.packages("glmnet", "AppliedPredictiveModeling")
library(plyr)
library(tidyverse)
library(data.table)
library(caret)
library(Hmisc)
library(elasticnet) # works in conjunction with caret for lasso models
library(corrplot)

### 1. Upload data and subset data ####
malawi <- fread("malawi.csv")

column_names <- c("sumConsumption", "hh_b05a", "reside", "region", "district","hhsize", "hh_s01", "hh_b03", "hh_c09", "hh_c24", "hh_d10", "hh_d11", "hh_d12_1", "hh_e70", "hh_f19", "hh_f34", "hh_t08", "hh_t01", "hh_t14")
malawi <- malawi[,column_names, with=FALSE]


### 2. Data pre-processing: missing values and zero-variance predictors ####

colSums(is.na(malawi))
row_miss <- which(is.na(malawi$sumConsumption))
malawi <- malawi[-row_miss,]

malawi_continuous <- malawi %>% select_if(~is.integer(.) | is.numeric(.)) 
hist.data.frame(malawi_continuous).

summary(malawi$hh_d10) #Amt [NAME] spent in the past 4 weeks for all illnesses and injuries?
unique(malawi$hh_d10) # variable distribution is legitimate.
ill_people_spentCash <- length(which(malawi$hh_d10!=0))
cat(sprintf("%.0f people/households spent money on an illness, out of 41,430, in the past 4 weeks.", ill_people_spentCash))
cat("Variables from the H section of the Household module all follow similar distributions due to the type of question asked.")

names(malawi)[names(malawi) == "hh_d10"] <- "total_spent_on_illness"
names(malawi)[names(malawi) == "hh_d11"] <- "total_spent_on_non_illness_medicalcare"
names(malawi)[names(malawi) == "hh_d12_1"] <- "total_spent_on_medicalinsurance"


malawi_factor <- malawi %>% select_if(~is.character(.)) # subset of the dataframe containing only factor variables
malawi_factor <- as.data.frame(unclass(malawi_factor),stringsAsFactors=TRUE)
llply(.data=malawi_factor, .fun=table) # create tables of all the variables in dataframe using the plyr package

# Variables with missing values: hh_c09, hh_c24, hh_e70 (too many, delete var), hh_t14, hh_t01, hh_t08

malawi <- as.data.frame(unclass(malawi),stringsAsFactors=TRUE) # Convert character vectors into factors in OG dataframe
which(colnames(malawi)=="hh_e70")
malawi <- malawi[,-14]

levels(malawi$hh_c09)[levels(malawi$hh_c09)==""] <- NA
levels(malawi$hh_c09)
sum(is.na(malawi$hh_c09)) #10,012

levels(malawi$hh_c24)[levels(malawi$hh_c24)==""] <- NA
levels(malawi$hh_c24)
sum(is.na(malawi$hh_c24)) #5,655

levels(malawi$hh_t14)[levels(malawi$hh_t14)==""] <- NA
levels(malawi$hh_t14)
sum(is.na(malawi$hh_t14)) #3

levels(malawi$hh_t01)[levels(malawi$hh_t01)==""] <- NA
levels(malawi$hh_t01)
sum(is.na(malawi$hh_t01)) #3

levels(malawi$hh_t08)[levels(malawi$hh_t08)==""] <- NA
levels(malawi$hh_t08)
sum(is.na(malawi$hh_t08)) #3

colSums(is.na(malawi)) # Some of these missings are overlapping.

malawi <- na.omit(malawi)
colSums(is.na(malawi))

# correlation matrix

malawi_corr <- as.data.frame(lapply(malawi,as.numeric)) # coerce dataframe to numeric, as the cor() command only takes in numeric types
M <- cor(malawi_corr, method = "spearman") # default to spearmand due to the presence of factor variables
corrplot(M, method="circle", addCoef.col ="black", number.cex = 0.8) # visualise it in a nice way

### 3. Lasso model with caret() ####

# 3.1 Create test and train data with the caret package

set.seed(12345) # use any number you want

train_idx <- createDataPartition(malawi$sumConsumption, p = .8, list = FALSE, times = 1) 

Train_df <- malawi[ train_idx,]
Test_df  <- malawi[-train_idx,]

# 3.2 use the train() function from the caret package to create a lasso model

model_lasso <- train(
    sumConsumption ~ .,
    data = Train_df,
    method = 'lasso',
    preProcess = c("center", "scale") #This will scale and center all relevant variables in the model
)

print(model_lasso)

# Let's see our model
plot(model_lasso)
cat("The x-axis is the fraction of the full solution (i.e., ordinary least squares with no penalty)")


# did the Lasso method get rid of some variables? 
plot(varImp(model_lasso))
cat(" We can clearly see the non-relevance of certain variables, and that the variable district was deleted / it's coefficient was shrunk to 0")
cat(" Recall that the optimal model had a fraction of .9, so we knew (!=1) that at least one variable was deleted.")

# 3.3 Make predictions with the test data set

test_features <- subset(Test_df, select=-c(sumConsumption))
test_target   <- subset(Test_df, select=sumConsumption)[,1]

lasso_predictions <-  predict(model_lasso, newdata = test_features)

# RMSE
sqrt(mean((test_target - lasso_predictions)^2))

# R^2
cor(test_target, lasso_predictions) ^ 2

### 4. Cross-validation: k-fold with caret() ####
cat("Cross-validation: beyond training and testing dataframes, what a k-fold (commonly 10-fold) cross validation does is resample the data to find an optimal (λ)lambda/penalisation for the lasso model.")
cat("Recall: The optimal λ(lambda)/penalisation minimizes the out-of-sample (or test) mean prediction error.")

set.seed(12345)

cv_10fold <- trainControl(
    method = "cv", #cross-validation
    number = 10, # k-fold = 10-fold (split the data into 10 similar-sized samples)
)

set.seed(12345)

lasso_kfold <- train(
    sumConsumption ~ .,
    data = Train_df,
    method = 'lasso',
    preProcess = c("center", "scale"),
    trControl = cv_10fold
)
print(lasso_kfold)
plot(lasso_kfold)
plot(varImp(lasso_kfold))


lasso_10kpredictions <- predict(lasso_kfold, newdata = test_features)

# RMSE
sqrt(mean((test_target - lasso_10kpredictions)^2))

# R2
cor(test_target, lasso_10kpredictions) ^ 2
