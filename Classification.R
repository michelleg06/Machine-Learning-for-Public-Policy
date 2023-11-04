#################################################################################################### 
##
## Project: Classification model using probit: LSMS Malawi 2019
## Script purpose: An introduction to classification models in R 
## Encoding: UTF-8
## Packages: tidyverse, data.table, caret, elasticnet, plyr, caTools
## Date: January 30, 2023.
## Author: Michelle González Amador
## Version: R ‘3.6.2’ // Rstudio ‘2022.7.2.576’
## Notes v00:
##
####################################################################################################

rm(list=ls())
setwd("/Users/michellegonzalez/Documents/GitHub/Machine-Learning-for-Public-Policy")

### 0. Libraries ####
library(tidyverse)
library(data.table)
library(caret)
#library(Hmisc)
library(plyr)
library(caTools)
library(elasticnet)

### 1. Upload data and subset data ####

malawi <- fread("malawi_short.csv")

# This dataset is the already clean version of the data we had been working with thus far. Our mission now is to build a prediction model
# for which its target variable is either binary or categorical. This will be a brief introduction to classification models and how to assess their performance

### 2. Binary outcome variable ####
cat("When we first started working with the LSMS data set, we talked about predicting consumption to help the government of malawi identify who should receive aid.
     We even decided to have a glimpse at what a binary outcome would look like instead of our continuous consumption measure.
    Now, we will work with that binary measure: 1 if the person lives on or below the poverty line, and 0 otherwise. The World Bank
    estimates the current poverty line at USD $1.90.")

summary(malawi$Cons_pcpd)
malawi$cons_dummy <- ifelse(malawi$Cons_pcpd<= 1.9,"Y","N")
prop.table(table(malawi$cons_dummy))

cat("More than half of the population lives with USD $1.9 or less per day.")
cat("Don't forget to make sure that your new binary vector is in factor format")

class(malawi$cons_dummy)
malawi$cons_dummy <- as.factor(malawi$cons_dummy)


### 3. Split data into train and test ####

set.seed(777) # use any number you want

train_idx <- createDataPartition(malawi$cons_dummy, p = .8, list = FALSE, times = 1) 

Train_df <- malawi[ train_idx,]
Test_df  <- malawi[-train_idx,]

# Create trainControl object
TrControl <- trainControl(
    method = "cv",
    number = 5,
    summaryFunction = twoClassSummary,
    classProbs = TRUE, # IMPORTANT!
    verboseIter = TRUE
)

### 4. Fit a glm logistic model ####

model1 <- train(
    cons_dummy ~ ., 
    Train_df, 
    method = "glm",
    family="binomial",
    trControl = TrControl,
    preProcess=c("center", "scale")
)

print(model1)
cat("for a roc curve, the preferred Area Under the Curve is 1")
cat("fitting a glm with caret often produces warnings about convergence or probabilities. These warnings can almost always be safely ignored")



### 5. Confusion Matrix #### 

probit <- glm(cons_dummy ~ ., family = binomial(link = "probit"), Train_df)
p <- predict(probit, Test_df, type = "response")

cat("Note that our predictions p are probabilities and not the two integers we expected. We need to get our binary outcome back")


cash <- ifelse(p > 0.5, "Y", "N")
p_class <- factor(cash, levels = levels(Test_df[["cons_dummy"]]))


cat("A confusion matrix allows you to observe the following:
    
    True positive rates
    True negative rates
    False positive rates
    False negative rates")


confusionMatrix(p_class, Test_df[["cons_dummy"]])

cat("Accuracy of our model: 0.9993")
cat("Kappa performance: poor")
cat("True positive rate (Sensitivity): 0.9994")
cat("True negative rate (Specificity): 0.9993")

### 6. Roc Curves ####

# predict on test
p <- predict(probit, Test_df, type = "response")

colAUC(p, Test_df[["cons_dummy"]], plotROC = TRUE)
cat("for a roc curve, the preferred Area Under the Curve is 1")


#NEXT UP: Tuning parameters (Random Forests!) There's gotta be a way to improve performance :)
