#################################################################################################### 
##
## Project: Random Forests for Public Policy Problems
## Script purpose: Introduce an applied example of a Random Forest algorithm
## Encoding: UTF-8
## Packages: caret, tidyverse, data.table, randomForest, party, skimr, rpart, rattle, RColorBrewer
## Date: 12 February 2023
## Author: Michelle González Amador
## Version: R 4.2.2 (2022-10-31)// R studio ‘2022.7.2.576’
## Notes: Do not forget to install.package(".") all the new packages!
####################################################################################################

# 0. Cleaning working environment and setting working directory ####
rm(list=ls())
setwd("/Users/michellegonzalez/Documents/GitHub/Machine-Learning-for-Public-Policy")

# 1. Uploading data and opening libraries ####
malawi_2005 <- readRDS("malawi_2005.RDS")

library(caret)
library(tidyverse)
library(data.table)
library(randomForest)
library(party)
library(skimr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# 2. Getting to know the data ####

cat("What are Random Forests?")

cat("
To implement a Rnadom Forest model, you must first familiarise yourself with the concept of Decision Trees. 
Random Forests are, in fact, made up of multiple decision trees.
Random Forests are, therefore, part of the family of models called Ensemble Learning Models.
The main concept behind Random Forests is that, if you partition the data that would be used to create a single decision tree into different parts, create one tree for each of these partitions, and then use a method to “average” the results of all of these different trees, you should end up with a better model.
The main mechanism behind Random Forests is bagging, which is shorthand for bootstrap aggregating.
Bagging is the concept of randomly sampling some data from a dataset, but with replacement. 
        ")

skim(malawi_2005)

cat("The dataframe contains no missing values. We should, however, transform the character vectors into factor vectors.")

malawi_2005$region <- as.factor(malawi_2005$region)
malawi_2005$eatype <- as.factor(malawi_2005$eatype)

# 3. Random Forest model: randomForest package ####

MyRandomForest <- randomForest(lnexp_pc_month ~ ., data = malawi_2005)
print(MyRandomForest)

cat("
Our Random Forest model did pretty well at explaining per capita monthly consumption of malawians (we used all the 37 non-target variables in the df, but only 12 were used by the model)
The MSE is low at 0.16
The R^2 is high at 64.17%
    ")

# 4. Examining other relevant statistics for our model ####

cat("Which variables are the best predictors?")

importance(MyRandomForest, type=2)

cat("
IncNodePurity relates to the loss function by which splits are chosen. The loss function is MSE for regression.
More useful variables achieve higher increases in node purities; that is, to find a split which has a high inter node 'variance' and a small intra node 'variance'.    
    ")

# 5. Visualising the Random Forest: party package ####

# Let's start with the visualisation of the Increase in Node Purity
varImpPlot(MyRandomForest)
cat("Recall that the Increase in Node Purity expresses the change in the homogeneity of the groups created by the ensemble of trees. ")

# Let's see the actual forest!

Forest_ <- rpart(lnexp_pc_month ~ ., data=malawi_2005)
fancyRpartPlot(Forest_, palettes = c("Oranges","Greens"))

cat("
Orange nodes represent individuals classified by the tree as A. Green nodes would be classified as B. There is only one classification. 
The gradient represents the accuracy of that node.
Each node box displays: 
    the classification, 
    the probability of each class at that node (i.e. the probability of the class conditioned on the node), 
    and the percentage of observations used at that node.
    ")

print(Forest_)
# Interpretation of the Forest_ visualisation object

cat("
The legend, which begins with node) indicates that each node is identified by a number, 
followed by a split (which will usually be in the form of a test on the value of a variable), 
the number of observations $n$ at that node, 
the residual sum of squares (RSS) measuring the level of variance in the error term (the $deviance$), 
the default classification for the node (the $yval$), 
and then the distribution of classes in that node (the $yprobs$) across No and Yes. 
The next line indicates that a “*” denotes a terminal node of the tree (i.e., a leaf node—the tree is not split any further at that node).
    ")

# Challenge! Split the data and crossvalidate the model ####