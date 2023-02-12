#################################################################################################### 
##
## Project: Random Forests for Public Policy Problems
## Script purpose: Introduce an applied example of a Random Forest algorithm
## Encoding: UTF-8
## Packages: caret, tidyverse, data.table
## Date: 12 February 2023
## Author: Michelle González Amador
## Version: R 4.2.2 (2022-10-31)// R studio ‘2022.7.2.576’
##
####################################################################################################

rm(list=ls())
setwd("/Users/michellegonzalez/Documents/GitHub/Machine-Learning-for-Public-Policy")

malawi_2005 <- readRDS("malawi_2005.RDS")
