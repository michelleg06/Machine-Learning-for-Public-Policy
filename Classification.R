#################################################################################################### 
##
## Project: Classification model using probit: LSMS Malawi 2019
## Script purpose: An introduction to classification models in R 
## Encoding: UTF-8
## Packages: tidyverse, data.table, caret, Hmisc, plyr
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

### 1. Upload data and subset data ####

