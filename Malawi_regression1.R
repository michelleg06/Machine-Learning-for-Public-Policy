#################################################################################################### 
##
## Project: Predictive Modelling with the Malawi 2019 LSMS data
## Script purpose: A primer on Machine Learning and Supervised Predictive Modeling
## Encoding: UTF-8
## Packages: tidyverse, data.table, caret, corrplot, Hmisc, plyr
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

### 1. Libraries ####

#install.packages("caret", "corrplot", "Hmisc", "plyr")
# Mac M1 Chip (and above) users will get an error installing the last package, please check with Michelle to fix it.

#library(usethis) 
#usethis::edit_r_environ() #use this if you need to increase memory use
library(plyr)
library(tidyverse)
library(data.table)
library(caret)
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
# Also know that you can subset with the column name instead of the number of the column. In this excercise we use the number as a way to introduce the which() function, which may come in handy in the future. 
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

malawi_continuous <- malawi %>% select_if(~is.integer(.) | is.numeric(.)) # this line selects all variables in the dataframe which are integer OR numeric, and can therefore be plotted as a histogram.
hist.data.frame(malawi_continuous) # from the Hmisc package, quick and painless.


# alternatively: (please note that factors, aka categorical data, will be coerced into numeric)
#df <- as.data.frame(lapply(malawi,as.numeric))

malawi_factor <- malawi %>% select_if(~is.factor(.)) # subset of the dataframe containing only factor variables
llply(.data=malawi_factor, .fun=table) # create tables of all the variables in dataframe using the plyr package

# Some of what we can gather from looking at the tables is that there are features for which the levels (also known as categories) are not labeled in a way that we can understand. 
# For example, feature hh_c09 (What is the highest educational qualification [NAME] has acquired) has about 10,012 values under an unnamed educational qualification. 
# Two things may be going on here: 1) the enumerators coded missing values differently from NA (which R automatically reads as missing), or 2) there is a value which was unlabeled and we may need to relabel.

levels(malawi$hh_c09)
# the line above tells us that there are empty cells, recognised by the level "".
head(malawi$hh_c09) # we can confirm that it is indeed an empty cell by looking at the first six values of the dataframe (using the head command, which returns the first six elements). One of the six elements is nothing. 

# now let's create a correlation matrix out of our dataset! (would be great to see all possible correlates, remember the >.5 rule of thumb!)

# correlation matrix
M <- cor(malawi_continuous) # create a correlation matrix of the continuous dataset, cor() uses Pearson's correlation coefficient as default. This means we can only take the correlation between continuous variables
corrplot(M, method="circle", addCoef.col ="black", number.cex = 0.8) # visualise it in a nice way

# Let's compute the Spearman correlation coefficient between categorical variables
malawi_factorC <- as.data.frame(lapply(malawi_factor,as.numeric)) # coerce dataframe to numeric, as the cor() command only takes in numeric types
malawi_factorC$sumConsumption <- malawi_continuous$sumConsumption
M2 <- cor(malawi_factorC, method = "spearman")
corrplot(M2, method="circle", addCoef.col ="black", number.cex = 0.8) # visualise it in a nice way

#OH NO! In some situations, the data generating mechanism can create predictors that only have a single unique value (i.e. a “zero-variance predictor”). For many models (excluding tree-based models), this may cause the model to crash or the fit to be unstable.

#This correlation matrix does not take into account the Spearman correlation btween factor and numeric variables!

### 4. Partition data into training and test data ####
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

### 5. Creating our first predictive model and evaluating its performance: RMSE and R^2 ######

model1 <- lm(sumConsumption ~ .,Train_df) # the dot asks the lm() function tu use all other variables in the df as predictors
summary(model1)
cat(sprintf("The R^2 value of our model, or total variance explained is: %.4f",0.1013))


#The output of our model, obtained with the summary() command, has three important indicators to assess the performance of our model:
    
#Model *residuals*: recall residuals are the observed value minus the predicted value.
print(summary(model1$residuals))

#The max(imum) error of $3290.59$ suggests that the model under-predicted expenses by circa $3,300 for at least one observation. 
#Fifty percent of the predictions (between the first and third quartiles) over-predict the true consumption value by $77 and $12.50. 
#From these data, we obtain the popular measure of perfomance evaluation known as the Root Mean Squared Error (RMSE, for short).

# Calculate the RMSE for the training dataset, or the in-sample RMSE.

# 1. Predict values on the training dataset
p0 <- predict(model1, Train_df)

# 2. Obtain the errors (predicted values minus observed values of target variable)
error0 <- p0 - Train_df[["sumConsumption"]]

# 3. In-sample RMSE
RMSE_insample <- sqrt(mean(error0 ^ 2))
print(RMSE_insample)


#The RMSE ($227.528$) gives us an absolute number that indicates how much our predicted values deviate from the true (observed) number. 
#Think of the question, *how far, on average, are the residuals away from zero?* Generally speaking, the lower the value, the better the model fit. 
#Besides being a good measure of goodness of fit, the RMSE is also useful for comparing the ability of our model to make predictions on different (e.g. test) datasets. 
#The in-sample RMSE should be close or equal to the out-of-sample RMSE.

#The *p-values*: represented by stars *** indicate the predictive power of each feature in the model. In the same line, 
#the magnitude of the coefficient is also important, especially given that we are interested in explanatory power and not causality. 

#The R-squared: arguably the go-to indicator for performance assessment. The total variance explained by our model (R^2) is ~ 10 percent (0.09986). 
#Low R^2 values are not uncommon, especially in the social sciences. However, when hoping to use a model for predictive purposes, 10 percent might not be enough, large number of statistically significant features notwithstanding. The drawback from relying solely on this measure is that it does not take into consideration the problem of model over-fitting; i.e. you can inflate the R-squared by adding as many variables as you want, even if those variables have little predicting power. This method will yield great results in the training data, but will underperform when extrapolating the model to the test (or indeed any other) data.

### 6. Out of sample model predictions ######

#Now that we have built and evaluated our model, we can proceed to make out-of-sample predictions.
p <- predict(model1, Test_df)

#Notice that our prediction produces a warning. The warning is just that, a warning, and it does not stop the program from running or producing results. 
#It does, however, indicate that those results may be meaningless. This is not big news for us at this point, since we have run into some interesting insights along the way: zero-variance predictors in the correlation matrix, a low R-squared value... 

#The bias-variance tradeoff in practice*
#We previously mentioned that the RMSE metric could also be used to compare between training and test model predictions. 
#Let us estimate the ou-of-sample RMSE:
    
error <- p - Test_df[["sumConsumption"]] # predicted values minus actual values
RMSE_test <- sqrt(mean(error^2))
print(RMSE_test) # this is known as the out-of-sample RMSE

#Notice that the *in-sample RMSE*[$227.528$] is very close to the *out-of-sample RMSE*[$235.9987$]. 
#This means that our our model makes consistent predictions across different datasets. 
#However, we also now by now that these predictions are not great. 
#What we are observing here is a model that has not found a balance between bias and variance.

# We can take a first glance at the realised household food consumption vs. the predicted food consumption stored in our object 'p' by selecting the first six elements of our dataset using the command head()

head(cbind(Test_df$sumConsumption,p))

#With the first six households, we're pretty far off the mark. Another way to visualise this is to use a *Confusion Matrix* (2×2 table that shows the predicted values from the model vs. the actual values from the test dataset). 
#Remember we are trying to build a model that can accurately predict a household's food consumption needs. Assume that the government of Malawi has decided to give cash transfers to households who spend less than \$95 a week. 
#Let's create binary variables for the realised outcome and the predicted outcome, which take on the value 1 if the household spends below \$95 a week and 0 otherwise. 
#We can then use those binary variables to build a confusion matrix.

Test_df$realised_consumption <- ifelse(Test_df$sumConsumption<95,1,0)
# How many households spend less than $95 a week on food?
table(Test_df$realised_consumption)
# Answer: 6114!

Test_df$predicted_consumption <- ifelse(p<95,1,0)
# How many households do we predict spend less than $95 a week?
table(Test_df$predicted_consumption)
# Answer: 3545!

# Confusion Matrix manually (it's really just a crosstabulation!)
table(Test_df$realised_consumption,Test_df$predicted_consumption)

# Confusion Matrix from the caret package
confusionMatrix(as.factor(Test_df$realised_consumption), as.factor(Test_df$predicted_consumption)) 

#The confusion matrix already gives us a glimpse on how a bad prediction model can affect the lives of people in need. Beyond what we can read from the crosstabs (or confusion matrix), the caret package confusionMatrix() function also includes some statistical measures of performance. 
# We won't discuss all of them, but focus on the Kappa statistic, a measure of model accuracy that is adjusted by accounting for the possibility of a correct prediction by chance alone. 
#It ranges from 0 to 1, and can be interpreted using the following thresholds:

# Poor = Less than 0.20

# Fair = 0.20 to 0.40

# Moderate = 0.40 to 0.60

# Good = 0.60 to 0.80

# Very good = 0.80 to 1.00

# With a kappa value of 0.2, our model has poor accuracy. Now let's recapitulate:
    
#What do we mean by bias in a model?*
    
#The bias is the difference between the average prediction of our model and the true (observed) value. Minimising the bias is analogous to minimising the RMSE.  

#What do we mean by variance in a model?*
    
#It is the observed variability of model prediction for a given data point. A model with high variance would yield low error values in the training data but high errors in the test data. 
#Our Food Consumption prediction model therefore exhibits a low level of variance (and thus it extrapolates well to other datasets) but a high level of bias. We have created an **under-fitted model!**
    
### 7. Challenge!####
    
#How can we improve our model? We want to avoid under-fitting, i.e. what we did throughout this example. We also want to avoid over-fitting the model. 
#Improve and assess your model following the example above. Here are some suggestions that you can use to improve the model:
    
#- Rethink the predictors: remember the zero-variance predictor in the 'continuous variables' correlation matrix? Perhaps it should be removed from the model. It adds nothing to our model and instead makes unstable predictions. You could also revisit the original dataset with more than 500 features and think of other variables that might be of interest. Finally, there were some factor features (a.k.a. categorical variables) that still had some missing values. Should we get rid of the features? Or get rid of the missing values and keep the features but decrease the sample size? This is up to you!
    
#- Include polynomial transformations: the relationship between any given predictor and the target feature (dependent variable) might be non-linear and thus better explained by a polynomial (squared, cubic) transformation.

#- Other variable transformations: remember the distribution of our consumption variable? It was highly skewed. Perhaps a log transformation might be needed. 

#- Include interaction terms: perhaps the true explanatory power of a feature comes from its interaction with another feature. Say, the effect of education on consumption is dominated by the gender of the household head. We know from previous [research](https://www.povertyactionlab.org/sites/default/files/publication/Briefcase_empowering-women-through-targeted-cash-transfers_north-macedonia_10152021.pdf) that when women are recipients of cash transfers from the government, households spent more of their budget on food. 


