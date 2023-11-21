#################################################################################################### 
##
## Project: Linear Models and Lasso Regression
## Script purpose: A primer on Machine Learning and Supervised Predictive Modeling for Public Policy
## Encoding: UTF-8
## Packages: dplyr, tidyverse, caret, corrplot, Hmisc, modelsummary, skimr, plyr, gt, stargazer, elasticnet
## Date: 16 November 2023
## Author: Michelle González Amador
## Version: R ‘4.3.1’ // Rstudio ‘2023.9.1.494’
## Notes v00:
##
####################################################################################################
rm(list = ls()) # clear working environment
setwd("~/Desktop/malawi") # set working directory
getRversion() # print your R version
RStudio.Version()$version # print your Rstudio version

### 1. Libraries ####
cat("
    
If this is your first time using R, you need to install the libraries before loading them. 
To do that, you can uncomment the next line by removing the # symbol.    
    ")

#install.packages("dplyr", "tidyverse", "caret", "corrplot", "Hmisc", "modelsummary", "plyr", "gt", "stargazer", elasticnet")

library(dplyr) # core package for dataframe manipulation. Usually installed and loaded with the tidyverse, but sometimes needs to be loaded in conjunction to avoid warnings.
library(tidyverse) # a large collection of packages for data manipulation and visualisation.  
library(caret) # a package with key functions that streamline the process for predictive modelling 
library(corrplot) # a package to plot correlation matrices
library(Hmisc) # a package for general-purpose data analysis 
library(modelsummary) # a package to describe model outputs
library(skimr) # a package to describe dataframes
library(plyr) # a package for data wrangling
library(gt) # a package to edit modelsummary (and other) tables
library(stargazer) # a package to visualise model output
#library(elasticnet) # package used in conjunction with caret for lasso ml models

### 2. Upload the data ####
data_malawi <- read_csv("malawi.csv") # the file is directly read from the working directory/folder

### 3. Get to know your data: visualisation and pre-processing ####
cat("

A key problem in the design of Social Policies is the identification of people in need of social assistance.
Social policies usually work with tight budgets and limited fiscal space. To allocate resources efficiently,
benefits need to be targeted to those who need them most. Yet, identifying needs isn’t easy 
and misclassifications can have severe and irreversible effects on people in need.

Think of a social protection programme that allocates food vouchers to families with children at risk of malnutrition, 
or a programme that establishes needs-based school grants. What happens when these limited and finite resources are given
to people that could do without, and those who need them most are excluded from them?

In this block we'll work with real-world data from the country of Malawi to predict cash-transfer programme beneficiaries:
People who live in poverty and need government assistance to make ends meet. The data comes from McBride and Nichol's (2018) paper 
'Retooling poverty targeting using out-of-sample validation and machine learning': 2004-2005 Malawi Second Integrated Household Survey (IHS2).
    ")

# = = = = = = = = = # 
# Summary of dataset
# = = = = = = = = = # 

skim(data_malawi)

cat("
    
- The dataset contains 38 variables and 11,280 observations.
- Not all of these variables are relevant for our prediction model.
- To find the labels and description of the variables, you can refer to the paper:
https://academic.oup.com/wber/article-abstract/32/3/531/2447896

[hhsize, hhsize2, age_head, age_head2, regions, rural, never
married, share_of_adults_without_education, share_of_adults_who_can_read, 
number of rooms, cement floor, electricity, flush toilet, soap, bed,
bike, music player, coffee table, iron, garden, goats]

- Luckily for us, we have no missing values (n_missing in summary output)!
A) Machine learning models cannot be trained when missing values are present.
B) Dealing with missingness is a non-trivial task: 
First and foremost, we should assess whether there is a pattern to missingness and if so, 
what that means to what we can learn from our (sub)population. If there is no discernible pattern, 
we can proceed to delete the missing values or impute them. A more detailed explanation and course of action
can be found here: https://stefvanbuuren.name/fimd/sec-MCAR.html

    ")

# = = = = = = = = = # 
# Subsetting dataset
# = = = = = = = = = # 

cat("

As part of our data pre-processing we will subset the dataframe, such that only relevant variables are left.
* Relevant: variables/features about a household that could help us determine whether they are in poverty.
That way, we save some memory space; but also, we can call the full set of variables in a dataframe in one go!

variables to delete (not included in the identified set above):
[ea, EA, hhwght, psu, strataid, lnzline, case_id, eatype] 
Note that we will leave some variables, such as grassroof, which were not identified in the previous set, for relevance illustration purposes. 
    ")

# object:vector that contains the names of the variables that we want to get rid of

cols <- c("ea", "EA", "psu","hhwght", "strataid", "lnzline", "case_id","eatype")


# subsett of the data_malawi object:datframe
data_malawi <- data_malawi[,-which(colnames(data_malawi) %in% cols)] 

cat("
    
A few notes for you:
- a dataframe follows the form data[rows,colums]
- colnames() is a function that identifies the column names of an object of class dataframe
- which() is an R base function that gives you the position of some value
- a minus sign will delete either the identified position in the row or the column space

    ")

# = = = = = = = = = # 
# data visualization
# = = = = = = = = = # 

cat("

A quick and effective way to take a first glance at our data is to plot histograms of relevant 
(continuous) features.

Recall (from the skim() dataframe summary output) that only two variables are non-numeric. However,
we need to make a distinction between class factor and numeric/numeric.
    ")

# identify categorical variables to transform from class numeric to factor
# print the number of unique values by variable

for (i in 1:ncol(data_malawi)) { # iterate over the length of columns in the data_malawi df
    
    # store the number of unique values in column.i 
    x <- length(unique(data_malawi[[i]]))
    
    # print the name of column.i
    print(colnames(data_malawi[i]))
    # print the number of unique values in column.i
    print(x)
    
}

cat("
Notice that we have a few variables with 2 unique values and one variable with 3 unique values. We should transform
these into factor() class. We can do this one by one, or in one shot. I'll give an example of both:
    ")
data_malawi$north <- factor(data_malawi$north)
str(data_malawi$north) # 2 is labelled as "1" and 1 is labelled as "0". This is not an issue. For future reference, you can label the categories.
head(data_malawi$north) # first 6 observations are 1 = north
tail(data_malawi$north) # last 6 observations are 0 = not north
levels(data_malawi$north) <- c("not_north","north") # the order of the label matters (label for "0" comes firts.)
head(data_malawi$north) # first 6 observations are north
tail(data_malawi$north) # last 6 observations are not north

# long story short, R stores factors as 1...n; but the label remains the numeric value assigned (1 for true, 0 false)

# transform all binary/categorical data into factor class

min_count <- 3 # vector: 3 categories is our max number of categories found

# apply a lenght(unique(x)) function to all the columns (rows = 1, columns = 2) of the data_malawi dataframe, then
# store boolean (true/false) if the number of unique values is lower or equal to the min_count vector
n_distinct2 <- apply(data_malawi, 2, function(x) length(unique(x))) <= min_count

print(n_distinct2) # prints boolean indicator object

# select the identified categorical variables and transform them into factors
data_malawi[n_distinct2] <- lapply(data_malawi[n_distinct2], factor) 

# skim your dataframe 
skim(data_malawi) #success!

# Select all variables in the dataframe which are numeric, and can therefore be plotted as a histogram.
malawi_continuous <- as.data.frame(data_malawi %>% select_if(~is.numeric(.))) 

# a quick glance at the summary statistics of our continuous variables
datasummary_skim(malawi_continuous) # from modelsummary pkg, output as plot in Plot Viewer

# Hmisc package, quick and painless hist.data.frame() function
# but first, make sure to adjust the number of rows and columns to be displayed on your Plot Viewer
par(mfrow = c(3, 3)) # 3 rows * 3 columns (9 variables)
hist.data.frame(malawi_continuous)

# Let's draw bar plots of the categorical variables
malawi_factor <- data_malawi %>% select_if(~is.factor(.)) # subset of the data with all factor variables

par(mfrow = c(3, 7)) # 7 rows, 3 columns (21 variables = length of df)

for (i in 1:ncol(malawi_factor)) { # Loop over all the columns in the factor df subset
    
    # store data in column.i as x
    x <- malawi_factor[,i]
    
    # store name of column.i as x_name
    x_name <- colnames(malawi_factor[i])
    
    # Plot bar graph of x using Rbase plotting tools
    barplot(table(x),
            main = paste(x_name)
    )
    
}

# We can also show tables of all factor variables (to get precise frequencies not displayed in bar plots)
llply(.data=malawi_factor, .fun=table) # create tables of all the variables in dataframe using the plyr package

cat("
What have we learned from the data visualisation?
a) Nothing worrying about the data itself. McBride and Nichols did a good job of pre-processing the data for us. 
No pesky missing values, or unkown categories.

b) From the bar plots, we can see that for the most part, people tend not to own assets. 
Worryingly, there is a lack of soap, flush toilets and electricity, all of which are crucial for human capital (health and education).

c) From the histograms, we can see logged per capita expenditure is normally distributed, 
but if we remove the log, it's incredibly skewed. Poverty is endemic. 
Households tend not to have too many educated individuals, and their size is non-trivially large (with less rooms than people need).

    ")

# = = = = = = = = = = = = = = = =# 
# Relationships between features
# = = = = = = = = = = = = = = = =#

cat("
To finalise our exploration of the dataset, we should define:
- the target variable (a.k.a. outcome of interest)
- correlational insights

Let's visualise two distinct correlation matrices; for our continuous dataframe, which includes our target variable, 
we will plot a Pearson r correlation matrix. For our factor dataframe, to which we will add our continuous target,
we will plot a Spearman rho correlation matrix. Both types of correlation coefficients are interpreted the same. 
    ")
par(mfrow=c(1,1)) # let's use the entirety of the grid from the Plot Viewer for our correlation matrix plot

# = = PEARSON CORRELATION MATRIX = = #

M <- cor(malawi_continuous) # create a correlation matrix of the continuous dataset, cor() uses Pearson's correlation coefficient as default. This means we can only take the correlation between continuous variables
corrplot(M, method="circle", addCoef.col ="black", number.cex = 0.8) # visualise it in a nice way

cat("
We can already tell that the size of the household and dependent ratio are highly negatively correlated to our target variable.    
    ")

# = = SPEARMAN CORRELATION MATRIX = = #

malawi_factorC <- as.data.frame(lapply(malawi_factor,as.numeric)) # coerce dataframe to numeric, as the cor() command only takes in numeric types
malawi_factorC$lnexp_pc_month <- malawi_continuous$lnexp_pc_month # include target variable in the dataframe

M2 <- cor(malawi_factorC, method = "spearman")
corrplot(M2, method="circle", addCoef.col ="black", number.cex = 0.8) # visualise it in a nice way

cat("
Ownership of some assets stand out: soap, a cement floor, electricity, a bed... owndership of these (an a couple of other) assets is positively correlated to per capita expenditure.
Living in a rural area, on the other hand, is negtively correlated to our target variable. 

We can also spot some correlation coefficients of zero. In some situations, the data generating mechanism can create predictors that only have a single unique value (i.e. a “zero-variance predictor”). 
For many ML models (excluding tree-based models), this may cause the model to crash or the fit to be unstable. Here, the only 0 we've spotted is not in relation to our target variable. 
But we do observe some near-zero-variance predictors. Besides uninformative, these can also create unstable model fits. There's a few strategies to deal with these; the quickest solution is to remove them.
A second option, which is especially interesting in scenarios with a large number of predictors/variables, is to work with penalised models. We'll discuss this option below. 
    ")

### 4. Model fit: data partition and performance evaluation parameters ####
cat("
We now have a general idea of the structure of the data we are working with,
and what we're trying to predict: per capita expenditures, which we believe are a proxy for poverty prevalence.

The next step is create a simple linear model (OLS) to predict per capitra expenditure using the variables in our dataset,
and introduce the elements with which we will evaluate our model.
    
    ")

# = = = = = = = = = # 
# Data partitioning
# = = = = = = = = = # 

cat("
When we want to build predictive models for machine learning purposes, it is important to have two data sets.
A training data set from which our model will learn, and a test data set containing the same features as our training data set. 
To split our main data set into two, we will work with an 80/20 split. 

The 80/20 split has its origins in the Pareto Principle, which states that 'in most cases, 80% of effects from from 20% of causes'. 
Without other relevant knowledge of the source or shape of the data, this partitioning method is a good place to start 
and indeed standard in the machine learning field.    
    ")

# First, set a seed to guarantee replicability of the process
set.seed(1234) # you can use any number you want, but to replicate the results in this tutorial you need to use this number

# We could split the data manually, but the caret package includes an useful function

train_idx <- createDataPartition(data_malawi$lnexp_pc_month, p = .8, list = FALSE, times = 1)
head(train_idx) # notice that observation 5 corresponds to resame indicator 7 and so on. We're shuffling and picking!

Train_df <- data_malawi[ train_idx,]
Test_df  <- data_malawi[-train_idx,]

# Note that we have created training and testing dataframes as an 80/20 split of the original dataset.

# = = = = = = = = = = = = # 
# Linear Model prediction
# = = = = = = = = = = = = #

cat("
We will start by fitting a predictive model using the training dataset; 
that is, our target variable *log of monthly per capita expenditures* or *lnexp_pc_month* will be a 
$Y$ dependent variable in a linear model $Y_i = \alpha + x'\beta_i + \epsilon_i$, and the remaining features in the data frame correspond to the row vectors $x'\beta$.    
    ")

model1 <- lm(lnexp_pc_month ~ .,Train_df) # the dot after the squiggle ~ asks the lm() function tu use all other variables in the dataframe as predictors to the dependent variable lnexp_pc_month
stargazer(model1, type = "text") # printed in the console as text
ms <- modelsummary(model1,
             vcov = list("iid","robust"), # include iid and HC3 (robust) standard errors
             statistic = c("p = {p.value}","s.e. = {std.error}"),
             stars = TRUE,
             output = "gt"
             ) # plotted as an image / object in "gt" format

ms %>% tab_header(
    title = md("**Linear Models with iid and robust s.e.**"),
    subtitle = md("Target: (log) per capita monthly expenditure")
)

# Keep in mind here that variable region was not included in the output (this is because we already have dummies of north (not north = south), and central region)

cat("
Recall that one of the assumptions of a linear model is that errors are independent and identically distributed.
We could run some tests to determine this, but with the contrast of the iid and robust errors in the modelsummary output table
we can already tell that this is not an issue/something to worry about in our estimations. 
    ")

# Besides the regression output table, we can can also visualise the magnitude and significance of the coefficients with a plot
modelplot(model1) + 
          aes(color = ifelse(p.value < 0.001, "Significant", "Not significant")) +
              scale_color_manual(values = c("grey", "black")
                                 )
           # grey points indicate statistically insignificat (p>0.001) coefficient estimates
           # The scale of the plot is large due to the intercept estimate

# = = = = = = = = = = = = = = # 
# Model Performance Indicators
# = = = = = = = = = = = = = = #

cat("
In predictive modelling, we are interested in the following performance metrics:

- Model *residuals*: recall residuals are the observed value minus the predicted value. We can estimate a model's
Root Mean Squared Error (RMSE) or the Mean Absolute Error (MAE). Residuals allow us to quantify the extent to which
the predicted response value (for a given observation) is close to the true response value. Small RMSE or MAE values
indicate that the prediction is close to the true observed value.

- The *p-values*: represented by stars ***, they indicate the predictive power of each feature in the model. 
In the same vein, the magnitude of the coefficient is also important, especially given that we are interested in explanatory power and not causality. 

- The *R-squared*: arguably the go-to indicator for performance assessment. 
Low R^2 values are not uncommon, especially in the social sciences. However, when hoping to use a model for predictive purposes, a low R^2 is a bad sign, large number of statistically significant features notwithstanding. 
The drawback from relying solely on this measure is that it does not take into consideration the problem of model over-fitting; i.e. you can inflate the R-squared by adding as many variables as you want, even if those variables have little predicting power. 
This method will yield great results in the training data, but will under perform when extrapolating the model to the test (or indeed any other) data set.

    ")

# = = Model Residuals = = #
print(summary(model1$residuals))

cat("
Recall the residual is estimated as the true (observed) value minus the predicted value. Thus:
The max(imum) error of 1.88 suggests that the model under-predicted expenditure by circa (log) \$2 (or \$6.5) for at least one observation. 
Fifty percent of the predictions (between the first and third quartiles) lie between (log) \$0.28 and (log) \$0.26 over the true value.
From these data, we obtain the popular measure of performance evaluation known as the Root Mean Squared Error (RMSE, for short). 
    ")

# Calculate the RMSE for the training dataset, or the in-sample RMSE.

# 1. Predict values on the training dataset
p0 <- predict(model1, Train_df)

# 2. Obtain the errors (predicted values minus observed values of target variable)
error0 <- p0 - Train_df[["lnexp_pc_month"]]

# 3. In-sample RMSE
RMSE_insample <- sqrt(mean(error0 ^ 2))
print(RMSE_insample)

cat("
The RMSE (0.4271) gives us an absolute number that indicates how much our predicted values deviate from the true (observed) number. 
Think of the question, *how far, on average, are the residuals away from zero?* Generally speaking, the lower the value, the better the model fit. Besides being a good measure of goodness of fit, the RMSE is also 
useful for comparing the ability of our model to make predictions on different (e.g. test) data sets. The in-sample RMSE should be close 
or equal to the out-of-sample RMSE.

In this case, our RMSE is ~0.4 units away from zero. Given the range of the target variable (roughly 4 to 11),
the number seems to be relatively small and close enough to zero. 
    ")

# = = P-values = = #
cat("
Recall the large number of statistically significant features in our model. The coefficient plot, where we
indicate that statistical significance is defined by a p-value threshold of 0.001, a strict rule, given the
popularity of the more relaxed 0.05 critical value, shows that only 4 out of 29 features/variables do not meet
this criterion. We can conclude that the features we have selected seem relevant.
    ")

# = = R-squared = = #
print(summary(model1))

cat("
We have printed our model's output once more. The estimated (Multiple) R-squared of 0.59 tells us that
our model predicts around 60 per cent of the variation in the independent variable (our target, log of per capita monthly expenditures).
Also note that when we have a large number of predictors, it's best to look at the Adjusted R-squared (of 0.59),
which corrects or adjusts for this by only increasing when a new feature improves the model than what would be expected by chance. 
    ")

# = = Out of Sample Predictions = = #

cat("
Now that we have built and evaluated our model, we can proceed to make out-of-sample predictions.
    ")

# Use the model generated with the train dataset to make prediction with the test dataset

p <- predict(model1, Test_df)
print(summary(data_malawi$lnexp_pc_month)) # observed summary statistics of target variable in full dataset
print(summary(p0)) # predictions based on the training dataset
print(summary(p)) # predictions from the testing dataset

cat("
The summary statistics for the predictions with the train and test datasets are very close to one another.
This is an indication that our model extrapolates well to other datasets.
Compared to the observed summary statistics of the target variable, they're relatively close, with the largest
deviations observed at the minimum and maximum values. 
    ")

# = = The bias-variance tradeoff in practice = = #
cat("
We previously mentioned that the RMSE metric could also be used to compare between train and test model predictions. 
Let us estimate the out-of-sample RMSE:    
    ")

error <- p - Test_df[["lnexp_pc_month"]] # predicted values minus actual values
RMSE_test <- sqrt(mean(error^2))
print(RMSE_test) # this is known as the out-of-sample RMSE

cat("
Notice that the *in-sample RMSE*[0.4271572] is very close to the *out-of-sample RMSE*[0.4284404]. 
This means that our model makes consistent predictions across different data sets. We also know by now 
that these predictions are relatively good. At least, above average good. What we are observing here is a model 
that has found a balance between bias and variance. However, both measures can still improve. 

*What do we mean by bias in a model?*

The bias is the difference between the average prediction of our model and the true (observed) value. 
Minimising the bias is analogous to minimising the RMSE.  

*What do we mean by variance in a model?*

It is the observed variability of our model prediction for a given data point (how much the model can adjust given the data set). 
A model with high variance would yield low error values in the training data but high errors in the test data.
    ")

# = = K-fold Cross-validation = = #
cat("
K-fold with the caret() pkg:
What is cross-validation? Broadly speaking, it is a technique that allows us to assess the performance of our machine learning model. 
How so? Well, it looks at the 'stability' of the model. It's a measure of how well our model would work on new, unseen data; 
i.e. it has correctly observed and recorded the patterns in the data and has not captured too much noise (what we know as the error term, 
or what we are unable to explain with our model). K-fold cross-validation is a good place to start for such a thing. In the words of 
[The Internet™](https://www.javatpoint.com/cross-validation-in-machine-learning), what k-fold cross validation does is:
   
Split the input dataset into K groups
- For each group:
        -Take one group as the reserve or test data set.
        -Use remaining groups as the training dataset.
        -Fit the model on the training set and evaluate the performance of the model using the test set.    
     ")

# Let's rumble! 

set.seed(12345)

# create an object that defines the training method as cross-validation and number of folds (caret pkg)
cv_10fold <- trainControl(
    method = "cv", #cross-validation
    number = 10 # k-fold = 10-fold (split the data into 10 similar-sized samples)
)

set.seed(12345)

# train a model 
ols_kfold <- train(
    lnexp_pc_month ~ .,
    data = Train_df,
    method = 'lm', # runs a linear regression model (or ols)
    trControl = cv_10fold # use 10 folds to cross-validate
)
ols_kfold2 <- train(
    lnexp_pc_month ~ .,
    data = Test_df,
    method = 'lm', # runs a linear regression model (or ols)
    trControl = cv_10fold # use 10 folds to cross-validate
)

ols_kfold3 <- train(
    lnexp_pc_month ~ .,
    data = data_malawi,
    method = 'lm', # runs a linear regression model (or ols)
    trControl = cv_10fold # use 10 folds to cross-validate
)

print(ols_kfold)
print(ols_kfold2)
print(ols_kfold3)

cat("
Having cross-validated our model, we can see that the results are virtually the same. 
We have a RMSE of .43, an R^2 of .59, (and a MAE of .33). We ran the same model for the
test and full datasets and find these results are consistent. Recall the R^2 tells us the model's 
predictive ability and the RMSE and MAE the model's accuracy. Cross-validation, fyi, is a tool to 
help us assess the out-of-sample performance of our model; i.e. how well our model extrapolates to other datasets. 
    
NOTE: k-fold cross-validation replaces our original 80/20 split (we don't need to do that anymore!)
Therefore, we use the full dataset in the train() function. Using the train,test and full datasets in
the example above was just for pedagogical purposes but it is no longer necessary and we can use the
reported estimates of R^2 (close to 1!), RMSE (close to 0!) and MAE (let's go low!) for model assessment without comparing them to another set of predictions.
    ")



# 5. Feature selection with Lasso ####
cat("
An OLS regression is not the only model that can be written in the form of $Y_i = \alpha + \beta_1X_{1i}, \beta_2X_{2i},..., \beta_pX_{pi}+ u_i$. 
In this section we will discuss 'penalised' models, which can also be expressed as a linear relationship between parameters. 
Penalised regression models are also known as regression shrinkage methods, and they take their name after the colloquial term for coefficient regularisation, 
'shrinkage' of estimated coefficients. The goal of penalised models, as opposed to a traditional linear model, is not to minimise bias, but to reduce variance 
by adding a constraint to the equation and effectively pushing coefficient parameters towards 0. This results in the the worse model predictors having a 
coefficient of zero or close to zero.     
    ")

cat("
Consider a scenario where you have dozens (maybe thousands?) of predictors. Which covariates are truly important for our known outcome? Including all of the predictors leads to *over-fitting*. 
We'll find that the R^2 value is high, and conclude that our in-sample fit is good. However, this may lead to bad out-of-sample predictions. *Model selection* is a particularly challenging endeavour 
when we encounter high-dimensional data: when the number of variables is close to or larger than the number of observations. Some examples where you may encounter high-dimensional data include:

1. Cross-country analyses: we have a small and finite number of countries, but we may collect/observe as many variables as we want. 

2. Cluster-population analyses: we wish to understand the outcome of some unique population $n$, e.g. all students from classroom A. We collect plenty of information on these students, but the sample 
and the population are analogous $n = N$, and thus the sample number of observations is small and finite. 

The LASSO - Least Absolute Shrinkage and Selection Operator imposes a shrinking penalty to those predictors that do not actually belong in the model, and reduces the size 
of the estimated $\beta$ coefficients towards and including zero (when the tuning parameter / shrinkage penalty $\lambda$ is sufficiently large). Note that $lambda$ is the penalty term called *L1-norm*, 
and corresponds to the sum of the absolute coefficients.    
    ")

# We can use the caret pkg to run a LASSO regression
# Recall we had previously left more variables in the dataset than the original paper.
# We also observed some unnecessary dummy variables and uninformative variables.

### small detour ... 
summary(model1)
# notice that region dummies are repeated and therefore return NA in the output. We should clean this before proceeding.
# these guys are troublesome (recall our conversation about zero-variance predictors?)
data_malawi2 <- data_malawi[,-which(colnames(data_malawi)=="region")]
summary(lm(lnexp_pc_month~., data = data_malawi2)) # no more excluded categorical variables (recall this happened in our original linear model due to repeated measures?)

set.seed(12345)
cv_5fold <- trainControl(
    method = "cv", 
    number = 5 
)
set.seed(12345)
model_lasso <- caret::train(
    lnexp_pc_month ~ .,
    data = data_malawi,
    method = 'lasso',
    preProcess = c("center", "scale"), #This will first center and then scale all relevant variables in the model
    trControl = cv_5fold # use 10 folds to cross-validate // we already know this parameter may not be needed!
    )

print(model_lasso) # best R^2 and RMSE is similar to the one using a linear regression...( and so is the MAE!)

#

# == Parameter tuning == #
cat("
Let's start with centering and scaling variables; why do we do that?
*Centering and Scaling in Lasso*: Recall that the L1-norm puts constraints on the size of the coefficients of the Lasso regression. 
The size, of course, differs based on the different scales the variables are measured. Having 1 and up to 55 electric gadgets at home 
is not the same as earning between 1000 and 100,000 monthly (of whichever currency you want to imagine here). There are two immediate 
consequences of centering and scaling (or normalising). 1) There is no longer an intercept. 2) It is easier to rank the relative magnitude 
of the coefficients post-shrinkage.

Cross-validation in Lasso:
Beyond training and testing dataframes, what a k-fold (commonly 10-fold) cross validation does is resample the data to find an optimal (λ) 
lambda/penalisation for the lasso model and assess its predictive error. Recall: The optimal λ (lambda)/penalisation minimizes the 
out-of-sample (or test) mean prediction error.

    ")

# Let's visualise our lasso model:
plot(model_lasso)
cat("The x-axis is the fraction of the full solution (i.e., ordinary least squares with no penalty)")

cat("
It seems that the fraction used (0.9) is closed to the non-penalised model (or OLS/linear).
    ")


cat("
    
A final note: there is some discussion over whether k-fold or really any cross-validation technique is optimal for choosing the lambda parameter
in Lasso models (see for example, this Coursera [video](https://www.coursera.org/lecture/ml-regression/choosing-the-penalty-strength-and-other-practical-issues-with-lasso-SPr8p)). 
It is true that cross-validation is something that we want to do for ALL our machine learning models. Just make sure to make an informed choice of 
which cv technique you'll implement in your model. 

In the end, Lasso has not helped us improve our prediction abilities (the model performed the same as a simple linear regression).

*Multiple categorical variables is problematic with Lasso!*
Lasso models do not perform well with multiple categorical variables. As is our case. Primarily because, in order to work with categorical
data in our lasso models we encode them as dummies (for example, norht = 1 and 0 otherwise, center = 1 and zero otherwise). Lasso will thus
only consider the dummy, and not the concept of the 'region'. An alternative to that is a Group Lasso. If you'd like to read more on that, 
you can have a look at this Towards Data Science Post (https://towardsdatascience.com/beyond-linear-regression-467a7fc3bafb#:~:text=Lasso%20feature%20selection%20works%20well,with%20few%20non%2Dzero%20coefficients.)
    ")


# Extra:
# Version 4.3.1 of R is experiencing difficulties plotting coefficient relevance of Lasso models with multiple binary predictors. 
# This is not the case with older version of R. This might be solved soon enough, though. Let me show you an example of that for when
# that is the case. We'll run a lasso model only on the continuous variables and plot the coefficient relevance. 
set.seed(12345)
cont_lasso <- caret::train(
    lnexp_pc_month ~ .,
    data = malawi_continuous,
    method = 'lasso',
    preProcess = c("center", "scale"), #This will first center and then scale all relevant variables in the model
    trControl = cv_5fold # use 5 folds to cross-validate // we already know this parameter may not be needed!
)

print(cont_lasso) 
cat("A significant decrease in R^2, although accuracy hasn't changed so drastically.")
plot(varImp(cont_lasso))
cat("
We can see in the Variable Importance plot, that household size is the most relevant feature
in our predictive model. We saw this in the linear model with the full data as well.
    ")


