# First, create a virtual environment
# use shift + cmd + P (or F1) to open the Command Palette 
# Type Python and select create python terminal 
# In the terminal, type python -m venv venv
# You've created your virtual environment!


# Second step: load the libraries you'll need
# install them directly in the terminal (below) using ' pip install pkg_name ' or...

import numpy as np # can be used to perform a wide variety of mathematical operations on arrays.
import pandas as pd # mainly used for data analysis and associated manipulation of tabular data in DataFrames
import matplotlib as mpl # comprehensive library for creating static, animated, and interactive visualizations in Python
import sklearn as sk #  implement machine learning models and statistical modelling.

# Let's start with the basics 

# Python works with the following list of elements:
# literals, variables, operators, delimiters, keywords, comments

# Assigning VARIABLES with LITERALS
an_integer = 4 # integer
a_float = 4.5 # numeric/float
a_boolean = False # boolean, true or false
a_string = "Number of literals:"

print(a_string , an_integer)

# In Python the data type is derived from the literal and does not have to be described explicitly, unlike in R
# Like in R, you can overwrite a variable (i.e. an object) with new information
an_integer = 7
print(an_integer)

# select a snippet of the script and use SHIFT + ENTER to only run that snippet (and not the whole script)

# Arithmetic operators

5 + 1 
5*3 
6 == 100 # returns FALSE
6 != 100 # returns TRUE
20 / 2
2 ** 3 # 2 to the power of 3, ** is used for exponentiation

# keywords and comments

a_var = "This is a variable, which contains a string." #and this is a comment about the string variable
print(a_var)

'''
This is a comment box, everything in here will not be read as a command by python,
and therefore will not be executed.
This is also known as a docstring.
'''

# data types
str("hello")
str(a_string)
float(a_string) # returns error, cannot convert a string to float! 

# lists: they are ordered arrays of elements, accesible via an index, e.g.


many_strings = ["Tolkien", "Orwell", "Austen"]
many_strings[0] # returns first element of list
# note that python starts counting from zero

many_strings.append("Mephistopheles")
print(many_strings) # we have succesfully appended an elemnt to our list

many_strings.insert(2,"Shrek") # Insert element Shrek in position 3 of list (0,1,2)
print(many_strings)

# finally, sort the list... 
many_strings.sort()
print(many_strings)
# python orders alphabetically :) 


# Let's use a toy dataset to get some basics done
    # import sklearn dataset
from sklearn.datasets import load_diabetes
    # load the  dataset, assign it to an object

diabetes = load_diabetes()
print(diabetes.DESCR) # DESCR is a description option from sklearn library.  Be mindful of the Note!


# print dataset description with pandas
# 1. convert the dataframe object into a pandas dataframe object
pandas_diabetes = pd.DataFrame(diabetes.data, columns=diabetes.feature_names)

# now we can use the handy describe() option from pandas
pandas_diabetes.describe()
pandas_diabetes.info()
'''
The diabetes dataset has 10 variables, including age of respondent, sex, bmi, etc...
Note that we also get (8) descriptive statistics of each variable in the dataset. 
'''

len(pandas_diabetes) # returns lenght of object = 442 data points

# print the first 4 observations of the variables
pandas_diabetes.head()

# pip install tabulate in the terminal. Split the terminal first and run the command on a bash terminal window

# this prints the dataframe nicely (but remember to only print the head/first observations, otherwise you display a large table!)
print(pandas_diabetes.head().to_markdown())

# Basic operations and linear model estimation

pandas_diabetes.iloc[:,1] # print first column
pandas_diabetes.iloc[:,1].mean() # get the mean of the first column

# Linear Model (OLS), split data into dep Y and indep X
'''
Luckily for us, because the diabetes toy dataframe is from the sklearn library, you can use the target and data functions.
For the same reason, we will use the diabetes dataset and not the pandas-converted dataset
'''
Y = diabetes.target # define targer/ Y var
X = diabetes.data # all remaining variables in the dataset are now X covariates

print(Y.shape, X.shape) # y is one column with 442 observations, X is 10 columns with 442 observations
print(Y)

from sklearn.linear_model import LinearRegression

lm = LinearRegression(fit_intercept=True).fit(X, Y)

#print model coefficients using pandas

print(lm.coef_) # displayed in the order of the column names
print(lm.intercept_) # model intercept = 152.133

# now in a nice way
column_names = pandas_diabetes.columns # to retrieve column names we should use the pandas dataset
coefficients = pd.concat([pd.DataFrame(column_names),pd.DataFrame(np.transpose(lm.coef_))], axis = 1)
print(coefficients)

# an ever more intuitive output would look like this 
# remember to pip install the libraries before importing them! And to do so in a different terminal (open a new bash terminal)
import statsmodels.api as sm
from scipy import stats

constant = sm.add_constant(X)
est = sm.OLS(Y, constant) # estimate a linear regression model
lm_2 = est.fit() # create an object that contains all the models' parameters
print(lm_2.summary()) # print linear model parameters

# Finally, matplotlib!

import matplotlib.pyplot as plt

pandas_diabetes.head() # to remember our variables

bmi = pandas_diabetes.iloc[:,2] # select and store bmi variable
age = pandas_diabetes.iloc[:,0] # select and store age variable

plt.hist(bmi)
plt.show() 

plt.scatter(age,bmi) # order is x,y
plt.show()

# let's add some elements#add line of best fit to plot

# find line of best fit
a, b = np.polyfit(age, bmi, 1)

# add the points to the plot
plt.scatter(age,bmi) # order is x,y
# add the line and then display
plt.plot(age, a*age+b, color='red', linestyle='--', linewidth=2)
plt.show()

# control flow

# for loops, to iterate over some length and repeat an operation, e.g.
count = 0 # initialize empty variable
nums = [1,2,3,4,5,6,7,8,9,10]

for i in nums:
    count += i
    
print(f"We have a total sum of: {count}.")

# we've essentially set a counter: 1 + 2 + 3 + 4  + ... 10 = 55!

if 101 == 100: 
    print("Hello, 5!")
elif 101 > 100:
    print("Much gold!")

# if logical statement is true, print(), else if new logical statement is true, print()

# defining functions: lottery draw!


def French_talk(name,age):
    print("Je m'appelle", name)
    print("Mon âge est", age)
    
French_talk("Saruman",1000)
