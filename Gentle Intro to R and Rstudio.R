#################################################################################################### 
##
## Project: Workshop on R and Rstudio
## Script purpose: Learn the basics of the R programming language
## Encoding: UTF-8
## Packages: tidyverse
## Date: 28 November 2022
## Author: Michelle González Amador
## Version:
getRversion() # R ‘3.6.2’
RStudio.Version()$version # Rstudio 2022.7.2.576’
## Notes v00:
##
####################################################################################################

#### 1. Set your environment ####

# A good practice is to clear the memory before you start a new project
rm(list=ls()) 

# Open the necessary libraries
#install.packages(tidyverse)
library(tidyverse)

#### 2. Features of R ####
# 2.1 Exammples of data types:

# Numeric (or double): 
numeric_vector <- c(1, 1.0, 65.5)
print(numeric_vector)

#Integers: 
integer_vector <- c(1L, 3L, 45L)
print(integer_vector)

#Logical (or boolean): 
boolean_vector <- c(TRUE, FALSE)
print(boolean_vector)

#Character: 
character_vector <- c("Harry Potter", "Star Wars", "Lord of the Rings")
print(character_vector)

#Factor: (also knows as categorical variables)
factor_vector <- as.factor(c("male","female"))
print(factor_vector)

#Missing: 
NA

# 2.2 Examples of data structures:

#Vectors: 
x <- c(1L,3L,5L,7L,9L) # we call this an integer vector
y <- c(1.3, 1, 5, 7, 11.2) # we call this a numerical (or double) vector

print(x)
print(y)

#Matrices: they have rows and columns containing elements of the same type.
A <- matrix(1:9, ncol=3, nrow=3, byrow= TRUE)
print(A)

#Arrays: A vector is a one-dimensional array. A matrix is a two-dimensional array. In short, an array is a collection of data of the same type.
n <- 5*5*3 
B <- array(1:n, c(5,5,3))
print(B)



#Lists: a list is a one-dimensional, heterogeneous data structure. Basically, it is an object that stores all object types. 

my_list <- list(42, "The answer is", TRUE)
print(my_list)



#Data frames: a data frame is a list of  column vectors. Each vector must contain the same data type, but the different vectors can store different data types. Note, however, that in a data frame all vectors must have the same length. 

a <- 1L:5L
class(a)
b <- c("a1", "b2", "c3", "d4", "e5")
class(b)
c <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
class(c)

df <- as.data.frame(cbind(a,b,c))

str(df)

# Notice that even though vector *a* in dataframe *df* is of class integer, vector *b* is of class character, and vector ^*c* is of class boolean/logical, when binding them together they have been coerced into factors. You'll have to manually transform them into their original class to be able to use them in math operations.

df$a <- as.integer(df$a)
df$b <- as.character(df$b)
df$c <- as.logical(df$c)

str(df)




#### 3. R base commands for data set exploration ####

# some basics to explore your data 
str(mtcars) # show the structure of the object in a compact format
dim(mtcars) # inspect the dimension of the dataset (returns #rows, #columns)
class(mtcars) # evaluate the class of the object
typeof(mtcars) # evaluate the type of the object. typeof returns R storage type (see: https://stackoverflow.com/questions/6258004/types-and-classes-of-variables)
View(mtcars$mpg) # Try to avoid running this one in big and complex databases.
mean(mtcars$mpg) # mean of all elements in vector mpg
sum(mtcars$mpg) # sum of all elements in vector mpg (similar to a column sum)
sd(mtcars$mpg) # standard deviation
median(mtcars$mpg) # median
cor(mtcars$mpg, mtcars$wt) # default is pearson correlation, specify method within function to change it  
table(mtcars$am) #categorical data in a table: counts
prop.table(table(mtcars$am)) #categorical data in a table: proportions

#### 4. Objects and Assignments ####

# Another important feature of the R programming language is that it is object oriented. For the most part, for every function used, there must be an object assigned! Let’s see an example of object assignment with a bivariate linear regression model:
ols_model <- lm(mpg ~ wt, data = mtcars) # lm stands for linear model. In parenthesis, dependent variable first, independent variable after the squiggly.
summary(ols_model)

# If you would like to see the results from your regression, you do not need to run it again. Instead, you print the object (ols_model) you have assigned for the linear model function. Similarly, you can call information stored in that object at any time, for example, the estimated coefficients:
ols_model$coefficients

#### 5. Plotting with and without special libraries ####

# Plotting with R base
plot(mtcars$wt, mtcars$mpg, pch = 14, col = "grey", main ="Mileage and Weight")
abline(ols_model, col ="blue") # Note that to add a line of best fit, we had to call our previously estimate linear model, stored in the ols_model object-

# with base R, you cannot directly assign an object to a plot, you need to use...
p_rbase <- recordPlot()
# plot.new() # don't forget to clean up your device afterwards!

# Plotting using the Grammar of Graphics (ggplot from the tidyverse)

# Steps in the Grammar of Graphics
# 1: linking plot to dataset, 
# 2: defining (aes)thetic mapping, 
# 3: use (geom)etric objects such as points, bars, etc. as markers, 
# 4: the plot has layers

p_ggplot <- ggplot(data = mtcars, aes(x = wt, y = mpg, col=am )) + #the color is defined by car type (automatic 0 or manual 1)
    geom_smooth(method = "lm", col= "orange") + # no need to run a regression ex-ante to add a line of best fit 
    geom_point(alpha=0.5) + #alpha controls transparency of the geom (a.k.a. data point)
    theme(legend.position="none") #removing legend
print(p_ggplot)

p_ggplot <- p_ggplot + theme(legend.position = "right") # I am saving it with the same name again, but I could easily choose another name and keep two versions of the plot. 
print(p_ggplot) # the legend we just added is NOT helpful. Why is that? 

class(mtcars$am) #for legends, we might prefer levels/categories 

mtcars$am <- as.factor(mtcars$am) # we have now transformed the numeric am into a factor variable 
#the importance of assigning objects :)

#  Now we can plot our scatterplot without issues
p_ggplot <- ggplot(data = mtcars, aes(x = wt, y = mpg, col = am )) +
    geom_smooth(method = "lm", col= "red") +
    geom_point(alpha=0.5) +
    theme(legend.position="right") +
    theme_classic() 

print(p_ggplot)

# Shall we continue?

p_ggplot <- p_ggplot + ggtitle("Scatterplot of mileage vs weight of car") +
    xlab("Weight of car") + ylab("Miles per gallon") +
    theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5))


p_ggplot <- p_ggplot + scale_colour_manual(name = "Automatic or Manual", 
                                           labels = c("0.Automatic", "1.Manual"),
                                           values = c("darkorange2", "darkgoldenrod1"))

print(p_ggplot)

# Finally, perhaps we want two lines of best fit that follow the shape of the value dispersion by car type, and not the linear model function?

p <- ggplot(data = mtcars, aes(x = wt, y = mpg, col = am )) +
    geom_smooth() +
    geom_point(alpha=0.5) +
    theme(legend.position="right") +
    theme_classic() + ggtitle("Scatterplot of mileage vs weight of car") +
    xlab("Weight of car") + ylab("Miles per gallon") +
    theme(plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5)) + 
    scale_colour_manual(name = "Automatic or Manual", 
                        labels = c("0.Automatic", "1.Manual"),
                        values = c("darkorange2", "darkgoldenrod1"))
print(p)

#### 6.  R base capabilities and reserved names #####

## Arithmetic and other operations

2+2 #addition
5-2 #subtraction
33.3/2 #division
5^2 #exponentiation
200%/%60 #integer division  [or, how many full hours in 200 minutes?]/aka quotient
200%%60 #remainder [or, how many minutes are left over? ]

## Logical operators
34<35 #smaller than
34<35|33 #smaller than OR than (returns true if it is smaller than any one of them)
34>35&33 #bigger than AND than (returns true only if both conditions apply)
34!=34 #negation
34 %in% 1:100 #value matching (is the object contained in the list of items?)
`%ni%` <- Negate(`%in%`) #let's create a function for NOT IN
1001 %ni% 1:100 # home-made function! :)
34==34 #evaluation

## Indexing

# The tidyverse comes with a cool Starwars dataframe (or tibble, in the tidyverse language). As long as you have loaded the tidyverse library, you can use it.

head(starwars) 
starwars$name[1] #indexing: extract the first element in the name vector
starwars[1,1] #alternative indexing: extract the element of row 1, col 1
starwars$name[2:4] # elements 2, 3, 4 of name vector
starwars[,1] #extract all elements from column 1
#notice that starwars is a tibble, not a dataframe, we'll see the implications of this later
starwars[1,] #extract all elements from row 1
starwars$height[starwars$height>150] # returns a logical vector TRUE for elements >150 in height vector
starwars[c('height', 'name')] #returns c(oncatenated) vectors

#as a note, indexing may vary depending on the object. E.g. matrices within lists are extracted using double brackets [[]]

# 1. not-observed data 
# 1.1 NaN: results that cannot be reasonably defined
h<-0/0
is.nan(h)
class(h)

# 1.2 NA: missing data
colSums(is.na(starwars)) #How many missings in the starwars tibble*?
mean(starwars$height) # Evaluation gives NA?
is.na(starwars$height) # logical vector returns 6 true statements for is.na (coincides with colSums table)
class(starwars$height) # class evaluation returns integer...
mean(as.integer(starwars$height), na.rm = TRUE) # read as integer, ignore NAs :) (rm stands for remove)
# missing values (NA) are regarded by R as non comparables, even to themselves

# 2. if, else, ifelse, function, for, print, length (etc...)
# these words are reserved for control flow and looping 

# if else
meanheight<- 174.358

if ( meanheight > 175) {
    print('very tall!')
} else {
    print('average to normal height')
} 
# a note on using if else: else should be on the same line as if, otherwise it is not recognized

# hand-made function

f1 <- function(x){
    return(sum(x+1))
}

print(f1(5)) # returns value of x+1 when x = 5

# for loops

class(starwars$height) # if it is not integer or numeric, then transform it! 
starwars$height <- as.numeric(as.character(starwars$height)) # transforming the height vector to numeric
starwars$height_judge = NA # if you're using a new object within a for loop, make sure you initialize it before running it

for (i in 1:length(starwars$height)) {
    
    starwars$height_judge[i] <- ifelse(starwars$height[i]<100, "Short", "Tall")
}
print(starwars[c("name", "height_judge", "height")])
