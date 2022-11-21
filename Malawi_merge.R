#################################################################################################### 
##
## Project: Predicting consumption in Malawi using Machine Learning
## Script purpose: ML4PublicPolicy Course Excercise
## Encoding: UTF-8
## Packages: tidyverse, data.table
## Date:7 November
## Author: Michelle González Amador
## Version: R ‘3.6.2’ // Rstudio ‘2022.2.3.492’
## Notes v00:
##
####################################################################################################
rm(list = ls())
setwd("~/Desktop/Malawi2019")
getRversion()
RStudio.Version()$version

##### 1. Libraries ####

# If you are new to R, you must install the packages before opening the libraries

#install.packages("tidyverse","data.table")
library(tidyverse)
library(data.table)

#### 2. Uploading data ####

# We will be working with the Malawi dataset from the LSMS surveys from the World Bank Group. To upload and merge the data, 
# you can refer to the documentation: https://microdata.worldbank.org/index.php/catalog/3818/related-materials

filenames <- list.files(pattern="hh_mod", full.names=TRUE, recursive=FALSE, ignore.case=TRUE)

# data object now contains all files from the 2019 round from the household module
print(filenames)


datList <- lapply(filenames, function(i) {
    df <- read.csv(i, header=TRUE, strip.white=TRUE)
    return(df)
}) # this function should read all the *listed* files from above, convert them into a data frame and store them in a list

head(datList[[1]]) # returns the first elements in the first dataframe from the list datList

# Note that the order of the datasets within datList follows the order of the datasets in filenames, such that dataset hh_mod_a_filt.csv would be 1,  HH_MOD_B.csv would be 2, and so forth.
# A smart thing to do is to clean your Global Environment every so often when you are done with an object, to free up some memory
rm(filenames)

#merge2 <- function(df1, df2){  # Create own merging function
#    merge(df1, df2, by="case_id")
#}
#Reduce(merge2, datList) # functional programming by applying the function sequentially to all the elements of the list using rbase Reduce

#### 3. Merging data ####

# We want to merge the first dataset [module A], containing household level information, with module B, a Household Roster with Individual level information. 
# the variable case_id is the unique identifier at the household level, which is also present in the individual level data
datAB <- merge(datList[[1]], datList[[2]], by="case_id", all = TRUE)

#Let's do some sanity checks to see whether our merging worked correctly:

# number of rows and columns of module A
dim(datList[[1]]) # 11434 rows, 21 columns
# number of rows and columns of module B
dim(datList[[2]]) # 50476 rows, 53 columns
# number of rows and columns of our merged data. Rows MUST be the same as module B
dim(datAB) # 50476 rows # 73 columns
# notice that the number of columns = Cols A + Cols B -1[variable we used to merge]

#Brief overview of our new dataset/dataframe!
str(datAB)

# Something that you might find interesting is that the numeric variable case_id (which we used to merge) looks odd
# This is due to the fact that R is reading this number in scientific form, if you'd like to see the standard number:

options(scipen = 100) # run this line to turn off scientific form, to set it back use options(scipen = 0)

# Now you can run the str(datAB) line again and see the case_id variable in standard form
# Now let's merge dataframe datAB with module C, which contains individual level data on Education
# The individual unique identifier, which exists in Module B, and C (and now also in datAB after the merge) is PID

datABC <- merge(datAB, datList[[3]], by=c("case_id","PID"), all = TRUE)

# We use the household unique identifier case_id, to match households, and the individual unique identifier, PID, to match individuals within each household
# The dataframe should increase in column number, but not in number of observations (feel free to check using dim() or in the global environment)

# Let's select other relevant modules from the IHS5 Household Database: D[Health; case_id PID], E[Time Use & Labour; case_id PID], F[Housing, case_id], H[Food Security, case_id], 
# T[Subjective Assessment of Well-Being, case_id]

datABCD <- merge(datABC, datList[[4]], by=c("case_id","PID"), all = TRUE)
datABCDE <- merge(datABCD, datList[[5]], by=c("case_id","PID"), all = TRUE)
# there's a warning! It isn't a big deal (the code still did it's job), but let's try to sort it anyways
# datABCD contains already variables HHID.x and HHID.y
# datABCDE now contains HHID, HHID.x and HHID.y, so perhaps it would be prudent to delete two of the three duplicates
# let's find out the position of these two columns in the dataframe

which( colnames(datABCDE)=="HHID.x") # 3 and 74
which( colnames(datABCDE)=="HHID.y") # 23 and 129
which( colnames(datABCDE)=="HHID") # 187 (let's keep this guy)
datABCDE <- datABCDE[,-c(3,23,74,129)] # this line removes the two selected duplicated columns.

#now let's continue to merge relevant modules
datABCDEF <- merge(datABCDE, datList[[6]], by="case_id", all = TRUE)
#Up until here, the link between datList dataframe and Module was linear. Now, we need to take into account that there exist modules F1, G1, G2 etc
#You can refer back to Table 10: Structure of the IHS5 Household Database of the Basic Information Document
datABCDEFH <- merge(datABCDEF, datList[[11]], by="case_id", all = TRUE)
datABCDEFHT <- merge(datABCDEFH, datList[[28]], by="case_id", all = TRUE)
which( colnames(datABCDEFHT)=="HHID.x") # 183 454
which( colnames(datABCDEFHT)=="HHID.y") # 340 494
datABCDEFHT <- datABCDEFHT[,-c(454,340,494)]
names(datABCDEFHT)[names(datABCDEFHT) == "HHID.x"] <- "HHID" # change the name of the non-eliminated HHID.x column to HHID

# Perhaps add consumption modules? 

# G1[Food Consumption Over Past One Week; case_id hh_g02] 8
# This module collects information on all food consumed by the household in the past 7 days
datG1 = datList[[8]]
datG2 = datList[[9]] # not actually interested in this module
datG3 = datList[[10]] # the item code is, in fact, not an item code. 


datG1 <- datG1[,c(1,7,8)] # I only want to keep the unique household identifier, item code,  and the total amount consumed in the past week
# we wan to make a household total consumption number


list <- unique(datG1$case_id)
idx  <- NA
hold <- NA
sumConsumption <- NA
temp <- NA
data <- NA

for (i in 1:length(list)) {
        
        idx    <- which(datG1$case_id == list[i])
        hold   <- datG1[idx,]
        sumConsumption  <- sum(hold$hh_g03a, na.rm = TRUE) 
        
        temp <- cbind(hold$case_id,sumConsumption)
        data  <- rbind(data,temp)
        
}

datG <- as.data.frame(data)
# let's keep only one observation per household
datG <- unique(datG)
# Now let's rename the variable and merge our consumption data!
names(datG)[names(datG) == "V1"] <- "case_id"
# The first row is empty, let's get rid of it
datG <- datG[-1,]

#### 4. Exporting data: FINAL DATASET! ####

datABCDEFHTG1 <- merge(datABCDEFHT, datG, by="case_id", all = TRUE)

write_rds(datABCDEFHTG1, "Malawi_2019.rds","xz", compression = 9L)

# checking that it saved correctly
x <- read_rds("Malawi_2019.rds")
str(x)
