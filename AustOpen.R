## Predicting who will win the Men's 2019 Australian Tennis Open  based on data from 2000 to 2019

rm(list=ls())                     # clear workspace

# Load packages
library(dplyr)
library(caret)
library(ggplot2)
library(readr)
library(tidyverse)
library(plyr)
library(dummies)

# Set the working directory
setwd("~/Desktop/ATP")

# Load the training data set
tennis_data <- read.csv("merged.csv",stringsAsFactors = FALSE,header = TRUE)

####################################################################
# Exploring raw data
# Understand  data 
####################################################################

#  View its dimensions        
dim(tennis_data)

# View its class
class(tennis_data)            # dataframe

# Review the first 5 observations
head(tennis_data)

# Explore the structure of the data
str(tennis_data)
glimpse(tennis_data)
# Explore dimensions of the data - 52383 rows and 83 columns

# Check for missing values
is.na(tennis_data)

################################################################# 
#Subsetting data 
################################################################

# Take only the first 26 columns of results data
names(tennis_data)[1:26]

# We want to filter data for the Autralian Tennis Open tournaments so that we can work with a subset of data

aust_open <- tennis_data[tennis_data$Tournament=="Australian Open", 1:26]

# View structure of training data the dplyr way.
glimpse(aust_open)

# Save the dataframe to a csv file to write the csv file into R working folder:
write.csv(aust_open,file = "aust_open.csv", row.names = FALSE)

##############################################################
# Pre-Processing the Training Data (Data Cleaning) 
##############################################################

# Exported aust_open.csv file was exported and a new column was created in Excel to extract the year from the data with non-standardised formatting

a <- read.csv("aus_open.csv",stringsAsFactors = FALSE,header = TRUE)

#############################################################
# Exploratory Data Analysis
#############################################################

glimpse(a)  # view the structure of the training data

summary(a) # descriptive statistics

# Transform character variables into numeric variables 
a$W1 <- as.numeric(a$W1)
a$L1 <- as.numeric(a$L1)
a$WRank <- as.numeric(a$WRank)
a$LRank <- as.numeric(a$LRank)

##########################################################
# encoding categorical features
##########################################################

# Convert categorical variables into factors to represent their levels
a$Location <- factor(a$Location)
a$Tournament <- factor(a$Tournament)
a$Series <- factor(a$Series)
a$Court <- factor(a$Court)
a$Surface <- factor(a$Surface)
a$Best.of <- factor(a$Best.of)
a$Round <- factor(a$Round)
a$Winner <- factor(a$Winner)
a$Loser <- factor(a$Loser)
a$Comment <- factor(a$Comment)


glimpse(a)  # check that structure of categorical variables have converted with levels

#######################################
# Detect Missing values
######################################


complete.cases(a) # view missing values
which(complete.cases(a)) # view which row  has full row values are located in
which(!complete.cases(a)) # view which row  has 'full 'NA' row values are located in

na_vec <- which(complete.cases(a))
na_vec <- which(!complete.cases(a)) # create a vector for NA values

# a[-na_vec]     # vector with NA rows removed. We do not want to remove all rows as it will impact on  observations

sum(is.na(a))     # Check for any missing values. There are 6810 missing values
mean(is.na(a))    # 10.4 % of data is missing values (5% is the acceptable threshold) hence I will remove the columns 'Series, Court ' which have the most missing values
colSums(is.na(a)) # Number of missing per column/variable

# To get a percentage of missing value of the attributes
sapply(a, function(df){
  sum(is.na(df) ==TRUE)/length(df);
})

# install.packages("Amelia") in the console
library(Amelia)
require(Amelia)

# plot the missing value map
missmap(a, main = "Missing Map")

## missing values are significant in Lsets, Wsets, L5, W5, L4, W4 and L1)
###########################################
# Impute missing values
############################################
# Techniques from the blog post https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

# remove categorical variables
a.mis <- subset(a, select = -c(Location, Tournament,Date, Series, Court,Surface, Round, Best.of, Winner, Loser, Comment))
summary(a.mis)

# install.packages("mice") into the console
library(mice)
md.pattern(a.mis)  # plot of the pattern of the missing values

# install.packages("VIM") into the console
library(VIM)    # plot the missing values
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(a.mis), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Impute missing values with "pmm" - predicted mean matching. m=5 imputed data sets is default
imputed_Data <- mice(a.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

# inspect that missing data has been imputed
imputed_Data$imp$Lsets

# check imputed method
imputed_Data$meth

# Inspecting the distribution of the original and plotted data
xyplot(imputed_Data,WRank ~ W1+L1+W2+L2+W3+L3+W4+L4+L5+W5+LRank,pch=18,cex=1)

# density plot
densityplot(imputed_Data)

# View the data as individual points
stripplot(imputed_Data, pch = 20, cex = 1.2)


# Create dummy variables for the categorical variables with more than 2 levels
library(dummies)

Round <- dummy(a$Round)
Best.of <- dummy(a$Best.of)
Winner <- dummy(a$Winner)
Loser <- dummy(a$Loser)
Comment <- dummy(a$Comment)

head(a)   # check that the values are been converted to dummy variables
str(a)

summary(a) # Descriptive statistics

#####################################################
# Explore and visualise the data in r
####################################################
# Descriptive statistics for each attribute
library(ggplot2)

# Scatterplot of a subset of data - non-linear
#pairs(imputed_Data[, c("WRank","LRank","W1","L1","W2","L2","L3","W3","L4","W4","L5","W5#","Wsets","Lsets")], main = "tennis results data")

# Density plot of numeric variables

p1 <- ggplot(a, aes(x=a$Year)) + geom_histogram() + ggtitle(" Histogram of Year")
p1

p2 <- ggplot(a, aes(x=a$WRank)) + geom_histogram()+ ggtitle(" Histogram of Winner's Ranking")
p2

p3 <- ggplot(a, aes(x=a$LRank)) + geom_histogram()+ ggtitle(" Histogram of Loser's Ranking")
p3

p4 <- ggplot(a, aes(x=a$W1)) + geom_histogram()+ ggtitle(" Histogram of Winner in the first set")
p4

p5 <- ggplot(a, aes(x=a$L1)) + geom_histogram()+ ggtitle(" Histogram of Loser in the first set")
p5

p6 <- ggplot(a, aes(x=a$W2)) + geom_histogram()+ ggtitle(" Histogram of Winner in the second set")
p6

p7 <- ggplot(a, aes(x=a$L2)) + geom_histogram()+ ggtitle(" Histogram of Loser in the second set")
p7

p8 <- ggplot(a, aes(x=a$W3)) + geom_histogram()+ ggtitle(" Histogram of Winner in the third set")
p8

p9 <- ggplot(a, aes(x=a$L3)) + geom_histogram()+ ggtitle(" Histogram of Loser in the third set")
p9

p10 <- ggplot(a, aes(x=a$W4)) + geom_histogram()+ ggtitle(" Histogram of Winner in the fourth set")
p10

p11 <- ggplot(a, aes(x=a$L4)) + geom_histogram()+ ggtitle(" Histogram of Loser in the fourth set")
p11

p12 <- ggplot(a, aes(x=a$W5)) + geom_histogram()+ ggtitle(" Histogram of Winner in the fifth set")
p12

p13 <- ggplot(a, aes(x=a$L5)) + geom_histogram()+ ggtitle(" Histogram of Loser in the fifth set")
p13

p14 <- ggplot(a, aes(x=a$Wsets)) + geom_histogram()+ ggtitle(" Histogram of Winner set")
p14

p15 <- ggplot(a, aes(x=a$Lsets)) + geom_histogram()+ ggtitle(" Histogram of Loser set")
p15

# visualise categorical variables that have been dummy coded or one-hot encoded

p16 <- plot(x = a$Comment,
            main = "Distribution of Comment", xlab = "Comment",
            ylab = "count")
p16

p17 <- plot(x= a$Winner,main = "Distribution of Winner", xlab = "Winner",
            ylab = "count")
p17

p18 <- plot( x = a$Loser, main = "Distribution of Loser", xlab = "Loser",
             ylab = "Count")
p18

p19 <- plot( x = a$Best.of, main = "Distribution of Best.of", xlab = "Best Of",
             ylab = "Count")
p19

p20 <- plot( x = a$Round, main = "Distribution of Tennis Round", xlab = "Round",
             ylab = "Count")
p20

p21 <- barplot(table(a$Winner), main="Name of Tennis Winners")

xyplot(imputed_Data,WRank ~ W1+L1+W2+L2+W3+L3+W4+L4+L5+W5+LRank,pch=18,cex=1)
