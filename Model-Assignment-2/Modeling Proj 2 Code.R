library(tidyverse)
library(data.table)
library(corrplot)
library(skimr)
mydata <- fread('ames_housing_data.csv')

# Add useful variables
mydata$TotalFloorSF <- mydata$FirstFlrSF + mydata$SecondFlrSF
mydata$HouseAge <- mydata$YrSold - mydata$YearBuilt
mydata$QualityIndex <- mydata$OverallQual * mydata$OverallCond
mydata$logSalePrice <- log(mydata$SalePrice)
mydata$price_sqft <- mydata$SalePrice/mydata$TotalFloorSF

# Data structure
dim(mydata)
names(mydata)
head(mydata)

# Filter to sample population from Module 1
subdat <- filter(mydata, TotalFloorSF < 4000, SaleCondition == "Normal",
                 BldgType == "1Fam", Zoning %in% c("RH","RM","RL"))

# Data Prep EDA
sapply(subdat, function(x) sum(is.na(x))) #Counts of missing values by column
summary(subdat)
skim(subdat)

# Model EDA


# Simple Linear Regression
cont_vars <- select_if(subdat, is.numeric)
cor_vals <- cor(cont_vars)
corrplot(cor_vals)
