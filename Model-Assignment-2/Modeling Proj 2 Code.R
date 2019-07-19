library(tidyverse)
library(magrittr)
library(data.table)
library(corrplot)
library(skimr)
library(corrr)
library(broom)
library(glue)

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
# Create a dataframe without any NAs and check correlation
numeric_non_na <- subdat %>%
  select_if(~ !any(is.na(.))) %>%
  select_if(~ is.numeric(.))

sorted.sale.correlations <- correlate(numeric_non_na) %>%
  focus(SalePrice) %>%
  arrange(desc(SalePrice))
fashion(sorted.sale.correlations)

# 1. Simple Linear Regression
# a.
ggplot(subdat, aes(TotalFloorSF, SalePrice))+
  geom_point()+
  geom_smooth(method = lm)
model1 <- lm(SalePrice ~ TotalFloorSF, subdat)
fit1 <- tidy(model1)
fit.stats1 <- glance(model1)

# b.
glue("Model: Y = ", round(fit1$estimate[2], 3), "*", fit1$term[2], " + ", round(fit1$estimate[1], 3))

# c.
glue("R-Squared: ", round(fit.stats1$r.squared, 3))

# d.
summary(model1)
anova(model1)

# e.
