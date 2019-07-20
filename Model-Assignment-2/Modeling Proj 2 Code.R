library(tidyverse)
library(magrittr)
library(data.table)
library(corrplot)
library(skimr)
library(corrr)
library(broom)
library(glue)
library(formattable)
library(scales)

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

fashion(sorted.sale.correlations)[2:38,] %>%
formattable(col.names = c("Variable", "Correlation to SalePrice"), row.names = FALSE)

# 1. Simple Linear Regression
# 1a.
ggplot(subdat, aes(TotalFloorSF, SalePrice))+
  geom_point(shape = 21, size = 0.9, color = "#21908CFF")+
  geom_smooth(method = lm, color = "#440154FF")+
  scale_y_continuous(labels = dollar_format())+
  labs(title = "Sale Price by Square Footage")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("model1.png")

model1 <- lm(SalePrice ~ TotalFloorSF, subdat)
fit1 <- tidy(model1)
fit.stats1 <- glance(model1)

# 1b.
glue("Model: Y = ", round(fit1$estimate[2], 3), "*", fit1$term[2], " + ", round(fit1$estimate[1], 3))

# 1c.
glue("R-Squared: ", round(fit.stats1$r.squared, 3))

# 1d.
summary(model1)
anova(model1)

# 1e.
model1residuals <- augment(model1, data = subdat)

ggplot(model1residuals, aes(x = .std.resid))+
  geom_histogram(binwidth = 0.2, color = "black", fill = "#21908CFF")+
  labs(x = "Standardized Residuals", y = "count", title = "Distribution of Model Residuals")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("model1residuals_hist.png")

ggplot(model1residuals, aes(x = .fitted, y = .std.resid))+
  geom_point(shape = 21, size = 0.9, color = "#21908CFF")+
  labs(x = "Predicted Values", y = "Standardized Residuals", title = "Distribution of Model Residuals by Predicted Value")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(labels = dollar_format())

ggsave("model1residuals_point.png")

# 2. Simple Linear Regression #2
# 2a.
ggplot(subdat, aes(OverallQual, SalePrice))+
  geom_point(shape = 21, size = 0.9, color = "#21908CFF")+
  geom_smooth(method = lm, color = "#440154FF")+
  scale_y_continuous(labels = dollar_format())+
  labs(title = "Sale Price by Overall Quality")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("model2.png")

model2 <- lm(SalePrice ~ OverallQual, subdat)
fit2 <- tidy(model2)
fit.stats2 <- glance(model2)

# 2b.
glue("Model: Y = ", round(fit2$estimate[2], 3), "*", fit2$term[2], " + ", round(fit2$estimate[1], 3))

# 2c.
glue("R-Squared: ", round(fit.stats2$r.squared, 3))

# 2d.
summary(model2)
anova(model2)

# 2e.
model2residuals <- augment(model2, data = subdat)

ggplot(model2residuals, aes(x = .std.resid))+
  geom_histogram(binwidth = 0.2, color = "black", fill = "#21908CFF")+
  labs(x = "Standardized Residuals", y = "count", title = "Distribution of Model Residuals")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("model2residuals_hist.png")

ggplot(model2residuals, aes(x = .fitted, y = .std.resid))+
  geom_point(shape = 21, size = 0.9, color = "#21908CFF")+
  labs(x = "Predicted Values", y = "Standardized Residuals", title = "Distribution of Model Residuals by Predicted Value")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(labels = dollar_format())

ggsave("model2residuals_point.png")

# 4. Multiple Linear Regression Models
model3 <- lm(SalePrice ~ TotalFloorSF + OverallQual, subdat)

# 4a.
fit3 <- tidy(model3)
fit.stats3 <- glance(model3)

glue("Model: Y = ", round(fit3$estimate[2], 3), "*", fit3$term[2], " + ", round(fit3$estimate[3], 3), "*", fit3$term[3], " + ", round(fit3$estimate[1], 3))

# 4b.
glue("R-Squared: ", round(fit.stats3$r.squared, 3))
glue("Difference in R-Squared from Model 1: ", round(fit.stats3$r.squared - fit.stats1$r.squared, 3))

# 4c.
summary(model3)
anova(model3)

# 4d.
model3residuals <- augment(model3, data = subdat)

ggplot(model3residuals, aes(x = .std.resid))+
  geom_histogram(binwidth = 0.2, color = "black", fill = "#21908CFF")+
  labs(x = "Standardized Residuals", y = "count", title = "Distribution of Model Residuals")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("model3residuals_hist.png")

ggplot(model3residuals, aes(x = .fitted, y = .std.resid))+
  geom_point(shape = 21, size = 0.9, color = "#21908CFF")+
  labs(x = "Predicted Values", y = "Standardized Residuals", title = "Distribution of Model Residuals by Predicted Value")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(labels = dollar_format())

ggsave("model3residuals_point.png")

ggplot(model3residuals, aes(sample = .std.resid)) +
  geom_qq()+
  geom_qq_line()

ggsave("model3residuals_qq.png")

# 5. Additional Variable
model4 <- lm(SalePrice ~ TotalFloorSF + OverallQual + GarageArea, subdat)

# 5a.
fit4 <- tidy(model4)
fit.stats4 <- glance(model4)

glue("Model: Y = ", round(fit4$estimate[2], 3), "*", fit4$term[2], " + ", round(fit4$estimate[3], 3), "*", fit4$term[3], " + ", round(fit4$estimate[4], 3), "*", fit4$term[4], " + ", round(fit4$estimate[1], 3))

# 5b.
glue("R-Squared: ", round(fit.stats4$r.squared, 3))
glue("Model 3 R-Squared: ", round(fit.stats3$r.squared, 3))
glue("Model 4 R-Squared: ", round(fit.stats4$r.squared, 3))
glue("Difference: ", round(fit.stats4$r.squared - fit.stats3$r.squared, 3))

# 5c.
fit4
anova(model4)

# 5d.
model4residuals <- augment(model4, data = subdat)

ggplot(model4residuals, aes(x = .std.resid))+
  geom_histogram()

ggplot(model4residuals, aes(x = .fitted, y = .std.resid)) +
  geom_point()

ggplot(model4residuals, aes(sample = .std.resid)) +
  geom_qq()+
  geom_qq_line()

# 6. Multiple Linear Regression Models on Transformed Response Variable
model1.log <- lm(logSalePrice ~ TotalFloorSF, subdat)
model3.log <- lm(logSalePrice ~ TotalFloorSF + OverallQual, subdat)
model4.log <- lm(logSalePrice ~ TotalFloorSF + OverallQual + GarageArea, subdat)

model1.log.stats <- glance(model1.log) %>%
  select(r.squared, adj.r.squared)
model3.log.stats <- glance(model3.log) %>%
  select(r.squared, adj.r.squared)
model4.log.stats <- glance(model4.log) %>%
  select(r.squared, adj.r.squared)
log.names <- tribble(~model, "log model 1", "log model 3", "log model 4")

log.model.stats <- bind_rows(model1.log.stats, model3.log.stats, model4.log.stats)
log.model.comparison <- bind_cols(log.names, log.model.stats)
colnames(log.model.comparison) <- c("model", "R-Squared", "Adjusted R-Squared")
formattable(log.model.comparison, align = c("l", "c", "c"),
            list('R-Squared' = percent,
                 'Adjusted R-Squared' = percent))

# 8. Multiple Linear Regression and Influential Points
