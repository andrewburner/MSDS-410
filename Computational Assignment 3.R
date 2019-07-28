library(data.table)
library(tidyverse)
library(broom)
library(glue)
library(skimr)
library(recipes)
library(gridExtra)
library(ggpubr)

mydata <- fread("NutritionStudy.csv")

summary(mydata)
skim(mydata)
str(mydata)

mydata$VitaminCode <- ifelse(mydata$VitaminUse == "No", 3, ifelse(mydata$VitaminUse == "Occasional", 2, 1))


# 2
# Model 1
model1 <- lm(Cholesterol ~ VitaminCode, data = mydata)
summary(model1)
fit1 <- broom::tidy(model1)
fit.stats1 <- glance(model1)
model1residuals <- augment(model1, data = mydata)
model1residuals <- model1residuals %>%
  mutate(DFFITS = dffits(model1))
threshold <- 2*sqrt((1+1))/(313-1-1)

glue("Model: Y = ", round(fit1$estimate[2], 3), "*", fit1$term[2], " + ", round(fit1$estimate[1], 3))
glue("R-Squared: ", round(fit.stats1$r.squared, 3))

ggplot(model1residuals, aes(x = .std.resid, y = .fitted)) +
  geom_point()+
  geom_smooth(method = loess)
model1_resid <- ggplot(model1residuals, aes(x = .std.resid)) +
  geom_histogram(color = "black", fill = "#21908CFF")
ggsave("model1resid_hist.png")

model1_dffits <- ggplot(model1residuals) +
  geom_point(aes(x = 1:nrow(model1residuals), y = DFFITS), color = "#21908CFF")+
  labs(x = "observation number")+
  geom_hline(yintercept = threshold, linetype = "dashed", color = "#440154FF") +
  geom_hline(yintercept = -threshold, linetype = "dashed", color = "#440154FF")
ggsave("model1_dffits.png")
grid.arrange(model1_resid, model1_dffits)


# Model 2
mydata$VitaminCodeNew <- ifelse(mydata$VitaminUse == "No", 1, ifelse(mydata$VitaminUse == "Occasional", 2, 3))

model2 <- lm(Cholesterol ~ VitaminCodeNew, data = mydata)
summary(model2)
fit2 <- broom::tidy(model2)
fit.stats2 <- glance(model2)
model2residuals <- augment(model2, data = mydata)
model2residuals <- model2residuals %>%
  mutate(DFFITS = dffits(model2))
threshold <- 2*sqrt((1+1))/(313-1-1)

glue("Model: Y = ", round(fit2$estimate[2], 3), "*", fit2$term[2], " + ", round(fit2$estimate[1], 3))
glue("R-Squared: ", round(fit.stats2$r.squared, 3))

ggplot(model2residuals, aes(x = .std.resid, y = .fitted)) +
  geom_point()+
  geom_smooth(method = loess)
model2_resid <- ggplot(model2residuals, aes(x = .std.resid)) +
  geom_histogram(color = "black", fill = "#21908CFF")
ggsave("model2resid_hist.png")

model2_dffits <- ggplot(model2residuals) +
  geom_point(aes(x = 1:nrow(model2residuals), y = DFFITS), color = "#21908CFF")+
  labs(x = "observation number")+
  geom_hline(yintercept = threshold, linetype = "dashed", color = "#440154FF") +
  geom_hline(yintercept = -threshold, linetype = "dashed", color = "#440154FF")
ggsave("model2_dffits.png")
grid.arrange(model2_resid, model2_dffits)
# 3
mydata_dummy <- mydata %>%
                  mutate(x_regular = ifelse(VitaminUse == "Regular", 1, 0)) %>%
                  mutate(x_occasional = ifelse(VitaminUse == "Occasional", 1, 0)) %>%
                  mutate(x_never = ifelse(VitaminUse == "No", 1, 0))

# Model 3
model3 <- lm(Cholesterol ~ x_occasional + x_regular, data = mydata_dummy)
summary(model3)
fit3 <- broom::tidy(model3)
fit.stats3 <- glance(model3)
model3residuals <- augment(model3, data = mydata)
model3residuals <- model3residuals %>%
  mutate(DFFITS = dffits(model3))
threshold <- 2*sqrt((2+1))/(313-2-1)

glue("Model: Y = ", round(fit3$estimate[2], 3), "*", fit3$term[2], " + ", round(fit3$estimate[3], 3), "*", fit3$term[3], " + ", round(fit3$estimate[1], 3))
glue("R-Squared: ", round(fit.stats3$r.squared, 3))

ggplot(model3residuals, aes(x = .std.resid, y = .fitted)) +
  geom_point()+
  geom_smooth(method = loess)
model3_resid <- ggplot(model3residuals, aes(x = .std.resid)) +
  geom_histogram(color = "black", fill = "#21908CFF")
ggsave("model3resid_hist.png")

model3_dffits <- ggplot(model3residuals) +
  geom_point(aes(x = 1:nrow(model3residuals), y = DFFITS), color = "#21908CFF")+
  labs(x = "observation number")+
  geom_hline(yintercept = threshold, linetype = "dashed", color = "#440154FF") +
  geom_hline(yintercept = -threshold, linetype = "dashed", color = "#440154FF")
grid.arrange(model3_resid, model3_dffits)

# 4
mydata_dummy <- mydata %>%
  mutate(effect_regular = ifelse(VitaminUse == "Regular", 1, ifelse(VitaminUse == "Occasional", 0, -1))) %>%
  mutate(effect_occasional = ifelse(VitaminUse == "Occasional", 1, ifelse(VitaminUse == "Regular", 0, -1)))

model4 <- lm(Cholesterol ~ effect_regular + effect_occasional, mydata_dummy)
summary(model4)
fit4 <- broom::tidy(model4)
fit.stats4 <- glance(model4)
model4residuals <- augment(model4, data = mydata)
model4residuals <- model4residuals %>%
  mutate(DFFITS = dffits(model4))
# threshold <- 2*sqrt((2+1))/(313-2-1)

glue("Model: Y = ", round(fit4$estimate[2], 3), "*", fit4$term[2], " + ", round(fit4$estimate[3], 3), "*", fit4$term[3], " + ", round(fit4$estimate[1], 3))
glue("R-Squared: ", round(fit.stats4$r.squared, 3))

ggplot(model4residuals, aes(x = .std.resid, y = .fitted)) +
  geom_point()+
  geom_smooth(method = loess)
model4_resid <- ggplot(model4residuals, aes(x = .std.resid)) +
  geom_histogram(color = "black", fill = "#21908CFF")
ggsave("model4resid_hist.png")

model4_dffits <- ggplot(model4residuals) +
  geom_point(aes(x = 1:nrow(model4residuals), y = DFFITS), color = "#21908CFF")+
  labs(x = "observation number")+
  geom_hline(yintercept = threshold, linetype = "dashed", color = "#440154FF") +
  geom_hline(yintercept = -threshold, linetype = "dashed", color = "#440154FF")
grid.arrange(model4_resid, model4_dffits)

# 5
mydata_dummy <- mydata_dummy %>%
  mutate(alcohol_indicator = ifelse(Alcohol == 0, 0, ifelse(Alcohol >= 10, 2, 1)))
mydata_dummy <- mydata_dummy %>%
  mutate(alcohol_medium = ifelse(alcohol_indicator == 1, 1, ifelse(alcohol_indicator == 2, 0, -1))) %>%
  mutate(alcohol_high = ifelse(alcohol_indicator == 2, 1, ifelse(alcohol_indicator == 1, 0, -1)))

mydata_dummy <- mydata_dummy %>%
  mutate(reg_high = alcohol_high * effect_regular) %>%
  mutate(reg_med = alcohol_medium * effect_regular) %>%
  mutate(occ_high = alcohol_high * effect_occasional) %>%
  mutate(occ_med = alcohol_medium * effect_occasional) %>%
  mutate(AlcoholUse = ifelse(alcohol_high == 1, "High", ifelse(alcohol_medium == 1, "Medium", "None")))

full_model <- lm(Cholesterol ~ effect_regular + effect_occasional +
                   alcohol_high + alcohol_medium + reg_high + reg_med +
                   occ_high + occ_med, data = mydata_dummy)
reduced_model <- lm(Cholesterol ~ effect_regular + effect_occasional +
                    alcohol_high + alcohol_medium, data = mydata_dummy)
anova(reduced_model, full_model)



ggplot(mydata_dummy) +
  geom_boxplot(aes(x = VitaminUse, y = Cholesterol, fill = AlcoholUse))

ggline(mydata_dummy, x = "VitaminUse", y = "Cholesterol", color = "AlcoholUse",
       add = c("mean_se"))

# 7
# Create effect codes for gender and smoke then create 
# interactions with vitamin and alcohol then fit models
mydata_dummy$GenderCode <- as.factor(mydata_dummy$Gender)
levels(mydata_dummy$GenderCode) = c(0, 1)
mydata_dummy$SmokeCode <- as.factor(mydata_dummy$Smoke)
levels(mydata_dummy$SmokeCode) = c(0, 1)

ggplot(mydata_dummy) +
  geom_boxplot(aes(x = GenderCode, y = Cholesterol, fill = SmokeCode))
ggsave("smoke_gender.png")

ggplot(mydata_dummy) +
  geom_boxplot(aes(x = VitaminUse, y = Cholesterol, fill = SmokeCode))
ggsave("smoke_vitamin.png")

ggplot(mydata_dummy) +
  geom_boxplot(aes(x = AlcoholUse, y = Cholesterol, fill = SmokeCode))
ggsave("smoke_alcohol.png")

ggplot(mydata_dummy) +
  geom_boxplot(aes(x = VitaminUse, y = Cholesterol, fill = GenderCode))
ggsave("gender_vitamin.png")

ggplot(mydata_dummy) +
  geom_boxplot(aes(x = AlcoholUse, y = Cholesterol, fill = GenderCode))
ggsave("gender_alcohol.png")

mydata_dummy <- mydata_dummy %>%
  mutate()

gender_smoke_full <- lm(Cholesterol ~ GenderCode + SmokeCode + GenderCode*SmokeCode, data = mydata_dummy)
gender_smoke_reduced <- lm(Cholesterol ~ GenderCode + SmokeCode, data = mydata_dummy)
anova(gender_smoke_full, gender_smoke_reduced)
