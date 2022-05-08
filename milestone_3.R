library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lme4)

library(dplyr)
library(tidyverse)
library(ggplot2)
library(zoo)
library(caret)
library(leaps)
library(MASS)
library(car)

original_data <- read.csv("C:/Users/NILESH/Documents/University/Spring 2022/Statistical_Methods_In_Research/project/milestone_3/data/KeyData.csv")

model_2 <- original_data

model_2 <- na.omit(model_2)

model_2$SR <- ifelse(model_2$SR >= 4, 1, 0)

model_2$Rank <- as.factor(model_2$Rank)
model_2$RS <- as.factor(model_2$RS)
model_2$WH <- as.factor(model_2$WH)
model_2$BF <- as.factor(model_2$BF)
model_2$NP <- as.factor(model_2$NP)
model_2$FA <- as.factor(model_2$FA)
model_2$AP <- as.factor(model_2$AP)
model_2$AR <- as.factor(model_2$AR)
model_2$DWH <- as.factor(model_2$DWH)
model_2$T <- as.factor(model_2$T)
model_2$DS <- as.factor(model_2$DS)
model_2$FC <- as.factor(model_2$FC)
model_2$SR <- as.factor(model_2$SR)


model_result <- glm(SR ~ .,  data = model_2, family = "binomial")
summary(model_result)
back.model <- stepAIC(model_result, direction = "backward", trace = TRUE)
summary(back.model)

model_result_2 <- glm(SR ~ NASA + TA + H + NP + FA + DWH + T + FC,  data = model_2, family = "binomial")
summary(model_result_2)
back.model_2 <- stepAIC(model_result_2, direction = "backward", trace = TRUE)
summary(back.model_2)


forward.model <- stepAIC(model_result, direction = "forward", trace = TRUE)
summary(forward.model)

