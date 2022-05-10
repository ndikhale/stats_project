library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lme4)
library(zoo)
library(caret)
library(leaps)
library(MASS)
library(car)
library(jtools)

original_data <- read.csv("C:/Users/NILESH/Documents/University/Spring 2022/Statistical_Methods_In_Research/project/milestone_3/data/KeyData.csv")

model_2 <- original_data

model_2 <- na.omit(model_2)

model_2$SR <- ifelse(model_2$SR >= 4, 1, 0)

model_2 <- model_2[, !names(model_2) %in% c("FC", "RS", "T", "DWH")]

model_2$WH <- as.factor(model_2$WH)
model_2$BF <- as.factor(model_2$BF)
model_2$NP <- as.factor(model_2$NP)
model_2$FA <- as.factor(model_2$FA)
#model_2$T <- as.factor(model_2$T)
#model_2$RS <- as.factor(model_2$RS)
model_2$DS <- as.factor(model_2$DS)



#model_2$FC <- as.factor(model_2$FC)



# forward model

model_result <- glm(SR ~ .,  data = model_2, family = "binomial")
summary(model_result)
back.model <- stepAIC(model_result, direction = "backward", trace = TRUE)
summary(back.model)

forward_model_result <- glm(SR ~ 1, data = model_2, family = "binomial")
summary(forward_model_result)

forward.model_2 <- stepAIC(forward_model_result, direction = "forward", trace = TRUE, scope = formula(model_result))
summary(forward.model_2)


both_model_result <- stepAIC(forward_model_result, direction="both", trace=TRUE, scope = formula(model_result))
summary(both_model_result)

new_model <- glm(SR ~ TA + H + NP + FA + DS, data = model_2, family = "binomial")
summary(new_model)

model_2 <- model_2[, !names(model_2) %in% c("RS")]
model_2 <- model_2[, !names(model_2) %in% c("DWH")]
model_2 <- model_2[, !names(model_2) %in% c("T")]
model_2 <- model_2[, !names(model_2) %in% c("DS")]

# diagram 4 / model 4 / $75

model_data_4 <- original_data

model_data_4$FC <- ifelse(model_data_4$FC >=5, 1, 0)

model_data_4$Rank <- as.factor(model_data_4$Rank)
model_data_4$RS <- as.factor(model_data_4$RS)
model_data_4$WH <- as.factor(model_data_4$WH)
model_data_4$BF <- as.factor(model_data_4$BF)
model_data_4$NP <- as.factor(model_data_4$NP)
model_data_4$FA <- as.factor(model_data_4$FA)
model_data_4$AP <- as.factor(model_data_4$AP)
model_data_4$AR <- as.factor(model_data_4$AR)
model_data_4$DWH <- as.factor(model_data_4$DWH)
model_data_4$T <- as.factor(model_data_4$T)
model_data_4$DS <- as.factor(model_data_4$DS)
model_data_4$SR <- as.factor(model_data_4$SR)

model_result_4 <- glm(FC ~ ., data = model_data_4, family = "binomial")
summary(model_result_4)

back.model <- stepAIC(model_result_4, direction = "backward", trace = TRUE)
summary(back.model)

model_4 <- glm(FC ~ FA + T + TWR + H + RS + O, data= model_data_4, family = "binomial")
summary(model_4)

# fa_val = factor(model_data_4$FA, levels = c(1, 2, 3, 4, 5, 6), 
#                 labels = c("NSF", "NIH", "DOE", "DOD", "NASA", "OTHER"))

color = c("gray", "black", "cyan", "orange", "black", "black")
plot_4_1 <- effect_plot(model_4, pred = FA, interval = TRUE, y.label = "S$75", colors = color) + scale_x_discrete(labels= c("NSF", "NIH", "DOE", "DOD", "NASA", "OT")) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"$75")) +
  ggtitle("Funding Agency")

color = c("gray", "cyan")
plot_4_2 <- effect_plot(model_4, pred = T, interval = TRUE, y.label = "S$75", colors = color) + scale_x_discrete(labels= c("TS1", "TS2")) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"$75")) +
  ggtitle("Time of Submission")

color = c("orange")
plot_4_3 <- effect_plot(model_4, pred = TWR, interval = TRUE, y.label = "S$75", colors = color) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  geom_vline(xintercept=mean(model_data_4$TWR), linetype="dashed", size = 1, color = "gray") +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"$75")) +
  ggtitle("Typical Week Research")
  
color = c("cyan")
plot_4_4 <- effect_plot(model_4, pred = H, interval = TRUE, y.label = "S$75", colors = color) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  geom_vline(xintercept=mean(model_data_4$H), linetype="dashed", size = 1, color = "gray") +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"$75")) +
  ggtitle("h-index")

color = c("gray", "cyan")
plot_4_5 <- effect_plot(model_4, pred = RS, interval = TRUE, y.label = "S$75", colors = color) + scale_x_discrete(labels= c("RS1", "RS2")) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"$75")) +
  ggtitle("Research Style")

color = c("cyan")
plot_4_6 <- effect_plot(model_4, pred = O, interval = TRUE, y.label = "S$75", colors = color) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  geom_vline(xintercept=mean(model_data_4$O), linetype="dashed", size = 1, color = "gray") +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"$75")) +
  ggtitle("Openness")

# diagram 4 / model 5 / $$

model_data <- original_data
model_data$FC <- ifelse(model_data$FC == 6, 1, 0)

model_data <- model_data[, !names(model_data) %in% c("SR")]

model_data$Rank <- as.factor(model_data$Rank)
model_data$RS <- as.factor(model_data$RS)
model_data$WH <- as.factor(model_data$WH)
model_data$BF <- as.factor(model_data$BF)
model_data$NP <- as.factor(model_data$NP)
model_data$FA <- as.factor(model_data$FA)
model_data$AP <- as.factor(model_data$AP)
model_data$AR <- as.factor(model_data$AR)
model_data$DWH <- as.factor(model_data$DWH)
model_data$T <- as.factor(model_data$T)
model_data$DS <- as.factor(model_data$DS)
model_data$SR <- as.factor(model_data$SR)

model_result_4 <- glm(FC ~ ., data = model_data, family = "binomial")
summary(model_result_4)

back.model <- stepAIC(model_result_4, direction = "backward", trace = TRUE)
summary(back.model)

model_4 <- glm(FC ~ FA + O +TWR, data= model_data, family = "binomial")
summary(model_4)

color = c("gray", "black", "black", "red", "black", "black")
plot_5_1 <- effect_plot(model_4, pred = FA, interval = TRUE, colors = color) + scale_x_discrete(labels= c("NSF", "NIH", "DOE", "DOD", "NASA", "OT")) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"$$")) +
  ggtitle("Funding Agency")

plot_5_2 <- ggplot() + theme_classic() +theme(panel.border = element_rect(color = "black", fill = NA, size = 1), plot.title = element_text(hjust = 0.5), axis.text.y = element_blank()) + 
  xlab("") + ylab(expression("")) +
  ggtitle("Plot research")

color = c("red")
plot_5_3 <- effect_plot(model_4, pred = TWR, interval = TRUE, colors = color) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  geom_vline(xintercept=mean(model_data$TWR), linetype="dashed", size = 1, color = "gray") +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"$$")) +
  ggtitle("Typical Week Research")

plot_5_4 <- ggplot() + theme_classic() +theme(panel.border = element_rect(color = "black", fill = NA, size = 1), plot.title = element_text(hjust = 0.5), axis.text.y = element_blank()) + 
  xlab("") + ylab(expression("")) +
  ggtitle("Plot research")

plot_5_5 <- ggplot() + theme_classic() +theme(panel.border = element_rect(color = "black", fill = NA, size = 1), plot.title = element_text(hjust = 0.5), axis.text.y = element_blank()) + 
  xlab("") + ylab(expression("")) +
  ggtitle("Plot research")

color = c("cyan")
plot_5_6 <- effect_plot(model_4, pred = O, interval = TRUE, colors = color) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  geom_vline(xintercept=mean(model_data$O), linetype="dashed", size = 1, color = "gray") +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"$$")) +
  ggtitle("Openness")


figure_3_1 <- ggarrange(plot_4_1, plot_4_3, plot_4_3, plot_5_1, plot_5_2, plot_5_3, nrow = 2, ncol = 3)

figure_3_2 <- ggarrange(plot_4_4, plot_4_5, plot_4_6, plot_5_4, plot_5_5, plot_5_6, nrow = 2, ncol = 3)

figure_final_4 <- ggarrange(plot_6_1,plot_6_2,plot_6_3,plot_6_4,
                            nrow = 1, ncol = 4)


# diagram 5 / model 6

model_data_6 <- original_data

model_data_6$condition_col <- ifelse((model_data_6$SR >= 5 & model_data_6$FC == 6), 1, 0)

model_data_6 <- model_data_6[, !names(model_data_6) %in% c("FC", "SR")]

model_data_6$Rank <- as.factor(model_data_6$Rank)
model_data_6$RS <- as.factor(model_data_6$RS)
model_data_6$WH <- as.factor(model_data_6$WH)
model_data_6$BF <- as.factor(model_data_6$BF)
model_data_6$NP <- as.factor(model_data_6$NP)
model_data_6$FA <- as.factor(model_data_6$FA)
model_data_6$AP <- as.factor(model_data_6$AP)
model_data_6$AR <- as.factor(model_data_6$AR)
model_data_6$DWH <- as.factor(model_data_6$DWH)
model_data_6$T <- as.factor(model_data_6$T)
model_data_6$DS <- as.factor(model_data_6$DS)
model_data_6$condition_col <- as.factor(model_data_6$condition_col)

model_result_6 <- glm(condition_col ~ ., data = model_data_6, family = "binomial")
summary(model_result_6)

back.model_6 <- stepAIC(model_result_6, direction = "backward", trace = TRUE)
summary(back.model_6)

forward.model_6 <- glm(condition_col ~ 1, data = model_data_6, family = "binomial")
summary(forward.model_6)

forward.model_6_2 <- stepAIC(forward.model_6, direction = "forward", trace = TRUE, scope = formula(model_result_6))
summary(forward.model_6_2)

both.model_6 <- stepAIC(forward.model_6, direction="both", trace=TRUE, scope = formula(model_result_6))
summary(both.model_6)

# removing variables RS2 and O

model_6_2 <- glm(condition_col ~ NP + TWR + E + A, data= model_data_6, family = "binomial")
summary(model_6_2)

back.model_6_2 <- stepAIC(model_6_2, direction = "backward", trace = TRUE)
summary(back.model_6_2)

forward.model_6_2 <- glm(condition_col ~ 1, data = model_data_6, family = "binomial")
summary(forward.model_6_2)

forward.model_6_2 <- stepAIC(forward.model_6_2, direction = "forward", trace = TRUE, scope = formula(model_6_2))
summary(forward.model_6_2)

both.model_6 <- stepAIC(forward.model_6_2, direction="both", trace=TRUE, scope = formula(model_6_2))
summary(both.model_6)

# all 3 selection forward, backward and both have same AIC value

# final model

final_model_6 <- glm(condition_col ~ NP + TWR + E + A, family = "binomial", data = model_data_6)
summary(final_model_6)

color = c("gray", "orange", "orange")
plot_6_1 <- effect_plot(final_model_6, pred = NP, interval = TRUE, colors = color) + scale_x_discrete(labels= c("NP1", "NP2", "NP3")) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"S50$$")) +
  ggtitle("Number of Proposals")

color = c("orange")
plot_6_2 <- effect_plot(final_model_6, pred = TWR, interval = TRUE, colors = color) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  geom_vline(xintercept=mean(model_data_6$TWR), linetype="dashed", size = 1, color = "gray") +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"S50$$")) +
  ggtitle("Typical Week Research")

color = c("cyan")
plot_6_3 <- effect_plot(final_model_6, pred = E, interval = TRUE, colors = color) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  geom_vline(xintercept=mean(model_data_6$E), linetype="dashed", size = 1, color = "gray") +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"S50$$")) +
  ggtitle("Extraversion")

color = c("cyan")
plot_6_4 <- effect_plot(final_model_6, pred = A, interval = TRUE, colors = color) +
  scale_y_continuous(breaks = seq(0, 1, .25), labels = scales::percent, limits = c(0,1)) +
  geom_vline(xintercept=mean(model_data_6$A), linetype="dashed", size = 1, color = "gray") +
  theme_classic() + theme(legend.position="none") + scale_color_manual(values=  color) + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold")) + 
  xlab("") + ylab(expression("S"^"S50$$")) +
  ggtitle("Aggreeableness")

figure_final_4 <- ggarrange(plot_6_1,plot_6_2,plot_6_3,plot_6_4,
                            nrow = 1, ncol = 4)