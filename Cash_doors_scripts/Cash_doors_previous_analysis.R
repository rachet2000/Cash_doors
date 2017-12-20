#I used this for my honors research thesis. I kind of believe this is useless by now, but
#could be used to reproduce the graphs from the thesis. 

setwd('R')
source('Cashdoors_extraction.R')

second_half <- subset(data, session == "second_time")
data <- subset(data, session == "first_time")

library(ggplot2)
#Cleaning a bit

data$chose_risky <- as.numeric(data$chose_risky)
#Recall of participants in each condition
ggplot(data, aes(risk_first_to_mind)) + 
  geom_bar(width = 0.5) + facet_wrap(~condition) + xlab("First outcome to mind") +
  ylab("# of participants") + theme(text = element_text(size=20)) +
  scale_fill_discrete(guide=FALSE)

ggplot(data, aes(outcome_condition, risk_first_to_mind, fill = outcome_condition)) + 
  stat_summary(fun.y = "mean", geom = "bar", width = 0.5) + facet_wrap(~condition) + 
  xlab("First outcome condition") +
  ylab("% who first recalled the positive outcome") + theme(text = element_text(size=20)) +
  scale_fill_discrete(guide=FALSE)


table <- table(data$risk_first_to_mind, data$condition)
chisq.test(table)

model_recall <- glm(risk_first_to_mind ~ condition, data = data, family = binomial(link ='logit'))

anova(model_recall, test = 'Chisq')
coef(summary(model_recall))[,4]

ggplot(subset(data, risk_first_to_mind == 0 | risk_first_to_mind == 2.5), aes(risk_first_to_mind, total_risk)) +
  stat_summary(fun.y = "mean", geom = "bar", width = 0.5) + stat_summary(fun.data = mean_cl_normal, geom="errorbar", width = 0.2) +
  facet_wrap(~condition) 



data <- subset(data, (risk_first_to_mind == 0 | risk_first_to_mind == 2.5))
data$risk_first_to_mind <- replace(data$risk_first_to_mind, data$risk_first_to_mind == 2.5, 1)
data$risk_first_to_mind <- replace(data$risk_first_to_mind, data$risk_first_to_mind == 0, -1)

data$risk_first_to_mind <- factor(data$risk_first_to_mind, labels = c("Loss", "Win"))
data$outcome_condition <- factor(data$outcome_condition, labels = c("Win", "Loss"))

#Get this
ggplot(data, aes(risk_first_to_mind, total_risk, fill = risk_first_to_mind)) + stat_summary(fun.y = "mean", geom = "bar") +
  facet_wrap(~condition) + xlab("First outcome to mind") + ylab("% of risky choices") +
  scale_fill_discrete(guide=FALSE) + theme(text = element_text(size=15))



#This is required for main grpah of memory condition
m1 <- mean(subset(data, condition == "Episodic")$total_risk, na.rm = T)
m2 <- mean(subset(data, condition == "General")$total_risk, na.rm = T)
plot <- data.frame(total_risk = c(m1,m2), condition = c("Episodic", "General"))
subset_choices <- subset(data, condition == "General")$chose_risky
se <- sd(subset_choices, na.rm=T)/sqrt(length(subset_choices))
limits <- aes(ymax = c(m1,m2) + se, ymin= c(m1,m2) - se)

#Main graph for main effect of memory condition
ggplot(subset(plot), aes(condition, total_risk)) + 
  stat_summary(fun.y = "mean", geom = "bar", width = 0.5, fill = c("blue", "red")) + 
  geom_errorbar(limits, width = 0.1) +
  xlab("Group") + ylab("% of risky choices") +
  theme(text = element_text(size=15))


data$chose_risky <- as.numeric(data$chose_risky)
ggplot(data, aes(block, chose_risky, colour = outcome_condition)) + 
  stat_summary(fun.y = "mean", geom = "point") + geom_smooth(method = 'lm') +
  xlab("Trials (shown in blocks)") + ylab("Proportion of risky choices") +
  ggtitle("Risk preferences in episodic vs general")


#2x2 design plot
levels(data$outcome_condition) <- c("Win", "Loss")

ggplot(subset(data, session == "first_time"), aes(outcome_condition, total_risk, fill = outcome_condition)) + 
  stat_summary(fun.y = "mean", geom = "bar") +
  facet_wrap(~condition) + xlab("First outcome condition") + ylab("% of risky choices") +
  scale_fill_discrete(guide=FALSE) + theme(text = element_text(size =15))
#+ stat_summary(fun.data = mean_cl_normal, geom = "errorbar")

#Yoooooooooooooooo
ggplot(data, aes(risk_first_to_mind, total_risk)) + 
  stat_summary(fun.y = "mean", geom = "bar", fill = c("red", "blue")) +
  xlab("First outcome to mind") + ylab("% of risky choices")

#According to Ross I need to do this (...)

data <- read.csv("data.csv")
previous_data <- data
data <- subset(data, session == "first_time" & participant != 6)
data$condition = replace(as.numeric(data$condition), as.numeric(data$condition) == 2, -1)
data$outcome_condition = replace(as.numeric(data$outcome_condition), as.numeric(data$outcome_condition) == 2, -1)
data$outcome_condition = replace(as.numeric(data$outcome_condition), as.numeric(data$outcome_condition) == 5, 1)
data$trial = (data$trial - min(data$trial))/(max(data$trial) - min(data$trial))
#data$age = (data$age - min(data$age, na.rm = T))/(max(data$age, na.rm = T) - min(data$age, na.rm = T))
data <- subset(data, (risk_first_to_mind == 0 | risk_first_to_mind == 2.5))
data$risk_first_to_mind <- replace(data$risk_first_to_mind, data$risk_first_to_mind == 2.5, 1)
data$risk_first_to_mind <- replace(data$risk_first_to_mind, data$risk_first_to_mind == 0, -1)
data$judged_percent = data$judged_percent/100
#data$gender <- replace(as.numeric(data$gender), as.numeric(data$gender) == 2, -1)

data <- subset(data, trial == 5)

data$chose_risky <- replace(data$chose_risky, data$chose_risky == -1, 0)

library(lme4)
library(lmerTest)
library(car)
model <- glmer(chose_risky ~ trial + condition + outcome_condition + outcome_condition*condition +
                 trial*condition + trial*outcome_condition + risk_first_to_mind + risk_first_to_mind*trial +
                trial*condition*outcome_condition + (1|participant) + (1|trial), data = data, family = binomial)

model <- glmer(chose_risky ~ trial + condition + trial*condition +
                          (1|participant) + (trial|participant), data = data, family = binomial)



model <- glmer(chose_risky ~ trial + risk_first_to_mind + risk_first_to_mind*trial +
                 (1|participant) ,data = data, family = binomial)

model <- glmer(chose_risky ~ trial + outcome_condition +
                 trial*outcome_condition + condition + trial*condition + risk_first_to_mind +
                 (1|participant) ,data = data, family = binomial)

modelx <- glm(risk_first_to_mind ~ outcome_condition + condition + condition*outcome_condition,
              data = data, family = binomial(link = 'logit'))
anova(modelx, test = "Chisq")

coefs <- data.frame(coef(summary(model)))


order <- rep("first_win + Episodic + Restaurant", 6 - nrow(subset(data, trial == 3 & outcome_condition == "first_win" & condition == "Episodic" & video == "Restaurant")))
order <- c(order, rep("first_win + Episodic + Hospital", 6 - nrow(subset(data, trial == 3 & outcome_condition == "first_win" & condition == "Episodic" & video == "Hospital"))))
order <- c(order, rep("first_win + General + Restaurant", 6 - nrow(subset(data, trial == 3 & outcome_condition == "first_win" & condition == "General" & video == "Restaurant"))))
order <- c(order, rep("first_win + General + Hospital", 6 - nrow(subset(data, trial == 3 & outcome_condition == "first_win" & condition == "General" & video == "Hospital"))))
order <- c(order, rep("second_win + Episodic + Restaurant", 6 - nrow(subset(data, trial == 3 & outcome_condition == "second_win" & condition == "Episodic" & video == "Restaurant"))))
order <- c(order, rep("second_win + Episodic + Hospital", 6 - nrow(subset(data, trial == 3 & outcome_condition == "second_win" & condition == "Episodic" & video == "Hospital"))))
order <- c(order, rep("second_win + General + Restaurant", 6 - nrow(subset(data, trial == 3 & outcome_condition == "second_win" & condition == "General" & video == "Restaurant"))))
order <- c(order, rep("second_win + General + Hospital", 6 - nrow(subset(data, trial == 3 & outcome_condition == "second_win" & condition == "General" & video == "Hospital"))))




