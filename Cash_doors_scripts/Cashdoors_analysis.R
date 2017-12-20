#setwd('Cash_doors')
#source('Cash_doors_scripts/Cashdoors_analysis.R')

source('Cash_doors_scripts/Cashdoors_extraction_win.R')
source('Cash_doors_scripts/Cashdoors_extraction_control.R')
#source('Cash_doors_scripts/Cashdoors_extraction_loss.R')
library(gtools)
library(ggplot2)
library(plyr)
library(Hmisc)
#library(agricolae)

data <- smartbind(data_win, data_control)
#data <- smartbind(data, data_loss)
data$chose_risky <- as.numeric(data$chose_risky)
data$rt <- as.numeric(data$rt)
data$participant <- factor(data$participant)
data$trial2 <- data$trial
data$trial <- data$trial/100

data$door_choice <- NA
data[data$key_pressed == 'right' & !is.na(data$key_pressed), 'door_choice'] <- 1
data[data$key_pressed == 'left' & !is.na(data$key_pressed), 'door_choice'] <- 0

data$condition <- factor(data$condition, labels = c('Episodic', 'Control', 'Baseline'))

#Input here the list of participants to be excluded
#data <- subset(data, participant != 'cd6' & participant != 'cd36' &
#                 participant != 'cd38' & participant != 'ct05' & participant != 'cd40' & participant != 'cd11' &
#                 participant != 'cd5' & participant != 'lo09x')

total_risk_per_participant <- ddply(data, c('participant'), function(subject){
  
  risk_left_door <- subset(subject, key_pressed == 'left')$chose_risky[1]
  
  #Here we calculate total_risk based on block 3 to 5
  return(data.frame(total_risk = mean(subset(subject, trial > (max(subject$trial)*0.401))$chose_risky, na.rm = T), condition = subject$condition[1],
                    outcome_condition = subject$outcome_condition[1], 
                    risk_first_to_mind = subject$risk_first_to_mind[1],
                    experiment = subject$experiment[1],
                    is_left_risk = risk_left_door,
                    door_choice = mean(subject$door_choice, na.rm = T),
                    total_judged = subject$total_judged[1],
                    slope = glm(chose_risky ~ trial, family = binomial, data = subject)$coefficients[2],
                    total_risk_all = mean(subject$total_risk),
                    total_risk_first10 = mean(subset(subject, trial < (max(subject$trial)/10))$chose_risky, na.rm = T),
                    total_risk_first20 = mean(subset(subject, trial < (max(subject$trial)/5))$chose_risky, na.rm = T),
                    total_risk_first30 = mean(subset(subject, trial <= (max(subject$trial)*0.3))$chose_risky, na.rm = T),
                    total_risk_first40 = mean(subset(subject, trial < (max(subject$trial)/2.5))$chose_risky, na.rm = T)
                    
                    ))})
total_risk_per_participant$risk_first_to_mind <- total_risk_per_participant$risk_first_to_mind/2.5



recall <- data.frame(Episodic = c(15,6), General = c(11,12))

makegraphs <- function(){
pdf('cashdoors_graphs.pdf')

#Barplot of amount of risk per participant in each condition
plot(ggplot(total_risk_per_participant, aes(condition, total_risk)) + 
  stat_summary(fun.y = 'mean', geom = 'bar') +
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(width = 0.2)) + theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#Boxplot of total amount of risk per participant in each condition
plot(ggplot(total_risk_per_participant, aes(condition, total_risk)) + geom_boxplot())

#Risk preferences over time (binned 5 trials at a time)
n_break <- 20
data$risk_break_trials <- cut(data$trial, breaks = seq(from = 0, to = 100, by = n_break))
plot(ggplot(data, aes(risk_break_trials, chose_risky, color = condition, group = condition)) + stat_summary(fun.y ='mean', geom= 'line') +
  scale_x_discrete(labels = seq(from = n_break, to = 100, by = n_break)) + xlab('Trial') + 
  ylab('p(chose_risky)') + stat_summary(fun.y = 'mean', geom = 'point') + theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#Risk preferences over time (binned 5 trials at a time) WITH OUTCOME_CONDITION
n_break <- 10
data$risk_break_trials <- cut(data$trial, breaks = seq(from = 0, to = 100, by = n_break))
plot(ggplot(data, aes(risk_break_trials, chose_risky, color = condition, group = condition)) + stat_summary(fun.y ='mean', geom= 'line') +
  scale_x_discrete(labels = seq(from = n_break, to = 100, by = n_break)) + xlab('Trial') + 
  ylab('p(chose_risky)') + stat_summary(fun.y = 'mean', geom = 'point') + facet_wrap(~outcome_condition))

#Risk preference if last gamble is a win or a loss
data$last_risk <- factor(data$last_risk)
plot(ggplot(data[!is.na(data$last_risk),], aes(last_risk, chose_risky, color = condition, group = condition)) + stat_summary(fun.y = 'mean', geom = 'point') +
  stat_summary(fun.y = 'mean', geom = 'line') + xlab('Reward last time risk door was chosen') +
  ylab('p(chose_risky)') + scale_x_discrete(labels = c('Bad', 'Good')))

#Reaction time in different conditions
plot(ggplot(data, aes(condition, rt, fill = condition)) + stat_summary(fun.y = 'mean', geom = 'bar') +
  ylab('Reaction time (ms)'))

#Reaction time vs trial in different conditions
n_break <- 10
data$risk_break_trials <- cut(data$trial, breaks = seq(from = 0, to = 100, by = n_break))
plot(ggplot(data, aes(risk_break_trials, rt, color = condition, group = condition)) + 
  stat_summary(fun.y = 'mean', geom = 'point') + stat_summary(fun.y = 'mean', geom = 'line') +
  ylab('Reaction time (ms)') + xlab('Trial') + scale_x_discrete(labels = seq(from = n_break, to = 100, by = n_break)))

#Reaction time and risk preferences in different conditions
n_break <- 100
data$risk_break_rt <- cut(data$rt, breaks = seq(from = 300, to = 1000, by = n_break))
plot(ggplot(data, aes(risk_break_rt, chose_risky, color = condition, group = condition)) + 
  stat_summary(fun.y = 'mean', geom = 'point') + stat_summary(fun.y = 'mean', geom = 'line') +
  ylab('p(chose_risky)') + scale_x_discrete(labels = seq(from = 300, to = 1000, by = n_break)) +
  xlab("Reaction time (ms)"))

#First outcome to come to mind in different conditions
plot(ggplot(total_risk_per_participant, aes(condition, risk_first_to_mind*40, fill = condition)) +
  stat_summary(fun.y = 'mean', geom = 'bar') +ylab('p(good outcome is reported)'))

#First outcome to come to mind in different memory conditions and outcome conditions
plot(ggplot(total_risk_per_participant, aes(condition, risk_first_to_mind*40, fill = condition)) +
  stat_summary(fun.y = 'mean', geom = 'bar') +ylab('p(good outcome is reported)') +
  facet_grid(~outcome_condition))

#Risk preferences as a function of outcome reported
total_risk_per_participant$risk_first_to_mind_factor <- factor(total_risk_per_participant$risk_first_to_mind, 
                                                               labels = c('bad', 'good'), levels = c(0, 2.5))
plot(ggplot(total_risk_per_participant, aes(risk_first_to_mind_factor, total_risk, fill = risk_first_to_mind)) +
  stat_summary(fun.y = 'mean', geom = 'bar') + facet_grid(~condition) + 
  xlab('First outcome that comes to mind') + ylab('p(chose_risky)'))

dev.off()


pdf('Graphs/individual_data.pdf')
for(id in unique(data$participant)){
  subject <- data[data$participant == id,]
  n_break_1 <- 5
  print(id)
  subject$risk_break_trials_1 <- cut(subject$trial, breaks = seq(from = 0, to = 100, by = n_break_1))
plot(ggplot(subject, aes(risk_break_trials_1, chose_risky)) + 
  stat_summary(fun.y = 'mean', geom = 'point') + ylab('p(chose_risky)') + xlab('trial') +
  scale_x_discrete(labels = seq(from = n_break_1, to = 100, by = n_break_1)) + 
  ggtitle(paste(id, 'in', unique(subject$condition), unique(subject$outcome_condition))) +
    stat_summary(fun.y = 'mean', geom = 'line', group = 1) + ylim(c(0,1)))
  
}
dev.off()
}


