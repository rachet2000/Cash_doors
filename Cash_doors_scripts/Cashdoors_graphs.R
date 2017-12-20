source('Cash_doors_scripts/Cashdoors_analysis.R')


pdf('Graphs/Paper_graphs_attempt1.pdf')
maxy <- 0.6
#Figure 2
a <- ggplot(total_risk_per_participant, aes(condition, total_risk)) + 
       stat_summary(fun.y = 'mean', geom = 'bar') +
       stat_summary(fun.data = mean_se, geom = "errorbar", aes(width = 0.2)) + theme_bw() +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_cartesian(ylim=c(0.2,maxy))+
       xlab('Type of memory induction') + ylab('Probability of choosing the risky option')+
       ggtitle('Risk-taking behavior')


#Mean for first_win
plot(ggplot(subset(total_risk_per_participant, outcome_condition == 'first_win'), aes(condition, total_risk)) + 
       stat_summary(fun.y = 'mean', geom = 'bar') +
       stat_summary(fun.data = mean_se, geom = "errorbar", aes(width = 0.2)) + theme_bw() +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       ggtitle('Risk-taking in the first-win condition')+ ylab('Probability of choosing the risky option') +
       xlab('Type of memory induction') +coord_cartesian(ylim=c(0,maxy)))

#Mean for second_win
plot(ggplot(subset(total_risk_per_participant, outcome_condition == 'second_win'), aes(condition, total_risk)) + 
       stat_summary(fun.y = 'mean', geom = 'bar') +
       stat_summary(fun.data = mean_se, geom = "errorbar", aes(width = 0.2)) + theme_bw() +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       ggtitle('Risk-taking in the second-win condition') + ylab('Probability of choosing the risky option')+
       xlab('Type of memory induction') + coord_cartesian(ylim=c(0,maxy)))



#Risk preferences over time. Question about numbers on the x axis. 
n_break <- 20
data$risk_break_trials <- cut(data$trial, breaks = seq(from = 0, to = 100, by = n_break))

b <- ggplot(data, aes(risk_break_trials, chose_risky, shape = condition, group = condition, linetype = condition)) + stat_summary(fun.y ='mean', geom= 'line') +
       scale_x_discrete(labels = seq(from = n_break, to = 100, by = n_break)) + xlab('Trial') + 
       ylab('p(chose_risky)') + stat_summary(fun.y = 'mean', geom = 'point', size = 3) + theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       ylab('Probability of choosing the risky option') + 
       stat_summary(fun.data = mean_se, geom = 'errorbar', aes(width = 0.15)) +
       coord_cartesian(ylim=c(0.2,maxy)) + ggtitle('Risk-taking over time')

#First win condition
plot(ggplot(subset(data, outcome_condition == 'first_win'), aes(risk_break_trials, chose_risky, shape = condition, group = condition)) + stat_summary(fun.y ='mean', geom= 'line') +
       scale_x_discrete(labels = seq(from = n_break, to = 100, by = n_break)) + xlab('Trial') + 
       stat_summary(fun.y = 'mean', geom = 'point') + theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       ylab('Probability of choosing the risky option') + stat_summary(fun.data = mean_se, geom = 'errorbar', aes(width = 0.15))+
       ggtitle('Risk-taking in the first win condition') + coord_cartesian(ylim=c(0,maxy)))

#Second win condition
plot(ggplot(subset(data, outcome_condition == 'second_win'), aes(risk_break_trials, chose_risky, shape = condition, group = condition)) + stat_summary(fun.y ='mean', geom= 'line') +
       scale_x_discrete(labels = seq(from = n_break, to = 100, by = n_break)) + xlab('Trial') + 
       stat_summary(fun.y = 'mean', geom = 'point') + theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       ylab('Probability of choosing the risky option') + stat_summary(fun.data = mean_se, geom = 'errorbar', aes(width = 0.15))+
       ggtitle('Risk-taking in the second win condition') + coord_cartesian(ylim=c(0,maxy)))

maxy2 <- 1

#First outcome to come to mind in different conditions
c <- ggplot(total_risk_per_participant, aes(condition, risk_first_to_mind)) +
       stat_summary(fun.y = 'mean', geom = 'bar') +
       xlab('Condition') + ylab('Proportion of participants reporting extreme positive outcome with risky action') +
       stat_summary(fun.data = mean_se, geom = 'errorbar', aes(width = (0.2))) + theme_bw() +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + coord_cartesian(ylim=c(0,maxy2)) +
       scale_x_discrete(labels = c('Episodic', 'Control', 'Baseline')) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(), panel.border = element_blank())

#First outcome to come to mind for first-win
plot(ggplot(subset(total_risk_per_participant, outcome_condition == 'first_win'),  aes(condition, risk_first_to_mind*0.4)) +
       stat_summary(fun.y = 'mean', geom = 'bar') +
       xlab('Type of memory induction') + ylab('Proportion (%)') +
       ggtitle('Recall for participants who received the first-win pattern') +
       stat_summary(fun.data = mean_se, geom = 'errorbar', aes(width = (0.2))) +
       ylim(0,1) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       coord_cartesian(ylim=c(0,maxy2)))

#First outcome to come to mind for second-win
plot(ggplot(subset(total_risk_per_participant, outcome_condition == 'second_win'),  aes(condition, risk_first_to_mind*0.4)) +
       stat_summary(fun.y = 'mean', geom = 'bar') +
       xlab('Type of memory induction') + ylab('Proportion (%)') +
       ggtitle('Recall for participants who received the second-win pattern') +
       stat_summary(fun.data = mean_se, geom = 'errorbar', aes(width = (0.2))) +
       ylim(0, 1) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
       coord_cartesian(ylim=c(0,maxy2)))


#Risk preferences as a function of outcome reported
total_risk_per_participant$risk_first_to_mind_factor <- factor(total_risk_per_participant$risk_first_to_mind, 
                                                               labels = c('bad', 'good'), levels = c(0, 1))
d <- ggplot(subset(total_risk_per_participant, !is.na(total_risk_per_participant$risk_first_to_mind_factor)), aes(risk_first_to_mind_factor, total_risk)) +
       stat_summary(fun.y = 'mean', geom = 'bar') + facet_grid(~condition) + 
       xlab('Outcome associated with risk') + ylab('Proportion of risky choices') +
       stat_summary(fun.data = mean_se, geom = 'errorbar', aes(width = 0.2)) + theme_bw() + 
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       coord_cartesian(ylim=c(0,0.6)) + theme(panel.grid.major = element_blank(),
                                              panel.grid.minor = element_blank(),
                                              strip.background = element_blank(),
                                              panel.border = element_blank())

dev.off()
library(ggpubr)

#Figure 2 of the paper
maxy <- 0.6
a1<-ggplot(total_risk_per_participant, aes(condition, total_risk)) + 
  stat_summary(fun.y = 'mean', geom = 'bar') +
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(width = 0.2)) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+coord_cartesian(ylim=c(0,maxy))+
  xlab('Type of memory induction') + ylab('Probability of choosing the risky option')+
  ggtitle('Risk-taking for all participants') + scale_x_discrete(labels = c('Episodic', 'General', 'Control (experiment 2)'))

b1 <- ggplot(subset(total_risk_per_participant, outcome_condition == 'first_win'), aes(condition, total_risk)) + 
  stat_summary(fun.y = 'mean', geom = 'bar') +
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(width = 0.2)) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle('Risk-taking in the first-win condition')+ 
  xlab('Type of memory induction') +coord_cartesian(ylim=c(0,maxy)) + ylab('') + 
  scale_x_discrete(labels = c('Episodic', 'General', 'Control (experiment 2)'))

c1 <- ggplot(subset(total_risk_per_participant, outcome_condition == 'second_win'), aes(condition, total_risk)) + 
  stat_summary(fun.y = 'mean', geom = 'bar') +
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(width = 0.2)) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle('Risk-taking in the second-win condition') + 
  xlab('Type of memory induction') + coord_cartesian(ylim=c(0,maxy)) + ylab('') + 
  scale_x_discrete(labels = c('Episodic', 'General', 'Control (experiment 2)'))

ggarrange(a1,b1,c1, ncol = 2, nrow = 2, labels = c('A', 'B', 'C'))


#Risk preferences over time. Question about numbers on the x axis. 
maxy <- 0.6
n_break <- 20
width_errorbar <- 0.2
width_dodge <- 0.02
data$risk_break_trials <- cut(data$trial, breaks = seq(from = 0, to = 100, by = n_break))

a2 <- ggplot(data, aes(risk_break_trials, chose_risky, shape = condition, group = condition)) + stat_summary(fun.y ='mean', geom= 'line') +
       scale_x_discrete(labels = seq(from = n_break, to = 100, by = n_break)) + xlab('Trial') + 
       ylab('p(chose_risky)') + stat_summary(fun.y = 'mean', geom = 'point') + theme_bw()+
       theme(legend.position = 'none',panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       ylab('Proportion of risky choices') + 
       stat_summary(fun.data = mean_se, geom = 'errorbar', aes(width = width_errorbar), position = position_dodge(width = width_dodge)) +
       coord_cartesian(ylim=c(0,maxy)) + ggtitle('Risk-taking for all participants')

#First win condition
b2 <- ggplot(subset(data, outcome_condition == 'first_win'), aes(risk_break_trials, chose_risky, shape = condition, group = condition)) + stat_summary(fun.y ='mean', geom= 'line') +
       scale_x_discrete(labels = seq(from = n_break, to = 100, by = n_break)) + xlab('Trial') + 
       stat_summary(fun.y = 'mean', geom = 'point') + theme_bw()+
       theme(legend.position = 'none',panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       ylab('') + stat_summary(fun.data = mean_se, geom = 'errorbar', aes(width = width_errorbar), position = position_dodge(width = width_dodge))+
       ggtitle('Risk-taking in the first-win condition') + 
       coord_cartesian(ylim=c(0,maxy))

#Second win condition
c2 <- ggplot(subset(data, outcome_condition == 'second_win'), aes(risk_break_trials, chose_risky, shape = condition, group = condition)) + stat_summary(fun.y ='mean', geom= 'line') +
       scale_x_discrete(labels = seq(from = n_break, to = 100, by = n_break)) + xlab('Trial') + 
       stat_summary(fun.y = 'mean', geom = 'point') + theme_bw()+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       ylab('') + stat_summary(fun.data = mean_se, geom = 'errorbar', aes(width = width_errorbar), position = position_dodge(width = width_dodge))+
       ggtitle('Risk-taking in the second-win condition') + 
       coord_cartesian(ylim=c(0,maxy))

ggarrange(a2, b2, c2, labels = c('A', 'B', 'C'))
