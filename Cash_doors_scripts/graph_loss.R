pdf('Loss_condition.pdf')

ggplot(subset(total_risk_per_participant, experiment == 'Loss'), aes(condition, total_risk)) + stat_summary(fun.y = 'mean', geom = 'bar') +
  ggtitle('Risk-taking in the loss condition (after trial 40)') + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2)

n_break <- 20

data$risk_break_trials <- cut(data$trial, breaks = seq(from = 0, to = 100, by = n_break))
ggplot(subset(data, experiment == "Loss"), aes(risk_break_trials, chose_risky, color = condition, group = condition)) + stat_summary(fun.y ='mean', geom= 'line') +
  scale_x_discrete(labels = seq(from = n_break, to = 100, by = n_break)) + xlab('Trial') + 
  ylab('p(chose_risky)') + stat_summary(fun.y = 'mean', geom = 'point') +
  ylab('Proportion of risky choices') + 
  coord_cartesian(ylim=c(0,1)) + ggtitle('Risk-taking in the loss condition')
dev.off()