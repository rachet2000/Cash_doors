source('Cash_doors_scripts/Cashdoors_analysis.R')

library(lme4)
library(afex)

pvalues_memory <- NULL
for(c in unique(total_risk_per_participant$condition)){
pvalues_memory$p <- c(pvalues_memory$p, anova(lm(total_risk ~ condition, data = subset(total_risk_per_participant, condition != c)))[[5]][1])
pvalues_memory$test <- c(pvalues_memory$test, paste('means without', c))

  model_slope <- glmer(chose_risky ~ trial + condition + condition*trial + (trial|participant), subset(data, condition != c), family = binomial)
  p <- data.frame(summary(model_slope)[10])[4,4]
  pvalues_memory$p <- c(pvalues_memory$p,p)
  pvalues_memory$test <- c(pvalues_memory$test, paste('slopes without', c))
  
  #model_intercept <- glmer(chose_risky ~ trial + condition + (trial|participant), subset(data, condition != c), family = binomial)
  #p <- data.frame(summary(model_intercept)[10])[3,4]
  #pvalues_memory$p <- c(pvalues_memory$p,p)
  #pvalues_memory$test <- c(pvalues_memory$test, paste('intercept without', c))
  
  
  
}

pvalues_memory <- data.frame(pvalues_memory)


pvalues_outcome <- NULL
for(c in unique(total_risk_per_participant$condition)){
  pvalues_outcome$p <- c(pvalues_outcome$p, anova(lm(total_risk ~ outcome_condition, data = subset(total_risk_per_participant, condition != c)))[[5]][1])
  pvalues_outcome$test <- c(pvalues_outcome$test, paste('means without', c))
  
  model_slope <- glmer(chose_risky ~ trial + outcome_condition + outcome_condition*trial + (trial|participant), subset(data, condition != c), family = binomial)
  p <- data.frame(summary(model_slope)[10])[4,4]
  pvalues_outcome$p <- c(pvalues_outcome$p,p)
  pvalues_outcome$test <- c(pvalues_outcome$test, paste('slopes without', c))
  
  model_intercept <- glmer(chose_risky ~ trial + outcome_condition + (trial|participant), subset(data, condition != c), family = binomial)
  p <- data.frame(summary(model_intercept)[10])[3,4]
  pvalues_outcome$p <- c(pvalues_outcome$p,p)
  pvalues_outcome$test <- c(pvalues_outcome$test, paste('intercept without', c))
  
  model_interaction <- glmer(chose_risky ~ trial + condition + outcome_condition + (trial|participant), subset(data, condition != c), family = binomial)
  p <- data.frame(summary(model_interaction)[10])[3,4]
  pvalues_outcome$p <- c(pvalues_outcome$p,p)
  pvalues_outcome$test <- c(pvalues_outcome$test, paste('interaction without', c))
  
  
}

pvalues_outcome <- data.frame(pvalues_outcome)


#----------------------------------------

anova(lm(total_risk ~ risk_first_to_mind + condition + 
           risk_first_to_mind*condition, data = total_risk_per_participant))


#################

a <- 'Baseline'
mean(subset(total_risk_per_participant, condition == a)$total_risk)
sd(subset(total_risk_per_participant, condition == a)$total_risk)

anova(lm(total_risk ~ condition, data = subset(total_risk_per_participant, condition !='Episodic')))


summary(glmer(chose_risky ~ trial + (trial|participant), 
              family = binomial, data = subset(data, condition == 'Control')))


summary(glmer(chose_risky ~ trial + condition + trial*condition + (trial|participant), 
              family = binomial, data = subset(data, condition != 'Episodic')))

sum(subset(total_risk_per_participant, condition == 'Baseline')$risk_first_to_mind)

anova(lm(total_risk ~ risk_first_to_mind, data = subset(total_risk_per_participant, condition != 'Baseline')))

positive1 = sum(subset(total_risk_per_participant, condition == 'Episodic')$risk_first_to_mind)
negative1 = length(subset(total_risk_per_participant, condition == 'Episodic')$risk_first_to_mind) - positive1

positive2 = sum(subset(total_risk_per_participant, condition == 'Control')$risk_first_to_mind)
negative2 = length(subset(total_risk_per_participant, condition == 'Control')$risk_first_to_mind) - positive2

chisq.test(data.frame(c(positive1, negative1), c(positive2, negative2)))


###################
t_test_all_trials <- function(condition1, condition2, exp){
pvalues <- NULL
p <- NULL
for(i in 1:90){
  means_control <- ddply(subset(data, condition == condition1 & experiment == exp), c('participant'), function(subject){
    return(mean(subset(subject, trial2 > i)$chose_risky, na.rm = T))
  })
  
  means_baseline <- ddply(subset(data, condition == condition2 & experiment == exp), c('participant'), function(subject){
    return(mean(subset(subject, trial2 > i)$chose_risky, na.rm = T))
  })
  
  
  p <- c(p, t.test(means_control$V1, means_baseline$V1, var.equal = F)[[3]])
}
ggplot(data.frame(t = 1:90, p), aes(t, p)) + geom_point() + geom_hline(yintercept = 0.05) + xlab('Analysis excluding every trial before X') +
  ylab('P-value') + ggtitle('P-values of the difference between the baseline and control groups')
}