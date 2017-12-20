source('Cash_doors_scripts/Cashdoors_analysis.R')
PANAS <- read.csv('gain_data/PANAS.csv', header = F)

colnames(PANAS) <- c('id', 1:20, 'choice')
PANAS$id <- unique(data_win$participant)

PANAS$positive <- rep(0,nrow(PANAS))
positive_columns <- c(1,3,5,9,10,12,14,16,17,19)
for(x in positive_columns){
  print(x)
PANAS$positive <- PANAS$positive + PANAS[, x + 1]
}

PANAS$negative <- rep(0, nrow(PANAS))
negative_columns <- c(2,4,6,7,8,11,13,15,18,20)
for(x in negative_columns){
  PANAS$negative <- PANAS$negative + PANAS[, x + 1]
}

draw_doors <- read.csv('gain_data/draw_the_doors.csv')

draw_doors$id <- draw_doors$X
draw_doors$X <- NULL
draw_doors$total_score <- rowSums(draw_doors[,c('shape1', 'color1', 'side1', 'knob1',
                                            'panel1', 'outline1', 'window1','shape1',
                                            'color2', 'side2', 'knob2',
                                            'panel2', 'outline2', 'window2')], na.rm = T)

draw_doors$left_door <- rowSums(draw_doors[,c('shape1', 'color1', 'side1', 'knob1',
                                          'panel1', 'outline1', 'window1','shape1')])


draw_doors$right_door <- rowSums(draw_doors[,c('shape2', 'color2', 'side2', 'knob2',
                                          'panel2', 'outline2', 'window2','shape2')])





total_risk_per_participant_gain <- total_risk_per_participant[total_risk_per_participant$experiment == 'Gain',]
total_risk_per_participant_gain$PANAS_positive <- PANAS$positive
total_risk_per_participant_gain$PANAS_negative <- PANAS$negative

total_risk_per_participant_gain$PANAS_total <- total_risk_per_participant_gain$PANAS_positive - total_risk_per_participant_gain$PANAS_negative


#cor(total_risk_per_participant_gain$PANAS_negative, 
#    total_risk_per_participant_gain$total_risk, use = 'complete.obs')


#anova(lm(total_risk ~ PANAS_total, total_risk_per_participant_gain))

total_risk_per_participant_gain$memory_doors <- draw_doors$total_score
total_risk_per_participant_gain$memory_left_door <- draw_doors$left_door
total_risk_per_participant_gain$memory_right_door <- draw_doors$right_door

#anova(lm(door_choice ~ memory_left_door + memory_right_door + memory_right_door*memory_left_door, data = total_risk_per_participant_gain))

#cor(total_risk_per_participant_gain$door_choice, total_risk_per_participant_gain$right_minus_left_door, use = 'complete.obs')

#cor(total_risk_per_participant_gain$memory_doors, 
#    total_risk_per_participant_gain$total_risk, use = 'complete.obs')
total_risk_per_participant_gain$is_left_risk <- factor(total_risk_per_participant_gain$is_left_risk)

total_risk_per_participant_gain$right_minus_left_door <- total_risk_per_participant_gain$memory_right_door - total_risk_per_participant_gain$memory_left_door

total_risk_per_participant_gain[total_risk_per_participant_gain$is_left_risk == 0,
                                'memory_risk_door'] <- total_risk_per_participant_gain[total_risk_per_participant_gain$is_left_risk == 0, 'memory_right_door']
total_risk_per_participant_gain[total_risk_per_participant_gain$is_left_risk == 1,
                                'memory_risk_door'] <- total_risk_per_participant_gain[total_risk_per_participant_gain$is_left_risk == 1, 'memory_left_door']



#anova(lm(total_risk ~ memory_left_door + memory_left_door*is_left_risk + memory_right_door + is_left_risk + memory_right_door*is_left_risk*condition, total_risk_per_participant_gain))


#ggplot(total_risk_per_participant_gain, aes(memory_right_door, total_risk)) + stat_summary(fun.y = 'mean', geom = 'point')+facet_grid(~is_left_risk)
#ggplot(total_risk_per_participant_gain, aes(memory_left_door, total_risk)) + stat_summary(fun.y = 'mean', geom = 'point')+facet_grid(~is_left_risk)


#Signy wanted this

data_win$participant <- as.character(data_win$participant)
total_risk_per_participant_gain$participant <- as.character(total_risk_per_participant_gain$participant)
new_data_win <- ddply(data_win, c('participant'), function(data_win_single_subject){
  
  single_subject <- subset(total_risk_per_participant_gain, participant == data_win_single_subject$participant[1])
  print(data_win_single_subject$participant[1])
  
  data_win_single_subject$PANAS_positive = single_subject$PANAS_positive[1]
  data_win_single_subject$PANAS_negative = single_subject$PANAS_negative[1]
  data_win_single_subject$draw_left = single_subject$memory_left_door[1]
  data_win_single_subject$draw_right = single_subject$memory_right_door[1]
  data_win_single_subject$draw_total = single_subject$memory_doors[1]
  data_win_single_subject$draw_risk = single_subject$memory_risk_door[1]
  data_win_single_subject$total_risk = single_subject$total_risk[1]

return(data_win_single_subject)})

data_signy <- smartbind(new_data_win, data_control)


