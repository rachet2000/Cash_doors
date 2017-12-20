#This file extracts data from the control condition and puts it in the data_loss dataframe. Make sure you are in the "Cash_doors" directory. 
conditions_loss <- c("Episodic",
                    'General',
                    'Episodic',
                    'General',
                    'Episodic',
                    'General',
                    'Episodic',
                    'Episodic',
                    'Episodic',
                    'Episodic',
                    'General',
                    'Episodic',
                    'Episodic',
                    'General',
                    'General',
                    'Episodic',
                    'General',
                    'General',
                    'Episodic'
            
)
#setwd('Cash_doors')
file_names <- list.files(path = "Loss_data", pattern = "lo")
extract <- NULL
s = 0
for(file in file_names){
  s = s + 1
  subject <- read.table(paste('Loss_data/', file, sep = ''), header = FALSE, sep = "\t", skip = 1, blank.lines.skip = TRUE,
                        fill = TRUE)
  subject$participant <- unlist(strsplit(file, split = '.d'))[1]
  subject$condition <- conditions_loss[s]
  
  extract <- rbind(extract, subject)
}

extract$V1 <- NULL
extract$V11 <- NULL
#Separate data into recall and responses
recall_data <- extract[extract$V3 == "left" | extract$V3 == "right",]
response_data <- extract[extract$V3 == "first_win" | extract$V3 == "second_win",]


#I think V7 is useless

recall_data$V7 <- NULL
recall_data$V10 <- NULL

#Give the columns names
colnames(recall_data) <- c("recall", "door_side", "question", "door", "risky_door", "risk", "to_remove", "participant", "condition")
recall_data$to_remove <- NULL
colnames(response_data) <- c("trial", "outcome_condition", "rt", "chose_risky", "key_pressed", "reward", "color",
                             "session", "total_earned", "participant", "condition")


#Running this is crucial or it glitches
recall_data$risk <- as.logical(recall_data$risk)
response_data$chose_risky <- as.logical(response_data$chose_risky)


data_loss <- NULL
#Create the real dataframe

#For every participant
for(par in unique(extract$participant)){
  not_first <- FALSE
  #For both sessions
  #Subset the response data only for that participant and that session
  temp_data <- subset(response_data, participant == par)
  
  
  #Create a single row to put new variables in
  for(x in 1:nrow(temp_data)){
    row_data <- temp_data[x,]
    
    #Assign the first outcome to mind to a new variable for that single row
    row_data$risk_first_to_mind <- subset(recall_data, question == "first_to_mind" & risk 
                                          & participant == par)[1, 1]
    row_data$total_risk <- mean(subset(response_data, participant == par)$chose_risky, na.rm= T)
    
    row_data$judged_percent <- subset(recall_data, question == 2.5 & participant == par)[1, 1]
    #Just added this
    row_data$total_judged <- sum(subset(recall_data, participant == par)[,1])
    
    
    row_data$block <- as.integer((row_data$trial -1)/20) + 1
    
    
    
    if(not_first == TRUE){
      row_data$last_risk <- last_risk$reward}
    else{
      row_data$last_risk <- NA}
    
    if(!is.na(row_data$chose_risky)){
      if(row_data$chose_risky){
        last_risk <- row_data
        not_first <- TRUE
      }}
    
    data_loss <- rbind(data_loss, row_data)
    
  }}

data_loss$experiment <- factor('Loss')
data_loss$condition = factor(data_loss$condition)


