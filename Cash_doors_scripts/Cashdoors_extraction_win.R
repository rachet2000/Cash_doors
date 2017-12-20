#This script extracts the win data from the files and puts it in the data_win dataframe
#It puts both behavioral and memory data into the same file: Memory data are represented
#as columns

conditions_first <- c("General",
                      "General",
                      "Episodic",
                      "Episodic",
                      "General", 
                      "General",
                      "Episodic",
                      "Episodic",
                      "General",
                      "General",
                      "General",
                      "Episodic",
                      "Episodic",
                      "General",
                      "General",
                      "Episodic",
                      "Episodic",
                      "Episodic",
                      "Episodic",
                      "Episodic",
                      "General",
                      "General",
                      "General",
                      "General",
                      "Episodic",
                      "General",
                      "General",
                      "Episodic",
                      "Episodic",
                      "Episodic",
                      "General",
                      "General",
                      "General",
                      "General",
                      "General",
                      "Episodic",
                      "Episodic",
                      "Episodic",
                      "Episodic",
                      "General",
                      "General",
                      "Episodic",
                      "General",
                      "Episodic",
                      "General",
                      "Episodic",
                      "Episodic"
                      
)
conditions_second <- as.character(factor(!(conditions_first == "General"), labels = c("Episodic", "General")))


library(ggplot2)

videos_first <- c("Restaurant",
                  "Restaurant",
                  "Hospital",
                  "Restaurant",
                  "Restaurant",
                  "Hospital",
                  "Hospital",
                  "Hospital",
                  "Hospital",
                  "Hospital",
                  "Hospital",
                  "Restaurant",
                  "Hospital",
                  "Restaurant",
                  "Hospital",
                  "Restaurant",
                  "Restaurant",
                  "Hospital",
                  "Restaurant",
                  "Hospital",
                  "Hospital",
                  "Hospital",
                  "Restaurant",
                  "Hospital",
                  "Restaurant",
                  "Hospital",
                  "Hospital",
                  "Hospital",
                  "Restaurant",
                  "Restaurant",
                  "Restaurant", #Starts from next row
                  "Hospital",
                  "Restaurant",
                  "Restaurant",
                  "Restaurant",
                  "Restaurant",
                  "Hospital",
                  "Hospital",
                  "Hospital",
                  "Hospital",
                  "Restaurant",
                  "Hospital",
                  "Hospital",
                  "Restaurant",
                  "Restaurant",
                  "Hospital",
                  "Restaurant")


videos_second <- as.character(factor(!(videos_first == "Restaurant"), labels = c("Hospital", "Restaurant")))


file_names <- list.files(path = "gain_data", pattern = "cd")

#Extract data from files
age_data <- read.csv("gain_data/age_data.csv")
pre_data <- NULL
x = 0
for(file in file_names){
  data_temp <- read.table(paste('gain_data',file, sep = '/'), header = FALSE, sep = "\t", skip = 1, blank.lines.skip = TRUE,
                          fill = TRUE)
  x <- x + 1
  data_temp$participant <- paste('cd',x, sep = '')
  data_temp$condition <- c(rep(conditions_first[x], 109), rep(conditions_second[x], 110))
  data_temp$video <- c(rep(videos_first[x], 109), rep(videos_second[x], 110))
  data_temp$age <- age_data$age[x]
  data_temp$gender <- age_data$gender[x]
  
  pre_data <- rbind(pre_data, data_temp)
}
pre_data$participant <- as.factor(pre_data$participant)
pre_data$video <- as.factor(pre_data$video)
pre_data$condition <- as.factor(pre_data$condition)


pre_data$V1 <- NULL
pre_data$V11 <- NULL

#Separate data into recall and responses
recall_data <- pre_data[pre_data$V3 == "left" | pre_data$V3 == "right",]
response_data <- pre_data[pre_data$V3 == "first_win" | pre_data$V3 == "second_win",]


#I think V7 is useless
recall_data$V7 <- NULL
recall_data$V10 <- NULL
#Give the columns names
colnames(recall_data) <- c("recall", "door_side", "question", "door", "risky_door", "risk", "to_remove", "participant", "condition", "video", "age", "gender")
recall_data$to_remove <- NULL
colnames(response_data) <- c("trial", "outcome_condition", "rt", "chose_risky", "key_pressed", "reward", "color",
                             "session", "total_earned", "participant", "condition", "video", "age", "gender")

recall_data$session <- rep(c(rep('first_time', 9), rep('second_time', 9)), nrow(recall_data)/18)
#Running this is crucial or it glitches
recall_data$risk <- as.logical(recall_data$risk)
response_data$chose_risky <- as.logical(response_data$chose_risky)


data <- NULL
#Create the real dataframe

#For every participant
for(par in unique(pre_data$participant)){
  print(par)
  not_first <- FALSE
  #For both sessions
  for (ses in c("first_time", "second_time")){
    #Subset the response data only for that participant and that session
    temp_data <- subset(response_data, participant == par & session == ses)
    
    
    #Create a single row to put new variables in
    for(x in 1:nrow(temp_data)){
      row_data <- temp_data[x,]
      
      #Splitting session into 1 and 2 to be able to access first_to_mind recalls using the subset function later
      if(ses == "first_time"){
        session_int <- 1
      }
      else{session_int <- 2}
      
      #Assign the first outcome to mind to a new variable for that single row
      row_data$risk_first_to_mind <- subset(recall_data, question == "first_to_mind" & risk 
                                            & participant == par)[session_int, 1]
      row_data$total_risk <- mean(subset(response_data, participant == par & session == ses)$chose_risky, na.rm= T)
      row_data$judged_percent <- subset(recall_data, question == 2.5 & participant == par)[session_int, 1]
      #Just added this
      recall_participant <- subset(recall_data, participant == par & session == ses & question != 'first_to_mind' & question != 'confidence')
      row_data$total_judged <- sum(recall_participant[,1])
      row_data$confidence <- subset(recall_data, participant == par & session == ses & question == 'confidence')[1, 'recall']
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
      
      data <- rbind(data, row_data)
      
    }}
}

data$experiment <- factor('Gain')
data_win <- subset(data, session == "first_time")
