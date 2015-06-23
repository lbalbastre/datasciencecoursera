rankhospital <- function(state, outcome, num) {
  #Read the outcome-of-care-measures file and load it into data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #state does not exist
  if(!any(state == data$State)) {
    stop('invalid state')
  }
  
  #Filter data by state
  data.state <- data[data$State == state,]
  
  #Specify which column are we going to take
  if(outcome == "heart attack") {
    col <- 11
  } else if(outcome == "heart failure") {
    col <- 17
  } else if(outcome == "pneumonia") {
    col <- 23
  } else {
    stop('invalid outcome')
  }
 
  #Convert that column to numeric class instead of character 
  data.state[, col] <- as.numeric(x=data.state[, col])
  
  #Delete NA from data
  data.state <- data.state[complete.cases(data.state), ]
  
  
  if(num == "best") {
    num <- 1;
  } else if(num == "worst") {
    num <- nrow(data.state)
  }
  
  if(nrow(data.state) < num) {
    NA
  } else {
    index <- with(data.state, order(data.state[, col], data.state[, 2]))
    data.state.ordered <- data.state[index, ]
    
    data.state.ordered[[num, 2]]
  }
  
}