best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!any(state == data$State)) {
    stop('invalid state')
  }
  
  
  data.state <- data[data$State == state,]
  
  if(outcome == "heart attack") {
    col <- 11
  } else if(outcome == "heart failure") {
    col <- 17
  } else if(outcome == "pneumonia") {
    col <- 23
  } else {
    stop('invalid outcome')
  }
  
  data.state[, col] <- as.numeric(x=data.state[, col])
  
  data.state <- data.state[complete.cases(data.state), ]
  
  data.best.hospital <- data.state[which(data.state[, col] == 
                                    min(data.state[, col])), ]
  
  print(data.best.hospital[[1,2]])
}