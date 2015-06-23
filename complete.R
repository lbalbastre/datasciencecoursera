complete <- function(directory, id = 1:332) {
  
  result <- data.frame(id=numeric(0), nobs=numeric(0))
  
  for(i in id) {
    filename <- formatC(i, width=3, flag="0")
    data <- read.csv(file= file.path(directory, paste(filename, ".csv", sep="")), header=TRUE, sep=",")
    
    completedata <- data[complete.cases(data),]
    
    
    result[nrow(result) + 1, ] <- c(i, nrow(completedata))
  }
  
  print(result)
}