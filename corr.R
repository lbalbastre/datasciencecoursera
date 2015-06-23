corr <- function(directory, threshold = 0) {
  
  result <- numeric(0)
  
  for(i in 1:332) {
    filename <- formatC(i, width=3, flag="0")
    data <- read.csv(file= file.path(directory, paste(filename, ".csv", sep="")), header=TRUE, sep=",")
    
    completedata <- data[complete.cases(data),]
    
    if(nrow(completedata) > threshold) { 
      result <- append(result, cor(completedata[["sulfate"]], completedata[["nitrate"]]))
    }
  }
  
  print(result)
}