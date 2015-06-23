pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  alldata = numeric()
  
  for(i in id) {
    filename <- formatC(i, width=3, flag="0")
    data <- read.csv(file= file.path(directory, paste(filename, ".csv", sep="")), header=TRUE, sep=",")
    
    alldata = c(alldata, data[[pollutant]])
    
  }
  meandata <- mean(alldata, na.rm = TRUE)
  print(meandata)
}