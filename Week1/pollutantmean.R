pollutantmean <- function(directory = character(),pollutant= character(), id = 1:332 ) {
  
  data1 <- vector()
  for(i in id) {
    
      if (i < 10) {
        filename <- paste(paste("00",i,sep=""),"csv",sep=".")
      }
      else if(i>= 10 && i<100)  {
        filename <- paste(paste("0",i,sep=""),"csv",sep=".")
      }
      else {
        filename <- paste(i,"csv",sep=".")
      }
    filename <- paste(directory,"/",filename,sep="")
    
    data <- read.csv(filename)
    
    bin <- is.na(data[pollutant])
    
    data1 <- c(data1,data[pollutant][!bin])
    }
 return(mean(data1))
}