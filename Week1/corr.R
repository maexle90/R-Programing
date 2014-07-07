corr <- function(directory, threshold = 0) {
  
  nFiles<-length(list.files(directory))
  
  vec <- c()
  
  for(i in 1:nFiles) {
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
      df <- read.csv(filename)
      good <- complete.cases(df)
    
    if(sum(good)>threshold) {
      data1 <- df[good,][,1:4]
      vec <- c(vec,cor(data1$sulfate,data1$nitrate))
    }
    
  }
  return(vec)
  
}