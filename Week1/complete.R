complete <- function(directory = character(),id = 1:332) {
  
  dataFrame <- NULL
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
    df <- read.csv(filename)
    good <- complete.cases(df)
    nobs <- sum(good)
    rbind(dataFrame,data.frame(id=i,nobs=nobs))->dataFrame
}
 return(dataFrame)
}