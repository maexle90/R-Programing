rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  ## Check that state and outcome are valid
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(! outcome %in% outcomes) {
    stop("invalid outcome")
  }
  if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
  
  data <- data[c(2,7,11,17,23)] 
  names(data)[1] <- "name"
  names(data)[2] <- "State"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  ## For each state, find the hospital of the given rank
  
  States <-unique(data$State)
  names <- vector()
  for ( s in States) {
    data1 <- data[data$State==s & data[outcome] != 'Not Available', ]
    data1[outcome] <- as.data.frame(sapply(data1[outcome], as.numeric))
    data1 <- data1[order(data1$name, decreasing = FALSE), ]
    data1 <- data1[order(data1[outcome], decreasing = FALSE), ]
    vals <- data1[, outcome]
    if(num == "best") {
      
      rowNum <- which.min(vals)
      
    }
    else if (num == "worst") {
      rowNum <- which.max(vals)
    }
    else {
      rowNum <- num
    }
    ##print(data1[rowNum, ]$name)
    names <- c(names,data1[rowNum, ]$name)
    
  }
  data2 <- data.frame(names,States)
  names(data2)[1] <- "hospital"
  names(data2)[2] <- "state"
  data2 <- data2[order(data2$state, decreasing = FALSE), ]
  return(data2)
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}