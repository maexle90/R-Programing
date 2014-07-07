rankhospital <- function(state, outcome, num = "best") {
  
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(! outcome %in% outcomes) {
    stop("invalid outcome")
  }
  
  ## Read outcome data
  
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Filter and simplify the column names
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "State"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
 ## Check that state and outcome are valid
 states <- data$State
 if(!state %in% states) {
   stop("invalid state")
 }
 
 ## Validate the num value
 if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
 
 
 data <- data[data$State==state & data[outcome] != 'Not Available', ]
 
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
 data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
 data <- data[order(data$name, decreasing = FALSE), ]
 data <- data[order(data[outcome], decreasing = FALSE), ]
 vals <- data[, outcome]
  if( num == "best" ) {
           rowNum <- which.min(vals)
       } 
 else if( num == "worst" ) {
             rowNum <- which.max(vals)
         } 
 else {
        rowNum <- num
          }
 data[rowNum, ]$name
}