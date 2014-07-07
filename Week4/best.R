best <- function(state, outcome) {
  ##Checks outcome is valid or not 
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
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- data[data$State==state & data[outcome] != 'Not Available', ]
  vals <- data[, outcome]
  rowNum <- which.min(vals)
  data[rowNum, ]$name
  
}
