
## ------------------ Task#3
rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!any(state==data$State))
    stop("Invalid State")
  if(outcome != "pneumonia" && outcome != "heart attack" && outcome != "heart failure")
    stop("Invalid Outcome")
  if(outcome == "heart attack")
    i <- 11
  if(outcome == "heart failure")
    i <- 17
  if(outcome == "pneumonia")
    i <- 23
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  dataOurState <- data[data$State == state,]
  dataOutcome <- suppressWarnings(as.numeric(dataOurState[, i]))
  good <- complete.cases(dataOutcome)
  dataOutcome <- dataOutcome[good]
  dataOurState <- dataOurState[good,]
  dataOurState <- dataOurState[ order(dataOutcome,dataOurState["Hospital.Name"]), ]
  # dataOurState$Hospital.Name[length(dataOutcome)]
  if (grepl("^[0-9]+$", num)) {
    if (as.numeric(num) > length(dataOutcome)) {
      result <- NA
    }
    else {
      result <- dataOurState[as.numeric(num), "Hospital.Name"]
    }
  }    
  else if (num == "best") {
    result <- dataOurState[1, "Hospital.Name"]
  }
  else if (num == "worst") {
    result <- dataOurState[length(dataOutcome), "Hospital.Name"]
  }
  else result <- NA
  return(result)
}
