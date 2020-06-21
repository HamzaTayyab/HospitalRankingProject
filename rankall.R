
## ------------------ Task#4
rankall <- function(outcome = "heart attack", num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(outcome != "pneumonia" && outcome != "heart attack" && outcome != "heart failure")
    stop("Invalid Outcome")
  if(outcome == "heart attack")
    i <- 11
  if(outcome == "heart failure")
    i <- 17
  if(outcome == "pneumonia")
    i <- 23
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  DfAllstates <- data.frame(hospital = character(), state = character())
  dataPerState <- split(data, data$State)
  for (states in names(dataPerState)) {
    dataOurState <- dataPerState[[states]]
    dataOutcome <- suppressWarnings(as.numeric(dataOurState[, i]))
    good <- complete.cases(dataOutcome)
    dataOutcome <- dataOutcome[good]
    dataOurState <- dataOurState[good,]
    dataOurState <- dataOurState[ order(dataOutcome,dataOurState["Hospital.Name"]), ]
    
    if (num == "best") {
      numState <- c(1)
    } else {
      if (num == "worst") {
        numState <- length(dataOutcome)
      } else {
        numState <- num
      }
    }
    DfPerState <-  data.frame(hospital = dataOurState[numState, "Hospital.Name"], state = states, row.names = states)
    DfAllstates <- rbind(DfAllstates,DfPerState)
  }
  return(DfAllstates)
}
