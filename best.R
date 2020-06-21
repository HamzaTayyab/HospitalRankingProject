
## ------------------ Task#2
best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(!any(state==outcomeData$State))
    stop("Invalid State")
  
  if(outcome != "pneumonia" && outcome != "heart attack" && outcome != "heart failure")
    stop("Invalid Outcome")
  if(outcome == "heart attack")
    i <- 11
  if(outcome == "heart failure")
    i <- 17
  if(outcome == "pneumonia")
    i <- 23
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  Statehosp <- outcomeData[outcomeData$State == state,]
  Statehosp <- Statehosp[order(Statehosp["Hospital.Name"]),]
  Lowest_death_rate = min(as.numeric(Statehosp[,i]), na.rm=TRUE)
  Hospital <- Statehosp$Hospital.Name[as.numeric(Statehosp[,i]) == Lowest_death_rate]
  hospitalName <- Hospital[complete.cases(Hospital)]
  
  return(hospitalName)
}


