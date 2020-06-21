## ------------------ Task#1 

library(ggplot2)

# Importing the data from the file
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

# Converting the 30 day Mortality rates from Heart Attack
outcome[, 11] <- as.numeric(outcome[, 11])

# Plotting 30 days Mortality rate
thirtyDayMortalityRate = outcome[, 11]
ggplot(outcome, aes(x=thirtyDayMortalityRate)) + geom_histogram(binwidth = 0.01)


source("best.R")
best("AK", "pneumonia")
best("TX", "heart attack")


source("rankhospital.R")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
rankhospital("TX", "heart failure", 4)

source("rankall.R")
head(rankall("heart attack", 20), 10)
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)


