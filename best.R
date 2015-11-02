#John Thompson
#R Programming Week4 Programming Assignment 3
#best.R
best <- function(state, outcome){
  library(dplyr)
  library(magrittr)

  outcome %<>% tolower
  validoutcomes = c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% validoutcomes) {
    stop("invalid outcome")
  }
  outcome = gsub(" ", ".", outcome) #replace space with period

  #read outcome data
  MyData <- read.csv("outcome-of-care-measures.csv",
          na.string=c("Not Available", "NA"), stringsAsFactors = F)

  #filter for state
  MyData %<>% filter(State == toupper(state))

  if (nrow(MyData) == 0) {
    stop("invalid state")
  }

  MyData %<>% select(Hospital.Name,  starts_with("Hospital.30.Day.Death"))

  #now select the right outcome column
  outcome = gsub(" ", ".", outcome) #replace space with period
  MyData %<>% select(Hospital.Name, ends_with(outcome))
  ##check that the state and outcome data are valid
  #squeeze out the NAs
  MyData = MyData[complete.cases(MyData),]
  #set the column names
  colnames(MyData) = c("Hospital.Name", outcome)
  MinMort = min(MyData[,outcome])
  MyData = MyData[MyData[outcome] == MinMort,]
  if (nrow(MyData) > 1) {
    MyData = MyData[order(MyData$Hospital.Name),]
  }
  ##Return Hospital.Name for state with the lowest 30d death rate.
  return (MyData$Hospital.Name[1])
}
