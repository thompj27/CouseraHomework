#John Thompson
#R Programming Week4 Programming Assignment 3
#rankhospital.R

rankhospital <- function(state, outcome, num="best"){

  ## Read outcome data
  library(dplyr)
  library(magrittr)
  ## Read Outcome data
  ## check that state and outcome are valid
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

  #select for Death rate columns
  MyData %<>% select(Hospital.Name,  starts_with("Hospital.30.Day.Death"))

  #now select the right outcome column
  outcome = gsub(" ", ".", outcome) #replace space with period
  MyData %<>% select(Hospital.Name, ends_with(outcome))

  #squeeze out the NAs
  MyData = MyData[complete.cases(MyData),]
  #set the column names
  colnames(MyData) = c("Hospital.Name", outcome)

  #Rank by outcome, Hospital.Name
  MyData = MyData[order(MyData[2], MyData[1]),]
  ## return hospital name in that state with the given rank

  #now have ranked list
  #Set the value of the row to return
  if (tolower(num) == "best") {
    num = 1
  } else if (tolower(num) == "worst") {
    num = nrow(MyData)
  }

  if (num > nrow(MyData)) {
    return(NA)
  }else {
    return(MyData[num,"Hospital.Name"])
  }
}
