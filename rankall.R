#need to sort desc for "worst"
#
#John Thompson
#R Programming Week4 Programming Assignment 3
##rankall.R
rankall <- function(outcome, num = "best"){
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

  #select for Death rate columns
  MyData %<>% select(Hospital.Name, State, starts_with("Hospital.30.Day.Death"))

  #now select the right outcome column
  outcome = gsub(" ", ".", outcome) #replace space with period
  MyData %<>% select(Hospital.Name, State, ends_with(outcome))

  #squeeze out the NAs
  MyData = MyData[complete.cases(MyData),]
  #set the column names
  colnames(MyData) = c("Hospital.Name", "State", outcome)

  #Rank by State, outcome, Hospital.Name
  if (num == "worst"){
    MyData = MyData[order(MyData$State, -MyData[outcome], MyData$Hospital.Name),]
  } else {
    MyData = MyData[order(MyData$State, MyData[outcome], MyData$Hospital.Name),]
  }
  ## return hospital name in that state with the given rank

  #now have ranked list
  #Set the value of the row to return
  if (tolower(num) == "best" | tolower(num) == "worst") {
    num = 1
  }

  #get the states
  mystates = unique(MyData$State)

  statelist = list()
  for (i in mystates){
    Dat = filter(MyData, State == i)
#     if (tolower(num) == "worst") {
#       num = nrow(Dat)
#     }
    if (num > nrow(Dat)){
      #add list of hosp name, state
      statelist[[i]] = list(Hospital = NA, State = i)
    } else {
      #add list (hospname[i,"Hospital.Name"], state)
      statelist[[i]] = list(Hospital = Dat$Hospital.Name[num], State = i)
    }
  }
  #unlist into a dataframe
  df = as.data.frame(matrix(unlist(statelist),
                            nrow=length(statelist), byrow=T))
  colnames(df) = c("hospital", "state")
  return(df)
}
