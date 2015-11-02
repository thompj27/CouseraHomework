#JRT
#Week4 Programming Assignment
#
library(magrittr)
library(dplyr)
outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
head(outcome)
outcome[,11] %<>% as.numeric
hist(outcome[,11])


#tests
source("best.R")
best("TX", "heart attack")
best("tx", "heart failure")
best("md", "heart attack")
best ("md", "pneumonia")

best("BB", "heart attack")
best("ny", "hert attack")

source("rankhospital.R")
rankhospital("TX", "heart failure", 4)
rankhospital("md", "heart attack", "worst")
rankhospital("mn", "heart attack", 5000)

source("rankall.R")
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)


> x = list(c("a", "A"), c("b", "B"), c("c", "C"))
> y = as.data.frame(matrix(unlist(x), nrow=3, byrow=T))
> y
V1 V2
1  a  A
2  b  B
3  c  C

