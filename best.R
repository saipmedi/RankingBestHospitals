best <-function(state,outcome) {
  
  ##read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  
  
  #check that state and outcome are valid
  validOutcome <-c("heart failure","pneumonia","heart attack")
  if (!outcome %in% validOutcome) {stop("Invalid Outcome")}
  
  validState <- unique(data[,7])
  if (!state %in% validState) {stop("Invalid State")}
  
  
#return hospital name in that state with lowest 30-day death rate
  
  stateselect<-data[data[,7]==state, ]
  
  #match outcome with  data column 
  if(outcome=="heart attack"){outcomeselect<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"}
  else if(outcome=="heart failure"){outcomeselect<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"}
  else if(outcome=="pneumonia"){outcomeselect<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
  
  #match columns to return the correct hospital
  metadata<-which.min(as.double(stateselect[,outcomeselect]))
  stateselect[metadata,"Hospital.Name"]  
  
}