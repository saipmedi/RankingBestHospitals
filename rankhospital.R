rankhospital <- function(state, outcome, num){
  #read the care of outcomes data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  
  #verify inputs of function
  validOutcome <-c("heart failure","pneumonia","heart attack")
  if (!outcome %in% validOutcome) {stop("Invalid Outcome")}
  
  validState <- unique(data[,7])
  if (!state %in% validState) {stop("Invalid State")}
  
  #match outcome with data column 
  if(outcome=="heart attack"){outcomeselect<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"}
  else if(outcome=="heart failure"){outcomeselect<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"}
  else if(outcome=="pneumonia"){outcomeselect<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
  
  #create state-metadata
  stateselect<-data[data[,7]==state, ]
  
  #rank the hospitals based on stateselect and outcomeselect
  sorted.stateselect <- stateselect[order(as.numeric(stateselect[[outcomeselect]]),stateselect[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
  
  #handling best/worst input for num
  if (num=="best") num = 1
  if (num=='worst') num = nrow(sorted.stateselect)
  sorted.stateselect[num,"Hospital.Name"]
  
}