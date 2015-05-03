rankall<-function(outcome,num){
  
  #read the care of outcomes data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  #verify inputs of function
  validOutcome <-c("heart failure","pneumonia","heart attack")
  if (!outcome %in% validOutcome) {stop("Invalid Outcome")}
  
  ValidState = sort(unique(data[,7]))
  if (!state %in% ValidState) stop("Invalid State")
  
  #match outcome with data column 
  if(outcome=="heart attack"){outcomeselect<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"}
  else if(outcome=="heart failure"){outcomeselect<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"}
  else if(outcome=="pneumonia"){outcomeselect<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
  
  #For each state, find the hospital of the given rank 
  hospital <-character(0)
  
  for(i in seq_along(ValidState)){
    ## Return hospital name in each state with the given rank 30-day death rate
    stateselect <- data[data$State==ValidState[i],]
    
    #rank metadata by outcome
    sorted.stateselect <- stateselect[order(as.numeric(stateselect[[outcomeselect]]),stateselect[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    
    #converting character value in num
    metanum = num
    if(metanum=="best"){metanum=1}
    if(metanum=="worst"){metanum=nrow(sorted.stateselect)}
    
    #build datatable with individual state-outcome data
    hospital[i]<- sorted.stateselect[metanum,"Hospital.Name"]
  }
  ##Return data table with all ranked hospitals in their respective states
  data.frame(hospital=hospital, state=ValidState,row.names=ValidState)
  
  
}