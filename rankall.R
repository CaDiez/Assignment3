rankall <- function(outcome, num = "best" ){
  
  ##Check that state and outcome are valid
  opt <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% opt) {stop("invalid outcome")} 
  ##Select the adequate column accordind to the outcome
  column <- switch (outcome, 
                    "heart attack"= 11,
                    "heart failure"=17,
                    "pneumonia"= 23)
  ## Read data from file.
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ##Create an array of unique states
  state <- data$State
  state <- sort(unique(state))
  
  ##Create a data frame that will contain the results
  OutList <- rep("", length(state))
  
  for (i in 1:length(state)) {
    statedata<- data[data$State==state[i],]
    ##Coerce the numeric value to be ordered
    valornum <- suppressWarnings(as.numeric(statedata[,column]))
    
    ##Gets Unique Ranks from values
    rank <- rank(valornum, na.last=NA)
    
    ## Converts best, worst in values
    ## Checks if the number of hospitals is bigger in the state.
    if (num=="best") {
      rank <- 1
    } else if (num =="worst") {
      rank <- length(rank)
    } else if (num <= length(rank) ) {
      rank <- num
    } else {
      rank <- NA
    }
    
    ##If there are nulls then writes a NA in the frame, else selects the hospital
    if (is.na(rank)) {
      OutList[i] <- NA
    } else {
      OutList[i] <- statedata$Hospital.Name[order(valornum, statedata$Hospital.Name)[rank]]
    }
  }
  ## Returns the frames with the data
  return(data.frame(hospital=OutList, state=state))
}
