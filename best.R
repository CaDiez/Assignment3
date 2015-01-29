best<-function(state, outcome){
        ## Check that state and outcome are valid and then check the outcome if not send the appropiate error
        opt <- c("heart attack", "heart failure", "pneumonia")
        if (!outcome %in% opt) {stop("invalid outcome")} 
        else {
                if (!state %in% data$State){stop ("invalid state")}
        }        
        ##select the appropiate colummn for the selection
        column <- switch (outcome,
                "heart attack"= 11,
                "heart failure"=17,
                "pneumonia"= 23)
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
                
        ## Return hospital name in that state with lowest 30-day death
        data<-subset(data, State==state)
        ##Coerce to numeric & supress warnings
        data[,column] <- suppressWarnings(as.numeric(data[,column]))   
        data<- data[order(data[,column], na.last=TRUE),2] 
        ## rate       
        return(as.vector(data[1]))        
}
