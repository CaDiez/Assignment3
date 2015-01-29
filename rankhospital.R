rankhospital<-function(state, outcome, num="best"){
        ##Check that state and outcome are valid
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
        
        ##Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
                
        ##Return hospital name in that state with the given rank
        ##30-day death rate
        data<-subset(data, State==state)
        ##Coerce to numeric & supress warnings
        data[,column] <- suppressWarnings(as.numeric(data[,column]))  
        ##Eliminates NA from the frame
        data<- data[complete.cases(data), ]        
        data<- data[order(data[,column], data[,2], na.last=T),c(2,column)] 
        ## Validates, check, worst and bigger than the number of rows       
        if (num=="best") {num=1}
        else { if (num=="worst") {num = nrow(data)} else
                {if (num > nrow(data)) num=NA}
        }
        
        if (is.na(num)) x<- NA
        else x<- as.vector(data[num,1])
        
        ##returns name of the hospital
        return(x)  
}
