rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    mort1 <- "heart attack"
    mort1idx <- 11
    mort2 <- "heart failure"
    mort2idx <- 17
    mort3 <- "pneumonia"
    mort3idx <- 23
    
    ## Validate if outcome is valid arg
    if (outcome == mort1 || outcome ==mort2 || outcome == mort3) {
        if (outcome == mort1) {
            idxMort <- mort1idx
        } else if (outcome == mort2) {
            idxMort <- mort2idx
        } else {
            idxMort <- mort3idx
        }
    } else stop("invalid outcome")
    
    
    
    ## Read from the file
    hospdata <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
    
    ## Validate if valid state 
    valSt <- (hospdata$State == state)
    if(any(valSt) == FALSE) stop("invalid state")
    
    ## Extract data for the given state
    StHospData <- subset(hospdata, hospdata$State == state)
    
    ## Convert the required column of mortality to numeric to get min
    StHospData[,idxMort] <- suppressWarnings(as.numeric(StHospData[,idxMort]))

    mortVect <- complete.cases(StHospData[,idxMort])
    StLowMort <- StHospData[mortVect,]

    StLowMort <- StLowMort[order(StLowMort[,idxMort], StLowMort$Hospital.Name),]

    ## determine the index to be returned from the result
    resultLen <- nrow(StLowMort)
    idx <- 1
    if (num == "best") {
        ## First rank
        idx <- 1
    } else if (num == "worst") {
        ## Last Rank
        idx = resultLen
    } else if (num > resultLen) {
        return(NA)
    } else {
        idx <- num
    }

    StLowMort$Hospital.Name[idx]
}