best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    mort1 <- "heart attack"
    mort1idx <- 13
    mort2 <- "heart failure"
    mort2idx <- 19
    mort3 <- "pneumonia"
    mort3idx <- 25

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
    
    ## Get row with minimum mortality
    StLowMort <- subset(StHospData, 
                        StHospData[,idxMort] == min(StHospData[,idxMort], na.rm = TRUE))

    StLowMort <- StLowMort[order(StLowMort$Hospital.Name),]
    StLowMort$Hospital.Name[1]
}