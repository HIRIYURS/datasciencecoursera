corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    if  (file.exists(directory) && threshold >= 0) {
        curwd <- getwd()
        setwd(directory)
        
        j <- 1
        corres <- vector(mode="numeric")
        
        flist <- list.files(pattern="*.csv")
        
        ## Read files
        for (i in 1:length(flist)) {
            tempdf <- read.csv(flist[i])
            tempdf <- na.omit(tempdf)
            if (nrow(tempdf) > threshold) {
                sulfcol <- tempdf$sulfate
                nitcol <- tempdf$nitrate
                corres[j] <- cor(sulfcol, nitcol)
                j <- j + 1
                rm(sulfcol, nitcol)
            }
            rm(tempdf)
        }
        ## Set back to the working directory
        setwd(curwd)
        
        ## Return the result in a vector
        corres<-round(corres, digits=5)
        corres
    } else {
        ## Invalid arguments, return 0
        0
    }
}