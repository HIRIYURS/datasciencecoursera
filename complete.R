complete - function(directory, id = 1332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    if (file.exists(directory)) {
        currwd - getwd()
        j - length(id)
        k - 1
        totobs - data.frame(id=integer(j), nobs=integer(j))
        
        if (id  0 && id  999) {
            ## Set directory to read files from
            setwd(directory)
            
            for (i in id) {
                filename - sprintf(%03d.csv, i)
                if (file.exists(filename)) {
                    nobsdata - read.csv(filename)
                    nobsdata - na.omit(nobsdata)
                    totobs$id[k] - i
                    totobs$nobs[k] - nrow(nobsdata)
                    k - k + 1
                    rm(nobsdata)
                }
            }
            setwd(currwd)
            totobs
        } else {
            NA
        }
        
    } else {
        ## Directory does not exist
        NA
    }
}