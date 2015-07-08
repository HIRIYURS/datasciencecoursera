pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        currwd <- getwd()
        
        ## Print Args
        ##print(id)
        ##print(pollutant)
        ##print(directory)
        
        ## Consider only if id is less than 999 (typically 332 here)
        if (id < 999 && (pollutant == "sulfate" || pollutant == "nitrate")) {

                ## set working directory from the first argument
                setwd(directory)
                
                if(exists("airqlty")) {
                        rm(airqlty)
                }
                
                ## Loop through files in id and get mean of each
                for (i in id) {
                        ## Form the filename
                        filename <- sprintf("%03d.csv", i)
                        
                        ## Start collating the file contents into
                        ## a data frame
                        if (!exists("airqlty")) {
                                ## Start reading the file contents
                                airqlty <- read.csv(filename)
                        } else {
                                ## Append the file contents to our data frame
                                tempdata <- read.csv(filename)
                                airqlty <- rbind(airqlty, tempdata)
                                rm(tempdata)
                        }
                        
                }
                
                ## get the mean now
                meanval <- mean(airqlty[[pollutant]], na.rm=TRUE)
                
                ## Setback the currwd
                setwd(currwd)
                
                ## return the meanval
                round(meanval, digits=3)
        } else {
                ## Return NA
                print("Invalid arguments passed")
                NA
        }
        
}