complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    v_id <- NULL
    v_nobs <- NULL
    
    nrep <- 0
    for (i in id){
        numstr <- as.character(i)
        if (i < 10){
            numstr <- paste("00",numstr,sep="")
        } else if (i < 100){
            numstr <- paste("0",numstr,sep="")
        }
        filename = paste(directory,"//",numstr,".csv",sep="")
        df <- read.csv(filename)
        nobs <- sum(complete.cases(df))
        v_nobs <- c(v_nobs,nobs)
        v_id <- c(v_id,i)
        nrep <- nrep + 1
    }
    
    mat <- cbind(v_id,v_nobs)
    retdf <- as.data.frame(mat)
    names(retdf) <- c("id","nobs")
    retdf
}
