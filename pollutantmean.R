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
    df = NULL
    for (i in id){
        numstr <- as.character(i)
        if (i < 10){
            numstr <- paste("00",numstr,sep="")
        } else if (i < 100){
            numstr <- paste("0",numstr,sep="")
        }
        filename = paste(directory,"//",numstr,".csv",sep="")
        df <- rbind(df,read.csv(filename))     
    }
    mean(df[[pollutant]], na.rm = TRUE)
}