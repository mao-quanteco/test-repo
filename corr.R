corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    id <- 1:332
    corr <- NULL
    for (i in id){
        numstr <- as.character(i)
        if (i < 10){
            numstr <- paste("00",numstr,sep="")
        } else if (i < 100){
            numstr <- paste("0",numstr,sep="")
        }
        filename = paste(directory,"//",numstr,".csv",sep="")
        df <- read.csv(filename)
        n_ccases <- sum(complete.cases(df)) 
        if (n_ccases > threshold){
            df_corr <- cor( df$sulfate, df$nitrate, use = "pairwise.complete.obs" )
            corr <- c(corr , df_corr)
        }
    }
    corr
}