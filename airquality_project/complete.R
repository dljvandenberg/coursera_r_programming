# Programming Assignment 1: Air Pollution


# Retrieve data from directory and report number of completely observed cases in each data file
complete <- function(directory, id = 1:332) {
    # 'directory' is a character vector of length 1 indicating location of the CSV files
    # 'id' is an integer vector indicating the monitor ID number  to be used
    
    # Read relevant *.csv files in directory and calculate number of complete cases for each file
    files <- dir(path=directory, pattern="*.csv", full.names=TRUE)
    # Initialize dataframe
    df.file_nrcomplete <- data.frame(id=integer(), nobs=integer())
    for (file in files) {
        # Use first lines to check if ID in file is in id
        df.filecontents_check <- read.csv(file,nrows=2)
        idnr <- df.filecontents_check$ID[1]
        if (idnr %in% id) {
            # Calculate number of complete cases in whole file
            df.filecontents <- read.csv(file)
            nobs <- sum(complete.cases(df.filecontents))
            
            # Add info to dataframe to be returned
            newrow <- c(idnr, nobs)
            df.file_nrcomplete <- rbind(df.file_nrcomplete, newrow)
        }
    }
    # Return data frame where column 1 is id number and column 2 is the number of complete cases (nobs)
    colnames(df.file_nrcomplete) <- c("id", "nobs")
    df.file_nrcomplete
}       
    
