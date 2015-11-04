# Programming Assignment 1: Air Pollution

# Retrieve data from directory and calculate mean of pollutant across all monitors list in the 'id' vector
pollutantmean <- function(directory, pollutant, id = 1:332) {
    # 'directory' is a character vector of length 1 indicating location of the CSV files
    # 'pollutant' is a character vector of length 1 indicating name of the pollutant for which the mean is calculated
    # 'id' is an integer vector indicating the monitor ID number  to be used
    
    # Read relevant *.csv files in directory and store into dataframe
    files <- dir(path=directory, pattern="*.csv", full.names=TRUE)
    dataframe <- data.frame(Date=character(), sulfate=numeric(), nitrate=numeric(), ID=integer())
    for (file in files) {
        # Use first lines to check if ID in file is in id and if so, add to dataframe
        dataframe_check <- read.csv(file,nrows=2)
        if (dataframe_check$ID[1] %in% id) {
            dataframe_new <- read.csv(file)
            dataframe <- rbind(dataframe, dataframe_new)
        }
    }

    # Select rows with ID in id and pollutant not NA from dataframe
    correctrows <- (dataframe$ID %in% id) & (!is.na(dataframe[pollutant]))

    # calculate mean of pollutant column
    mean(dataframe[correctrows,pollutant])    
}