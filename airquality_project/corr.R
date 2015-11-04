# Takes a directory of data files and a threshold for complete cases and
# calculates the correlation between sulfate and nitrate for monitor locations
# where the number of completely observed cases (on all variables) is greater than the threshold.

corr <- function(directory, threshold = 0) {
    # 'directory' is a character vector of length 1 indicating the location of the CSV files
    # 'threshold' is a numeric vector of length 1 indicating the number of completely observed observations (on all
    #     variables) required to compute the correlation between nitrate and sulfate; the default is 0
    
    # Determine complete cases for each file
    df.complete_cases <- complete(directory)
        
    # Select idnrs with complete cases >= threshold
    selected_idnrs <- df.complete_cases[df.complete_cases$nobs > threshold,"id"]

    # Loop over *.csv files in directory
    files <- dir(path=directory, pattern="*.csv", full.names=TRUE)
    correlationcoefficients <- c()
    for (file in files) {
        # Use first lines to check if ID in file is in selected_idnrs and if so, do calculations
        df.filecontents_check <- read.csv(file,nrows=2)
        if (df.filecontents_check$ID[1] %in% selected_idnrs) {
            df.filecontents <- read.csv(file)
            nitratedata <- df.filecontents[complete.cases(df.filecontents),"nitrate"]
            sulfatedata <- df.filecontents[complete.cases(df.filecontents),"sulfate"]

            # Calculate correlation between sulfate and nitrate
            corcoefficient <- cor(nitratedata,sulfatedata)

            # Append corcoefficient
            correlationcoefficients <- c(correlationcoefficients,corcoefficient)
        }
    }
         
    # Return vector of correlations for the monitors that meet the threshold requirement
    correlationcoefficients
}