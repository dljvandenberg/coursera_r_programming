# Reads the outcome-of-care-measures.csv file and returns a 2-column data frame containing the hospital in each state
# that has the ranking specified in num and state in the second column
rankall <- function(outcome, num = "best") {
    # Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # Check that 'outcome' is valid and select column
    if(outcome == "heart attack") {
        sel_column_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        sel_column_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia") {
        sel_column_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("invalid outcome")
    }

    # Convert 'outcome' column to numeric
    outcome_data[,sel_column_name] <- suppressWarnings(as.numeric(outcome_data[,sel_column_name]))
    # Remove NA's
    data_without_na <- outcome_data[!is.na(outcome_data[[sel_column_name]]),]
    # Only select relevant columns
    clean_data <- data_without_na[,c("Hospital.Name", "State", sel_column_name)]
    
    # For each state, find the hospital of the given rank
    split_data <- split(clean_data, data_without_na$State)
    list_of_hospitals_with_state <- lapply(split_data, function (x) retrieve_ranked_hospital(x, sel_column_name, num))
    
    ## Return a data frame with the hospital names and the (abbreviated) state name
    df.hospitals_with_state <- as.data.frame(do.call(rbind,list_of_hospitals_with_state))
    df.hospitals_with_state$state <- rownames(df.hospitals_with_state)
    names(df.hospitals_with_state) <- c("hospital", "state")
    df.hospitals_with_state
}


# Retrieves Hospital.Name of hospital ranked 'num' in dataframe 'df'
retrieve_ranked_hospital <- function(df, sel_column_name, num) {
    # Sort on sel_column_name and Hospital.Name
    df.sorted <- df[order(df[,sel_column_name], df$Hospital.Name),]
    
    # Select hospital based on 'num' parameter
    maxnum <- nrow(df.sorted)
    if (num == "best") {
        selected_hospital <- df.sorted$Hospital.Name[[1]]
    } else if (num == "worst") {
        selected_hospital <- df.sorted$Hospital.Name[[maxnum]]
    } else if (is.numeric(num) && num>0 && num<=maxnum && round(num)==num) {
        selected_hospital <- df.sorted$Hospital.Name[[num]]
    } else {
        selected_hospital <- "<NA>"
    }
    
    #state <- df.sorted[df.sorted$Hospital.Name==selected_hospital,"State"]
    #c(selected_hospital, state)
    selected_hospital
}