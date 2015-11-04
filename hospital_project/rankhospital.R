# Reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital
# that has the requested rank with respect to 30-day mortality for the specified outcome in that state.
# 'state' parameter is 2 letter abbreviation for state. 'outcome' parameter can be one of "heart attack", "heart failure", or "pneumonia"
# 'num' is the rank and can be a postive integer, or "best" or "worst"
rankhospital <- function(state, outcome, num) {
    # Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check that 'state' is valid
    if(! state %in% outcome_data$State) {
        stop("invalid state")
    }
    
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
    # Select relevant columns
    selected_data <- outcome_data[outcome_data$State==state, c("Hospital.Name", sel_column_name)]
    # Sort on sel_column_name and then on Hospital.Name
    sorted_data <- selected_data[order(selected_data[,sel_column_name], selected_data$Hospital.Name),]
    # Remove NA's
    sorted_without_na <- sorted_data[!is.na(sorted_data[[sel_column_name]]),]

    # Select hospital based on 'num' parameter
    maxnum <- nrow(sorted_without_na)
    if (num == "best") {
        selected_hospital <- sorted_without_na$Hospital.Name[[1]]
    } else if (num == "worst") {
        selected_hospital <- sorted_without_na$Hospital.Name[[maxnum]]
    } else if (is.numeric(num) && num>0 && num<=maxnum && round(num)==num) {
        selected_hospital <- sorted_without_na$Hospital.Name[[num]]
    } else {
        selected_hospital <- "NA"
    }
    
    # Return hospital name in that state with specific rank in 30-day death rate
    selected_hospital
}