# Reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital
# that has the lowest 30-day mortality for the specified outcome in that state.
# 'state' parameter is 2 letter abbreviation for state. 'outcome' parameter can be one of "heart attack", "heart failure", or "pneumonia"
best <- function(state, outcome) {
    # Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # Check that state is valid
    if(! state %in% outcome_data$State) {
        stop("invalid state")
    }
    
    # Check that outcome is valid and select column
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

    # Return hospital name in that state with lowest 30-day death rate
    selected_data <- outcome_data[outcome_data$State==state, c("Hospital.Name", sel_column_name)]
    sorted_data <- selected_data[order(selected_data[,sel_column_name], selected_data$Hospital.Name),]
    best_hospital <- sorted_data$Hospital.Name[[1]]
    best_hospital
}