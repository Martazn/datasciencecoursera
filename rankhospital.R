setwd("C:\\Users\\ladym\\Downloads\\rprog_data_ProgAssignment3-data")
## ============================================================
## Function: rankhospital
## Purpose: Return the hospital in a state that has the
##          specified rank (best, worst, or numeric) for a given outcome
## ============================================================

rankhospital <- function(state, outcome, num = "best") {
  ## -------------------------------
  ## 1. Read data
  ## -------------------------------
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## -------------------------------
  ## 2. Validate state
  ## -------------------------------
  valid_states <- unique(data[, 7])  # State is column 7
  if (!(state %in% valid_states)) {
    stop("invalid state")
  }
  
  ## -------------------------------
  ## 3. Validate outcome
  ## -------------------------------
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  ## Map outcome to column index
  outcome_col <- switch(outcome,
                        "heart attack" = 11,
                        "heart failure" = 17,
                        "pneumonia" = 23)
  
  ## -------------------------------
  ## 4. Filter rows by state
  ## -------------------------------
  state_data <- data[data[, 7] == state, c(2, 7, outcome_col)]
  names(state_data) <- c("Hospital.Name", "State", "Outcome")
  
  ## -------------------------------
  ## 5. Convert to numeric and remove NAs
  ## -------------------------------
  state_data$Outcome <- suppressWarnings(as.numeric(state_data$Outcome))
  state_data <- state_data[!is.na(state_data$Outcome), ]
  
  ## -------------------------------
  ## 6. Sort by outcome and hospital name
  ## -------------------------------
  ordered_data <- state_data[order(state_data$Outcome, state_data$Hospital.Name), ]
  
  ## -------------------------------
  ## 7. Determine rank
  ## -------------------------------
  if (num == "best") {
    rank <- 1
  } else if (num == "worst") {
    rank <- nrow(ordered_data)
  } else if (is.numeric(num)) {
    rank <- as.numeric(num)
  } else {
    stop("invalid rank")
  }
  
  ## -------------------------------
  ## 8. Handle out-of-range ranks
  ## -------------------------------
  if (rank > nrow(ordered_data)) {
    return(NA)
  }
  
  return(ordered_data$Hospital.Name[rank])
}


## ============================================================
## Example calls 
## ============================================================
# source("rankhospital.R")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
