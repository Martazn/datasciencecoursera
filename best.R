## ============================================================
## Assignment: Plot 30-day mortality rates for heart attack
## Files required:
##   - outcome-of-care-measures.csv
##   - hospital-data.csv
##   - Hospital_Revised_Flatfiles.pdf (for reference)
## ============================================================
setwd("C:\\Users\\ladym\\Downloads\\rprog_data_ProgAssignment3-data")
## 1. Read in the outcome data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Look at the first few rows
head(outcome)

## Check how many columns and rows
ncol(outcome)
nrow(outcome)

## See the names of the columns
names(outcome)

## ============================================================
## 2. Convert the heart attack mortality rate column to numeric
## According to the documentation, 
## column 11 corresponds to "Hospital 30-Day Death (Mortality) Rates from Heart Attack"
## ============================================================

## Convert column 11 to numeric (it was read as character)
outcome[, 11] <- as.numeric(outcome[, 11])

## You may see warnings about NAs being introduced — that’s fine
## (those are from non-numeric entries such as "Not Available")

## ============================================================
## 3. Plot a histogram of the heart attack mortality rates
## ============================================================

hist(
  outcome[, 11],
  main = "Hospital 30-Day Mortality Rates from Heart Attack",
  xlab = "30-Day Death Rate (%)",
  col = "lightblue",
  border = "black"
)

## ============================================================
## 4. (Optional) Basic summary of the data
## ============================================================
summary(outcome[, 11])


## ============================================================
## Function: best
## Purpose: Return the hospital in a state with the lowest
##          30-day mortality rate for a given outcome
## ============================================================

best <- function(state, outcome) {
  ## -------------------------------
  ## 1. Read outcome data
  ## -------------------------------
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## -------------------------------
  ## 2. Validate 'state' input
  ## -------------------------------
  valid_states <- unique(data$State)
  if (!(state %in% valid_states)) {
    stop("invalid state")
  }
  
  ## -------------------------------
  ## 3. Validate 'outcome' input
  ## -------------------------------
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  ## Map outcome names to column numbers based on documentation
  outcome_col <- switch(outcome,
                        "heart attack" = 11,   # "Hospital 30-Day Death (Mortality) Rates from Heart Attack"
                        "heart failure" = 17,  # "Hospital 30-Day Death (Mortality) Rates from Heart Failure"
                        "pneumonia" = 23)      # "Hospital 30-Day Death (Mortality) Rates from Pneumonia"
  
  ## -------------------------------
  ## 4. Filter by state
  ## -------------------------------
  state_data <- data[data$State == state, ]
  
  ## -------------------------------
  ## 5. Convert outcome column to numeric (handle 'Not Available')
  ## -------------------------------
  state_data[, outcome_col] <- suppressWarnings(as.numeric(state_data[, outcome_col]))
  
  ## Remove rows with missing values
  state_data <- state_data[!is.na(state_data[, outcome_col]), ]
  
  ## -------------------------------
  ## 6. Find the hospital(s) with the lowest mortality rate
  ## -------------------------------
  min_rate <- min(state_data[, outcome_col], na.rm = TRUE)
  best_hospitals <- state_data[state_data[, outcome_col] == min_rate, "Hospital.Name"]
  
  ## -------------------------------
  ## 7. Handle ties — alphabetical order
  ## -------------------------------
  best_hospital <- sort(best_hospitals)[1]
  
  return(best_hospital)
}

## ============================================================
## Example calls
## ============================================================
# source("best.R")
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
