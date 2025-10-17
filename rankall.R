setwd("C:\\Users\\ladym\\Downloads\\rprog_data_ProgAssignment3-data")
## ============================================================
## Function: rankall
## Purpose: For each state, return the hospital that has the
##          specified rank (best, worst, or numeric) for a given outcome
## ============================================================

rankall <- function(outcome, num = "best") {
  ## -------------------------------
  ## 1. Read outcome data
  ## -------------------------------
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## -------------------------------
  ## 2. Validate 'outcome' input
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
  ## 3. Prepare the data
  ## -------------------------------
  ## Column 2 = Hospital Name, Column 7 = State, Outcome = chosen column
  data_subset <- data[, c(2, 7, outcome_col)]
  names(data_subset) <- c("Hospital.Name", "State", "Outcome")
  
  ## -------------------------------
  ## 4. Convert outcome to numeric and drop NA
  ## -------------------------------
  data_subset$Outcome <- suppressWarnings(as.numeric(data_subset$Outcome))
  data_subset <- data_subset[!is.na(data_subset$Outcome), ]
  
  ## -------------------------------
  ## 5. Split data by state
  ## -------------------------------
  state_groups <- split(data_subset, data_subset$State)
  
  ## -------------------------------
  ## 6. Rank hospitals within each state
  ## -------------------------------
  result <- lapply(state_groups, function(state_data) {
    ordered <- state_data[order(state_data$Outcome, state_data$Hospital.Name), ]
    
    ## Determine the rank
    if (num == "best") {
      rank <- 1
    } else if (num == "worst") {
      rank <- nrow(ordered)
    } else if (is.numeric(num)) {
      rank <- as.numeric(num)
    } else {
      stop("invalid rank")
    }
    
    ## If rank is higher than total hospitals, return NA
    if (rank > nrow(ordered)) {
      hospital <- NA
    } else {
      hospital <- ordered$Hospital.Name[rank]
    }
    
    ## Return a small data frame for this state
    data.frame(hospital = hospital, state = state_data$State[1])
  })
  
  ## -------------------------------
  ## 7. Combine all states and sort by state name
  ## -------------------------------
  result_df <- do.call(rbind, result)
  result_df <- result_df[order(result_df$state), ]
  rownames(result_df) <- NULL

  return(result_df)
}
## ============================================================
## Example calls 
## ============================================================
# source("rankall.R")
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
