# Course 3:Getting and Cleaning Data Project

This repository contains the R script and documentation for the Coursera *Getting and Cleaning Data* course project.\
The goal is to demonstrate how to collect, clean, and create a tidy dataset from the **Human Activity Recognition Using Smartphones** dataset.

## Files in this repository

-   `run_analysis.R` – R script that performs the data cleaning and transformation.
-   `CodeBook.md` – Describes the variables, the data, and the transformations performed.
-   `TidyData.txt` – The final tidy dataset (output file from `run_analysis.R`).

## How the script works

The script `run_analysis.R` performs the following steps:

### 1. Download and unzip the dataset

If the dataset is not found in your working directory, the script downloads the ZIP file from: [UCI HAR Dataset](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

It then unzips the folder `UCI HAR Dataset`.

### 2. Load data into R

The script reads: - **features.txt** – names of all measured features\
- **activity_labels.txt** – links activity codes to descriptive names\
- **train/** and **test/** datasets: - `X_train.txt`, `y_train.txt`, `subject_train.txt` - `X_test.txt`, `y_test.txt`, `subject_test.txt`

### 3. Merge the training and test datasets

The training and test sets are combined using `rbind()` and `cbind()` to create one complete dataset with all observations.

### 4. Extract mean and standard deviation measurements

Using the `dplyr` package, the script filters columns that contain `mean` or `std` in their names.\
This keeps only measurements on the mean and standard deviation for each variable.

### 5. Use descriptive activity names

The activity codes (numbers) are replaced by their descriptive activity names (e.g., `WALKING`, `SITTING`, `LAYING`).

### 6. Label dataset with descriptive variable names

The variable names are cleaned and expanded for readability: - `Acc` → `Accelerometer` - `Gyro` → `Gyroscope` - `Mag` → `Magnitude` - `t` prefix → `Time` - `f` prefix → `Frequency` - `mean()` → `Mean` - `std()` → `STD`

### 7. Create a tidy dataset with averages

The script groups data by `subject` and `activity`, and calculates the **mean of each variable** for each pair.\
This produces a tidy dataset where each row represents one subject–activity combination.

### 8. Output the final dataset

The final tidy dataset is written to a text file: You can load it back into R with:\
`r tidy \<-read.table("TidyData.txt", header = TRUE) View(tidy)`
