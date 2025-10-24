########################################################################
# Course 4: Electric power consuption Proyect 
#
# Purpose: Create plots from the data (plot1.png, plot2.png, plot3.png, plot4.png (each 480x480))
# Assumptions:
# - The zip file "exdata_data_household_power_consumption.zip" is present
#   in the working directory or the unzipped file
#   "household_power_consumption.txt" is already present.
# - Missing values are coded as "?" in the file (we convert them to NA).
#
# Usage:
# - set your working directory to the folder containing the zip/file
#   or run this script from that folder.
#
# Author: Moonmar
########################################################################
setwd("C:\\Users\\ladym\\Downloads")

zipfile <- "exdata_data_household_power_consumption.zip"
txtfile <- "household_power_consumption.txt"

# -----------------------------------
# --- ESTIMATE MEMORY REQUIREMENT ---
# -----------------------------------
# The dataset has 2,075,259 rows and 9 columns (as per description).
nrows_est <- 2075259
ncols <- 9
# Numeric values in R are typically 8 bytes each. 
mem_bytes <- nrows_est * ncols * 8
mem_MB <- mem_bytes / 1024^2
cat(sprintf("Estimated memory for full dataset: %.1f MB\n", mem_MB)) #~150MB
# Note: this is an estimate for numeric-only data (Text columns and overhead)

# ---------
# 1) UNZIP
# ---------
if (!file.exists(txtfile) && file.exists(zipfile)) {
  cat("Unzipping data...\n")
  unzip(zipfile)
}
if (!file.exists(txtfile)) {
  stop("Data file not found. Please place 'household_power_consumption.txt' or the .zip in the working directory.")
}

# -----------------------------------------------------------
# 2) READ ONLY THE 2 TARGET DATES (2007-02-01 and 2007-02-02)
# -----------------------------------------------------------
# However, pipe("grep ...") may not work on Windows Rterm without grep installed.
# We'll try the grep approach first and fall back to reading & subsetting
# using data.table::fread (if available) or read.table (last resort).

# Read header (column names) first 
header <- read.table(txtfile, header = TRUE, sep = ";", nrows = 1, stringsAsFactors = FALSE)
col_names <- names(header)

date_pattern <- "^[1|2]/2/2007" # Date pattern in file: d/m/YYYY 

use_grep <- FALSE #Use OS grep to extract relevant lines (fast & memory efficient):looks through a text file line by line and outputs only those lines that match a given pattern.
data <- NULL

try({
  # Construct a shell command that finds lines starting with "1/2/2007" or "2/2/2007"
  # The caret ^ anchors to start of line to match the Date column.
  cmd <- paste0("grep '", date_pattern, "' ", txtfile)
  con <- pipe(cmd)
  # If pipe opens successfully we attempt to read; on Windows this may fail.
  data <- read.table(con, sep = ";", header = FALSE, col.names = col_names, na.strings = "?", stringsAsFactors = FALSE)
  close(con)
  use_grep <- TRUE
}, silent = TRUE)

if (!use_grep) {
  cat("grep method unavailable or failed — using fallback method.\n")
  # Fallback 1: use data.table::fread if available (fast)
  if (requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
    cat("Reading with data.table::fread and subsetting (fast fallback)...\n")
    dt <- data.table::fread(txtfile, sep = ";", na.strings = "?", header = TRUE, showProgress = FALSE)
    # Convert Date column (strings like "16/12/2006") to Date class and subset
    dt[, Date2 := as.Date(Date, format = "%d/%m/%Y")]
    sel <- dt$Date2 >= as.Date("2007-02-01") & dt$Date2 <= as.Date("2007-02-02")
    data <- as.data.frame(dt[sel, setdiff(names(dt), "Date2"), with = FALSE])
    rm(dt)
  } else {
    # Fallback 2: read full file with read.table (may be slow / memory heavy)
    cat("data.table not installed. Reading full dataset with read.table (may be slow)...\n")
    full <- read.table(txtfile, header = TRUE, sep = ";", na.strings = "?", stringsAsFactors = FALSE)
    full$Date2 <- as.Date(full$Date, format = "%d/%m/%Y")
    data <- subset(full, Date2 >= as.Date("2007-02-01") & Date2 <= as.Date("2007-02-02"))
    # cleanup
    rm(full)
  }
}

# Check if we have the data
if (is.null(data) || nrow(data) == 0) {
  stop("No data was read for the dates 2007-02-01 and 2007-02-02. Check that the file exists and the date format matches.")
}
cat(sprintf("Rows read for 2007-02-01 and 2007-02-02: %d\n", nrow(data)))

# -------------------------------------------
# 3)CONVERT DATE & TIME COLUMNS TO R DATETIME
# -------------------------------------------
# Original Date format: dd/mm/yyyy, Time format: hh:mm:ss
data$DateTime <- as.POSIXct(strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")) #create a POSIXct 'DateTime' column for plotting.

num_cols <- c("Global_active_power", "Global_reactive_power", "Voltage",
              "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

#Ensure numeric columns are numeric
for (nc in num_cols) {
  if (!is.numeric(data[[nc]])) {
    data[[nc]] <- as.numeric(data[[nc]])
  }
}

axis_dates <- seq(from = as.POSIXct("2007-02-01 00:00:00"),
                  to   = as.POSIXct("2007-02-03 00:00:00"),
                  by   = "day")

axis_labels <- c("Thu", "Fri", "Sat")
# -------------------------------------------
# 4) PLOT 1: Histogram of Global Active Power
# -------------------------------------------
png(filename = "plot1.png", width = 480, height = 480) # Save to PNG 480x480.
hist(data$Global_active_power,
     col = "red",
     main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)",
     ylab = "Frequency")  
dev.off()
cat("Created plot1.png\n")

# ----------------------------------------
# 5) PLOT 2: Global Active Power over time
# ----------------------------------------
png(filename = "plot2.png", width = 480, height = 480)
plot(data$DateTime, data$Global_active_power,
     type = "l",
     xlab = "",
     xaxt = "n", #remove atomatic lables
     ylab = "Global Active Power (kilowatts)")
axis(1, at = axis_dates, labels = axis_labels)
dev.off()
cat("Created plot2.png\n")

# ------------------------------
# 6) PLOT 3: Energy sub-metering 
# ------------------------------
png(filename = "plot3.png", width = 480, height = 480)
plot(data$DateTime, data$Sub_metering_1, 
     type = "l", 
     xlab = "",
     xaxt = "n", 
     ylab = "Energy sub metering")
axis(1, at = axis_dates, labels = axis_labels)

lines(data$DateTime, data$Sub_metering_2, col = "red")
lines(data$DateTime, data$Sub_metering_3, col = "blue")
legend("topright",
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black", "red", "blue"),
       lty = 1)
dev.off()
cat("Created plot3.png\n")

# ------------------------------------------------------------------------------------------------
# 8) PLOT 4: 2x2 panel with: Global Active Power,Voltage,Energy sub metering,Global Reactive Power
# ------------------------------------------------------------------------------------------------
png(filename = "plot4.png", width = 480, height = 480)
par(mfrow = c(2, 2))

# Top-left: Global Active Power
plot(data$DateTime, data$Global_active_power, 
     type = "l", 
     xlab = "",
     xaxt = "n",
     ylab = "Global Active Power")
axis(1, at = axis_dates, labels = axis_labels)

# Top-right: Voltage
plot(data$DateTime, data$Voltage, 
     type = "l", 
     xlab = "datetime", 
     xaxt = "n",
     ylab = "Voltage")
axis(1, at = axis_dates, labels = axis_labels)

# Bottom-left: Energy sub metering 
plot(data$DateTime, data$Sub_metering_1, 
     type = "l", 
     xlab = "", 
     xaxt = "n",
     ylab = "Energy sub metering")
axis(1, at = axis_dates, labels = axis_labels)
lines(data$DateTime, data$Sub_metering_2, col = "red")
lines(data$DateTime, data$Sub_metering_3, col = "blue")

legend("topright",
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black", "red", "blue"), lty = 1, cex = 0.9) #smaller legend

# Bottom-right: Global Reactive Power
plot(data$DateTime, data$Global_reactive_power, 
     type = "l", 
     xlab = "datetime", 
     xaxt ="n",
     ylab = "Global_reactive_power")
axis(1, at = axis_dates, labels = axis_labels)
dev.off()
cat("Created plot4.png\n")
