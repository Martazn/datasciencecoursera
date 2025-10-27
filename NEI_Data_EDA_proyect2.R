setwd("C:\\Users\\ladym\\Downloads")


# 1. Unzip the dataset
zipfile <- "exdata_data_NEI_data.zip"
unzip(zipfile, exdir = "data")

# 2. Read the data files
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# 3. Summarize total emissions by year
total_by_year <- aggregate(Emissions ~ year, NEI, sum)

# -------
# plot1.R
# -------
## Question: Have total PM2.5 emissions in USA decreased from 1999 to 2008? Yes

# Create a PNG file for the plot
png("plot1.png", width = 480, height = 480)

# Plot total emissions by year using base R
barplot(
  total_by_year$Emissions,
  names.arg = total_by_year$year,
  col = "steelblue",
  xlab = "Year",
  ylab = "Total PM2.5 Emissions (tons)",
  main = "Total PM2.5 Emissions in the United States (1999–2008)"
)

# Labels above each bar
text(
  x = seq_along(total_by_year$Emissions),
  y = total_by_year$Emissions,
  labels = round(total_by_year$Emissions, 0),
  pos = 3, cex = 0.8
)
dev.off()
cat("Created plot1.png\n")


# -------
# plot2.R
# -------
## Question: Have total PM2.5 emissions in Baltimore City decreased from 1999 to 2008? Yes

# Subset data for Baltimore City, Maryland (fips == "24510")
baltimore_data <- subset(NEI, fips == "24510")

# Aggregate total emissions by year
total_by_year <- aggregate(Emissions ~ year, baltimore_data, sum)

png("plot2.png", width = 480, height = 480)
barplot(
  total_by_year$Emissions,
  names.arg = total_by_year$year,
  col = "tomato",
  xlab = "Year",
  ylab = "Total PM2.5 Emissions (tons)",
  main = "Total PM2.5 Emissions in Baltimore City (1999–2008)"
)
text(
  x = seq_along(total_by_year$Emissions),
  y = total_by_year$Emissions,
  labels = round(total_by_year$Emissions, 1),
  pos = 3, cex = 0.8
)
dev.off()
cat("Created plot2.png\n")

# -------
# plot3.R
# -------
## Question: Of the four source types, which have seen decreases or increases
# in emissions from 1999–2008 for Baltimore City? 

library(ggplot2)
# Filter for Baltimore City (fips == "24510")
baltimore_data <- subset(NEI, fips == "24510")

# Aggregate total emissions by year and source type
library(dplyr)
baltimore_summary <- baltimore_data %>%
  group_by(year, type) %>%
  summarise(Emissions = sum(Emissions), .groups = "drop")

png("plot3.png", width = 700, height = 500)

ggplot(baltimore_summary, aes(x = factor(year), y = Emissions, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "PM2.5 Emissions in Baltimore City (1999–2008) by Source Type",
    x = "Year",
    y = "Total PM2.5 Emissions (tons)",
    fill = "Source Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
dev.off()
cat("Created plot3.png\n")

# -------
# plot4.R
# -------
## Question:Across the United States, how have emissions from
# coal combustion-related sources changed from 1999–2008? Decreased

# We look for 'coal' and 'comb' keywords in SCC classification fields
coal_combustion <- grepl("coal", SCC$Short.Name, ignore.case = TRUE) &
  grepl("comb", SCC$Short.Name, ignore.case = TRUE)
coal_SCC <- SCC[coal_combustion, "SCC"]
coal_NEI <- subset(NEI, SCC %in% coal_SCC) #Filter NEI data for coal-related SCCs
total_coal_by_year <- aggregate(Emissions ~ year, coal_NEI, sum)

png("plot4.png", width = 480, height = 480)
barplot(
  total_coal_by_year$Emissions,
  names.arg = total_coal_by_year$year,
  col = "darkblue",
  xlab = "Year",
  ylab = "Total PM2.5 Emissions (tons)",
  main = "Coal Combustion-related PM2.5 Emissions in the U.S. (1999–2008)"
)
text(
  x = seq_along(total_coal_by_year$Emissions),
  y = total_coal_by_year$Emissions,
  labels = round(total_coal_by_year$Emissions, 0),
  pos = 3, cex = 0.8
)
dev.off()
cat("Created plot4.png\n")

# -------
# plot5.R
# -------
## Question:How have emissions from motor vehicle sources changed
# from 1999–2008 in Baltimore City? Decreased

baltimore_motor <- subset(NEI, fips == "24510" & type == "ON-ROAD")
motor_by_year <- aggregate(Emissions ~ year, baltimore_motor, sum)

png("plot5.png", width = 480, height = 480)
barplot(
  motor_by_year$Emissions,
  names.arg = motor_by_year$year,
  col = "mediumseagreen",
  xlab = "Year",
  ylab = "Motor Vehicle PM2.5 Emissions (tons)",
  main = "Motor Vehicle PM2.5 Emissions in Baltimore City (1999–2008)"
)

# Add numeric labels on bars
text(
  x = seq_along(motor_by_year$Emissions),
  y = motor_by_year$Emissions,
  labels = round(motor_by_year$Emissions, 2),
  pos = 3, cex = 0.8
)
dev.off()
cat("Created plot5.png\n")

# -------
# plot6.R
# -------
## Question:Compare motor vehicle emissions in Baltimore City and Los Angeles County (1999–2008). IN LA
library(ggplot2)
library(dplyr)

motor_data <- subset(NEI, type == "ON-ROAD" & fips %in% c("24510", "06037"))
motor_summary <- motor_data %>%
  group_by(year, fips) %>%
  summarise(Emissions = sum(Emissions), .groups = "drop")
motor_summary$city <- ifelse(motor_summary$fips == "24510",
                             "Baltimore City, MD", "Los Angeles County, CA")

png("plot6.png", width = 700, height = 500)
ggplot(motor_summary, aes(x = factor(year), y = Emissions, color = city, group = city)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Motor Vehicle PM2.5 Emissions (1999–2008): Baltimore vs Los Angeles",
    x = "Year",
    y = "Total PM2.5 Emissions (tons)",
    color = "City"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
dev.off()
cat("Created plot6.png\n")
