# ============================================================================== 
# Visualization Cleaning Script for the Final Project
# Author(s): Jeff Grant and Paul Kim
# Date: 08-11-2015 
# Description: This code takes the raw storm csv data and cleans it in preparation
#              for use in visualizations.
# Data: "raw_NA.csv" & "raw_EP.csv"
# ==============================================================================

#loading libraries and reading in the data
library(dplyr)
library(stringr)

raw_NA = read.csv(file = "./rawdata/raw_NA.csv", header = FALSE, skip = 3, col.names = 
                    c("Serial_Number", "Season", "Num", "Basin", "Sub_Basin", "Name", "Time",
                      "Nature", "Lat", "Long", "Wind", "Pressure", "Center", "Wind_Percentile",
                      "Pressure_Percentile", "Track_Type"))

raw_EP = read.csv(file = "./rawdata/raw_EP.csv", header = FALSE, skip = 3, col.names = 
                    c("Serial_Number", "Season", "Num", "Basin", "Sub_Basin", "Name", "Time",
                      "Nature", "Lat", "Long", "Wind", "Pressure", "Center", "Wind_Percentile",
                      "Pressure_Percentile", "Track_Type"))

#Isolating storms between 1980 and 2010
clean_NA = filter(raw_NA, Season >= 1980, Season <= 2010)
clean_EP = filter(raw_EP, Season >= 1980, Season <= 2010)

##Combining NA and EP data
combined = rbind(clean_NA, clean_EP)

#Formatting changes
combined$Time = as.character(combined$Time)
combined$year = combined$Season

##adding decade factor
combined = mutate(combined, decade = as.factor(ifelse(year < 1990, "1980s",
                                           ifelse(year < 2000, "1990s",
                                                  "2000s"))))

##Adding month factor
combined = mutate(combined, month = as.numeric(str_extract(str_extract(Time,
              pattern = "-[0-9][0-9]-"), pattern = "[0-9][0-9]")))
month = str_extract(str_extract(combined$Time, pattern = "-[0-9][0-9]-"), pattern = "[0-9][0-9]")


combined = mutate(combined, month = as.factor(ifelse(month == 1, "January",
                                              ifelse(month == 2, "February", 
                                              ifelse(month == 3, "March", 
                                              ifelse(month == 4, "April", 
                                              ifelse(month == 5, "May", 
                                              ifelse(month == 6, "June", 
                                              ifelse(month == 7, "July", 
                                              ifelse(month == 8, "August", 
                                              ifelse(month == 9, "September", 
                                              ifelse(month == 10, "October", 
                                              ifelse(month == 11, "November", 
                                              "December")))))))))))))

##Ordering the month factors
combined$month = factor(combined$month, levels = c("January", "February", "March", "April", "May", "June",
                                                   "July", "August", "September", "October", "November",
                                                   "December"))

#Making the final data table with the required data.
clean = select(combined, Serial_Number, Lat, Long, Wind, month, year, decade)

#Exporting the cleaned data
write.csv(clean, file = "./data/visualization_clean.csv", row.names = FALSE)
