# ============================================================================== 
# Analysis Script for the Final Project
# Author(s): Jeff Grant and Paul Kim
# Date: 08-11-2015 
# Description: This code takes the previous csv files and uses them for analysis
# Data: "storms.csv" & "tracks.csv"
# ==============================================================================


#loading libraries and reading in the data
library(readr)
library(stringr)
library(ggplot2)
library(dplyr)

storms = read_csv("./data/storms.csv")
tracks = read_csv("./data/tracks.csv")

#Totaling the number of storms per year and plotting
tracks$year = as.numeric(str_extract(tracks$tracks_date, "[0-9]{4}"))

modern = tracks[32665:45735,]

year_total = aggregate(modern$tracks_wind, 
                     list(modern$tracks_id, modern$year), sum )
tapply(year_total$x, year_total$Group.2, length)

barplot(tapply(year_total$x, year_total$Group.2, length), 
        xlab = "Year", 
        ylab = "Number of Storms", 
        main = "Annual Frequency of Storms")
#getting the number of storms that are above 35 in each year and plotting
group = aggregate(modern$tracks_wind>=35, 
                list(modern$tracks_id, modern$year), sum)

group = group[group$x!=0,]

tapply( group$x, group$Group.2, length)

barplot(tapply( group$x, group$Group.2, length), 
        xlab = "Year", 
        ylab = "Number of Tropical Storms", 
        main = "Annual Frequency of Tropical Storms")

#getting the number of storms that are above 64 in each year and plotting
group1 = aggregate(modern$tracks_wind>=64, 
                 list(modern$tracks_id, modern$year), sum )

group1 = group1[group1$x!=0,]

tapply(group1$x, group1$Group.2, length)                 

barplot(tapply( group1$x, group1$Group.2, length), 
        xlab = "Year", 
        ylab = "Number of Hurricanes",
        main = "Annual Frquency of Hurricanes")

#getting the number of storms that are above 96 in each year and plotting
group2 = aggregate(modern$tracks_wind>=96, 
                 list(modern$tracks_id, modern$year), sum )

group2 = group2[group2$x!=0,]

tapply(group2$x, group2$Group.2, length)

barplot(tapply( group2$x, group2$Group.2, length), 
        xlab = "Year", 
        ylab = "Number of Stage 3 and Above Hurricanes",
        main = "Annual Frequency of Stage 3 Hurricanes")

#getting the number of storms in each month and plotting
modern$month = as.numeric(str_extract(modern$tracks_date, "[0-9]{2}"))

month = aggregate(modern$tracks_wind, 
                list(modern$tracks_id, modern$month), sum )

tapply(month$x, month$Group.2, length)

barplot(tapply( month$x, month$Group.2, length), 
        xlab = "Month", 
        ylab = "Number of Storms",
        main = "Monthly Frequency of Storms")

#getting the number of storms that are above 35 in each month and plotting
group3 = aggregate(modern$tracks_wind>=35
                 ,list(modern$tracks_id, modern$month), sum )

group3 = group3[group3$x!=0,]

tapply( group3$x, group3$Group.2, length)

barplot(tapply( group3$x, group3$Group.2, length), 
        xlab = "Month", 
        ylab = "Number of Tropical Storms",
        main = "Monthly Frequency of Tropical Storms")

#getting the number of storms that are above 64 in each month and plotting
group4 = aggregate(modern$tracks_wind>=64, 
                 list(modern$tracks_id, modern$month), sum )

group4 = group4[group4$x!=0,]

tapply( group4$x, group4$Group.2, length)

barplot(tapply( group4$x, group4$Group.2, length), 
        xlab = "Month", 
        ylab = "Number of Hurricanes",
        main = "Monthly Frequency of Hurricanes")

#getting the number of storms that are above 96 in each month and plotting
group5 = aggregate(modern$tracks_wind>=96, 
                 list(modern$tracks_id, modern$month), sum )

group5 = group5[group5$x!=0,]

tapply( group5$x, group5$Group.2, length)

barplot(tapply( group5$x, group5$Group.2, length), 
        xlab = "Month", 
        ylab = "Number of Stage 3 and Above Hurricanes",
        main = "Monthly Frequency of Stage 3 and Above Hurricanes")

#Creating the summary statistics for the 3 storm types

#Summary statistics for Tropical Storms
trop_storm = (tapply( group$x, group$Group.2, length))

#average value for hurricanes
mean(trop_storm)
mean_trop = as.numeric(strtrim(as.character(mean(trop_storm)), width = 4))

#standard deviation for hurricanes
sd(trop_storm)
sd_trop = round(sd(trop_storm), 2)

#median for hurricanes
median(trop_storm)
median_trop = median(trop_storm)

#1st quartile for hurricanes
as.numeric(summary(trop_storm)[2])
onequart_trop = as.numeric(summary(trop_storm)[2])

#3rd quartile for hurricanes
as.numeric(summary(trop_storm)[5])
threequart_trop = as.numeric(summary(trop_storm)[5])


#Summary statistics for Hurricanes
trop_hurr = (tapply( group1$x, group1$Group.2, length))

#average value for hurricanes
mean(trop_hurr)
mean_hurr = round(mean(trop_hurr), 1)

#standard deviation for hurricanes
sd(trop_hurr)
sd_hurr = as.numeric(strtrim(as.character(sd(trop_hurr)), width = 4))

#median for hurricanes
median(trop_hurr)
median_hurr= median(trop_hurr)

#1st quartile for hurricanes
as.numeric(summary(trop_hurr)[2])
onequart_hurr=as.numeric(summary(trop_hurr)[2])

#3rd quartile for hurricanes
as.numeric(summary(trop_hurr)[5])
threequart_hurr=as.numeric(summary(trop_hurr)[5])

#Summary statistics for Hurricanes (stage 3)
trop_hurr3 = (tapply( group2$x, group2$Group.2, length))

#average value for hurricanes
mean(trop_hurr3)
mean_hurr3 = as.numeric(strtrim(as.character(mean(trop_hurr3)), width = 3))

#standard deviation for hurricanes
sd(trop_hurr3)
sd_hurr3 = as.numeric(strtrim(as.character(sd(trop_hurr3)), width = 4))

#median for hurricanes
median(trop_hurr3)
median_hurr3=median(trop_hurr3)

#1st quartile for hurricanes
as.numeric(summary(trop_hurr3)[2])
onequart_hurr3=as.numeric(summary(trop_hurr3)[2])

#3rd quartile for hurricanes
as.numeric(summary(trop_hurr3)[5])
threequart_hurr3 = as.numeric(summary(trop_hurr3)[5])

#Table of summary statistics
storms_summary = data.frame(
  "Avg" = c(mean_trop, 
            mean_hurr, 
            mean_hurr3),
  "Std Dev" = c(sd_trop, 
                sd_hurr, 
                sd_hurr3),
  "First Quartile" = c(onequart_trop, 
                       onequart_hurr,
                       onequart_hurr3),
  "Median"= c(median_trop, 
              median_hurr, 
              median_hurr3),
  "Third Quartile" = c(threequart_trop,
                       threequart_hurr,
                       threequart_hurr3),
          row.names = c("35 Knots", "64 Knots", "96 Knots")
)

storms_summary

#Creating png files for each of the plots
png("./images/storms_annual.png")
barplot(tapply(year_total$x, year_total$Group.2, length), 
        xlab = "Year", 
        ylab = "Number of Storms", 
        main = "Annual Frequency of Storms")
dev.off()   
png("./images/trop_annual.png")
barplot(tapply( group$x, group$Group.2, length), 
        xlab = "Year", 
        ylab = "Number of Tropical Storms", 
        main = "Annual Frequency of Tropical Storms")
dev.off()
png("./images/hurr_annual.png")
barplot(tapply( group1$x, group1$Group.2, length), 
        xlab = "Year", 
        ylab = "Number of Hurricanes",
        main = "Annual Frquency of Hurricanes")
dev.off()
png("./images/hurr3_annual.png")
barplot(tapply( group2$x, group2$Group.2, length), 
        xlab = "Year", 
        ylab = "Number of Stage 3 and Above Hurricanes",
        main = "Annual Frequency of Stage 3 Hurricanes")
dev.off()
png("./images/storms_monthly.png")
barplot(tapply( month$x, month$Group.2, length), 
        xlab = "Month", 
        ylab = "Number of Storms",
        main = "Monthly Frequency of Storms")
dev.off()
png("./images/trop_monthly.png")
barplot(tapply( group3$x, group3$Group.2, length), 
        xlab = "Month", 
        ylab = "Number of Tropical Storms",
        main = "Monthly Frequency of Tropical Storms")
dev.off()
png("./images/hurr_monthly.png")
barplot(tapply( group4$x, group4$Group.2, length), 
        xlab = "Month", 
        ylab = "Number of Hurricanes",
        main = "Monthly Frequency of Hurricanes")
dev.off()
png("./images/hurr3_monthly.png")
barplot(tapply( group5$x, group5$Group.2, length), 
        xlab = "Month", 
        ylab = "Number of Stage 3 and Above Hurricanes",
        main = "Monthly Frequency of Stage 3 and Above Hurricanes")
dev.off()


#Creating pdf files for each of the plots
pdf("./images/storms_annual.pdf")
barplot(tapply(year_total$x, year_total$Group.2, length), 
        xlab = "Year", 
        ylab = "Number of Storms", 
        main = "Annual Frequency of Storms")
dev.off()   
pdf("./images/trop_annual.pdf")
barplot(tapply( group$x, group$Group.2, length), 
        xlab = "Year", 
        ylab = "Number of Tropical Storms", 
        main = "Annual Frequency of Tropical Storms")
dev.off()
pdf("./images/hurr_annual.pdf")
barplot(tapply( group1$x, group1$Group.2, length), 
        xlab = "Year", 
        ylab = "Number of Hurricanes",
        main = "Annual Frquency of Hurricanes")
dev.off()
pdf("./images/hurr3_annual.pdf")
barplot(tapply( group2$x, group2$Group.2, length), 
        xlab = "Year", 
        ylab = "Number of Stage 3 and Above Hurricanes",
        main = "Annual Frequency of Stage 3 Hurricanes")
dev.off()
pdf("./images/storms_monthly.pdf")
barplot(tapply( month$x, month$Group.2, length), 
        xlab = "Month", 
        ylab = "Number of Storms",
        main = "Monthly Frequency of Storms")
dev.off()
pdf("./images/trop_monthly.pdf")
barplot(tapply( group3$x, group3$Group.2, length), 
        xlab = "Month", 
        ylab = "Number of Tropical Storms",
        main = "Monthly Frequency of Tropical Storms")
dev.off()
pdf("./images/hurr_monthly.pdf")
barplot(tapply( group4$x, group4$Group.2, length), 
        xlab = "Month", 
        ylab = "Number of Hurricanes",
        main = "Monthly Frequency of Hurricanes")
dev.off()
pdf("./images/hurr3_monthly.pdf")
barplot(tapply( group5$x, group5$Group.2, length), 
        xlab = "Month", 
        ylab = "Number of Stage 3 and Above Hurricanes",
        main = "Monthly Frequency of Stage 3 and Above Hurricanes")
dev.off()



##Regression Analysis
#mean pressure on mean wind
mean_pressures = aggregate(modern[, "tracks_pressure"], list(modern$tracks_id), mean)
mean_winds = aggregate(modern[, "tracks_wind"], list(modern$tracks_id), mean)
mean_pressure_wind = merge(mean_pressures, mean_winds, by = "Group.1")
mean_pressure_wind = filter(mean_pressure_wind, tracks_pressure > 0)
mean_pressure_wind = mean_pressure_wind[, c(2, 3)]

mean_pressure_wind_reg = lm(tracks_pressure ~ tracks_wind, data = mean_pressure_wind)
mean_pressure_wind_plot = ggplot(mean_pressure_wind, aes(x = tracks_pressure, y = tracks_wind)) + 
  geom_point() + geom_smooth(method = "lm") + xlab("Pressure") + ylab("Wind") +
  ggtitle("Mean Pressure vs Mean Wind")

#Outliers skew the regression line. Removing outliers:
mean_pressure_wind_clean = filter(mean_pressure_wind, tracks_pressure > 950)
mean_pressure_wind_clean_reg = lm(tracks_pressure ~ tracks_wind, data = mean_pressure_wind_clean)
mean_pressure_wind_clean_plot = ggplot(mean_pressure_wind_clean, aes(x = tracks_pressure, y = tracks_wind)) + 
  geom_point() + geom_smooth(method = "lm") + xlab("Pressure") + ylab("Wind") +
  ggtitle("Mean Pressure vs Mean Wind, outliers removed")

#median pressure on median wind
median_pressures = aggregate(modern[, "tracks_pressure"], list(modern$tracks_id), median)
median_winds = aggregate(modern[, "tracks_wind"], list(modern$tracks_id), median)
median_pressure_wind = merge(median_pressures, median_winds, by = "Group.1")
median_pressure_wind = filter(median_pressure_wind, tracks_pressure > 0)
median_pressure_wind = median_pressure_wind[, c(2, 3)]

median_pressure_wind_reg = lm(tracks_pressure ~ tracks_wind, data = median_pressure_wind)
median_pressure_wind_plot = ggplot(median_pressure_wind, aes(x = tracks_pressure, y = tracks_wind)) + 
  geom_point() + geom_smooth(method = "lm") + xlab("Pressure") + ylab("Wind") +
  ggtitle("Median Pressure vs Median Wind")

#png's and pdf's of regression plots:
png("./images/mean_pressure_wind.png")
mean_pressure_wind_plot
dev.off()
png("./images/mean_pressure_wind_clean.png")
mean_pressure_wind_clean_plot
dev.off()
png("./images/median_pressure_wind.png")
median_pressure_wind_plot
dev.off()

pdf("./images/mean_pressure_wind.pdf")
mean_pressure_wind_plot
dev.off()
pdf("./images/mean_pressure_wind_clean.pdf")
mean_pressure_wind_clean_plot
dev.off()
pdf("./images/median_pressure_wind.pdf")
median_pressure_wind_plot
dev.off()

