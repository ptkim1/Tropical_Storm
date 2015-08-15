# ============================================================================== 
# Source Script for the Final Project (analysis.R without plots)
# Author(s): Jeff Grant and Paul Kim
# Date: 08-11-2015 
# Description: This code takes the previous csv files and uses them for analysis
# Data: "storms.csv" & "tracks.csv"
# ==============================================================================


#loading libraries and reading in the data
library(readr)
library(stringr)
storms = read_csv("./data/storms.csv")
tracks = read_csv("./data/tracks.csv")

#Totaling the number of storms per year and plotting
tracks$year = as.numeric(str_extract(tracks$tracks_date, "[0-9]{4}"))

modern = tracks[32665:45735,]

year_total = aggregate(modern$tracks_wind, 
                       list(modern$tracks_id, modern$year), sum )
tapply(year_total$x, year_total$Group.2, length)


#getting the number of storms that are above 35 in each year and plotting
group = aggregate(modern$tracks_wind>=35, 
                  list(modern$tracks_id, modern$year), sum)

group = group[group$x!=0,]

tapply( group$x, group$Group.2, length)


#getting the number of storms that are above 64 in each year and plotting
group1 = aggregate(modern$tracks_wind>=64, 
                   list(modern$tracks_id, modern$year), sum )

group1 = group1[group1$x!=0,]

tapply(group1$x, group1$Group.2, length)                 



#getting the number of storms that are above 96 in each year and plotting
group2 = aggregate(modern$tracks_wind>=96, 
                   list(modern$tracks_id, modern$year), sum )

group2 = group2[group2$x!=0,]

tapply(group2$x, group2$Group.2, length)


#getting the number of storms in each month and plotting
modern$month = as.numeric(str_extract(modern$tracks_date, "[0-9]{2}"))

month = aggregate(modern$tracks_wind, 
                  list(modern$tracks_id, modern$month), sum )

tapply(month$x, month$Group.2, length)



#getting the number of storms that are above 35 in each month and plotting
group3 = aggregate(modern$tracks_wind>=35
                   ,list(modern$tracks_id, modern$month), sum )

group3 = group3[group3$x!=0,]

tapply( group3$x, group3$Group.2, length)



#getting the number of storms that are above 64 in each month and plotting
group4 = aggregate(modern$tracks_wind>=64, 
                   list(modern$tracks_id, modern$month), sum )

group4 = group4[group4$x!=0,]

tapply( group4$x, group4$Group.2, length)



#getting the number of storms that are above 96 in each month and plotting
group5 = aggregate(modern$tracks_wind>=96, 
                   list(modern$tracks_id, modern$month), sum )

group5 = group5[group5$x!=0,]

tapply( group5$x, group5$Group.2, length)


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



