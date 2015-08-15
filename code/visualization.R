# ============================================================================== 
# Visualization Script for the Final Project
# Author(s): Jeff Grant and Paul Kim
# Date: 08-11-2015 
# Description: This code takes the previous csv files and uses them for analysis
#              It also exports png and pdf files of the graphics created. 
# Data: "visualization_clean.csv"
# ==============================================================================

#loading libraries and reading in the data
library(ggplot2)
library(maps)
library(mapproj)
library(mapdata)
library(dplyr)

storm_data = read.csv("data/visualization_clean.csv", header = TRUE)

#Re-factoring and ordering the month column
storm_data$month = factor(storm_data$month, 
                    levels = c("January", "February", "March", "April", "May", "June",
                               "July", "August", "September", "October", "November",
                               "December"))

#Getting map data for pacific and atlantic regions
pacific_map = filter(map_data("world2"), long > 100, long < 280, lat > -30)
atlantic_map = filter(map_data("world"), long > -120, long < 30, lat > -10)

#splitting storm_data into pacific and atlantic data sets
pacific_data = storm_data

#re-centering pacific_data to align with pacific_map
pacific_data$Long[which(pacific_data$Long < 0)] = pacific_data$Long[which(pacific_data$Long < 0)] + 360

#isolating pacific storms
pacific_data = filter(pacific_data, Long > 100, Long < 280, Lat > -50)

#isolating atlantic storms (no alignment required - already centered at prime meridian)
atlantic_data = storm_data
atlantic_data = filter(atlantic_data, Long > -120, Long < 30, Lat > -10)

#making plots

#all pacific and atlantic storms
all_pacific = ggplot(pacific_data) + 
  geom_map(dat = pacific_map, map = pacific_map, 
          aes(map_id = region, x = long, y = lat), 
          fill = "#FFF5EE", color = "#4682B4", size = 0.25) +
  geom_path(aes(x = Long, y = Lat, group = Serial_Number), size = .1, color = "#FF6346") +
  xlab("Latitude") + ylab("Longitude") + ggtitle("All Pacific Ocean Storms")

all_atlantic = ggplot(atlantic_data) + 
  geom_map(dat = atlantic_map, map = atlantic_map, 
           aes(map_id = region, x = long, y = lat), 
           fill = "#FFF5EE", color = "#4682B4", size = 0.25) +
  geom_path(aes(x = Long, y = Lat, group = Serial_Number), size = .1, color = "#FF6346") +
  xlab("Latitude") + ylab("Longitude") + ggtitle("All Atlantic Ocean Storms")

#storms by month
pacific_monthly = ggplot(pacific_data) + 
  geom_map(dat = pacific_map, map = pacific_map, 
           aes(map_id = region, x = long, y = lat), 
           fill = "#FFF5EE", color = "#4682B4", size = 0.25) +
  geom_path(aes(x = Long, y = Lat, group = Serial_Number), size = .1, color = "#FF6346") +
  facet_wrap(~ month) + xlab("Latitude") + ylab("Longitude") + ggtitle("Pacific Storms by Month")

atlantic_monthly = ggplot(atlantic_data) + 
  geom_map(dat = atlantic_map, map = atlantic_map, 
           aes(map_id = region, x = long, y = lat), 
           fill = "#FFF5EE", color = "#4682B4", size = 0.25) +
  geom_path(aes(x = Long, y = Lat, group = Serial_Number), size = .1, color = "#FF6346") +
  facet_wrap(~ month) + xlab("Latitude") + ylab("Longitude") + ggtitle("Atlantic Storms by Month")

#storms by decade
pacific_80s = ggplot(filter(pacific_data, decade == "1980s")) +
  geom_map(dat = pacific_map, map = pacific_map, 
           aes(map_id = region, x = long, y = lat), 
           fill = "#FFF5EE", color = "#4682B4", size = 0.25) +
  geom_path(aes(x = Long, y = Lat, group = Serial_Number), size = .1, color = "#FF6346") +
  facet_wrap(~ year) + xlab("Latitude") + ylab("Longitude") + ggtitle("Pacific Storms, 1980-1990")

pacific_90s = ggplot(filter(pacific_data, decade == "1990s")) +
  geom_map(dat = pacific_map, map = pacific_map, 
           aes(map_id = region, x = long, y = lat), 
           fill = "#FFF5EE", color = "#4682B4", size = 0.25) +
  geom_path(aes(x = Long, y = Lat, group = Serial_Number), size = .1, color = "#FF6346") +
  facet_wrap(~ year) + xlab("Latitude") + ylab("Longitude") + ggtitle("Pacific Storms, 1990-2000")

pacific_00s = ggplot(filter(pacific_data, decade == "2000s")) +
  geom_map(dat = pacific_map, map = pacific_map, 
           aes(map_id = region, x = long, y = lat), 
           fill = "#FFF5EE", color = "#4682B4", size = 0.25) +
  geom_path(aes(x = Long, y = Lat, group = Serial_Number), size = .1, color = "#FF6346") +
  facet_wrap(~ year) + xlab("Latitude") + ylab("Longitude") + ggtitle("Pacific Storms, 2000-2010")

atlantic_80s = ggplot(filter(atlantic_data, decade == "1980s")) +
  geom_map(dat = atlantic_map, map = atlantic_map, 
           aes(map_id = region, x = long, y = lat), 
           fill = "#FFF5EE", color = "#4682B4", size = 0.25) +
  geom_path(aes(x = Long, y = Lat, group = Serial_Number), size = .1, color = "#FF6346") +
  facet_wrap(~ year) + xlab("Latitude") + ylab("Longitude") + ggtitle("Atlantic Storms, 1980-1990")

atlantic_90s = ggplot(filter(atlantic_data, decade == "1990s")) +
  geom_map(dat = atlantic_map, map = atlantic_map, 
           aes(map_id = region, x = long, y = lat), 
           fill = "#FFF5EE", color = "#4682B4", size = 0.25) +
  geom_path(aes(x = Long, y = Lat, group = Serial_Number), size = .1, color = "#FF6346") +
  facet_wrap(~ year) + xlab("Latitude") + ylab("Longitude") + ggtitle("Atlantic Storms, 1990-2000")

atlantic_00s = ggplot(filter(atlantic_data, decade == "2000s")) +
  geom_map(dat = atlantic_map, map = atlantic_map, 
           aes(map_id = region, x = long, y = lat), 
           fill = "#FFF5EE", color = "#4682B4", size = 0.25) +
  geom_path(aes(x = Long, y = Lat, group = Serial_Number), size = .1, color = "#FF6346") +
  facet_wrap(~ year) + xlab("Latitude") + ylab("Longitude") + ggtitle("Atlantic Storms, 2000-2010")

#Exporting images:
png("images/all_pacific.png")
all_pacific
dev.off()
png("images/all_atlantic.png")
all_atlantic
dev.off()
png("images/pacific_monthly.png")
pacific_monthly
dev.off()
png("images/atlantic_monthly.png")
atlantic_monthly
dev.off()
png("images/pacific_80s.png")
pacific_80s
dev.off()
png("images/pacific_90s.png")
pacific_90s
dev.off()
png("images/pacific_00s.png")
pacific_00s
dev.off()
png("images/atlantic_80s.png")
atlantic_80s
dev.off()
png("images/atlantic_90s.png")
atlantic_90s
dev.off()
png("images/atlantic_00s.png")
atlantic_00s
dev.off()

pdf("images/all_pacific.pdf")
all_pacific
dev.off()
pdf("images/all_atlantic.pdf")
all_atlantic
dev.off()
pdf("images/pacific_monthly.pdf")
pacific_monthly
dev.off()
pdf("images/atlantic_monthly.pdf")
atlantic_monthly
dev.off()
pdf("images/pacific_80s.pdf")
pacific_80s
dev.off()
pdf("images/pacific_90s.pdf")
pacific_90s
dev.off()
pdf("images/pacific_00s.pdf")
pacific_00s
dev.off()
pdf("images/atlantic_80s.pdf")
atlantic_80s
dev.off()
pdf("images/atlantic_90s.pdf")
atlantic_90s
dev.off()
pdf("images/atlantic_00s.pdf")
atlantic_00s
dev.off()
