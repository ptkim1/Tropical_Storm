# ===================================================================== 
# Skeleton script for Final Project
# Author(s): Jeff Grant and Paul Kim
# Date: 08-11-2015 
# Description: This code creates the directories and downloads raw data files.
#              It also writes the READMD.md file.  
# =====================================================================

#Creating directories
dir.create("code")
dir.create("rawdata")
dir.create("data")
dir.create("resources")
dir.create("report")
dir.create("images")

#Downloading data:
#For analysis
download.file("ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/hurdat_format/basin/Basin.NA.ibtracs_hurdat.v03r06.hdat"
               ,"./rawdata/storms.txt")
#For Visualizations
download.file("ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.NA.ibtracs_wmo.v03r06.csv",
              "./rawdata/raw_NA.csv")
download.file("ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.EP.ibtracs_wmo.v03r06.csv",
              "./rawdata/raw_EP.csv")

#Writing README.md

file.create("README.md")
cat(
"# Tropical Storms 

### Introduction

This project presents an analysis of tropical storm data from  the National Climate Data Center's International Best Track Archive for Climate Stewardship (IBTrACS). It specifically focuses on storms between 1980 and 2013. 

### Authors
Paul Kim, luapkim@berkeley.edu

Jeff Grant grantj@berkeley.edu

### Organization
The project folder contains the following directories and files. 
- code: contains all .R script files
- rawdata: contains all unprocessed data, downloaded via the skeleton.R file
- data: contains the processed and cleaned data
- resources: contains any downloaded resource files
- report: contains the project report RMarkdown form, as well as the corresponding pdf
- images: contains all images and plots
- README.md is this document
- skeleton.R is an R script that creates the directories and this README.md file, and also downloads raw data into the rawdata directory. 
"
, file = "README.md")
