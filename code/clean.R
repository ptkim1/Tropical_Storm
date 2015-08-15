# ===================================================================== 
# Cleaning Script for the Final Project
# Author(s): Jeff Grant and Paul Kim
# Date: 08-11-2015 
# Description: This code cleans the ibtracs data and writes the cleaned
#              file to the data directory. 
# Data: "Basin.NA.ibtracs_hurdat.v03r06.cxml" 
# =====================================================================


#libraries
library(readr)
library(stringr)

#creating storms 
storms = read.csv(na.strings = " ", stringsAsFactors = TRUE,
                  header = FALSE,  "./rawdata/storms.txt")


#selecting for headers and cleaning
header = subset(storms, grepl("M", storms[,1]))

header1 = as.data.frame(gsub("= ", "=", as.character(header[1:1777,])))

header2 = as.data.frame(gsub("=  ", "=", as.character(header1[1:1777,])))

header3 = as.data.frame(gsub("=   ", "=", as.character(header2[1:1777,])))

header4 = as.data.frame(gsub("SNBR= ", "=", as.character(header3[1:1777,])))

#outputting as a table
write.table(quote = FALSE, sep = " ", na = "NA", header4, file = "./data/header.txt")

#creatinging the data frame
storm = read.table(skip = 1, col.names = 
            c("id", "ID", "date", "days", "s#", "SNBR","NAME", "XING", "SSS"),
            file="./data/header.txt")

#Removing the extra ID column
storm$ID = NULL

#Putting the dates and days in the proper format
storm$date <- format(as.Date(storm$date, "%m/%d/%Y"), "%m/%d/%Y")


storm$days = as.numeric(grep(x =unlist(
  strsplit(as.character(storm$days), split = "=")), 
  pattern="[0-9]", value = TRUE))

storm$s. = NULL

storm$SNBR = NULL

storm$XING = NULL

storm$SSS = NULL

#finally creating th csv
write_csv(x = storm, path="./data/storms.csv")



#creating adequate date data for tracks.csv
storm$date <- as.Date(storm$date, "%m/%d/%Y")

end_date = storm$date+(storm$days-1)

dex = character(0)

for (i in 1:1777) {
  yep = as.character(seq(from = storm$date[i], to = end_date[i], by = 1))
  dex = c(dex, yep)
}

date1 = as.Date(dex)

date2 = as.character(format(date1, "%m/%d/%Y"))

nums = rep(4, time = 12889)

tracks_date = rep(date2, times = nums)

tracks_date = format(as.Date(tracks_date, "%m/%d/%Y"), "%m/%d/%Y")

#creating identity column
num = (storm$days*4)

iden = c(1:1777)

identity = rep(iden, times = num )

tracks_id = identity

#creating times column
times = c("00H", "06H", "12H", "18H")

times_1 = rep(times,times = 12889)

tracks_period = times_1

#creating storm types

#exluding the header and trailer and cleaning the raw data
ntheader = subset(storms, grepl("\\*", storms[,1]))

ntheader1=(as.data.frame(gsub(as.character(ntheader[1:12889, ]), 
              pattern = "\\*  0   0", replacement="\\*0000000")))

ntheader2=(as.data.frame(gsub(as.character(ntheader1[1:12889, ]),
                              pattern = "\\* ", replacement="\\*0")))

ntheader3 = as.data.frame(gsub("0E720  50", "0E7200050", 
                             as.character(ntheader2[1:12889,])))

ntheader4 = as.data.frame(gsub("0E725 140", "0E7250140", 
                             as.character(ntheader3[1:12889,])))

ntheader5 = as.data.frame(gsub("16E725 220", "16E7250220", 
                             as.character(ntheader4[1:12889,])))

ntheader6 = as.data.frame(gsub("0E723 280", "0E7230280", 
                             as.character(ntheader5[1:12889,])))

ntheader7 = as.data.frame(gsub("0E626  25", "0E6260025", 
                             as.character(ntheader6[1:12889,])))

ntheader8 = as.data.frame(gsub("07E629  70", "07E6290070", 
                             as.character(ntheader7[1:12889,])))

ntheader9 = as.data.frame(gsub("0E451  15", "0E4510015", 
                             as.character(ntheader8[1:12889,])))

#checking things out
head(ntheader9)

tail(ntheader9)

#making storm types
cyc = unlist(strsplit(as.character(ntheader9[1:12889, ]), split=" "))

cyc1 = grep(cyc, pattern="[\\*SE].*[0-9]$", value = TRUE)

storm_type = unlist(str_extract_all(cyc1, pattern="\\*|S|E"))

#transforming from symbols to names
storm_type[storm_type=="*"]="cyclone"

storm_type[storm_type=="S"]="subtropical"

storm_type[storm_type=="E"]="extratropical"

tracks_stage = storm_type

#creating latitude and longitude
lat_long = unlist(strsplit(cyc1, split="[\\*]|S|E"))
nams = seq(from = 2, to = 103112, by = 2)
latlong_clean = lat_long[nams]
tracks_latitude = as.numeric(str_sub(latlong_clean, start = 1, end = 3))/10
tracks_longitude = as.numeric(str_sub(latlong_clean, start = 4, end = 7))/10
#creating pressure
cyc1 = grep(cyc, pattern="\\*|s|S|E|W|L", value = TRUE)

nem = seq(from = 1, to = 64441, by = 5)

press = cyc1[-nem]

press_clean = unlist(str_extract_all(press, pattern = "^[0-9]{1,4}[\\*|E|S]"))

tracks_pressure = as.numeric(gsub("\\*|E|S", "", press_clean))

#creating wind
wind = unlist(grep(cyc, pattern="[0-9]", value = TRUE))

wind1 = as.numeric(grep(wind, pattern="[0-9A-Z]{5}"))

wind2 = wind[-wind1]

wind3 = as.numeric(grep(wind2, pattern="[\\*]"))

tracks_wind = wind2[-wind3]

#constructing the data frame
tracks = data.frame(tracks_id, tracks_date, tracks_period, tracks_stage, 
              tracks_latitude, tracks_longitude, tracks_wind, tracks_pressure)

tracks_clean = tracks[tracks$tracks_latitude!=0 | tracks$tracks_longitude!=0 | 
                      tracks$tracks_wind!=0 | tracks$tracks_pressure!=0,]

tracks_clean$tracks_longitude=(tracks_clean$tracks_longitude-360)

write_csv(x = tracks_clean, path= "./data/tracks.csv")


