index_m=seq(from=1, to=5329, by=3)
index_y=seq(from=3, to=5331, by=3)
index_d=seq(from=2, to=5330, by=3)

num=(storm$days*4)
month=unlist(strsplit(storm$date, split="/"))[index_m]
year=unlist(strsplit(storm$date, split="/"))[index_y]
day=unlist(strsplit(storm$date, split="/"))[index_d]
day=as.numeric(day)
monthcorr=rep(month, times=num)
yearcorr=rep(year, times=num)

index_dd=(day+(as.numeric(storm$days)-1))
dex=numeric(0)
for (i in 1:1777) {
  yep=seq(from=day[i], to=index_dd[i], by=1)
  dex=c(dex, yep)
}

daycorr=rep(dex, times=nums)


dates=paste(monthcorr, daycorr, yearcorr, sep="/")
tracks_date=dates
#creating an exlusion vector for pressure at end of each row

nam=seq(from=5, to=64445, by=5)
head(cyc1, 100)
cyc1[nam]=gsub("\\*", "", cyc1[nam])


#excluding those pressure values to isolate storm type
cyc2=grep(cyc1, pattern="[\\*|s|S|E|W|L]", value=TRUE)
storm_type=unlist(str_extract_all(cyc1, pattern="\\*|S|E"))


dex=numeric(0)
for (i in 1851:2013) {
  dex[i]=length(unique(tracks$tracks_id[tracks$tracks_wind>=35&tracks$year==i]))
  
  
}
dex[1851:2013]
data.frame(dex, storms$year)
summary(dex[1851:2013])
tail(storms$year)
tail(storms)
summary(dax[1851:2013])


dex=numeric(0)
length(unique(tracks$tracks_id[tracks$tracks_wind>=35&tracks$year==1851]))
for (i in 1851:2013) {
  dex[i]=length(unique(tracks$tracks_id[1:length(tracks$tracks_wind
                                                 [tracks$year==i]>=35)]))
  
}
dex=dex[1851:2013]
summary(dex)
data.frame(dex, year)
as.matrix(
  data.frame(dex, year))


storms = read.csv(na.strings = " ", stringsAsFactors = TRUE,header = FALSE,  "Basin.NA.ibtracs_hurdat.v03r06.cxml")

storms = read.csv(na.strings = " ", stringsAsFactors = TRUE,
                  header = FALSE,  "Basin.NA.ibtracs_hurdat.v03r06.cxml")
