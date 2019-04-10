#This is a script for the climate data calculations that went into creating the file NWT_ClimateData_2015_11_02.xlsx
#Made by Emily Farrer
# > Modified by CTW to include years 2015-2017 to update pika figure for Chris Ray
# ">" denotes comment from CTW

library(tidyverse)
library(lubridate)
#library(plyr)
library(lattice)
library(Hmisc)
library(vegan)
library(reshape)
library(plotrix)
#library(tidyr)


##### MAT and mean temp by season #####
##Saddle temp data
#recalculate spr sum wnt fal based on the lake LTER ms where summer is Jun/Jul/Aug
#Saddletemp<-read.csv("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/sdltdayv.ml.data.infilled.csv",na.strings = c("."))

# > CTW: read in ctw's infilled temperature dataset (NSF data + ctw infilling 2015-2017)
Saddletemp <- read_csv("extended_summer/output_path/sdl_temp_infilled_19822017_ctw.csv",
                      trim_ws=TRUE, 
                      na = c("", "NA"))
# > CTW: check it read in correctly
glimpse(Saddletemp)
summary(Saddletemp) 

# > CTW: create and rename variables so matches Emily's colnames
Saddletemp <- Saddletemp %>%
  dplyr::select(-X1) %>% # remove index variable created when writing .csv from R
  # need day, month, year
  mutate(`year` = year(`date`),
         `month` = month(`date`),
         `day` = day(`date`)) %>%
# temp names are mean_temp, max_temp, min_temp
  dplyr::rename(max_temp = airtemp_max,
                min_temp = airtemp_min,
                mean_temp = airtemp_mean)

#define season2, summer is Jun/Jul/Aug, etc
Saddletemp$season<-NA
Saddletemp$season[Saddletemp$month%in%c(12,1,2)]<-"wnt"
Saddletemp$season[Saddletemp$month%in%c(3,4,5)]<-"spr"
Saddletemp$season[Saddletemp$month%in%c(6,7,8)]<-"sum"
Saddletemp$season[Saddletemp$month%in%c(9,10,11)]<-"fal"

#define water year, same as year for all months except 9, 10, 11, 12, for which it is the following year
# > ctw: water year is Oct 1 - Sep 30.. Sep should not be included in this unless EF had a biological reason (?)
# > e.g. is "water year" here what becomes "eco year" later?
Saddletemp$water_yr<-Saddletemp$year
Saddletemp$water_yr[Saddletemp$month==9]<-Saddletemp$water_yr[Saddletemp$month==9]+1
Saddletemp$water_yr[Saddletemp$month==10]<-Saddletemp$water_yr[Saddletemp$month==10]+1
Saddletemp$water_yr[Saddletemp$month==11]<-Saddletemp$water_yr[Saddletemp$month==11]+1
Saddletemp$water_yr[Saddletemp$month==12]<-Saddletemp$water_yr[Saddletemp$month==12]+1


#summer=Jun/Jul/Aug
Saddletemp3long<-aggregate.data.frame(Saddletemp$mean_temp,by=list(season=Saddletemp$season,water_year=Saddletemp$water_yr),function(x){mean(x,na.rm=T)})
Saddletemp3short<-cast(Saddletemp3long,water_year~season,value="x",fun=mean)

#make a column with mean annual temp
Saddletemp3short$MAT<-aggregate.data.frame(Saddletemp$mean_temp,by=list(Saddletemp$water_yr),function(x){mean(x,na.rm=T)})[,2]
Saddletemp3short<-Saddletemp3short[-1,];Saddletemp3short<-Saddletemp3short[-37,] # remove 1981 and remove last year bc incomplete
colnames(Saddletemp3short)<-c("water_year","fal_meanT","spr_meanT",	"sum_meanT","wnt_meanT","MAT")



##### GDD #####
#use Saddletemp to calculate growing degree days for a base of 5C (for pikas). There are no NAs in temp columns
head(Saddletemp)
which(is.na(Saddletemp$mean_temp)==T)
which(is.na(Saddletemp$max_temp)==T)
which(is.na(Saddletemp$min_temp)==T)
#If the mean daily temperature is lower than the base temperature then GDD=0
#GDD = (Tmax +Tmin)/2 - Tbase
#where Tmax is maximum daily temperature and is set equal to 25 when temperatures exceed 25 (or I think many people either don't set this or set it according to wikipedia to 30). The max temp in Saddle dataset is 25 so no need to do anything.
#Tmin is the minimum dail temperature and is set equal to 5C when temperatures fall below 5C #alpine plant papers use 0 or 5 or -4 or -2
#Tbase is the base temperature for the organism
# In captivity, temperatures as low as 25.5°C have proved fatal (Smith 1974). (Shinderman 2015 ecology and evolution)

which((Saddletemp$max_temp+Saddletemp$min_temp)/2!=Saddletemp$mean_temp)#there are a lot of places where the average of max and min does not equal the mean_temp. looks like wherever infilling took place, but the means are not that off
# > CTW: check difference with boxplot
boxplot((Saddletemp$max_temp+Saddletemp$min_temp)/2!=Saddletemp$mean_temp) # off by 1 degree at most, typically much less

max(Saddletemp$max_temp, na.rm = T)
Saddletemp$min_tempforGDD<-ifelse(Saddletemp$min_temp<5,5,Saddletemp$min_temp)
Saddletemp$GDD<-ifelse(((Saddletemp$min_tempforGDD+Saddletemp$max_temp)/2-5)<0,0,((Saddletemp$min_tempforGDD+Saddletemp$max_temp)/2-5))
head(Saddletemp)
plot(Saddletemp$GDD[1:300])
SaddleGDD<-aggregate.data.frame(Saddletemp$GDD,by=list(year=Saddletemp$year),function(x){sum(x,na.rm=T)})#this is a little odd becase there are still a lot of Growing Degree Days in Sept so this would count for that same year
SaddleGDD<-SaddleGDD[-1,];colnames(SaddleGDD)[2]<-"GDD"
SaddleGDDwateryear<-aggregate.data.frame(Saddletemp$GDD,by=list(year=Saddletemp$water_yr),function(x){sum(x,na.rm=T)})#this is using water year, use this for final climate excel file
SaddleGDDwateryear<-SaddleGDDwateryear[-1,];SaddleGDDwateryear<-SaddleGDDwateryear[-37,];colnames(SaddleGDDwateryear)[2]<-"GDD"

#Look for correlation between GDD and mean summer temp, they are highly correlated
plot(Saddletemp3short$sum_meanT,SaddleGDD$GDD)

#get growing degree days for jun/jul/aug
head(Saddletemp)
SaddleGDDwateryearseason<-aggregate.data.frame(Saddletemp$GDD,by=list(year=Saddletemp$water_yr, season=Saddletemp$season),function(x){sum(x,na.rm=T)})#this is using water year
SaddleGDDwateryearsum<-subset(SaddleGDDwateryearseason,SaddleGDDwateryearseason$season=="sum")
SaddleGDDwateryearsum<-SaddleGDDwateryearsum[-1,] # exclude 1981 because the record starts mid July
colnames(SaddleGDDwateryearsum)[3]<-"GDDsum"
#write.csv(SaddleGDDwateryearsum,"~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/SaddleGDDwateryearsum.csv",row.names=F,quote=F)
write.csv(SaddleGDDwateryearsum,"/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/climate_update/SaddleGDDwateryearsum.csv",row.names=F,quote=F)





##### Five day running temps #####
#Days after June 1 when the five day running mean of maximum temperature reaches 24C. From Veblen 2015
head(Saddletemp)
Saddletemp$fivedayrunning<-NA
for(i in 3:(length(Saddletemp$max_temp)-3)){
  Saddletemp$fivedayrunning[i]<-mean(Saddletemp$max_temp[(i-2):(i+2)])
}
plot(Saddletemp$fivedayrunning)
Saddletemp$fivedayrunning[300:360]

#summer=Jun/Jul/Aug for max temp
Saddletemp3longmax<-aggregate.data.frame(Saddletemp$max_temp,by=list(season=Saddletemp$season,water_year=Saddletemp$water_yr),function(x){mean(x,na.rm=T)})
Saddletemp3shortmax<-cast(Saddletemp3longmax,water_year~season,value="x",fun=mean)
Saddletemp3shortmax<-Saddletemp3shortmax[-1,];Saddletemp3shortmax<-Saddletemp3shortmax[-37,] # drop 1981 and 2018
colnames(Saddletemp3shortmax)<-c("water_year","fal_maxT","spr_maxT","sum_maxT","wnt_maxT")
mean(Saddletemp3shortmax$sum_maxT)#mean summer max temp is 12.5 (# >12.5 through 2017 )
mean(Saddletemp3short$sum_meanT)#mean summer temp is 8.1 (#8.0 through 2017)

#The mean max temp in summer (jun/jul/aug) is 12.5. If you use 5C (because of the pikas), then you have to go back to March to get positive numbers. 15 doesn't work you get NAs because in a few years you never reach a 5-day mean of 15. 12C works if you start in May. 
head(Saddletemp)
Saddlefivedayrunning5C<-data.frame(year=rep(NA,38),fivedayrunning=rep(NA,38))
for(i in 2:length(unique(Saddletemp$year))){
  years<-unique(Saddletemp$year)
  tempdata<-subset(Saddletemp,year==years[i])
  Saddlefivedayrunning5C$year[i]<-years[i]
  Saddlefivedayrunning5C$fivedayrunning[i]<-which(tempdata$fivedayrunning>5)[1]-which(tempdata$month==3&tempdata$day==1)+1
}

Saddlefivedayrunning5C

Saddlefivedayrunning12C<-data.frame(year=rep(NA,38),fivedayrunning=rep(NA,38))
for(i in 2:length(unique(Saddletemp$year))){
  years<-unique(Saddletemp$year)
  tempdata<-subset(Saddletemp,year==years[i])
  Saddlefivedayrunning12C$year[i]<-years[i]
  Saddlefivedayrunning12C$fivedayrunning[i]<-which(tempdata$fivedayrunning>12)[1]-which(tempdata$month==5&tempdata$day==1)+1
}
Saddlefivedayrunning12C
#
plot(Saddlefivedayrunning5C,Saddlefivedayrunning12C)
#they are not correlated at all, weird




##### GSL #####
#use saddletemp to figure out growing season length GSL
#Growing season length -- with end of spring / autumn onset based on 2 different freezing/frost thresholds (0C and, for a hard freeze, -3C) and 3 seasonal markers:
#a) freeze at night (Tmin<0C and <-3C)
#b) freeze during day (Tmax<0C and <-3C)
#c) freeze at night for 3 consecutive days (Tmin<0C and <-3C)
tail(Saddletemp)

#calculate three day running sum, look for the day it = 3
#first do 0 degrees at night (tmin) for 3 consecutive days
#change min temp into T and F (<0)
Saddletemp$min_tempTF<-ifelse(Saddletemp$min_temp<0,T,F)
head(Saddletemp)

Saddletemp$mintempthreeday0<-NA
for(i in 3:(length(Saddletemp$min_temp)-3)){
  Saddletemp$mintempthreeday0[i]<-sum(Saddletemp$min_tempTF[(i-2):(i)])
}

Saddlemintempthreeday0<-data.frame(year=rep(NA,38),GSLthreeday0C=rep(NA,38))
for(i in 2:length(unique(Saddletemp$year))){
  years<-unique(Saddletemp$year)
  tempdata<-subset(Saddletemp,year==years[i])
  Saddlemintempthreeday0$year[i]<-years[i]
  sprdata<-tempdata$mintempthreeday0[1:196]#july15 is julian day 196
  faldata<-tempdata$mintempthreeday0[197:365]
  sprtempday<-max(which(sprdata==3))
  faltempday<-min(which(faldata==3))+196
  Saddlemintempthreeday0$GSLthreeday0C[i]<-faltempday-sprtempday
}

plot(Saddlemintempthreeday0$year,Saddlemintempthreeday0$GSLthreeday0C,type="l")

Saddlemintempthreeday0


#having the cutoff be three days where min temp is -3C
Saddletemp$min_tempTF3<-ifelse(Saddletemp$min_temp<(-3),T,F)
head(Saddletemp)

Saddletemp$mintempthreedayneg3<-NA
for(i in 3:(length(Saddletemp$min_temp)-3)){
  Saddletemp$mintempthreedayneg3[i]<-sum(Saddletemp$min_tempTF3[(i-2):(i)])
}

Saddletemp[100:200,]

Saddlemintempthreedayneg3<-data.frame(year=rep(NA,38),GSLthreedayneg3C=rep(NA,38))
for(i in 2:length(unique(Saddletemp$year))){
  years<-unique(Saddletemp$year)
  tempdata<-subset(Saddletemp,year==years[i])
  Saddlemintempthreedayneg3$year[i]<-years[i]
  sprdata<-tempdata$mintempthreedayneg3[1:196]#july15 is julian day 196
  faldata<-tempdata$mintempthreedayneg3[197:365]
  sprtempday<-max(which(sprdata==3))
  faltempday<-min(which(faldata==3))+196
  Saddlemintempthreedayneg3$GSLthreedayneg3C[i]<-faltempday-sprtempday
}

Saddlemintempthreedayneg3

#write.csv(Saddlemintempthreedayneg3,"~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/Saddlemintempthreedayneg3.csv",row.names=F,quote=F)
write.csv(Saddlemintempthreedayneg3,"/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/climate_update/Saddlemintempthreedayneg3.csv",row.names=F,quote=F)



plot(Saddlemintempthreedayneg3$GSLthreedayneg3C,Saddlemintempthreeday0$GSLthreeday0C)






##### Frost free days ####
#Use saddle temp to calculate #frost days in May and Sept
head(Saddletemp)
Saddletemp[1:100,]
frost<-aggregate.data.frame(cbind(Saddletemp$min_tempTF,Saddletemp$min_tempTF3), by=list(year=Saddletemp$water_yr,month=Saddletemp$month),sum)
colnames(frost)[3:4]<-c("frosts0","frostsneg3")
frost<-frost[-which(frost$year %in% c(1981,2018)),]

#write.csv(frost,"~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/frost.csv",row.names=F,quote=F)
#write.csv(frost,"/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/climate_update/frost.csv",row.names=F,quote=F)
head(frost)


frostseason<-aggregate.data.frame(cbind(Saddletemp$min_tempTF,Saddletemp$min_tempTF3),by=list(year=Saddletemp$water_yr,month=Saddletemp$season),sum)
colnames(frostseason)[3:4]<-c("frosts0","frostsneg3")
frostseason<-frostseason[-which(frostseason$year %in% c(1981,2018)),]
#write.csv(frostseason,"~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/frostseason.csv",row.names=F,quote=F)
#write.csv(frostseason,"/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/climate_update/frostseason.csv",row.names=F,quote=F)





###### Saddle precip #####
#Saddleprecip<-read.csv("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/infilling_sdl_precip_1982-2014_distribution_(all_09-14_infilled).csv")#na.strings = c(".")
# > CTW: read in infilled through 2017 precip (NSF proposal data + 2015-2017 infilled by ctw)
# > IMPORTANTE: all precip values in this have already been snow corrected as Emily did (months not JJA * 0.39)
Saddleprecip <- read_csv("/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/sdl_ppt_infill_19822017_ctw.csv",
                         trim_ws=TRUE, 
                         na = c("", "NA"))
# > CTW: check it read in correctly
glimpse(Saddleprecip)
summary(Saddleprecip) #no NAs
head(Saddleprecip)

# > CTW: create and rename variables so matches Emily's colnames
Saddleprecip <- Saddleprecip %>%
  dplyr::select(-X1) %>% # remove index variable created when writing .csv from R
  # rename fields, otherwise okay
  dplyr::rename(`year` = yr,
                `month` = mon,
                precip_mm = ppt_tot)


# > CTW: don't need to execute these lines because already infilled (no NAs, and already snow-corrected following Emily's method)
# > NOTE: For more accuracy, may want to update 0.39 to average adjustment numbers reported in Jennings et al. 2018
# > Also CTW doesn't think September should have been snow corrected (have read Oct - May), but followed Emily's method for consistency
# #replace NA with 0 in the precip_mm column (because the infilling accounts for precip in those days)
# Saddleprecip$precip_mm[which(is.na(Saddleprecip$precip_mm)==T)]<-0

#Correct for blowing snow, multiply all non summer (not JJA) months by .39, MW Williams et al. 1998 Atmospheric Environment
# ind<-which(Saddleprecip$month%in%c(1,2,3,4,5,9,10,11,12))
# Saddleprecip$precip_mm[ind]<-Saddleprecip$precip_mm[ind]*.39##don't run this more than once!!!

#define season, summer is Jun/Jul/Aug, etc
Saddleprecip$season<-NA
Saddleprecip$season[Saddleprecip$month%in%c(12,1,2)]<-"wnt"
Saddleprecip$season[Saddleprecip$month%in%c(3,4,5)]<-"spr"
Saddleprecip$season[Saddleprecip$month%in%c(6,7,8)]<-"sum"
Saddleprecip$season[Saddleprecip$month%in%c(9,10,11)]<-"fal"

head(Saddleprecip)

# > CTW note: hydrologic water year is not Sept - Aug; is Oct-Sep
#define water year, same as year for all months except 9, 10, 11, 12, for which it is the following year
Saddleprecip$water_yr<-Saddleprecip$year
Saddleprecip$water_yr[Saddleprecip$month==9]<-Saddleprecip$water_yr[Saddleprecip$month==9]+1
Saddleprecip$water_yr[Saddleprecip$month==10]<-Saddleprecip$water_yr[Saddleprecip$month==10]+1
Saddleprecip$water_yr[Saddleprecip$month==11]<-Saddleprecip$water_yr[Saddleprecip$month==11]+1
Saddleprecip$water_yr[Saddleprecip$month==12]<-Saddleprecip$water_yr[Saddleprecip$month==12]+1

#summer=Jun/Jul/Aug
Saddleprecip3long<-aggregate.data.frame(Saddleprecip$precip_mm,by=list(season=Saddleprecip$season,water_year=Saddleprecip$water_yr),function(x){sum(x,na.rm=T)})
Saddleprecip3short<-cast(Saddleprecip3long,water_year~season,value="x",fun=sum)
#make a column with total annual precip
Saddleprecip3short$tot_precip<-aggregate.data.frame(Saddleprecip$precip_mm,by=list(Saddleprecip$water_yr),function(x){sum(x,na.rm=T)})[,2]
colnames(Saddleprecip3short)<-c("water_year","fal_precip","spr_precip",	"sum_precip","wnt_precip","tot_precip")
Saddleprecip3short<-Saddleprecip3short[-1,];Saddleprecip3short<-Saddleprecip3short[-37,] # remove 1981 and 2018 because not complete

#export daily values for AET # values are corrected for blowing snow
#write.csv(Saddleprecip,"~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/Saddleprecip_2018-4-20.csv",row.names=F,quote=F)
#write.csv(Saddleprecip,"/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/climate_update/Saddleprecip_2018-9-26_efmethod.csv",row.names=F,quote=F)





##### Then run the AET script to get all those outputs #####









###### Saddle meltout date #####

#saddlesnow<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/NWTlter/SaddleSnowDepth/NWT_SaddleGridSnow_1992.2014Emily.csv")
# > CTW: will try using data on NWT portal (current though 2017, not QAd by me)
saddlesnow <- read_csv("http://niwot.colorado.edu/data_csvs/saddsnow.dw.data.csv",
                       na = c("NA", " ", "NaN"),
                       trim_ws = TRUE)
head(saddlesnow)
tail(saddlesnow)
which(is.na(saddlesnow$mean_depth)==T)

#define water year, same as year for all months except 10, 11, 12, for which it is the following year
#I'm not sure why the sept lin is commented out, it is probably b/c I didn't need to ever reference water year for these meltout calculations
saddlesnow$water_yr<-saddlesnow$year
#saddlesnow$water_yr[saddlesnow$month==9]<-saddlesnow$water_yr[saddlesnow$month==9]+1
saddlesnow$water_yr[saddlesnow$month==10]<-saddlesnow$water_yr[saddlesnow$month==10]+1
saddlesnow$water_yr[saddlesnow$month==11]<-saddlesnow$water_yr[saddlesnow$month==11]+1
saddlesnow$water_yr[saddlesnow$month==12]<-saddlesnow$water_yr[saddlesnow$month==12]+1


#julian day, jan 1 is 0
saddlesnow$julian<-NA
for(i in 1:dim(saddlesnow)[1]){
  saddlesnow$julian[i]<-julian(as.Date(saddlesnow$date[i]),origin = as.Date(paste(saddlesnow$year[i],"01","01",sep="/")))+1
}


saddlemeltout<-data.frame(year=rep(NA,22*88),sort_num=rep(NA,22*88),julian=rep(NA,22*88))
i=1
#the loop assesses the period between Jan 1 and Aug 30 for snow. 
for(p in 1:88){
  for(t in 1993:2014){#22 years
    current.plot<-p
    current.year<-t
    tempdata<-saddlesnow[which(saddlesnow$sort_num==p&saddlesnow$year==t&saddlesnow$month<9),]
    tempdata<-tempdata[which(is.na(tempdata$mean_depth)==F),]
    last.snow.ind<-max(which(tempdata$mean_depth>0))
    if(last.snow.ind==dim(tempdata)[1]){
      saddlemeltout[i,"julian"]<-NA
    }
    if(last.snow.ind<dim(tempdata)[1]){
      saddlemeltout[i,"julian"]<-tempdata$julian[last.snow.ind]
    }
    if(last.snow.ind==-Inf){
      saddlemeltout[i,"julian"]<-0
    }
    saddlemeltout[i,"year"]<-current.year
    saddlemeltout[i,"sort_num"]<-current.plot
    i<-i+1
  }
}
#41 warnings occur from the plots that had no snow (that's fine). 0 means the plot had no snow over the entire time period, NA means the plot never melted out (by the last survey date)

#check how many zero snow plots/years there are
temp<-aggregate.data.frame(saddlesnow$mean_depth,by=list(saddlesnow$water_yr,saddlesnow$sort_num),function(x){sum(x,na.rm=T)})
temp[which(temp$x==0),]

#export for plot level data
#use snowprod data file to get the class of each plot
snowprod<-read.csv("/Users/farrer/Dropbox/NWT_data/NWT_SnowXProd.csv",na.strings = c("."))
snowprod$plot<-as.factor(snowprod$plot)
head(snowprod)
#if you ever want to use the productivity data: in years before 2008, whole cushion plants were harvested for anpp which makes the anpp super high in dry and moist meadow. so I'm changing all of those to NA
ind<-which(snowprod$year<2008&snowprod$class_3%in%c("DM","FF"))
snowprod$anpp[ind]<-NA

#calculate average meltout date, not including 0s or NAs for the entire saddle and for each community type
#snowprod has class_3 info in it
head(saddlemeltout)
classinfo<-snowprod[1:88,c("sort_num","class_3")]
saddlemeltout2<-merge(saddlemeltout,classinfo,by="sort_num")
head(saddlemeltout2)

#take out NAs
saddlemeltout2<-saddlemeltout2[which(is.na(saddlemeltout2$julian)==F),]

#take out 0s
saddlemeltout2<-saddlemeltout2[which(saddlemeltout2$julian>0),]

sdlmeltout<-aggregate.data.frame(saddlemeltout2$julian,by=list(year=saddlemeltout2$year),mean)
colnames(sdlmeltout)[2]<-"sdlmeltout"
sdlmeltoutclass<-aggregate.data.frame(saddlemeltout2$julian,by=list(year=saddlemeltout2$year,class_3=saddlemeltout2$class_3),mean)
sdlmeltoutclassshort<-cast(sdlmeltoutclass,year~class_3,value="x",fun=mean)

sdlmeltout<-cbind(sdlmeltout,sdlmeltoutclassshort[,2:9])
colnames(sdlmeltout)[2:10]<-c("sdl_meltout","dm_meltout","ff_meltout","mm_meltout","rock_meltout","sb_meltout","sf_meltout","st_meltout","wm_meltout")
#











###### Getting all files together for dataset #####
#Saddle files to merge for full Saddle climate data set
#SaddleGDD #calendar year
#Saddlemoisture #calendar year

#all range from 1982,2014
Saddletemp3short #(jun/jul/aug=summer)
Saddleprecip3short #(jun/jul/aug=summer) 
Saddlemoisturewateryear<-rbind(c(1982,NA),Saddlemoisturewateryear) #deficit based on water year jun/jul/aug
SaddleGDDwateryear
Saddlefivedayrunning5C<-Saddlefivedayrunning5C[-1,]
Saddlefivedayrunning12C<-Saddlefivedayrunning12C[-1,]

ClimateAll<-cbind(Saddletemp3short,Saddleprecip3short[,-1],moisturedeficit=Saddlemoisturewateryear[,-1],GDD=SaddleGDDwateryear[,-1],fivedayrunning5C=Saddlefivedayrunning5C[,-1],fivedayrunning12C=Saddlefivedayrunning12C[,-1])
head(ClimateAll)
rownames(ClimateAll)<-1:33

plot(1982:2014,wholesite$spr_ppt,type="l",ylim=c(100,700))
points(1982:2014,ClimateAll$spr_precip,type="l",col=2)
plot(1982:2014,ClimateAll$moisturedeficit,type="l")


#Merge with climate data from whole site and saddle prod

#Whole site data
wholesite<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/NWTlter/wholesite/NWT_SiteData_2015-06-15.csv",na.strings = c("."))
ind<-which(wholesite$water.year%in%1992:1997)
wholesite[ind,c("saddleprod","dmsaddleprod","ffsaddleprod")]<-NA
head(wholesite)

ClimateAll<-cbind(ClimateAll,wholesite[,c(7,9)],wholesite[,15:20])
ClimateAll<-cbind(ClimateAll,wholesite[,55:60])
head(ClimateAll)



#meltout
head(ClimateAll)
names(sdlmeltout)[1]<-"water_year"
sdlmeltout$rock_meltout<-NULL
sdlmeltout$sf_meltout<-NULL
sdlmeltout$st_meltout<-NULL
ClimateAll<-merge(ClimateAll,sdlmeltout,by="water_year",all.x = T)
head(ClimateAll)
#write.csv(ClimateAll,"ClimateAll.csv",row.names=F,quote=F)

ClimateAll<-cbind(ClimateAll,GSLthreeday0C=Saddlemintempthreeday0[-1,2],GSLthreedaynt3C=Saddlemintempthreedayneg3[-1,2])
#write.csv(ClimateAll,"ClimateAll.csv",row.names=F,quote=F)






