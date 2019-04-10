# This is modified version of the script for the climate data calculations that went into creating the file NWT_ClimateData_2015_11_02.xlsx
# Original script made by Emily Farrer (see Climate_Data_Calculations.R)
# Modified by CTW to update climate variables used in sumAllPC scores for years 2015-2017
# As of 2018-09-20, data availability too limited to fully update climate dataset Emily created for NSF NWT Renewal Proposal (went through summer 2014)

# this script depends on the Saddle chart summer precip and temperature data infilled by CTW
# see R script Saddle_infilling_2014-2017.R

# ---------------------------
# load needed libraries
library(tidyverse)
library(lubridate)

# Mean temp for summer only (Jun/Jul/Aug)
# (MAT and mean temp by season not possible at this time)
Saddletemp <- sdl_temp_sum1517 # infilled summer months only, not winter (will see NAs in temp values)
summary(Saddletemp)
glimpse(Saddletemp)
## > CTW: add in day to make Emily's code work
Saddletemp$day <- day(Saddletemp$date)

#define season2, summer is Jun/Jul/Aug, etc
Saddletemp$season<-NA
Saddletemp$season[Saddletemp$month%in%c(12,1,2)]<-"wnt"
Saddletemp$season[Saddletemp$month%in%c(3,4,5)]<-"spr"
Saddletemp$season[Saddletemp$month%in%c(6,7,8)]<-"sum"
Saddletemp$season[Saddletemp$month%in%c(9,10,11)]<-"fal"

#define water year (ecoyear), same as year for all months except 9, 10, 11, 12, for which it is the following year
Saddletemp$ecoyear<-Saddletemp$year
Saddletemp$ecoyear[Saddletemp$month==9]<-Saddletemp$ecoyear[Saddletemp$month==9]+1
Saddletemp$ecoyear[Saddletemp$month==10]<-Saddletemp$ecoyear[Saddletemp$month==10]+1
Saddletemp$ecoyear[Saddletemp$month==11]<-Saddletemp$ecoyear[Saddletemp$month==11]+1
Saddletemp$ecoyear[Saddletemp$month==12]<-Saddletemp$ecoyear[Saddletemp$month==12]+1

#summer=Jun/Jul/Aug
## Emily's code
#Saddletemp3long<-aggregate.data.frame(Saddletemp$mean_temp,by=list(season=Saddletemp$season,water_year=Saddletemp$water_yr),function(x){mean(x,na.rm=T)})
#Saddletemp3short<-cast(Saddletemp3long,water_year~season,value="x",fun=mean)
## rewrite in tidyverse, for summer only
Saddletemp3short <- Saddletemp %>%
  filter(season == "sum") %>% # if ever get full temp dataset, take this line out
  group_by(ecoyear, season) %>%
  summarise(sum_meanT = mean(mean_temp))

## > CTW: not calculating MAT bc not enough infilled data
## > CTW: rewrite in tidyverse when all data infilled
#make a column with mean annual temp
#Saddletemp3short$MAT<-aggregate.data.frame(Saddletemp$mean_temp,by=list(Saddletemp$water_yr),function(x){mean(x,na.rm=T)})[2:34,2]
#colnames(Saddletemp3short)<-c("water_year","fal_meanT","spr_meanT",	"sum_meanT","wnt_meanT","MAT")



##### GDD #####
#use Saddletemp to calculate growing degree days for a base of 5C (for pikas). 
#There are no NAs in temp columns (CTW: for summer months only)
head(Saddletemp)
# all dates
which(is.na(Saddletemp$mean_temp)==T)
which(is.na(Saddletemp$max_temp)==T)
which(is.na(Saddletemp$min_temp)==T)
# NAs by season (most in spring of 2017)
sapply(split(Saddletemp$mean_temp, Saddletemp$season), function(x) sum(is.na(x)))
sapply(split(Saddletemp$max_temp, Saddletemp$season), function(x) sum(is.na(x)))
sapply(split(Saddletemp$min_temp, Saddletemp$season), function(x) sum(is.na(x)))

#If the mean daily temperature is lower than the base temperature then GDD=0
#GDD = (Tmax +Tmin)/2 - Tbase
#where Tmax is maximum daily temperature and is set equal to 25 when temperatures exceed 25 (or I think many people either don't set this or set it according to wikipedia to 30). 
#The max temp in Saddle dataset is 25 so no need to do anything.
#Tmin is the minimum daily temperature and is set equal to 5C when temperatures fall below 5C #alpine plant papers use 0 or 5 or -4 or -2
#Tbase is the base temperature for the organism
# In captivity, temperatures as low as 25.5°C have proved fatal (Smith 1974). (Shinderman 2015 ecology and evolution)

## > CTW: Create summer-only Saddle temp df to carry out the reason of the calculations
## > if update temp for other seasons, can run using original Saddletemp (all dates) [search and replace "Saddetemp_sum" with "Saddletemp"]
Saddletemp_sum <- Saddletemp[Saddletemp$season == "sum",]

summary((Saddletemp_sum$max_temp+Saddletemp_sum$min_temp)/2!=Saddletemp_sum$mean_temp) # some places where average of max and min does not equal the mean_temp
which((Saddletemp_sum$max_temp+Saddletemp_sum$min_temp)/2!=Saddletemp_sum$mean_temp) # which rows?
boxplot((Saddletemp_sum$max_temp+Saddletemp_sum$min_temp)/2 -Saddletemp_sum$mean_temp) # 2 mean(max,min) values more than 1 degree greater than mean temp

max(Saddletemp_sum$max_temp) #max in dataset well below 25
# set anything lower than base temp for pikas (5C) to 5C
Saddletemp_sum$min_tempforGDD<-ifelse(Saddletemp_sum$min_temp<5,5,Saddletemp_sum$min_temp)
# if mean of base temp and max temp is less than 5, set GDD to 0, otherwise take difference from base temp
Saddletemp_sum$GDD<-ifelse(((Saddletemp_sum$min_tempforGDD+Saddletemp_sum$max_temp)/2-5)<0,0,((Saddletemp_sum$min_tempforGDD+Saddletemp_sum$max_temp)/2-5))
head(Saddletemp_sum)
plot(Saddletemp_sum$GDD[1:nrow(Saddletemp_sum)])
## > CTW: look at month x year distribution using ggplot
ggplot(Saddletemp_sum, aes(as.factor(month), GDD)) + geom_boxplot() + facet_wrap(~year)
# Emily's code
# SaddleGDD<-aggregate.data.frame(Saddletemp_sum$GDD,by=list(year=Saddletemp_sum$year),function(x){sum(x,na.rm=T)})#this is a little odd becase there are still a lot of Growing Degree Days in Sept so this would count for that same year
# SaddleGDD<-SaddleGDD[-1,];colnames(SaddleGDD)[2]<-"GDD"
# SaddleGDDwateryear<-aggregate.data.frame(Saddletemp_sum$GDD,by=list(year=Saddletemp_sum$water_yr),function(x){sum(x,na.rm=T)})#this is using water year, use this for final climate excel file
# SaddleGDDwateryear<-SaddleGDDwateryear[-1,];SaddleGDDwateryear<-SaddleGDDwateryear[-34,];colnames(SaddleGDDwateryear)[2]<-"GDD"

## > CTW: rewrite in tidyverse
# keeping code for whole year in case temp data for recent years completely infilled
SaddleGDD <- Saddletemp_sum %>%
  group_by(year) %>%
  summarise(GDD = sum(GDD))
SaddleGDDecoyear <- Saddletemp_sum %>% #use ecoyear for final climate dataset (altho ecoyr=yr in summer months)
  group_by(ecoyear) %>%
  summarise(GDD = sum(GDD))
#get growing degree days for jun/jul/aug
SaddleGDDecoyearsum <- Saddletemp_sum %>% #use ecoyear for final climate dataset (altho ecoyr=yr in summer months)
  filter(season == "sum") %>%
  group_by(ecoyear) %>%
  summarise(GDDsum = sum(GDD))

#Look for correlation between GDD and mean summer temp, they are highly correlated
plot(Saddletemp3short$sum_meanT,SaddleGDDecoyearsum$GDDsum)


##### Five day running temps #####
#Days after June 1 when the five day running mean of maximum temperature reaches 24C. From Veblen 2015
head(Saddletemp)
Saddletemp$fivedayrunning<-NA
for(i in 3:(length(Saddletemp$max_temp)-3)){
  Saddletemp$fivedayrunning[i]<-mean(Saddletemp$max_temp[(i-2):(i+2)])
}
plot(Saddletemp$fivedayrunning) # nothing reachs 24
#Saddletemp_sum$fivedayrunning[300:360]

#summer=Jun/Jul/Aug for max temp
#Emily's code
# Saddletemp3longmax<-aggregate.data.frame(Saddletemp$max_temp,by=list(season=Saddletemp$season,water_year=Saddletemp$water_yr),function(x){mean(x,na.rm=T)})
# Saddletemp3shortmax<-cast(Saddletemp3longmax,water_year~season,value="x",fun=mean)
# Saddletemp3shortmax<-Saddletemp3shortmax[-1,];Saddletemp3shortmax<-Saddletemp3shortmax[-34,]
# colnames(Saddletemp3shortmax)<-c("water_year","fal_maxT","spr_maxT","sum_maxT","wnt_maxT")
# # > CTW: rewrite in tidyverse
Saddletemp3shortmax <- Saddletemp %>%
  filter(season == "sum") %>% # if ever get full temp dataset, take this line out
  group_by(ecoyear, season) %>%
  summarise(sum_maxT = mean(max_temp)) # will need to rewrite this line too (spread by season)
mean(Saddletemp3shortmax$sum_maxT)#mean summer max temp is 12.5 (for 1982-2014), 11.6 for 2015-2017
mean(Saddletemp3short$sum_meanT)#mean summer temp is 8.1 (for 1982-2014), 7.2 for 2015-2017

#The mean max temp in summer (jun/jul/aug) is 12.5. If you use 5C (because of the pikas), then you have to go back to March to get positive numbers. 15 doesn't work you get NAs because in a few years you never reach a 5-day mean of 15. 12C works if you start in May. 
head(Saddletemp)
Saddlefivedayrunning5C<-data.frame(year=rep(NA,34),fivedayrunning=rep(NA,34))
for(i in 2:length(unique(Saddletemp$year))){
  years<-unique(Saddletemp$year)
  tempdata<-subset(Saddletemp,year==years[i])
  Saddlefivedayrunning5C$year[i]<-years[i]
  Saddlefivedayrunning5C$fivedayrunning[i]<-which(tempdata$fivedayrunning>5)[1]-which(tempdata$month==3&tempdata$day==1)+1
}

Saddlefivedayrunning12C<-data.frame(year=rep(NA,length(unique(Saddletemp$year))),fivedayrunning=rep(NA,length(unique(Saddletemp$year))))
for(i in 2:length(unique(Saddletemp$year))){
  years<-unique(Saddletemp$year)
  tempdata<-subset(Saddletemp,year==years[i])
  Saddlefivedayrunning12C$year[i]<-years[i]
  Saddlefivedayrunning12C$fivedayrunning[i]<-which(tempdata$fivedayrunning>12)[1]-which(tempdata$month==5&tempdata$day==1)+1
}
na.omit(Saddlefivedayrunning12C)

plot(na.omit(Saddlefivedayrunning5C$fivedayrunning),na.omit(Saddlefivedayrunning12C$fivedayrunning))
cor(na.omit(Saddlefivedayrunning5C$fivedayrunning),na.omit(Saddlefivedayrunning12C$fivedayrunning))
#they are not correlated at all, weird (Emily)
## > CTW: but few observtions (years) here



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

Saddlemintempthreeday0<-data.frame(year=rep(NA,34),GSLthreeday0C=rep(NA,34))
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

Saddlemintempthreedayneg3<-data.frame(year=rep(NA,34),GSLthreedayneg3C=rep(NA,34))
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



plot(Saddlemintempthreedayneg3$GSLthreedayneg3C,Saddlemintempthreeday0$GSLthreeday0C)


## > CTW: Not calculating this part, not needed in summer PCA
## > rewrite in tidyverse when ready
# ##### Frost free days ####
# #Use saddle temp to calculate #frost days in May and Sept
# head(Saddletemp)
# Saddletemp[1:100,]
# frost<-aggregate.data.frame(cbind(Saddletemp$min_tempTF,Saddletemp$min_tempTF3), by=list(year=Saddletemp$water_yr,month=Saddletemp$month),sum)
# colnames(frost)[3:4]<-c("frosts0","frostsneg3")
# frost<-frost[-which(frost$year==1981),]
# 
# #write.csv(frost,"~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/frost.csv",row.names=F,quote=F)
# head(frost)
# 
# frostseason<-aggregate.data.frame(cbind(Saddletemp$min_tempTF,Saddletemp$min_tempTF3),by=list(year=Saddletemp$water_yr,month=Saddletemp$season),sum)
# colnames(frostseason)[3:4]<-c("frosts0","frostsneg3")
# frostseason<-frostseason[-which(frostseason$year==1981),]
# #write.csv(frostseason,"~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/frostseason.csv",row.names=F,quote=F)


###### Saddle precip #####
## > CTW: not using Emily's code because all I need is total summer precip for each year for summer PCA, have that

# .
# .
# . 


##### Then run the AET script to get all those outputs #####
## > CTW: need this for sum_moisturedeficit and sum_PET


###### Saddle meltout date #####
## > CTW: also skipping Emily's meltout code because only updating vars for summer PCA


###### Getting all files together for dataset #####
#Saddle files to merge for full Saddle climate data set
#SaddleGDD #calendar year
#Saddlemoisture #calendar year

