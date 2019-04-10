# This is a script for the climate data calculations that went into creating the file NWT_ClimateData_2015_11_02.xlsx
# Made by Emily Farrer
# CTW slightly modified EF's code to be more generic, so can re-use on future climate datasets (i.e. EF wrote for NWT data through 2014 only)


summarizeTemp <- function(tempdat, date, tmin, tmax, tmean, outpath){
  # libraries needed
  require(lubridate)
  require(reshape)
  
  ##Saddle temp data
  Saddletemp <- get(tempdat)
  
  print("Checking data for NAs...")
  # ensure no NAs in tmin or tmax
  stopifnot(sum(is.na(Saddletemp[[tmin]]))==0)
  stopifnot(sum(is.na(Saddletemp[[tmax]]))==0)
  
  # if mean temp already present, check for NAs and calculate mean from tmin, tmax for any NAs
  if(tmean != "no"){
    # rename column
    colnames(Saddletemp)[colnames(Saddletemp)==tmean] <- "mean_temp"
    if(sum(is.na(Saddletemp$mean_temp))>0){
      print(paste0(sum(is.na(Saddletemp$mean_temp)), " NAs present in mean temp. Infilling by averaging ", tmin, " and ", tmax, "."))
      Saddletemp$mean_temp[is.na(Saddletemp$mean_temp)] <- (Saddletemp[[tmin]] + Saddletemp[[tmax]])/2
    }
    
  } else{
    print(paste0("Creating mean temp from ", tmin, " and ", tmax, "."))
    Saddletemp$mean_temp <- round((Saddletemp[[tmin]] + Saddletemp[[tmax]])/2, 2)
  }
  
  # check for extreme values (possible outliers) in dataset
  ## write in later...
  
  # rename date column
  colnames(Saddletemp)[which(colnames(Saddletemp) == date)] <- "date"
  
  # create and rename variables so matches Emily's colnames
  Saddletemp$year <- year(Saddletemp$`date`)
  Saddletemp$month <- month(Saddletemp$`date`)
  Saddletemp$day <- day(Saddletemp$`date`)
  
  colnames(Saddletemp)[which(colnames(Saddletemp)==tmin)] <- "min_temp"
  colnames(Saddletemp)[which(colnames(Saddletemp)==tmax)] <- "max_temp"
  
  #reorganize temp data frame
  Saddletemp <- Saddletemp[c("date", "year", "month", "day", "min_temp", "max_temp", "mean_temp")]
  Saddletemp <- Saddletemp[order(Saddletemp$date), ]
  
  
  ##### MAT and mean temp by season #####
  print("Calculating mean annual and seasonal temperatures...")
  
  #recalculate spr sum wnt fal based on the lake LTER ms where summer is Jun/Jul/Aug
  
  #define season2, summer is Jun/Jul/Aug, etc
  Saddletemp$season<-NA
  Saddletemp$season[Saddletemp$month%in%c(12,1,2)]<-"wnt"
  Saddletemp$season[Saddletemp$month%in%c(3,4,5)]<-"spr"
  Saddletemp$season[Saddletemp$month%in%c(6,7,8)]<-"sum"
  Saddletemp$season[Saddletemp$month%in%c(9,10,11)]<-"fal"
  
  #define water year (ecoyear), same as year for all months except 9, 10, 11, 12, for which it is the following year
  # > ctw: water year is Oct 1 - Sep 30.. Sep should not be included in this unless EF had a biological reason (?)
  # > e.g. is "water year" here what becomes "eco year" later?
  Saddletemp$water_yr<-Saddletemp$year
  Saddletemp$water_yr[Saddletemp$month > 8]<-Saddletemp$water_yr[Saddletemp$month> 8]+1
  
  # determine which water years are complete (total number of days will be 365 or 366 [leap year])
  yrs_keep <- sapply(split(Saddletemp$day, Saddletemp$water_yr), function(x) length(x) %in% 365:366)
  yrs_keep <- names(yrs_keep)[yrs_keep]
  
  #summer=Jun/Jul/Aug
  Saddletemp3long<-aggregate.data.frame(Saddletemp$mean_temp,by=list(season=Saddletemp$season,water_year=Saddletemp$water_yr),function(x){mean(x,na.rm=T)})
  Saddletemp3short<-cast(Saddletemp3long,water_year~season,value="x",fun=mean)
  
  #make a column with mean annual temp *based on sep-aug "water year" (eco year) 
  Saddletemp3short$MAT<-aggregate.data.frame(Saddletemp$mean_temp,by=list(Saddletemp$water_yr),function(x){mean(x,na.rm=T)})[,2]
  Saddletemp3short<-Saddletemp3short[Saddletemp3short$water_year %in% yrs_keep,]# remove 1981 and remove last year bc incomplete
  colnames(Saddletemp3short)<-c("water_year","fal_meanT","spr_meanT",	"sum_meanT","wnt_meanT","MAT")
  
  
  ##### GDD #####
  print("Calculating growing degree days...")
  #use Saddletemp to calculate growing degree days for a base of 5C (for pikas). Requires no NAs in temp columns.
  
  #If the mean daily temperature is lower than the base temperature then GDD=0
  #GDD = (Tmax +Tmin)/2 - Tbase
  #where Tmax is maximum daily temperature and is set equal to 25 when temperatures exceed 25 (or I think many people either don't set this or set it according to wikipedia to 30). The max temp in Saddle dataset is 25 so no need to do anything.
  #Tmin is the minimum dail temperature and is set equal to 5C when temperatures fall below 5C #alpine plant papers use 0 or 5 or -4 or -2
  #Tbase is the base temperature for the organism
  # In captivity, temperatures as low as 25.5°C have proved fatal (Smith 1974). (Shinderman 2015 ecology and evolution)
  
  #which((Saddletemp$max_temp+Saddletemp$min_temp)/2!=Saddletemp$mean_temp)#there are a lot of places where the average of max and min does not equal the mean_temp. looks like wherever infilling took place, but the means are not that off
  # > CTW: check difference with boxplot
  #boxplot((Saddletemp$max_temp+Saddletemp$min_temp)/2!=Saddletemp$mean_temp) # off by 1 degree at most, typically much less
  #max(Saddletemp$max_temp, na.rm = T)
  
  Saddletemp$min_tempforGDD<-ifelse(Saddletemp$min_temp<5,5,Saddletemp$min_temp)
  Saddletemp$GDD<-ifelse(((Saddletemp$min_tempforGDD+Saddletemp$max_temp)/2-5)<0,0,((Saddletemp$min_tempforGDD+Saddletemp$max_temp)/2-5))
  head(Saddletemp)
  plot(Saddletemp$GDD[1:300])
  # saddle growing degree days based on calendar year (summer GDDs include september GDDs)
  SaddleGDD<-aggregate.data.frame(Saddletemp$GDD,by=list(year=Saddletemp$year),function(x){sum(x,na.rm=T)})#this is a little odd becase there are still a lot of Growing Degree Days in Sept so this would count for that same year
  SaddleGDD<-SaddleGDD[SaddleGDD$year %in% yrs_keep,];colnames(SaddleGDD)[2]<-"GDD"
  # saddle growing degree days based on sep-aug water year (september GDDs not included with summer GDDs)
  SaddleGDDwateryear<-aggregate.data.frame(Saddletemp$GDD,by=list(year=Saddletemp$water_yr),function(x){sum(x,na.rm=T)})#this is using water year, use this for final climate excel file
  #SaddleGDDwateryear<-SaddleGDDwateryear[-1,];SaddleGDDwateryear<-SaddleGDDwateryear[-37,];colnames(SaddleGDDwateryear)[2]<-"GDD"
  SaddleGDDwateryear<-SaddleGDDwateryear[SaddleGDDwateryear$year %in% yrs_keep,];colnames(SaddleGDDwateryear)[2]<-"GDD"
  
  
  #Look for correlation between GDD and mean summer temp, they are highly correlated
  plot(Saddletemp3short$sum_meanT,SaddleGDD$GDD)
  
  #get growing degree days for jun/jul/aug *aggregated on water year
  head(Saddletemp)
  SaddleGDDwateryearseason<-aggregate.data.frame(Saddletemp$GDD,by=list(year=Saddletemp$water_yr, season=Saddletemp$season),function(x){sum(x,na.rm=T)})#this is using water year
  SaddleGDDwateryearsum<-subset(SaddleGDDwateryearseason,SaddleGDDwateryearseason$season=="sum")
  SaddleGDDwateryearsum<-SaddleGDDwateryearsum[SaddleGDDwateryearsum$year %in% yrs_keep,] # only keep years that have complete water year months
  colnames(SaddleGDDwateryearsum)[3]<-"GDDsum"
  
  
  ##### Five day running temps #####
  print("Calculating five-day running mean temperatures at 5°C and 12°C...")
  #Days after June 1 when the five day running mean of maximum temperature reaches 24C. From Veblen 2015
  head(Saddletemp)
  Saddletemp$fivedayrunning<-NA
  for(i in 3:(length(Saddletemp$max_temp)-3)){
    Saddletemp$fivedayrunning[i]<-mean(Saddletemp$max_temp[(i-2):(i+2)])
  }
  plot(Saddletemp$fivedayrunning)
  Saddletemp$fivedayrunning[300:360]
  
  #summer=Jun/Jul/Aug for max temp *aggregated on water year
  Saddletemp3longmax<-aggregate.data.frame(Saddletemp$max_temp,by=list(season=Saddletemp$season,water_year=Saddletemp$water_yr),function(x){mean(x,na.rm=T)})
  Saddletemp3shortmax<-cast(Saddletemp3longmax,water_year~season,value="x",fun=mean)
  #Saddletemp3shortmax<-Saddletemp3shortmax[-1,];Saddletemp3shortmax<-Saddletemp3shortmax[-37,] # drop 1981 and 2018
  Saddletemp3shortmax<-Saddletemp3shortmax[Saddletemp3shortmax$water_year %in% yrs_keep,]# drop yrs that aren't complete
  colnames(Saddletemp3shortmax)<-c("water_year","fal_maxT","spr_maxT","sum_maxT","wnt_maxT")
  mean(Saddletemp3shortmax$sum_maxT)#mean summer max temp is 12.5 (# >12.5 through 2017 )
  mean(Saddletemp3short$sum_meanT)#mean summer temp is 8.1 (#8.0 through 2017)
  
  #The mean max temp in summer (jun/jul/aug) is 12.5. If you use 5C (because of the pikas), then you have to go back to March to get positive numbers. 15 doesn't work you get NAs because in a few years you never reach a 5-day mean of 15. 12C works if you start in May. 
  head(Saddletemp)
  Saddlefivedayrunning5C<-data.frame(year=rep(NA,length(unique(Saddletemp$year))),fivedayrunning=rep(NA,length(unique(Saddletemp$year))))
  for(i in 1:length(unique(Saddletemp$year))){
    years<-unique(Saddletemp$year)
    tempdata<-subset(Saddletemp,year==years[i])
    Saddlefivedayrunning5C$year[i]<-years[i]
    Saddlefivedayrunning5C$fivedayrunning[i]<-which(tempdata$fivedayrunning>5)[1]-which(tempdata$month==3&tempdata$day==1)+1
  }
  
  Saddlefivedayrunning5C <- Saddlefivedayrunning5C[Saddlefivedayrunning5C$year %in% yrs_keep,]
  
  Saddlefivedayrunning12C<-data.frame(year=rep(NA,length(unique(Saddletemp$year))),fivedayrunning=rep(NA,length(unique(Saddletemp$year))))
  for(i in 1:length(unique(Saddletemp$year))){
    years<-unique(Saddletemp$year)
    tempdata<-subset(Saddletemp,year==years[i])
    Saddlefivedayrunning12C$year[i]<-years[i]
    Saddlefivedayrunning12C$fivedayrunning[i]<-which(tempdata$fivedayrunning>12)[1]-which(tempdata$month==5&tempdata$day==1)+1
  }
  Saddlefivedayrunning12C <- Saddlefivedayrunning12C[Saddlefivedayrunning12C$year %in% yrs_keep,]
  
  #
  plot(Saddlefivedayrunning5C,Saddlefivedayrunning12C)
  #they are not correlated at all, weird
  
  
  ##### GSL #####
  print("Calculating growing season length...")
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
  
  Saddlemintempthreeday0<-data.frame(year=rep(NA,length(unique(Saddletemp$year))),GSLthreeday0C=rep(NA,length(unique(Saddletemp$year))))
  for(i in 1:length(unique(Saddletemp$year))){
    years<-unique(Saddletemp$year)
    tempdata<-subset(Saddletemp,year==years[i])
    Saddlemintempthreeday0$year[i]<-years[i]
    sprdata<-tempdata$mintempthreeday0[1:196]#july15 is julian day 196
    faldata<-tempdata$mintempthreeday0[197:365]
    sprtempday<-ifelse(any(sprdata==3), max(which(sprdata==3)), NA)
    faltempday<-ifelse(any(faldata==3), min(which(faldata==3))+196, NA)
    Saddlemintempthreeday0$GSLthreeday0C[i]<-faltempday-sprtempday
  }
  
  Saddlemintempthreeday0 <- Saddlemintempthreeday0[Saddlemintempthreeday0$year %in% yrs_keep,]
  plot(Saddlemintempthreeday0$year,Saddlemintempthreeday0$GSLthreeday0C,type="l")
  
  
  #having the cutoff be three days where min temp is -3C
  Saddletemp$min_tempTF3<-ifelse(Saddletemp$min_temp<(-3),T,F)
  head(Saddletemp)
  
  Saddletemp$mintempthreedayneg3<-NA
  for(i in 3:(length(Saddletemp$min_temp)-3)){
    Saddletemp$mintempthreedayneg3[i]<-sum(Saddletemp$min_tempTF3[(i-2):(i)])
  }
  
  Saddletemp[100:200,]
  
  Saddlemintempthreedayneg3<-data.frame(year=rep(NA,length(unique(Saddletemp$year))),GSLthreedayneg3C=rep(NA,length(unique(Saddletemp$year))))
  for(i in 1:length(unique(Saddletemp$year))){
    years<-unique(Saddletemp$year)
    tempdata<-subset(Saddletemp,year==years[i])
    Saddlemintempthreedayneg3$year[i]<-years[i]
    sprdata<-tempdata$mintempthreedayneg3[1:196]#july15 is julian day 196
    faldata<-tempdata$mintempthreedayneg3[197:365]
    sprtempday<-ifelse(any(sprdata==3), max(which(sprdata==3)), NA)
    faltempday<-ifelse(any(sprdata==3), min(which(faldata==3))+196, NA)
    Saddlemintempthreedayneg3$GSLthreedayneg3C[i]<-faltempday-sprtempday
  }
  
  Saddlemintempthreedayneg3 <- Saddlemintempthreedayneg3[Saddlemintempthreedayneg3$year %in% yrs_keep,]
  plot(Saddlemintempthreedayneg3$GSLthreedayneg3C,Saddlemintempthreeday0$GSLthreeday0C)
  
  
  ##### Frost free days ####
  print("Calculating frost free days...")
  #Use saddle temp to calculate #frost days in May and Sept **aggregated on water year
  head(Saddletemp)
  Saddletemp[1:100,]
  frost<-aggregate.data.frame(cbind(Saddletemp$min_tempTF,Saddletemp$min_tempTF3), by=list(year=Saddletemp$water_yr,month=Saddletemp$month),sum)
  colnames(frost)[3:4]<-c("frosts0","frostsneg3")
  frost<-frost[frost$year %in% yrs_keep,]
  head(frost)
  
  # *aggregated on water year
  frostseason<-aggregate.data.frame(cbind(Saddletemp$min_tempTF,Saddletemp$min_tempTF3),by=list(year=Saddletemp$water_yr,month=Saddletemp$season),sum)
  colnames(frostseason)[3:4]<-c("frosts0","frostsneg3")
  frostseason<-frostseason[frostseason$year %in% yrs_keep,]
  
  ##### FINISHING #####
  print(paste("Writing out temperature summaries datasets to", outpath, "and returning compiled temperature summaries to global environment"))
  # write out datasets:
  ## annual and seasonal temp summaries
  write.csv(Saddletemp3short, paste0(outpath, "Saddletemp3short.csv"), row.names = F, quote = F)
  write.csv(Saddletemp3shortmax, paste0(outpath, "Saddletemp3shortmax.csv"), row.names = F, quote = F)
  ## growing degree days summarized on sep-aug ecoyear
  write.csv(SaddleGDD,paste0(outpath, "SaddleGDD.csv"),row.names=F,quote=F)
  write.csv(SaddleGDDwateryear,paste0(outpath, "SaddleGDDwateryear.csv"),row.names=F,quote=F)
  write.csv(SaddleGDDwateryearsum,paste0(outpath, "SaddleGDDwateryearsum.csv"),row.names=F,quote=F)
  ## 3day minimum temp below 0C and 3C
  write.csv(Saddlemintempthreeday0,paste0(outpath, "Saddlemintempthreeday0.csv"),row.names=F,quote=F)
  write.csv(Saddlemintempthreedayneg3,paste0(outpath, "Saddlemintempthreedayneg3.csv"),row.names=F,quote=F)
  ## running temp
  write.csv(Saddlefivedayrunning5C, paste0(outpath, "Saddlefivedayrunning5C.csv"), row.names = F, quote = F)
  write.csv(Saddlefivedayrunning12C, paste0(outpath, "Saddlefivedayrunning12C.csv"), row.names = F, quote = F)
  ## frost, frost season
  write.csv(frost,paste0(outpath,"frost.csv"),row.names=F,quote=F)
  write.csv(frostseason,paste0(outpath,"frostseason.csv"),row.names=F,quote=F)
  
  # return compiled temp dataset
  #SaddleGDD #calendar year
  compiled_temp <- cbind(Saddletemp3short, sum_GDD = SaddleGDDwateryearsum$GDDsum, GDD = SaddleGDDwateryear$GDD,fivedayrunning5C=Saddlefivedayrunning5C$fivedayrunning,fivedayrunning12C=Saddlefivedayrunning12C$fivedayrunning, GSLthreeday0C = Saddlemintempthreeday0$GSLthreeday0C, GSLthreedaynt3C = Saddlemintempthreedayneg3$GSLthreedayneg3C)
  return(compiled_temp)
}


summarizePrecip <- function(pptdat, date, precip, correction, outpath){
  
  require(lubridate)
  require(reshape)
  
  ###### Saddle precip #####
  #Saddleprecip<-read.csv("~/Dropbox/EmilyComputerBackup/Documents/NWTlter/Saddletemp&ppt/infilling_sdl_precip_1982-2014_distribution_(all_09-14_infilled).csv")#na.strings = c(".")
  # > CTW: read in infilled through 2017 precip (NSF proposal data + 2015-2017 infilled by ctw)
  # > IMPORTANTE: all precip values in this have already been snow corrected as Emily did (months not JJA * 0.39)
  
  Saddleprecip <- get(pptdat)
  
  # ensure no NAs in tmin or tmax
  stopifnot(sum(is.na(Saddleprecip[[precip]]))==0)
  
  # rename date and precip column
  colnames(Saddleprecip)[which(colnames(Saddleprecip) == date)] <- "date"
  colnames(Saddleprecip)[which(colnames(Saddleprecip) == precip)] <- "precip_mm"
  
  # create and rename variables so matches Emily's colnames
  Saddleprecip$year <- year(Saddleprecip$`date`)
  Saddleprecip$month <- month(Saddleprecip$`date`)
  
  # reorganize columns
  Saddleprecip <- Saddleprecip[c("date", "year", "month", "precip_mm")]
  
  # > CTW: don't need to execute these lines because already infilled (no NAs, and already snow-corrected following Emily's method)
  # > NOTE: For more accuracy, may want to update 0.39 to average adjustment numbers reported in Jennings et al. 2018
  # > Also CTW doesn't think September should have been snow corrected (have read Oct - May), but followed Emily's method for consistency
  # #replace NA with 0 in the precip_mm column (because the infilling accounts for precip in those days)
  # Saddleprecip$precip_mm[which(is.na(Saddleprecip$precip_mm)==T)]<-0
  
  #Correct for blowing snow, multiply all non summer (not JJA) months by .39, MW Williams et al. 1998 Atmospheric Environment
  if(correction){
    ind<-which(!Saddleprecip$month%in% 6:8)
    Saddleprecip$precip_mm[ind]<-Saddleprecip$precip_mm[ind]*.39 ##don't run this more than once!!!
  }
  
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
  Saddleprecip$water_yr[Saddleprecip$month>8]<-Saddleprecip$water_yr[Saddleprecip$month>8]+1
  
  # determine which water years are complete (total number of days will be 365 or 366 [leap year])
  yrs_keep <- sapply(split(Saddleprecip$date, Saddleprecip$water_yr), function(x) length(x) %in% 365:366)
  yrs_keep <- names(yrs_keep)[yrs_keep]
  
  #summer=Jun/Jul/Aug
  Saddleprecip3long<-aggregate.data.frame(Saddleprecip$precip_mm,by=list(season=Saddleprecip$season,water_year=Saddleprecip$water_yr),function(x){sum(x,na.rm=T)})
  Saddleprecip3short<-cast(Saddleprecip3long,water_year~season,value="x",fun=sum)
  #make a column with total annual precip
  Saddleprecip3short$tot_precip<-aggregate.data.frame(Saddleprecip$precip_mm,by=list(Saddleprecip$water_yr),function(x){sum(x,na.rm=T)})[,2]
  colnames(Saddleprecip3short)<-c("water_year","fal_precip","spr_precip",	"sum_precip","wnt_precip","tot_precip")
  Saddleprecip3short<-Saddleprecip3short[Saddleprecip3short$water_year %in% yrs_keep,]
  
  
  ###### FINISHING #####
  #export daily values for AET # values are corrected for blowing snow
  write.csv(Saddleprecip,paste0(outpath, "Saddleprecip_summary.csv"),row.names=F,quote=F)
  
  # return compiled precip summary
  compiled_precip <- merge(Saddleprecip3short)
  return(compiled_precip)
  
}


##### Then run the AET script to get all those outputs #####



summariseSnowmelt <- function(snowdat, date, site, depth){
  
  require(lubridate)
  require(reshape)
  # read in sdl grid plot classes used by EF in 2015 NWT renewal ("class_3")
  classinfo <- read.csv("extended_summer/output_data/sdlprodsnowclass.csv", stringsAsFactors = F)
  
  ###### Saddle meltout date #####
  
  #saddlesnow<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/NWTlter/SaddleSnowDepth/NWT_SaddleGridSnow_1992.2014Emily.csv")
  # > CTW: will try using data on NWT portal (current though 2017, not QAd by me)
  saddlesnow <- get(snowdat)
  
  head(saddlesnow)
  tail(saddlesnow)
  which(is.na(saddlesnow$mean_depth)==T)
  
  # rename date and precip column
  colnames(saddlesnow)[which(colnames(saddlesnow) == date)] <- "date"
  colnames(saddlesnow)[which(colnames(saddlesnow) == depth)] <- "mean_depth"
  colnames(saddlesnow)[which(colnames(saddlesnow) == site)] <- "sort_num"
  
  # create and rename variables so matches Emily's colnames
  saddlesnow$year <- year(saddlesnow$`date`)
  saddlesnow$month <- month(saddlesnow$`date`)
  saddlesnow$day <- day(saddlesnow$`date`)
  
  # reorganize columns
  saddlesnow <- saddlesnow[c("date", "year", "month", "day", "sort_num", "mean_depth")]
  saddlesnow <- saddlesnow[order(saddlesnow$date),]
  
  #define water year, same as year for all months except 10, 11, 12, for which it is the following year
  #I'm not sure why the sept lin is commented out, it is probably b/c I didn't need to ever reference water year for these meltout calculations
  saddlesnow$water_yr<-saddlesnow$year
  saddlesnow$water_yr[saddlesnow$month==9]<-saddlesnow$water_yr[saddlesnow$month==9]+1
  saddlesnow$water_yr[saddlesnow$month==10]<-saddlesnow$water_yr[saddlesnow$month==10]+1
  saddlesnow$water_yr[saddlesnow$month==11]<-saddlesnow$water_yr[saddlesnow$month==11]+1
  saddlesnow$water_yr[saddlesnow$month==12]<-saddlesnow$water_yr[saddlesnow$month==12]+1
  
  # determine which water years are complete.. 
  # come back to this: not all months across years sampled consistently (sometimes started in late fall, sometimes started in jan; some went thru early summer, some not)
  snow_dates <- unique(saddlesnow[c("month", "water_yr")])
  #yrs_keep <- sapply(split(snow_dates$month, snow_dates$water_yr), function(x) length(x))
  #yrs_keep <- names(yrs_keep)[yrs_keep>0]
  
  #julian day, jan 1 is 0
  saddlesnow$julian<-NA
  for(i in 1:dim(saddlesnow)[1]){
    saddlesnow$julian[i]<-julian(as.Date(saddlesnow$date[i]),origin = as.Date(paste(saddlesnow$year[i],"01","01",sep="/")))+1
  }
  
  
  saddlemeltout<-data.frame(year=rep(NA,length(unique(saddlesnow$year))*length(unique(saddlesnow$sort_num))),
                            sort_num=rep(NA,length(unique(saddlesnow$year))*length(unique(saddlesnow$sort_num))),
                            julian=rep(NA,length(unique(saddlesnow$year))*length(unique(saddlesnow$sort_num))))
  i=1
  #the loop assesses the period between Jan 1 and Aug 30 for snow. 
  for(p in 1:length(unique(saddlesnow$sort_num))){
    for(t in unique(saddlesnow$year)){# number of years
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
  #warnings occur from the plots that had no snow (that's fine). 0 means the plot had no snow over the entire time period, NA means the plot never melted out (by the last survey date)
  
  #check how many zero snow plots/years there are
  temp<-aggregate.data.frame(saddlesnow$mean_depth,by=list(saddlesnow$water_yr,saddlesnow$sort_num),function(x){sum(x,na.rm=T)})
  temp[which(temp$x==0),]
  
  #export for plot level data
  #use snowprod data file to get the class of each plot
  # snowprod<-read.csv("/Users/farrer/Dropbox/NWT_data/NWT_SnowXProd.csv",na.strings = c("."))
  # snowprod$plot<-as.factor(snowprod$plot)
  # head(snowprod)
  # #if you ever want to use the productivity data: in years before 2008, whole cushion plants were harvested for anpp which makes the anpp super high in dry and moist meadow. so I'm changing all of those to NA
  # ind<-which(snowprod$year<2008&snowprod$class_3%in%c("DM","FF"))
  # snowprod$anpp[ind]<-NA
  
  #calculate average meltout date, not including 0s or NAs for the entire saddle and for each community type
  #snowprod has class_3 info in it
  # head(saddlemeltout)
  # classinfo<-snowprod[1:88,c("sort_num","class_3")]
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
  
  sdlmeltout<-merge(sdlmeltout,sdlmeltoutclassshort)
  #colnames(sdlmeltout)[2:10]<-c("sdl_meltout","dm_meltout","ff_meltout","mm_meltout","rock_meltout","sb_meltout","sf_meltout","st_meltout","wm_meltout")
  # rename colnames, flexible for classes present and column position
  colnames(sdlmeltout)[grepl("sdlm", colnames(sdlmeltout), ignore.case = T)] <- "sdl_meltout"
  ## lower-case all
  colnames(sdlmeltout)<- casefold(colnames(sdlmeltout))
  # append _meltout to all but sdl_meltout (i.e. where not already appended)
  colnames(sdlmeltout)[grepl("DM|FF|MM|ro|SB|SF|ST|WM", colnames(sdlmeltout), ignore.case = T)]<- paste0(colnames(sdlmeltout)[grepl("DM|FF|MM|ro|SB|SF|ST|WM", colnames(sdlmeltout), ignore.case = T)], "_meltout")
  
  # finishing
  write.csv(sdlmeltout, "extended_summer/output_data/sdlmeltout.csv", row.names = F, quote = F)
  return(sdlmeltout)
  
}


compileClimate <- function(temp=NULL, precip=NULL, AET=NULL, snow=NULL){
  
  ###### Getting all files together for dataset #####
  #Saddle files to merge for full Saddle climate data set
  #SaddleGDD #calendar year
  #Saddlemoisture #calendar year
  
  # #all range from 1982 to present (most recent available date on EDI Data Portal)
  # Saddletemp3short #(jun/jul/aug=summer)
  # Saddleprecip3short #(jun/jul/aug=summer) 
  # Saddlemoisturewateryear #deficit based on water year jun/jul/aug
  # SaddleGDDwateryear
  # Saddlefivedayrunning5C
  # Saddlefivedayrunning12C
  
  # extract column of year for merging data frames
  base <- get(c(temp, precip, AET, snowmelt)) # prioritize order by datasets with most years
  # initiate data frame with eco_year
  ClimateAll <- data.frame(eco_year = base[,1])
  # join in datasets specified in function
  if(!is.null(temp)){
    tempdat <- get(temp)
    colnames(tempdat)[1] <- "eco_year"
    ClimateAll <- merge(ClimateAll, tempdat, by.x = "eco_year", by.y = "water_year", all.x = T)
  }
  if(!is.null(precip)){
    pptdat <- get(precip)
    colnames(pptdat)[1] <- "eco_year"
    ClimateAll <- merge(ClimateAll, pptdat, by.x = "eco_year", by.y = "water_year", all.x = T)
  }
  if(!is.null(AET)){
    aetdat <- get(AET)
    colnames(aetdat)[colnames(aetdat)=="def"] <- "moisturedeficit"
    ClimateAll <- merge(Climateall, aetdat, by.x = "eco_year", by.y = "year", all.x = T)
  }
  if(!is.null(snowmelt)){
    meltdat <- get(snowmelt)
    colnames(meltdat)[1]<-"eco_year"
    meltdat$rock_meltout<-NULL
    meltdat$sf_meltout<-NULL
    meltdat$st_meltout<-NULL
    ClimateAll<-merge(ClimateAll,sdlmeltout, by ="eco_year", all.x = T)
  }
  
  # finishing
  write.csv(ClimateAll, paste0(outpath, "ClimateAll.csv"), row.names = F, quote = F)
  return(ClimateAll)
}










# ClimateAll<-cbind(Saddletemp3short,Saddleprecip3short[,-1],moisturedeficit=Saddlemoisturewateryear[,-1],GDD=SaddleGDDwateryear[,-1],fivedayrunning5C=Saddlefivedayrunning5C[,-1],fivedayrunning12C=Saddlefivedayrunning12C[,-1])
# head(ClimateAll)
# rownames(ClimateAll)<-1:33
# 
# plot(1982:2014,wholesite$spr_ppt,type="l",ylim=c(100,700))
# points(1982:2014,ClimateAll$spr_precip,type="l",col=2)
# plot(1982:2014,ClimateAll$moisturedeficit,type="l")


#Merge with climate data from whole site and saddle prod
#Whole site data
# wholesite<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/NWTlter/wholesite/NWT_SiteData_2015-06-15.csv",na.strings = c("."))
# ind<-which(wholesite$water.year%in%1992:1997)
# wholesite[ind,c("saddleprod","dmsaddleprod","ffsaddleprod")]<-NA
# head(wholesite)
# 
# ClimateAll<-cbind(ClimateAll,wholesite[,c(7,9)],wholesite[,15:20])
# ClimateAll<-cbind(ClimateAll,wholesite[,55:60])
# head(ClimateAll)

