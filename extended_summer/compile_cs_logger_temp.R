# compile and qa campbell scientific saddle logger temp, all years
# (streamlined version after comparing all saddle temperature datasets)


# script purpose:
# read in all campbell scientific logger datasets, and sdl, d1, and c1 chart temp datasets for comparison
# tidy datasets
# determine routine checks to flag suspect values relative to nearby temp trends (i.e. comparative datasets)
# flag values
# write out QA dataset with flags for next step (infilling)



# -- SETUP ------
rm(list = ls())
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN")

# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("edi_functions.R")



# -- GET DATA -----
#campbell logger 1990- # no colnames in data file so read in from copy/paste url
cr21x <- read_csv("http://pasta.lternet.edu/package/data/eml/knb-lter-nwt/78/2/74edfb5a907b5d4960d1e1dbe08faba4", col_names = FALSE, na = na_vals, trim_ws = T) %>% as.data.frame()
cr21xeml <- readLines("https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-nwt.78.2&contentType=application/xml")
cr21x_names <- cr21xeml[grep("attributeName", cr21xeml)] %>%
  str_extract(">.*<") %>% 
  gsub("<|>", "",.)
#set colnames for cr21x data
colnames(cr21x) <- cr21x_names

# cr23x and cr1000 logger data
#crlogs <- getTabular() # most current logger dataset not on EDI yet, provided by SCE
crlogs <- read.csv("/Users/serahsierra/Documents/nwt_lter/unpub_data/sdlcr23x-cr1000.daily.ml.data.csv",
                   strip.white = T, na.strings = c("", " ", ".", NA, NaN, "NA", "NaN"))
# sdl chart
sdl <- getTabular(413) %>% as.data.frame()
# d1 chart
d1 <- getTabular(412) %>% as.data.frame()
# c1 chart
c1 <- getTabular(411) %>% as.data.frame()


# -- REVIEW DATA -----
# review how dats read in
glimpse(cr21x) # no flag cols, date is date class
glimpse(crlogs) # flag cols present, date is character
glimpse(sdl) # flags cols present
glimpse(d1) # flag cols present
glimpse(c1) # flag cols present

# clean up cr23x and cr1000 data
# drop cols not needed in cr logger dataset (i.e. only need airtemp cols up thru avg airtemp, flag cols not useful since only n or NA)
crlogs <- crlogs[,1:12]
# convert date to Date class
crlogs$date <- as.Date(crlogs$date, format = "%Y-%m-%d")
# unique values of flags? and frequency of their occurrence?
sapply(crlogs[grepl("flag", colnames(crlogs))], function(x) summary(as.factor(x))) # only n's, correspond to no flag -- not useful
# drop flag cols since nothing flagged, add month col, and clean up names
crlogs <- mutate(crlogs, mon = month(date)) %>%
  rename(doy = jday,
         yr = year) %>%
  dplyr::select(LTER_site:date, yr, mon, doy, airtemp_max, airtemp_min, airtemp_avg)

# clean up 21x data 
# drop non airtemp cols in cr21x
cr21x <- cr21x[grepl("date|Julian|maximum temp|minimum temp|average temp", colnames(cr21x))] %>%
  # add logger, LTER_site, and local_site cols so simlar to other datasets
  mutate(LTER_site = "NWT",
         local_site = "sdl", 
         logger = "cr21x",
         yr = year(date),
         mon = month(date)) %>%
  # rename cols to match other datasets
  rename(airtemp_max = 'maximum temperature',
         airtemp_min = 'minimum temperature',
         airtemp_avg = 'average temperature',
         doy = 'Julian day') %>%
  # select same cols as in cr23x and cr1000 datasets + time of temp cols
  dplyr::select(c(colnames(crlogs), cr21x_names[grepl("^time.*temperature", cr21x_names)])) %>%
  rename(time_airtemp_max = 'time of maximum temperature',
         time_airtemp_min = 'time of minimum temperature')
# clean up 
rm(cr21x_names, cr21xeml)

# review flags in reference datasets 
# sdl
sapply(sdl[grepl("flag", colnames(sdl))], function(x) summary(as.factor(x))) # 2 types of flags, type 1 used most often
# > saddle chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available)
# d1
sapply(d1[grepl("flag", colnames(d1))], function(x) summary(as.factor(x))) # 3 types of flags.. type 1 and type 2 nearly equally used
# > d1 chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available); 3 not explained
# c1
sapply(c1[grepl("flag", colnames(c1))], function(x) summary(as.factor(x))) # 2 types of flags (same as sdl), mostly infilled via type 1
# > c1 chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available)
# >> conclusion: keep flag cols in chart temp datasets, useful info

# # preface temp and flag names with name of data source (**ctw does this later in script**)
# colnames(cr21x)[grepl("^air|^flag", colnames(cr21x))] <- paste0("cr_", colnames(cr21x)[grepl("temp", colnames(cr21x))])
# colnames(crlogs)[grepl("^air|^flag", colnames(crlogs))] <- paste0("cr_", colnames(crlogs)[grepl("temp", colnames(crlogs))])
# colnames(sdl)[grepl("^air|^flag", colnames(sdl))] <- paste0("sdl_", colnames(sdl)[grepl("temp", colnames(sdl))])
# colnames(d1)[grepl("^air|^flag", colnames(d1))] <- paste0("d1_", colnames(d1)[grepl("temp", colnames(d1))])
# colnames(c1)[grepl("^air|^flag", colnames(c1))] <- paste0("c1_", colnames(c1)[grepl("temp", colnames(c1))])
# 


# -- TIDY TEMPERATURE DATASETS -----
# function to tidy temp (this could be made generic for ppt too..)
tidytemp <- function(dat, datasource = NA, sep = "_", special = "flag", dropcol = NA){
  #if cols to drop, drop
  if(!is.na(dropcol)){
   dat <- dat[!colnames(dat) %in% dropcol] 
  }
  
  # gather temp and any special cols
  # id start of temp cols
  temp_pos <- min(grep("temp", colnames(dat)))
  dat_long <- dat %>%
    gather(met, temp, temp_pos:ncol(.)) %>%
    arrange(met, date)
  
  # if special cols exist, pull out special cols and rejoin wide-form
  if(!is.na(special)){
  tempspecial <- dat_long %>%
    filter(grepl(special, met)) %>%
    mutate(met = gsub(paste0(special,"_"), "", met))
  # rename temp col as special val
  colnames(tempspecial)[which(colnames(tempspecial) == "temp")] <- special
  
  # drop special vals from long-form dat and join wide to temp vals
  dat_long <- subset(dat_long, !grepl(special, met)) %>%
    # add month and year
    mutate(yr = year(date),
           mon = month(date),
           doy = yday(date)) %>%
    left_join(tempspecial) %>%
    dplyr::select(LTER_site:date, yr:doy, met:ncol(.)) 
  }
  
  # if desired, prefix temp and special col colname with datasource
  if(!is.na(datasource)){
  colnames(dat_long)[colnames(dat_long) %in% c("temp", special)] <- paste(datasource, colnames(dat_long)[colnames(dat_long) %in% c("temp", special)], sep = sep)
  }
  
  # return tidy dataset and clean up environment
  return(dat_long)
  rm(tempspecial, temp_pos)
}

# tidy chart temp datatsets
sdl_long <- tidytemp(sdl, datasource = "sdl", dropcol = "airtemp_avg")
d1_long <- tidytemp(d1, datasource = "d1", dropcol = "airtemp_avg")
c1_long <- tidytemp(c1, datasource = "c1", dropcol = "airtemp_avg")

# tidy logger datasets
cr21x_long <- tidytemp(cr21x, datasource = "cr", special = "time", dropcol = "airtemp_avg")
crlogs_long <- tidytemp(crlogs, datasource = "cr", special = NA, dropcol = "airtemp_avg")

  # # lag and lead temp by min/max temp
  # group_by(met) %>%
  # mutate(lag1_c1temp = lag(c1_temp),
  #        lead1_c1temp = lead(c1_temp)) %>%
  # ungroup() %>%
  # # diff temp val from lag and lead
  # mutate(delta_c1lag = c1_temp - lag1_c1temp,
  #        delta_c1lead = c1_temp - lead1_c1temp) %>%
  # # flag anomoly as +4sd departure from lag and lead temps
  # group_by(met, mon) %>%
  # mutate(delta_c1thresh_lag = (mean(abs(delta_c1lag), na.rm = T)) + 4*(sd(abs(delta_c1lag), na.rm = T)),
  #        delta_c1thresh_lead = mean(abs(delta_c1lead), na.rm = T) + 4*(sd(abs(delta_c1lag), na.rm = T))) %>%
  # ungroup() %>%
  # # flag departures
  # mutate(flag_c1deltalag = ifelse(abs(delta_c1lag)> delta_c1thresh_lag, 1, 0),
  #        flag_c1deltalead = ifelse(abs(delta_c1lead)> delta_c1thresh_lead, 1, 0)) %>%
  # dplyr::select(date, yr, mon, met:ncol(.))



# how many tmax temps occurred between 11pm and 1am?
nrow(subset(cr21x_long, met == "airtemp_max" & cr_time > 2300 | cr_time < 100)) #711! boo.
View(subset(cr21x_long, met == "airtemp_max" & cr_time > 2300 | cr_time < 100))
# what is the breakdown of count tmax where timestamp between 2300 & 0100 by month?
group_by(subset(cr21x_long, met == "airtemp_max" & cr_time > 2300 | cr_time < 100), mon) %>%
  summarize(ct = length(cr_time)) # most occur in winter months (e.g. 118 in december), but there are a decent amount in summer months
# what is the breakdown of count tmin where timestamp between 900 & 1700 by month?
group_by(subset(cr21x_long, met == "airtemp_min" & cr_time > 900 & cr_time < 1700), mon) %>%
  summarize(ct = length(cr_time)) # not so many.. but expect 0 (esp in summer months)
# conclusion: time is perhaps not reliable for flagging if clock sometimes malfunctioning. drop and combine cr21x with cr23x and cr1000 logger


# all cr logger datasets -- stack both cr21x and cr23x/cr1000 datasets then tidy
## stack, removing time cols from cr21x dataset
crall_long <- rbind(cr21x_long[,!grepl("time", colnames(cr21x_long))], crlogs_long) %>%
  arrange(met,date)
# clean up
rm(cr21x_long, crlogs_long)

# join comparative chart datasets to cr logger data
crall_long_master <- left_join(crall_long, sdl_long) %>%
  left_join(dplyr::select(d1_long, -local_site)) %>%
  left_join(dplyr::select(c1_long, -local_site))



# -- FUNCTION FOR PLOTTING SUSPECT TEMP VALUES -----
# function to panel plot flagged data
visual_qa <- function(dat, qadat, sorttime = "date"){
  # initiate list for storing ggplots
  plot_list <- list()
  # id temperature cols in reference data frame
  tempcols <- colnames(dat)[grepl("temp", colnames(dat))]
  
  for(m in c("airtemp_max", "airtemp_min")){
    tempdf <- qadat[qadat$met == m,] %>% as.data.frame()
    # order by preferred time sort (default is date)
    tempdf <- tempdf[order( tempdf[,which(colnames(tempdf) == sorttime)]),]
    
    for(d in as.character(tempdf$date)){
      d <- as.Date(d, format = "%Y-%m-%d")
      tempplot <- ggplot(data = subset(dat, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1))) +
        geom_line(aes(date, cr_temp)) +
        geom_point(aes(date, cr_temp)) +
        # circle the flagged value in red
        geom_point(data = subset(dat, met == m & date == as.Date(d)),
                   aes(date, cr_temp), col = "red", pch  = 1, size = 3) +
        labs(y = gsub("airtemp_", "T", m),
             x = d) +
        # add sdl chart temp for comparison (purple dots)
        geom_line(aes(date, sdl_temp), col = "purple") +
        geom_point(aes(date, sdl_temp), col = "purple", pch = 1) +
        geom_line(aes(date, d1_temp), col = "steelblue2") +
        geom_point(aes(date, d1_temp), col = "steelblue4", pch = 1) +
        geom_line(aes(date, c1_temp), col = "forestgreen") +
        geom_point(aes(date, c1_temp), col = "darkgreen", pch = 1) +
        theme_bw()
      
      # store plot in a list
      plot_list[length(plot_list)+1] <- list(tempplot)
    }
  }
  return(plot_list)
}


# -- QA SENSOR FAILS (OBVIOUS OUTLIERS) -----
# create working copy
working_dat <- crall_long_master
# add empty col for qa flags added
working_dat$qa_flag <- NA

# look at tails for any obvious bad values
## logger temp, split by logger
with(working_dat, lapply(split(cr_temp, paste(met, logger)), function(x) tail(sort(x)))) #31.45.. Jen Morse said she thinks max T shouldn't exceed 30
with(working_dat, lapply(split(cr_temp, paste(met, logger)), function(x) tail(sort(x, decreasing = T)))) #cr21x: -75 and -6999 in airtemp_min; cr1000: -187 tmin

## chart data
#sdl
with(working_dat, lapply(split(sdl_temp, met), function(x) tail(sort(x))))
with(working_dat, lapply(split(sdl_temp, met), function(x) tail(sort(x, decreasing = T)))) #-38 is kind of a jump from other tmin, but not impossible value
#d1
with(working_dat, lapply(split(d1_temp, met), function(x) tail(sort(x))))
with(working_dat, lapply(split(d1_temp, met), function(x) tail(sort(x, decreasing = T))))
#c1
with(working_dat, lapply(split(c1_temp, met), function(x) tail(sort(x))))
with(working_dat, lapply(split(c1_temp, met), function(x) tail(sort(x, decreasing = T))))

# flag: -187 in cr1000; anything < -70 in cr21x (-75 and -6999)
working_dat$qa_flag[working_dat$cr_temp < -50 & !is.na(working_dat$cr_temp)] <- "sensor fail"
# remove bad values from working cr_temp before moving on
working_dat$cr_temp[!is.na(working_dat$qa_flag)] <- NA



# -- FLAG DAILY DEPARTURES FROM COMPARATIVE DATASETS -----
# function to difference daily logger temp from comparative chart dataset daily temps
diff_daily <- function(dat){
  dat %>%
  mutate(cr_diff_sdl = abs(cr_temp-sdl_temp),
         cr_diff_d1 = abs(cr_temp-d1_temp),
         cr_diff_c1 = abs(cr_temp-c1_temp)) %>%
  group_by(logger, met, mon) %>%
  # set threshold for sdl logger deviance at 3sd away from the absolute average difference (by logger, metric, and month)
  mutate(thresh_diff_sdl = mean(cr_diff_sdl, na.rm = T) + (3*sd(cr_diff_sdl, na.rm = T)),
         thresh_diff_d1 = mean(cr_diff_d1, na.rm = T) + (3*sd(cr_diff_d1, na.rm = T)),
         thresh_diff_c1 = mean(cr_diff_c1, na.rm = T) + (3*sd(cr_diff_c1, na.rm = T))) %>%
  ungroup() %>%
  # flag logger value if exceeds daily diff threshold for chart comparative datasets
  mutate(flag_diffsdl = cr_diff_sdl > thresh_diff_sdl,
         flag_diffd1 = cr_diff_d1 > thresh_diff_d1,
         flag_diffc1 = cr_diff_c1 > thresh_diff_c1)
}

# round 1 diff daily and flagging
working_dat <- diff_daily(working_dat)  
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff1 <- filter(working_dat, flag_diffsdl == T & flag_diffd1 == T & flag_diffc1 == T)
# run through visual qa
qa_daily_diff1 <- visual_qa(working_dat, check_daily_diff1)
plot_grid(plotlist = qa_daily_diff1) 
# round 1: all look bad excep tmin on 1996-12-19 (looks kind of of like temps ot shifted fwd one day for that week?) 
flag_dailydiff1 <- subset(check_daily_diff1, date!= "1996-12-19")

# function for flagging and removing high values in working dataset
flag_temp <- function(flagdat, error = 1){
  tempdat <- flagdat
  for(row in 1:nrow(tempdat)){
    pos <- with(working_dat, which(met == tempdat$met[row] & logger == tempdat$logger[row] & date == tempdat$date[row]))
    working_dat$qa_flag[pos] <- error
    working_dat$cr_temp[pos] <- NA 
  }
  return(working_dat)
}

# flag and remove values in working dataset
working_dat <- flag_temp(flag_dailydiff1, error = "comparative deviance")

# round 2 daily diff (recalc mean and deviance with bad deviance values from round 1 removed)
working_dat <- diff_daily(working_dat)
# check logger temps that exceed daily deviance allowed on all three chart datasets
check_daily_diff2 <- filter(working_dat, flag_diffsdl == T & flag_diffd1 == T & flag_diffc1 == T)
# run through visual qa
qa_daily_diff2 <- visual_qa(working_dat, check_daily_diff2)
plot_grid(plotlist = qa_daily_diff2) 
# > round 2: 
# > 1990-08-18: flag and remove, other datasets were staying flat or getting warmer
# > leave 1992-07-02 value (low, but entire 20 day period shows logger cooler in tmin than chart sources)
# > 1996-12-19: same from round 1, shift 1996-12-17 to 1996-12-30 back by 1 day so lines up with chart trends

working_dat <- flag_temp(check_daily_diff2[check_daily_diff2$date == "1990-08-18",], "comparative deviance")
# manual adjustment to cr_temp tmin vals in dec 1996
## view both tmin and tmax before adjusting
subset(working_dat, date > "1996-12-13" & date < "1997-01-13") %>%
  dplyr::select(date, met, cr_temp, sdl_temp) %>%
  gather(datsource, temp_c, cr_temp:ncol(.)) %>%
  ggplot(aes(date, temp_c, col = datsource)) +
  geom_line() +
  geom_point(alpha = 0.6) +
  scale_color_viridis_d() +
  facet_wrap(~met)
# > it looks like both tmin and tmax could be shifted back 1 day
# > select 1996-12-17 through 1997-jan-06 since everything in early jan is NA anyway (i.e. doesn't matter if NA is shifted to an NA)  


# -- QA EXTREMES (TMIN/TMAX) -----
## IMPORTANTE!!: 
## logger lifecycles: cr21x = 1980s-2000; cr23x = 2000 - 2012; cr1000 = dec 2012 - ongoing
## panel arrangement: cr21x = left panel, cr23x = middle, cr1000 = right panel

# grand max and min
check_max <- working_dat %>%
  group_by(logger, met) %>%
  filter(cr_temp == max(cr_temp, na.rm = T))
qa_max <- visual_qa(working_dat, check_max)
plot_grid(plotlist = qa_max) 
# >round 1: cr21x and cr23x tmax vals look bad, flag; max tmins look okay
# >round 2: cr21x looks bad, others okay

flag_max1 <- filter(check_max, met == "airtemp_max" & logger != "cr1000")
flag_max2 <- filter(check_max, met == "airtemp_max" & logger == "cr21x")

# function for flagging and removing high values in working dataset
flag_high <- function(flagdat){
  flagdat <- flagdat
  for(row in 1:nrow(flagdat)){
    pos <- with(working_dat, which(met == flagdat$met[row] & logger == flagdat$logger[row] & cr_temp == flagdat$cr_temp[row]))
    working_dat$qa_flag[pos] <- "high value"
    working_dat$cr_temp[pos] <- NA 
  }
}

working_dat <- flag_high(flag_max1)
working_dat <- flag_high(flag_max2)
# re-run max check, if looks okay move on or keep iterating until clean (only did 2 rounds)

check_min <- working_dat %>%
  group_by(logger, met) %>%
  filter(cr_temp == min(cr_temp, na.rm = T))
qa_min <- visual_qa(working_dat, check_min)
plot_grid(plotlist = qa_min)

# monthly max temps of tmin and tmax
check_monthly_max <- crall_long %>%
  group_by(logger, met, mon) %>%
  filter(cr_temp == max(cr_temp, na.rm = T))

qa_mon_max <- visual_qa(crall_long_master, check_monthly_max, sorttime = "mon")

# visualize logger monthly maximums, by logger and metric
## max of airtemp_max
plot_grid(plotlist = qa_mon_max[grep("cr21x.*airtemp_max", qa_mon_max)])
plot_grid(plotlist = qa_mon_max[grep("cr23x.*airtemp_max", qa_mon_max)])
plot_grid(plotlist = qa_mon_max[grep("cr1000.*airtemp_max", qa_mon_max)])
## max of airtemp_min
plot_grid(plotlist = qa_mon_max[grep("cr21x.*airtemp_min", qa_mon_max)])
plot_grid(plotlist = qa_mon_max[grep("cr23x.*airtemp_min", qa_mon_max)])
plot_grid(plotlist = qa_mon_max[grep("cr1000.*airtemp_min", qa_mon_max)])

# monthly min temps of tmin and tmax
check_monthly_min <- crall_long %>%
  group_by(logger, met, mon) %>%
  filter(cr_temp == min(cr_temp, na.rm = T))








# -- CHECK EXTREMES (GRAND TMIN AND TMAX) VALUES -----
# look at tails for any obvious bad values
## cr 21x
sapply(cr21x[grepl("temp", colnames(cr21x))], function(x) tail(sort(x))) #31.45.. Jen Morse said she thinks max T shouldn't exceed 30
sapply(cr21x[grepl("temp", colnames(cr21x))], function(x) tail(sort(x, decreasing = T))) #-75 and -6999 in airtemp_min
## cr 23x, 1000 data loggers
sapply(crlogs[grepl("^airtemp", colnames(crlogs))], function(x) tail(sort(x))) #okay
sapply(crlogs[grepl("^airtemp", colnames(crlogs))], function(x) tail(sort(x, decreasing = T))) # -187 in min temp
## sdl chart
sapply(sdl[grepl("^airtemp", colnames(sdl))], function(x) tail(sort(x))) #okay, high max value agrees with cr logger high value
sapply(sdl[grepl("^airtemp", colnames(sdl))], function(x) tail(sort(x, decreasing = T))) #-38 is kind of a jump from other tmin
## d1 chart
sapply(d1[grepl("^airtemp", colnames(d1))], function(x) tail(sort(x))) #+25C at d1.. seems kinda high for d1?
sapply(d1[grepl("^airtemp", colnames(d1))], function(x) tail(sort(x, decreasing = T))) #okay
## c1 chart
sapply(c1[grepl("^airtemp", colnames(c1))], function(x) tail(sort(x))) #seems alright.. would be warmer at c1 compared to sdl/d1, so 32C possible
sapply(c1[grepl("^airtemp", colnames(c1))], function(x) tail(sort(x, decreasing = T))) #okay


# were mean values affected by bad values in logger dataset?
na.omit(crlogs[crlogs$airtemp_min == -187, grepl("^airtemp", colnames(crlogs))]) #doesn't look like it
#       airtemp_max airtemp_min airtemp_avg
# 6087       0.812        -187    -3.32507

# change bad values to NAs and move on
cr21x$`minimum temperature`[cr21x$`minimum temperature` < -70 & !is.na(cr21x$`minimum temperature`)] <- NA
crlogs$airtemp_min[crlogs$airtemp_min == -187 & !is.na(crlogs$airtemp_min)] <- NA

summary(cr21x[grepl("temp", colnames(cr21x))])
lapply(split(crlogs[grepl("^airtemp", colnames(crlogs))], crlogs$logger), summary) # maxT for cr1000 is 22, 25.4 for cr23x; minT for cr1000 is -29, -36 for cr23x.. 
summary(sdl[grepl("^airtemp", colnames(sdl))]) # max T is 25, minT -38

# look at maxT in cr21x further..
cr21x[cr21x$`maximum temperature` == 31.45 & !is.na(cr21x$`maximum temperature`),] # time of maxT seems reasonable (11:47)
# 10days before and after 31.45
ggplot(data = cr21x[(which(cr21x$`maximum temperature` == 31.45)-10):(which(cr21x$`maximum temperature` == 31.45)+10),],
       aes(date, `maximum temperature`)) +
  geom_line() +
  geom_point() +
  # add sdl chart temp for comparison (purple dots)
  geom_point(data = sdl[(which(sdl$date == "1990-07-09")-10):(which(sdl$date == "1990-07-09")+10),], aes(date, airtemp_max), col = "purple", pch = 1)
# conclusion: 31.45 looks like a bad value

cr21x[cr21x$`maximum temperature` == 28.41 & !is.na(cr21x$`maximum temperature`),] # time of maxT seems reasonable (11:26), minT missing so suspect
# 10days before and after 28.41
ggplot(data = cr21x[(which(cr21x$`maximum temperature` == 28.41)-10):(which(cr21x$`maximum temperature` == 28.41)+10),],
       aes(date, `maximum temperature`)) +
  geom_line() +
  geom_point() +
  # add sdl chart temp for comparison (purple dots)
  geom_point(data = sdl[(which(sdl$date == "1997-07-30")-10):(which(sdl$date == "1997-07-30")+10),], aes(date, airtemp_max), col = "purple", pch = 1)
# conclusion: 28.41 also looks like a bad value

#look at maxT by time of day (i.e. is it ever reasonable maxT occurred at 2359?)
ggplot(subset(cr21x, year(date)>1991), aes(`time of maximum temperature`, `maximum temperature`, col = year(date))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~month(date))
#minT for comparison
ggplot(subset(cr21x, year(date)>1991), aes(`time of minimum temperature`, `minimum temperature`, col = year(date))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~month(date))
# thoughts: it could be that the clock failed in the winter months, but tmax and tmin are still good. maxt values and times around midnight in the summer months look bad tho
ggplot(cr21x, aes(`Julian day`, `maximum temperature`)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~year(date))

# convert high cr21x temps that don't track sdl chart to NAs and move on to automatic flagging and data prep
# 31.45
cr21x$`maximum temperature`[cr21x$`maximum temperature` == 31.45 & !is.na(cr21x$`maximum temperature`)] <- NA
cr21x$`maximum temperature`[cr21x$`maximum temperature` == 28.41 & !is.na(cr21x$`maximum temperature`)] <- NA

# spot check hi and low values in cr23x against sdl chart
# cr23x has warmest and coldest temp so no need to subset logger == cr23x
crlogs[crlogs$airtemp_max == max(crlogs$airtemp_max, na.rm =T) & !is.na(crlogs$airtemp_max),] # time of maxT seems reasonable (11:47)
# 10days before and after 31.45
ggplot(data = crlogs[(which(crlogs$airtemp_max == max(crlogs$airtemp_max, na.rm =T))-10):(which(crlogs$airtemp_max == max(crlogs$airtemp_max, na.rm =T))+10),],
       aes(date, airtemp_max)) +
  geom_line() +
  geom_point() +
  # add sdl chart temp for comparison (purple dots)
  geom_line(data = sdl[(which(sdl$date == "2000-09-06")-10):(which(sdl$date == "2000-09-06")+10),], aes(date, airtemp_max), col = "purple") +
  geom_point(data = sdl[(which(sdl$date == "2000-09-06")-10):(which(sdl$date == "2000-09-06")+10),], aes(date, airtemp_max), col = "purple", pch = 1) +
  geom_line(data = d1[(which(d1$date == "2000-09-06")-10):(which(d1$date == "2000-09-06")+10),], aes(date, airtemp_max), col = "steelblue2") +
  geom_point(data = d1[(which(d1$date == "2000-09-06")-10):(which(d1$date == "2000-09-06")+10),], aes(date, airtemp_max), col = "steelblue4", pch = 1)
# conclusion: +25C looks like a bad value in the CR23x logger, all other data around then fine

crlogs[crlogs$airtemp_min == min(crlogs$airtemp_min, na.rm =T) & !is.na(crlogs$airtemp_min),]
# 10days before and after 31.45
ggplot(data = crlogs[(which(crlogs$airtemp_min == min(crlogs$airtemp_min, na.rm =T))-10):(which(crlogs$airtemp_min == min(crlogs$airtemp_min, na.rm =T))+10),],
       aes(date, airtemp_min)) +
  geom_line() +
  geom_point() +
  # add sdl chart temp for comparison (purple dots)
  geom_line(data = sdl[(which(sdl$date == "2011-02-01")-10):(which(sdl$date == "2011-02-01")+10),], aes(date, airtemp_min), col = "purple") +
  geom_point(data = sdl[(which(sdl$date == "2011-02-01")-10):(which(sdl$date == "2011-02-01")+10),], aes(date, airtemp_min), col = "purple", pch = 1) +
  geom_line(data = d1[(which(d1$date == "2011-02-01")-10):(which(d1$date == "2011-02-01")+10),], aes(date, airtemp_min), col = "steelblue2") +
  geom_point(data = d1[(which(d1$date == "2011-02-01")-10):(which(d1$date == "2011-02-01")+10),], aes(date, airtemp_min), col = "steelblue4", pch = 1)
# conclusion: real value. twas cold that day.

# change high value in cr23x to NA
# 25.46
crlogs$airtemp_max[crlogs$airtemp_max == max(crlogs$airtemp_max, na.rm =T) & !is.na(crlogs$airtemp_max)] <- NA

# look at cr21x with other loggers one last time:
summary(cr21x[grepl("temp", colnames(cr21x))])
lapply(split(crlogs[grepl("^airtemp", colnames(crlogs))], crlogs$logger), summary) # max T is 25
summary(sdl[grepl("^airtemp", colnames(sdl))]) # max T is 25

# look at d1 high temp val
d1[d1$airtemp_max == max(d1$airtemp_max, na.rm =T) & !is.na(d1$airtemp_max),] # a 1956 data point.. idk?
# 10days before and after 31.45
ggplot(data = d1[(which(d1$airtemp_max == max(d1$airtemp_max, na.rm =T))-10):(which(d1$airtemp_max == max(d1$airtemp_max, na.rm =T))+10),],
       aes(date, airtemp_max)) +
  geom_line(color = "steelblue2") +
  geom_point(color = "steelblue4") +
  # add sdl chart temp for comparison (purple dots)
  geom_line(data = c1[(which(c1$date == "1956-06-13")-10):(which(c1$date == "1956-06-13")+10),], aes(date, airtemp_max), col = "forestgreen") +
  geom_point(data = c1[(which(c1$date == "1956-06-13")-10):(which(c1$date == "1956-06-13")+10),], aes(date, airtemp_max), col = "darkgreen", pch = 1)
# conclusion: leave d1 tmax be. similar trend in c1.. doesn't seem likely d1 was that much warmer than c1 on that day, but who knows 

# look at sdl chart tmin value
sdl[sdl$airtemp_min == min(sdl$airtemp_min, na.rm =T) & !is.na(sdl$airtemp_min),]
# 10days before and after 31.45
ggplot(data = sdl[(which(sdl$airtemp_min == min(sdl$airtemp_min, na.rm =T))-10):(which(sdl$airtemp_min == min(sdl$airtemp_min, na.rm =T))+10),],
       aes(date, airtemp_min)) +
  geom_line(col = "orchid") +
  geom_point(col = "purple") +
  # add logger dat for comparison (black dots)
  geom_line(data = crlogs[(which(crlogs$date == "2011-02-01")-10):(which(crlogs$date == "2011-02-01")+10),], aes(date, airtemp_max), col = "black") +
  geom_point(data = crlogs[(which(crlogs$date == "2011-02-01")-10):(which(crlogs$date == "2011-02-01")+10),], aes(date, airtemp_max), col = "black", pch = 1) +
  # add d1 chart temp for comparison (blue dots)
  geom_line(data = d1[(which(d1$date == "2011-02-01")-10):(which(d1$date == "2011-02-01")+10),], aes(date, airtemp_max), col = "steelblue2") +
  geom_point(data = d1[(which(d1$date == "2011-02-01")-10):(which(d1$date == "2011-02-01")+10),], aes(date, airtemp_max), col = "steelblue4", pch = 1) +
  # add c1 chart temp for comparison (green dots)
  geom_line(data = c1[(which(c1$date == "2011-02-01")-10):(which(c1$date == "2011-02-01")+10),], aes(date, airtemp_max), col = "forestgreen") +
  geom_point(data = c1[(which(c1$date == "2011-02-01")-10):(which(c1$date == "2011-02-01")+10),], aes(date, airtemp_max), col = "darkgreen", pch = 1)
# conclusion: leave be. saddle chart seems consistently off (wouldn't except sdl that much colder than d1, altho could be? but follows trend)









# -- CHECK MONTHLY EXTREMES -----
with(subset(d1_long, met == "airtemp_min"), lapply(split(d1_temp, mon), function(x) summary(x)))
with(subset(d1_long, met == "airtemp_max"), lapply(split(d1_temp, mon), function(x) summary(x))) # highest tmax occurs in june.. seems unlikely?
with(subset(d1_long, met == "airtemp_min"), lapply(split(d1_temp, mon), function(x) head(sort(x)))) # -11 tmin in august? plausible?
with(subset(d1_long, met == "airtemp_max"), lapply(split(d1_temp, mon), function(x) tail(sort(x)))) # still seems high.. could flag to leave as is but not use in a regression

with(subset(c1_long, met == "airtemp_min"), lapply(split(c1_temp, mon), function(x) summary(x)))
with(subset(c1_long, met == "airtemp_max"), lapply(split(c1_temp, mon), function(x) summary(x))) # highest tmax occurs in june.. seems unlikely?
with(subset(c1_long, met == "airtemp_min"), lapply(split(c1_temp, mon), function(x) head(sort(x))))
with(subset(c1_long, met == "airtemp_max"), lapply(split(c1_temp, mon), function(x) tail(sort(x)))) # check 20 jan, 29 in march is weird, 18tmax in december looks weird.. next highest is 13

with(subset(sdl_long, met == "airtemp_min"), lapply(split(sdl_temp, mon), function(x) summary(x)))
with(subset(sdl_long, met == "airtemp_max"), lapply(split(sdl_temp, mon), function(x) summary(x))) # highest tmax occurs in june.. seems unlikely?
with(subset(sdl_long, met == "airtemp_min"), lapply(split(sdl_temp, mon), function(x) head(sort(x))))
with(subset(sdl_long, met == "airtemp_max"), lapply(split(sdl_temp, mon), function(x) tail(sort(x)))) # check 20 jan, 29 in march is weird, 18tmax in december looks weird.. next highest is 13

with(subset(cr21x_long, met == "airtemp_min"), lapply(split(temp, mon), function(x) summary(x))) # check -36 in aug
with(subset(cr21x_long, met == "airtemp_max"), lapply(split(temp, mon), function(x) summary(x))) # highest tmax occurs in june.. seems unlikely?
with(subset(cr21x_long, met == "airtemp_min"), lapply(split(temp, mon), function(x) head(sort(x))))
with(subset(cr21x_long, met == "airtemp_max"), lapply(split(temp, mon), function(x) tail(sort(x)))) # check 20 jan, 29 in march is weird, 18tmax in december looks weird.. next highest is 13


# -- AUTOMATE QA LOGGER TEMP DATA -----
# flags:
# max temp !< min temp
# min temp !> max temp
# jen morse said at sdl, tmax shouldn't exceed 25C and tmin shouldn't fall below -40C
# diurnal temp !> defined allowance (shouldn't swing more than 35C in a day?)
# jen liked the flag for temp value more than 3sd outside monthly mean, and day-to-day flux more than 3sd outside monthly day-to-day flux
# need to screen out obvious/unreasonable/improbable values (e.g. -187C) before calculating means and sds (so outliers don't influence those values)
# perhaps compare delta of logger with sdl chart and d1 chart.. if both differ by more than usual (e.g. +3sd, review data point/perhaps change to NA)


# diff logger temp from d1 and sdl chart, if timestamp in unexpected window (e.g. around midnight for tmax, flag it)
cr21x_long2 <- cr21x_long %>%
  arrange(met, date) %>% #to be sure sorted by date..
  # # lag and lead temp vals by 1 day
  # group_by(met) %>%
  # mutate(lag1_temp = lag(temp),
  #        lead1_temp = lead(temp)) %>%
  # ungroup() %>%
  # # diff lag and lead
  # mutate(delta_lag = temp-lag1_temp,
  #        delta_lead = temp-lead1_temp) %>%
  # # flag anomoly as +4sd departure from lag and lead temps
  # group_by(met, mon) %>%
  # mutate(delta_thresh_lag = (mean(abs(delta_lag), na.rm = T)) + 4*(sd(abs(delta_lag), na.rm = T)),
#        delta_thresh_lead = mean(abs(delta_lead), na.rm = T) + 4*(sd(abs(delta_lag), na.rm = T))) %>%
# ungroup() %>%
# # flag departures
# mutate(flag_deltalag = ifelse(abs(delta_lag)> delta_thresh_lag, 1, 0),
#        flag_deltalead = ifelse(abs(delta_lead)> delta_thresh_lead, 1, 0)) %>%
left_join(dplyr::select(d1_long, date:delta_d1lag)) %>%
  left_join(dplyr::select(sdl_long, date:delta_sdllag)) %>%
  left_join(dplyr::select(c1_long, date:delta_c1lag)) %>%
  mutate(d1_delta = temp-d1_temp,
         #diff_d1_deltalag = delta_lag - delta_d1lag,
         sdl_delta = temp-sdl_temp,
         #diff_sdl_deltalag = delta_lag - delta_sdllag,
         c1_delta = temp-c1_temp ) %>%
  #diff_c1_deltalag = delta_lag - delta_c1lag) %>%
  # group by monthly trends for flags
  group_by(met, mon) %>%
  mutate(mean_temp = mean(temp, na.rm = T),
         sd_temp = sd(temp, na.rm = T),
         mean_d1delta = mean(d1_delta, na.rm = T),
         sd_d1delta = sd(d1_delta, na.rm = T),
         mean_sdldelta = mean(sdl_delta, na.rm = T),
         sd_sdldelta = sd(sdl_delta, na.rm =T),
         mean_c1delta = mean(c1_temp, na.rm = T),
         sd_c1delta = sd(c1_delta, na.rm = T)) %>%
  # mean_d1deltalag = mean(diff_d1_deltalag, na.rm = T),
  # sd_d1deltalag = sd(diff_d1_deltalag, na.rm = T),
  # mean_sdldeltalag = mean(diff_sdl_deltalag, na.rm = T),
  # sd_sdldeltalag = sd(diff_sdl_deltalag, na.rm = T),
  # mean_c1deltalag = mean(diff_c1_deltalag, na.rm = T),
  # sd_c1deltalag = sd(diff_c1_deltalag, na.rm = T)) %>%
  ungroup() %>%
  # flags vals 3sd outside normal range; flag time if tmax around midnight or tmin in btwn 10am and 2pm (both unlikely)
  mutate(flag_temp = ifelse((mean_temp - temp) > (mean_temp + 4*sd_temp) | (mean_temp - temp) < (mean_temp - 4*sd_temp), 1, 0),
         # make temporary unflagged time flag col
         flag_time = 0,
         flag_d1_delta = ifelse((mean_d1delta - d1_delta) > (mean_d1delta + 3*sd_d1delta) | (mean_d1delta - d1_delta) < (mean_d1delta - 3*sd_d1delta), 1, 0),
         flag_sdl_delta = ifelse((mean_sdldelta - sdl_delta) > (mean_sdldelta + 3*sd_sdldelta) | (mean_sdldelta - sdl_delta) < (mean_sdldelta - 3*sd_sdldelta), 1, 0),
         flag_c1_delta = ifelse((mean_c1delta - c1_delta) > (mean_c1delta + 3*sd_c1delta) | (mean_c1delta - c1_delta) < (mean_c1delta - 3*sd_c1delta), 1, 0))
# flag_d1_deltalag = ifelse((mean_d1deltalag - diff_d1_deltalag) > (mean_d1deltalag + 3*sd_d1deltalag) | (mean_d1deltalag - diff_d1_deltalag) < (mean_d1deltalag - 3*sd_d1deltalag), 1, 0),
# flag_sdl_deltalag = ifelse((mean_sdldeltalag - diff_sdl_deltalag) > (mean_sdldeltalag + 3*sd_sdldeltalag) | (mean_sdldeltalag - diff_sdl_deltalag) < (mean_sdldeltalag - 3*sd_sdldeltalag), 1, 0),
# flag_c1_deltalag = ifelse((mean_c1deltalag - diff_c1_deltalag) > (mean_c1deltalag + 3*sd_c1deltalag) | (mean_c1deltalag - diff_c1_deltalag) < (mean_c1deltalag - 3*sd_c1deltalag), 1, 0))

# flag time val by temp measurement
## flag tmax if timestamp at night (after 7pm to before 6am)
cr21x_long2$flag_time[cr21x_long2$met == "airtemp_max"] <- with(cr21x_long2[cr21x_long2$met == "airtemp_max",],
                                                                ifelse(time > 1900 | time < 600, 1, 0))
## flag tmin if timestamp between 10am and 2pm
cr21x_long2$flag_time[cr21x_long2$met == "airtemp_min"] <- with(cr21x_long2[cr21x_long2$met == "airtemp_min",],
                                                                ifelse(time > 1000 & time < 1400, 1, 0))  


# plot out flagged values
ggplot(subset(cr21x_long2, flag_d1_delta == 1 & flag_sdl_delta == 1 & flag_c1_delta == 1)) +
  geom_point(aes(mon, temp)) +
  geom_point(aes(mon, sdl_temp), col = "purple", alpha = 0.5) +
  geom_point(aes(mon, d1_temp), col = "steelblue4", alpha = 0.5 ) +
  geom_point(aes(mon, c1_temp), col = "forestgreen", alpha = 0.5 ) +
  facet_wrap(~met, scales = "free_y")

test <- subset(cr21x_long2, flag_sdl_delta == 1 & flag_d1_delta == 1 & flag_c1_delta == 1)

# function to panel plot flagged data
qa_temps <- function(dat){
  plot_list <- list()
  for(m in c("airtemp_max", "airtemp_min")){
    tempdf <- subset(dat, met == m) %>% as.data.frame()
    
    for(d in as.character(tempdf$date)){
      d <- as.Date(d, format = "%Y-%m-%d")
      tempplot <- ggplot(data = subset(cr21x_long3, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)),
                         aes(date, temp)) +
        geom_line() +
        geom_point() +
        # circle the flagged value in red
        geom_point(data = subset(cr21x_long, met == m & date == as.Date(d)),
                   aes(date, temp), col = "red", pch  = 1, size = 3) +
        labs(y = gsub("airtemp_", "T", m),
             x = year(d)) +
        # add sdl chart temp for comparison (purple dots)
        geom_line(data = subset(sdl_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), aes(date, sdl_temp), col = "purple") +
        geom_point(data = subset(sdl_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), aes(date, sdl_temp), col = "purple", pch = 1) +
        geom_line(data = subset(d1_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), aes(date, d1_temp), col = "steelblue2") +
        geom_point(data = subset(d1_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), aes(date, d1_temp), col = "steelblue4", pch = 1) +
        geom_line(data = subset(c1_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), aes(date, c1_temp), col = "forestgreen") +
        geom_point(data = subset(c1_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), aes(date, c1_temp), col = "darkgreen", pch = 1) +
        theme_bw()
      
      # store plot in a list
      plot_list[length(plot_list)+1] <- list(tempplot)
    }
  }
  return(plot_list)
}

cr21x_qa <- qa_temps(test)
plot_grid(plotlist = cr21x_qa) #all points look bad

# start correcting.. bad values to NA
for(i in 1:nrow(test)){
  temp <- test[i,]
  cr21x_long2$temp[cr21x_long2$date == temp$date & cr21x_long2$met == temp$met] <- NA
}

# now diff deltas with anomolies removed..
cr21x_long3 <- cr21x_long2 %>%
  arrange(met, date) %>% #to be sure sorted by date..
  # lag and lead temp vals by 1 day
  group_by(met) %>%
  mutate(lag1_temp = lag(temp),
         lead1_temp = lead(temp)) %>%
  ungroup() %>%
  # diff lag and lead
  mutate(delta_lag = temp-lag1_temp,
         delta_lead = temp-lead1_temp) %>%
  # flag anomoly as +4sd departure from lag and lead temps
  group_by(met, mon) %>%
  mutate(delta_thresh_lag = (mean(abs(delta_lag), na.rm = T)) + 4*(sd(abs(delta_lag), na.rm = T)),
         delta_thresh_lead = mean(abs(delta_lead), na.rm = T) + 4*(sd(abs(delta_lag), na.rm = T))) %>%
  ungroup() %>%
  # flag departures
  mutate(flag_deltalag = ifelse(abs(delta_lag)> delta_thresh_lag, 1, 0),
         flag_deltalead = ifelse(abs(delta_lead)> delta_thresh_lead, 1, 0)) %>%
  mutate(d1_delta = temp-d1_temp,
         sdl_delta = temp-sdl_temp,
         c1_delta = temp-c1_temp, 
         diff_d1_deltalag = delta_lag - delta_d1lag,
         diff_sdl_deltalag = delta_lag - delta_sdllag,
         diff_c1_deltalag = delta_lag - delta_c1lag) %>%
  # group by monthly trends for flags
  group_by(met, mon) %>%
  mutate(mean_temp = mean(temp, na.rm = T),
         sd_temp = sd(temp, na.rm = T),
         mean_d1delta = mean(d1_delta, na.rm = T),
         sd_d1delta = sd(d1_delta, na.rm = T),
         mean_sdldelta = mean(sdl_delta, na.rm = T),
         sd_sdldelta = sd(sdl_delta, na.rm =T),
         mean_c1delta = mean(c1_temp, na.rm = T),
         sd_c1delta = sd(c1_delta, na.rm = T),
         mean_d1deltalag = mean(diff_d1_deltalag, na.rm = T),
         sd_d1deltalag = sd(diff_d1_deltalag, na.rm = T),
         mean_sdldeltalag = mean(diff_sdl_deltalag, na.rm = T),
         sd_sdldeltalag = sd(diff_sdl_deltalag, na.rm = T),
         mean_c1deltalag = mean(diff_c1_deltalag, na.rm = T),
         sd_c1deltalag = sd(diff_c1_deltalag, na.rm = T)) %>%
  ungroup() %>%
  # flags vals 3sd outside normal range; flag time if tmax around midnight or tmin in btwn 10am and 2pm (both unlikely)
  mutate(flag_temp = ifelse((mean_temp - temp) > (mean_temp + 4*sd_temp) | (mean_temp - temp) < (mean_temp - 4*sd_temp), 1, 0),
         flag_d1_delta = ifelse((mean_d1delta - d1_delta) > (mean_d1delta + 3*sd_d1delta) | (mean_d1delta - d1_delta) < (mean_d1delta - 3*sd_d1delta), 1, 0),
         flag_sdl_delta = ifelse((mean_sdldelta - sdl_delta) > (mean_sdldelta + 3*sd_sdldelta) | (mean_sdldelta - sdl_delta) < (mean_sdldelta - 3*sd_sdldelta), 1, 0),
         flag_c1_delta = ifelse((mean_c1delta - c1_delta) > (mean_c1delta + 3*sd_c1delta) | (mean_c1delta - c1_delta) < (mean_c1delta - 3*sd_c1delta), 1, 0),
         flag_d1_deltalag = ifelse((mean_d1deltalag - diff_d1_deltalag) > (mean_d1deltalag + 3*sd_d1deltalag) | (mean_d1deltalag - diff_d1_deltalag) < (mean_d1deltalag - 3*sd_d1deltalag), 1, 0),
         flag_sdl_deltalag = ifelse((mean_sdldeltalag - diff_sdl_deltalag) > (mean_sdldeltalag + 3*sd_sdldeltalag) | (mean_sdldeltalag - diff_sdl_deltalag) < (mean_sdldeltalag - 3*sd_sdldeltalag), 1, 0),
         flag_c1_deltalag = ifelse((mean_c1deltalag - diff_c1_deltalag) > (mean_c1deltalag + 3*sd_c1deltalag) | (mean_c1deltalag - diff_c1_deltalag) < (mean_c1deltalag - 3*sd_c1deltalag), 1, 0),
         flag_warm = temp > c1_temp & temp > sdl_temp & temp > d1_temp)

# screen same test as above
test <- subset(cr21x_long3, flag_sdl_delta == 1 & flag_d1_delta == 1 & flag_c1_delta == 1)
newqa <- qa_temps(test)
plot_grid(plotlist = newqa) #it's fine
test <- subset(cr21x_long3, flag_sdl_deltalag == 1 & flag_d1_deltalag == 1 & flag_c1_deltalag == 1)
plot_grid(plotlist = qa_temps(test))
test <- subset(cr21x_long3, flag_warm == TRUE)
test <- test[1:40,]
warmqa <- qa_temps(test)
plot_grid(plotlist = warmqa[21:40])
# test$date[test$met == "airtemp_max"][2]
# plot second example
ggplot(data = subset(cr21x, date %in% seq(as.Date(test$date[test$met == "airtemp_max"][2])-10, as.Date(test$date[test$met == "airtemp_max"][2])+10, 1)),
       aes(date, `maximum temperature`)) +
  geom_line() +
  geom_point() +
  # add sdl chart temp for comparison (purple dots)
  geom_line(data = sdl[(which(sdl$date == test$date[test$met == "airtemp_max"][2])-10):(which(sdl$date == test$date[test$met == "airtemp_max"][2])+10),], aes(date, airtemp_max), col = "purple") +
  geom_point(data = sdl[(which(sdl$date == test$date[test$met == "airtemp_max"][2])-10):(which(sdl$date == test$date[test$met == "airtemp_max"][2])+10),], aes(date, airtemp_max), col = "purple", pch = 1) +
  geom_line(data = d1[(which(d1$date == test$date[test$met == "airtemp_max"][2])-10):(which(d1$date == test$date[test$met == "airtemp_max"][2])+10),], aes(date, airtemp_max), col = "steelblue2") +
  geom_point(data = d1[(which(d1$date == test$date[test$met == "airtemp_max"][2])-10):(which(d1$date == test$date[test$met == "airtemp_max"][2])+10),], aes(date, airtemp_max), col = "steelblue4", pch = 1) +
  geom_line(data = c1[(which(c1$date == test$date[test$met == "airtemp_max"][2])-10):(which(c1$date == test$date[test$met == "airtemp_max"][2])+10),], aes(date, airtemp_max), col = "forestgreen") +
  geom_point(data = c1[(which(c1$date == test$date[test$met == "airtemp_max"][2])-10):(which(c1$date == test$date[test$met == "airtemp_max"][2])+10),], aes(date, airtemp_max), col = "darkgreen", pch = 1)

