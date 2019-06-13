# compile and qa campbell scientific saddle logger temp, all years
# (streamlined version after comparing all saddle temperature datasets)


# script purpose:
# read in all campbell scientific logger datasets, and sdl, d1, and c1 chart temp datasets for comparison
# tidy datasets
# determine routine checks to flag suspect values relative to nearby temp trends (i.e. comparative datasets)
# flag values
# write out QA dataset with flags for next step (infilling)


# notes:
# color scheme in panel figures:
# blue = d1 chart, purple = sdl chart, green = c1 chart, black = sdl cr loggers



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


# quick visual of all logger data as they are from EDI/SCE
ggplot(crall_long, aes(date, cr_temp, col = logger)) +
  geom_point(alpha =0.5) +
  scale_color_viridis_d() +
  facet_wrap(~met) # serious tmin outliers present..

# plot without outliers
# quick visual of all logger data as they are from EDI/SCE - no outliers
crallfig <- ggplot(subset(crall_long, cr_temp > -50 & date < max(sdl_long$date)), aes(date, cr_temp, col = logger)) +
  geom_point(alpha =0.5) +
  scale_color_viridis_d() +
  scale_x_date(breaks = "5 years", date_labels = "%Y") +
  theme(legend.position = "top",
        axis.title.x = element_blank()) + 
  facet_wrap(~met) # serious tmin outliers present..
# raw sdl chart
sdlfig <- ggplot(subset(sdl_long, date > min(crall_long$date)), aes(date, sdl_temp)) +
  geom_point(alpha =0.5) +
  scale_x_date(breaks = "5 years", date_labels = "%Y") +
  facet_wrap(~met)
# plot together for comparison
plot_grid(crallfig, sdlfig, nrow = 2,
          rel_heights = c(1.1,1))



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
         thresh_diff_c1 = mean(cr_diff_c1, na.rm = T) + (3*sd(cr_diff_c1, na.rm = T)),
         sd_diff_sdl = sd(cr_diff_sdl, na.rm = T),
         sd_diff_d1 = sd(cr_diff_d1, na.rm = T),
         sd_diff_c1 = sd(cr_diff_c1, na.rm = T)) %>%
  ungroup() %>%
  # flag logger value if exceeds daily diff threshold for chart comparative datasets
  mutate(flag_diffsdl = cr_diff_sdl > thresh_diff_sdl,
         deviance_sdl = cr_diff_sdl/sd_diff_sdl,
         flag_diffd1 = cr_diff_d1 > thresh_diff_d1,
         deviance_d1 = cr_diff_d1/sd_diff_d1,
         flag_diffc1 = cr_diff_c1 > thresh_diff_c1,
         deviance_c1 = cr_diff_c1/sd_diff_c1)
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
flag_temp <- function(flagdat, error = "comparative deviance"){
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
View(subset(working_dat, date %in% seq(as.Date("1996-12-15"), as.Date("1997-01-06"), 1)))
# looks like start 12/17 to 1/5, shift back 1 day
shift_dates <- seq(as.Date("1996-12-17"), as.Date("1997-01-05"), 1)
working_dat$qa_flag[working_dat$date %in% (shift_dates-1)] <- "shifted temp -1 day"
working_dat$cr_temp[working_dat$met == "airtemp_max" & working_dat$date %in% (shift_dates-1)] <- working_dat$cr_temp[working_dat$met == "airtemp_max" & working_dat$date %in% (shift_dates)]
working_dat$cr_temp[working_dat$met == "airtemp_min" & working_dat$date %in% (shift_dates-1)] <- working_dat$cr_temp[working_dat$met == "airtemp_min" & working_dat$date %in% (shift_dates)]

# plot again to ensure check temps adjusted as expected
subset(working_dat, date > "1996-12-13" & date < "1997-01-13") %>%
  dplyr::select(date, met, cr_temp, sdl_temp) %>%
  gather(datsource, temp_c, cr_temp:ncol(.)) %>%
  ggplot(aes(date, temp_c, col = datsource)) +
  geom_line() +
  geom_point(alpha = 0.6) +
  scale_color_viridis_d() +
  facet_wrap(~met) #yes


# round 3: check extreme sdl deviance values
working_dat <- diff_daily(working_dat)
# check logger temps that exceed daily deviance more than 5sds for sdl_chart and d1_chart
check_daily_diff3 <- filter(working_dat, flag_diffsdl == TRUE & flag_diffd1 == T & round(deviance_c1) >= 4)
# run through visual qa
qa_daily_diff3 <- visual_qa(working_dat, check_daily_diff3)
plot_grid(plotlist = qa_daily_diff3)
# > sdl chart flatlines in tmin so ignore Sep 1990 tmin, leave 1992 tmin be
# > all tmax vals can be flagged and removed -- are clear spikes or otherwise off
working_dat <- flag_temp(check_daily_diff3[check_daily_diff3$met == "airtemp_max",], "comparative deviance")

# move on to checking grand and monthly tmax and tmin vals..
#clean up environment
rm(check_daily_diff1, flag_dailydiff1, check_daily_diff2, check_daily_diff3,
   qa_daily_diff1, qa_daily_diff2, qa_daily_diff3)



# -- QA EXTREMES (TMIN/TMAX) -----
## IMPORTANTE!!: 
## logger lifecycles: cr21x = 1980s-2000; cr23x = 2000 - 2012; cr1000 = dec 2012 - ongoing
## panel arrangement: cr21x = left panel, cr23x = middle, cr1000 = right panel

# visual qa grand max and min
# max temps by logger
check_max1 <- working_dat %>%
  group_by(logger, met) %>%
  filter(cr_temp == max(cr_temp, na.rm = T))
qa_max1 <- visual_qa(working_dat, check_max1)
plot_grid(plotlist = qa_max1) 
# >round 1: cr23x tmax 2000-09-06 seems unlikely givven trends in c1, sdl and d1, all else fine 
flag_max1 <- filter(check_max, met == "airtemp_max" & logger == "cr23x" & date == "2000-09-06")
working_dat <- flag_temp(flag_max1, error = "high value")
# run through grand max once more
check_max2 <- working_dat %>%
  group_by(logger, met) %>%
  filter(cr_temp == max(cr_temp, na.rm = T))
qa_max2 <- visual_qa(working_dat, check_max2)
plot_grid(plotlist = qa_max2) #looks okay

# min temps by logger
check_min1 <- working_dat %>%
  group_by(logger, met) %>%
  filter(cr_temp == min(cr_temp, na.rm = T))
qa_min1 <- visual_qa(working_dat, check_min1)
plot_grid(plotlist = qa_min1) #temperature minimums look good

# move on to monthlies...
#clean up env
rm(check_max1, check_max2, qa_max1, qa_max2,
   check_min1, qa_min1)



# -- QA MONTHLY MIN/MAX -----
# monthly max temps of tmin and tmax
check_monthly_max1 <- working_dat %>%
  group_by(logger, met, mon) %>%
  filter(cr_temp == max(cr_temp, na.rm = T))
# run through visual qa function
qa_mon_max1 <- visual_qa(working_dat, check_monthly_max1, sorttime = "mon")

# visualize logger monthly maximums, by logger and metric
## max of airtemp_max
plot_grid(plotlist = qa_mon_max1[grep("cr21x.*airtemp_max", qa_mon_max1)]) #flag sep (1987-09-01) and nov (11-29-1999) 
plot_grid(plotlist = qa_mon_max1[grep("cr23x.*airtemp_max", qa_mon_max1)]) #flag dec (2001-12-26)
plot_grid(plotlist = qa_mon_max1[grep("cr1000.*airtemp_max", qa_mon_max1)]) #okay
## max of airtemp_min
plot_grid(plotlist = qa_mon_max1[grep("cr21x.*airtemp_min", qa_mon_max1)]) #okay
plot_grid(plotlist = qa_mon_max1[grep("cr23x.*airtemp_min", qa_mon_max1)]) #okay
plot_grid(plotlist = qa_mon_max1[grep("cr1000.*airtemp_min", qa_mon_max1)]) #okay.. dec looks a little different, but since sdl chart missing not taking action

# monthly min temps of tmin and tmax
check_monthly_min <- working_dat %>%
  group_by(logger, met, mon) %>%
  filter(cr_temp == min(cr_temp, na.rm = T))
# run through visual qa function
qa_mon_min1 <- visual_qa(working_dat, check_monthly_min, sorttime = "mon")

# visualize logger monthly minimums, by logger and metric
## min of airtemp_max
plot_grid(plotlist = qa_mon_min1[grep("cr21x.*airtemp_max", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr23x.*airtemp_max", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr1000.*airtemp_max", qa_mon_min1)]) #okay
## min of airtemp_min
plot_grid(plotlist = qa_mon_min1[grep("cr21x.*airtemp_min", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr23x.*airtemp_min", qa_mon_min1)]) #okay
plot_grid(plotlist = qa_mon_min1[grep("cr1000.*airtemp_min", qa_mon_min1)]) #okay


# > general observation: seems like deviances from chart data occurs more in tmax vals than tmin vals (even max of tmin not so bad, but lots of problems in max of tmax)
# flag and remove noted monthly max values from cr21x (sep and nov) and cr23x (dec)
flag_mon_max1 <- subset(check_monthly_max, met == "airtemp_max" & (logger == "cr21x" & mon %in% c(9,11) |
                          logger == "cr23x" & mon == 12))
# note the diffs are flagged for sdl and d1 in these observations:
dplyr::select(flag_mon_max1, date, logger, met, flag_diffsdl:ncol(flag_mon_max1))
# flag and remove from working copy
working_dat <- flag_temp(flag_mon_max1, error = "high value")

# check monthly max again to be sure
check_monthly_max2 <- working_dat %>%
  group_by(logger, met, mon) %>%
  filter(cr_temp == max(cr_temp, na.rm = T))
# run through visual qa function
qa_mon_max2 <- visual_qa(working_dat, check_monthly_max2, sorttime = "mon")
# visualize cr21x and cr23x only (what was adjusted)
plot_grid(plotlist = qa_mon_max2[grep("cr21x.*airtemp_max", qa_mon_max2)]) #okay
plot_grid(plotlist = qa_mon_max2[grep("cr23x.*airtemp_max", qa_mon_max2)]) #okay

# clean up environment and move on..
rm(flag_mon_max1, check_monthly_max1, check_monthly_max2, check_monthly_min,
   qa_mon_max1, qa_mon_max2, qa_mon_min1)



# -- QA LAG SPIKES/FLATLINES WITHIN LOGGER DATASET ----
# look for consecutive day temp spikes/declines that exceed 40C within the same metric
# also look for flatlining in temperature (jen morse said try 5-consec days)

# save copy of working dat in case mess up (so don't need to re-run from top)
working_dat_copy <- working_dat

## add lag temp
working_dat <- working_dat %>%
  arrange(met, date) %>%
  group_by(logger, met) %>%
  mutate(lag1_crtemp = lag(cr_temp)) %>%
  ungroup() %>%
  mutate(lag1_diffcr = abs(cr_temp - lag1_crtemp))
# check distribution of absolute difference with current day's temp and yesterday's temp
boxplot(working_dat$lag1_diffcr)  
sapply(split(working_dat$lag1_diffcr, working_dat$met), function(x) tail(sort(x))) # at most swung 19 degrees.. could happen
sapply(split(working_dat$lag1_diffcr, working_dat$logger), function(x) tail(sort(x))) # loggers swing similarly

# can visualize swings to be sure
swing_check1 <- subset(working_dat, lag1_diffcr >= 15) 
qa_swings <- visual_qa(working_dat, swing_check1)
plot_grid(plotlist = qa_swings)
# > conclusion: other sources swung similar amounts too, is fine

# look for 4+ consecutive days of 0 change
count0 <- rle(working_dat$lag1_diffcr)
consec0 <- count0$lengths[count0$values == 0]
consec0[!is.na(consec0)] # 0 change in consec days only ever occurs for runs of 1 day, which is fine

# to be sure, compare lead delta
working_dat %>%
  arrange(met, date) %>%
  group_by(logger, met) %>%
  mutate(lead1_crtemp = lead(cr_temp)) %>%
  ungroup() %>%
  mutate(lead_diffcr = abs(cr_temp - lead1_crtemp)) %>%
  subset(lead_diffcr == 0 & lag1_diffcr == 0) %>%
  nrow() # nada, all clear

# clean up environment
rm(count0, consec0, swing_check1, qa_swings)



# -- QA DAY-TO-DAY DELTA DEVIANCE -----
# diff current from lag temp in sdl chart, d1 and c1, then compare daily deltas with logger daily deltas
# pull out observations where delta deviates more than 3sd of logger-other source diff on day-to-day fluxes














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

