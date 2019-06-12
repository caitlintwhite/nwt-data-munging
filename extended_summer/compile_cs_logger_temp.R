# compile campbell scientific logger temp and infill with saddle chart
# (streamlined version after comparing all saddle temperature datasets)


# script purpose:
# read in all campbell scientific logger datasets, saddle chart


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
cr21x <- read_csv("http://pasta.lternet.edu/package/data/eml/knb-lter-nwt/78/2/74edfb5a907b5d4960d1e1dbe08faba4", col_names = FALSE, na = na_vals, trim_ws = T)
cr21xeml <- readLines("https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-nwt.78.2&contentType=application/xml")
cr21x_names <- cr21xeml[grep("attributeName", cr21xeml)] %>%
  str_extract(">.*<") %>% 
  gsub("<|>", "",.)
#set colnames for cr21x data
colnames(cr21x) <- cr21x_names
#crlogs <- getTabular() # most current logger dataset not on EDI yet, provided by SCE
crlogs <- read.csv("/Users/serahsierra/Documents/nwt_lter/unpub_data/sdlcr23x-cr1000.daily.ml.data.csv",
                   strip.white = T, na.strings = c("", " ", ".", NA, NaN, "NA", "NaN"))
sdlchart <- getTabular(413)

#read in d1 airtemp as check
d1 <- getTabular(412)
c1 <- getTabular(411)

# -- REVIEW DATA -----
# review how dats read in
glimpse(cr21x) # no flag cols, date is date class
glimpse(crlogs) # flag cols present
glimpse(sdlchart) # flags cols present
glimpse(d1) # flag cols present
glimpse(c1) # flag cols present

# drop cols not needed in cr logger dataset (i.e. only need airtemp cols up thru avg airtemp)
crlogs <- crlogs[,1:12]
# drop non airtemp cols in cr21x
cr21x <- cr21x[grepl("date|Julian|maximum temp|minimum temp", colnames(cr21x))]

# convert dates in all to Date class
crlogs$date <- as.Date(crlogs$date, format = "%Y-%m-%d")
sdlchart$date <- as.Date(sdlchart$date, format = "%Y-%m-%d")

# unique values of flags? and frequency of their occurrence?
sapply(crlogs[grepl("flag", colnames(crlogs))], function(x) summary(as.factor(x))) # only n's, correspond to no flag
sapply(sdlchart[grepl("flag", colnames(sdlchart))], function(x) summary(as.factor(x))) # 246 tmax, 238 tmin "1" flags; 14 tmax, 16 tmin "2" flags
# saddle chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available)

# look at tails for any obvious bad values
## cr 21x
sapply(cr21x[grepl("temp", colnames(cr21x))], function(x) tail(sort(x))) #31.45.. Jen Morse said she thinks max T shouldn't exceed 30, and is it reasonable max/min temp occured at midnight?
sapply(cr21x[grepl("temp", colnames(cr21x))], function(x) tail(sort(x, decreasing = T))) #-75 and -6999 in airtemp_min
## cr 23x, 1000 data loggers
sapply(crlogs[grepl("^airtemp", colnames(crlogs))], function(x) tail(sort(x))) #okay
sapply(crlogs[grepl("^airtemp", colnames(crlogs))], function(x) tail(sort(x, decreasing = T))) # -187 in min temp
## sdl chart
sapply(sdlchart[grepl("^airtemp", colnames(sdlchart))], function(x) tail(sort(x))) #okay, high max value agrees with cr logger high value
sapply(sdlchart[grepl("^airtemp", colnames(sdlchart))], function(x) tail(sort(x, decreasing = T))) #okay
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
summary(sdlchart[grepl("^airtemp", colnames(sdlchart))]) # max T is 25, minT -38

# look at maxT in cr21x further..
cr21x[cr21x$`maximum temperature` == 31.45 & !is.na(cr21x$`maximum temperature`),] # time of maxT seems reasonable (11:47)
# 10days before and after 31.45
ggplot(data = cr21x[(which(cr21x$`maximum temperature` == 31.45)-10):(which(cr21x$`maximum temperature` == 31.45)+10),],
       aes(date, `maximum temperature`)) +
  geom_line() +
  geom_point() +
  # add sdl chart temp for comparison (purple dots)
  geom_point(data = sdlchart[(which(sdlchart$date == "1990-07-09")-10):(which(sdlchart$date == "1990-07-09")+10),], aes(date, airtemp_max), col = "purple", pch = 1)
# conclusion: 31.45 looks like a bad value

cr21x[cr21x$`maximum temperature` == 28.41 & !is.na(cr21x$`maximum temperature`),] # time of maxT seems reasonable (11:26), minT missing so suspect
# 10days before and after 28.41
ggplot(data = cr21x[(which(cr21x$`maximum temperature` == 28.41)-10):(which(cr21x$`maximum temperature` == 28.41)+10),],
       aes(date, `maximum temperature`)) +
  geom_line() +
  geom_point() +
  # add sdl chart temp for comparison (purple dots)
  geom_point(data = sdlchart[(which(sdlchart$date == "1997-07-30")-10):(which(sdlchart$date == "1997-07-30")+10),], aes(date, airtemp_max), col = "purple", pch = 1)
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
  geom_line(data = sdlchart[(which(sdlchart$date == "2000-09-06")-10):(which(sdlchart$date == "2000-09-06")+10),], aes(date, airtemp_max), col = "purple") +
  geom_point(data = sdlchart[(which(sdlchart$date == "2000-09-06")-10):(which(sdlchart$date == "2000-09-06")+10),], aes(date, airtemp_max), col = "purple", pch = 1) +
  geom_line(data = d1[(which(d1$date == "2000-09-06")-10):(which(d1$date == "2000-09-06")+10),], aes(date, airtemp_max), col = "steelblue2") +
  geom_point(data = d1[(which(d1$date == "2000-09-06")-10):(which(d1$date == "2000-09-06")+10),], aes(date, airtemp_max), col = "steelblue4", pch = 1)
# conclusion: +25C looks like a bad value in the CR23x logger, all other data around then fine

crlogs[crlogs$airtemp_min == min(crlogs$airtemp_min, na.rm =T) & !is.na(crlogs$airtemp_min),] # time of maxT seems reasonable (11:47)
# 10days before and after 31.45
ggplot(data = crlogs[(which(crlogs$airtemp_min == min(crlogs$airtemp_min, na.rm =T))-10):(which(crlogs$airtemp_min == min(crlogs$airtemp_min, na.rm =T))+10),],
       aes(date, airtemp_min)) +
  geom_line() +
  geom_point() +
  # add sdl chart temp for comparison (purple dots)
  geom_line(data = sdlchart[(which(sdlchart$date == "2011-02-01")-10):(which(sdlchart$date == "2011-02-01")+10),], aes(date, airtemp_min), col = "purple") +
  geom_point(data = sdlchart[(which(sdlchart$date == "2011-02-01")-10):(which(sdlchart$date == "2011-02-01")+10),], aes(date, airtemp_min), col = "purple", pch = 1) +
  geom_line(data = d1[(which(d1$date == "2011-02-01")-10):(which(d1$date == "2011-02-01")+10),], aes(date, airtemp_min), col = "steelblue2") +
  geom_point(data = d1[(which(d1$date == "2011-02-01")-10):(which(d1$date == "2011-02-01")+10),], aes(date, airtemp_min), col = "steelblue4", pch = 1)
# conclusion: real value. twas cold that day.

# change high value in cr23x to NA
# 25.46
crlogs$airtemp_max[crlogs$airtemp_max == max(crlogs$airtemp_max, na.rm =T) & !is.na(crlogs$airtemp_max)] <- NA

# look at cr21x with other loggers one last time:
summary(cr21x[grepl("temp", colnames(cr21x))])
lapply(split(crlogs[grepl("^airtemp", colnames(crlogs))], crlogs$logger), summary) # max T is 25
summary(sdlchart[grepl("^airtemp", colnames(sdlchart))]) # max T is 25

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


# -- TIDY CHART COMPARISON DATASETS -----
# prep chart datasets to join with logger datasets in long-form
#c1 airtemp
c1_long <- c1 %>%
  dplyr::select(-airtemp_avg) %>%
  gather(met, c1_temp, airtemp_max:ncol(.)) %>%
  arrange(met, date)
c1_flags <- c1_long %>%
  filter(grepl("flag", met)) %>%
  rename(c1_flag = c1_temp) %>%
  mutate(met = gsub("flag_", "", met))
c1_long <- subset(c1_long, !grepl("flag", met)) %>%
  # add month and year
  mutate(yr = year(date),
         mon = month(date)) %>%
  left_join(c1_flags) %>%
  # lag and lead temp by min/max temp
  group_by(met) %>%
  mutate(lag1_c1temp = lag(c1_temp),
         lead1_c1temp = lead(c1_temp)) %>%
  ungroup() %>%
  # diff temp val from lag and lead
  mutate(delta_c1lag = c1_temp - lag1_c1temp,
         delta_c1lead = c1_temp - lead1_c1temp) %>%
  # flag anomoly as +4sd departure from lag and lead temps
  group_by(met, mon) %>%
  mutate(delta_c1thresh_lag = (mean(abs(delta_c1lag), na.rm = T)) + 4*(sd(abs(delta_c1lag), na.rm = T)),
         delta_c1thresh_lead = mean(abs(delta_c1lead), na.rm = T) + 4*(sd(abs(delta_c1lag), na.rm = T))) %>%
  ungroup() %>%
  # flag departures
  mutate(flag_c1deltalag = ifelse(abs(delta_c1lag)> delta_c1thresh_lag, 1, 0),
         flag_c1deltalead = ifelse(abs(delta_c1lead)> delta_c1thresh_lead, 1, 0)) %>%
  dplyr::select(date, yr, mon, met:ncol(.))

# d1 airtemp
d1_long <- d1 %>%
  dplyr::select(-airtemp_avg) %>%
  gather(met, d1_temp, airtemp_max:ncol(.)) %>%
  arrange(met, date)
d1_flags <- d1_long %>%
  filter(grepl("flag", met)) %>%
  rename(d1_flag = d1_temp) %>%
  mutate(met = gsub("flag_", "", met))
d1_long <- subset(d1_long, !grepl("flag", met)) %>%
  # add month and year
  mutate(yr = year(date),
         mon = month(date)) %>%
  left_join(d1_flags) %>%
  # lag and lead temp by min/max temp
  group_by(met) %>%
  mutate(lag1_d1temp = lag(d1_temp),
         lead1_d1temp = lead(d1_temp)) %>%
  ungroup() %>%
  # diff temp val from lag and lead
  mutate(delta_d1lag = d1_temp - lag1_d1temp,
         delta_d1lead = d1_temp - lead1_d1temp) %>%
  # flag anomoly as +4sd departure from lag and lead temps
  group_by(met, mon) %>%
  mutate(delta_d1thresh_lag = (mean(abs(delta_d1lag), na.rm = T)) + 4*(sd(abs(delta_d1lag), na.rm = T)),
         delta_d1thresh_lead = mean(abs(delta_d1lead), na.rm = T) + 4*(sd(abs(delta_d1lag), na.rm = T))) %>%
  ungroup() %>%
  # flag departures
  mutate(flag_d1deltalag = ifelse(abs(delta_d1lag)> delta_d1thresh_lag, 1, 0),
         flag_d1deltalead = ifelse(abs(delta_d1lead)> delta_d1thresh_lead, 1, 0)) %>%
  dplyr::select(date, yr, mon, met:ncol(.))


#sdl airtemp
sdlchart_long <- sdlchart %>%
  dplyr::select(-airtemp_avg) %>%
  gather(met, sdlchart_temp, airtemp_max:ncol(.)) %>%
  arrange(met, date)
sdlchart_flags <- sdlchart_long %>%
  filter(grepl("flag", met)) %>%
  rename(sdlchart_flag = sdlchart_temp) %>%
  mutate(met = gsub("flag_", "", met))
sdlchart_long <- subset(sdlchart_long, !grepl("flag", met)) %>%
  # add month and year
  mutate(yr = year(date),
         mon = month(date)) %>%
  left_join(sdlchart_flags) %>%
  # lag and lead temp by min/max temp
  group_by(met) %>%
  mutate(lag1_sdlcharttemp = lag(sdlchart_temp),
         lead1_sdlcharttemp = lead(sdlchart_temp)) %>%
  ungroup() %>%
  # diff temp val from lag and lead
  mutate(delta_sdlchartlag = sdlchart_temp - lag1_sdlcharttemp,
         delta_sdlchartlead = sdlchart_temp - lead1_sdlcharttemp) %>%
  # flag anomoly as +4sd departure from lag and lead temps
  group_by(met, mon) %>%
  mutate(delta_sdlchartthresh_lag = (mean(abs(delta_sdlchartlag), na.rm = T)) + 4*(sd(abs(delta_sdlchartlag), na.rm = T)),
         delta_sdlchartthresh_lead = mean(abs(delta_sdlchartlead), na.rm = T) + 4*(sd(abs(delta_sdlchartlag), na.rm = T))) %>%
  ungroup() %>%
  # flag departures
  mutate(flag_sdlchartdeltalag = ifelse(abs(delta_sdlchartlag)> delta_sdlchartthresh_lag, 1, 0),
         flag_sdlchartdeltalead = ifelse(abs(delta_sdlchartlead)> delta_sdlchartthresh_lead, 1, 0)) %>%
  dplyr::select(date, yr, mon, met:ncol(.))

# clean up
rm(d1_flags, c1_flags, sdlchart_flags)






# -- AUTOMATE QA LOGGER TEMP DATA -----
# flags:
# max temp !< min temp
# min temp !> max temp
# jen morse said at sdl, tmax shouldn't exceed 25C and tmin shouldn't fall below -40C
# diurnal temp !> defined allowance (shouldn't swing more than 35C in a day?)
# jen liked the flag for temp value more than 3sd outside monthly mean, and day-to-day flux more than 3sd outside monthly day-to-day flux
# need to screen out obvious/unreasonable/improbable values (e.g. -187C) before calculating means and sds (so outliers don't influence those values)
# perhaps compare delta of logger with sdl chart and d1 chart.. if both differ by more than usual (e.g. +3sd, review data point/perhaps change to NA)

# step 1: flag cr21x using d1 and sdl chart AND timestamps (esp. in summer months, not as useful in winter since clock might have frozen/malfunctioned in cold weather)
cr21x_long <- cr21x %>%
  # change names to match other logger and chart colnames
  rename(jday = `Julian day`,
         airtemp_max = `maximum temperature`,
         airtemp_min = `minimum temperature`,
         time_tmax = `time of maximum temperature`,
         time_tmin = `time of minimum temperature`)
cr21x_times <- dplyr::select(cr21x_long, date, jday, time_tmax, time_tmin) %>%
  # rename time cols for joining back in to long-form temp dataset based date and metric
  rename(airtemp_max = time_tmax, 
         airtemp_min = time_tmin) %>%
  # tidy time cols
  gather(met, time, airtemp_max:airtemp_min)
# drop time cols from cr21x dataset, tidy temp cols, and join time in 
cr21x_long <- dplyr::select(cr21x_long, -c(time_tmax, time_tmin)) %>%
  gather(met, temp, airtemp_max:airtemp_min) %>%
  left_join(cr21x_times) %>%
  # add in month and year columns
  mutate(yr = year(date),
         mon = month(date)) %>%
  # rearrange cols
  dplyr::select(date, yr, mon, jday:ncol(.))
# clean up
rm(cr21x_times)

# how many tmax temps occurred between 11pm and 1am?
nrow(subset(cr21x_long, met == "airtemp_max" & time > 2300 | time < 100)) #711! boo.
View(subset(cr21x_long, met == "airtemp_max" & time > 2300 | time < 100))
# what is the breakdown of count tmax where timestamp between 2300 & 0100 by month?
group_by(subset(cr21x_long, met == "airtemp_max" & time > 2300 | time < 100), mon) %>%
  summarize(ct = length(time)) # most occur in winter months (e.g. 118 in december), but there are a decent amount in summer months

# diff logger temp from d1 and sdl chart, if timestamp in unexpected window (e.g. around midnight for tmax, flag it)
cr21x_long2 <- cr21x_long %>%
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
  left_join(dplyr::select(d1_long, date:delta_d1lag)) %>%
  left_join(dplyr::select(sdlchart_long, date:delta_sdlchartlag)) %>%
  left_join(dplyr::select(c1_long, date:delta_c1lag)) %>%
  mutate(d1_delta = temp-d1_temp,
         diff_d1_deltalag = delta_lag - delta_d1lag,
         sdlchart_delta = temp-sdlchart_temp,
         diff_sdlchart_deltalag = delta_lag - delta_sdlchartlag,
         c1_delta = temp-c1_temp,
         diff_c1_deltalag = delta_lag - delta_c1lag) %>%
  # group by monthly trends for flags
  group_by(met, mon) %>%
  mutate(mean_temp = mean(temp, na.rm = T),
         sd_temp = sd(temp, na.rm = T),
         mean_d1delta = mean(d1_delta, na.rm = T),
         sd_d1delta = sd(d1_delta, na.rm = T),
         mean_sdlchartdelta = mean(sdlchart_delta, na.rm = T),
         sd_sdlchartdelta = sd(sdlchart_delta, na.rm =T),
         mean_c1delta = mean(c1_temp, na.rm = T),
         sd_c1delta = sd(c1_delta, na.rm = T),
         mean_d1deltalag = mean(diff_d1_deltalag, na.rm = T),
         sd_d1deltalag = sd(diff_d1_deltalag, na.rm = T),
         mean_sdlchartdeltalag = mean(diff_sdlchart_deltalag, na.rm = T),
         sd_sdlchartdeltalag = sd(diff_sdlchart_deltalag, na.rm = T),
         mean_c1deltalag = mean(diff_c1_deltalag, na.rm = T),
         sd_c1deltalag = sd(diff_c1_deltalag, na.rm = T)) %>%
  ungroup() %>%
  # flags vals 3sd outside normal range; flag time if tmax around midnight or tmin in btwn 10am and 2pm (both unlikely)
  mutate(flag_temp = ifelse((mean_temp - temp) > (mean_temp + 3*sd_temp) | (mean_temp - temp) < (mean_temp - 3*sd_temp), 1, 0),
         # make temporary unflagged time flag col
         flag_time = 0,
         flag_d1_delta = ifelse((mean_d1delta - d1_delta) > (mean_d1delta + 3*sd_d1delta) | (mean_d1delta - d1_delta) < (mean_d1delta - 3*sd_d1delta), 1, 0),
         flag_sdlchart_delta = ifelse((mean_sdlchartdelta - sdlchart_delta) > (mean_sdlchartdelta + 3*sd_sdlchartdelta) | (mean_sdlchartdelta - sdlchart_delta) < (mean_sdlchartdelta - 3*sd_sdlchartdelta), 1, 0),
         flag_c1_delta = ifelse((mean_c1delta - c1_delta) > (mean_c1delta + 3*sd_c1delta) | (mean_c1delta - c1_delta) < (mean_c1delta - 3*sd_c1delta), 1, 0),
         flag_d1_deltalag = ifelse((mean_d1deltalag - diff_d1_deltalag) > (mean_d1deltalag + 3*sd_d1deltalag) | (mean_d1deltalag - diff_d1_deltalag) < (mean_d1deltalag - 3*sd_d1deltalag), 1, 0),
         flag_sdlchart_deltalag = ifelse((mean_sdlchartdeltalag - diff_sdlchart_deltalag) > (mean_sdlchartdeltalag + 3*sd_sdlchartdeltalag) | (mean_sdlchartdeltalag - diff_sdlchart_deltalag) < (mean_sdlchartdeltalag - 3*sd_sdlchartdeltalag), 1, 0),
         flag_c1_deltalag = ifelse((mean_c1deltalag - diff_c1_deltalag) > (mean_c1deltalag + 3*sd_c1deltalag) | (mean_c1deltalag - diff_c1_deltalag) < (mean_c1deltalag - 3*sd_c1deltalag), 1, 0))

# flag time val by temp measurement
## flag tmax if timestamp at night (after 7pm to before 6am)
cr21x_long2$flag_time[cr21x_long2$met == "airtemp_max"] <- with(cr21x_long2[cr21x_long2$met == "airtemp_max",],
                                                                ifelse(time > 1900 | time < 600, 1, 0))
## flag tmin if timestamp between 10am and 2pm
cr21x_long2$flag_time[cr21x_long2$met == "airtemp_min"] <- with(cr21x_long2[cr21x_long2$met == "airtemp_min",],
                                                                ifelse(time > 1000 & time < 1400, 1, 0))  

cr21x_long3 <- cr21x_long2[,!grepl("^mean|^sd", colnames(cr21x_long2))]


# plot out flagged values
ggplot(subset(cr21x_long2, flag_d1_delta == 1 & flag_sdlchart_delta == 1 & flag_c1_delta == 1)) +
  geom_point(aes(mon, temp)) +
  geom_point(aes(mon, sdlchart_temp), col = "purple", alpha = 0.5) +
  geom_point(aes(mon, d1_temp), col = "steelblue4", alpha = 0.5 ) +
  geom_point(aes(mon, c1_temp), col = "forestgreen", alpha = 0.5 ) +
  facet_wrap(~met, scales = "free_y")

test <- subset(cr21x_long2, flag_d1_delta == 1 & flag_sdlchart_delta == 1 & flag_c1_delta == 1)
test <- subset(cr21x_long2, flag_sdlchart_delta == 1 & flag_d1_delta == 1 | flag_c1_delta == 1)
test <- subset(cr21x_long3, flag_d1_deltalag == 1 & flag_sdlchart_deltalag == 1 & flag_c1_deltalag == 1)

plot_list <- list()
for(m in c("airtemp_max", "airtemp_min")){
  tempdf <- subset(test, met == m) %>% as.data.frame()
  
  for(d in as.character(tempdf$date)){
    d <- as.Date(d, format = "%Y-%m-%d")
    tempplot <- ggplot(data = subset(cr21x_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)),
                       aes(date, temp)) +
      geom_line() +
      geom_point() +
      # circle the flagged value in red
      geom_point(data = subset(cr21x_long, met == m & date == as.Date(d)),
                 aes(date, temp), col = "red", pch  = 1, size = 3) +
      labs(y = gsub("airtemp_", "T", m),
           x = year(d)) +
      # add sdl chart temp for comparison (purple dots)
      geom_line(data = subset(sdlchart_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), aes(date, sdlchart_temp), col = "purple") +
      geom_point(data = subset(sdlchart_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), aes(date, sdlchart_temp), col = "purple", pch = 1) +
      geom_line(data = subset(d1_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), aes(date, d1_temp), col = "steelblue2") +
      geom_point(data = subset(d1_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), aes(date, d1_temp), col = "steelblue4", pch = 1) +
      geom_line(data = subset(c1_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), aes(date, c1_temp), col = "forestgreen") +
      geom_point(data = subset(c1_long, met == m & date %in% seq(as.Date(d)-10, as.Date(d)+10, 1)), aes(date, c1_temp), col = "darkgreen", pch = 1) +
      theme_bw()
    
    # store plot in a list
    plot_list[length(plot_list)+1] <- list(tempplot)
  }
}

plot_grid(plotlist = plot_list)
# test$date[test$met == "airtemp_max"][2]
# plot second example
ggplot(data = subset(cr21x, date %in% seq(as.Date(test$date[test$met == "airtemp_max"][2])-10, as.Date(test$date[test$met == "airtemp_max"][2])+10, 1)),
       aes(date, `maximum temperature`)) +
  geom_line() +
  geom_point() +
  # add sdl chart temp for comparison (purple dots)
  geom_line(data = sdlchart[(which(sdlchart$date == test$date[test$met == "airtemp_max"][2])-10):(which(sdlchart$date == test$date[test$met == "airtemp_max"][2])+10),], aes(date, airtemp_max), col = "purple") +
  geom_point(data = sdlchart[(which(sdlchart$date == test$date[test$met == "airtemp_max"][2])-10):(which(sdlchart$date == test$date[test$met == "airtemp_max"][2])+10),], aes(date, airtemp_max), col = "purple", pch = 1) +
  geom_line(data = d1[(which(d1$date == test$date[test$met == "airtemp_max"][2])-10):(which(d1$date == test$date[test$met == "airtemp_max"][2])+10),], aes(date, airtemp_max), col = "steelblue2") +
  geom_point(data = d1[(which(d1$date == test$date[test$met == "airtemp_max"][2])-10):(which(d1$date == test$date[test$met == "airtemp_max"][2])+10),], aes(date, airtemp_max), col = "steelblue4", pch = 1) +
  geom_line(data = c1[(which(c1$date == test$date[test$met == "airtemp_max"][2])-10):(which(c1$date == test$date[test$met == "airtemp_max"][2])+10),], aes(date, airtemp_max), col = "forestgreen") +
  geom_point(data = c1[(which(c1$date == test$date[test$met == "airtemp_max"][2])-10):(which(c1$date == test$date[test$met == "airtemp_max"][2])+10),], aes(date, airtemp_max), col = "darkgreen", pch = 1)

