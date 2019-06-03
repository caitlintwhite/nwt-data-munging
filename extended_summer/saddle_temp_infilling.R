# infilling for saddle temp 2015-current

# IMPORTANTE!: saddle chart recording discontinued after 31 Dec 2017; estimate saddle chart temp with saddle CR23X data logger using simple linear regression

# hierarchy of infill methods for temp (based on methods in saddle chart metadata)
# 1) 2 week moving window regression using saddle logger data
# 2) monthly regression using saddle logger data
# 3) std. deviation ratio method using logger data
# 4) 2 week moving window regression using D1 chart data
# 5) monthly regression using D1 chart data
# 6) std. deviation ratio method using D1 chart

# some rules:
# adj R^2 > 0.6 (higher better)
# pval =< 0.05



# -- SETUP ------
# clean environment, load needed libraries, modify default settings
rm(list = ls())
library(tidyverse)
library(lubridate)
options(stringsAsFactors = F)
theme_set(theme_bw())


# -- FUNCTIONS -----
# SCE + CTW code to determine most recent version of package ID and read in current dataset on EDI
#function to determine current version of data package on EDI
getCurrentVersion<-function(edi_id){
  versions=readLines(paste0('https://pasta.lternet.edu/package/eml/knb-lter-nwt/', edi_id), warn=FALSE)
  currentV <- max(as.numeric(versions))
  return(currentV)
}
# function to get entity ID for current version
getEntityId <- function(edi_id, version){
  entID <- readLines(paste0('https://pasta.lternet.edu/package/eml/knb-lter-nwt/', edi_id, "/", version, "/"), warn=FALSE)[1]
  entID <- gsub(paste0("http.*/",edi_id,"/",version,"/"), "", entID) # remove all chars except what comes after last /
  return(entID)
}
# reads in tabular dataset for data package that has only one csv data file (could make more generic with read table, but should know what you're reading in to use)
getTabular <- function(edi_id, na_vals = c("", "NA", NA, NaN, ".", "NaN", " ")){
  v <- getCurrentVersion(edi_id)
  id <- getEntityId(edi_id, v)
  dat <- read.csv(paste0("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.", edi_id, ".", v, 
                         "&entityid=", id),
                  strip.white =TRUE, na.strings = na_vals)
  print(paste0("Reading in knb-lter-nwt.", edi_id, ".", v))
  return(dat)
}


# -- GET DATA -----
# read in NWT Saddle temperature datasets
# > read in from EDI data portal
# sdl chart
sdl_charttemp <- getTabular(413)
# cr23x logger
#sdl_loggerdat <- getTabular(405)
## SCE unpublished cr23x dataset thru current (SCE and JM still troubleshooting solar rad data)
sdl_loggerdat <- read.csv("/Users/serahsierra/Documents/nwt_lter/unpub_data/sdlcr23x-cr1000.daily.ml.data.csv",
                          strip.white = T, na.strings = c("", " ", ".", NA, NaN, "NA", "NaN"))
# c1 chart
c1_charttemp <- getTabular(411)
# d1 chart
d1_charttemp <- getTabular(412)
# Jennings, Molotch, and Kittel (2018) infilled data for sdl, d1 and c1
# Keith Jennings et al. infilled *hourly* data
Jennings_infill <- getTabular(168)

# data used in NSF proposal
# Hope Humphries sent the infilled saddle data to Emily Farrer, CTW doesn't know how infilled
# assume infilled data sent to HH from Tim Kittel, and methodology likely similar to that used in Jennings, Molotch, and Kittel (2018)
NSF_temp <- read.csv("~/Dropbox/NWT_data/Saddle_precip_temp_formoisturedeficit.csv") %>%
  mutate(`date` = as.Date(paste(Year,Month,Day, sep="-")),
         LTER_site = "NWT",
         local_site = "sdl") %>%
  dplyr::select(LTER_site, local_site, date, TMIN, TMAX)


# -- DATA PREP/CLEAN UP -----
glimpse(sdl_loggerdat);names(sdl_loggerdat)
# keep only temp data from logger (don't need other variables right now)
sdl_loggerdat <- sdl_loggerdat[,1:12] # should keep "LTER_site" through "flag_airtemp_avg 
# what are the flags?
sapply(sdl_loggerdat[grep("flag",names(sdl_loggerdat))], unique) #"n" or "NA"
# how many missing vals by year?
sapply(split(sdl_loggerdat[c("airtemp_min", "airtemp_max", "airtemp_avg")], sdl_loggerdat$year), summary) # no NAs 2013 onwards

glimpse(sdl_charttemp)
# what are the flags?
sapply(sdl_charttemp[grep("flag",names(sdl_charttemp))], unique) #NA, 2 or 1 
# how many missing vals by year?
sapply(split(sdl_charttemp[c("airtemp_min", "airtemp_max", "airtemp_avg")], substr(sdl_charttemp$date,1,4)), summary) # yikes 2013, 2014; only NAs 2010-2017

glimpse(d1_charttemp)
glimpse(c1_charttemp)
glimpse(Jennings_infill)
glimpse(NSF_temp)

#convert dates in all chart and logger datasets to date class
sdl_charttemp$date <- as.Date(sdl_charttemp$date, format = "%Y-%m-%d")
sdl_loggerdat$date <- as.Date(sdl_loggerdat$date, format = "%Y-%m-%d")
c1_charttemp$date <- as.Date(c1_charttemp$date, format = "%Y-%m-%d")
d1_charttemp$date <- as.Date(d1_charttemp$date, format = "%Y-%m-%d")
Jennings_infill$date <- as.Date(Jennings_infill$date, format = "%Y-%m-%d")

# summarise Jennings et al. data to daily max, min, or mean (temp)
Jennings_summarized <- Jennings_infill %>%
  group_by(LTER_site, local_site, year, date, jday) %>%
  summarise(J_tmax = max(airtemp_avg),
            J_tmin = min(airtemp_avg),
            J_tmean = mean(airtemp_avg))




###########################
## Visual inspection ###
########################
# what's missing in 2015-2017 years?
sdl_charttemp %>%
  mutate(yr = year(date),
         mon = month(date),
         dy = day(date),
         doy = yday(date)) %>%
  dplyr::select(date, yr, mon, dy, doy, airtemp_max, airtemp_min, airtemp_avg) %>%
  gather(metric, value, airtemp_max:airtemp_avg) %>%
  filter(yr > 2014) %>%
  ggplot(aes(doy, value)) +
  geom_line(col="grey50") +
  geom_point(alpha=0.5) +
  labs(y="Daily temperature (°C)") +
  facet_grid(metric~yr, scales = "free_y") +
  theme_bw()

# 2010 (first NAs and on?)?
sdl_charttemp %>%
  mutate(yr = year(date),
         mon = month(date),
         dy = day(date),
         doy = yday(date)) %>%
  dplyr::select(date, yr, mon, dy, doy, airtemp_max, airtemp_min, airtemp_avg) %>%
  gather(metric, value, airtemp_max:airtemp_avg) %>%
  filter(yr > 2009) %>%
  ggplot(aes(doy, value)) +
  geom_line(col="grey50") +
  geom_point(alpha=0.5) +
  labs(y="Daily temperature (°C)") +
  facet_grid(metric~yr, scales = "free_y") +
  theme_bw()

sdl_charttemp %>%
  mutate(yr = year(date),
         mon = month(date),
         dy = day(date),
         doy = yday(date)) %>%
  dplyr::select(date, yr, mon, dy, doy, airtemp_max, airtemp_min, airtemp_avg) %>%
  gather(metric, value, airtemp_max:airtemp_avg) %>%
  filter(yr > 2009) %>%
  group_by(yr, mon, metric) %>%
  summarise(missing = sum(is.na(value))) %>%
  ggplot(aes(as.factor(mon), missing)) +
  geom_point() +
  labs(y="Missing days", x = "Month") +
  facet_grid(metric~yr) +
  theme_bw() # 2017 has the most missing days .. overall missing days most often in non-summer months (fewer ppl up there checking equip.)

## > Can I use Keith's D1 or C1 infilled data for infilling??
### >> Answer: No, too different from chart data. Stopped after looking at D1..
# plot Jennings et al. against chart data to see if can use Jennings sdl data for regression
sdl_temp_all <- inner_join(sdl_charttemp, Jennings_summarized) %>%
  left_join(NSF_temp)
colnames(sdl_loggerdat)[grep("airtemp", names(sdl_loggerdat))] <- paste0("CR23", colnames(sdl_loggerdat)[grep("airtemp", names(sdl_loggerdat))]) 
sdl_temp_all <- left_join(sdl_temp_all, sdl_loggerdat)

# compare saddle chart with Jennings infilled
par(mfrow=c(1,2))
boxplot(sdl_temp_all$airtemp_max - sdl_temp_all$J_tmax,
        ylab = "chart tmax - Jennings tmax")
boxplot(sdl_temp_all$airtemp_max[sdl_temp_all$flag_airtemp_max==1] - sdl_temp_all$J_tmax[sdl_temp_all$flag_airtemp_max==1],
        ylab = "chart tmax (no flags) - Jennings infilled")

# compare saddle chart with NSF proposal data (1990 onwards)
boxplot(sdl_temp_all$airtemp_max - sdl_temp_all$TMAX,
        ylab = "chart tmax - NSF tmax") # no difference
boxplot(sdl_temp_all$airtemp_max[sdl_temp_all$flag_airtemp_max==1] - sdl_temp_all$TMAX[sdl_temp_all$flag_airtemp_max==1],
        ylab = "chart tmax (no flags) - NSF tmax")

# compare saddle chart with CR23X logger data (2000 onwards)
boxplot(sdl_temp_all$airtemp_max - sdl_temp_all$CR23airtemp_max,
        ylab = "chart tmax - CR23X tmax") 
boxplot(sdl_temp_all$airtemp_max[sdl_temp_all$flag_airtemp_max==1] - sdl_temp_all$CR23airtemp_max[sdl_temp_all$flag_airtemp_max==1],
        ylab = "chart tmax (no flgs) - CR23X tmax") # logger dat is warmer
boxplot(sdl_temp_all$airtemp_min - sdl_temp_all$CR23airtemp_min,
        ylab = "chart tmin - CR23X tmin") 
boxplot(sdl_temp_all$airtemp_min[sdl_temp_all$flag_airtemp_min==1] - sdl_temp_all$CR23airtemp_min[sdl_temp_all$flag_airtemp_min==1],
        ylab = "chart tmin (no flags) - CR23X tmin (no flags)")
plot(sdl_temp_all$airtemp_max, sdl_temp_all$CR23airtemp_max)
abline(a = 0, b = 1, col = "red") #tmax logger dat typically warmer than chart, with some noticeable exceptions
plot(sdl_temp_all$airtemp_min, sdl_temp_all$CR23airtemp_min)
abline(a = 0, b = 1, col = "red") #tmin logger dat pairs a little better with chart tmin, but still consistently warmer
par(mfrow = c(1,1))
# is diurnal comparable?
plot(sdl_temp_all$airtemp_max - sdl_temp_all$airtemp_min, sdl_temp_all$CR23airtemp_max - sdl_temp_all$CR23airtemp_min)
abline(a = 0, b = 1, col = "red") # diurnal wider range for logger dat..

# compare D1 chart with Jennings infilled
d1_temp_all <- inner_join(d1_charttemp, Jennings_summarized)

# compare saddle chart with Jennings infilled
par(mfrow = c(1,3))
boxplot(d1_temp_all$airtemp_max - d1_temp_all$J_tmax,
        ylab = "D1 chart tmax - Jennings tmax")
boxplot(d1_temp_all$airtemp_max[d1_temp_all$flag_airtemp_max==1] - d1_temp_all$J_tmax[d1_temp_all$flag_airtemp_max==1],
        ylab = "D1 chart tmax (no flags) - Jennings tmax")
boxplot(d1_temp_all$airtemp_min - d1_temp_all$J_tmin,
        ylab = "D1 chart tmin - Jennings tmin")
par(mfrow = c(1,1))

ggplot(d1_temp_all, aes(jday, airtemp_max)) +
  geom_point(alpha = 0.5) +
  geom_point(aes(jday, J_tmax), col = "dodgerblue", pch=1) +
  ggtitle("TMAX: D1 chart (black) vs. Jennings infilled (blue)") +
  facet_wrap(~year)

ggplot(d1_temp_all, aes(jday, airtemp_min)) +
  geom_point(alpha = 0.5) +
  geom_point(aes(jday, J_tmin), col = "dodgerblue", pch=1, alpha=0.5) +
  ggtitle("TMIN: D1 chart (black) vs. Jennings infilled (blue)") +
  facet_wrap(~year)

ggplot(d1_temp_all, aes(jday, airtemp_avg)) +
  geom_point(alpha = 0.5) +
  geom_point(aes(jday, J_tmean), col = "dodgerblue", pch=1, alpha=0.5) +
  ggtitle("TMEAN: D1 chart (black) vs. Jennings infilled (blue)") +
  facet_wrap(~year)

### ---------------------------- > Pressing on with saddle logger and D1 and C1 chart data for infilling..
# NOTE!: logger data is not QAd (according to NWT metadata)

# quick logical check of logger data (day to day variation)
colnames(sdl_loggerdat) <- gsub("CR23", "", colnames(sdl_loggerdat))
logger_check <- sdl_loggerdat %>%
  mutate(yr = year(date),
         mon = month(date),
         dy = day(date),
         doy = yday(date)) %>%
  dplyr::select(logger, date, yr, mon, dy, doy, airtemp_max, airtemp_min, airtemp_avg) %>%
  mutate(max_lag1 = lag(airtemp_max,1),
         min_lag1 = lag(airtemp_min,1),
         mean_lag1 = lag(airtemp_avg,1),
         diffmax = airtemp_max - max_lag1,
         diffmin = airtemp_min - min_lag1,
         diffmean = airtemp_avg - mean_lag1)

# how much overlap is there between logging equipment? 
group_by(logger_check, logger, yr) %>%
  summarise(nobs = length(airtemp_max)) # answer: none switched in 2012, on dec 5.


# what is sd and distribution of day to day variation in temp in logger data?
## max temp
hist(logger_check$diffmax)
summary(logger_check$diffmax)
sd(logger_check$diffmax, na.rm=T)
## min temp
boxplot(logger_check$diffmin)
summary(logger_check$diffmin)
sd(logger_check$diffmin, na.rm=T)
## mean temp
boxplot(logger_check$diffmean)
summary(logger_check$diffmean)
sd(logger_check$diffmean, na.rm=T)
# bc it's colorado, day to day swings of 20 degrees seem possible .. but check by month (is there bias in winter?)

boxplot(logger_check$diffmax ~ logger_check$mon,
        ylab = "Tmax - Tmax_lag1", xlab ="Month")
boxplot(logger_check$diffmin ~ logger_check$mon,
        ylab = "Tmin - Tmin_lag1", xlab ="Month") # 21 Feb 2017 has tmin of -187 with no flag... airtemp_avg value is reasonable that day (not the actual mean using -187)
boxplot(logger_check$diffmean ~ logger_check$mon,
        ylab = "Tmean - Tmean_lag1", xlab ="Month")

## most day to day swings in non-summer months .. is this kind of variation in Keith's data?

# day to day variation at saddle using Jennings et al. infilled data
# quick logical check of logger data (day to day variation)
jennings_check <- Jennings_summarized %>%
  ungroup() %>%
  filter(local_site == "sdl",
         `date` %in% logger_check$date) %>% # same time period as logger data
  mutate(yr = year(date),
         mon = month(date),
         dy = day(date),
         doy = yday(date)) %>%
  dplyr::select(date, yr, mon, dy, doy, J_tmax, J_tmin, J_tmean) %>%
  mutate(max_lag1 = lag(J_tmax,1),
         min_lag1 = lag(J_tmin,1),
         mean_lag1 = lag(J_tmean,1),
         diffmax = J_tmax - max_lag1,
         diffmin = J_tmin - min_lag1,
         diffmean = J_tmean - mean_lag1)

# what is sd and distribution of day to day variation in temp in logger data?
# plot with logger variation
par(mfrow=c(1,2))
## max temp
hist(jennings_check$diffmax, main = "Jennings", ylab = "Tmax - Tmax_lag1")
hist(logger_check$diffmax, main = "logger")
summary(jennings_check$diffmax)
sd(jennings_check$diffmax, na.rm=T)
## min temp
boxplot(jennings_check$diffmin, main = "Jennings", ylab = "Tmin - Tmin_lag1")
boxplot(logger_check$diffmin[logger_check$airtemp_min > -180 & logger_check$min_lag1 > -180], main = "logger") # remove outliers from feb 21 2017; comparable day-to-day variation
summary(jennings_check$diffmin)
sd(jennings_check$diffmin, na.rm=T)
## mean temp
boxplot(jennings_check$diffmean, main = "Jennings", ylab = "Tmean - Tmean_lag1")
boxplot(logger_check$diffmean, main = "logger")
summary(jennings_check$diffmean)
sd(jennings_check$diffmean, na.rm=T)
# bc it's colorado, day to day swings of 20 degrees seem possible .. but check by month (is there bias in winter?)

boxplot(jennings_check$diffmax ~ jennings_check$mon,
        ylab = "Tmax - Tmax_lag1", xlab ="Month", main = "Jennings")
boxplot(logger_check$diffmax ~ logger_check$mon,
        ylab = "Tmax - Tmax_lag1", xlab ="Month", main = "logger")
boxplot(jennings_check$diffmin ~ jennings_check$mon,
        ylab = "Tmin - Tmin_lag1", xlab ="Month", main = "Jennings")
boxplot(logger_check$diffmin[logger_check$airtemp_min > -180 & logger_check$min_lag1 > -180] ~ logger_check$mon[logger_check$airtemp_min > -180 & logger_check$min_lag1 > -180],
        ylab = "Tmin - Tmin_lag1", xlab ="Month", main = "logger")
boxplot(jennings_check$diffmean ~ jennings_check$mon,
        ylab = "Tmean - Tmean_lag1", xlab ="Month", main = "Jennings")
boxplot(logger_check$diffmean ~ logger_check$mon,
        ylab = "Tmean - Tmean_lag1", xlab ="Month", main = "logger")

# close enough .. moving on.. (but someone should QA logger dat more seriously later, mean temp is least consistent with Keith's)
# clean up environment
rm(logger_check, jennings_check, sdl_temp_all)


###############################################
### Regression infilling with saddle logger ###
###############################################
# note, 2019-06-02: decide to infill all missing values using metadata methods (for sensitivity comparison with NSF renewal dataset)

# correct unrealistic tmin value in loggerdat before moving on (-187C on 2-21-2017)
# tmax and tmean seems more reasonable those days in context with preceeding and subsequent dates, so estimate tmin from those
outlier <- which(sdl_loggerdat$airtemp_min == -187 & !is.na(sdl_loggerdat$airtemp_min))
sdl_loggerdat$airtemp_min[outlier] <- (sdl_loggerdat$airtemp_avg[outlier]*2) - sdl_loggerdat$airtemp_max[outlier]

# > years 2010 onwards...
sdl_chartlogger_2010 <- sdl_charttemp %>%
  filter(`date` > as.Date("2009-12-15")) %>%
  dplyr::rename(chartmax = airtemp_max,
                chartmin = airtemp_min,
                chartmean = airtemp_avg) %>%
  dplyr::select(-c(flag_airtemp_max, flag_airtemp_min)) %>% #no flagging done in these years
  inner_join(sdl_loggerdat) %>%
  dplyr::rename(loggermax = airtemp_max,
                loggermin = airtemp_min,
                loggermean = airtemp_avg) %>%
  dplyr::select(-c(flag_airtemp_max, flag_airtemp_min, flag_airtemp_avg)) %>% #no flagging done in these years
  mutate(mon = as.factor(month(date))) %>%
  #reorder cols
  dplyr::select(LTER_site:date, mon, chartmax:loggermean)

# which rows missing?
which(is.na(sdl_chartlogger_2010$chartmax))
which(is.na(sdl_chartlogger_2010$chartmin))
# about 10% of data missing in each tmax and tmin :(

# rule for for loop: if less than 5 days in 2 week span, skip (something with only 5 days probably won't have significant R2 anyway)
tmax_missing_dates <- sdl_chartlogger_2010$date[is.na(sdl_chartlogger_2010$chartmax)]
tmin_missing_dates <- sdl_chartlogger_2010$date[is.na(sdl_chartlogger_2010$chartmin)]


# ***** Method 1: moving window regression *******
## > 2 week before and after date missing

#initialize df for max temp
max_temp_infill_2wk <- data.frame()
# for loop to execute moving window regressions on max T
for(i in 1:length(tmax_missing_dates)){
  current_date <- tmax_missing_dates[i]
  # subset data to 2 weeks before and 2 weeks after missing date
  begin_date <- current_date - 14
  end_date <-  current_date + 14
  temp_df <- subset(sdl_chartlogger_2010,date >= begin_date & date <=end_date)
  # count complete records of chart tmax and logger tmax
  complete_obs <- nrow(temp_df[!is.na(temp_df$chartmax & temp_df$loggermax),])
  # fill in date and count of complete observations
  infill_tempdf <- data.frame(missing_date = current_date, 
                              logger = temp_df$logger[temp_df$date == current_date],
                              complete_nobs = complete_obs,
                              mon = month(current_date), year = year(current_date))
  
  ## logic check: at least 10 complete observations (both sources have tmax data on same day)
  if(complete_obs < 10) {
    #NAs for all other cols
    infill_tempdf <- cbind(infill_tempdf, 
                           tmax_logger = temp_df$loggermax[temp_df$date == current_date],
                           fit = NA, upr = NA, lwr = NA, se = NA,
                           adjr2 = NA, pval = NA, RMSE = NA, method = NA)
    next # skip to next date
  }
  
  # if passes logic check, continue with linear regression ..  
  else {
    temp_model <- lm(chartmax ~ loggermax, data=temp_df)
    temp_predict <- predict.lm(temp_model, newdata = temp_df[temp_df$date == current_date,], se.fit = T, interval = "prediction")
    infill_tempdf <- cbind(infill_tempdf,
                           tmax_logger = temp_df$loggermax[temp_df$date == current_date],
                           temp_predict$fit,
                           se = temp_predict$se.fit,
                           adjr2 = summary(temp_model)$adj.r.squared,
                           pval = summary(temp_model)$coefficients[8],
                           RMSE = summary(temp_model)$sigma,
                           method = paste(temp_df$logger[temp_df$date == current_date], "2wk lm"))
  }
  max_temp_infill_2wk <- rbind(max_temp_infill_2wk, infill_tempdf)
}

# clean up
max_temp_infill_2wk <- max_temp_infill_2wk %>%
  filter(adjr2 > 0.6 & pval <= 0.05) # keep only infilled values with r2 > 0.6 and pval <= 0.05 (per metadata methods)

# asssess:
# how many dates missing?
length(tmax_missing_dates)
# how many infilled?
nrow(max_temp_infill_2wk)
# how many remaining missing value dates have corresponding data in logger dataset?
tmax_dates_remain <- tmax_missing_dates[!tmax_missing_dates %in% max_temp_infill_2wk$missing_date]
summary(tmax_dates_remain %in% sdl_loggerdat$date[!is.na(sdl_loggerdat$airtemp_max)]) # apparently all there..


## ****** Repeat above but for minimum temperature
#initialize df for min temp
min_temp_infill_2wk <- data.frame()
# for loop to execute moving window regressions on min T
for(i in 1:length(tmin_missing_dates)){
  current_date <- tmin_missing_dates[i]
  # subset data to 2 weeks before and 2 weeks after missing date
  begin_date <- current_date - 14
  end_date <-  current_date + 14
  temp_df <- subset(sdl_chartlogger_2010,date >= begin_date & date <=end_date)
  # count complete records of chart tmin and logger tmin
  complete_obs <- nrow(temp_df[!is.na(temp_df$chartmin & temp_df$loggermin),])
  # fill in date and count of complete observations
  infill_tempdf <- data.frame(missing_date = current_date, 
                              logger = temp_df$logger[temp_df$date == current_date],
                              complete_nobs = complete_obs,
                              mon = month(current_date), year = year(current_date))
  
  ## logic check: at least 10 complete observations (both sources have tmin data on same day)
  if(complete_obs < 10) {
    #NAs for all other cols
    infill_tempdf <- cbind(infill_tempdf, 
                           tmin_logger = temp_df$loggermin[temp_df$date == current_date],
                           fit = NA, upr = NA, lwr = NA, se = NA,
                           adjr2 = NA, pval = NA, RMSE = NA, method = NA)
    next # skip to next date
  }
  
  # if passes logic check, continue with linear regression ..  
  else {
    temp_model <- lm(chartmin ~ loggermin, data=temp_df)
    temp_predict <- predict.lm(temp_model, newdata = temp_df[temp_df$date == current_date,], se.fit = T, interval = "prediction")
    infill_tempdf <- cbind(infill_tempdf,
                           tmin_logger = temp_df$loggermin[temp_df$date == current_date],
                           temp_predict$fit,
                           se = temp_predict$se.fit,
                           adjr2 = summary(temp_model)$adj.r.squared,
                           pval = summary(temp_model)$coefficients[8],
                           RMSE = summary(temp_model)$sigma,
                           method = paste(temp_df$logger[temp_df$date == current_date], "2wk lm"))
  }
  min_temp_infill_2wk <- rbind(min_temp_infill_2wk, infill_tempdf)
}

# clean up
min_temp_infill_2wk <- min_temp_infill_2wk %>%
  filter(adjr2 > 0.6 & pval <= 0.05) # keep only infilled values with r2 > 0.6 and pval <= 0.05 (per metadata methods)

# assess:
# how many dates missing?
length(tmin_missing_dates)
# how many infilled?
nrow(min_temp_infill_2wk)
# how many remaining missing value dates have corresponding data in logger dataset?
tmin_dates_remain <- tmin_missing_dates[!tmin_missing_dates %in% min_temp_infill_2wk$missing_date]
summary(tmin_dates_remain %in% sdl_loggerdat$date[!is.na(sdl_loggerdat$airtemp_min)]) # apparently all there..


# ***** Method 2: monthly lm regression *******
# linear regression of chart_temp ~ logger_temp + month; use all data points available
# notes: 
## logger dat consistently warmer than logger dat for tmax and min (esp in tmax), and diurnal ranger consistently wider in logger dat
## CR1000 begins Dec 2012-onwards, CR23X runs 2000-Dec 2012; run different regressions for each instrument
sdlcr1000 <- subset(sdl_charttemp, date %in% sdl_loggerdat$date[sdl_loggerdat$logger == "cr1000"]) # CR1000 is more recent logger
sdlcr1000 <- sdlcr1000[!grepl("flag", colnames(sdlcr1000))] # remove flag columns (no flag values, all NAs)
#prefix sdl chart airtemp with "chart_"
colnames(sdlcr1000)[grep("airtemp", names(sdlcr1000))] <- paste0("chart_", colnames(sdlcr1000)[grep("airtemp", names(sdlcr1000))])
# join logger dat
sdlcr1000 <- full_join(sdlcr1000, subset(sdl_loggerdat, logger == "cr1000")) #no flags in any of the logger data (all "n" vals)

#prefix logger data with "cr1000"
colnames(sdlcr1000)[grepl("^airtemp", colnames(sdlcr1000))] <- paste0("cr1000", colnames(sdlcr1000)[grepl("^airtemp", colnames(sdlcr1000))])

# since no flags for either chart or logger dats, and corrected suspicious value in logget dat, regress using all complete pairs available
sdlcr1000$mon <- as.factor(month(sdlcr1000$date))
summary(lm(chart_airtemp_max ~ cr1000airtemp_max, data = sdlcr1000)) #adj-R2 = 0.95, RMSE 2.068
summary(lm(chart_airtemp_max ~ cr1000airtemp_max + mon, data = sdlcr1000)) #adj-R2 = 0.9537, RMSE 1.989
summary(lm(chart_airtemp_max ~ cr1000airtemp_max + mon + year, data = sdlcr1000)) #adj-R2 = 0.9551, RMSE 1.96

# compare top models with anova
anova(lm(chart_airtemp_max ~ cr1000airtemp_max + mon + year, data = sdlcr1000),
      lm(chart_airtemp_max ~ cr1000airtemp_max + mon, data = sdlcr1000))

nulltmax <- lm(chart_airtemp_max ~ 1, data = subset(sdlcr1000, !is.na(cr1000airtemp_max)))
tmaxlm <- lm(chart_airtemp_max ~ cr1000airtemp_max + mon + year, data = sdlcr1000)
plot(tmaxlm) # 3-4 points influential.. keeping in for now
nulltmin <- lm(chart_airtemp_min ~ 1, data = subset(sdlcr1000, !is.na(cr1000airtemp_min)))
tminlm <- lm(chart_airtemp_min ~ cr1000airtemp_min + mon + year, data = sdlcr1000)
plot(tminlm)


## ***** Infill missing 2010-2017 values not filled by moving window regression + all 2018 daily tmin and tmax using CR1000 regression (sdl chart stops after 2017-12-31)
predtmax <- predict.lm(tmaxlm, newdata = subset(sdlcr1000, date %in% tmax_dates_remain | year == 2018), se.fit = T, type = "response", interval = "prediction")
predtmin <- predict.lm(tminlm, newdata = subset(sdlcr1000, date %in% tmin_dates_remain | year == 2018), se.fit = T, type = "response", interval = "prediction")

# compile tmax predictions
cr1000tmax_monthly_regress <- subset(sdlcr1000, date %in% tmax_dates_remain | year == 2018) %>%
  mutate(complete_nobs = nrow(tmaxlm$model)) %>%
  dplyr::select(date, logger, complete_nobs, mon, year, cr1000airtemp_max) %>%
  rename(missing_date = date) %>%
  cbind(data.frame(predtmax$fit,
                   se = predtmax$se.fit,
                   adjr2 = summary(tmaxlm)$adj.r.squared,
                   pval = anova(nulltmax, tmaxlm)$'Pr(>F)'[2],
                   RMSE = sigma(tmaxlm),
                   method = "cr1000 month lm")) %>%
  rename(tmax_logger = cr1000airtemp_max)

# compile tmin predictions
cr1000tmin_monthly_regress <- subset(sdlcr1000, date %in% tmin_dates_remain | year == 2018) %>%
  mutate(complete_nobs = nrow(tminlm$model)) %>%
  dplyr::select(date, logger, complete_nobs, mon, year, cr1000airtemp_min) %>%
  rename(missing_date = date) %>%
  cbind(data.frame(predtmin$fit,
                   se = predtmin$se.fit,
                   adjr2 = summary(tminlm)$adj.r.squared,
                   pval = anova(nulltmin, tminlm)$'Pr(>F)'[2],
                   RMSE = sigma(tminlm),
                   method = "cr1000 month lm")) %>%
  rename(tmin_logger = cr1000airtemp_min)


# -- repeat monthly regression process for CR23X logger -----
## CR23X runs 2000-Dec 2012
sdlcr23x <- subset(sdl_charttemp, date %in% sdl_loggerdat$date[sdl_loggerdat$logger == "cr23x"]) # cr23x is earlier logger
#prefix sdl chart airtemp vals and flgs with "chart_"
colnames(sdlcr23x)[grep("airtemp", names(sdlcr23x))] <- paste0("chart_", colnames(sdlcr23x)[grep("airtemp", names(sdlcr23x))])
summary(sdlcr23x) # flag values of 1 present but metadata does not explain what 1 means..
# how many flags?
sapply(sdlcr23x[grepl("flag", colnames(sdlcr23x))], function(x)summary(as.factor(x))) # 18 for tmax, #20 for tmin..
# join logger dat
sdlcr23x <- full_join(sdlcr23x, subset(sdl_loggerdat, logger == "cr23x")) #no flags in any of the logger data (all "n" vals)
#prefix logger data with "cr23x"
colnames(sdlcr23x)[grepl("^airtemp", colnames(sdlcr23x))] <- paste0("cr23x", colnames(sdlcr23x)[grepl("^airtemp", colnames(sdlcr23x))])

# since no flags for either chart or logger dats, and corrected suspicious value in logget dat, regress using all complete pairs available
sdlcr23x$mon <- as.factor(month(sdlcr23x$date))
summary(lm(chart_airtemp_max ~ cr23xairtemp_max, data = sdlcr23x)) #adj-R2 = 0.9722, RMSE 1.573
summary(lm(chart_airtemp_max ~ cr23xairtemp_max + mon, data = sdlcr23x)) #adj-R2 = 0.973, RMSE 1.551
summary(lm(chart_airtemp_max ~ cr23xairtemp_max + mon + year, data = sdlcr23x)) #adj-R2 = 0.973, RMSE 1.551
#summary(lm(chart_airtemp_max ~ cr23xairtemp_max + mon + year + logger, data = sdlcr23x)) #adj-R2 = 0.9667, RMSE 1.687

# compare top models with anova
anova(lm(chart_airtemp_max ~ cr23xairtemp_max + mon, data = sdlcr23x),
      lm(chart_airtemp_max ~ cr23xairtemp_max + mon + year, data = sdlcr23x)) # marginal improvement with year included.. will keep to be consistent with CR1000 infill methods

cr23x_nulltmax <- lm(chart_airtemp_max ~ 1, data = subset(sdlcr23x, !is.na(cr23xairtemp_max)))
cr23x_tmaxlm <- lm(chart_airtemp_max ~ cr23xairtemp_max + mon + year, data = sdlcr23x)
plot(cr23x_tmaxlm) # 3-4 points influential.. keeping in for now
cr23x_nulltmin <- lm(chart_airtemp_min ~ 1, data = subset(sdlcr23x, !is.na(cr23xairtemp_min)))
cr23x_tminlm <- lm(chart_airtemp_min ~ cr23xairtemp_min + mon + year, data = sdlcr23x)
plot(cr23x_tminlm)


## ***** Infill missing 2010-2017 values not filled by moving window regression + all 2018 daily tmin and tmax using cr23x regression (sdl chart stops after 2017-12-31)
cr23x_predtmax <- predict.lm(cr23x_tmaxlm, newdata = subset(sdlcr23x, date %in% tmax_dates_remain), se.fit = T, type = "response", interval = "prediction")
cr23x_predtmin <- predict.lm(cr23x_tminlm, newdata = subset(sdlcr23x, date %in% tmin_dates_remain), se.fit = T, type = "response", interval = "prediction")

# compile tmax predictions
cr23xtmax_monthly_regress <- subset(sdlcr23x, date %in% tmax_dates_remain) %>%
  mutate(complete_nobs = nrow(cr23x_tmaxlm$model)) %>%
  dplyr::select(date, logger, complete_nobs, mon, year, cr23xairtemp_max) %>%
  rename(missing_date = date) %>%
  cbind(data.frame(cr23x_predtmax$fit,
                   se = cr23x_predtmax$se.fit,
                   adjr2 = summary(cr23x_tmaxlm)$adj.r.squared,
                   pval = anova(cr23x_nulltmax, cr23x_tmaxlm)$'Pr(>F)'[2],
                   RMSE = sigma(cr23x_tmaxlm),
                   method = "cr23x month lm")) %>%
  rename(tmax_logger = cr23xairtemp_max)

# compile tmin predictions
cr23xtmin_monthly_regress <- subset(sdlcr23x, date %in% tmin_dates_remain) %>%
  mutate(complete_nobs = nrow(cr23x_tminlm$model)) %>%
  dplyr::select(date, logger, complete_nobs, mon, year, cr23xairtemp_min) %>%
  rename(missing_date = date) %>%
  cbind(data.frame(cr23x_predtmin$fit,
                   se = cr23x_predtmin$se.fit,
                   adjr2 = summary(cr23x_tminlm)$adj.r.squared,
                   pval = anova(cr23x_nulltmin, cr23x_tminlm)$'Pr(>F)'[2],
                   RMSE = sigma(cr23x_tminlm),
                   method = "cr23x month lm")) %>%
  rename(tmin_logger = cr23xairtemp_min)





# visualize estimates and error with available saddle chart data
ggplot(cr1000tmax_monthly_regress, aes(missing_date, fit)) +
  geom_errorbar(aes(ymax = upr, ymin = lwr), col = "dodgerblue") +
  geom_errorbar(data = max_temp_infill_2wk, aes(ymax = upr, ymin = lwr), col = "goldenrod") +
  geom_errorbar(data = cr23xtmax_monthly_regress, aes(ymax = upr, ymin = lwr), col = "orchid") +
  geom_point(col = "dodgerblue4") +
  geom_point(data = cr23xtmax_monthly_regress, aes(missing_date, fit), col = "purple") +
  geom_point(data = max_temp_infill_2wk, aes(missing_date, fit), col = "chocolate4") +
  geom_point(data = subset(sdl_charttemp, date > "2009-12-31" & !is.na(airtemp_max)), aes(date, airtemp_max), pch = 1)
boxplot(tmax_monthly_regress$se)

ggplot(cr1000tmin_monthly_regress, aes(missing_date, fit)) +
  geom_errorbar(aes(ymax = upr, ymin = lwr), col = "dodgerblue") +
  geom_errorbar(data = min_temp_infill_2wk, aes(ymax = upr, ymin = lwr), col = "goldenrod") +
  geom_errorbar(data = cr23xtmin_monthly_regress, aes(ymax = upr, ymin = lwr), col = "orchid") +
  geom_point(col = "dodgerblue4") +
  geom_point(data = cr23xtmin_monthly_regress, aes(missing_date, fit), col = "purple") +
  geom_point(data = min_temp_infill_2wk, aes(missing_date, fit), col = "chocolate4") +
  geom_point(data = subset(sdl_charttemp, date > "2009-12-31" & !is.na(airtemp_min)), aes(date, airtemp_min), pch = 1)
boxplot(tmin_monthly_regress$se)

# note: low value is 2011 is real. checked manually against saddle logger data, and compared day before and day after, along with historic lows in saddle chart data

# compile all estimated results and write out for reference
predicted_tmax_all <- rbind(cr1000tmax_monthly_regress, cr23xtmax_monthly_regress, max_temp_infill_2wk) %>%
  arrange(missing_date)
predicted_tmin_all <- rbind(cr1000tmin_monthly_regress, cr23xtmin_monthly_regress, min_temp_infill_2wk) %>%
  arrange(missing_date)

write.csv(predicted_tmax_all, "extended_summer/output_data/ctw/predicted_tmax_all.csv", row.names = F)
write.csv(predicted_tmin_all, "extended_summer/output_data/ctw/predicted_tmin_all.csv", row.names = F)

#######################################################
### Compile complete infilled dataset, 1982-current ##
#######################################################
# write out both nsf data + metadata-methods infilled 2015-2017 and sdl chart + metatdata-methods only datast (to compare with NSF pca)

# nsf (1981-2014) + metadata-methods infilled (2015-current)
nsf_2015current <- NSF_temp %>%
  rename(airtemp_min = TMIN,
         airtemp_max = TMAX) %>%
  mutate(tmin_method = "NSF renewal data",
         tmax_method = "NSF renewal data")  %>%
  rbind(data.frame(subset(sdl_charttemp[year(sdl_charttemp$date) > 2014, 
                      c("LTER_site", "local_site", "date", "airtemp_min", "airtemp_max")]),
                   tmin_method = "sdl chart data",
                   tmax_method = "sdl chart data")) %>%
  full_join(subset(predicted_tmax_all, year(missing_date) > 2014), by = c("date" = "missing_date")) %>%
  # if airtemp_max NA, replace methods predicted methods, if not NA do nothing
  mutate(tmax_method = ifelse(is.na(airtemp_max), method, tmax_method),
         # replace NAs in airtemp_max with predicted values, if not NA do nothing
         airtemp_max = ifelse(is.na(airtemp_max), fit, airtemp_max)) %>%
  # also need to infill site info for 2018 since no 2018 dates present in sdl chart dataset
  mutate(LTER_site = unique(sdl_charttemp$LTER_site),
         local_site = unique(sdl_charttemp$local_site)) %>%
  # rename intervals to reflect pertain to tmax estimates
  rename(tmax_se = se,
         tmax_prupr = upr,
         tmax_prlwr = lwr) %>%
  # clean up cols
  dplyr::select(LTER_site:tmax_method, tmax_prlwr:tmax_se) %>%
  # join predicted tmin data
  left_join(subset(predicted_tmin_all, year(missing_date) > 2014),  by = c("date" = "missing_date")) %>%
  # if airtemp_max NA, replace methods predicted methods, if not NA do nothing
  mutate(tmin_method = ifelse(is.na(airtemp_min), method, tmin_method),
         # replace NAs in airtemp_max with predicted values, if not NA do nothing
         airtemp_min = ifelse(is.na(airtemp_min), fit, airtemp_min)) %>%
  rename(tmin_se = se,
         tmin_prupr = upr,
         tmin_prlwr = lwr) %>%
  # clean up cols
  dplyr::select(LTER_site:tmax_method, tmax_prlwr:tmax_se, tmin_prlwr:tmin_se) %>%
  arrange(date)
  
# check for NAs in tmin and tmax
summary(nsf_2015current[c("airtemp_max", "airtemp_min")]) # no NAs!
summary(duplicated(nsf_2015current$date)) # no duplicate dates

# write out
write.csv(nsf_2015current, "extended_summer/output_data/suding/allyrs/sdl_temp_infilled_19822018_nsfctw.csv", row.names = F)

# compile metadata methods-infilled only dataset
sdl_ctwinfill <- data.frame(dplyr::select(sdl_charttemp, LTER_site:date, airtemp_min, airtemp_max),
                   tmin_method = "sdl chart data",
                   tmax_method = "sdl chart data") %>%
  full_join(subset(predicted_tmax_all, year(missing_date) > 2008), by = c("date" = "missing_date")) %>%
  # if airtemp_max NA, replace methods predicted methods, if not NA do nothing
  mutate(tmax_method = ifelse(is.na(airtemp_max), method, tmax_method),
         # replace NAs in airtemp_max with predicted values, if not NA do nothing
         airtemp_max = ifelse(is.na(airtemp_max), fit, airtemp_max)) %>%
  # also need to infill site info for 2018 since no 2018 dates present in sdl chart dataset
  mutate(LTER_site = unique(sdl_charttemp$LTER_site),
         local_site = unique(sdl_charttemp$local_site)) %>%
  # rename intervals to reflect pertain to tmax estimates
  rename(tmax_se = se,
         tmax_prupr = upr,
         tmax_prlwr = lwr) %>%
  # clean up cols
  dplyr::select(LTER_site:tmax_method, tmax_prlwr:tmax_se) %>%
  # join predicted tmin data
  left_join(subset(predicted_tmin_all, year(missing_date) > 2008),  by = c("date" = "missing_date")) %>%
  # if airtemp_max NA, replace methods predicted methods, if not NA do nothing
  mutate(tmin_method = ifelse(is.na(airtemp_min), method, tmin_method),
         # replace NAs in airtemp_max with predicted values, if not NA do nothing
         airtemp_min = ifelse(is.na(airtemp_min), fit, airtemp_min)) %>%
  rename(tmin_se = se,
         tmin_prupr = upr,
         tmin_prlwr = lwr) %>%
  # clean up cols
  dplyr::select(LTER_site:tmax_method, tmax_prlwr:tmax_se, tmin_prlwr:tmin_se) %>%
  arrange(date)

# check for NAs in tmin and tmax
summary(sdl_ctwinfill[c("airtemp_max", "airtemp_min")]) # no NAs!
summary(duplicated(sdl_ctwinfill$date)) # no duplicate dates

# write out
write.csv(sdl_ctwinfill, "extended_summer/output_data/ctw/sdl_temp_infilled_19822018_ctwonly.csv", row.names = F)
  


# -- VISUALIZE FINAL DATASETS ------
# nsf renewal data + metadata-methods infilling 2015-current 
ggplot(nsf_2015current, aes(date, airtemp_max, col = tmax_method)) +
  geom_point()
ggplot(nsf_2015current, aes(date, airtemp_min, col = tmin_method)) +
  geom_point()
# sdl chart data + metadata-methods infilling 2010-current (no NAs in sdl chart dataset 2008 or 2009 even tho methods says infilling stopped after 2008)
ggplot(sdl_ctwinfill, aes(date, airtemp_max, col = tmax_method)) +
  geom_point()
ggplot(sdl_ctwinfill, aes(date, airtemp_min, col = tmin_method)) +
  geom_point()
# overlay both datasets with jennings et al. infilled
#tmin comparison
ggplot(Jennings_summarized, aes(date, J_tmin)) +
  geom_point(pch = 1, alpha = 0.5) +
  geom_point(data = subset(nsf_2015current, date %in% Jennings_summarized$date),
             aes(date,airtemp_min), col = "orchid", pch = 1, alpha = 0.6) +
  geom_point(data = subset(sdl_ctwinfill, date %in% Jennings_summarized$date),
             aes(date,airtemp_min), col = "dodgerblue2", alpha = 0.3)
# tmax comparison
ggplot(Jennings_summarized, aes(date, J_tmax)) +
  geom_point() +
  geom_point(data = subset(nsf_2015current, date %in% Jennings_summarized$date),
             aes(date,airtemp_max), col = "orchid", pch = 1, alpha = 0.6) +
  geom_point(data = subset(sdl_ctwinfill, date %in% Jennings_summarized$date),
             aes(date,airtemp_max), col = "dodgerblue2", alpha = 0.3)


























## OLD CODE BELOW (used for ASM 2019 poster) -- DON'T USE BUT DON'T DELETE -----
# 
# 
# 
# ############################################
# ### Regression infilling using D1 ##
# ###########################################
# 
# # tidy, make long form
# sdl_charttemp_long <- sdl_charttemp %>%
#   dplyr::select(-c(flag_airtemp_max, flag_airtemp_min)) %>% # flagging only indicates infill method used up to 2008
#   gather(metric, value, airtemp_max:airtemp_avg)
# 
# d1_charttemp_long <- d1_charttemp %>%
#   dplyr::select(-c(flag_airtemp_max, flag_airtemp_min)) %>% # flagging only indicates infill method used up to 2008
#   gather(metric, value, airtemp_max:airtemp_avg)
# 
# sdl_d1_chartlong <- rbind(sdl_charttemp_long, d1_charttemp_long) %>%
#   filter(year(`date`) > 1981) %>% # sdl chart starts July 9, 1981
#   mutate(metric = gsub("airtemp_", "t", metric)) %>% # lazy typing
#   spread(local_site, value) %>%
#   mutate(yr = year(`date`),
#          mon = month(`date`)) # for monthly regressions
# 
# # which rows missing? [will show na for tmax, tmin, and tmean]
# which(is.na(sdl_d1_chartlong$sdl))
# 
# # create vector of dates missing for all temp types (will discard any infilled value don't need later)
# temp_missing_1517 <- sdl_d1_chartlong$date[is.na(sdl_d1_chartlong$sdl) & sdl_d1_chartlong$yr > 2014] %>%
#   unique()
# 
# # ***** Method 1: moving window regression *******
# ## > 2 week before and after date missing
# # rule for for loop: if less than 5 days in 2 week span, skip (something with only 5 days probably won't have significant R2 anyway)
# 
# #initialize df for max temp
# sdl_temp_infill_d1 <- data.frame(missing_date= NA, complete_nobs = NA, temp_type=NA, intercept = NA, 
#                                  slope = NA, d1_value = NA, infill_value = NA, r2 = NA, pval = NA )
# 
# # for loop to execute moving window regressions on max T
# for(t in c("tmax", "tmin")){
#   subset_df <- subset(sdl_d1_chartlong, metric == t)
#   missing_dates <- subset_df$date[is.na(subset_df$sdl) & subset_df$yr > 2014]
#   for(i in missing_dates){
#     # subset data to 2 weeks before and 2 weeks after missing date
#     begin_date <- i - 14
#     end_date <-  i + 14
#     temp_df <- subset(subset_df,date >= begin_date & date <=end_date)
#     # count complete records of chart tmax and logger tmax
#     complete_obs <- nrow(temp_df[!is.na(temp_df$sdl & temp_df$d1),])
#     
#     ## logic check: at least 10 complete observations (both sources have tmax data on same day)
#     if(complete_obs < 10) {
#       null_row <- data.frame(i,complete_obs, t, NA, NA, NA, NA, NA, NA)
#       colnames(null_row)[1:9] <- colnames(sdl_temp_infill_d1)
#       sdl_temp_infill_d1 <- rbind(sdl_temp_infill_d1, null_row)
#     }
#     
#     # if passes logic check, continue with linear regression ..  
#     else {
#       temp_model <- lm(sdl ~ d1, data=temp_df)
#       temp_x <- temp_df$d1[temp_df$date == i]
#       
#       infill_row <- data.frame(
#         missing_date = i,
#         complete_nobs = complete_obs,
#         temp_type = t,
#         intercept = temp_model$coefficients[[1]],
#         slope = temp_model$coefficients[[2]],
#         d1_value = temp_x,
#         infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
#         r2 = summary(temp_model)$r.squared,
#         pval = summary(temp_model)$coefficients[8]
#       )
#       # add to infill data frame
#       sdl_temp_infill_d1 <- rbind(sdl_temp_infill_d1,infill_row) 
#     }
#   }
# }
# 
# # clean up
# # add dates
# temp_missing_1517 <- data.frame(`date` = temp_missing_1517)
# temp_missing_1517$missing_date <- as.numeric(temp_missing_1517$date)
# 
# sdl_temp_infill_d1_max_2wk <- sdl_temp_infill_d1 %>%
#   left_join(temp_missing_1517) %>%
#   dplyr::select(`date`,complete_nobs:pval) %>% #drop date as number field
#   mutate(infill_value = round(infill_value,2)) %>%
#   # drop NAs and r2 <=0.6
#   filter(!is.na(infill_value) &
#            r2 >0.6 &
#            temp_type == "tmax")
# 
# sdl_temp_infill_d1_min_2wk <- sdl_temp_infill_d1 %>%
#   left_join(temp_missing_1517) %>%
#   dplyr::select(`date`,complete_nobs:pval) %>% #drop date as number field
#   mutate(infill_value = round(infill_value,2)) %>%
#   # drop NAs and r2 <=0.6
#   filter(!is.na(infill_value) &
#            r2 >0.6 &
#            temp_type == "tmin")
# 
# # compare infill values
# max_temp_infill_2wk %>%
#   dplyr::rename(`date` = tmax_missing_dates) %>%
#   full_join(sdl_temp_infill_d1_max_2wk, by = "date") %>%
#   ggplot() +
#   #geom_point(data = sdl_d1_1517, aes(date, sdlmax), col = "grey50") +
#   geom_point(aes(date, tmax_infill), col = "orchid", alpha = 0.5) +
#   geom_point(aes(date, infill_value), col = "navy", alpha = 0.5) +
#   theme_bw() #looks plausible
# 
# min_temp_infill_2wk %>%
#   dplyr::rename(`date` = tmin_missing_dates) %>%
#   full_join(sdl_temp_infill_d1_min_2wk[sdl_temp_infill_d1_min_2wk$temp_type == "tmin",], by = "date") %>%
#   ggplot() +
#   #geom_point(data = sdl_d1_1517, aes(date, sdlmin), col = "grey50") +
#   geom_point(aes(date, tmin_infill), col = "orchid", alpha = 0.5) +
#   geom_point(aes(date, infill_value), col = "navy", alpha = 0.5) +
#   theme_bw() # still a lot of missing values..
# 
# # merge infill values with chart data, see how many missing left
# sdl_temp_1517_final <- sdl_charttemp %>%
#   filter(year(`date`) > 2014) %>%
#   dplyr::select(-c(flag_airtemp_max, flag_airtemp_min)) %>%
#   left_join(sdl_temp_infill_d1_max_2wk[c("date", "infill_value", "r2")], by = "date") %>%
#   dplyr::rename(d1_tmax_infill = infill_value,
#                 d1_tmax_r2 = r2) %>%
#   left_join(sdl_temp_infill_d1_min_2wk[c("date", "infill_value", "r2")], by = "date") %>%
#   dplyr::rename(d1_tmin_infill = infill_value,
#                 d1_tmin_r2 = r2) %>%
#   left_join(max_temp_infill_2wk[c("tmax_missing_dates", "tmax_infill", "r2")], by = c("date" = "tmax_missing_dates")) %>%
#   dplyr::rename(log_tmax_infill = tmax_infill,
#                 log_tmax_r2 = r2) %>%
#   left_join(min_temp_infill_2wk[c("tmin_missing_dates", "tmin_infill", "r2")], by = c("date" = "tmin_missing_dates")) %>%
#   dplyr::rename(log_tmin_infill = tmin_infill,
#                 log_tmin_r2 = r2) %>%
#   mutate(tmax_infill = ifelse(!is.na(airtemp_max), airtemp_max, log_tmax_infill),
#          tmax_source = ifelse(!is.na(airtemp_max), "sdl chart", 
#                               ifelse(!is.na(log_tmax_infill), "2wk logger",
#                                      ifelse(!is.na(d1_tmax_infill), "2wk d1", NA))),
#          tmax_infill = ifelse(!is.na(tmax_infill), tmax_infill, d1_tmax_infill),
#          tmin_infill = ifelse(!is.na(airtemp_min), airtemp_min, log_tmin_infill),
#          tmin_source = ifelse(!is.na(airtemp_min), "sdl chart", 
#                               ifelse(!is.na(log_tmin_infill), "2wk logger",
#                                      ifelse(!is.na(d1_tmin_infill), "2wk d1", NA))),
#          tmin_infill = ifelse(!is.na(tmin_infill), tmin_infill, d1_tmin_infill),
#          tmax_infill = round(tmax_infill, 2),
#          tmin_infill = round(tmin_infill, 2)) %>%
#   dplyr::select(LTER_site, local_site, date, tmax_infill, tmax_source, tmin_infill, tmin_source)
# 
# 
# ## ****************
# #### clean up global environment before proceed to monthly regression infilling 
# rm(d1_temp_all, infill_row, jennings_check, Jennings_infill, logger_check,
#    max_temp_infill, max_temp_infill_2wk, min_temp_infill, min_temp_infill_2wk,
#    null_row, sdl_chartlogger_2010, sdl_temp_all, sdl_temp_infill_d1, sdl_temp_infill_d1_2wk,
#    sdl_temp_infill_d1_max_2wk, sdl_temp_infill_d1_min_2wk, subset_df, temp_df, temp_missing_1517,
#    temp_model, test)
# rm(tmax_missing_dates, tmin_missing_dates, missing_dates, 
#    begin_date, end_date, complete_obs, i, t)
# 
# 
# ###############################
# ### ------------------------- > proceed to infill remaining missing through monthly regressions
# 
# tmax_missing <- sdl_temp_1517_final$date[is.na(sdl_temp_1517_final$tmax_infill)]
# tmin_missing <- sdl_temp_1517_final$date[is.na(sdl_temp_1517_final$tmin_infill)]
# 
# # rename colnames in infilled dataset to match NSF_temp names
# sdl_temp_1517_final <- sdl_temp_1517_final %>%
#   dplyr::rename(TMIN = tmin_infill,
#                 TMAX = tmax_infill)
# 
# ## monthly regressions for 2015-2016 using logger data
# sdl_logger_all <- NSF_temp %>%
#   rbind(sdl_temp_1517_final[c("LTER_site", "local_site", "date", "TMIN", "TMAX")]) %>%
#   inner_join(sdl_loggerdat) %>%
#   mutate(mon = month(`date`)) %>%
#   dplyr::rename(loggermax = airtemp_max,
#                 loggermin = airtemp_min) %>%
#   # remove unneeded columns so easier to look at
#   dplyr::select(-c(flag_airtemp_max, flag_airtemp_min, flag_airtemp_avg, jday))
# 
# # initialize df for tmax
# tmax_monthly_regress <- data.frame()
# 
# # for loop...
# # have to write the first part wonky bc R converts my dates to numbers..
# for(missing in as.character(tmax_missing[year(tmax_missing)<2017])){
#   i <- as.Date(missing)
#   temp_df <- subset(sdl_logger_all,mon == month(i))
#   
#   # regress all logger obs from month on all sdl obs (will automatically exclude incomplete pairs)
#   temp_model <- lm(TMAX ~ loggermax, data=temp_df)
#   
#   temp_x <- temp_df$loggermax[temp_df$date == i]
#   
#   infill_row <- data.frame(
#     missing_date = i,
#     complete_nobs = nrow(temp_model$model),
#     intercept = temp_model$coefficients[[1]],
#     slope = temp_model$coefficients[[2]],
#     logger_value = temp_x,
#     infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
#     r2 = summary(temp_model)$r.squared,
#     pval = summary(temp_model)$coefficients[8] 
#   )
#   tmax_monthly_regress <- rbind(tmax_monthly_regress, infill_row)
# }
# 
# 
# ## **** repeat for tmin
# 
# # initialize df for tmin
# tmin_monthly_regress <- data.frame()
# 
# # for loop...
# # have to write the first part wonky bc R converts my dates to numbers..
# for(missing in as.character(tmin_missing[year(tmin_missing)<2017])){
#   i <- as.Date(missing)
#   temp_df <- subset(sdl_logger_all,mon == month(i))
#   
#   # regress all logger obs from month on all sdl obs (will automatically exclude incomplete pairs)
#   temp_model <- lm(TMIN ~ loggermin, data=temp_df)
#   
#   temp_x <- temp_df$loggermin[temp_df$date == i]
#   
#   infill_row <- data.frame(
#     missing_date = i,
#     complete_nobs = nrow(temp_model$model),
#     intercept = temp_model$coefficients[[1]],
#     slope = temp_model$coefficients[[2]],
#     logger_value = temp_x,
#     infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
#     r2 = summary(temp_model)$r.squared,
#     pval = summary(temp_model)$coefficients[8] 
#   )
#   tmin_monthly_regress <- rbind(tmin_monthly_regress, infill_row)
# }
# 
# 
# 
# ###############################
# ### ****** now infill for 2017 with D1 data ...
# ## monthly regressions for 2017 using D1 chart data
# 
# # merge sdl infilled data and d1 chart data
# # Jennings et al. D1 infilled data is cooler in temp than d1 chart so sticking with raw chart so any pre-2015 bias consistent with 2015-2017 
# sdl_d1_all <- NSF_temp %>%
#   rbind(sdl_temp_1517_final[c("LTER_site", "local_site", "date", "TMIN", "TMAX")]) %>%
#   inner_join(d1_charttemp, by = c("LTER_site", "date")) %>%
#   mutate(mon = month(`date`)) %>%
#   dplyr::rename(d1max = airtemp_max,
#                 d1min = airtemp_min) %>%
#   # remove unneeded columns so easier to look at
#   dplyr::select(-c(flag_airtemp_max, flag_airtemp_min)) 
# 
# 
# # initialize df for tmax
# tmax_monthly_regress_d1 <- data.frame()
# 
# # for loop...
# # have to write the first part wonky bc R converts my dates to numbers..
# for(missing in as.character(tmax_missing)){
#   i <- as.Date(missing)
#   temp_df <- subset(sdl_d1_all,mon == month(i))
#   
#   # regress all logger obs from month on all sdl obs (will automatically exclude incomplete pairs)
#   temp_model <- lm(TMAX ~ d1max, data=temp_df)
#   
#   temp_x <- temp_df$d1max[temp_df$date == i]
#   
#   infill_row <- data.frame(
#     missing_date = i,
#     complete_nobs = nrow(temp_model$model),
#     intercept = temp_model$coefficients[[1]],
#     slope = temp_model$coefficients[[2]],
#     d1_value = temp_x,
#     infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
#     r2 = summary(temp_model)$r.squared,
#     pval = summary(temp_model)$coefficients[8] 
#   )
#   tmax_monthly_regress_d1 <- rbind(tmax_monthly_regress_d1, infill_row)
# }
# 
# 
# ## **** repeat for tmin
# 
# # initialize df for tmin
# tmin_monthly_regress_d1 <- data.frame()
# 
# # for loop...
# # have to write the first part wonky bc R converts my dates to numbers..
# for(missing in as.character(tmin_missing)){
#   i <- as.Date(missing)
#   temp_df <- subset(sdl_d1_all,mon == month(i))
#   
#   # regress all logger obs from month on all sdl obs (will automatically exclude incomplete pairs)
#   temp_model <- lm(TMIN ~ d1min, data=temp_df)
#   
#   temp_x <- temp_df$d1min[temp_df$date == i]
#   
#   infill_row <- data.frame(
#     missing_date = i,
#     complete_nobs = nrow(temp_model$model),
#     intercept = temp_model$coefficients[[1]],
#     slope = temp_model$coefficients[[2]],
#     d1_value = temp_x,
#     infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
#     r2 = summary(temp_model)$r.squared,
#     pval = summary(temp_model)$coefficients[8] 
#   )
#   tmin_monthly_regress_d1 <- rbind(tmin_monthly_regress_d1, infill_row)
# }
# 
# 
# ###############################
# ### ****** now infill for April 2017 with C1 data ...
# ## D1 chart data not available those dates for tmin or tmax
# 
# # merge sdl infilled data and d1 chart data
# # Jennings et al. D1 infilled data is cooler in temp than d1 chart so sticking with raw chart so any pre-2015 bias consistent with 2015-2017 
# sdl_c1_all <- NSF_temp %>%
#   rbind(sdl_temp_1517_final[c("LTER_site", "local_site", "date", "TMIN", "TMAX")]) %>%
#   inner_join(c1_charttemp, by = c("LTER_site", "date")) %>%
#   mutate(mon = month(`date`)) %>%
#   dplyr::rename(c1max = airtemp_max,
#                 c1min = airtemp_min) %>%
#   # remove unneeded columns so easier to look at
#   dplyr::select(-c(flag_airtemp_max, flag_airtemp_min)) 
# 
# 
# # initialize df for tmax
# tmax_monthly_regress_c1 <- data.frame()
# 
# # for loop...
# # have to write the first part wonky bc R converts my dates to numbers..
# for(missing in as.character(tmax_missing)){
#   i <- as.Date(missing)
#   temp_df <- subset(sdl_c1_all,mon == month(i))
#   
#   # regress all logger obs from month on all sdl obs (will automatically exclude incomplete pairs)
#   temp_model <- lm(TMAX ~ c1max, data=temp_df)
#   
#   temp_x <- temp_df$c1max[temp_df$date == i]
#   
#   infill_row <- data.frame(
#     missing_date = i,
#     complete_nobs = nrow(temp_model$model),
#     intercept = temp_model$coefficients[[1]],
#     slope = temp_model$coefficients[[2]],
#     c1_value = temp_x,
#     infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
#     r2 = summary(temp_model)$r.squared,
#     pval = summary(temp_model)$coefficients[8] 
#   )
#   tmax_monthly_regress_c1 <- rbind(tmax_monthly_regress_c1, infill_row)
# }
# 
# 
# ## **** repeat for tmin
# 
# # initialize df for tmin
# tmin_monthly_regress_c1 <- data.frame()
# 
# # for loop...
# # have to write the first part wonky bc R converts my dates to numbers..
# for(missing in as.character(tmin_missing)){
#   i <- as.Date(missing)
#   temp_df <- subset(sdl_c1_all,mon == month(i))
#   
#   # regress all logger obs from month on all sdl obs (will automatically exclude incomplete pairs)
#   temp_model <- lm(TMIN ~ c1min, data=temp_df)
#   
#   temp_x <- temp_df$c1min[temp_df$date == i]
#   
#   infill_row <- data.frame(
#     missing_date = i,
#     complete_nobs = nrow(temp_model$model),
#     intercept = temp_model$coefficients[[1]],
#     slope = temp_model$coefficients[[2]],
#     c1_value = temp_x,
#     infill_value = temp_model$coefficients[[1]] + (temp_model$coefficients[[2]] * temp_x),
#     r2 = summary(temp_model)$r.squared,
#     pval = summary(temp_model)$coefficients[8] 
#   )
#   tmin_monthly_regress_c1 <- rbind(tmin_monthly_regress_c1, infill_row)
# }
# 
# 
# ########## compare values (just to see, will always defer to d1 infill if available)
# # tmax infill
# ggplot() +
#   geom_point(data=tmax_monthly_regress_d1, aes(missing_date, infill_value),
#              size = tmax_monthly_regress_d1$r2,
#              col = "blue", alpha = 0.5) +
#   geom_point(data=tmax_monthly_regress_c1, aes(missing_date, infill_value), 
#              size = tmax_monthly_regress_c1$r2,
#              col = "darkgreen", alpha = 0.5) +
#   theme_bw()
# 
# ggplot() +
#   geom_point(data=tmin_monthly_regress_d1, aes(missing_date, infill_value),
#              size = tmin_monthly_regress_d1$r2,
#              col = "blue", alpha = 0.5) +
#   geom_point(data=tmin_monthly_regress_c1, aes(missing_date, infill_value), 
#              size = tmin_monthly_regress_c1$r2,
#              col = "darkgreen", alpha = 0.5) +
#   theme_bw()
# 
# 
# unique(tmax_monthly_regress_d1$r2)
# unique(tmax_monthly_regress_c1$r2)
# 
# unique(tmin_monthly_regress_d1$r2)
# unique(tmin_monthly_regress_c1$r2)
# 
# ## create final infilled dataset
# # NOTES: all d1 monthly infilled data have r2 > 0.6 except for July 2017 tmin infill (r2 = 0.55)
# # 
# 
# d1_monthly_infill_max <- tmax_monthly_regress_d1 %>%
#   dplyr::select(missing_date, infill_value, r2) %>%
#   mutate(type = "TMAX",
#          source = "d1 monthly lm")
# 
# d1_monthly_infill_min <- tmin_monthly_regress_d1 %>%
#   dplyr::select(missing_date, infill_value, r2) %>%
#   mutate(type = "TMIN",
#          source = "d1 monthly lm")
# 
# c1_monthly_infill_max <- tmax_monthly_regress_c1 %>%
#   filter(missing_date %in% d1_monthly_infill_max$missing_date[is.na(d1_monthly_infill_max$infill_value)]) %>%
#   dplyr::select(missing_date, infill_value, r2) %>%
#   mutate(type = "TMAX",
#          source = "c1 monthly lm")
# 
# c1_monthly_infill_min <- tmin_monthly_regress_c1 %>%
#   filter(missing_date %in% d1_monthly_infill_min$missing_date[is.na(d1_monthly_infill_min$infill_value)]) %>%
#   dplyr::select(missing_date, infill_value, r2) %>%
#   mutate(type = "TMIN",
#          source = "c1 monthly lm")
# 
# monthly_infill_all <- rbind(d1_monthly_infill_max, d1_monthly_infill_min,
#                             c1_monthly_infill_max, c1_monthly_infill_min) %>%
#   dplyr::arrange(`type`, missing_date)
# 
# #write.csv(monthly_infill_all, "/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/sdl_infill_monthlylm_2015-2017.csv")
# 
# monthly_tmax <- monthly_infill_all %>%
#   filter(type == "TMAX") %>%
#   mutate(LTER_site = "NWT",
#          local_site = "sdl") %>%  
#   dplyr::rename(`date` = missing_date,
#                 mo_TMAX = infill_value,
#                 mo_tmax_source = source) %>%
#   dplyr::select(LTER_site, local_site, `date`, mo_TMAX, mo_tmax_source) %>%
#   na.omit()
# 
# monthly_tmin <- monthly_infill_all %>%
#   filter(type == "TMIN") %>%
#   mutate(LTER_site = "NWT",
#          local_site = "sdl") %>%  
#   dplyr::rename(`date` = missing_date,
#                 mo_TMIN = infill_value,
#                 mo_tmin_source = source) %>%
#   dplyr::select(LTER_site, local_site, `date`, mo_TMIN, mo_tmin_source) %>%
#   na.omit()
# 
# 
# ########### FINALIZE TEMP DATA (finally!) #######
# sdl_infilled_2wk_monthly_1517 <- sdl_temp_1517_final %>%
#   left_join(monthly_tmax) %>%
#   left_join(monthly_tmin) %>%
#   mutate(airtemp_max = ifelse(!is.na(TMAX), TMAX, mo_TMAX),
#          tmax_source_cw =ifelse(!is.na(TMAX), tmax_source, mo_tmax_source),
#          airtemp_min = ifelse(!is.na(TMIN), TMIN, mo_TMIN),
#          tmin_source_cw =ifelse(!is.na(TMIN), tmin_source, mo_tmin_source)) %>%
#   dplyr::select(LTER_site, local_site, date, airtemp_max, tmax_source_cw,
#                 airtemp_min, tmin_source_cw)
# 
# sdl_infilled_2wk_monthly_1517$airtemp_mean <- with(sdl_infilled_2wk_monthly_1517, round(((airtemp_max + airtemp_min)/2),2))
# 
# #write.csv(sdl_infilled_2wk_monthly_1517, 
# #          "/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/sdl_temp_infilled_20152017_ctw.csv")
# 
# # plot all to see
# 
# temp_long <- sdl_infilled_2wk_monthly_1517 %>%
#   gather(metric, value, airtemp_max, airtemp_min, airtemp_mean) %>%
#   mutate(source = ifelse(metric == "airtemp_max", tmax_source_cw,
#                          ifelse(metric == "airtemp_min", tmin_source_cw, 
#                                 ifelse(tmax_source_cw == tmin_source_cw, tmax_source_cw, "multi"))))
# 
# ggplot(temp_long, aes(date, value)) +
#   geom_point(aes(fill = source), pch=21, col="grey70", alpha = 0.7) +
#   labs(title = "NWT LTER: Saddle daily temperature, 2015-2017, colored by data source",
#        subtitle = "Sources: Raw chart data, 2-week moving window regression, monthly regression, or multiple methods",
#        y = "Temperature (°C)",
#        x = "Date") +
#   scale_fill_brewer(palette = "Set2", direction = -1) +
#   theme_bw() +
#   facet_grid(.~metric)
# 
# # ctw notices in some parts of record chart temp has decimal places and in more recent years is rounded (i didn't do this)
# 
# 
# 
# ######################################################
# ## Combine ALL years (NSF data + ctw infilled data) ##
# 
# sdl_temp_infill_19822017 <- NSF_temp %>%
#   dplyr::rename(airtemp_min = TMIN,
#                 airtemp_max = TMAX) %>%
#   mutate(tmax_source_cw = "NSF proposal data",
#          tmin_source_cw = "NSF proposal data") %>%
#   dplyr::select(LTER_site, local_site, date, airtemp_max, tmax_source_cw,
#                 airtemp_min, tmin_source_cw)
# sdl_temp_infill_19822017$airtemp_mean <- with(sdl_temp_infill_19822017, (round(((airtemp_max + airtemp_min)/2), 2)))
# 
# sdl_temp_infill_19822017 <- rbind(sdl_temp_infill_19822017,sdl_infilled_2wk_monthly_1517)
# 
# #write.csv(sdl_temp_infill_19822017, 
# #          "/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/sdl_temp_infilled_19822017_ctw.csv")
# 
# 
# # one last plot of all data..
# ggplot(sdl_temp_infill_19822017, aes(date, airtemp_max)) +
#   #geom_point(data=sdl_charttemp, aes(date, airtemp_max), size = 1.5) +
#   geom_point(aes(col = tmax_source_cw),alpha=0.5) +
#   geom_smooth(method = "lm", col = "black") +
#   theme_bw()
# 
# ggplot(sdl_temp_infill_19822017, aes(date, airtemp_min)) +
#   geom_point(aes(col = tmax_source_cw),alpha=0.5) +
#   geom_smooth(method = "lm", col = "black")
# 
# ggplot(sdl_temp_infill_19822017, aes(date, airtemp_mean, col = tmax_source_cw)) +
#   geom_point(alpha=0.5)
# 
