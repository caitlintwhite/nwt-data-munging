# infill saddle logger, project logger temp backwards

# script purpose:
# read in qa'd sdl logger data and sdl chart (AND d1 logger for comparison?)
# infill missing values in sdl logger dataset following infill methods for sdl chart in sdl chart metadata
# project missing 1980s from sdl chart (using .. current saddle logger? whichever has best correlation? closest in time?)
# > could try projecting 1980s using all loggers and compare
# write out infilled dataset for extended summer PCA

# notes: 
# sdl chart values mar 2015-aug 2016 no bueno (as of 2019-06-13), need to be corrected (ctw and jen morse figured this out last week, jm looking into it)
# don't use those chart values in regression
# sdl chart infilled by NWT staff from start of dataset through 2008, but no missing values in 2009


# hierarchy of infill methods for temp (based on methods in saddle chart metadata)
# 1) 2 week moving window regression using saddle chart data
# 2) monthly regression using saddle chart data
# 3) std. deviation ratio method using chart data
# 4) 2 week moving window regression using D1 data (*d1 logger rather than chart better?)
# 5) monthly regression using D1 data
# 6) std. deviation ratio method using D1 data

# some rules:
# adj R^2 > 0.6 (higher better)
# pval =< 0.05



# -- SETUP ----
#rm(list = ls())
library(tidyverse)
library(lubridate)
#library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN")

# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("edi_functions.R")


# get data
# qa'd sdl cr logger data, 1986-ongoing
sdlcr_qa <- read_csv("extended_summer/output_data/ctw/qa_sdlcr_temp.csv", na = na_vals, trim_ws = T)
# sdl chart
sdl <- getTabular(413) %>% as.data.frame()




# -- FUNCTIONS ------
# function to tidy temp datasets (this could be made generic for ppt too..)
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


# -- REVIEW AND PREP DATA FOR REGRESSIONS -----
sdl_long <- tidytemp(sdl, dropcol = "airtemp_avg")
glimpse(sdl_long)




###############################################
### Regression infilling with saddle chart ###
###############################################
# combine both sdl logger and sdl chart temp datasets wide-form
# > don't include mar 2015 - aug 2016 in chart data (tmax vals got truncated)
sdl_crchart <- sdlcr_qa %>%
  dplyr::select(-c(cr_temp)) %>% # use qa'd temps
  full_join(subset(sdl_long, date < as.Date("2015-03-01") | date > as.Date("2016-08-30)"))) %>%
  arrange(met, `date`) %>%
  rename(cr_temp = qa_temp,
         cr_flag = qa_flag,
         sdl_temp = temp,
         sdl_flag = flag) %>%
  mutate(mon = as.factor(month(date)))

# how many missing data points for logger?
with(sdl_crchart, lapply(split(cr_temp, met), function(x)summary(is.na(x)))) # about half of the data missing over entire record
with(sdl_crchart, lapply(split(cr_temp[date >= min(sdlcr_qa$date)], met[date >= min(sdlcr_qa$date)]), function(x)summary(is.na(x)))) # about half of the data missing over entire record
# 2040 tmax points missing, and 1912 tmin points missing since cr loggers launched in 1986
with(sdl_crchart, lapply(split(cr_temp[date >= min(sdlcr_qa$date)], logger[date >= min(sdlcr_qa$date)]), function(x)summary(is.na(x)))) # about half of the data missing over entire record
#cr21x missing the most data points (about half), then cr23x, cr1000 missing one point (due to tmin sensor fail)

# grab missing dates
tmax_missing_dates <- sdl_crchart$date[is.na(sdl_crchart$cr_temp) & sdl_crchart$met == "airtemp_max"]
tmin_missing_dates <- sdl_crchart$date[is.na(sdl_crchart$cr_temp) & sdl_crchart$met == "airtemp_min"]
# subset to time period of cr logger datasets for 2-week regression
tmax_missing_dates_cr <- tmax_missing_dates[tmax_missing_dates >= min(sdlcr_qa$date)]
tmin_missing_dates_cr <- tmin_missing_dates[tmin_missing_dates >= min(sdlcr_qa$date)]



# ***** Method 1: moving window regression *******
## > 2 week before and after date missing

#initialize df for max temp
max_temp_infill_2wk <- data.frame()
# for loop to execute moving window regressions on max T
for(i in 1:length(tmax_missing_dates_cr)){
  current_date <- tmax_missing_dates_cr[i]
  # subset data to 2 weeks before and 2 weeks after missing date
  begin_date <- current_date - 14
  end_date <-  current_date + 14
  temp_df <- subset(sdl_crchart, date >= begin_date & date <=end_date & met == "airtemp_max")
  # count complete records of chart tmax and logger tmax
  complete_obs <- nrow(temp_df[!is.na(temp_df$sdl_temp & temp_df$cr_temp),])
  # fill in date and count of complete observations
  infill_tempdf <- data.frame(missing_date = current_date, 
                              met = "airtemp_max",
                              logger = temp_df$logger[temp_df$date == current_date],
                              complete_nobs = complete_obs,
                              mon = month(current_date), year = year(current_date))
  
  ## logic check: at least 10 complete observations (both sources have tmax data on same day)
  if(complete_obs < 10) {
    #NAs for all other cols
    infill_tempdf <- cbind(infill_tempdf, 
                           sdl_temp = temp_df$sdl_temp[temp_df$date == current_date],
                           fit = NA, upr = NA, lwr = NA, se = NA,
                           adjr2 = NA, pval = NA, RMSE = NA, method = NA)
    next # skip to next date
  }
  
  # if passes logic check, continue with linear regression ..  
  else {
    temp_model <- lm(cr_temp ~ sdl_temp, data=temp_df)
    temp_predict <- predict.lm(temp_model, newdata = temp_df[temp_df$date == current_date,], se.fit = T, interval = "prediction")
    infill_tempdf <- cbind(infill_tempdf,
                           sdl_temp = temp_df$sdl_temp[temp_df$date == current_date],
                           temp_predict$fit,
                           se = temp_predict$se.fit,
                           adjr2 = summary(temp_model)$adj.r.squared,
                           pval = summary(temp_model)$coefficients[8],
                           RMSE = summary(temp_model)$sigma,
                           method = "sdl chart 2wk lm")
  }
  max_temp_infill_2wk <- rbind(max_temp_infill_2wk, infill_tempdf)
}

# clean up
max_temp_infill_2wk <- max_temp_infill_2wk %>%
  filter(adjr2 > 0.6 & pval <= 0.05) # keep only infilled values with r2 > 0.6 and pval <= 0.05 (per metadata methods)

# asssess:
# how many dates missing?
length(tmax_missing_dates_cr)
# how many infilled?
nrow(max_temp_infill_2wk)
# how many remaining missing value dates?
tmax_dates_remain <- tmax_missing_dates_cr[!tmax_missing_dates_cr %in% max_temp_infill_2wk$missing_date]


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
tmin_dates_remain <- tmin_missing_dates_cr[!tmin_missing_dates_cr %in% min_temp_infill_2wk$missing_date]
summary(tmin_dates_remain %in% sdl_loggerdat$date[!is.na(sdl_loggerdat$airtemp_min)]) # apparently all there..


# ***** Method 2: monthly lm regression *******
# linear regression of cr_temp ~ sdl_temp + month + logger; use all data points available
# notes: 

# regress using all complete pairs available
summary(lm(cr_temp ~ sdl_temp + logger, data = subset(sdl_crchart, met == "airtemp_max"))) #adj-R2 = 0.9742, RSE 1.524
summary(lm(cr_temp ~ sdl_temp + mon + logger, data = subset(sdl_crchart, met == "airtemp_max"))) #adj-R2 = 0.9751, RSE 1.497
summary(lm(cr_temp ~ sdl_temp + mon + yr * logger, data = subset(sdl_crchart, met == "airtemp_max"))) #adj-R2 = 0.9755, RMSE 1.483

# compare top models with anova
anova(lm(cr_temp ~ sdl_temp + mon + logger, data = subset(sdl_crchart, met == "airtemp_max")),
      lm(cr_temp ~ sdl_temp + mon + yr * logger, data = subset(sdl_crchart, met == "airtemp_max"))) # model with yr*logger interation is better

# specify lms to predict tmin and tmax (use same for both)
tmaxlm <- lm(cr_temp ~ sdl_temp + mon + yr * logger, data = subset(sdl_crchart, met == "airtemp_max"))
plot(tmaxlm) # 3-4 points influential.. keeping in for now
tminlm <- lm(cr_temp ~ sdl_temp + mon + yr * logger, data = subset(sdl_crchart, met == "airtemp_min"))
summary(tminlm)
plot(tminlm)

# create null models to calculate p-vals for predicted vals
nulltmax <- lm(cr_temp ~ 1, data = subset(sdl_crchart, met == "airtemp_max" & !is.na(cr_temp) & !is.na(sdl_temp)))
nulltmin <- lm(cr_temp ~ 1, data = subset(sdl_crchart, met == "airtemp_min" & !is.na(cr_temp) & !is.na(sdl_temp)))


## ***** Infill missing 2010-2017 values not filled by moving window regression + all 2018 daily tmin and tmax using CR1000 regression (sdl chart stops after 2017-12-31)
predtmax <- predict.lm(tmaxlm, newdata = subset(sdl_crchart, date %in% tmax_dates_remain), se.fit = T, type = "response", interval = "prediction")
predtmin <- predict.lm(tminlm, newdata = subset(sdl_crchart, date %in% tmin_dates_remain), se.fit = T, type = "response", interval = "prediction")

# compile tmax predictions
crtmax_monthly_regress <- subset(sdl_crchart, date %in% tmax_dates_remain & met == "airtemp_max") %>%
  mutate(complete_nobs = nrow(tmaxlm$model)) %>%
  dplyr::select(date, met, logger, complete_nobs, mon, yr, sdl_temp) %>%
  rename(missing_date = date) %>%
  cbind(data.frame(predtmax$fit,
                   se = predtmax$se.fit,
                   adjr2 = summary(tmaxlm)$adj.r.squared,
                   pval = anova(nulltmax, tmaxlm)$'Pr(>F)'[2],
                   RMSE = sigma(tmaxlm),
                   method = "sdl chart month lm"))
rename(tmax_logger = cr1000airtemp_max)

# compile tmin predictions
crtmin_monthly_regress <- subset(sdl_crchart, date %in% tmin_dates_remain & met == "airtemp_min") %>%
  mutate(complete_nobs = nrow(tminlm$model)) %>%
  dplyr::select(date, met, logger, complete_nobs, mon, yr, sdl_temp) %>%
  rename(missing_date = date) %>%
  cbind(data.frame(predtmin$fit,
                   se = predtmin$se.fit,
                   adjr2 = summary(tminlm)$adj.r.squared,
                   pval = anova(nulltmin, tminlm)$'Pr(>F)'[2],
                   RMSE = sigma(tminlm),
                   method = "sdl chart month lm"))

# compile all estimated results and write out for reference
predicted_cr_all <- rbind(max_temp_infill_2wk, min_temp_infill_2wk) %>%
  rename(yr = `year`) %>%
  rbind(crtmax_monthly_regress, crtmin_monthly_regress) %>%
  distinct() %>%
  arrange(met, missing_date) %>%
  # change month back in to numeric for joining with sdl cr full dataset
  mutate(mon = month(missing_date))

test <- subset(predicted_cr_all, met == "airtemp_min") %>%
  filter(duplicated(missing_date) == T)

View(subset(predicted_cr_all, missing_date %in% test$missing_date))
# -- APPEND PREDICTED TO CR DATASET, PLOT TO ASSESS -----
sdlcr_fits <- left_join(sdlcr_qa, predicted_cr_all, 
                        by = c("date" = "missing_date", "met", "logger", "yr", "mon")) 
