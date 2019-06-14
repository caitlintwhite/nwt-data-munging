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

# metadata methods from sdl chart:
# A regression was performed using the chart recorder data at the missing site and the adjacent site using a 14 day window prior to and after the missing day, the regression equation was applied to the value at the known site to determine the value for filling the missing site. 
# A r^2 value above 0.6 were considered acceptable. If the r^2 was below 0.6, or missing, a Standard Deviation Method was used to fill the missing value (see Method Flag 2). 

#sd method:
# (Known Site Date Value (yyyy-mm-dd) / Std. Dev. of the day (mm-dd) at the known site) : ((x) / Std. Dev. of the day(mm-dd) at the unknown site)


# -- SETUP ----
rm(list = ls())
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
sdlcr_qa <- read_csv("extended_summer/output_data/ctw/qa_sdlcr_temp.csv", na = na_vals, trim_ws = T) %>% as.data.frame()
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



# 14-day moving window regression
movingfill <- function(dat, metric, ytemp, xtemp, nmin = 10, days = 14, r2 = 0.6, p = 0.05){
  
  require(dplyr)
  
  #initialize df for storing infilled temp values
  infill_df <- data.frame()
  
  dat <- as.data.frame(dat)
  # id missing dates
  missing_dates <- sort(dat["date"][is.na(dat[ytemp])]) %>% as.Date()
  
  # for loop to execute moving window regressions on max T
  for(i in 1:length(missing_dates)){
    current_date <- missing_dates[i]
    # subset data to 2 weeks before and 2 weeks after missing date
    begin_date <- current_date - days
    end_date <-  current_date + days
    temp_df <- subset(dat, date >= begin_date & date <=end_date)
    # count complete records of chart tmax and logger tmax
    complete_obs <- sum(complete.cases(temp_df[c(xtemp, ytemp)]))
    # fill in date and count of complete observations
    infill_tempdf <- data.frame(missing_date = current_date, 
                                met = metric,
                                logger = temp_df$logger[temp_df$date == current_date],
                                complete_nobs = complete_obs,
                                mon = month(current_date), yr = year(current_date))
    
    ## logic check: at least 10 complete observations (both sources have tmax data on same day)
    if(complete_obs < nmin) {
      #NAs for all other cols
      infill_tempdf <- cbind(infill_tempdf, 
                             xtemp = temp_df[[xtemp]][temp_df$date == current_date],
                             fit = NA, upr = NA, lwr = NA, se = NA,
                             adjr2 = NA, pval = NA, RSE = NA, method = NA)
      #next # skip to next date
    } else {
      # if passes logic check, continue with linear regression .. 
      temp_model <- lm(as.formula(paste(ytemp, xtemp, sep = "~")), data = temp_df)
      temp_predict <- predict.lm(temp_model, newdata = temp_df[temp_df$date == current_date,], se.fit = T, interval = "prediction")
      infill_tempdf <- cbind(infill_tempdf,
                             xtemp = temp_df[[xtemp]][temp_df$date == current_date],
                             temp_predict$fit,
                             se = temp_predict$se.fit,
                             adjr2 = summary(temp_model)$adj.r.squared,
                             pval = summary(temp_model)$coefficients[8],
                             RSE = summary(temp_model)$sigma,
                             method = paste(xtemp, days, "day lm"))
    }
    infill_df <- rbind(infill_df, infill_tempdf)
  }
  
  # clean up
  infill_df <- infill_df %>%
    filter(adjr2 > r2 & pval <=p) # keep only infilled values with r2 > 0.6 and pval <= 0.05 (per metadata methods)
  # correct colnames
  names(infill_df)[grepl("xtemp", colnames(infill_df))] <- xtemp
  return(infill_df)
}


# -- REVIEW AND PREP DATA FOR REGRESSIONS -----
# tidy sdl chart
sdl_long <- tidytemp(sdl, dropcol = "airtemp_avg")
glimpse(sdl_long)


# combine both sdl logger and sdl chart temp datasets wide-form
# > don't include mar 2015 - aug 2016 in chart data (tmax vals got truncated)
sdl_crchart <- sdlcr_qa %>%
  #dplyr::select(-c(cr_temp)) %>% # use qa'd temps
  full_join(subset(sdl_long, date < as.Date("2015-03-01") | date > as.Date("2016-08-30)"))) %>%
  arrange(met, `date`) %>%
  rename(sdl_temp = temp,
         sdl_flag = flag) %>%
  mutate(mon = as.factor(month(date)))

# how many missing data points for logger?
with(sdl_crchart, lapply(split(cr_temp, met), function(x)summary(is.na(x)))) # about half of the data missing over entire record
with(sdl_crchart, lapply(split(cr_temp[date >= min(sdlcr_qa$date)], met[date >= min(sdlcr_qa$date)]), function(x)summary(is.na(x)))) # about half of the data missing over entire record
# 2040 tmax points missing, and 1912 tmin points missing since cr loggers launched in 1986
with(sdl_crchart, lapply(split(cr_temp[date >= min(sdlcr_qa$date)], logger[date >= min(sdlcr_qa$date)]), function(x)summary(is.na(x)))) # about half of the data missing over entire record
#cr21x missing the most data points (about half), then cr23x, cr1000 missing one point (due to tmin sensor fail)



# -- (1) MOVING WINDOW REGRESSION INFILLING -----
# ***** Method 1: moving window regression *******
## > 2 week before and after date missing
tmax_2wksdlchart <- movingfill(dat = subset(sdl_crchart, met == "airtemp_max"), 
                      metric = "airtemp_max", xtemp = "sdl_temp", ytemp = "qa_temp")
tmin_2wksdlchart <- movingfill(dat = subset(sdl_crchart, met == "airtemp_min"), 
                               metric = "airtemp_min", xtemp = "sdl_temp", ytemp = "qa_temp")




# -- (2) MONTHLY LM REGRESSION ------
# ***** Method 2: monthly lm regression *******
# linear regression of cr_temp ~ sdl_temp + month + logger; use all data points available
 
# compare model options..
# regress using all complete pairs available
summary(lm(cr_temp ~ sdl_temp + logger, data = subset(sdl_crchart, met == "airtemp_max"))) #adj-R2 = 0.9742, RSE 1.524
summary(lm(cr_temp ~ sdl_temp + mon + logger, data = subset(sdl_crchart, met == "airtemp_max"))) #adj-R2 = 0.9751, RSE 1.497
summary(lm(cr_temp ~ sdl_temp + mon + yr * logger, data = subset(sdl_crchart, met == "airtemp_max"))) #adj-R2 = 0.9755, RMSE 1.483

# compare top models with anova
anova(lm(cr_temp ~ sdl_temp + mon + logger, data = subset(sdl_crchart, met == "airtemp_max")),
      lm(cr_temp ~ sdl_temp + mon + yr * logger, data = subset(sdl_crchart, met == "airtemp_max"))) # model with yr*logger interation is better

# specify lms to predict tmin and tmax (use same for both)
tmaxlm <- lm(qa_temp ~ sdl_temp + mon + yr * logger, data = subset(sdl_crchart, met == "airtemp_max"))
summary(tmaxlm)
plot(tmaxlm) # 3-4 points influential.. keeping in for now
tminlm <- lm(qa_temp ~ sdl_temp + mon + yr * logger, data = subset(sdl_crchart, met == "airtemp_min"))
summary(tminlm)
plot(tminlm)

# create null models to calculate p-vals for predicted vals
nulltmax <- lm(qa_temp ~ 1, data = subset(sdl_crchart, met == "airtemp_max" & !is.na(qa_temp) & !is.na(sdl_temp)))
nulltmin <- lm(qa_temp ~ 1, data = subset(sdl_crchart, met == "airtemp_min" & !is.na(qa_temp) & !is.na(sdl_temp)))


## ***** Infill missing values not filled by moving window regression
predtmax <- predict.lm(tmaxlm, newdata = subset(sdl_crchart, met == "airtemp_max" & is.na(qa_temp)), #& !date %in% tmax_2wksdlchart$missing_date), 
                       se.fit = T, type = "response", interval = "prediction")
predtmin <- predict.lm(tminlm, newdata = subset(sdl_crchart, met == "airtemp_min" & is.na(qa_temp)), #& !date %in% tmin_2wksdlchart$missing_date), 
                       se.fit = T, type = "response", interval = "prediction")

# compile tmax predictions
tmax_monthly_regress <- subset(sdl_crchart, met == "airtemp_max" & is.na(qa_temp)) %>%
  mutate(complete_nobs = nrow(tmaxlm$model)) %>%
  dplyr::select(date, met, logger, complete_nobs, mon, yr, sdl_temp) %>%
  rename(missing_date = date) %>%
  cbind(data.frame(predtmax$fit,
                   se = predtmax$se.fit,
                   adjr2 = summary(tmaxlm)$adj.r.squared,
                   pval = anova(nulltmax, tmaxlm)$'Pr(>F)'[2],
                   RSE = sigma(tmaxlm),
                   method = "sdl chart month lm"))

# compile tmin predictions
tmin_monthly_regress <- subset(sdl_crchart, met == "airtemp_min" & is.na(qa_temp)) %>%
  mutate(complete_nobs = nrow(tminlm$model)) %>%
  dplyr::select(date, met, logger, complete_nobs, mon, yr, sdl_temp) %>%
  rename(missing_date = date) %>%
  cbind(data.frame(predtmin$fit,
                   se = predtmin$se.fit,
                   adjr2 = summary(tminlm)$adj.r.squared,
                   pval = anova(nulltmin, tminlm)$'Pr(>F)'[2],
                   RSE = sigma(tminlm),
                   method = "sdl chart month lm"))



# -- INFILL BY SD METHOD FOR COMPARISON -----
# don't group by logger to blend whatever differences by logger (also cr1000 would only have 4 pts for std deviation)
dat <- sdl_crchart %>%
  group_by(doy) %>%
  mutate(sdcr = sd(qa_temp, na.rm = T),
         sdsdl = sd(sdl_temp, na.rm = T)) %>%
  ungroup() %>%
  mutate(sdlrat = sdl_temp/sdsdl,
         proj_cr = sdlrat * sdcr)

ggplot(dat, aes(qa_temp, proj_cr)) +
  geom_point(alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0), col = "red") +
  facet_wrap(~ met)



# -- PROJECT LOGGER TO 1980s FROM SDL CHART ---
# use cr1000 since least qa problems and most recent, will fit better when cr21x values start in 1986

cr1000lm <- lm(qa_temp ~ sdl_temp + mon * met, data = subset(sdl_crchart, logger == "cr1000"))
cr21xlm <- lm(qa_temp ~ sdl_temp + mon * met, data = subset(sdl_crchart, logger == "cr21x"))

cr1000_pred <- predict(cr1000lm, newdata = subset(sdl_crchart, date < min(sdlcr_qa$date)), se.fit = T, interval = "predict") 
cr21x_pred <- predict(cr21xlm, newdata = subset(sdl_crchart, date < min(sdlcr_qa$date)), se.fit = T, interval = "predict")
  
compare80 <- subset(sdl_crchart, date < min(sdlcr_qa$date)) %>%
  cbind(cr1000_pred$fit, se = cr1000_pred$se.fit, method = "cr1000") %>%
  rbind(cbind(subset(sdl_crchart, date < min(sdlcr_qa$date)),
              cr21x_pred$fit, se = cr21x_pred$se.fit, method = "cr21x")) %>%
  as.data.frame()

# visualize predictions
ggplot(compare80, aes(date, fit, col = method)) +
  geom_point(data = subset(dat, date %in% compare80$date), aes(date, proj_cr), col = "purple", alpha = 0.3) +
  geom_point(alpha = 0.3) +
  scale_color_grey() +
  facet_grid(method ~ met)

ggplot(compare80, aes(doy, fit, col = method)) +
  geom_line(data = subset(dat, date %in% compare80$date), aes(doy, proj_cr), col = "purple", alpha = 0.6) +
  geom_line(alpha = 0.6) +
  scale_color_viridis_d(option = "E") +
  facet_grid(yr ~ met)

# plot logger 1 vs logger 2 predictions
## tmax
ggplot(compare80, aes(date, fit, col = method)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ met)
dplyr::select(compare80, date:met, fit, method) %>%
  spread(method, fit) %>%
  ggplot(aes(cr1000, cr21x)) +
  geom_point(alpha = 0.5) +
  geom_abline(aes(slope=1, intercept = 0), col = "red") +
  facet_wrap(~ met, scale = "free")
dplyr::select(compare80, date:met, fit, method) %>%
  spread(method, fit) %>%
  mutate(delta = cr1000 - cr21x) %>%
  ggplot(aes(date, delta)) +
  geom_hline(aes(yintercept = 0), col = "red") +
  geom_point(alpha = 0.5) +
  labs(y = "cr1000 Tpred - cr21x Tpred") +
  facet_wrap(~ met)
dplyr::select(compare80, date:met, fit, method) %>%
  spread(method, fit) %>%
  mutate(delta = cr1000 - cr21x) %>%
  ggplot(aes(mon, delta)) +
  geom_boxplot(alpha = 0.5) +
  labs(y = "cr1000 Tpred - cr21x Tpred") +
  facet_wrap(~ met)


# -- COMPARE RESULTS -----
# in dates that overlap, compare moving window regression against monthly regression

# compile all estimated results and write out for reference
predict_all <- rbind(tmax_2wksdlchart, tmin_2wksdlchart) %>%
  #rearrange cols so match monthly regress cols
  dplyr::select(colnames(tmax_monthly_regress)) %>%
  rbind(tmax_monthly_regress, tmin_monthly_regress) %>%
  arrange(met, missing_date) %>%
  # change month back in to numeric for joining with sdl cr full dataset
  mutate(mon = month(missing_date))


# visualize predicted values
## predicted temps against date, colored by method
ggplot(predict_all, aes(missing_date, fit, col = method)) +
  geom_point(data = dat, aes(date, proj_cr), col = "purple", alpha = 0.4) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~met)

## predicted temp vs predicted temp
dplyr::select(predict_all, missing_date:logger, mon:fit, method) %>%
  subset(!is.na(logger)) %>%
  spread(method, fit) %>%
  ggplot(aes(`sdl chart month lm`, `sdl_temp 14 day lm`)) +
  geom_point(alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0), col ="red") +
  facet_wrap(~met)

## plot with logger data
ggplot(predict_all, aes(missing_date, fit, col = method)) +
  geom_point(data = sdl_crchart, aes(date, qa_temp), col = "grey60", alpha = 0.6) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~met)

## plot fitted values with cr logger data 
### tmax values
subset(predict_all, !is.na(fit) & met == "airtemp_max") %>%
  mutate(doy = yday(missing_date)) %>%
  ggplot(aes(doy, fit, col = method)) +
  geom_point(data = subset(sdl_crchart, met == "airtemp_max" & yr > 1985), aes(doy, qa_temp), col = "grey60", alpha = 0.4) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~yr)

### tmin values
subset(predict_all, !is.na(fit) & met == "airtemp_min") %>%
  mutate(doy = yday(missing_date)) %>%
  ggplot(aes(doy, fit, col = method)) +
  geom_point(data = subset(sdl_crchart,met == "airtemp_min" & yr > 1985), aes(doy, qa_temp), col = "grey60", alpha = 0.4) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~yr)


# what is the difference in summer mean temp by infill method?
subset(compare80, mon %in% 6:8) %>%
  group_by(method, met) %>%
  summarise(meanT = mean(fit),
          sdT = sd(fit)) # about a 2degree diff using cr1000 vs cr21x, sd is comparable

# what does it look like with raw data? 1981-1991 as example..
ggplot(compare80, aes(date, fit)) +
  geom_point(alpha = 0.5, col = "purple") +
  geom_point(data = predict_all[predict_all$yr < 1990, colnames(predict_all) != "method"], aes(missing_date, fit), col = "seagreen", alpha = 0.5) +
  geom_point(data = subset(sdlcr_qa, yr <1990), aes(date, qa_temp), alpha = 0.5) +
  facet_grid(met~method, scales = "free_y")



# -- APPEND PREDICTED TO CR DATASET, PLOT TO ASSESS -----
sdlcr_fits <- left_join(sdlcr_qa, predicted_cr_all, 
                        by = c("date" = "missing_date", "met", "logger", "yr", "mon")) 
