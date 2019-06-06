# compile and infill (as needed) saddle logger daily air tmax and tmin for extended summer pca

# steps:
# read in saddle logger datasets: DP211 saddle logger (1990-2006) [knb-lter-nwt.80], CR23X (2006-2012) + CR1000 (2012-ongoing) [knb-lter-nwt.413]
# look for unusual values and differences air temp by logger (compare relationship to saddle chart? -- altho SCE said there's drift in the chart over time)
# compile all years data in data table and write out to run in ext sum PCA sensitivity analysis

# notes:




# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(lubridate)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN")

# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("edi_functions.R")

# -- GET DATA -----
dp211 <- getTabular(80)
#crlogs <- getTabular() # most current logger dataset not on EDI yet, provided by SCE
crlogs <- read.csv("/Users/serahsierra/Documents/nwt_lter/unpub_data/sdlcr23x-cr1000.daily.ml.data.csv",
                   strip.white = T, na.strings = c("", " ", ".", NA, NaN, "NA", "NaN"))
sdlchart <- getTabular(413)
# keith jennings et al. infill (best reference for data quality?) -- **hourly data**
jennings <- getTabular(168)
# read in jennings raw data to compare uncorrected daily max and min temp
jenraw <- read.csv("/Users/serahsierra/Documents/nwt_lter/unpub_data/sdl_hrly_met_data_all_NOQC.csv", na.strings = na_vals)
#nwt renewal data  
nwt_temps <- read.csv("~/Dropbox/NWT_data/Saddle_precip_temp_formoisturedeficit.csv") %>%
  mutate(`date` = as.Date(paste(Year,Month,Day, sep="-")),
         LTER_site = "NWT",
         local_site = "sdl") %>%
  dplyr::select(LTER_site, local_site, date, TMIN, TMAX) 
  

# -- REVIEW DATA -----
# review how dats read in
glimpse(dp211) # no flag cols
glimpse(crlogs) # flag cols present
glimpse(sdlchart) # flags cols present
glimpse(jennings)

# drop cols not needed in cr logger dataset (i.e. only need airtemp cols up thru avg airtemp)
crlogs <- crlogs[,1:12]
# drop solar radiation in dp211
dp211 <- dp211[!grepl("solrad", names(dp211))]

# convert dates in all to Date class
dp211$date <- as.Date(dp211$date, format = "%Y-%m-%d")
crlogs$date <- as.Date(crlogs$date, format = "%Y-%m-%d")
sdlchart$date <- as.Date(sdlchart$date, format = "%Y-%m-%d")
jennings$date <- as.Date(jennings$date, format = "%Y-%m-%d")
jenraw$date <- as.Date(jenraw$date, format = "%Y-%m-%d")


# aggregate jennings et al. airtemp to daily summary values (max, min)
jendaily <- dplyr::select(jennings, LTER_site, local_site, year:date, airtemp_avg) %>%
  subset(local_site == "sdl") %>%
  group_by(LTER_site, local_site, year, jday, date) %>%
  summarize(airtemp_min = min(airtemp_avg),
         airtemp_max = max(airtemp_avg))
# repeat for raw values
summary(duplicated(jenraw$datetime)) # no timestamps are duplicated, that's good
jenraw_daily <- dplyr::select(jenraw, year:temp, logger) %>%
  group_by(date, doy) %>%
  summarize(airtemp_min = min(temp),
            airtemp_max = max(temp))

# unique values of flags? and frequency of their occurrence?
sapply(crlogs[grepl("flag", colnames(crlogs))], function(x) summary(as.factor(x))) # only n's, correspond to no flag
sapply(sdlchart[grepl("flag", colnames(sdlchart))], function(x) summary(as.factor(x))) # 246 tmax, 238 tmin "1" flags; 14 tmax, 16 tmin "2" flags
# saddle chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available)

# look at tails for any obvious bad values
## cr data loggers
sapply(crlogs[grepl("^airtemp", colnames(crlogs))], function(x) tail(sort(x))) #okay
sapply(crlogs[grepl("^airtemp", colnames(crlogs))], function(x)tail(sort(x, decreasing = T))) #-187 in airtemp_min
## dp211 logger
sapply(dp211[grepl("^airtemp", colnames(dp211))], function(x) tail(sort(x))) #77.5 in airtemp_max
sapply(dp211[grepl("^airtemp", colnames(dp211))], function(x)tail(sort(x, decreasing = T))) #-50 in airtemp_min
## sdl chart
sapply(sdlchart[grepl("^airtemp", colnames(sdlchart))], function(x) tail(sort(x))) #okay, high max value agrees with cr logger high value
sapply(sdlchart[grepl("^airtemp", colnames(sdlchart))], function(x)tail(sort(x, decreasing = T))) #okay
## jennings (not expecting anything but to be sure..)
sapply(jendaily[grepl("^airtemp", colnames(jendaily))], function(x) tail(sort(x))) # reasonable
sapply(jendaily[grepl("^airtemp", colnames(jendaily))], function(x)tail(sort(x, decreasing = T))) #reasonable

# were mean values affected by bad values in logger dataset?
na.omit(crlogs[crlogs$airtemp_min == -187, grepl("^airtemp", colnames(crlogs))]) #doesn't look like it
#       airtemp_max airtemp_min airtemp_avg
# 6087       0.812        -187    -3.32507
na.omit(dp211[dp211$airtemp_min == -50, grepl("^airtemp", colnames(dp211))]) #row 1068 has the bad maxtemp value also
#         airtemp_max airtemp_min airtemp_avg
# 1068        77.5         -50         0.6
# 4179        -2.5         -50        -7.4
# 4347         0.5         -50       -31.8
# 4766         5.5         -50        -0.7
# 5145        12.5         -50         3.0

# change bad values to NAs and move on
crlogs$airtemp_min[crlogs$airtemp_min == -187 & !is.na(crlogs$airtemp_min)] <- NA
dp211$airtemp_min[dp211$airtemp_min == -50 & !is.na(dp211$airtemp_min)] <- NA
dp211$airtemp_max[dp211$airtemp_max == 77.5 & !is.na(dp211$airtemp_max)] <- NA

summary(dp211[grepl("^airtemp", colnames(dp211))])
summary(crlogs[grepl("^airtemp", colnames(crlogs))])
summary(sdlchart[grepl("^airtemp", colnames(sdlchart))])
# ranges look reasonable. move on..

# prep jennings long form
jenlong <- gather(jendaily, met, jenval, airtemp_max, airtemp_min)



# -- REVIEW AND PREP SDL CHART DATA FOR PAIRING WITH LOGGER DATA -----
# visualize infilled sdl chart values
ggplot(sdlchart, aes(yday(date), airtemp_min, col = as.factor(flag_airtemp_min))) +
  geom_point(alpha = 0.5) +
  ggtitle("sdl chart tmin; colors = infilled method by NWT (1, regression; 2, sd; NA, no infill done)") +
  facet_wrap(~year(date)) #1984 tmin infilled vals look low relative to entire record..
ggplot(sdlchart, aes(yday(date), airtemp_max, col = as.factor(flag_airtemp_max))) +
  geom_point(alpha = 0.5) +
  ggtitle("sdl chart tmin; colors = infilled method by NWT (1, regression; 2, sd; NA, no infill done)") +
  facet_wrap(~year(date))

# combine logger temp datasets for comparison with saddle
sdllong <- sdlchart %>%
  gather(metric, chart_temp, airtemp_max:airtemp_avg)
sdlflags <- subset(sdllong, grepl("flag", metric)) %>%
  rename(chart_flag = chart_temp) %>%
  mutate(metric = gsub("flag_", "", metric))
sdllong <- subset(sdllong, !grepl("flag", metric)) %>%
  left_join(sdlflags)
rm(sdlflags)



# -- VISUAL QA DP211 DATASET ----
# time series with NA vals infilled to visualize where data absent
dp211 %>%
  gather(met, val, airtemp_max:ncol(.)) %>%
  mutate(NAval = ifelse(is.na(val), "yes", "no"),
         val = ifelse(is.na(val), 0, val)) %>% # infill NAs with 0 to see missing
  ggplot(aes(date, val, col = NAval)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("yes" = "red", "no" = "black")) +
  facet_wrap(~met)

# look at airtemp max and min more closely by year
ggplot(dp211, aes(jday,airtemp_max)) +
  geom_line() +
  #geom_point(alpha = 0.5) +
  facet_wrap(~year, scales = "free_y")

# make dp211 data long form with flags for plotting
dp211_long <- arrange(dp211, date) %>% # arrange by date to make sure
  mutate(mon = month(date)) %>%
  dplyr::select(-airtemp_avg) %>%
  gather(met, val, airtemp_max,airtemp_min) %>%
  group_by(met) %>%
  mutate(lag1 = lag(val, 1),
         lead1 = lead(val, 1)) %>%
  ungroup() %>%
  mutate(deltafwd = val-lead1,
         deltabck = val-lag1) %>%
  group_by(mon, met) %>%
  # identify points more than 3sd from their monthly mean
  # group by month to calculate monthly mean and sd with upper and lower bounds
  mutate(tmon_mean = mean(val, na.rm =T),
         tmon_sd = sd(val, na.rm = T),
         tmon_hi = tmon_mean + 3*tmon_sd,
         tmon_lo = tmon_mean - 3*tmon_sd,
         lead_mean = mean(abs(deltafwd), na.rm =T),
         lead_sd = sd(abs(deltafwd), na.rm = T),
         lead_thresh = lead_mean + 3*lead_sd,
         lag_mean = mean(abs(deltabck), na.rm =T),
         lag_sd = sd(abs(deltabck), na.rm = T),
         lag_thresh = lag_mean + 3*lag_sd) %>%
  ungroup() %>%
  # flag vals outside 3sd from monthly mean, and calculate day to day deltas
  mutate(mas3sd = ifelse(val < tmon_lo | val > tmon_hi, "yes", "no"),
         leadflag = ifelse(abs(deltafwd) > lead_thresh, "yes", "no"),
         lagflag = ifelse(abs(deltabck) > lag_thresh, "yes", "no")) %>%
  # pair sdl chart data for comparing with flagged logger values
  left_join(sdllong, by = c("date", "met" = "metric")) %>%
  # crunch difference with sdl logger val and flag if more 3sd than grand mean (don't expect seasonal or monthly logger departure from sdl chart?)
  mutate(delta_chart = val - chart_temp,
         deltachart_mean = mean(abs(delta_chart), na.rm = T),
         deltachart_sd = sd(abs(delta_chart), na.rm =T),
         deltachart_thresh = deltachart_mean + 3*deltachart_sd,
         deltachart_flag = ifelse(abs(delta_chart) > deltachart_thresh, "yes", "no")) %>%
  # pair jennings dataset to compare with flagged logger values
  left_join(jenlong) %>%
  mutate(delta_jen = val - jenval,
         deltajen_mean = mean(abs(delta_jen), na.rm = T),
         deltajen_sd = sd(abs(delta_jen), na.rm =T),
         deltajen_thresh = deltajen_mean + 3*deltajen_sd,
         deltajen_flag = ifelse(abs(delta_jen) > deltajen_thresh, "yes", "no"))

# plot daily values, highlight values 3sds outside monthly mean
ggplot(subset(dp211_long, met == "airtemp_min"), aes(jday,val)) +
  geom_line() +
  geom_point(data = subset(dp211_long, met == "airtemp_min" & mas3sd == "yes"), aes(jday, val), col ="red", alpha = 0.5) + 
  geom_point(data = subset(dp211_long, met == "airtemp_min" & lagflag == "yes"), aes(jday, val), col ="orange", alpha = 0.5) + 
  geom_point(data = subset(dp211_long, met == "airtemp_min" & leadflag == "yes"), aes(jday, val), col ="blue", alpha = 0.5) + 
  labs(title = "Saddle DP211 logger: TMIN, red pts = more than 3sd from monthly grand mean",
       y = "TMIN (°C)", x = "Day of year") +
  facet_wrap(~year, scales = "free_x")
ggplot(subset(dp211_long, met == "airtemp_max"), aes(jday,val)) +
  geom_line() +
  #geom_point(data = subset(dp211_long, met == "airtemp_max" & mas3sd == "yes" & leadflag == "yes" & lagflag == "yes"), aes(jday, val), col ="red", alpha = 0.5) + 
  geom_point(data = subset(dp211_long, met == "airtemp_max" & mas3sd == "yes"), aes(jday, val), col ="red", alpha = 0.5) + 
  geom_point(data = subset(dp211_long, met == "airtemp_max" & lagflag == "yes"), aes(jday, val), col ="orange", alpha = 0.5) + 
  geom_point(data = subset(dp211_long, met == "airtemp_max" & leadflag == "yes"), aes(jday, val), col ="blue", alpha = 0.5) + 
  labs(title = "Saddle DP211 logger: TMAX, red pts = more than 3sd from monthly grand mean",
       y = "TMAX (°C)", x = "Day of year") +
  facet_wrap(~year)

# day-to-day differences
ggplot(dp211_long, aes(date, deltabck)) +
  geom_line() +
  geom_point(data = subset(dp211_long, lagflag == "yes" & leadflag == "yes"), aes(date, deltabck), col ="red", alpha = 0.5) + 
  facet_wrap(~met, scales = "free_y")
ggplot(subset(dp211_long, met == "airtemp_max"), aes(jday, deltafwd)) +
  geom_line() +
  geom_point(data = subset(dp211_long, met == "airtemp_max" & leadflag == "yes" & lagflag == "yes"), aes(jday, deltafwd), col ="red", alpha = 0.5) + 
  facet_wrap(~year)
ggplot(subset(dp211_long, met == "airtemp_min"), aes(jday, deltafwd)) +
  geom_line() +
  geom_point(data = subset(dp211_long, met == "airtemp_min" & leadflag == "yes" & lagflag == "yes"), aes(jday, deltafwd), col ="red", alpha = 0.5) + 
  facet_wrap(~year)

# monthly boxplots
dp211 %>%
  gather(met, val, airtemp_max:airtemp_avg) %>%
  ggplot(aes(month(date), val, group = month(date))) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5, width = 0.2, col = "dodgerblue") +
  scale_x_continuous(breaks = seq(1,12,1)) +
  facet_wrap(~met)
# jan and dec values also look a little funky (appears some jan dates comparably warm to spring days?), but since ext sum PCA doesn't rely on jan or dec values, not a concern to address

# look for logger sensor getting stuck
View(subset(dp211_long, deltabck == 0 & deltafwd == 0))


# plot dp211 deltas with sdl chart
ggplot(dp211_long, aes(date, delta_chart)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(data = subset(dp211_long, deltabck == 0 & deltafwd == 0), aes(date, delta_chart), col = "green", size = 5, pch = 4) +
  geom_point(data = subset(dp211_long, mas3sd == "yes"), aes(date, delta_chart), col = "purple", size = 4, pch = 1) +
  geom_point(data = subset(dp211_long, lagflag == "yes"), aes(date, delta_chart), col = "blue", size = 3, alpha = 0.5) +
  geom_point(aes(col = deltachart_flag), alpha = 0.5) +
  scale_color_manual(values = c("yes" = "red", "no" = "black"))

# there is a negative temporal trend in dp211-jennings infilled temp values...
# ctw saw this same negative temporal trend sdl chart-jennings infilled delta in a different analysis..
ggplot(dp211_long, aes(date, delta_jen)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(data = subset(dp211_long, deltabck == 0 & deltafwd == 0), aes(date, delta_jen), col = "green", size = 5, pch = 4) +
  geom_point(data = subset(dp211_long, mas3sd == "yes"), aes(date, delta_jen), col = "purple", size = 4, pch = 1) +
  geom_point(data = subset(dp211_long, lagflag == "yes"), aes(date, delta_jen), col = "blue", size = 3, alpha = 0.5) +
  geom_point(aes(col = deltajen_flag), alpha = 0.5) +
  scale_color_manual(values = c("yes" = "red", "no" = "black")) +
  facet_wrap(~met)



# -- VISUAL QA CR LOGGERS (BY LOGGER)-----
# repeat above for CR logger data
# occurence of missing values in cr logger data
crlogs[!grepl("flag", colnames(crlogs))] %>% # all flags are "n"
  gather(met, val, airtemp_max, airtemp_min,airtemp_avg) %>%
  mutate(NAval = ifelse(is.na(val), "yes", "no"),
         val = ifelse(is.na(val), 0, val)) %>% # infill NAs with 0 to see missing
  ggplot(aes(date, val, col = NAval)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("yes" = "red", "no" = "black")) +
  facet_wrap(~met)

# make long form with deltas and flags
crlogs_long <- crlogs[,!grepl("flag", colnames(crlogs))] %>% #remove flag cols
  arrange(date, logger) %>% # be sure arranged by date since two loggers involved
  mutate(mon = month(date)) %>%
  dplyr::select(-airtemp_avg) %>% # discard mean airtemp
  gather(met, val, airtemp_max,airtemp_min) %>%
  group_by(met, logger) %>%
  # calculate lead and lags
  mutate(lag1 = lag(val, 1),
         lead1 = lead(val, 1)) %>%
  ungroup() %>%
  # calculate fwd and backward deltas
  mutate(deltafwd = val-lead1,
         deltabck = val-lag1) %>%
  group_by(mon, met, logger) %>%
  # identify points more than 3sd from their monthly mean
  # group by month to calculate monthly mean and sd with upper and lower bounds
  mutate(tmon_mean = mean(val, na.rm =T),
         tmon_sd = sd(val, na.rm = T),
         tmon_hi = tmon_mean + 3*tmon_sd,
         tmon_lo = tmon_mean - 3*tmon_sd,
         lead_mean = mean(abs(deltafwd), na.rm =T),
         lead_sd = sd(abs(deltafwd), na.rm = T),
         lead_thresh = lead_mean + 3*lead_sd,
         lag_mean = mean(abs(deltabck), na.rm =T),
         lag_sd = sd(abs(deltabck), na.rm = T),
         lag_thresh = lag_mean + 3*lag_sd) %>%
  ungroup() %>%
  # flag vals outside 3sd from monthly mean or outside 3sd day-to-day delta
  mutate(mas3sd = ifelse(val < tmon_lo | val > tmon_hi, "yes", "no"),
         leadflag = ifelse(abs(deltafwd) > lead_thresh, "yes", "no"),
         lagflag = ifelse(abs(deltabck) > lag_thresh, "yes", "no")) %>%
  # pair sdl chart data for comparing with flagged logger values
  left_join(sdllong, by = c("LTER_site", "local_site", "date", "met" = "metric")) %>%
  # crunch difference with sdl logger val and flag if more 3sd than grand mean (don't expect seasonal or monthly logger departure from sdl chart?)
  mutate(delta_chart = val - chart_temp,
         deltachart_mean = mean(abs(delta_chart), na.rm = T),
         deltachart_sd = sd(abs(delta_chart), na.rm =T),
         deltachart_thresh = deltachart_mean + 3*deltachart_sd,
         deltachart_flag = ifelse(abs(delta_chart) > deltachart_thresh, "yes", "no")) %>%
  # pair jennings dataset to compare with flagged logger values
  left_join(jenlong) %>%
  mutate(delta_jen = val - jenval,
         deltajen_mean = mean(abs(delta_jen), na.rm = T),
         deltajen_sd = sd(abs(delta_jen), na.rm =T),
         deltajen_thresh = deltajen_mean + 3*deltajen_sd,
         deltajen_flag = ifelse(abs(delta_jen) > deltajen_thresh, "yes", "no"))


# plot out daily vals, colored if more than 3sd outside daily delta and/or logger historic monthly mean
ggplot(subset(crlogs_long, met == "airtemp_min"), aes(jday,val)) +
  geom_line() +
  geom_point(data = subset(crlogs_long, met == "airtemp_min" & mas3sd == "yes"), aes(jday, val), col ="red", alpha = 0.5) + 
  geom_point(data = subset(crlogs_long, met == "airtemp_min" & lagflag == "yes"), aes(jday, val), col ="orange", alpha = 0.5) + 
  geom_point(data = subset(crlogs_long, met == "airtemp_min" & leadflag == "yes"), aes(jday, val), col ="blue", alpha = 0.5) + 
  labs(title = "Saddle CR loggers: TMIN, red pts = more than 3sd from monthly grand mean",
       y = "TMIN (°C)", x = "Day of year") +
  facet_wrap(~year, scales = "free_x")
ggplot(subset(crlogs_long, met == "airtemp_max"), aes(jday,val)) +
  geom_line() +
  #geom_point(data = subset(crlogs_long, met == "airtemp_max" & mas3sd == "yes" & leadflag == "yes" & lagflag == "yes"), aes(jday, val), col ="red", alpha = 0.5) + 
  geom_point(data = subset(crlogs_long, met == "airtemp_max" & mas3sd == "yes"), aes(jday, val), col ="red", alpha = 0.5) + 
  geom_point(data = subset(crlogs_long, met == "airtemp_max" & lagflag == "yes"), aes(jday, val), col ="orange", alpha = 0.5) + 
  geom_point(data = subset(crlogs_long, met == "airtemp_max" & leadflag == "yes"), aes(jday, val), col ="blue", alpha = 0.5) + 
  labs(title = "Saddle CR loggers: TMAX, red pts = more than 3sd from monthly grand mean",
       y = "TMAX (°C)", x = "Day of year") +
  facet_wrap(~year)

# day-to-day differences
ggplot(crlogs_long, aes(date, deltabck)) +
  geom_line() +
  geom_point(data = subset(crlogs_long, lagflag == "yes" & leadflag == "yes"), aes(date, deltabck), col ="red", alpha = 0.5) + 
  facet_wrap(~met, scales = "free_y")
ggplot(subset(crlogs_long, met == "airtemp_max"), aes(jday, deltafwd)) +
  geom_line() +
  geom_point(data = subset(crlogs_long, met == "airtemp_max" & leadflag == "yes" & lagflag == "yes"), aes(jday, deltafwd), col ="red", alpha = 0.5) + 
  facet_wrap(~year)
ggplot(subset(crlogs_long, met == "airtemp_min"), aes(jday, deltafwd)) +
  geom_line() +
  geom_point(data = subset(crlogs_long, met == "airtemp_min" & leadflag == "yes" & lagflag == "yes"), aes(jday, deltafwd), col ="red", alpha = 0.5) + 
  facet_wrap(~year)


# monthly boxplots
crlogs[!grepl("flag", colnames(crlogs))] %>% # all flags are "n"
  gather(met, val, airtemp_max, airtemp_min,airtemp_avg) %>%
  ggplot(aes(month(date), val, group = month(date))) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, width = 0.2, col = "dodgerblue") +
  scale_x_continuous(breaks = seq(1,12,1)) +
  facet_wrap(~met) 



# plot crlogs deltas with sdl chart
ggplot(crlogs_long, aes(date, delta_chart)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(data = subset(crlogs_long, deltabck == 0 & deltafwd == 0), aes(date, delta_chart), col = "green", size = 5, pch = 4) +
  geom_point(data = subset(crlogs_long, mas3sd == "yes"), aes(date, delta_chart), col = "purple", size = 4, pch = 1) +
  geom_point(data = subset(crlogs_long, lagflag == "yes"), aes(date, delta_chart), col = "blue", size = 3, alpha = 0.5) +
  geom_point(aes(col = deltachart_flag), alpha = 0.5) +
  scale_color_manual(values = c("yes" = "red", "no" = "black"))

# beginning of new sensor implementation in 2000 looks awful.. the rest not too bad? gap around 0 line is due to directional differences in deltas for tmin and tmax
ggplot(crlogs_long, aes(date, delta_jen)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(data = subset(crlogs_long, deltabck == 0 & deltafwd == 0), aes(date, delta_jen), col = "green", size = 5, pch = 4) +
  geom_point(data = subset(crlogs_long, mas3sd == "yes"), aes(date, delta_jen), col = "purple", size = 4, pch = 1) +
  geom_point(data = subset(crlogs_long, lagflag == "yes"), aes(date, delta_jen), col = "blue", size = 3, alpha = 0.5) +
  geom_point(aes(col = deltajen_flag), alpha = 0.5) +
  scale_color_manual(values = c("yes" = "red", "no" = "black"))



## -- STACK ALL LOGGER DATA ------
# combine all to plot out for KNS and SCE to look at...
logdat_all <- mutate(dp211_long, logger = "dp211") %>%
  dplyr::select(colnames(crlogs_long)) %>%
  rbind(crlogs_long)


# plot all logger deltas with sdl chart
ggplot(logdat_all, aes(date, delta_chart)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(data = subset(logdat_all, deltabck == 0 & deltafwd == 0), aes(date, delta_chart), col = "green", size = 5, pch = 4) +
  geom_point(data = subset(logdat_all, mas3sd == "yes"), aes(date, delta_chart), col = "purple", size = 4, pch = 1) +
  geom_point(data = subset(logdat_all, lagflag == "yes"), aes(date, delta_chart), col = "blue", size = 3, alpha = 0.5) +
  geom_point(aes(col = deltachart_flag), alpha = 0.5) +
  scale_color_manual(values = c("yes" = "red", "no" = "black"))

# beginning of new sensor implementation in 2000 looks awful.. the rest not too bad?
ggplot(logdat_all, aes(date, delta_jen)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(data = subset(logdat_all, deltabck == 0 & deltafwd == 0), aes(date, delta_jen), col = "green", size = 5, pch = 4) +
  geom_point(data = subset(logdat_all, mas3sd == "yes"), aes(date, delta_jen), col = "purple", size = 4, pch = 1) +
  geom_point(data = subset(logdat_all, lagflag == "yes"), aes(date, delta_jen), col = "blue", size = 3, alpha = 0.5) +
  geom_point(aes(col = deltajen_flag), alpha = 0.5) +
  scale_color_manual(values = c("yes" = "red", "no" = "black")) +
  facet_wrap(~met)

# plot logger temp against chart temp
ggplot(logdat_all, aes(val, chart_temp)) +
  geom_point(aes(col = logger), alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0), col = "blue") +
  scale_color_viridis_d() + 
  facet_wrap(logger~met, scales = "free")

# plot logger temp against chart temp
ggplot(logdat_all, aes(val, jenval)) +
  geom_point(aes(col = logger), alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0), col = "blue") +
  scale_color_viridis_d() + 
  facet_wrap(logger~met, scales = "free")

# plot logger temps over time, look for obvious breaks/steps in values based on change in logger (i.e. dp211 -> cr23x -> cr1000)
sdltemp_temporal <- ggplot(logdat_all, aes(date, val)) +
  geom_point(aes(col = logger), alpha = 0.5) +
  scale_color_viridis_d(option = "B") + 
  facet_wrap(~met) +
  theme_dark() +
  labs(y = "Daily temperature (°C)",
       x = "Date",
       title = "Daily Tmax and Tmin at the Saddle, colored by data logger source",
       subtitle = "Tmin range comparable over time between loggers, Tmax range discrepancy for DP211 vs CR loggers")


ggsave("extended_summer/figs/sdl_loggertemp_1990-ongoing.pdf", sdltemp_temporal,
       scale = 2)


#compare dp211 data against jennings infilled and raw data source for jennings et al
# min temp comparison
ggplot(data = jenraw_daily, aes(date, airtemp_min)) +
  geom_point(alpha = 0.5) +
  geom_point(data = jendaily, aes(date, airtemp_min), col = "salmon", alpha = 0.4) +
  geom_point(data = dp211, aes(date, airtemp_min), col = "navajowhite", alpha = 0.3) +
  geom_point(data = crlogs, aes(date, airtemp_min), col = "white", alpha = 0.3) +
  theme_dark()
# max temp comparison
ggplot(data = jenraw_daily, aes(date, airtemp_max)) +
  geom_point(alpha = 0.5) +
  geom_point(data = jendaily, aes(date, airtemp_max), col = "salmon", alpha = 0.4) +
  geom_point(data = dp211, aes(date, airtemp_max), col = "navajowhite", alpha = 0.3) +
  #geom_point(data = crlogs, aes(date, airtemp_max), col = "white", alpha = 0.3) +
  theme_dark()



## -- PROJECT SADDLE CHART USING CR LOGGERS -----
# 2019-06-04: After reviewing temp discrepancies, KNS suggests using saddle loggers and projects 1980s chart data to logger values (use most recent logger)
# for dp211.. can try standardize max temp? [but that will mess up temp threshold tallies]
# > SCE suggests dropping dp211 sensor data, and using only saddle chart and CR logger data (project 1982-2000 chart data)
#!! use NWT renewal chart data bc (assuming) prepped by tim kittel's methodology?, and only need to use until CR 1000 comes in
  
# combine nsf nwt temp data and cr1000 logger data
cr1000nsf_df <- rename(nwt_temps, airtemp_min = TMIN, airtemp_max = TMAX) %>%
  gather(met, nsfval, airtemp_min, airtemp_max) %>%
  full_join(subset(crlogs_long, logger == "cr1000")) %>%
  mutate(mon = as.factor(month(`date`)))

ggplot(cr1000nsf_df, aes(val, nsfval)) +
  geom_point(alpha = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1), col = "red") +
  facet_wrap(~ met, scales = "free")

# simple regressions..
nsfcr1000tmax_lm <- lm(val ~ mon + nsfval, data = subset(cr1000nsf_df, met == "airtemp_max"))
summary(nsfcr1000tmax_lm)  
plot(nsfcr1000tmax_lm)

nsfcr1000tmin_lm <- lm(val ~ mon + nsfval, data = subset(cr1000nsf_df, met == "airtemp_min"))
summary(nsfcr1000tmin_lm) 
plot(nsfcr1000tmin_lm)

nsfcr1000tminsimple_lm <- lm(val ~ nsfval, data = subset(cr1000nsf_df, met == "airtemp_min"))
summary(nsfcr1000tminsimple_lm) 
plot(nsfcr1000tminsimple_lm)

anova(nsfcr1000tmin_lm, nsfcr1000tminsimple_lm) #including month is better

# predict sdl chart values based on simple regression
predtmax <- predict.lm(nsfcr1000tmax_lm, newdata = subset(cr1000nsf_df, met == "airtemp_max"), se.fit = T, interval = "prediction")
predtmin <- predict.lm(nsfcr1000tmin_lm, newdata = subset(cr1000nsf_df, met == "airtemp_min"), se.fit = T, interval = "prediction")

newdat_tmax <- subset(cr1000nsf_df, met == "airtemp_max") %>%
  dplyr::select(date,year,mon, jday, met, logger, val, nsfval) %>%
  cbind(data.frame(predtmax$fit, se = predtmax$se.fit))
#colnames(newdat)[colnames(newdat) %in% c("fit", "upr", "lwr", "se")] <- paste0("TMAX", colnames(newdat)[grepl("fit|lwr|upr|se", colnames(newdat))])
newdat_tmin <- subset(cr1000nsf_df, met == "airtemp_min") %>%
  dplyr::select(date,year,mon, jday, met, logger, val, nsfval) %>%
  cbind(data.frame(predtmin$fit, se = predtmin$se.fit))

newdat <- rbind(newdat_tmax, newdat_tmin) %>%
  as.data.frame() %>%
  mutate(year = year(`date`),
         jday = yday(`date`)) %>%
  rename(doy = jday,
         cr1000_temp = val,
         nsf_chart_temp = nsfval,
         PIupr = upr,
         PIlwr = lwr)

# look at prediction over time with interval
ggplot(newdat) +
  #geom_errorbar(aes(date, fit, ymax = PIupr, ymin = PIlwr)) +
  geom_point(aes(date, cr1000_temp), col = "dodgerblue", alpha = 0.5) +
  geom_point(aes(date, fit),alpha = 0.3) +
  facet_wrap(~met)

ggplot(subset(newdat, met == "airtemp_max")) +
  geom_errorbar(aes(doy, ymax = PIupr, ymin = PIlwr)) +
  geom_point(aes(doy, cr1000_temp), col = "dodgerblue", alpha = 0.5) +
  geom_point(aes(doy, fit),alpha = 0.3) +
  facet_wrap(~year)
#look at overlapping years only
ggplot(subset(newdat, met == "airtemp_max" & year %in% c(2012,2013,2014))) +
  geom_errorbar(aes(doy, ymax = PIupr, ymin = PIlwr), col = "chocolate2", alpha =0.8) +
  geom_point(aes(doy, cr1000_temp), col = "dodgerblue", alpha = 0.5) +
  geom_point(aes(doy, fit),alpha = 0.5, col = "navajowhite") +
  geom_point(data = subset(crlogs, year == 2012 & logger == "cr23x"), aes(jday, airtemp_max), col = "purple", alpha = 0.3) +
  facet_wrap(~year) +
  theme_gray()

ggplot(newdat, aes(nsf_chart_temp, cr1000_temp)) +
  geom_point(alpha = 0.5) +
  geom_point(data = subset(newdat, date > as.Date("2012-12-05")), aes(nsf_chart_temp, fit), col ="seagreen", alpha = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1), col ="blue") +
  facet_wrap(~met)



# write out and see what happens with PC scores
write.csv(newdat, "extended_summer/output_data/ctw/predict_cr1000temp_1982-ongoing.csv", row.names = F)








# test NEON sensor QA script on dp211
dp211temps <- dplyr::select(dp211, airtemp_min, airtemp_max)
dp211dates <- as.POSIXlt.Date(dp211$date)
source("extended_summer/neon_sensor_QA.R")
test <- def.plau(dp211temps, ts = dp211dates, RngMin = c(-35, -35), RngMax = c(25,25), TestNull = c(T,T), DiffStepMax = c(30,30), NumGap = c(3,3))

testdf <- dp211 %>%
  dplyr::select(date, jday, airtemp_max) %>%
  na.omit()
sptest <- smooth.spline(testdf$jday, testdf$airtemp_max)
ggplot(dp211, aes(jday, airtemp_max)) +
  geom_point(alpha = 0.5) +
  geom_line(data = data.frame(x = sptest$x, y = sptest$y), aes(x,y), col = "blue")

fit <- sreg(testdf$jday, testdf$airtemp_max)
SE<- data.frame(SE = fit$shat.GCV*sqrt(fit$diagA), jday = 1:366)
# 95% pointwise prediction intervals
Zvalue<-  qnorm(.0975)
upper<- fit$fitted.values + Zvalue* SE
lower<- fit$fitted.values - Zvalue* SE

ZBvalue<-  qnorm(1- .025/fit$N)

testdf <- left_join(testdf, SE)
testdf2 <- cbind(testdf, fit = fit$fitted.values)
testdf2$upper <- testdf2$fit + Zvalue * testdf2$SE
testdf2$lower <- testdf2$fit - Zvalue * testdf2$SE

testdf2$upperB<- testdf2$fit + ZBvalue* testdf2$SE
testdf2$lowerB<- testdf2$fit - ZBvalue* testdf2$SE

ggplot(testdf2, aes(jday, airtemp_max)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(jday, upper), col = "red") +
  geom_line(aes(jday, lower), col = "red") +
  geom_line(aes(jday, upperB), col = "blue") +
  geom_line(aes(jday, lowerB), col = "blue")
  geom_line(aes(fit$x, fit$y), col =  "pink")
#
# conservative, simultaneous Bonferroni bounds
#
ZBvalue<-  qnorm(1- .025/fit$N)
upperB<- fit$fitted.values + ZBvalue* SE
lowerB<- fit$fitted.values - ZBvalue* SE
#
# take a look

plot( fit$x, fit$y)
lines( fit$predicted, lwd=2)
matlines( fit$x, 
          cbind( lower, upper, lowerB, upperB), type="l", col=c( 2,2,4,4), lty=1)
title( "95 pct pointwise  and simultaneous intervals")


library(anomalize)


dp211 %>%
  dplyr::select(date, airtemp_min) %>%
  na.omit() %>%
  as_tibble() %>%
  time_decompose(airtemp_min, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
  plot_anomaly_decomposition()

dp211 %>% 
  #mutate(airtemp_min2 = ifelse(airtemp_min > -40, airtemp_min, NA)) %>%
  dplyr::select(date, airtemp_min) %>%
  filter(!is.na(airtemp_min)) %>%
  as_tibble() %>%
  time_decompose(airtemp_min) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)
