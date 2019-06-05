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


# -- REVIEW DATA -----
# review how dats read in
glimpse(dp211) # no flag cols
glimpse(crlogs) # flag cols present
glimpse(sdlchart) # flags cols present

# drop cols not needed in cr logger dataset (i.e. only need airtemp cols up thru avg airtemp)
crlogs <- crlogs[,1:12]

# convert dates in all to Date class
dp211$date <- as.Date(dp211$date, format = "%Y-%m-%d")
crlogs$date <- as.Date(crlogs$date, format = "%Y-%m-%d")
sdlchart$date <- as.Date(sdlchart$date, format = "%Y-%m-%d")

# unique values of flags? and frequency of their occurrence?
sapply(crlogs[grepl("flag", colnames(crlogs))], function(x) summary(as.factor(x))) # only n's, correspond to no flag
sapply(sdlchart[grepl("flag", colnames(sdlchart))], function(x) summary(as.factor(x))) # 246 tmax, 238 tmin "1" flags; 14 tmax, 16 tmin "2" flags
# saddle chart flags: 1 = infilled by regression; 2 = infilled by standard deviation (take sd through that day in all years available)




# -- VISUAL COMPARE TEMP DATASETS -----
# visualize infilled sdl chart values
ggplot(sdlchart, aes(yday(date), airtemp_min, col = as.factor(flag_airtemp_min))) +
  geom_point(alpha = 0.5) +
  ggtitle("sdl chart tmin; colors = infilled method by NWT (1, regression; 2, sd; NA, no infill done)") +
  facet_wrap(~year(date)) #1984 tmin infilled vals look low relative to entire record..
ggplot(sdlchart, aes(yday(date), airtemp_max, col = as.factor(flag_airtemp_max))) +
  geom_point(alpha = 0.5) +
  ggtitle("sdl chart tmin; colors = infilled method by NWT (1, regression; 2, sd; NA, no infill done)") +
  facet_wrap(~year(date))


# -- VISUAL QA DP211 DATASET ----
# time series with NA vals infilled to visualize where data absent
dp211 %>%
  gather(met, val, airtemp_max:solrad_tot) %>%
  mutate(NAval = ifelse(is.na(val), "yes", "no"),
         val = ifelse(is.na(val), 0, val)) %>% # infill NAs with 0 to see missing
  ggplot(aes(date, val, col = NAval)) +
  geom_point(alpha = 0.5) + # clearly some bad values (sensor fail?), but not too many
  scale_color_manual(values = c("yes" = "red", "no" = "black")) +
  facet_wrap(~met)

# look at airtemp max and min more closely by year
ggplot(dp211, aes(jday,airtemp_max)) +
  geom_line() +
  #geom_point(alpha = 0.5) +
  facet_wrap(~year, scales = "free_y")

# make dp211 data long form with flags for plotting
dp211_long <- dp211 %>%
  mutate(mon = month(date)) %>%
  dplyr::select(-c(airtemp_avg, solrad_tot)) %>%
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
         lagflag = ifelse(abs(deltabck) > lag_thresh, "yes", "no"))

# plot daily values, highlight values 3sds outside monthly mean
ggplot(subset(dp211_long, met == "airtemp_min"), aes(jday,val)) +
  geom_line() +
  geom_point(data = subset(dp211_long, met == "airtemp_min" & mas3sd == "yes" & lagflag %in% c(NA,"yes")), aes(jday, val), col ="red", alpha = 0.5) + 
  labs(title = "Saddle DP211 logger: TMIN, red pts = more than 3sd from monthly grand mean",
       y = "TMIN (°C)", x = "Day of year") +
  facet_wrap(~year)
ggplot(subset(dp211_long, met == "airtemp_max"), aes(jday,val)) +
  geom_line() +
  geom_point(data = subset(dp211_long, met == "airtemp_max" & mas3sd == "yes" & leadflag == "yes" & lagflag == "yes"), aes(jday, val), col ="red", alpha = 0.5) + 
  labs(title = "Saddle DP211 logger: TMAX, red pts = more than 3sd from monthly grand mean",
       y = "TMAX (°C)", x = "Day of year") +
  facet_wrap(~year)

# day-to-day differences
ggplot(dp211_long, aes(date, day1delta)) +
  geom_line() +
  geom_point(data = subset(dp211_long, lag_mas3sd == "yes"), aes(date, day1delta), col ="red", alpha = 0.5) + 
  facet_wrap(~met, scales = "free_y")
ggplot(subset(dp211_long, met == "airtemp_max"), aes(jday, deltafwd)) +
  geom_line() +
  geom_point(data = subset(dp211_long, met == "airtemp_max" & leadflag == "yes" & lagflag == "yes"), aes(jday, deltafwd), col ="red", alpha = 0.5) + 
  facet_wrap(~year)
ggplot(subset(dp211_long, met == "airtemp_min"), aes(jday, day1delta)) +
  geom_line() +
  geom_point(data = subset(dp211_long, met == "airtemp_min" & lag_mas3sd == "yes"), aes(jday, day1delta), col ="red", alpha = 0.5) + 
  facet_wrap(~year)

# monthly boxplots
dp211 %>%
  dplyr::select(-solrad_tot) %>%
  gather(met, val, airtemp_max:airtemp_avg) %>%
  ggplot(aes(month(date), val, group = month(date))) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5, width = 0.2) + # clearly some bad values (sensor fail?), but not too many
  scale_x_continuous(breaks = seq(1,12,1)) +
  facet_wrap(~met) # bad values in airtemp min, airtemp max, probably airtemp avg (in april, but check against saddle)
# jan and dec values also look a little funky (appears some jan dates comparably warm to spring days?), but since ext sum PCA doesn't rely on jan or dec values, not a concern to address

# cr data loggers
crlogs[!grepl("flag", colnames(crlogs))] %>% # all flags are "n"
  gather(met, val, airtemp_max, airtemp_min,airtemp_avg) %>%
  mutate(NAval = ifelse(is.na(val), "yes", "no"),
         val = ifelse(is.na(val), 0, val)) %>% # infill NAs with 0 to see missing
  ggplot(aes(date, val, col = NAval)) +
  geom_point(alpha = 0.5) + # clearly some bad values (sensor fail?), but not too many
  scale_color_manual(values = c("yes" = "red", "no" = "black")) +
  facet_wrap(~met) # 1 bad value in airtemp_min (after 2015)
# monthly boxplots
crlogs[!grepl("flag", colnames(crlogs))] %>% # all flags are "n"
  gather(met, val, airtemp_max, airtemp_min,airtemp_avg) %>%
  ggplot(aes(month(date), val, group = month(date))) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, width = 0.2, col = "dodgerblue") +
  scale_x_continuous(breaks = seq(1,12,1)) +
  facet_wrap(~met) 


# combine logger temp datasets for comparison with saddle
sdllong <- sdlchart %>%
  gather(metric, chart_temp, airtemp_max:airtemp_avg)
sdlflags <- subset(sdllong, grepl("flag", metric)) %>%
  rename(chart_flag = chart_temp) %>%
  mutate(metric = gsub("flag_", "", metric))
sdllong <- subset(sdllong, !grepl("flag", metric)) %>%
  left_join(sdlflags)

logdat_all <- mutate(dp211, logger = "dp211") %>%
  dplyr::select(logger, date:airtemp_avg) %>%
  rbind(dplyr::select(crlogs, logger:airtemp_max, airtemp_min, airtemp_avg)) %>%
  gather(metric, logger_temp, airtemp_max:airtemp_avg)

logdat_all %>%
  left_join(sdllong) %>%
  ggplot(aes(logger_temp, chart_temp)) +
  geom_point(aes(col = logger), alpha = 0.5) +
  geom_abline(aes(slope = 1, intercept = 0), col = "blue") +
  scale_color_viridis_d() + 
  facet_wrap(logger~metric, scales = "free")
  

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
