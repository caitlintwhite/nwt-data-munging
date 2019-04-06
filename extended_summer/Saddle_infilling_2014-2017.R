#' ---
#' title: Update NWT LTER climate dataset and "Extended Summer" PCA
#' author: CTW
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' 
#'
#' **NOTE: THIS IS A QUICK INFILL JOB. Not precise.** 

library(tidyverse)
library(lubridate)

#+ r setup
# read in NWT climate dataset used in NSF proposal
# > read in from EDI data portal
Saddletemp <- read_csv("https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-nwt.413.10&entityid=afc48b6fab15649c9f91a9367debd2e0",
                       trim_ws=TRUE, na = c("", "NA", ".", "NaN"))
tail(Saddletemp) #goes through 12/31/2017 but NA values present
glimpse(Saddletemp)

# need to add in 9/1/2014 - 8/30/2017
Saddletemp_20142017 <- Saddletemp %>%
  filter(date %in% as.Date("2014-09-01"):as.Date("2017-08-30")) %>%
  mutate(yr = year(date),
         mon = month(date),
         ecoyear = ifelse(mon %in% 9:12, yr+1, yr))

Saddle_loggerdat <- read_csv("http://niwot.colorado.edu/data_csvs/sdlcr23x-cr1000.daily.ml.data.csv",
                             na = c("", "NA", "Nan")) %>%
  filter(date %in% as.Date("2014-09-01"):as.Date("2017-08-30"))
  
# how many NAs in temp fields?
summary(Saddletemp_20142017)
# NAs by month and year
missingtemps <- Saddletemp_20142017%>%
  dplyr::select(date, airtemp_max, airtemp_min, airtemp_avg) %>%
  gather(metric, value, airtemp_max:airtemp_avg) %>%
  mutate(mon = month(date),
         yr = year(date),
         missing = is.na(value)) %>%
  filter(missing == TRUE) %>%
  group_by(yr, mon, metric) %>%
  summarise(ct_missing = length(missing))

left_join(missingtemps, Saddletemp_20142017[c("yr", "mon", "ecoyear")]) %>%
  filter(mon %in% 6:8) %>%
ggplot(aes(mon, ct_missing)) +
  geom_point() +
  #scale_x_continuous(breaks = seq(2,12, 2)) +
  facet_grid(ecoyear~metric, scales = "free_x") # sad face.. will need to infill

# check Saddle precip data for issues..
Saddleprecip <- read_csv("https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-nwt.416.8&entityid=c1c73287f2e10039c0d89dba8fd44993",
                         trim_ws = TRUE,
                         na = c("", "NA", "NaN"))

Saddleprecip_20142017 <- Saddleprecip %>%
  filter(date %in% as.Date("2014-09-01"):as.Date("2017-08-30")) %>%
  mutate(yr = year(date),
         mon = month(date),
         ecoyear = ifelse(mon %in% 9:12, yr+1, yr))

# how many NAs in precip fields?
summary(Saddleprecip_20142017)
# NAs by month and year
missingprecip <- Saddleprecip_20142017%>%
  dplyr::select(date, ppt_tot) %>%
  mutate(mon = month(date),
         yr = year(date),
         missing = is.na(ppt_tot)) %>%
  filter(missing == TRUE) %>%
  group_by(yr, mon) %>%
  summarise(ct_missing = length(missing))

left_join(missingprecip, Saddleprecip_20142017[c("yr", "mon", "ecoyear")]) %>%
  filter(mon %in% 6:8) %>%
ggplot(aes(mon, ct_missing)) +
  geom_point() +
  #scale_x_continuous(breaks = seq(2,12, 2)) +
  facet_grid(ecoyear~., scales = "free_x") # sad face.. will need to infill

# ID missing dates to see if data available in D1 or C1
temp_missing <- Saddletemp_20142017$date[is.na(Saddletemp_20142017$airtemp_avg)]
precip_missing <- Saddleprecip_20142017$date[is.na(Saddleprecip_20142017$ppt_tot)]

# read in C1 and D1 to compare for quick infilling
# all chart data
C1temp <- read_csv("http://niwot.colorado.edu/data_csvs/c-1tdayv.ml.data.csv",
                   na = c("", "NA", "NaN")) %>%
  filter(date %in% as.Date("2014-09-01"):as.Date("2017-08-30"))
C1precip <- read_csv("http://niwot.colorado.edu/data_csvs/c-1pdayv.ml.data.csv",
                     na = c("", "NA", "NaN")) %>%
  filter(date %in% as.Date("2014-09-01"):as.Date("2017-08-30"))

D1temp <- read_csv("http://niwot.colorado.edu/data_csvs/d-1tdayv.ml.data.csv",
                   na = c("", "NA", "NaN")) %>%
  filter(date %in% as.Date("2014-09-01"):as.Date("2017-08-30"))
D1precip <- read_csv("http://niwot.colorado.edu/data_csvs/d-1pdayv.ml.data.csv",
                     na = c("", "NA", "NaN")) %>%
  filter(date %in% as.Date("2014-09-01"):as.Date("2017-08-30"))

# how much can C1 temp help?
summary(temp_missing %in% C1temp$date[is.na(C1temp$airtemp_avg)==FALSE]) # available 119/142 days
# how much can D1 temp help?
summary(temp_missing %in% D1temp$date[is.na(D1temp$airtemp_avg)==FALSE]) # available 131/142 days

# how much can C1 precip help?
summary(precip_missing %in% C1precip$date[is.na(C1precip$ppt_tot)==FALSE]) # available 135/141 days
# how much can D1 precip help?
summary(precip_missing %in% D1precip$date[is.na(D1precip$pppt_tot)==FALSE]) #available 124/141 days

cor(C1precip$ppt_tot, Saddleprecip_20142017$ppt_tot, use = "complete.obs")
cor(D1precip$pppt_tot, Saddleprecip_20142017$ppt_tot, use = "complete.obs")
cor(C1temp$airtemp_avg, Saddletemp_20142017$airtemp_avg, use = "complete.obs")
cor(D1temp$airtemp_avg, Saddletemp_20142017$airtemp_avg, use = "complete.obs")

par(mfrow=c(3,1))
plot(D1precip$date, D1precip$pppt_tot)
plot(Saddleprecip_20142017$date, Saddleprecip_20142017$ppt_tot)
plot(C1precip$date, C1precip$ppt_tot)



### need to infill
# 9 days temp July 2015
# 10 days temp Aug 2015
# 6 days precip July 2015 (metadata says total ppt for those days is 0.85mm)
# 3 days July 2017

# methods for infilling temp on NWT website:
# (Method flag 1) regression methodology:
# Regression was performed using the chart recorder data at the missing site and the adjacent site using a 14 day window prior to and after the missing day. 
# The regression equation was applied to the value at the known site to determine the value for infilling the missing site. An r2 value above 0.6 was considered acceptable. 
# If the r2 was below 0.6 or missing, the standard deviation method was used to fill in the missing value (see Method Flag 2).

#(Method flag 2) standard deviation methodology:
#  Ratios of known site values and standard deviations were used in order to determine the replacement values. The standard deviation was taken for the day (mm-dd) in question throughout the climate record at both the known site and the unknown site and applied as follows:
  #
  #     known site date value (yyyy-mm-dd)                                                   (x)
  # ______________________________________________  :  __________________________________________________
  # std. dev. of the day (mm-dd) at the known site                        std. dev. of the day (mm-dd) at the unknown site
  #

# Order of adjacent sites used:
# Temperature data adjacency hierarchy (if value missing from charts):
# (1) Saddle dp211 data logger
# (2) Saddle cr21x data logger
# (3) D1 chart recorder

# some other notes from metadata
# 2015-06-29    Max and min only before 1000
# 2015-06-30    Max and min only after 0930
# clock issues 2015-07-07 through 2015-08-10
# 2015-08-11    Max and min only after 0800
# -----------------------
# compare Saddle chart temp and logger temp for summer months
Saddle_compare <- Saddle_loggerdat %>%
  mutate(mon = month(date)) %>%
  dplyr::select(date, year, mon, jday, airtemp_max, airtemp_min, airtemp_avg) %>%
  rename(logger_airmax = airtemp_max,
         logger_airmin = airtemp_min,
         logger_airmean = airtemp_avg,
         yr = year) %>%
  left_join(Saddletemp_20142017[c("date", "airtemp_max", "airtemp_min", "airtemp_avg", "yr", "mon", "ecoyear")]) %>%
  rename(chart_airmax = airtemp_max,
         chart_airmin = airtemp_min,
         chart_airmean = airtemp_avg)

plot(Saddle_compare$logger_airmean, Saddle_compare$chart_airmean)
cor.test(Saddle_compare$logger_airmean, Saddle_compare$chart_airmean, use="complete.obs")

Saddle_compare %>%
  gather(metric, value, c(logger_airmax:chart_airmean)) %>%
  mutate(source = ifelse(grepl("logger",metric)==TRUE, "logger","chart")) %>%
  ggplot(aes(date, value)) +
  geom_point(alpha=0.5) +
  facet_grid(.~source) +
  theme_light()

Saddle_compare %>%
  na.omit() %>%
  gather(metric, value, c(logger_airmax:chart_airmean)) %>%
  mutate(source = ifelse(grepl("logger",metric)==TRUE, "logger","chart"),
         metric = gsub("logger_|chart_", "", metric)) %>%
  filter(mon %in% 6:8) %>%
  ggplot(aes(jday, value, col = source)) +
  geom_point(alpha=0.5) +
  geom_smooth() +
  facet_grid(ecoyear~metric, scales = "free_x") +
  theme_light()
# looks like 2015 chart data parallel trends in 2015 logger data well, but not as much in 2016
# okay for this purpose since I only need to infill 2015 dates in July and Aug
# logger data is reliably above chart data in 2015
  
# infill temp using logger data via regression method (28 days window) if possible
sum_temp_missing <- temp_missing[month(temp_missing) %in% 6:8]
# a whole string of dates July-Aug missing, so 14 day above/below missing date not possible
# will try linear relationship between dates over entire summer

lm_meantemp2015 <- lm(chart_airmean ~ logger_airmean, 
                  data = subset(Saddle_compare, yr == 2015 & mon %in% 6:8))
summary(lm_meantemp2015)
# r^2 is > 0.6 so good enough

lm_mintemp2015 <- lm(chart_airmin ~ logger_airmin, 
                      data = subset(Saddle_compare, yr == 2015 & mon %in% 6:8))
summary(lm_mintemp2015)
# r^2 is > 0.6 so good enough

lm_maxtemp2015 <- lm(chart_airmax ~ logger_airmax, 
                      data = subset(Saddle_compare, yr == 2015 & mon %in% 6:8))
summary(lm_maxtemp2015)
# r^2 is > 0.6 so good enough

Saddle_compare <- Saddle_compare %>%
  mutate(airmax_infill = ifelse(is.na(chart_airmax)==TRUE, 
                                 round((lm_maxtemp2015$coefficients[[1]] + lm_maxtemp2015$coefficients[[2]]*logger_airmax),1),
                                 chart_airmax),
         airmin_infill = ifelse(is.na(chart_airmin)==TRUE, 
                                 round((lm_mintemp2015$coefficients[[1]] + lm_mintemp2015$coefficients[[2]]*logger_airmin),1),
                                 chart_airmin),
         airmean_infill = ifelse(is.na(chart_airmean)==TRUE, 
                                 round((lm_meantemp2015$coefficients[[1]] + lm_meantemp2015$coefficients[[2]]*logger_airmean),1),
                                 chart_airmean))
# check how infill looks visually
Saddle_compare %>%
  dplyr::select(date, yr, mon, jday, ecoyear, logger_airmax:airmin_infill) %>%
  gather(metric, value, c(logger_airmax:airmin_infill)) %>%
  mutate(source = ifelse(grepl("logger",metric)==TRUE, "logger",
                         ifelse(grepl("chart",metric)==TRUE, "chart", "infill")),
         metric = gsub("logger_|chart_|_infill", "", metric)) %>%
  filter(mon %in% 6:8 & yr == 2015) %>%
  mutate(infilled = date %in% sum_temp_missing) %>%
  ggplot(aes(jday, value)) +
  geom_point(aes(col=infilled)) +
  geom_smooth(aes(jday, value), method="lm", col="black") +
  facet_grid(metric~source, scales = "free") +
  theme_light()


## troubleshoot precip...
# put all precip data together in common data frame
precip_all <- Saddleprecip_20142017 %>%
  dplyr::select(date, yr, mon, ecoyear, ppt_tot) %>%
  rename(saddle_ppt = ppt_tot) %>%
  left_join(D1precip[c("date", "pppt_tot")], by =c("date")) %>%
  rename(D1_ppt = pppt_tot) %>%
  left_join(C1precip[c("date", "ppt_tot")], by =c("date")) %>%
  rename(C1_ppt = ppt_tot)

precip_all %>%
  mutate(doy = yday(date)) %>%
  filter(mon %in% 6:8) %>%
  gather(site, ppt, saddle_ppt:C1_ppt) %>%
  ggplot(aes(doy, ppt)) +
  geom_point(alpha = 0.5) +
  facet_grid(yr~site, scales = "free_y") +
  theme_light()

# methods from metadata
# (Method Flag 1) Daily Ratio Methodology:
# Total precipitation for the date in question was calculated for the the period 1981-2008, for both the 'Known Site' and the 'Unknown Site', only for days where daily values existed for both sites (missing values and QD's > 1 were removed). 
# A ratio was determined [Unknown Site : Known Site] based on these totals.  
# This ratio was then multiplied by the value for that date at the 'Known Site' in order to determine the 'Unknown Site' value.  
# Precipitation data adjacency hierarchy:
  #SDL:
  #1) D1
  #2) C1

# this requires going back through all years with complete precip obs and qdays of 1

D1precip_all <- read_csv("http://niwot.colorado.edu/data_csvs/d-1pdayv.ml.data.csv",
                     na = c("", "NA", "NaN")) %>%
  rename(ppt_tot = pppt_tot)

precip_compare <- rbind(D1precip_all, Saddleprecip) %>%
  filter(qdays==1) %>%
  gather(metric, value, ppt_tot:flag_ppt_tot) %>%
  unite(varname, local_site, metric) %>%
  spread(varname, value) %>%
  filter(is.na(d1_ppt_tot)==FALSE & is.na(sdl_ppt_tot)==FALSE) %>%
  mutate(doy=yday(date)) %>%
  group_by(doy) %>%
  summarise(d1_ppt = sum(d1_ppt_tot),
            sdl_ppt = sum(sdl_ppt_tot)) %>%
  mutate(ppt_ratio = sdl_ppt/d1_ppt)

precip_all <- precip_all %>%
  mutate(doy = yday(date)) %>%
  left_join(precip_compare[c("doy", "ppt_ratio")]) %>%
  mutate(sdl_infill = ifelse(is.na(saddle_ppt), D1_ppt*ppt_ratio, saddle_ppt))

precip_all %>%
  dplyr::select(date, yr, mon, doy, saddle_ppt, sdl_infill, D1_ppt) %>%
  gather(site, ppt, saddle_ppt:D1_ppt) %>%
  filter(mon %in% 6:8) %>%
  mutate(infilled = date %in% precip_missing) %>%
  ggplot(aes(doy, ppt)) +
  geom_point(aes(col = infilled), alpha=0.6) +
  facet_grid(yr~site, scales = "free_y") +
  theme_light()

precip_all %>%
  dplyr::select(date, yr, mon, doy, saddle_ppt, sdl_infill, D1_ppt) %>%
  gather(site, ppt, saddle_ppt:D1_ppt) %>%
  filter(mon %in% 6:8) %>%
  group_by(yr, mon, site) %>%
  summarise(tot_ppt = sum(ppt, na.rm=TRUE)) %>%
  ggplot(aes(mon, tot_ppt)) +
  geom_point(alpha=0.6) +
  facet_grid(yr~site, scales = "free_y") +
  scale_x_continuous(breaks=seq(6,8,1)) +
  theme_light()

## put together infilled summer temp and precip dataset for 2015-2017
# create summer temperate df first, with infilled data from 2015, 2016; add in 2017 chart data (complete record)
sdl_temp_sum1517 <- Saddle_compare %>%
  dplyr::select(date, yr, mon, ecoyear, airmax_infill, airmin_infill, airmean_infill) %>%
  rename(airtemp_max = airmax_infill, airtemp_min = airmin_infill, airtemp_avg = airmean_infill) %>%
  rbind(Saddletemp_20142017[Saddletemp_20142017$yr == 2017,c("date", "yr", "mon", "ecoyear", "airtemp_max", "airtemp_min", "airtemp_avg")]) %>%
  # rename vars to match Emily's code
  rename(min_temp = airtemp_min, max_temp = airtemp_max, mean_temp = airtemp_avg,
         year = yr, month = mon) %>%
  #left_join(precip_all[c("date", "yr", "mon", "ecoyear","sdl_infill")], by = c("date", "yr", "mon", "ecoyear")) %>%
  #mutate(ppt_tot = round(sdl_infill,1)) %>%
  dplyr::select(-ecoyear)
         

# read in Emily's infilled dataset to compare temp values (only overlaps Sep-Dec August 2014 for the moment..)
temp_precip_for_sm <- read_csv("~/Dropbox/NWT_data/Saddle_precip_temp_formoisturedeficit.csv")

test <- sdl_tempppt_sum1517 %>%
  mutate(dy = day(date)) %>%
  inner_join(temp_precip_for_sm, by = c("yr"="Year", "mon"="Month", "dy" = "Day")) %>%
  mutate(snow_adj_ppt = ppt_tot * 0.39)

test2 <- test %>%
  gather(metric, value, c(airtemp_max:ppt_tot, TMIN:snow_adj_ppt)) %>%
  mutate(category = ifelse(grepl("TMIN|TMAX|PCP", metric), "Emily", "CTW"))

ggplot() +
  geom_point(data = subset(test2, metric %in% c("PCP", "snow_adj_ppt")), aes(`date`, value, col = category), alpha=0.6) +
  geom_point(data = subset(test2, metric %in% c("PCP", "snow_adj_ppt") & `date` %in% precip_missing),
             aes(date, value), pch=1, col = "black")
  facet_wrap(~category)

test3 <- test2 %>%
  filter(metric %in% c("TMAX", "TMIN", "airtemp_min", "airtemp_max")) %>%
  mutate(metric = ifelse(grepl("max", metric, ignore.case = T)==TRUE, "TMAX", "TMIN"))
  
ggplot() +
  geom_point(data = test3, aes(test3$date, value, col = category), alpha=0.6) +
  geom_point(data= subset(test3, test3$date %in% temp_missing), aes(date, value), pch=1, col="black", size=1.5) +
  facet_grid(metric~., scales = "free_y")

nwtchart_emily_temp <- Saddletemp %>%
  mutate(yr = year(date),
         mon = month(date),
         Day = day(date)) %>%
  full_join(Saddleprecip) %>%
  full_join(temp_precip_for_sm, by = c("yr"="Year", "mon" = "Month", "Day"))
  #filter(!is.na(flag_airtemp_min))

ggplot(subset(test, yr>=2010)) +
  geom_point(aes(date, airtemp_max), col = "dodgerblue", size=2) +
  geom_point(aes(date, TMAX), col = "goldenrod", size =1) +
  theme_bw()

nwtchart_emily_temp %>%
  mutate(chartppt = ifelse(mon %in% 6:8, ppt_tot, ppt_tot*0.39)) %>%
  filter(yr %in% 2009:2014 & qdays>1) %>%
ggplot() +
  geom_point(aes(date, chartppt), col = "dodgerblue", size=2) +
  geom_point(aes(date, PCP), col = "goldenrod", size =1) +
  theme_bw()

# how does D1 chart data compare to infilled Jennings et al data?
Jennings_infill <- read_csv("http://niwot.colorado.edu/data_csvs/infillclimate_c1_d1_sdl.hourly.kj.data.csv")

Jennings_precip <- Jennings_infill %>%
  group_by(LTER_site, local_site, year, date,jday) %>%
  summarise(daily_ppt = round(sum(ppt_tot)))

test_Jennings <- Jennings_precip %>%
  #filter(local_site != "sdl") %>%
  full_join(nwtchart_emily_temp)
  #full_join(D1precip, by = c("LTER_site", "local_site", "date"))
  #full_join(C1precip, by = c("LTER_site", "local_site", "date"))

test_Jennings %>%
  filter(local_site == "sdl" & 
           year %in% 1990:2012 &
           qdays >1) %>%
  mutate(mon = month(date),
         chartppt = ifelse(mon %in% 6:8, ppt_tot, ppt_tot*0.39)) %>%
  ggplot() +
  geom_point(aes(date, daily_ppt), col = "seagreen", size=2) +
  geom_point(aes(date, chartppt), col = "goldenrod", size =1) +
  theme_bw()

Jennings_temp <- Jennings_infill %>%
  group_by(LTER_site, local_site, year, date,jday) %>%
  summarise(mean_temp = mean(airtemp_avg))

Jennings_temp %>%
  filter(local_site == "sdl") %>%
  left_join(Saddletemp) %>%
  filter(year == 2000) %>%
  ggplot() +
  geom_point(aes(mean_temp, airtemp_avg)) +
  #geom_point(aes(date, mean_temp), col = "seagreen", col=2) +
  #geom_point(aes(date, airtemp_avg), col = "goldenrod", col=1) +
  theme_bw()
