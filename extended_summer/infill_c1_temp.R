# infill c1 and d1 air temp...

source("edi_functions.R")
library(tidyverse)
library(lubridate)
options(stringsAsFactors = F)
theme_set(theme_bw())
d1 <- getTabular(412)
c1 <- getTabular(411)

#nwt renewal data  
nwt_temps <- read.csv("~/Dropbox/NWT_data/c1_infilled_daily_temp.csv") %>%
  mutate(`date` = as.Date(paste(Year,Month,Day, sep="-")),
         LTER_site = "NWT") %>%
         #local_site = "sdl") %>%
  dplyr::select(LTER_site, local_site, date, TMIN, TMAX) 

glimpse(d1)
glimpse(c1)
range(d1$date)



d1 %>%
  mutate(decade = ifelse(year(date) < 1960, 1950,
                         ifelse(year(date) %in% 1960:1969, 1960, 
                                ifelse(year(date) %in% 1970:1979, 1970,
                                       ifelse(year(date) %in% 1980:1989, 1980,
                                              ifelse(year(date) %in% 1990:1999, 1990,
                                                     ifelse(year(date) %in% 2000:2009, 2000, 2010))))))) %>%
  mutate(flag_airtemp_max = ifelse(is.na(flag_airtemp_max), "none", flag_airtemp_max)) %>%
  ggplot(aes(date, airtemp_max, group = year(date), col = flag_airtemp_max)) +
  #geom_line(alpha = 0.5) +
  geom_point(alpha = 0.4) +
  #scale_x_date(breaks = "6 months") +
  scale_colour_viridis_d(name = "infill method") +
  facet_wrap(~decade, scales = "free_x")
  

c1 %>%
  mutate(decade = ifelse(year(date) < 1960, 1950,
                         ifelse(year(date) %in% 1960:1969, 1960, 
                                ifelse(year(date) %in% 1970:1979, 1970,
                                       ifelse(year(date) %in% 1980:1989, 1980,
                                              ifelse(year(date) %in% 1990:1999, 1990,
                                                     ifelse(year(date) %in% 2000:2009, 2000, 2010))))))) %>%
  mutate(flag_airtemp_max = ifelse(is.na(flag_airtemp_max), "none", flag_airtemp_max)) %>%
  ggplot(aes(date, airtemp_max, group = year(date), col = flag_airtemp_max)) +
  #geom_line(alpha = 0.5) +
  geom_point(alpha = 0.4) +
  #scale_x_date(breaks = "6 months") +
  scale_colour_viridis_d(name = "infill method") +
  facet_wrap(~decade, scales = "free_x")

# number of NAs by month (x year?)
d1 %>%
  mutate(yr = year(date),
         mon = month(date, abbr = T)) %>%
  group_by(yr, mon, is.na(airtemp_min)) %>%
  summarise(naTMIN = length(airtemp_min)) %>%
  filter(`is.na(airtemp_min)`==TRUE) %>%
  ggplot(aes(mon, naTMIN, group = mon)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1,12,1)) +
  facet_wrap(~yr)

# number of NAs by month x year
c1 %>%
  dplyr::select(date, airtemp_max, airtemp_min) %>%
  gather(met, val, airtemp_max:airtemp_min) %>%
  mutate(yr = year(date),
         mon = month(date, abbr = T)) %>%
  group_by(yr, mon, met, is.na(val)) %>%
  summarise(na_cnt = length(val)) %>%
  filter(`is.na(val)`==TRUE) %>%
  ggplot(aes(mon, na_cnt, group = mon, col = met)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  facet_wrap(~yr)

# amount of infilling by month/yr
d1 %>%
  dplyr::select(date, flag_airtemp_max, flag_airtemp_min) %>%
  gather(met, method, flag_airtemp_max:flag_airtemp_min) %>%
  mutate(yr = year(date),
         mon = month(date, abbr = T)) %>%
  filter(!is.na(method)) %>%
  group_by(yr, mon, met, method) %>%
  summarise(infill_cnt = length(method)) %>%
  ggplot(aes(mon, infill_cnt, group = mon, shape = met, col = as.factor(method))) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  facet_wrap(~yr)
c1 %>%
  dplyr::select(date, flag_airtemp_max, flag_airtemp_min) %>%
  gather(met, method, flag_airtemp_max:flag_airtemp_min) %>%
  mutate(yr = year(date),
         mon = month(date, abbr = T)) %>%
  filter(!is.na(method)) %>%
  group_by(yr, mon, met, method) %>%
  summarise(infill_cnt = length(method)) %>%
  ggplot(aes(mon, infill_cnt, group = mon, shape = met, col = as.factor(method))) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  scale_x_continuous(breaks = seq(1,12,1)) +
  facet_wrap(~yr)
  
ggplot(subset(d1, year(date)> 2010), aes(yday(date), airtemp_max)) +
  geom_line() +
  geom_point(alpha = 0.4) +
  facet_wrap(~year(date))

ggplot(subset(c1, year(date)> 2010), aes(yday(date), airtemp_max)) +
  geom_line() +
  geom_point(alpha = 0.4) +
  facet_wrap(~year(date))
