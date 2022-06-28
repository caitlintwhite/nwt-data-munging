# test flag functions on D1 chart


# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN")

# set path to climdat folder
datpath <- "c1_d1_sdl_clim/homogenize_climdat/"
# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")
# functions to flag potentially errant values
source(paste0(datpath,"scripts/flagging_functions.R"))


# -- GET DATA -----
# relatively uncorrected datasets
# sdl chart
sdl <- getTabular(413) %>% data.frame()
# d1 chart
d1 <- getTabular(412) %>% data.frame() #temp
d1ppt <- getTabular(415) %>% data.frame() # ppt

# nwt site visit 2019 datasets
# d1 chart, kittel et al. infilled
d1_kittel <- getTabular(187) %>% data.frame()
d1_kittel_ppt <- getTabular(186) %>% data.frame()
# logger datasets
d1_log_daily <- getTabular(402) %>% data.frame()


# test sdl as well
sdlchart
sdl_cr21x

# sdl (since 2000)
sdl_log_daily <- getTabular(405) %>% data.frame()


# -- SCRATCH PAD -----
# flag physically implausible values (outside climate boundaries per J. Morse recommendations)
test <- flag_limits(d1, "airtemp_max", "airtemp_min", maxval = 20, minval = -30)
sdltest <- flag_limits(sdl, "airtemp_max", "airtemp_min", maxval = 20, minval = -30)

ppt_test <- flag_limits(d1ppt, "ppt_tot", maxval = 100, minval = 0)

check_datetime(d1)
check_datetime(d1ppt)
check_datetime(sdl)

# test flatlines
test <- flag_flatlines(d1, "airtemp_max")
test <- flag_flatlines(test, "airtemp_min")
test <- flag_flatlines(test, "airtemp_avg")

# test temporal spikes
test <- flag_spikes(d1, "airtemp_max", abs_limit = 20)
test$DTR <- test$airtemp_max - test$airtemp_min
test_tmin <- flag_spikes(test, "airtemp_min", returnworking = T)
test_dtr <- flag_spikes(test, "DTR", returnworking = T)

ggplot(test_tmin, aes(year(date), airtemp_min_change_std, group = year(date))) +
  geom_boxplot() +
  geom_point(aes(col = year(date)),position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0), alpha = 0.4) +
  scale_color_distiller(palette = "Reds")

mutate(test_tmin, season = ifelse(month(date) %in% c(11:12,1:5), "winter/spring", "summer/fall")) %>%
  ggplot(aes(season, airtemp_min_change_std, group = season)) +
  geom_boxplot() +
  geom_point(aes(col = year(date)), position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0)) +
  scale_color_distiller(palette = "Reds")

test_tmin <- merge(test_tmin, d1_kittel[c("date", "year", "min_temp", "source_station", "Tmin_QAflag")], all.x = T) %>%
  mutate(infilled = source_station != "D1",
         discrep = min_temp != airtemp_min)


# test deviation extreme flagging
test <- flag_deviations(d1, metric = "airtemp_max")
test <- flag_deviations(test, metric = "airtemp_min")
test$DTR <- test$airtemp_max - test$airtemp_min
#test <- flag_deviations(test, metric = "DTR")
test <- flag_deviations(test, metric = "DTR", sd_num = 5)
# note: gets funky if re-run same flagging function with different sd threshold. 
# consider removing col in function before re-calculating?
# or re-run with different colname appended?

# compare with probability of observing extreme val given sample mean and sd approach true pop params
xbar <- mean(d1$airtemp_max, na.rm = T)
s <- sd(d1$airtemp_max, na.rm = T)
maxval <- max(d1$airtemp_max, na.rm = T)
pnorm(maxval, mean = xbar, sd = s, lower.tail = F) #0.005
test[which(d1$airtemp_max == maxval),] # doesn't get flagged using sd threshold cutoff


# visualize dist to see where flagged values fall
ggplot(test) +
  geom_histogram(aes(airtemp_min), fill = "transparent", col = "grey50") +
  geom_vline(data = subset(test, !is.na(flag_grpstd_airtemp_min)), aes(xintercept = airtemp_min), col = "red") +
  facet_wrap(~mon, scales = "free_x")

ggplot(test) +
  geom_histogram(aes(airtemp_max), fill = "transparent", col = "grey50") +
  geom_vline(data = subset(test, !is.na(flag_std_airtemp_max)), aes(xintercept = airtemp_max, col = year(date)), lwd = 2, alpha = 0.75) +
  geom_vline(data = subset(test, !is.na(flag_grpstd_airtemp_max)), aes(xintercept = airtemp_max), col = "red") +
  facet_wrap(~mon, scales = "free_x")

ggplot(group_by(test, mon)) +
  geom_histogram(aes(scale(airtemp_min)), fill = "transparent", col = "grey50") +
  facet_wrap(~mon, scales = "free_x")

ggplot(test) +
  geom_histogram(aes(DTR), fill = "transparent", col = "grey50") +
  geom_vline(data = subset(test, !is.na(flag_std_DTR)), aes(xintercept = DTR, col = year(date)), lwd = 2, alpha = 0.75) +
  geom_vline(data = subset(test, !is.na(flag_grpstd_DTR)), aes(xintercept = DTR), col = "red") +
  facet_wrap(~mon, scales = "free_x")
# nothing looks unrealistic by itself of all these metrics.. temp swings more and distrib is less Gaussian in winter months
# tails for temp are longer left; DTR tail is long right

# test gathering flag and check columns, tidying dataset to transition to multi-station comparison
# run through full suite of checks
test <- flag_limits(d1,maxcol = "airtemp_max", mincol = "airtemp_min", maxval = 25, minval = -35)
test <- flag_flatlines(test, "airtemp_max", numdays = 5)
test <- flag_flatlines(test, "airtemp_min", numdays = 5)
test <- flag_spikes(test, "airtemp_max")
test <- flag_spikes(test, "airtemp_min")
test <- flag_deviations(test, metric = "airtemp_max")
tidytest <- dplyr::select(test, !c(airtemp_avg)) %>%
  gather(met, val, names(.)[!names(.) %in% c("LTER_site", "local_site", "date", "mon")])
# gather(metric, temp, airtemp_max, airtemp_min) %>%
# gather(nwtflag, nwtflag_val, flag_airtemp_max, flag_airtemp_min) %>%
# distinct()


## SDL -----
sdltemp <- sdl_log_daily[grepl("LTER|local|logger|date|year|jday|airtemp", names(sdl_log_daily))]

sdltemp_1920 <- subset(sdltemp, year > 2018)
summary(sdltemp_1920)
sdltemp1920_tidy <- sdltemp_1920 %>%
  gather(met, val, airtemp_max:ncol(.)) %>%
  mutate(type = ifelse(grepl("^flag", met), "flag", "airtemp"),
         met = gsub("flag_|airtemp_", "", met)) %>%
  spread(type, val) %>%
  mutate(instrument = ifelse(grepl("hmp", met), str_extract(met, "hmp[0-9]"), "logger"),
         met = gsub("hmp[0-9]_", "", met),
         airtemp = as.numeric(airtemp),
         met = factor(met, levels = c("max", "avg", "min"))) %>%
  #check that there is at least 1 obs per met per day
  group_by(date, met) %>%
  mutate(nobs = sum(!is.na(airtemp))) %>%
  ungroup()
str(sdltemp1920_tidy)
table(sdltemp1920_tidy$nobs) # there are enough obs per day to cover all dates

ggplot(sdltemp1920_tidy, aes(date, airtemp, col = factor(nobs))) +
  geom_point(alpha = 0.25) +
  geom_point(data = subset(sdltemp1920_tidy, nobs == 1), aes(date, airtemp), col = "red", alpha = 0.7, size = 2) +
  facet_grid(instrument~met)

sdldat <- read_csv("c1_d1_sdl_clim/homogenize_climdat/data/prep_data/crall1000hcn_ctw_2020.csv")
  