#' ---
#' title: Gap-fill temperature data with prev QC'd tempdat
#' author: CTW  
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#' 
#' ### Temperature gap-filling and post-infill review
#' 
#' Test report to:
#' 1. Read in prepared, qc'd NWT and neighbor station temperature datasets
#' 2. Stack all for cycling through infilling
#' 3. Run infill methods, choose best per TK methods
#' 4. Quick review to be sure no infill values violate qc flags
#' 5. Write out for homogenization
#' 
#' All code will be displayed to show procedure and work out bugs



# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)

source("nwt_climate/R/fetch_data_functions.R") # to read in prepped tidy datasets
source("nwt_climate/R/dataviz_functions.R")
source("nwt_climate/R/temp_infill_functions.R")

# set path to prepared data
datpath <- "~/Documents/nwt_lter/nwt_climate/data/"
# list all rds files in qc'd data folder (full path directory)
rdsfiles_qc <- list.files(paste0(datpath, "qc"), pattern = "rds", full.names = T)

# read in prepared precip data
charttemp <- get_tidydat("chartTEMP", rdsfiles_qc, "*")
logtemp <- get_tidydat("loggerTEMP", rdsfiles_qc, "*")
ameriflux <- get_tidydat("fluxT", rdsfiles_qc, "*")
snotel <- get_tidydat("telT", rdsfiles_qc, "*")
ghcnd <- get_tidydat("ghcndT", rdsfiles_qc, "*")
allsites <- readRDS(rdsfiles_qc[grep("infoTEM", rdsfiles_qc)])

# ctw needs to go back and fix code so sdl measurements not NA'd (probably happened due to tk and ctw adjusted flags)
# use what raw is unless was previously infilled
# previously infilled d1 and c1 are not NA'd, must have written out wrong dataset
charttemp2 <- mutate(charttemp, measurement = ifelse(is.na(tk_predicted), raw, measurement))


# read in previously QC'd dats (from 2018 renewal)
# set pathway to long-term-trends repo (qhere previously qc'd data live)
lttrepo <- "~/github/long-term-trends/"
qcdat <- "output_data/prep_data"
qcc1d1 <- list.files(paste0(lttrepo, "climate_d1_c1/", qcdat), full.names = T)
qcsdl <- list.files(paste0(lttrepo, "extended_summer/analysis/", qcdat), full.names = T)

# loggers qc'd
qc_d1cr <- read_csv(qcc1d1[grep("d1cr", qcc1d1)]) %>% data.frame()
qc_c1cr <- read_csv(qcc1d1[grep("c1cr", qcc1d1)]) %>% data.frame()
qc_sdlcr <- read_csv(qcsdl[grep("sdlcr_temp[.]", qcsdl)]) %>% data.frame()

# charts qc'd
qc_d1 <- read_csv(qcc1d1[grep("d1_te", qcc1d1)]) %>% data.frame()
qc_c1 <- read_csv(qcc1d1[grep("c1_te", qcc1d1)]) %>% data.frame()
qc_sdl <- read_csv(qcsdl[grep("sdlchart", qcsdl)]) %>% data.frame()
  
  

# -- STACK DAT FOR 2018-2020 QUICK QC ----
# want qc'd temp data thru 2018, then more recent (non-QC'd data)

# plot qc'd data first to review what got flagged
ggplot(subset(qc_c1, yr > 2010 & grepl("+3C", qa_flag)), aes(date, qa_temp)) +
  geom_point(aes(date, c1_temp), alpha = 0.5) +
  geom_point(aes(date, qa_temp), col = "red", alpha = 0.2) +
  facet_wrap(~met, scales = "free_x")

ggplot(subset(qc_d1, yr > 2010 & grepl("drop", qa_flag)), aes(date, qa_temp)) +
  geom_point(aes(date, d1_temp), alpha = 0.5) +
  geom_point(aes(date, qa_temp), col = "red", alpha = 0.2) +
  facet_wrap(~met, scales = "free_x")

# plot c1, d1 and sdl to screen for drop in 15-16 at sdl
ggplot() +
  geom_point(data = subset(qc_c1, yr > 2010 & grepl("artificial", qa_flag, ignore.case = T)), aes(date, c1_temp), color = "white", alpha = 0.8) +
  geom_line(data = subset(qc_c1, yr > 2010), aes(date, qa_temp), color = "green", alpha = 0.5) +
  geom_point(data = subset(qc_d1, yr > 2010 & grepl("artificial", qa_flag, ignore.case = T)), aes(date, d1_temp), color = "white", alpha = 0.8) +
  geom_line(data = subset(qc_d1, yr > 2010), aes(date, qa_temp), color = "blue", alpha = 0.5) +
  geom_point(data = subset(qc_sdl, yr > 2010), aes(date, sdl_temp), color = "white", alpha = 0.8) +
  geom_line(data = subset(qc_sdl, yr > 2010), aes(date, sdl_qatemp), color = "orchid", alpha = 0.5) +
  facet_grid(met~local_site) +
  theme_dark()
  
ggplot() +
  geom_point(data = subset(qc_c1cr, !is.na(qa_flag) & c1cr_temp > -100), aes(date, c1cr_temp), color = "white", alpha = 0.8) +
  geom_line(data = subset(qc_c1cr), aes(date, qa_temp), color = "green", alpha = 0.5) +
  geom_point(data = subset(qc_d1cr, !is.na(qa_flag) & d1cr_temp > -100), aes(date, d1cr_temp), color = "white", alpha = 0.8) +
  geom_line(data = subset(qc_d1cr), aes(date, qa_temp), color = "blue", alpha = 0.5) +
  geom_point(data = subset(qc_sdlcr, !is.na(qa_flag) & cr_temp > -100), aes(date, cr_temp), color = "white", alpha = 0.8) +
  geom_line(data = subset(qc_sdlcr), aes(date, qa_temp), color = "orchid", alpha = 0.5) +
  facet_grid(met~local_site) +
  theme_dark()
# i'm ok with these points being dropped (using qa_temp in these cases)
# okay to use adjusted chart data also (for artificial drops)

# use ctw qc'd sdl from renewal, and be sure to NA anything where !is.na(flag) [previously infilled]
# use 2011 onwards c1 and d1 from ctw, otherwise what's in charttemp
charttemp_c1d1 <- subset(charttemp, grepl("c1|d1", local_site) & yr < 2011 | yr > 2018) %>%
  # assign raw to measurement if tk predicted is NA (applies to 2019 onwards)
  mutate(measurement = ifelse(is.na(tk_predicted), raw, measurement))
unique(charttemp_c1d1$yr[is.na(charttemp_c1d1$tk_predicted)])

# prep qc'd 2011-2018 data
c1d1_qcd_1118 <- rbind(subset(qc_c1, yr > 2010, select = c(LTER_site:met, qa_temp)),
                       subset(qc_d1, yr > 2010, select = c(LTER_site:met, qa_temp))) %>%
  rename(metric = met, measurement = qa_temp) %>%
  # append chart to local_site
  mutate(local_site = paste0(local_site, "_chart"))

sdlchart_prep <- rename(qc_sdl, measurement = sdl_qatemp, metric = met) %>%
  mutate(local_site = paste0(local_site, "_chart"),
         # if infilled with old method, NA
         measurement = ifelse(!is.na(sdl_flag), NA, measurement)) %>%
  subset(select = names(c1d1_qcd_1118))

charttemp_prep <- rbind(c1d1_qcd_1118, sdlchart_prep) %>%
  rbind(charttemp_c1d1[names(.)]) %>%
  arrange(local_site, date, metric) %>% ## 2019
  distinct()

# prep logger dat
logger_prep <- rbind(subset(qc_c1cr, select = c(LTER_site:met, qa_temp)),
                subset(qc_d1cr, select = c(LTER_site:met, qa_temp)),
                subset(qc_sdlcr, select = c(LTER_site:met, qa_temp))) %>%
  rename(metric = met, measurement = qa_temp) %>%
  unite(local_site, local_site, logger, sep = "_")
# hmps at d1, c1 and sdl after these

# pull gl4 and hmps from logtemp (needs qc)
hmp_prep <- subset(logtemp, grepl("hmp", local_site)) 
gl4_prep <- subset(logtemp, grepl("gl4", station_name))



# -- QUICK QC ALL DATA ----
# remove min/max outliers in all

# in hmps and recent chart data (2019-2021): 
# 1) check sd deparature for daily measurements and daily delta (t-t1) WITHIN SITE
# 2) check sd departures for comparative sites

ggplot(hmp_prep, aes(mon, measurement, group = mon)) +
  geom_boxplot() +
  geom_jitter(aes(col = factor(yr)), alpha = 0.5) +
  facet_wrap(~local_site) # d1 hmp2 in march looks suss; sdl hmp3 stops june 2021

ggplot(gl4_prep, aes(mon, measurement, group = mon)) +
  geom_boxplot() +
  geom_jitter(aes(col = factor(yr)), alpha = 0.5)

hmp_dailysds <- arrange(hmp_prep, date) %>%
  group_by(metric, local_site) %>%
  mutate(delta = measurement - lag(measurement, 1), #ifelse(date != min(date), diff(measurement), NA),
         xs_scaledelta = scale(delta),
         xs_scaletemp = scale(measurement)) %>%
  group_by(mon, metric, local_site) %>%
  mutate(wi_mon_scaledelta = scale(delta),
         wi_mon_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  # calculate scaled measurements by metric rather than instrument (within station)
  group_by(mon, metric, station_name) %>%
  mutate(ws_comp_mon_scaledelta = scale(delta),
         ws_comp_mon_scaletemp = scale(measurement)) %>%
  # by date, across sites and instruments
  ungroup() %>%
  group_by(date, metric) %>%
  mutate(xs_comp_date_scaledelta = scale(delta),
         xs_comp_date_scaletemp = scale(measurement)) %>%
  # by date, within site (e.g., try to flag last value of sdl hmp 3)
  ungroup() %>%
  group_by(date, station_name) %>%
  mutate(ws_comp_date_scaledelta = scale(delta),
         ws_comp_date_scaletemp = scale(measurement)) %>%
  ungroup()
# count number of scales > abs(3.75)
hmp_dailysds$n_devs <- apply(hmp_dailysds[grep("scale", names(hmp_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 2.75))
# but count number of stations with violations on a certain date -- if many, then mb a real deviation in temp
hmp_dailysds <- group_by(hmp_dailysds, date, station_name) %>%
  mutate(ignore_station = length(unique(local_site[n_devs >= 3]))) %>%
  ungroup()


hmp_dailysds <- arrange(hmp_prep, date) %>%
  group_by(metric, local_site) %>%
  mutate(delta = measurement - lag(measurement, 1)) %>%
  ungroup() %>%
  #compare within station, by date
  group_by(date, station_name) %>%
  mutate(ws_scaledelta = scale(delta),
         ws_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  group_by(date, metric) %>%
  mutate(xs_scaledelta = scale(delta),
         xs_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  mutate(ws_deltarank = rank(-abs(ws_scaledelta)),
         ws_temprank = rank(-abs(ws_scaletemp)),
         xs_deltarank = rank(-abs(xs_scaledelta)),
         xs_temprank = rank(-abs(xs_scaletemp))
         ) %>%
  # see if can check max/min here
  group_by(date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] < measurement[metric == "airtemp_min"],
         flag_avg = measurement[metric == "airtemp_avg"] < measurement[metric == "airtemp_min"] | measurement[metric == "airtemp_avg"] > measurement[metric == "airtemp_max"])
hmp_dailysds$flag_ws <- apply(hmp_dailysds[grep("ws_sc", names(hmp_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 2) == 2)
hmp_dailysds$flag_xs <- apply(hmp_dailysds[grep("xs_sc", names(hmp_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 2) == 2)


ggplot(hmp_dailysds, aes(date, measurement, col = metric)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(hmp_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(hmp_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
  geom_point(data = subset(hmp_dailysds, ws_temprank <= 10), pch = 4, alpha = 0.9) +
  geom_point(data = subset(hmp_dailysds, xs_temprank <= 10), pch = 2, alpha = 0.9) +
  geom_point(data = subset(hmp_dailysds, flag_xs), pch = 1,alpha = 0.9) +
  facet_wrap(~local_site)

hmp_dailysds$qcflag <- FALSE
hmp_dailysds$qcflag[hmp_dailysds$xs_deltarank <= 15] <- TRUE
hmp_dailysds$qcflag[hmp_dailysds$ws_deltarank <= 15] <- TRUE
hmp_dailysds$qcflag[hmp_dailysds$xs_temprank <= 10] <- TRUE
hmp_dailysds$qcflag[hmp_dailysds$ws_temprank <= 10] <- TRUE
hmp_dailysds$qcflag[hmp_dailysds$flag_xs] <- TRUE
hmp_dailysds$qcflag[hmp_dailysds$flag_ws] <- TRUE


# screen chart data 2019-2021 same way
charttemp_dailysds <- subset(charttemp_prep, yr >= 1980) %>%
  arrange(date) %>%
  group_by(metric, local_site) %>%
  mutate(delta = measurement - lag(measurement, 1)) %>%
  ungroup() %>%
  #compare within station, by date
  group_by(mon, local_site) %>%
  mutate(ws_scaledelta = scale(delta),
         ws_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  group_by(mon, metric) %>%
  mutate(xs_scaledelta = scale(delta),
         xs_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  mutate(ws_deltarank = rank(-abs(ws_scaledelta)),
         ws_temprank = rank(-abs(ws_scaletemp)),
         xs_deltarank = rank(-abs(xs_scaledelta)),
         xs_temprank = rank(-abs(xs_scaletemp))
  ) %>%
  # see if can check max/min here
  group_by(date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] < measurement[metric == "airtemp_min"])
# make same flags
charttemp_dailysds$flag_ws <- apply(charttemp_dailysds[grep("ws_sc", names(charttemp_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 3) == 2)
charttemp_dailysds$flag_xs <- apply(charttemp_dailysds[grep("xs_sc", names(charttemp_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 3) == 2)


ggplot(charttemp_dailysds, aes(date, measurement, col = local_site)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(charttemp_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, flag_ws), pch = 1, alpha = 0.9) +
  geom_point(data = subset(charttemp_dailysds, flag_xs), pch = 6, alpha = 0.9) +
  facet_wrap(~metric, nrow = 2)

# just for logger regressions, remove these points to be more cautious (some are likely real, e.g., 1980s cold snap)
charttemp_dailysds$qcflag <- FALSE
charttemp_dailysds$qcflag[charttemp_dailysds$xs_deltarank <= 15] <- TRUE
charttemp_dailysds$qcflag[charttemp_dailysds$ws_deltarank <= 15] <- TRUE
charttemp_dailysds$qcflag[charttemp_dailysds$xs_temprank <= 15] <- TRUE
charttemp_dailysds$qcflag[charttemp_dailysds$ws_temprank <= 15] <- TRUE
charttemp_dailysds$qcflag[charttemp_dailysds$flag_xs] <- TRUE
charttemp_dailysds$qcflag[charttemp_dailysds$flag_ws] <- TRUE
charttemp_dailysds$qcflag[charttemp_dailysds$flag_max] <- TRUE
# except leave 1984 and 1985 at sdl and d1 and 2011 at sdl and c1 alone (manual review)
charttemp_dailysds$qcflag[charttemp_dailysds$qcflag & charttemp_dailysds$mon %in% c(1,2) & charttemp_dailysds$yr %in% c(1984, 1985, 2011)] <- FALSE


# quick qc snotel
snotel_dailysds <- subset(snotel) %>%
  # add absolute lim flags
  mutate(limit_flag = measurement < -50 | measurement > 40)
# manually remove tmax for niwot and snotel (manual review via iteration)
snotel_dailysds$sensor_fail <- FALSE
snotel_dailysds$sensor_fail[grepl("Niw", snotel_dailysds$local_site) & snotel_dailysds$date %in% seq.Date(as.Date("2005-06-01"), as.Date("2007-02-01"), 1) & snotel_dailysds$metric == "airtemp_max"] <- TRUE 
snotel_dailysds$sensor_fail[grepl("Univer", snotel_dailysds$local_site) & snotel_dailysds$date %in% seq.Date(as.Date("2010-05-01"), as.Date("2011-09-01"), 1) & snotel_dailysds$metric == "airtemp_max"] <- TRUE 
# resume quick qc
snotel_dailysds <- mutate(snotel_dailysds, measurement = ifelse(limit_flag | sensor_fail, NA, measurement)) %>%
  arrange(date) %>%
  group_by(metric, local_site) %>%
  mutate(delta = measurement - lag(measurement, 1)) %>%
  ungroup() %>%
  #compare within station, by date
  group_by(mon, local_site) %>%
  mutate(ws_scaledelta = scale(delta),
         ws_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  group_by(mon, metric) %>%
  mutate(xs_scaledelta = scale(delta),
         xs_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  mutate(ws_deltarank = rank(-abs(ws_scaledelta)),
         ws_temprank = rank(-abs(ws_scaletemp)),
         xs_deltarank = rank(-abs(xs_scaledelta)),
         xs_temprank = rank(-abs(xs_scaletemp))
  ) %>%
  # see if can check max/min here
  group_by(date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] < measurement[metric == "airtemp_min"])
# make same flags
snotel_dailysds$flag_ws <- apply(snotel_dailysds[grep("ws_sc", names(snotel_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 3.5) == 2)
snotel_dailysds$flag_xs <- apply(snotel_dailysds[grep("xs_sc", names(snotel_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 3.5) == 2)
snotel_dailysds$flag_temp <- apply(snotel_dailysds[grep("scaletemp", names(snotel_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 3.5) == 2)


ggplot(snotel_dailysds, aes(date, measurement, col = local_site)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(snotel_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, flag_ws), pch = 1, alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, flag_xs), pch = 6, alpha = 0.9) +
  geom_point(data = subset(snotel_dailysds, flag_temp), pch = 13, alpha = 0.9) +
  facet_wrap(~metric, nrow = 2)

# seems reasonable for a quick flagging
# just for logger regressions, remove these points to be more cautious (some are likely real, e.g., 1980s cold snap)
snotel_dailysds$qcflag <- FALSE
snotel_dailysds$qcflag[snotel_dailysds$xs_deltarank <= 15] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$ws_deltarank <= 15] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$xs_temprank <= 15] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$ws_temprank <= 15] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$flag_xs] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$flag_ws] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$flag_temp] <- TRUE
snotel_dailysds$qcflag[snotel_dailysds$flag_max] <- TRUE

# see if it takes care of high periods for niwot and univ camp
ggplot(subset(snotel_dailysds, !qcflag), aes(date, measurement, col = local_site)) +
  geom_point(alpha = 0.3) +
  facet_grid(local_site~metric)

ggplot(subset(snotel_dailysds, !qcflag & grepl("Univ",local_site)), aes(date, measurement, col = measurement > 27)) +
  geom_point(alpha = 0.3) +
  facet_grid(metric~local_site, scales = "free_x")
# remove niwot tmax july 2005 through all of feb 2007 (manual review)
# uc: tmax errors may 2010 through sep 1 2011
snotel_dailysds$qcflag[grepl("Uni", snotel_dailysds$local_site) & snotel_dailysds$measurement > 27 & !is.na(snotel_dailysds$measurement)] <- TRUE


# quick qc ghcnd
ghcnd_dailysds <- subset(ghcnd, metric != "TOBS") %>%
  # add absolute lim flags
  mutate(limit_flag = measurement < -60 | measurement > 50,
         measurement = ifelse(limit_flag, NA, measurement)) %>%
  arrange(date) %>%
  group_by(metric, local_site) %>%
  mutate(delta = measurement - lag(measurement, 1)) %>%
  ungroup() %>%
  #compare within station, by date
  group_by(mon, local_site) %>%
  mutate(ws_scaledelta = scale(delta),
         ws_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  group_by(mon, metric) %>%
  mutate(xs_scaledelta = scale(delta),
         xs_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  mutate(ws_deltarank = rank(-abs(ws_scaledelta)),
         ws_temprank = rank(-abs(ws_scaletemp)),
         xs_deltarank = rank(-abs(xs_scaledelta)),
         xs_temprank = rank(-abs(xs_scaletemp))
  ) %>%
  # see if can check max/min here
  group_by(date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] < measurement[metric == "airtemp_min"])
# make same flags
ghcnd_dailysds$flag_ws <- apply(ghcnd_dailysds[grep("ws_sc", names(ghcnd_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 5) == 2)
ghcnd_dailysds$flag_xs <- apply(ghcnd_dailysds[grep("xs_sc", names(ghcnd_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 5) == 2)
ghcnd_dailysds$flag_temp <- apply(ghcnd_dailysds[grep("scaletemp", names(ghcnd_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 5) == 2)


ggplot(ghcnd_dailysds, aes(date, measurement, col = metric)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(ghcnd_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(ghcnd_dailysds, ws_deltarank <= 10), pch = 8, alpha = 0.9) +
  #geom_point(data = subset(ghcnd_dailysds, ws_temprank <= 10), pch = 4, alpha = 0.9) +
  geom_point(data = subset(ghcnd_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
  geom_point(data = subset(ghcnd_dailysds, flag_ws), pch = 1, alpha = 0.9) +
  geom_point(data = subset(ghcnd_dailysds, flag_xs), pch = 6, alpha = 0.9) +
  geom_point(data = subset(ghcnd_dailysds, flag_temp), pch = 13, alpha = 0.9) +
  facet_wrap(~local_site)
# let 681, 183, 759 keep their cold temps (no flag by ws_rank)

ghcnd_dailysds$qcflag <- FALSE
ghcnd_dailysds$qcflag[ghcnd_dailysds$xs_deltarank <= 15] <- TRUE
ghcnd_dailysds$qcflag[ghcnd_dailysds$ws_deltarank <= 10] <- TRUE
ghcnd_dailysds$qcflag[ghcnd_dailysds$xs_temprank <= 15] <- TRUE
#ghcnd_dailysds$qcflag[ghcnd_dailysds$ws_temprank <= 10] <- TRUE
ghcnd_dailysds$qcflag[ghcnd_dailysds$flag_xs] <- TRUE
ghcnd_dailysds$qcflag[ghcnd_dailysds$flag_ws] <- TRUE
ghcnd_dailysds$qcflag[ghcnd_dailysds$flag_temp] <- TRUE
ghcnd_dailysds$qcflag[ghcnd_dailysds$flag_max] <- TRUE


# ameriflux + gl4 logger
# for ameriflux, keep qc_measurement for ameriflux forest site, but raw for tvans sites
ameriflux$measurement <- with(ameriflux, ifelse(grepl("NR1", local_site), qc_measurement, raw_measurement))

amerigl4 <- ameriflux[names(ameriflux) %in% names(gl4_prep)] %>%
  rbind(gl4_prep[names(.)])
amerigl4_dailysds <- subset(amerigl4) %>%
  arrange(date) %>%
  group_by(metric, local_site) %>%
  mutate(delta = measurement - lag(measurement, 1)) %>%
  ungroup() %>%
  #compare within station, by date
  group_by(mon, local_site) %>%
  mutate(ws_scaledelta = scale(delta),
         ws_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  group_by(mon, metric) %>%
  mutate(xs_scaledelta = scale(delta),
         xs_scaletemp = scale(measurement)) %>%
  ungroup() %>%
  mutate(ws_deltarank = rank(-abs(ws_scaledelta)),
         ws_temprank = rank(-abs(ws_scaletemp)),
         xs_deltarank = rank(-abs(xs_scaledelta)),
         xs_temprank = rank(-abs(xs_scaletemp))
  ) %>%
  # see if can check max/min here
  group_by(date, local_site) %>%
  mutate(flag_max = measurement[metric == "airtemp_max"] < measurement[metric == "airtemp_min"],
         flag_avg = measurement[metric == "airtemp_avg"] < measurement[metric == "airtemp_min"] | measurement[metric == "airtemp_avg"] > measurement[metric == "airtemp_max"])
  
# make same flags
amerigl4_dailysds$flag_ws <- apply(amerigl4_dailysds[grep("ws_sc", names(amerigl4_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 4) == 2)
amerigl4_dailysds$flag_xs <- apply(amerigl4_dailysds[grep("xs_sc", names(amerigl4_dailysds))], MARGIN = 1, function(x) sum(abs(x) >= 4) == 2)


ggplot(amerigl4_dailysds, aes(date, measurement, col = local_site)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(amerigl4_dailysds, xs_deltarank <= 15), alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, ws_deltarank <= 15), pch = 8, alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, ws_temprank <= 15), pch = 4, alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, xs_temprank <= 15), pch = 2, alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, flag_ws), pch = 1, alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, flag_xs), pch = 6, alpha = 0.9) +
  facet_wrap(~metric, nrow = 2)

# flag first, and then undo flag by # of flags per date (e.g., to allow cold period in winter 2011)
amerigl4_dailysds$qcflag <- FALSE
amerigl4_dailysds$qcflag[amerigl4_dailysds$xs_deltarank <= 15] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$ws_deltarank <= 15] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$xs_temprank <= 15] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$ws_temprank <= 15] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$flag_xs] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$flag_ws] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$flag_max] <- TRUE
amerigl4_dailysds$qcflag[amerigl4_dailysds$flag_avg] <- TRUE

# group_by date and count flags
amerigl4_dailysds <- group_by(amerigl4_dailysds, date) %>%
  mutate(postcheck = sum(qcflag)) %>%
  ungroup()

ggplot(amerigl4_dailysds, aes(date, measurement, col = local_site)) +
  geom_line(alpha = 0.3) +
  geom_point(data = subset(amerigl4_dailysds, qcflag), alpha = 0.9) +
  geom_point(data = subset(amerigl4_dailysds, postcheck > 1), col = "black", alpha = 0.9) +
  facet_wrap(~metric, nrow = 2)
# looks okay
amerigl4_dailysds$qcflag[amerigl4_dailysds$postcheck > 1] <- FALSE




# -- PREP DATA FOR REGRESSIONS -----

# need data to have date, yr, doy, local_site (unique ID), metric, and measurement
charttemp_lm <- mean_and_diurnalT(charttemp2)
nwtlog_lm <- mean_and_diurnalT(logtemp)
snotel_lm <- mean_and_diurnalT(snotel)
# for ameriflux, keep qc_measurement for ameriflux forest site, but raw for tvans sites
ameriflux$measurement <- with(ameriflux, ifelse(grepl("NR1", local_site), qc_measurement, raw_measurement))
ameriflux_lm <- mean_and_diurnalT(ameriflux) 
ghcnd_lm <- mean_and_diurnalT(subset(ghcnd, metric != "TOBS"))

# need date, yr, mon, doy, local_site, and measurement (qc'd)
alldats <- rbind(charttemp_lm, nwtlog_lm, snotel_lm, ameriflux_lm, ghcnd_lm) %>%
  subset(grepl("avg|DTR", metric)) %>%
  arrange(local_site, date, metric)

# make sure allsites ordered by pair rank
allsites <- arrange(allsites, local_site, final_rank, paired_site)



# -- INFILL NWT -----



# -- SDL loggers -----
unique(allsites$local_site[grepl("sdl", allsites$local_site)])

sdl21x_missing <- idInfillDates(alldats, "sdl_cr21x", 1980)
sdl21x_order <- with(allsites, paired_site[grepl("sdl_cr21x", local_site) & final_rank!= 1])
# moving window infill
sdl21x_season <- tk_temp_movingfill(alldats, target_site = "sdl_cr21x", missing_dates = sdl21x_missing, site_order =  sdl21x_order)
sdl21x_season_tempnobs <- tk_temp_movingfill(alldats, target_site = "sdl_cr21x", missing_dates = sdl21x_missing, site_order =  sdl21x_order)
# historic infill
sdl21x_historic <- tk_temp_historicfill(alldats, "sdl_cr21x", sdl21x_missing, sdl21x_order)


# selection for sdl21x
select_sdl121x <- select_model(sdl21x_historic, sdl21x_season_tempnobs)
outstanding_dates <- sdl21x_missing[!sdl21x_missing %in% select_sd121x$date]

# plots using calculate_tmintmax code in progress with sdl21x
ggplot(dat_out, aes(month(date), dtr_infill, group = month(date))) +
  geom_violin(fill = "yellow", col = "goldenrod", alpha = 0.5) +
  geom_violin(aes(month(date), airtemp_max-airtemp_min), alpha = 0.5, col = "purple")

ggplot(subset(dat_out, !is.na(meanadj_tmin_infill))) +
  geom_line(aes(as.numeric(factor(date)), adj_tmin_infill), col = "goldenrod", alpha = 0.5) +
  geom_line(aes(as.numeric(factor(date)), meanadj_tmin_infill), col = "orchid", alpha = 0.5) +
  geom_line(aes(as.numeric(factor(date)), adj_tmin_measured), col = "skyblue") +
  theme(axis.text.x = element_blank()) # predicted DTR is generally truncated, but within range, compared to actual DTR (except in fall.. diurnal falls below observed range. is especially truncated)

ggplot(subset(dat_out, !is.na(meanadj_tmin_infill))) +
  geom_line(aes(as.numeric(factor(date)), tmin_infill), col = "black", alpha = 0.5) +
  geom_line(aes(as.numeric(factor(date)), adj_tmin_infill), col = "goldenrod", alpha = 0.5) +
  geom_point(aes(as.numeric(factor(date)), adj_tmin_infill, size = month(date)), col = "goldenrod", alpha = 0.5) +
  geom_line(aes(as.numeric(factor(date)), meanadj_tmin_infill), col = "orchid", alpha = 0.5) +
  geom_line(aes(as.numeric(factor(date)), adj_tmin_measured), col = "skyblue") +
  geom_point(aes(as.numeric(factor(date)), adj_tmin_measured), col = "skyblue") +
  theme(axis.text.x = element_blank()) # diurnal predicted yields more muted/warmer tmin compared to tmean observed or tmean predicted adjusted tmin

ggplot(subset(dat_out, !is.na(meanadj_tmax_infill))) +
  geom_line(aes(as.numeric(factor(date)), tmax_infill), col = "black", alpha = 0.5) +
  geom_line(aes(as.numeric(factor(date)), adj_tmax_infill), col = "goldenrod", alpha = 0.5) +
  geom_point(aes(as.numeric(factor(date)), adj_tmax_infill, size = month(date)), col = "goldenrod", alpha = 0.5) +
  geom_line(aes(as.numeric(factor(date)), meanadj_tmax_infill), col = "orchid", alpha = 0.5) +
  geom_line(aes(as.numeric(factor(date)), adj_tmax_measured), col = "skyblue") +
  geom_point(aes(as.numeric(factor(date)), adj_tmax_measured), col = "skyblue") +
  theme(axis.text.x = element_blank()) # diurnal predicted tmax seems like a reasonable compromise in this case? observed tmean adjusted tmax is warmer than other (with a high point), tmean predicted is cooler (with a few low points)
# go with diurnal adjusted (since follows tim's method, but reject any tmin greater than 20C [even in summer would not be 70F overnight at saddle])
# looking at the raw data, the high adj-predicted point for tmin is a tmax value that would get flagged and NA'd
# if diurnal-adj tmax is cooler than measured observed tmean, use predicted tmean (this only applies in one case) .. maybe reasons to return dat_out for review before selection
# if diurnal-adj tmin is warmer than observed tmean, use predicted tmean

# looking at flagged tmeans than are warmer/cooler than adj tmax/tmin, predicted tmax and tmin those days are closer to sdl chart (just looked at a few)
# maybe scratch and just use all predicted values that day since unsure if tmean or the extreme is more correct (and eventually write comparison to nearby stations to select)


sdl21x_predicted <- calculate_minmax(select_sdl121x, logtemp, "sdl_cr21x")
sdl21x_predicted$flagmax <- with(sdl21x_predicted, airtemp_max < airtemp_min | airtemp_avg > airtemp_max)
sdl21x_predicted$flagmin <- with(sdl21x_predicted, airtemp_avg < airtemp_min)

# how does it look?
ggplot(sdl21x_predicted) +
  geom_vline(aes(xintercept = as.Date("1989-01-01"))) +
  geom_line(aes(date, airtemp_avg), col = "green", alpha = 0.3) +
  geom_line(aes(date, airtemp_max), col = "purple", alpha = 0.3) +
  geom_point(data = subset(sdl21x_predicted, airtemp_max_method != "raw"), aes(date, airtemp_max), col = "red", alpha = 0.5) +
  geom_line(aes(date, airtemp_min), col = "blue", alpha = 0.3) +
  geom_point(data = subset(sdl21x_predicted, airtemp_min_method != "raw"), aes(date, airtemp_min), col = "dodgerblue", alpha = 0.5) +
  geom_point(data = subset(sdl21x_predicted, flagmin), aes(date, airtemp_min), size = 2, col = "blue", alpha = 0.8) +
  geom_point(data = subset(sdl21x_predicted, flagmin), aes(date, airtemp_avg), size = 2, col = "forestgreen", alpha = 0.8) +
  geom_point(data = subset(sdl21x_predicted, flagmin), aes(date, airtemp_max), size = 2, col = "chocolate", alpha = 0.8) +
  geom_smooth(aes(date, airtemp_avg), col = "black", alpha = 0.3)
