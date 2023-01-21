# format infill temp and ppt dats for EDI

# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)

source("~/github/nwt-data-munging/nwt_climate/R/fetch_data_functions.R")
datpath <- "/Users/scarlet/Documents/nwt_lter/nwt_climate/data/"

# read in saddle dats used for jhux and mo papers
# > qc'd, gapfilled, homogenized/date-cutoff daily datasets

sdl_temp <- readRDS(paste0(datpath, "homogenize/nwt_sdl_homogenized_temperature_draft.rds"))
sdl_ppt <- readRDS(paste0(datpath, "infill/sdlPPT_infilled_draft.rds"))

# pull in all sites for saddle (temp and precip)
sitesppt <- readRDS(paste0(datpath, "qc/siteinfoPPT_qc.rds"))
sitestemp <- readRDS(paste0(datpath, "qc/siteinfoTEMP_qc.rds"))

# need to look at all sites stacked data to know which sites to describe in codes for source stations (e.g., no ghcnd stations for hmp period if those stations had stopped)
qcdats <- list.files(paste0(datpath, "qc"), full.names = T)
ghcndtemp <- readRDS(qcdats[grepl("cndT", qcdats)])
ghcndppt <- readRDS(qcdats[grepl("cndP", qcdats)])
fluxtemp <- readRDS(qcdats[grepl("fluxT", qcdats)])
fluxppt <- readRDS(qcdats[grepl("fluxP", qcdats)])
snoteltemp <- readRDS(qcdats[grepl("telT", qcdats)])
snotelppt <- readRDS(qcdats[grepl("telP", qcdats)])
nwtlogtemp <- readRDS(qcdats[grepl("loggerT", qcdats)])
allppt <- readRDS(paste0(datpath, "qc/siteinfoPPT_qc.rds"))

# read in kittel datasets to check colnames
tkd1_temp <- getTabular(185, datanum = 1)
tkd1_ppt <- getTabular(186)

# what sites were temporally available to use for sdl temp?
# 1986-2017 = loggers
# 2018 - present = HMP sensor period

subset(ghcndtemp, yr %in% 1986:2017, select = c(station_id, station_name)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() # berthoud only available for 1981-1985

subset(ghcndtemp, yr > 2017, select = c(station_id, station_name)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() 

subset(fluxtemp, yr > 2017, select = c(station_id, station_name)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() # all available, only forest available for ppt

subset(snoteltemp, yr < 2017, select = c(station_id, station_name)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() # all available post and before 2017

# check loggers
subset(nwtlogtemp, yr< 2017, select = c(station_id, station_name, local_site)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() # gl4 cr850 in operation 2017-2019, so available for hmp infilling. all hmps at all stations were there in 2018
# c1 hmps all began in jan 2017, but only c1 had hmps in 2017

# look at sites used for sdl
subset(sitesppt, station_name %in% sitesppt$paired_site[sitesppt$station_name == "sdl"], select = c(station_id:station_name)) %>% 
  distinct() %>%
  data.frame()

subset(sitesppt, station_id %in% sitesppt$paired_site[sitesppt$station_name == "sdl"], select = c(station_id:station_name)) %>% 
  distinct() %>%
  data.frame() # not all there because of white space difference

# temp
subset(sitestemp, station_name %in% sitesppt$paired_site[grepl("sdl", sitesppt$station_name)], select = c(station_id:station_name)) %>% 
  distinct() %>%
  data.frame()

subset(sitestemp, station_id %in% sitesppt$paired_site[grepl("sdl", sitesppt$station_name)], select = c(station_id:station_name)) %>% 
  distinct() %>%
  arrange(station_id)


# -- SCREEN PPT ----
# check that all flags present, no weird values in columns
glimpse(sdl_ppt)

summary(is.na(sdl_ppt))
# columns that have NAs:
# pvalue, r2, regression (all same amount of NAs)
# raw ppt and raw qdays (okay to not have same # NAs)
# qc notes
# > all okay

# look at unique values
sapply(sdl_ppt[grepl("site|ye|flag|note", names(sdl_ppt))], function(x) sort(unique(x)))
# okay to release infilled dataset oct 01 1987 onwards -- need to subset dataset

# maybe need to clarify...
# "outdated method" and "JM flagged" in infill_qcnote. explain previous method and QA-flagged or flagged by NWT climate lead during QA review
# also if "raw" values present for outdated method row, explain "raw" is actually old-method infilled

# all comparative qc flags need to be clarified (e.g., "scaled limit", z-score, 15% threshold)
# 


# repeat for ppt
# what sites were temporally available to use for sdl ppt?
# 1987-present 

subset(ghcndppt, yr > 1986, select = c(station_id, station_name)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() # berthoud only available for 1981-1985


subset(snoteltemp, yr > 1986, select = c(station_id, station_name)) %>%
  distinct() %>%
  arrange(station_id) %>%
  data.frame() # all available 1987 onwards


# -- PRETTY DATASETS ----
# make sure colnames match naming convention for C1 and D1 gap-filled
# pval = pvalue, r2 = rsquared
# > going to leave airtemp_max/min/avg as is and dtr lowcase (not following C1 and D1 colnames, will suggest to sarah we adjust those)
# > not going to put "num_obs...regression_equation" because will be too long for hmps (end it at "..regression")

sdl_temp_pretty <- sdl_temp
names(sdl_temp_pretty) <- gsub("pval$|pval(?=_)" , "pvalue", names(sdl_temp), perl = T)
names(sdl_temp_pretty) <- gsub("r2" , "rsquared", names(sdl_temp_pretty))
names(sdl_temp_pretty)[names(sdl_temp_pretty) == "yr"] <- "year"

# i guess put year before date to match c1 and d1 infilled? and in raw datasets and tk infilled, max, min and avg is the order
# order temps: max, min, avg
sdl_temp_pretty <- subset(sdl_temp_pretty, select = c(LTER_site:logger, year, date,
                                     airtemp_max_homogenized, airtemp_min_homogenized, airtemp_avg_homogenized, dtr_homogenized,
                                     airtemp_max_adjustment, airtemp_avg_adjustment, airtemp_min_adjustment,
                                     airtemp_max_gapfilled, airtemp_min_gapfilled, airtemp_avg_gapfilled, dtr_gapfilled,
                                     flag_1:ncol(sdl_temp_pretty)))
                          
                                     
# make sure flags are correct


# QAQC notes should be intelligible


# source station names should be clear


# remove mon, doy, pay attention to letter casing

