# prep precip and temp data for ext summer pca

# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(lubridate)
options(stringsAsFactors = F)
na_vals <- c("", " ", NA, "NA")

# set datpath
datpath <- "extended_summer/output_data/"

# read in data
jennings <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.168.1&entityid=d4385f9a680c860c9c444e021ea1a35d",
                     trim_ws = T,
                     na = na_vals)

# CTW: infilled 1982-2017 precip data (NSF renewal data [HH/EF] + ctw infilling 2015-current)
suding_ppt <- read.csv("extended_summer/output_data/suding/allyrs/sdl_ppt_infill_19822018_nsfctw.csv",
                       strip.white = TRUE, 
                       na.strings = na_vals)
# CTW: infilled 1982-2017 temp data (NSF data [HH/EF] + ctw infilling 2015-current)
suding_temp <- read.csv("extended_summer/output_data/suding/allyrs/sdl_temp_infilled_19822018_nsfctw.csv",
                        strip.white = T,
                        na.strings = na_vals)
# CTW: raw sdl chart temp data + ctw infilling 2010-current (first NAs in raw data appear in 2010)
ctw_temp <- read.csv("extended_summer/output_data/ctw/sdl_temp_infilled_19822018_ctwonly.csv",
                    strip.white = T,
                    na.strings = na_vals)
# CTW: backwards predicted cr1000 vals
prd_c1000temp <- read.csv("extended_summer/output_data/ctw/predict_cr1000temp_1982-ongoing.csv",
                          strip.white = T,
                          na.strings = na_vals)
# CTW: infilled sdl logger temp dataset
prd_cralltemp <- read.csv("extended_summer/output_data/ctw/predict_sdl_cralltemp_1982-ongoing.csv",
                          strip.white = T,
                          na.strings = na_vals)

# TK infilled d1 temp
d1temp <- read.csv("/Users/serahsierra/Dropbox/NWT_data/d1_infilled_daily_temp.csv") %>%
  subset(year > 1980)

# TK infilled d1 ppt
d1ppt <- read.csv("/Users/serahsierra/Dropbox/NWT_data/d1_infilled_daily_ppt.csv") %>%
  subset(Year > 1980)


# -- PREP JENNINGS DATA -----
# examine jennings et al. data and structure
str(jennings)
summary(jennings)
glimpse(jennings)
unique(jennings$local_site)

# create sdl-only precip and temp dataset from jennings et al. 2018
# reqs: yr, mo, day, tmin (C), tmax (C), ppt (mm)
sdl_jennings <- jennings %>%
  # select sdl only for comparison with nwt_renewal data (sdl only)
  filter(local_site == "sdl") %>%
  rename(Year = year) %>%
  # aggregate on daily basis
  ## note!: i'm ignoring keith's flags for now so I can do an initial comparison.
  ## come back and review if initial comparison look promising
  ## match Emily's colnames for hcn (Year, Month, Day, PCP, TMIN, TMAX)
  dplyr::group_by(Year, date) %>%
  summarize(TMIN = min(airtemp_avg),
            TMAX = max(airtemp_avg),
            PCP = sum(ppt_tot)) %>%
  ungroup() %>%
  mutate(Month = month(date),
         Day = day(date)) %>%
  dplyr::select(date, Year, Month, Day, TMIN, TMAX, PCP)

# review (needs to start at Jan 1 [first yyyy], and end Dec 31 [last yyyy])
summary(sdl_jennings)
head(sdl_jennings) # starts at Jan 1 1990
tail(sdl_jennings) # ends at Dec 31 2013

# review to make sure all looks reasonable (again, quick first comparison, then come back to review if promising)
plot(sdl_jennings$Year, sdl_jennings$TMIN)
plot(sdl_jennings$Year, sdl_jennings$TMAX)
plot(sdl_jennings$Year, sdl_jennings$PCP) #outlier in 1995


# -- PREP TIM KITTEL INFILLED D1 FOR ExtSum PCA -----
d1_met <- left_join(d1ppt[,1:4], d1temp[,1:5],
                    by = c("Month" = "month", "Day" = "day", "Year" = "year")) %>%
  rename(PCP = `D1.mm.ppt`,
         TMAX = Tmax,
         TMIN = Tmin) %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
  dplyr::select(date, Year, Month, Day, TMIN, TMAX, PCP) 
  


# -- PREP NWT RENEWAL/CTW INFILLED (SUDING) DATA -----
# precip: clean up, format column names to match Emily's (Year, Month, Day, PCP)
pcp <- suding_ppt %>%
  #filter(`date` > as.Date("1981-07-31")) %>% # start at Jan 1 1982
  dplyr::select(yr, mon, day, ppt_tot) %>%
  dplyr::rename(Year = yr,
                Month = mon,
                Day = `day`,
                PCP = ppt_tot)

# clean up, format column names to match Emily's (Year, Month, Day, TMIN, TMAX)
temp <- suding_temp %>%
  dplyr::select(date, airtemp_max, airtemp_min) %>%
  #filter(`date` > as.Date("1981-07-31")) %>% # start at Jan 1 1982
  mutate(Year = year(`date`),
         Month = month(`date`),
         Day = day(`date`)) %>%
  dplyr::rename(TMIN = airtemp_min,
                TMAX = airtemp_max) %>%
  select(Year, Month, Day, TMIN, TMAX)

temp2 <- ctw_temp %>%
  dplyr::select(date, airtemp_max, airtemp_min) %>%
  #filter(`date` > as.Date("1981-07-31")) %>% # start at Jan 1 1982
  mutate(Year = year(`date`),
         Month = month(`date`),
         Day = day(`date`)) %>%
  dplyr::rename(TMIN = airtemp_min,
                TMAX = airtemp_max) %>%
  select(Year, Month, Day, TMIN, TMAX)


# -- PREP INFILLED CR TEMP DATASETS  ----
# clean up, format column names to match Emily's (Year, Month, Day, TMIN, TMAX)
cr1000temp_allyrs <- prd_c1000temp %>%
  # prioritize actual saddle logger temp over predicted saddle logger temp
  mutate(hcntemp = ifelse(!is.na(cr1000_temp), cr1000_temp, fit)) %>%
  dplyr::select(date, met, hcntemp) %>%
  spread(met, hcntemp) %>%
  #filter(`date` > as.Date("1981-07-31")) %>% # start at Jan 1 1982
  mutate(Year = year(`date`),
         Month = month(`date`),
         Day = day(`date`)) %>%
  dplyr::rename(TMIN = airtemp_min,
                TMAX = airtemp_max) %>%
  select(Year, Month, Day, TMIN, TMAX)

# all cr loggers -- use cr21x-sdl chart relationship for infilling 1980s
# > average temp on 4 days in june 2000 cr21x and cr23 x overlap (cr23x is 1 degree warmer than cr21x in tmax and tmin)
crallT_1980cr21x <- prd_cralltemp %>%
  # select cr21x for infill method in 1980s
  filter(!(is.na(logger) & grepl("cr1000-sdl", method))) %>%
  # prioritize actual saddle logger temp over predicted saddle logger temp
  mutate(hcntemp = ifelse(!is.na(qa_temp), qa_temp, fit)) %>%
  # average temp vals for days where 21x and 23x overlapped
  group_by(met, date) %>%
  mutate(hcntemp = mean(hcntemp),
         check = length(hcntemp)) %>% # check should equal 2 just for 8 obs (4 days x two types airtemp)
  ungroup() %>%
  dplyr::select(date, met, hcntemp) %>%
  distinct() %>%
  spread(met, hcntemp) %>%
  #filter(`date` > as.Date("1981-07-31")) %>% # start at Jan 1 1982
  mutate(Year = year(`date`),
         Month = month(`date`),
         Day = day(`date`)) %>%
  dplyr::rename(TMIN = airtemp_min,
                TMAX = airtemp_max) %>%
  select(Year, Month, Day, TMIN, TMAX)


# all cr loggers -- use cr1000-sdl chart relationship for infilling 1980s
crallT_1980cr1000 <- prd_cralltemp %>%
  # select cr21x for infill method in 1980s
  filter(!(is.na(logger) & grepl("cr21x-sdl", method))) %>%
  # prioritize actual saddle logger temp over predicted saddle logger temp
  mutate(hcntemp = ifelse(!is.na(qa_temp), qa_temp, fit)) %>%
  # average temp vals for days where 21x and 23x overlapped
  group_by(met, date) %>%
  mutate(hcntemp = mean(hcntemp),
         check = length(hcntemp)) %>% # check should equal 2 just for 8 obs (4 days x two types airtemp)
  ungroup() %>%
  dplyr::select(date, met, hcntemp) %>%
  distinct() %>%
  spread(met, hcntemp) %>%
  #filter(`date` > as.Date("1981-07-31")) %>% # start at Jan 1 1982
  mutate(Year = year(`date`),
         Month = month(`date`),
         Day = day(`date`)) %>%
  dplyr::rename(TMIN = airtemp_min,
                TMAX = airtemp_max) %>%
  select(Year, Month, Day, TMIN, TMAX)

# -- PREP HCN FOR AET -----
###   LOAD DATA FILE AND CREATE MATRICES:
###  tmin[ye,mo,da], tmax[ye,mo,da], pcp[ye,mo,da] 
###   == minimum daily temperature, maximum daily temperature and precip for the year, month, and day.
###     ye ranges from 1 to n.years.

# > CTW: combine pcp and temp to get hcn df (cols: Year, Month, Day, TMIN, TMAX, PCP)
suding_hcn <- full_join(temp, pcp)
summary(suding_hcn)
head(suding_hcn) # starts at July 31 1981
tail(suding_hcn) # ends at Dec 31 2018

# > CTW test: try adding in 0's for Jan 1 1981 - July 30 1981 so code will run without breaking using real July 31-Dec 1981 data
# will need to compare to Emily's numbers to make sure 0s didn't change anything fundamentally (their results should be kicked out in the code anyway since keep Sep and onwards only each year)
###  vars to use: Day Month Year TMIN TMAX PCP
dates <- seq(from = as.Date("1981-01-01"), to = as.Date("2018-12-31"), by = "day")
dates <- data.frame(`date` = dates) %>%
  mutate(Year = year(`date`),
         Month = month(`date`),
         Day = day(`date`))
suding_hcn_new <- left_join(dates, suding_hcn)
suding_hcn_new$TMIN[is.na(suding_hcn_new$TMIN)] <- 0
suding_hcn_new$TMAX[is.na(suding_hcn_new$TMAX)] <- 0
suding_hcn_new$PCP[is.na(suding_hcn_new$PCP)] <- 0
suding_hcn <- suding_hcn_new
# review
head(suding_hcn)
tail(suding_hcn)

suding_hcn_jenningsdates <- suding_hcn %>%
  subset(Year %in% min(sdl_jennings$Year):max(sdl_jennings$Year))
# review
head(suding_hcn_jenningsdates) # starts Jan 1 1990
tail(suding_hcn_jenningsdates) # ends Dec 31 2013


# repeat for alternate temp dataset (infilled by metadata methods only 2010-on, no nsf data)
# > CTW: combine pcp and temp to get hcn df (cols: Year, Month, Day, TMIN, TMAX, PCP)
ctw_hcn <- full_join(temp2, pcp)
summary(ctw_hcn)
head(ctw_hcn) # starts at July 31 1981
tail(ctw_hcn) # ends at Dec 31 2018

# > CTW test: try adding in 0's for Jan 1 1981 - July 30 1981 so code will run without breaking using real July 31-Dec 1981 data
# will need to compare to Emily's numbers to make sure 0s didn't change anything fundamentally (their results should be kicked out in the code anyway since keep Sep and onwards only each year)
###  vars to use: Day Month Year TMIN TMAX PCP
dates <- seq(from = as.Date("1981-01-01"), to = as.Date("2018-12-31"), by = "day")
dates <- data.frame(`date` = dates) %>%
  mutate(Year = year(`date`),
         Month = month(`date`),
         Day = day(`date`))
ctw_hcn_new <- left_join(dates, ctw_hcn)
ctw_hcn_new$TMIN[is.na(ctw_hcn_new$TMIN)] <- 0
ctw_hcn_new$TMAX[is.na(ctw_hcn_new$TMAX)] <- 0
ctw_hcn_new$PCP[is.na(ctw_hcn_new$PCP)] <- 0
ctw_hcn <- ctw_hcn_new
# review
head(ctw_hcn)
tail(ctw_hcn)


# cr1000 backwards predicted dataset
cr1000clim_allyrs <- left_join(cr1000temp_allyrs, pcp)
# infill from Jan 1 1981 with 0 so AET will run on full year (can use true 1981 fall metrics)
cr1000_hcn_new <- left_join(dates, cr1000clim_allyrs)
cr1000_hcn_new$TMIN[is.na(cr1000_hcn_new$TMIN)] <- 0
cr1000_hcn_new$TMAX[is.na(cr1000_hcn_new$TMAX)] <- 0
cr1000_hcn_new$PCP[is.na(cr1000_hcn_new$PCP)] <- 0
cr1000_hcn <- cr1000_hcn_new
# review
head(cr1000_hcn)
tail(cr1000_hcn)


# cr logger dat using cr21x for 1980s infil
crall21xclim_allyrs <- left_join(crallT_1980cr21x, pcp)
# infill from Jan 1 1981 with 0 so AET will run on full year (can use true 1981 fall metrics)
crall21x_hcn_new <- left_join(dates, crall21xclim_allyrs)
crall21x_hcn_new$TMIN[is.na(crall21x_hcn_new$TMIN)] <- 0
crall21x_hcn_new$TMAX[is.na(crall21x_hcn_new$TMAX)] <- 0
crall21x_hcn_new$PCP[is.na(crall21x_hcn_new$PCP)] <- 0
crall21x_hcn <- crall21x_hcn_new
# review
head(crall21x_hcn)
tail(crall21x_hcn)


# cr logger dat using cr1000 for 1980s infil
crall1000clim_allyrs <- left_join(crallT_1980cr1000, pcp)
# infill from Jan 1 1981 with 0 so AET will run on full year (can use true 1981 fall metrics)
crall1000_hcn_new <- left_join(dates, crall1000clim_allyrs)
crall1000_hcn_new$TMIN[is.na(crall1000_hcn_new$TMIN)] <- 0
crall1000_hcn_new$TMAX[is.na(crall1000_hcn_new$TMAX)] <- 0
crall1000_hcn_new$PCP[is.na(crall1000_hcn_new$PCP)] <- 0
crall1000_hcn <- crall1000_hcn_new
# review
head(crall1000_hcn)
tail(crall1000_hcn)



# -- FINISHING -----
# write out hcn datasets
write.csv(sdl_jennings, paste0(datpath, "jennings/hcn_jennings.csv"), quote = F, row.names = F)
write.csv(d1_met, paste0(datpath, "d1/hcn_kittel.csv"), quote = F, row.names = F)
write.csv(suding_hcn, paste0(datpath, "suding/allyrs/hcn_suding.csv"), quote = F, row.names = F)
write.csv(suding_hcn_jenningsdates, paste0(datpath, "suding/sensitivity_subset/hcn_suding_19902013.csv"), quote = F, row.names = F)
write.csv(ctw_hcn, paste0(datpath, "ctw/hcn_ctw.csv"), quote = F, row.names = F)
write.csv(cr1000_hcn, paste0(datpath, "ctw/cr1000hcn_ctw.csv"), quote = F, row.names = F)
write.csv(crall21x_hcn, paste0(datpath, "ctw/crall21xhcn_ctw.csv"), quote = F, row.names = F)
write.csv(crall1000_hcn, paste0(datpath, "ctw/crall1000hcn_ctw.csv"), quote = F, row.names = F)
