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
jennings <- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.168.1&entityid=d4385f9a680c860c9c444e021ea1a35d",
                     strip.white = T,
                     na.strings = na_vals)

# CTW: infilled 1982-2017 precip data (NSF renewal data [HH/EF] + ctw infilling 2015-2017)
suding_ppt <- read.csv("extended_summer/output_data/suding/sdl_ppt_infill_19822017_ctw.csv",
                       strip.white = TRUE, 
                       na.strings = na_vals)
# CTW: infilled 1982-2017 temp data (NSF data [HH/EF] + ctw infilling 2015-2017)
suding_temp <- read.csv("extended_summer/output_data/suding/sdl_temp_infilled_19822017_ctw.csv",
                        strip.white = T,
                        na.strings = na_vals)


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



# -- PREP HCN FOR AET -----
###   LOAD DATA FILE AND CREATE MATRICES:
###  tmin[ye,mo,da], tmax[ye,mo,da], pcp[ye,mo,da] 
###   == minimum daily temperature, maximum daily temperature and precip for the year, month, and day.
###     ye ranges from 1 to n.years.

# > CTW: combine pcp and temp to get hcn df (cols: Year, Month, Day, TMIN, TMAX, PCP)
suding_hcn <- full_join(temp, pcp)
summary(suding_hcn)
head(suding_hcn) # starts at July 31 1981
tail(suding_hcn) # ends at Dec 31 2017

# > CTW test: try adding in 0's for Jan 1 1981 - July 30 1981 so code will run without breaking using real July 31-Dec 1981 data
# will need to compare to Emily's numbers to make sure 0s didn't change anything fundamentally (their results should be kicked out in the code anyway since keep Sep and onwards only each year)
###  vars to use: Day Month Year TMIN TMAX PCP
dates <- seq(from = as.Date("1981-01-01"), to = as.Date("2017-12-31"), by = "day")
dates <- data.frame(`date` = dates) %>%
  mutate(Year = year(`date`),
         Month = month(`date`),
         Day = day(`date`))
suding_hcn_new <- left_join(dates, suding_hcn)
suding_hcn_new$TMIN[which(is.na(suding_hcn_new$TMIN))] <- 0
suding_hcn_new$TMAX[which(is.na(suding_hcn_new$TMAX))] <- 0
suding_hcn_new$PCP[which(is.na(suding_hcn_new$PCP))] <- 0
suding_hcn <- suding_hcn_new
# review
head(suding_hcn)
tail(suding_hcn)

suding_hcn_jenningsdates <- suding_hcn %>%
  subset(Year %in% min(sdl_jennings$Year):max(sdl_jennings$Year))
# review
head(suding_hcn_jenningsdates) # starts Jan 1 1990
tail(suding_hcn_jenningsdates) # ends Dec 31 2013


# -- FINISHING -----
# write out hcn datasets
write.csv(sdl_jennings, paste0(datpath, "jennings/hcn_jennings.csv"), quote = F, row.names = F)
write.csv(suding_hcn, paste0(datpath, "suding/hcn_suding.csv"), quote = F, row.names = F)
write.csv(suding_hcn_jenningsdates, paste0(datpath, "suding/hcn_suding_19902013.csv"), quote = F, row.names = F)
