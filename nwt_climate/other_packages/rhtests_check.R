# RClimDex + RHTests testing

# steps:


# -- SETUP -----
# prep data for SDL, D1, and C1 as references
library(tidyverse)
library(lubridate)
library(RClimDex)
theme_set(theme_test())

# source RHTests monthly and daily scripts
# latest RHtests: to check for breaks and homogenize daily temp, or monthly or yearly temp or prcp
source("/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/RHtests-master/V4_files/RHtestsV4_20190301.r")
#source("/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/RHtests-master/RHtestsV4_20180301.r")
# latest dly precip adjustment
source("/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/RHtests-master/V4_files/RHtests_dlyPrcp_20160621.r")
# script to convert data for RClimdex to RHtest format
source("/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/RHtests-master/V4_files/RClimDex_RHtest_ctw.r")

# set path to data
datpath <- "/Users/scarlet/Documents/nwt_lter/nwt_climate/data/"
infillfiles <- list.files(paste0(datpath, "infill"), full.names = T, )
homfiles <- list.files(paste0(datpath, "homogenize"), full.names = T)

# sdl infilled dats
# ppt sent to Jared
sdlppt <- readRDS(infillfiles[grep("sdlP.*rds$", infillfiles)])
# sdl chart infilled (don't remember if I QC'd this, but guessing I used the QC'd from site visit [too lazy to look back at code, not critical for testing RHtest])
sdltemp_chart <- readRDS(infillfiles[grep("sdlch.*MOdraft.r", infillfiles)])
# sdl ongoing temp sent to Meagan
sdltemp_hom <- readRDS(homfiles[grep("_hom.*rds$", homfiles)])

# get infilled C1 and D1 as well...
# temp from renewal (chart-based):
d1temp <- read_csv("nwt_climate/nwt8-renewal_homogenize_climdat/data/d1_temp_1952-2020_draft.csv")
c1temp <- read_csv("nwt_climate/nwt8-renewal_homogenize_climdat/data/c1_temp_1952-2020_draft.csv")

# dat tidy-prepped already by CTW for losleben package
d1ppt <- readRDS(infillfiles[grep("d1P.*draft.rds$", infillfiles)])
c1ppt <- readRDS(infillfiles[grep("c1P.*draft.rds$", infillfiles)])
loggers <- readRDS("/Users/scarlet/Documents/nwt_lter/nwt_climate/data/prep/nwtloggerTemp_prep.rds")
logger_qc <- readRDS("/Users/scarlet/Documents/nwt_lter/nwt_climate/data/qc/nwtloggerTEMP_qc.rds")
ppt <- readRDS("/Users/scarlet/Documents/nwt_lter/nwt_climate/data/prep/nwtchartPPT_prep.rds")


# -- PREP DATA ----
# choose dat that has the longest record as base
# RClimDex requires: year, month, dd, prcp, tmax, tmin
# make tidy by site so can subset as needed for writing out
# start with daily, then aggregate monthly (and maybe yearly for comparison)
# log transform aggregated ppt for comparison




# just for testing RClimDex, stitch chart to logger directly (see if picks up instrument change)
sdlmet_allyrs <- subset(sdltemp_hom, select = c(date:mon, airtemp_avg_gapfilled, airtemp_max_gapfilled, airtemp_min_gapfilled)) %>%
  rename_all(function(x) gsub("_gapfilled", "",x)) %>%
  rbind(sdltemp_chart[sdltemp_chart$date < min(sdltemp_hom$date), names(.)]) %>%
  arrange(date) %>%
  full_join(sdlppt[c("date", "precip")]) %>%
  mutate(dy = day(date),
         dat_source = "sdl ongoing") %>%
  dplyr::select(dat_source, date, yr, mon, dy, precip, airtemp_max, airtemp_min, airtemp_avg) %>%
  rename(year = yr,
         month = mon,
         day = dy,
         prcp = precip,
         tmin = airtemp_min,
         tmax = airtemp_max,
         tmean = airtemp_avg)

# compile all other sites
d1c1ppt <- subset(c1ppt, select = c(local_site:precip)) %>%
  rbind(subset(d1ppt, select = c(local_site:precip))) %>%
  mutate(month = month(date),
         day = day(date)) %>%
  rename(prcp = precip)

# deal with CTW drift shifts later. Just see how RHTests runs first
d1c1temp <- subset(c1temp, select = c(local_site:mean_temp)) %>%
  rbind(subset(d1temp, select = c(local_site:mean_temp))) %>%
  rename_at(names(.)[grepl("temp", names(.))], function(x) paste0("t", x)) %>%
  rename_all(function(x) gsub("_temp", "", x))

d1c1met_allyrs <- left_join(d1c1ppt, d1c1temp) %>%
  rename(dat_source = local_site) %>%
  dplyr::select(names(sdlmet_allyrs)) %>%
  mutate(dat_source = paste(casefold(dat_source), "ongoing"))
  

# write out and test RClimDex
write_csv(subset(sdlmet_allyrs, select = -c(dat_source, date,tmean)), "/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/climdex/sdl_input.csv", col_names = F)
write_csv(subset(d1c1met_allyrs, dat_source == "d1 ongoing", select = -c(dat_source, date,tmean)), "/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/climdex/d1_input.csv", col_names = F)
write_csv(subset(d1c1met_allyrs, dat_source == "c1 ongoing", select = -c(dat_source, date,tmean)), "/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/climdex/c1_input.csv", col_names = F)
# run climdex -- choose 5 SDs for temp
rclimdex.start()

# assuming basic qc checks all good (review output), run homogenization check
# un RClimDex_2_RHtests
RClimDex2RHtest("/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/climdex/sdl_input.csv", MissingStr = "NA")
RClimDex2RHtest("/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/climdex/d1_input.csv", MissingStr = "NA")
RClimDex2RHtest("/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/climdex/c1_input.csv", MissingStr = "NA")
# this preps all daily and monthly files

# run RHtests on monthly first
# > be sure to re-run RHTests script bc daily prcp script also has StartGUI() function and will load wrong GUI
StartGUI()

# run RHtests on daily ppt
# know this can take some minutes and will be silent on progress (i.e., the program is not necessarily timing out/not responding)
# re-run source prcp daily script first
StartGUI()

# note > daily UD check didn't produce after letting it run for +90min. Forced quit. Seems like a no go from the GUI, but haven't tried running directly in R from function (possibility)

rsdl_climdex_qc <- read_csv("/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/climdex/sdl_input.indcal.csv")
sdl_climdex_qc$date <- with(sdl_climdex_qc, as.Date(paste(year, month, day, sep = "-")))
sdltemp_allyrs$date <- with(sdltemp_allyrs, as.Date(paste(year, month, day, sep = "-")))
# is anything different?
summary(sdl_climdex_qc$prcp[sdl_climdex_qc$date %in% sdltemp_allyrs$date] == sdltemp_allyrs$prcp)
summary(sdl_climdex_qc$prcp[sdl_climdex_qc$date %in% sdltemp_allyrs$date] == sdltemp_allyrs$prcp)


# prep data for RHtests v4
# needs:
# YYYY, MM, DD, value (each metric in its own file; reference dat too [try D1])
# recommends testing on monthly values first

monthlySDL_met <- group_by(sdlmet_allyrs, year, month) %>%
  summarise(tmax = max(tmax),
            tmin = min(tmin),
            tmean = mean(tmean),
            prcp = sum(prcp),
            nobs = length(date),
            day = 00) %>%
  ungroup()

gather(monthlySDL_met, met, val, tmax:prcp) %>%
  group_by(met) %>%
  mutate(rolling = zoo::rollmean(val, k = 6, fill = NA)) %>%
  ggplot(aes(as.Date(paste(year, month, 1, sep = "-")), val)) +
  geom_line(col = "grey50", alpha = 0.5) +
  geom_point(col = "grey30") +
  geom_line(aes(as.Date(paste(year, month, 1, sep = "-")), rolling, col = met), alpha = 0.75, lwd = 1) +
  geom_smooth(method = "gam", col = "black") +
  facet_wrap(~met, scales = "free")

# want all years available for prcp
sdlmon_prcp <- mutate(sdlppt, mon = month(date), dd = 00) %>%
  dplyr::select(year, mon, dd, date, precip) %>%
  group_by(year, mon, dd) %>%
  summarise(prcp = sum(precip),
            nobs = length(date)) %>%
  ungroup() %>%
  subset(nobs > 25)# remove july 1981 but keep possible check on other months low (manual check)

# plot precip to see full trend
mutate(sdlmon_prcp, rolling = zoo::rollmean(prcp, k = 6, fill = NA)) %>%
  ggplot(aes(as.Date(paste(year, mon, 1, sep = "-")), prcp)) +
  geom_line(col = "grey50", alpha = 0.5) +
  geom_point(col = "grey30") +
  geom_line(aes(as.Date(paste(year, mon, 1, sep = "-")), rolling), col = "red", alpha = 0.5, lwd = 1) +
  geom_line(aes(as.Date(paste(year, mon, 1, sep = "-")), zoo::rollmean(prcp, k = (3*12), fill = NA)), col = "purple", lwd = 1, alpha = 0.75) +
  geom_smooth(method = "gam", col = "black") +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
  theme_bw()

sdlmon_prcp_ln  <- mutate(sdlmon_prcp, prcp = log(prcp)) 
  
write_csv(subset(monthlySDL_met, select = c(year, month, day, tmax)), col_names = FALSE,
          "/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/RHtests_datfiles/sdl_mon_tmax.csv")
write_csv(subset(monthlySDL_met, select = c(year, month, day, tmin)), col_names = FALSE,
          "/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/RHtests_datfiles/sdl_mon_tmin.csv")
write_csv(subset(monthlySDL_met, select = c(year, month, day, tmean)), col_names = FALSE,
          "/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/RHtests_datfiles/sdl_mon_tmean.csv")
write_csv(subset(sdlmon_prcp, select = c(year, mon, dd, prcp)), col_names = FALSE,
          "/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/RHtests_datfiles/sdl_mon_prcp.csv")
write_csv(subset(sdlmon_prcp_ln, select = c(year, mon, dd, prcp)), col_names = FALSE,
          "/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/RHtests_datfiles/sdl_mon_prcpln.csv")

library(hydroTSM)
data(SanMartinoPPts)
x <- SanMartinoPPts
#Getting the monthly ts
pcp.m <- daily2monthly(x, FUN=sum, na.rm=FALSE)
## Not run:
# From zoo to the input format required by 'FindU.dlyPrcp' function
zoo2RHtest(x=pcp.m, fname="pcp-monthly.txt", tstep.out="monthly", na="-999.0")
# Homogeneity analysis
FindU.dlyPrcp(InSeries="pcp-monthly.txt", output="pcp-monthly", MissingValueCode="-999.0",
              GUI=T, pthr=0, Mq=10, p.lev=0.95, Iadj=10000)
