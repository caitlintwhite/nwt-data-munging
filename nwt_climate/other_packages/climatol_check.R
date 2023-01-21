# revisit Climatol package
# prep daily for Climatol homogenization

# notes:
# Climatol will infill missing data, so be sure tables written out start at first date of target site for treatment
# here, CTW is focusing on Saddle treatment more than D1 or C1


# -- SETUP -----
# prep data for SDL, D1, and C1 as references
library(tidyverse)
library(lubridate)
theme_set(theme_test())

#climatol::rclimdex2climatol() can be used to convert but i'm lazy + impatient

# specify paths and list files for different data needed
# set path to all data prepped for inilling in losleben package
datpath_in <- "/Users/scarlet/Documents/nwt_lter/nwt_climate/data/"
qcdata <- list.files(paste0(datpath_in, "qc"), full.names = T)
# set path to write out Climatol prepped data
datpath_out <- "/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/Climatol/prepped_data/"

# path to gapfilled saddle temp and ppt (main dataset)
sdl_ongoing_temp <- readRDS(paste0(datpath_in, "homogenize/nwt_sdl_homogenized_temperature_draft.rds")) 

# nwt ppt prepped
c1_ongoing_ppt <- readRDS(list.files(paste0(datpath_in, "infill/"), pattern = "^c1P.*draft.rds", full.names = T))
d1_ongoing_ppt <- readRDS(list.files(paste0(datpath_in, "infill/"), pattern = "^d1P.*draft.rds", full.names = T))
sdl_ongoing_ppt <- readRDS(list.files(paste0(datpath_in, "infill/"), pattern = "^sdlP.*draft.rds", full.names = T))

# grab all ghcnd just in case need station info for stations that got subsetted out (came up below)
ghcndppt <- readRDS(qcdata[grep("ghcndP",qcdata)])



# -- DATA PREP ----
# must create stations file with format: 
# coords X (lat), Y (long), Z (elev), station code, station name
# use station info from losleben prep
siteinfo <- readRDS(paste0(datpath_in,"qc/siteinfoTEMP_qc.rds"))
# select and order columns needed for Climatol
stations_out <- subset(siteinfo, select = c(longitude, latitude, elevation, local_site, station_name)) %>%
  distinct()

# have ghcnd handy
ghcnd_stations <- subset(ghcndppt, select = c(longitude, latitude, elevation, station_id, station_name)) %>%
  distinct() %>%
  rename(local_site = station_id)
# which stations not in data used to infill/check SDL?
with(ghcnd_stations, local_site[!local_site %in% stations_out$local_site]) # fraser..station 116 is split by the time of observation

# add in the other fraser station 53113 (time never changed)
stations_out <- rbind(stations_out, subset(ghcnd_stations, grepl("53113$", local_site), select = names(stations_out)))



# all mets tidy, ordered by date, each station in its own column (wide)
# leave out met label and date when writing out, and only write out one met per file
# 1. prep ppt ----
allppt <- readRDS(paste0(datpath_in,"infill/allPPTdats_wNWTqdays.rds"))

# pull non-NWT ppt
otherppt <- subset(allppt, !grepl("^c1|^d1|^sdl", local_site), select = c(date, local_site, measurement))
# stack NWT ppt to incorporate
nwtppt <- subset(c1_ongoing_ppt, select = c(date, local_site, precip)) %>%
  # use non-winter adjusted for SDL so jump more apparent
  rbind(d1_ongoing_ppt[names(.)], sdl_ongoing_ppt[names(.)]) %>%
  # adjust local_site name so matches name in station
  mutate(local_site = paste0(casefold(local_site), "_chart")) %>%
  # subset to post-shield change dates (min date in other)
  subset(date >= min(otherppt$date))

allppt_daily <- rbind(nwtppt, rename(otherppt, precip = measurement)) %>%
  mutate(yr = year(date), mon = month(date)) %>%
  group_by(yr) %>%
  mutate(count_mon = length(unique(mon))) %>%
  ungroup()
# review month count -- climatol only wants full calendar years
with(allppt_daily, unique(yr[count_mon < 12])) # drop these
allppt_daily <- subset(allppt_daily, count_mon == 12)


# summarize monthlyy
allppt_monthly <- group_by(allppt_daily, yr, mon, local_site) %>%
  summarise(mon_ppt_wNA = sum(precip),
            mon_ppt = sum(precip, na.rm = T),
            nobs = length(precip),
            na_count = length(precip[is.na(precip)])) %>%
  ungroup()
# review NAs..
ggplot(subset(allppt_monthly, na_count > 0)) +
  geom_histogram(aes(mon_ppt), bins = 50) +
  facet_wrap(~na_count, scales = "free")
# look at month dist
ggplot(subset(allppt_monthly, na_count %in% 1:5), aes(factor(mon), mon_ppt, group = mon)) +
  geom_boxplot() +
  geom_point(aes(col = yr)) +
  facet_wrap(~na_count, scales = "free") # I guess it's okay to keep 5 days missing or less. I think ACMANT uses 7d or less.
ggplot(subset(allppt_monthly, na_count %in% 1:5), aes(factor(na_count), mon_ppt)) +
  geom_boxplot() +
  geom_point(aes(col = yr)) +
  facet_wrap(~local_site, scales = "free") # ok

# NA anything that has na_count > 5
allppt_monthly$mon_ppt[allppt_monthly$na_count > 5] <- NA
# anything whose nobs is < 28 needs to be NA as well (record keeping starts partway thru a month)
allppt_monthly$mon_ppt[allppt_monthly$nobs < 27] <- NA
# double check station names match in station key
summary(unique(allppt_daily$local_site) %in% stations_out$local_site)
unique(allppt_daily$local_site)[!unique(allppt_daily$local_site) %in% stations_out$local_site] # "USC00053113" was missing from key, only ran 64-74
# > it's Fraser, active 1908-1974...
# double check that Fraser 1964-174 dat ok
ggplot(subset(ghcndppt, grepl("FRA", station_name)), aes(date, measurement)) + geom_line() + facet_wrap(~station_id, scales = "free")
# seems ok to keep

# spread monthly and daily
allppt_daily <- spread(allppt_daily, local_site, precip)
allppt_monthly <- subset(allppt_monthly, select = c(yr, mon, local_site, mon_ppt)) %>%
  spread(local_site, mon_ppt) 



# -- PREP SADDLE -----
# for full calendar years, start Saddle at 1981 (it starts in July 1981.. would rather try to keep data than discard data [start at 1982])
range(sdl_ongoing_ppt$year)

sdlppt_daily <- subset(allppt_daily, yr %in% unique(sdl_ongoing_ppt$year)) %>% data.frame()
sdlppt_monthly <- subset(allppt_monthly, yr %in% unique(sdl_ongoing_ppt$year)) %>% data.frame()

# check for cols that are all NAs
dailyppt_sumcheck <- sapply(sdlppt_daily, function(x) round(100*(sum(is.na(x))/nrow(sdlppt_daily)),2))
sort(dailyppt_sumcheck) # this is why I dropped Fraser. I suppose keep the others because they might have needed temporal coverage, even if short
# drop Fraser that is entirely NA
dropstation <- names(dailyppt_sumcheck)[dailyppt_sumcheck == 100]
sdlppt_daily <- sdlppt_daily[!names(sdlppt_daily) %in% dropstation]
sdlppt_monthly <- sdlppt_monthly[!names(sdlppt_monthly) %in% dropstation]

sdl_rank <- subset(siteinfo, station_id == "sdl_chart" & paired_site %in% names(sdlppt_monthly)) %>%
  arrange(final_rank)
# order sdl dats by station ranking
sdlppt_daily <- sdlppt_daily[c("date", sdl_rank$paired_site)]
sdlppt_monthly <- sdlppt_monthly[c("yr", "mon", sdl_rank$paired_site)] %>% data.frame()

sdl_stations <- subset(stations_out, local_site %in% sdl_rank$paired_site) %>%
  mutate(local_site = factor(local_site, levels = sdl_rank$paired_site)) %>%
  arrange(local_site)
# fraser needs to have unique station names (indicate 700 and 1600)
sdl_stations$station_name[grepl("_0700", sdl_stations$local_site)] <- gsub("FRASER,", "FRASER 0700,", sdl_stations$station_name[grepl("_0700", sdl_stations$local_site)])
sdl_stations$station_name[grepl("_1600", sdl_stations$local_site)] <- gsub("FRASER,", "FRASER 1600,", sdl_stations$station_name[grepl("_1600", sdl_stations$local_site)])
#sdl_stations$station_name <- sdl_stations$local_site
sdl_stations <- data.frame(sdl_stations)

# -- ORDER STATION AND MET COLS -----
# make sure SDL is first, then other order doesn't matter because program takes spatial prox into account
# BUT ppt station list should only have stations used in ppt dat (not all stations)
ppt_stations <- names(subset(allppt_monthly, select = -c(yr,mon)))
sdl_rank <- subset(siteinfo, station_id == "sdl_chart" & paired_site %in% ppt_stations) %>%
  arrange(final_rank)

allppt_monthly_out <- allppt_monthly[,c(sdl_rank$paired_site, "USC00053113")]
allppt_daily_out <- allppt_daily[,c(sdl_rank$paired_site, "USC00053113")] 
pptstations_out <- subset(stations_out, local_site %in% ppt_stations) %>%
  mutate(local_site = factor(local_site, levels = c(sdl_rank$paired_site, "USC00053113")),
         # edit FRASER station names based on time collected
         station_name = ifelse(grepl("_1600", local_site), gsub("FRASER,", "FRASER 1600,", station_name),
                               ifelse(grepl("_0700", local_site), gsub("FRASER,", "FRASER 0700,", station_name), 
                                      station_name))) %>%
  arrange(local_site)

# try natural log of data
hist(log(allppt_monthly$sdl_chart))
hist(sqrt(allppt_monthly$USC00050674)) #sqrt is a better trans to normalize data


# -- WRITE OUT -----
# write to folder for data prepped for Climatol
write_csv(stations_out, paste0(datpath_out, "stations.txt"), na = "NA", col_names = F)



write.table(est.c, paste0(datpath_out, "Ttest_1981-2000.est"), row.names = F, col.names = F)


# PRECIP
write.table(pptstations_out, paste0(datpath_out, "DLYppt_1965-2021.est"), row.names = F, col.names = F)
write.table(pptstations_out, paste0(datpath_out, "MONppt_1965-2021.est"), row.names = F, col.names = F)
# check date range to write out with naming convention climatol wants
range(allppt_daily$yr)
write(as.matrix(allppt_daily[pptstations_out$local_site]), paste0(datpath_out, "DLYppt_1965-2021.dat"))
write(as.matrix(allppt_monthly_out[pptstations_out$local_site]), paste0(datpath_out, "MONppt_1965-2021.dat"))



# sdl years only
write.table(sdl_stations, paste0(datpath_out, "sdlDLYppt_1981-2021.est"), row.names = F, col.names = F)
write.table(sdl_stations, paste0(datpath_out, "sdlMONppt_1981-2021.est"), row.names = F, col.names = F)
# check date range before writing out tables
summary(sdlppt_daily$date)
write(as.matrix(sdlppt_daily[sdl_rank$paired_site]), paste0(datpath_out, "sdlDLYppt_1981-2021.dat"))
write(as.matrix(sdlppt_monthly[sdl_rank$paired_site]), paste0(datpath_out, "sdlMONppt_1981-2021.dat"))
write(as.matrix(sdlppt_monthly[sdlppt_monthly$yr > 1987, sdl_rank$paired_site]), paste0(datpath_out, "sdlMONppt_1987-2021.dat"))


# try 1988 onwards to see if corrs are better
write.table(subset(sdl_stations, !local_site %in% c("USC00055878", "USC00050674")), paste0(datpath_out, "sdlDLYppt_1988-2021.est"), row.names = F, col.names = F)
write(as.matrix(sdlppt_daily[year(sdlppt_daily$date) > 1987, sdl_rank$paired_site[!sdl_rank$paired_site %in% c("USC00055878", "USC00050674")]]), paste0(datpath_out, "sdlDLYppt_1988-2021.dat"))
sdl87 <- sdlppt_monthly[sdlppt_monthly$yr > 1987,sdl_rank$paired_site]
nobs87 <- sapply(sdl87, function(x) sum(is.na(x)))
sort(nobs87)
sdl87 <- sdl87[,nobs87 < 350]
write.table(subset(sdl_stations, !local_site %in% names(nobs87)[nobs87 > 350]), paste0(datpath_out, "sdlMONppt_1987-2021.est"), row.names = F, col.names = F)
write(as.matrix(sdl87), paste0(datpath_out, "sdlMONppt_1987-2021.dat"))

# example to verify I'm prepping correctly
write(dat, paste0(datpath_out, "Ttest_1981-2000.dat"))
