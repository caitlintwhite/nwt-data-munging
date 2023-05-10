# pull data to design cars for NWT outreach


# to show:
# SDL PPT off (annual records)

# Daily errors

# -- SETUP -----
library(tidyverse)
library(lubridate)
library(rgdal)
library(readxl)
library(cowplot)
theme_set(theme_test())
options(stringsAsFactors = F)

# path to write out figs
figpath <- "/Users/scarlet/Documents/cu_boulder/Fall\ 2022/SciComm/figs/"

# specify path to data
datpath <- "/Users/scarlet/Documents/nwt_lter/nwt_climate/data/"
infill_dats <- list.files(paste0(datpath, "infill"), full.names = T)
qc_dats <- list.files(paste0(datpath, "qc"), full.names = T)
prev_qcdats <- list.files("../long-term-trends/extended_summer/analysis/output_data/prep_data/", full.names = T)

# read in D1, C1, SDL corrected data
# temp
sdl_temp <- readRDS(paste0(datpath, "homogenize/nwt_sdl_homogenized_temperature_draft.rds"))
c1_temp <- read_csv("nwt_climate/nwt8-renewal_homogenize_climdat/data/c1_temp_1952-2020_draft.csv")
d1_temp <- read_csv("nwt_climate/nwt8-renewal_homogenize_climdat/data/d1_temp_1952-2020_draft.csv")
sdl_chart_temp <- readRDS(infill_dats[grep("sdlch.*MOdraft", infill_dats)])
sdl_logger_temp <- readRDS(infill_dats[grep("sdllog.*MOdraft", infill_dats)])
sdl_chart_temp_qc <- read_csv(prev_qcdats[grepl("qa_sdlch", prev_qcdats)])
nwt_logger_temp_qc <- readRDS(qc_dats[grep("nwtloggerT", qc_dats)])

#ppt
sdl_ppt <- readRDS(infill_dats[grep("sdlPP.*draft.rds", infill_dats)])
d1_ppt <- readRDS(infill_dats[grep("d1P.*draft.rds", infill_dats)])
c1_ppt <- readRDS(infill_dats[grep("c1P.*draft.rds", infill_dats)])
all_ppt <- readRDS(infill_dats[grep("allP.*rds", infill_dats)])

#site info, w rankings
siteinfo_ppt <- readRDS(qc_dats[grepl("infoPPT", qc_dats)])
siteinfo <- subset(siteinfo_ppt, select = c(station_id:elevation)) %>%
  distinct() %>%
  mutate(local_site = ifelse(grepl("^US", station_id), station_id, station_name),
         local_site = gsub("-", "_", local_site),
         local_site = ifelse(grepl("NR1", local_site), paste0(local_site, "_1_1_1"), gsub(" ", "", local_site)))
siteinfo$CD <- with(siteinfo, ifelse(grepl("FRA|BER|GRAN|HIGH", station_name), "West", "East"))
siteinfo$CD <- factor(siteinfo$CD, levels = c("West", "East"))
# assign middle region too
siteinfo$CD2 <- with(siteinfo, ifelse(longitude <= unique(longitude[local_site == "c1"]) & longitude >= unique(longitude[local_site == "d1"]),
                                      "Middle", as.character(CD)))
siteinfo$CD2 <- factor(siteinfo$CD2, levels = c("West", "Middle", "East"))
# make simple site name for plotting
siteinfo <- mutate(siteinfo, simple_name = ifelse(grepl("chart", station_id), casefold(station_name, upper = T), station_name))
siteinfo$simple_name[grepl("US-NR1", siteinfo$station_id)] <- "Niwot Ridge Forest (AmeriFlux)"
siteinfo$simple_name <- gsub(", CO US", "", siteinfo$simple_name)
siteinfo$simple_name <- gsub("ALLENSPARK", "Allenspark", siteinfo$simple_name)
siteinfo$simple_name <- gsub("COAL CREEK CANYON", "Coal Creek Canyon", siteinfo$simple_name)
siteinfo$simple_name <- gsub("LAKE", "Lake", siteinfo$simple_name)
siteinfo$simple_name <- gsub("PARK", "Park", siteinfo$simple_name)
siteinfo$simple_name <- gsub("ESTES", "Estes", siteinfo$simple_name)
siteinfo$simple_name <- gsub("WINTER", "Winter", siteinfo$simple_name)
siteinfo$simple_name <- gsub("BOULDER", "Boulder", siteinfo$simple_name)
siteinfo$simple_name <- gsub("GRAND", "Grand", siteinfo$simple_name)
siteinfo$simple_name <- gsub("NEDERLAND", "Nederland", siteinfo$simple_name)
siteinfo$simple_name <- gsub("BERTHOUD PASS", "Berthoud Pass", siteinfo$simple_name)
siteinfo$simple_name[grepl("53116", siteinfo$station_id)] <- "Fraser 53116"
siteinfo$simple_name[grepl("53113", siteinfo$station_id)] <- "Fraser 53113"
siteinfo$simple_name[siteinfo$station_id == "663"] <- "Niwot (Snotel)"
siteinfo$simple_name[grepl("sdl", siteinfo$station_id)] <- "Saddle (NWT LTER, tundra)"
siteinfo$simple_name[grepl("c1", siteinfo$station_id)] <- "C1 (NWT LTER, forest)"
siteinfo$simple_name[grepl("d1", siteinfo$station_id)] <- "D1 (NWT LTER, alpine)"


# other temp data
ghcnd_temp <- readRDS(qc_dats[grepl("ghcndT", qc_dats)])
ameriflux_temp <- readRDS(qc_dats[grepl("fluxTEMP", qc_dats)])
temp_sites <- readRDS(qc_dats[grepl("infoTEM", qc_dats)])
snotel_temp <- readRDS(qc_dats[grepl("snotelTEMP", qc_dats)])

# pull ghcnd and ameriflux sites to be sure all there (some extra ones used for temp)
ghcnd_sites <- subset(ghcnd_temp, select = c(yr, station_id:elevation)) %>%
  group_by(station_id, station_name) %>%
  filter(yr == max(yr) | yr == min(yr)) %>%
  distinct() %>%
  mutate(yrgrp = ifelse(yr == min(yr), "Begin", "End")) %>%
  ungroup() %>%
  spread(yrgrp, yr)

# silver lake and others in TK's dat not there
ghcnd_stations <- read_csv("/Users/scarlet/Documents/nwt_lter/nwt_climate/data/raw/GHCNd/otherfiles/stations.csv")

addstations <- subset(ghcnd_stations, !STATION %in% siteinfo$station_name)
names(addstations) <- casefold(names(addstations))
addstations <- subset(addstations, select = -c(begin_date, end_date, state, country))
addstations <- rename(addstations, elevation = 'elevation_(m)')
addstations$station_id <- gsub("GHCND:" , "", addstations$station_id)
addstations$local_site <- addstations$station_id
addstations$CD <- "East"
addstations$simple_name <- addstations$station
addstations$simple_name <- gsub(", CO US", "", addstations$simple_name)
addstations$simple_name <- gsub("TABERNASH", "Tabernash", addstations$simple_name)
addstations$simple_name <- gsub("CARIBOU RANCH", "Caribou Ranch", addstations$simple_name)
addstations$simple_name <- gsub("SILVER LAKE", "Silver Lake", addstations$simple_name)
addstations$simple_name <- gsub("NEDERLAND", "Nederland", addstations$simple_name)
addstations$CD2 <- with(addstations, ifelse(longitude <= unique(siteinfo$longitude[siteinfo$local_site == "c1"]) & longitude >= unique(siteinfo$longitude[siteinfo$local_site == "d1"]),
                                            "Middle", as.character(CD)))
addstations <- rename(addstations, station_name = station)

boulderstation <- data.frame(station_id = "USC00050848",
                             station_name = "BOULDER, CO US",
                             latitude = 39.992772,
                             longitude = -105.266163,
                             elevation = 1671.5,
                             local_site = "USC00050848",
                             CD = "East",
                             CD2 = "East",
                             simple_name = "City of Boulder")

# pull tvan east and west, gl4
other_nwt_sites <- distinct(subset(temp_sites, grepl("NR3|NR4|gl4_cr10$", local_site), select = c(station_id:source)))
other_nwt_sites$CD <- "East"
other_nwt_sites$CD2 <- "Middle"
addstations$CD2 <- factor(addstations$CD2, levels = c("West", "Middle", "East"))
other_nwt_sites[grepl("gl4", other_nwt_sites$station_id), c("station_id", "local_site")] <- "Green Lake 4"
other_nwt_sites$simple_name <- other_nwt_sites$station_name
other_nwt_sites$simple_name[grepl("gl4", other_nwt_sites$station_name)] <- "Green Lake 4 (NWT LTER, alpine lake)"

# ctw boulder climate dataset for monthly extremes
ctw_bldr <- read_csv("../bosmp-grassland-analyses/dat/boulder_daily_climate_metric_infilled.csv")




# -- WRITE OUT KML FOR REFERENCE SITES -----
# boulder station coords: 39.992772,-105.266163, GHCND:USC00050848, 1671.5 m elevation, BOULDER CO, US
allsites_kml <- subset(siteinfo, select = -c(CD, CD2)) %>%
  rbind(boulderstation[names(.)]) %>%
  rbind(addstations[names(.)]) %>%
  rbind(other_nwt_sites[names(.)]) %>%
  mutate(data_source = ifelse(grepl("NWT LTER", simple_name), "Niwot Ridge LTER", "NOAA GHCND"))
allsites_kml$data_source[grepl("Saw|Niwot$|Univ|Lake Eld|High Lo", allsites_kml$station_name)] <- "NRCS SNOTEL"
allsites_kml$data_source[grepl("^US-", allsites_kml$station_id)] <- "AmeriFlux"
allsites_kml <- data.frame(allsites_kml)
allsites_kml_out <- SpatialPointsDataFrame(coords = allsites_kml[c("longitude", "latitude")],
                                           data = allsites_kml,
                                           proj4string = CRS("+proj=longlat +datum=WGS84"))

plot(allsites_kml_out)
# writeOGR(allsites_kml_out, layer = 'allsites',
#          "/Users/scarlet/Documents/cu_boulder/Fall\ 2022/SciComm/allsites.kml", 
#          driver="KML")



# -- PREP BOULDER HISTORICAL DATA TOO ----
# can read in monthly summaries because don't need dailies for summaries?
boulderWx <- read_table("https://psl.noaa.gov/boulder/data/boulderdaily.complete.txt",skip_empty_rows = T,  skip = 1,
                        col_names = c("year", "mon", "day", "tmax", "tmin", "precip", "snow", "snowcover")) %>%
  data.frame()
str(boulderWx)
# convert -998 to missing
boulderWx <- mutate_all(boulderWx, function (x) ifelse(x == -998, NA, x))
# -999 to trace: Trace values (less than 1/10 for snow and 1/100 for rain) are indicated by -999.0.
boulderWx$precip[boulderWx$precip == -999 & ! is.na(boulderWx$precip)] <- 1/100
boulderWx$snow[boulderWx$snow == -999 & ! is.na(boulderWx$snow)] <- 1/10
# remove NA rows
boulderWx <- subset(boulderWx, !is.na(year))

# how many NAs are there (I only have 1990 onwards infilled)
missingBoulder <- boulderWx %>%
  gather(met, val, tmax:snowcover) %>%
  group_by(year, met) %>%
  summarise(missing = sum(is.na(val))) %>%
  ungroup()
ggplot(missingBoulder) +
  geom_point(aes(year, missing)) +
  facet_wrap(~met, scales = "free_y")

month_convert <- data.frame(mon = casefold(month.abb, upper = T),
                            monnum = 1:12)
bldrx <-"/Users/scarlet/Documents/cu_boulder/Fall\ 2022/SciComm/boulder_monthlies.xlsx"
bldr_ppt <- read_excel(bldrx, sheet = "ppt") %>%
  # convert trace to 1/10 inch
  mutate_all(function(x) ifelse(grepl("^T",x), 1/10, x)) %>%
  mutate_if(is.character, function(x) parse_number(x)) %>%
  gather(mon, val, JAN:DEC) %>%
  rename(ann_ppt = 'YEAR TOTAL') %>%
  mutate(metric = "precip",
         # convert ppt to mm
         val = val * 25.4,
         ann_ppt = ann_ppt * 25.4) %>%
  left_join(month_convert)

bldr_tmax <- read_excel(bldrx, sheet = "tmax", na = c("X", "Miss", "MISS", NA)) %>%
  mutate(metric = "tmax")
bldr_tmin <- read_excel(bldrx, sheet = "tmin", na = c("X", "Miss", "MISS", NA)) %>%
  mutate(metric = "tmin")
bldr_tmean <- read_excel(bldrx, sheet = "tmean", na = c("X", "Miss", "MISS", NA)) %>%
  mutate(metric = "tmean")
bldr_monT <- rbind(bldr_tmin, bldr_tmax, bldr_tmean) %>%
  gather(mon, val, JAN:DEC) %>%
  left_join(month_convert) %>%
  # convert F to C
  mutate(val = parse_number(val),
         val = (val-32)*(5/9))

bldrmon_met <- subset(bldr_monT) %>%
  rbind(bldr_ppt[names(.)]) %>%
  mutate(plotdate = as.Date(paste(Year, monnum, 1, sep = "-")))

# check to see how it looks
ggplot(bldrmon_met, aes(plotdate, val)) +
  geom_line() +
  facet_wrap(~metric, scales = "free_y")

ggplot(bldrmon_met, aes(factor(monnum), val)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = "free_y")

bldr_mon4nwt <- subset(bldrmon_met, select = -mon) %>%
  rename(yr = Year, mon = monnum) %>%
  mutate(metric = ifelse(!metric %in%  c("tmean", "precip"), paste0("avg", metric), metric),
         plotdate = as.Date(paste(yr, mon, 1, sep = "-"))) %>%
  spread(metric, val) %>%
  #make placeholder extremes
  mutate(tmax = NA,
         tmin = NA) %>%
  rename(mon_ppt = precip)

ctw_mon_bldr <- group_by(ctw_bldr, year, mon) %>%
  summarise(avgtmin = mean(tmin),
            avgtmax = mean(tmax),
            tmean = mean(tmean),
            tmax = max(tmax),
            tmin = min(tmin),
            mon_ppt = sum(precip),
            nobs = length(day)) %>%
  ungroup() %>%
  rename(yr = year) %>%
  mutate(plotdate = as.Date(paste(yr, mon, 1, sep = "-")))


bldr_mon4nwt_clean <- subset(bldr_mon4nwt, !plotdate %in% unique(ctw_mon_bldr$plotdate)) %>%
  rbind(ctw_mon_bldr[names(.)]) %>%
  arrange(plotdate)



# -- SUMMARIZE REGIONAL PPT -------
# swap out infilled NWT for unfilled in allppt dat
# summarize NWT ppt
nwt_ppt <- subset(sdl_ppt, select =c(local_site:date, precip_winteradj)) %>%
  rename(precip = precip_winteradj) %>%
  rbind(d1_ppt[names(.)], c1_ppt[names(.)]) %>%
  rename(yr = year) %>%
  mutate(mon = month(date), doy = yday(date),
         local_site = casefold(local_site))


# order sites by elev
elev_order <- siteinfo$local_site[order(siteinfo$elevation)]

# make days of month reference
monLUT <- data.frame(date = seq.Date(as.Date("2022-01-01"), as.Date("2022-12-31"), 1)) %>%
  mutate(mon = month(date),
         days = day(date)) %>%
  group_by(mon) %>%
  filter(days == max(days)) %>%
  ungroup()

# summarize monthly and yearly for nonNWT ppt
regional_ppt <- subset(all_ppt, !grepl("^sdl|^c1|^d1", local_site, ignore.case = T), select = -c(metric, qdays, raw)) %>%
  rename(precip = measurement) %>%
  rbind(nwt_ppt[names(.)]) %>%
  # ignore time diff for visualizing monthly and yr susms of stations (i.e., TOBS switch at Fraser)
  mutate(local_site = gsub("_[0-9]{4}$", "", local_site)) %>%
  group_by(local_site, yr, mon) %>%
  # count non-NA nobs
  mutate(nobs = length(precip[!is.na(precip)])) %>%
  ungroup() %>%
  # only summarize mons that have at least 27 days of data
  left_join(monLUT[c("mon", "days")]) %>%
  mutate(prop_present = nobs/days)

# look at dist of days available per month by station
ggplot(distinct(regional_ppt, local_site, yr, mon, nobs, prop_present), aes(factor(mon), prop_present, group = mon)) +
  geom_violin() +
  facet_wrap(~local_site)
# let's say, need 88% of obs nonNA (would be 25 days for Feb, 27 for other months)


regional_mon_ppt <- group_by(regional_ppt, local_site, yr, mon, nobs, days, prop_present) %>%
  summarise(mon_ppt = sum(precip, na.rm = T)) %>%
  ungroup() %>%
  mutate(plotdate = as.Date(paste(yr, mon, 1, sep= "-")))

# plot by nobs
ggplot(regional_mon_ppt, aes(plotdate, mon_ppt, col = nobs)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "gam") +
  facet_wrap(~local_site, scales = "free")

# drop ppt for prop < .88
regional_mon_ppt <- mutate(regional_mon_ppt, mon_ppt = ifelse(prop_present < 0.88, NA, mon_ppt))

ggplot(regional_mon_ppt, ) +
  geom_bin2d(aes(factor(mon), mon_ppt)) +
  geom_smooth(aes(mon, mon_ppt), col = "snow") +
  scale_fill_viridis_c()

regional_mon_ppt$local_site_elev <- factor(regional_mon_ppt$local_site, levels = rev(elev_order))
regional_mon_ppt <- left_join(regional_mon_ppt, siteinfo)
regional_mon_ppt$wymon <- with(regional_mon_ppt, ifelse(mon %in% 10:12, mon-9, mon+3))

site_labels <- unique(regional_mon_ppt$simple_name[order(regional_mon_ppt$local_site_elev)])
ggplot(regional_mon_ppt) +
  #geom_bin2d(aes(factor(mon), mon_ppt)) +
  #geom_smooth(aes(wymon, mon_ppt, col = local_site_elev, fill = local_site_elev), se = F) +
  #stat_summary(aes(wymon, mon_ppt, col = local_site_elev, fill = local_site_elev), alpha = 0.8, position = position_dodge(width = 0.2)) +
  stat_summary(aes(wymon, mon_ppt, col = local_site_elev, fill = local_site_elev, lwd = local_site_elev %in% c("d1", "c1", "sdl")), fun = mean, geom = "area", alpha = 0.15) + #position = position_dodge(width = 0.2)
  labs(x = NULL, y = "Average monthly precipitation (mm)",
       caption = "Niwot Ridge stations in bold outline") +
  scale_color_viridis_d(name ="Site, by elevation (high to low)", direction = 1, 
                        labels = site_labels) +
  scale_fill_viridis_d(name ="Site, by elevation (high to low)", direction = 1,
                       labels = site_labels) +
  guides(fill = guide_legend(ncol = 1), 
         color = guide_legend(ncol = 1)) +
  # scale_color_brewer(name ="Site, by elevation (high to low)", palette = "BuPu", direction = 1) +
  # scale_fill_brewer(name ="Site, by elevation (high to low)", palette = "BuPu", direction = 1) +
  scale_size_manual(values= c(0.5,1.5), guide = NULL) +
  #scale_x_continuous(expand = c(0,0), breaks = seq(1,12, 1), labels = c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S")) +
  scale_x_continuous(expand = c(0,0), breaks = seq(2,12, 2), labels = c("Nov", "Jan", "Mar", "May", "Jul", "Sep")) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~CD2, labeller = as_labeller(c('West' = "West of Divide", 'Middle' = "Below Divide, east", 'East' = "Further east of Divide"))) +
  theme(strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(face ="bold"),
        plot.caption = element_text(hjust = 0))

# -- print this -----
# ggsave(paste0(figpath, "monthlycompare.pdf"), width = 8.5, height = 6, units = "in")
# ggsave(paste0(figpath, "monthlycompare.png"), width = 8.5, height = 6, units = "in")

regional_wy_ppt <- regional_mon_ppt %>%
  mutate(wy = ifelse(mon %in% 10:12, yr+1, yr)) %>%
  group_by(local_site_elev, local_site, CD, CD2, wy) %>%
  summarise(wy_ppt = sum(mon_ppt), # anything with missing month will be NA)
            nobs = length(mon)) %>% # ct months to be sure 
  ungroup() %>%
  # NA anything not 12 months
  mutate(wy_ppt = ifelse(nobs < 12, NA, wy_ppt))

ggplot(regional_wy_ppt, aes(wy, wy_ppt, col = local_site_elev)) +
  geom_line(aes(wy, zoo::rollmean(wy_ppt, 3, fill = NA), group = local_site_elev)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_d() +
  facet_wrap(~CD2)

ggplot(subset(regional_wy_ppt, CD2 == "Middle" & !(local_site == "sdl" & wy < 1988)), aes(wy, ymin = 0, ymax = wy_ppt, group = local_site_elev, fill = local_site_elev, col = local_site_elev)) +
  #geom_line(alpha = 0.5) +
  #geom_line(aes(wy, zoo::rollmean(wy_ppt, 5, align = "right", fill = NA), group = local_site_elev), lwd = 2) +
  geom_ribbon(alpha = 0.15) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  facet_wrap(~!local_site_elev %in% c("d1", "sdl", "c1"), nrow = 2)

ggplot(subset(regional_wy_ppt, grepl("d1|c1|sdl", local_site) & !(local_site == "sdl" & wy < 1988)), aes(wy, wy_ppt, col = local_site_elev)) +
  geom_line(alpha = 0.25) +
  geom_line(aes(wy, zoo::rollmean(wy_ppt, 5, align = "right", fill = NA), group = local_site_elev), lwd = 1.5, alpha = 0.75) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = NULL, values = c("blue", "purple", "forestgreen")) +
  labs(y = "Total precipitation (mm)", x = "Water year (Oct 1 - Sep 30)") +
  scale_fill_viridis_d() +
  theme(legend.position = c(0.05,0.9),
        legend.justification = "left")



# -- SUMMARIZE REGIONAL TEMP -------
c1d1temp <- rbind(c1_temp, d1_temp) %>%
  rename(yr = year, dtr = DTR, avg_temp = mean_temp) %>%
  rename_all(function(x) gsub("_temp", "", x)) %>%
  mutate(mon = month(date), doy = yday(date))

# summarize NWT ppt
nwt_temp <- subset(sdl_temp, select =c(local_site, date:dtr_homogenized)) %>%
  rename_all(function(x) gsub("airtemp_", "", x)) %>%
  rename_all(function(x) gsub("_homogenized", "", x)) %>%
  rbind(c1d1temp[names(.)]) %>%
  mutate(local_site = casefold(local_site))

nwt_temp_mon <-group_by(nwt_temp, local_site, yr, mon) %>%
  summarise(tmin = min(min),
            tmax = max(max),
            avgtmax = mean(max),
            avgtmin = mean(min),
            tmean = mean(avg)) %>%
  ungroup() %>%
  left_join(siteinfo) %>%
  mutate(local_site_elev = factor(local_site, levels = c("d1", "sdl", "c1")))


ggplot(nwt_temp, aes(date, avg, col = local_site)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = NULL, values = c("blue", "purple", "forestgreen"))

nwt_temp_mon %>%
  gather(metric, temp, tmin:tmean) %>%
  mutate(plotdate = as.Date(paste(yr, mon, 1, sep = "-"))) %>%
  group_by(local_site, metric) %>%
  mutate(rollingtemp = zoo::rollmean(temp, 6, "right", fill = NA)) %>%
  ggplot(aes(plotdate, temp, col = local_site_elev)) +
  geom_point(alpha = 0.25) +
  geom_line(aes(plotdate, rollingtemp), lwd = 1, alpha = 0.5) +
  scale_color_manual(name = NULL, values = c("blue", "purple", "forestgreen")) +
  facet_wrap(~metric, nrow = 3, scales = "free_y")


nwt_temp_mon %>%
  gather(metric, temp, tmin:tmean) %>%
  mutate(plotdate = as.Date(paste(yr, mon, 1, sep = "-"))) %>%
  group_by(local_site, metric) %>%
  mutate(rollingtemp = zoo::rollmean(temp, 12, "center", fill = NA)) %>%
  ggplot(aes(plotdate, temp, col = local_site_elev)) +
  #geom_point(alpha = 0.25) +
  geom_line(aes(plotdate, rollingtemp), lwd = 1, alpha = 0.5) +
  scale_color_manual(name = NULL, values = c("blue", "purple", "forestgreen")) +
  facet_wrap(~metric, nrow = 3, scales = "free_y")


nwt_temp_mon %>%
  gather(metric, temp, tmin:tmean) %>%
  mutate(plotdate = as.Date(paste(yr, mon, 1, sep = "-"))) %>%
  group_by(local_site, metric) %>%
  mutate(rollingtemp = zoo::rollmean(temp, 6, "right", fill = NA)) %>%
  ggplot(aes(mon, temp)) +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey50") +
  geom_violin(aes(group = paste(mon, local_site_elev), fill = local_site_elev), alpha = 0.5) +
  geom_smooth(aes(col = local_site_elev, fill = local_site_elev,)) +
  scale_color_manual(values = c("blue", "purple", "forestgreen")) +
  scale_fill_manual(values = c("blue", "purple", "forestgreen")) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~metric, nrow = 3, scales = "free")

nwt_temp_mon %>%
  gather(metric, temp, tmin:tmean) %>%
  mutate(plotdate = as.Date(paste(yr, mon, 1, sep = "-"))) %>%
  group_by(local_site, metric) %>%
  mutate(rollingtemp = zoo::rollmean(temp, 6, "right", fill = NA)) %>%
  ggplot(aes(mon, temp)) +
  geom_boxplot(aes(group = paste(mon, metric), fill = metric), alpha = 0.5) +
  geom_smooth(aes(group = metric, col = metric, fill = metric)) +
  #scale_fill_manual(values = c("blue", "purple", "forestgreen")) +
  facet_wrap(~local_site_elev)

nwt_temp %>%
  group_by(local_site, yr) %>%
  summarise(tmax = max(max), 
            tmin = min(min),
            avgtmax = mean(max),
            avgtmin = mean(min),
            tmean = mean(avg),
            nobs = length((avg))) %>%
  subset(nobs > 360, select = -nobs) %>%
  gather(metric, temp, tmax:tmean) %>%
  group_by(local_site, metric) %>%
  mutate(rollingtemp = zoo::rollmean(temp, 5, "right", fill = NA)) %>%
  ggplot(aes(yr, temp, col = local_site)) +
  geom_point(alpha = 0.25) +
  geom_line(alpha = 0.25) +
  #geom_line(aes(yr, rollingtemp), lwd = 1, alpha = 0.5) +
  scale_color_manual(name = NULL, values = c("blue", "purple", "forestgreen")) +
  facet_wrap(~metric, nrow = 3, scales = "free_y")


# plot all mets by site to be sure at least that is logical

nwt_temp %>%
  group_by(local_site, yr) %>%
  summarise(tmax = max(max), 
            tmin = min(min),
            avgtmax = mean(max),
            avgtmin = mean(min),
            tmean = mean(avg),
            nobs = length((avg))) %>%
  subset(nobs > 360, select = -nobs) %>%
  subset(yr > 1985) %>%
  gather(metric, temp, tmax:tmean) %>%
  mutate(local_site = factor(local_site, levels = c("c1", "sdl", "d1"))) %>%
  group_by(local_site, metric) %>%
  mutate(rollingtemp = zoo::rollmean(temp, 5, "right", fill = NA)) %>%
  ggplot(aes(yr, temp, col = metric)) +
  geom_point(alpha = .95) +
  geom_line(alpha = 0.95) +
  #geom_line(aes(yr, rollingtemp), lwd = 1, alpha = 0.5) +
  #scale_color_manual(name = NULL, values = c("blue", "purple", "forestgreen")) +
  facet_wrap(~local_site, nrow = 3, scales = "free_y")



# -- VISUALIZE MAIN SUMMARY FIGS -----
figpath <- "/Users/scarlet/Documents/cu_boulder/Fall\ 2022/SciComm/figs/"
# c1, d1, sdl, and boulder
bldr_mon4nwt_clean <- mutate(bldr_mon4nwt_clean, local_site = "Boulder", simple_name = "City of Boulder")

mon_met_all <- left_join(nwt_temp_mon, regional_mon_ppt) %>%
  subset(select = c(local_site, simple_name, yr:tmean, mon_ppt, plotdate)) %>%
  #rbind(bldr_mon4nwt_clean[names(.)]) %>%
  arrange(plotdate) %>%
  mutate(wy = ifelse(mon %in% 10:12, yr+1, yr),
         wymon = ifelse(mon %in% 10:12, mon-9, mon +1),
         local_site_elev = factor(simple_name, levels = c("D1 (NWT LTER, alpine)", "Saddle (NWT LTER, tundra)", "C1 (NWT LTER, forest)", "City of Boulder")))

wymon_met_all <- mon_met_all %>%
  left_join(allsites_kml[c("simple_name", "elevation")]) %>%
  # convert elev to ft
  mutate(elev_ft = round(elevation* 3.28084,0),
         site_label = paste0(local_site,", ", elev_ft, " ft"),
         site_label = paste0(casefold(substr(site_label, 1, 1), upper = T), substr(site_label,2, nchar(site_label))),
         site_label = gsub("Sdl", "Saddle (NWT LTER, alpine tundra)", site_label),
         site_label = gsub("D1", "D1 (NWT LTER, upper alpine)", site_label),
         site_label = gsub("C1", "C1 (NWT LTER, subalpine forest)", site_label),
         site_label = gsub("Boulder", "City of Boulder", site_label),
         site_label = factor(site_label, levels = unique(site_label[order(local_site_elev)]), ordered = TRUE))

monthlydat <- wymon_met_all %>%
  gather(metric, value, tmin:mon_ppt) %>%
  #subset(metric != "mon_ppt") %>%
  mutate(metric = factor(metric, levels = c("tmax", "avgtmax", "tmean", "avgtmin", "tmin", "mon_ppt"),
                         labels = c("Max Temp (°F)", "Avg. Max Temp (°F)", "Avg. Temp (°F)", "Avg. Min Temp (°F)", "Min Temp (°F)", "Precipitation (in.)")),
         # convert value to F
         value = ifelse(grepl("Temp", metric), (value*1.8) +32, 
                        value/25.4),
         x_mon = month(plotdate, label = T, abbr = T)) %>%
  #subset to 1990 onwards
  subset(yr >= 1990)
ggplot(monthlydat, aes(x_mon, value)) +
  geom_hline(data = subset(monthlydat, !grepl("Prec", metric)), aes(yintercept = 32), lty = 2, alpha = 0.75, col = "grey30") +
  # geom_point(aes(group = paste(x_mon, site_label), fill = site_label), pch = 21, col = "grey50", alpha = 0.25, 
  #            position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.1, jitter.height = 0)) +
  geom_boxplot(aes(group = paste(site_label, x_mon), fill = site_label),
               outlier.color = "grey50", outlier.shape = 21, alpha = 0.75) +
  geom_smooth(data = subset(monthlydat, grepl("Temp", metric)), aes(x = mon, y = value, col = site_label, fill = site_label)) +
  geom_smooth(data = subset(monthlydat, grepl("Pre", metric)), aes(x = mon, y = value, col = site_label, fill = site_label), method = "loess") +
  scale_color_viridis_d(name = "Climate station") +
  scale_fill_viridis_d(name = "Climate station") +
  guides(fill=guide_legend(ncol=2, nrow = 2), #direction = "vertical"
         color=guide_legend(ncol=2, nrow = 2)) +
  labs(y = NULL, x = NULL,
       caption = "Monthly temperatures and precipitation, 1990 to present") +
  scale_x_discrete(labels = substr(month.abb, 1, 1)) +
  facet_wrap(~metric, ncol = 2, scales = "free") +
  theme(legend.position = "top",
        legend.title.align = 0,
        #legend.title = element_text(vjust = 0),
        #legend.justification = c(0,0),
        #legend.box = "vertical",
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        plot.caption = element_text(hjust = 0))

ggsave(paste0(figpath, "monthlytemp.pdf"), width = 8, height =8, units = "in")
ggsave(paste0(figpath, "monthlytemp.png"), width = 8, height =8, units = "in")
ggsave(paste0(figpath, "monthlymet.pdf"), width = 10, height =7, units = "in")

## -- yr summary ----
yr_met_all <- group_by(mon_met_all, yr, local_site_elev, simple_name, local_site) %>%
  summarise(avgtmin = mean(avgtmin),
            avgtmax = mean(avgtmax),
            tmax = max(tmax),
            tmin = min(tmin),
            yr_ppt = sum(mon_ppt),
            ctmon = length(mon)) %>%
  ungroup() %>%
  subset(ctmon == 12)

wy_met_all <- group_by(mon_met_all, wy, local_site_elev, simple_name, local_site) %>%
  summarise(avgtmin = mean(avgtmin),
            avgtmax = mean(avgtmax),
            tmean = mean(tmean),
            tmax = max(tmax),
            tmin = min(tmin),
            yr_ppt = sum(mon_ppt),
            ctmon = length(mon)) %>%
  ungroup() %>%
  subset(ctmon == 12) %>%
  left_join(allsites_kml[c("simple_name", "elevation")]) %>%
  # convert elev to ft
  mutate(elev_ft = round(elevation* 3.28084,0),
         site_label = paste0(local_site,", ", elev_ft, " ft"),
         site_label = paste0(casefold(substr(site_label, 1, 1), upper = T), substr(site_label,2, nchar(site_label))),
         site_label = gsub("Sdl", "Saddle (NWT LTER, alpine tundra)", site_label),
         site_label = gsub("D1", "D1 (NWT LTER, upper alpine)", site_label),
         site_label = gsub("C1", "C1 (NWT LTER, subalpine forest)", site_label),
         site_label = gsub("Boulder", "City of Boulder", site_label),
         site_label = factor(site_label, levels = unique(site_label[order(local_site_elev)]), ordered = TRUE))

# boxplots of monthly temp with trend



# boxplots of monthly ppt with trend?



plot_grid(annppt, anntemp, ncol = 2, align = "vh", rel_widths = c(0.75, 1))
ggsave(paste0(figpath, "annual_climate_timeseries.pdf"), width = 8, height = 5.5, units = "in", scale = 1.35)
ggsave(paste0(figpath, "annual_climate_timeseries.png"), width = 8, height = 5.5, units = "in", scale = 1.35)

# time series annual temp
yr_met_all %>%
  gather(metric, value, avgtmin:yr_ppt) %>%
  ggplot(aes(yr, value, col = local_site_elev)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.7) +
  facet_wrap(~metric, scales = "free_y")


subset(wy_met_all, select = -yr_ppt) %>%
  gather(metric, value, avgtmin:tmin) %>%
  mutate(local_site_elev2 = factor(simple_name, rev(levels(local_site_elev))),
         metric2 = factor(metric, levels = c("tmax", "avgtmax", "tmean", "avgtmin", "tmin"), labels = c("Max", "Avg. Max", "Avg.", "Avg. Min", "Min"))) %>%
  ggplot(aes(wy, value, col = metric2)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.7) +
  facet_grid(~local_site_elev2, space = "free_x", scales = "free_x")

striplabels <- unique(wy_met_all$site_label)[unique(wy_met_all$local_site_elev)]
anntemp <- subset(wy_met_all, select = -yr_ppt) %>%
  gather(metric, value, avgtmin:tmin) %>%
  # convert to F
  #mutate(value = (value*1.8) +32) %>%
  group_by(local_site, metric) %>%
  mutate(rollingtemp = zoo::rollmean(value, k = 10, fill = NA, align = "right")) %>%
  ungroup() %>%
  mutate(local_site_elev2 = factor(simple_name, rev(levels(local_site_elev))),
         metric2 = factor(metric, levels = c("tmax", "avgtmax", "tmean", "avgtmin", "tmin"), labels = c("Max", "Avg. Max", "Avg.", "Avg. Min", "Min"))) %>%
  ggplot(aes(wy, value, col = metric2)) +
  geom_line(alpha = 0.5) +
  geom_point(aes(fill = metric2), pch = 21, col = "grey50", alpha = 0.7) +
  geom_line(aes(wy, rollingtemp), lwd = 1, alpha = 0.7) +
  scale_x_continuous(breaks = seq(1880, 2020, 20)) +
  scale_y_continuous(breaks = seq(-40, 100, 20)) +
  scale_color_brewer(name = NULL, palette = "PuOr") +
  scale_fill_brewer(name = NULL, palette = "PuOr") +
  labs(y = "Temperature (°C)", x = "Water year (Oct 1-Sep 30)") +
  #labs(y = "Temperature (°F)", x = "Water year (Oct 1-Sep 30)") +
  facet_wrap(~site_label, nrow = 4) +
  theme_bw() +
   theme(legend.position = c(0.1, 0.5),
     #legend.position = c(0.2,1),
        #legend.justification = c(1,1),
     legend.justification = c("left", "bottom"),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        strip.background = element_rect(fill= "transparent"),
        strip.text = element_text(face = "bold"))

# time series annual ppt
annppt <- subset(wy_met_all, select = c(wy:local_site, yr_ppt, site_label)) %>%
  # put ppt in inches
  mutate(yr_ppt = yr_ppt/25.4) %>%
  mutate(local_site_elev2 = factor(simple_name, rev(levels(local_site_elev)))) %>%
  group_by(local_site) %>%
  mutate(rollingppt = zoo::rollmean(yr_ppt, k = 10, fill = NA, align = "right")) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(wy, yr_ppt), alpha = 0.5, col = "dodgerblue") +
  geom_point(aes(wy, yr_ppt), pch = 21, col ="blue", alpha = 0.7, fill = "dodgerblue") +
  geom_line(aes(wy, rollingppt), lwd = 1.5, col = "blue", alpha = 0.7) +
  #geom_smooth(method = "gam") +
  labs(y = "Precipitation (inches)", x = "Water year (Oct 1-Sep 30)") +
  scale_x_continuous(breaks = seq(1880, 2020, 20)) +
  facet_wrap(~site_label, nrow = 4) +
  theme_bw() +
  theme(legend.position = c(0.2,1),
        legend.justification = c(1,1),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        strip.background = element_rect(fill= "transparent"),
        strip.text = element_text(face = "bold"))

# what does log look like?
subset(wy_met_all, select = c(wy:local_site, yr_ppt)) %>%
  mutate(local_site_elev2 = factor(simple_name, rev(levels(local_site_elev))),
         yr_ppt = log(yr_ppt)) %>%
  group_by(local_site) %>%
  mutate(rollingppt = zoo::rollmean(yr_ppt, k = 10, fill = NA, align = "right")) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(wy, yr_ppt), alpha = 0.5, col = "dodgerblue") +
  geom_point(aes(wy, yr_ppt), pch = 21, col ="blue", alpha = 0.7, fill = "dodgerblue") +
  geom_line(aes(wy, rollingppt), lwd = 1.5, col = "blue", alpha = 0.7) +
  #geom_smooth(method = "gam") +
  scale_x_continuous(breaks = seq(1880, 2020, 20)) +
  facet_wrap(~local_site_elev, nrow = 4) +
  theme_bw()


plot_grid(annppt, anntemp, ncol = 2, align = "vh", rel_widths = c(0.75, 1))
ggsave(paste0(figpath, "annual_climate_timeseries.pdf"), width = 8, height = 5.5, units = "in", scale = 1.35)
ggsave(paste0(figpath, "annual_climate_timeseries.png"), width = 8, height = 5.5, units = "in", scale = 1.35)

# -- day of year summary for each nwt station ----
# color by year

nwt_ppt_plot <- nwt_ppt %>%
  mutate(wy = ifelse(month(date) %in% 10:12, yr+1, yr),
         wymon = ifelse(mon %in% 10:12, mon-9, mon +3),
         day = day(date)) %>%
  arrange(wy, wymon, day) %>%
  group_by(local_site, wy) %>%
  mutate(runningppt = cumsum(precip),
         nobsmon = length(unique(wymon)),
         wydoy = 1:length(date)) %>%
  subset(nobsmon == 12)

nwt_temp_plot <- nwt_temp %>%
  gather(metric, temp, max:dtr) %>%
  mutate(decade = as.numeric(paste0(substr(yr, 1, 3), 0)),
         metric = factor(metric, levels = c("max", "avg", "min", "dtr"), 
                         labels = c("Max Temp (°C)", "Avg. Temp (°C)", "Min. Temp (°C)", "Diurnal Temp (°C)")))

#sdl
sdl_doy_ppt <- ggplot(subset(nwt_ppt_plot, local_site == "sdl"), aes(wydoy, runningppt, group = wy, color = wy)) +
  geom_line(alpha = 0.5, lwd = 2) +
  labs(x = "Day of water year (Oct 1-Sep 30)", y = NULL,
       caption = " ") +
  facet_wrap(~"Cumulative precipitation (mm)") +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,350, 50)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 1500, 250)) +
  scale_color_viridis_c(name = NULL, breaks = c(1980, 1990, 2000, 2010, 2020), option = "B") +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(0.99,-0.05),
        legend.direction = "horizontal",
        legend.justification = c(1,1),
        legend.key.height = unit(5, "pt"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(hjust = 1),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold"))


sdl_doy_temp <- ggplot(subset(nwt_temp_plot, local_site == "sdl"), ) +
  geom_line(aes(doy, temp, col = yr, group = yr), alpha = 0.5) +
  geom_smooth(aes(doy, temp, col = decade, group = decade), method = "gam") +
  scale_color_viridis_c(name = NULL, option = "B") +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,350, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Day of calendar year (Jan 1-Dec 31)", y = NULL,
       caption = "Saddle dailies, with decadal trend lines") +
  facet_wrap(~metric, scale = "free_y", nrow = 4) +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(1,-0.05),
        legend.direction = "horizontal",
        legend.justification = c(1,1),
        legend.key.height = unit(5, "pt"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(hjust = 1),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold"))

plot_grid(sdl_doy_temp, sdl_doy_ppt, ncol = 2, align = "vh")
ggsave(paste0(figpath, "sdl_doy.png"), width = 8, height = 5.5, units = "in", scale = 1.35)


#c1
c1_doy_ppt <- ggplot(subset(nwt_ppt_plot, local_site == "c1"), aes(wydoy, runningppt, group = wy, color = wy)) +
  geom_line(alpha = 0.5, lwd = 2) +
  labs(x = "Day of water year (Oct 1-Sep 30)", y = NULL,
       caption = " ") +
  facet_wrap(~"Cumulative precipitation (mm)") +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,350, 50)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 1500, 250)) +
  scale_color_viridis_c(name = NULL, option = "B") +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(0.99,-0.05),
        legend.direction = "horizontal",
        legend.justification = c(1,1),
        legend.key.height = unit(5, "pt"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(hjust = 1),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold"))


c1_doy_temp <- ggplot(subset(nwt_temp_plot, local_site == "c1"), ) +
  geom_line(aes(doy, temp, col = yr, group = yr), alpha = 0.5) +
  geom_smooth(aes(doy, temp, col = decade, group = decade), method = "gam") +
  scale_color_viridis_c(name = NULL, option = "B") +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,350, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Day of calendar year (Jan 1-Dec 31)", y = NULL,
       caption = "C1 dailies, with decadal trend lines") +
  facet_wrap(~metric, scale = "free_y", nrow = 4) +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(1,-0.05),
        legend.direction = "horizontal",
        legend.justification = c(1,1),
        legend.key.height = unit(5, "pt"),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_rect(fill = "transparent"),
        legend.text = element_text(hjust = 1),
        strip.text = element_text(face = "bold"))

plot_grid(c1_doy_temp, c1_doy_ppt, ncol = 2, align = "vh")
ggsave(paste0(figpath, "c1_doy.png"), width = 8, height = 5.5, units = "in", scale = 1.35)


#d1
d1_doy_ppt <- ggplot(subset(nwt_ppt_plot, local_site == "d1"), aes(wydoy, runningppt, group = wy, color = wy)) +
  geom_line(alpha = 0.5, lwd = 2) +
  labs(x = "Day of water year (Oct 1-Sep 30)", y = NULL,
       caption = " ") +
  facet_wrap(~"Cumulative precipitation (mm)") +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,350, 50)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 1500, 250)) +
  scale_color_viridis_c(name = NULL, option = "B") +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(0.99,-0.05),
        legend.direction = "horizontal",
        legend.justification = c(1,1),
        legend.key.height = unit(5, "pt"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(hjust = 1),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(face = "bold"))


d1_doy_temp <- ggplot(subset(nwt_temp_plot, local_site == "d1"), ) +
  geom_line(aes(doy, temp, col = yr, group = yr), alpha = 0.5) +
  geom_smooth(aes(doy, temp, col = decade, group = decade), method = "gam") +
  scale_color_viridis_c(name = NULL, option = "B") +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,350, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Day of calendar year (Jan 1-Dec 31)", y = NULL,
       caption = "D1 dailies, with decadal trend lines") +
  facet_wrap(~metric, scale = "free_y", nrow = 4) +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(1,-0.05),
        legend.direction = "horizontal",
        legend.justification = c(1,1),
        legend.key.height = unit(5, "pt"),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_rect(fill = "transparent"),
        legend.text = element_text(hjust = 1),
        strip.text = element_text(face = "bold"))

plot_grid(d1_doy_temp, d1_doy_ppt, ncol = 2, align = "vh")
ggsave(paste0(figpath, "d1_doy.png"), width = 8, height = 5.5, units = "in", scale = 1.35)


# -- PULL EXAMPLE ERRORS -----
figpath <- "/Users/scarlet/Documents/cu_boulder/Fall\ 2022/SciComm/figs/"
# good annual precip periods
sdlmiddle <- subset(regional_wy_ppt, wy %in% 1995:2000 & grepl("d1|sdl|c1|Eldor|Univ", local_site))
ggplot(subset(sdlmiddle), aes(wy, wy_ppt)) +
  geom_line(aes(col = local_site_elev)) +
  geom_line(data = subset(sdlmiddle, local_site == "sdl"), lwd = 1, col = "black") +
  scale_color_viridis_d()



# -- break in annual precip -----
sdlearly <- subset(regional_wy_ppt, wy %in% 1982:1990 & grepl("d1|sdl|c1|Eldor|Niwot", local_site)) %>%
  left_join(siteinfo[c("local_site", "elevation")])

ggplot(subset(sdlearly, grepl("d1|c1|sdl|Eldor|Niwot", local_site)), aes(wy, wy_ppt)) +
  geom_line(data = subset(sdlearly, local_site == "sdl"), lwd = 2, col = "grey80", alpha = 0.7) +
  geom_line(aes(col = elevation, group = local_site), lty = 1) +
  geom_point(aes(fill = elevation, group = local_site), pch = 21, col = "grey30") +
  labs(x = "Year", y = "Rainfall") +
  scale_color_viridis_c(name = "Elev.", direction = -1, n.breaks = 5, labels = c("", "Low", "", "High", "")) +
  scale_fill_viridis_c(name = "Elev.", direction = -1, n.breaks = 5, labels = c("", "Low", "", "High", "")) +
  theme_classic() +
  theme(legend.position = c(1,1), #c(1,1),
        legend.direction = "horizontal",
        legend.justification = c(1,1),
        legend.key.height = unit(3, "pt"),
        legend.background = element_blank(),
        #legend.text = element_text(size = 8),
        legend.title = element_text(size = 10, vjust = 1),
        #legend.text.align = 0,
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "last")),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "last")),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


ggsave(paste0(figpath, "sdl_annppt_break.pdf"), width = 3, height = 3, units = "in")


# period where sdl lines up well with other sites
ppt2000 <- subset(regional_wy_ppt, wy %in% 2010:2017 & CD2 == "Middle") %>%
  left_join(siteinfo[c("local_site", "elevation")])

ggplot(subset(ppt2000, !grepl("Saw", local_site)), aes(wy, wy_ppt)) +
  geom_line(data = subset(ppt2000, local_site == "sdl"), lwd = 2, col = "grey80", alpha = 0.75) +
  geom_line(aes(col = elevation, group = local_site), alpha = 0.75) +
  geom_point(aes(fill = elevation), col = "grey30", pch = 21) +
  geom_line(data = subset(ppt2000, local_site == "sdl")) +
  labs(x = "Year", y = "Rainfall") +
  #scale_color_brewer(palette = "BuPu") +
  scale_color_viridis_c(name = "Elev.", direction = -1, n.breaks = 5, labels = c("", "Low", "", "High", "")) +
  scale_fill_viridis_c(name = "Elev.", direction = -1, n.breaks = 5, labels = c("", "Low", "", "High", "")) +
  theme_classic() +
  theme(legend.position = c(1,1), #c(1,1),
        legend.direction = "horizontal",
        legend.justification = c(1,1),
        legend.key.height = unit(3, "pt"),
        legend.background = element_blank(),
        #legend.text = element_text(size = 8),
        legend.title = element_text(size = 10, vjust = 1),
        #legend.text.align = 0,
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "last")),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "last")),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


ggsave(paste0(figpath, "annual_ppt_agree.pdf"), width = 3, height = 3, units = "in")





# -- day SDL is only station that has precip (Nov 2020 is a good example)
# period where sdl lines up well with other sites
novppt2020 <- subset(all_ppt, yr == 2020 & mon == 11) %>%  #& local_site %in% regional_wy_ppt$local_site[regional_wy_ppt$CD2 == "Middle"]) %>%
  # take out timestamp from FRASER
  mutate(local_site = gsub("_[0-9]{4}$", "", local_site)) %>%
  left_join(siteinfo[c("local_site", "elevation")]) %>%
  mutate(ppt = ifelse(local_site == "sdl", raw, measurement))

ggplot(subset(novppt2020), aes(date, ppt)) +
  geom_line(data = subset(novppt2020, local_site == "sdl"), lwd = 2, col = "grey80", alpha = 0.75) +
  geom_point(data = subset(novppt2020, doy == 317 & local_site == "sdl"), size = 3, col = "grey80", alpha = 0.75) +
  geom_line(aes(col = elevation, group = local_site), alpha = 0.75) +
  geom_point(aes(fill = elevation), col = "grey30", pch = 21, alpha = 0.9) +
  geom_line(data = subset(novppt2020, local_site == "sdl")) +
  labs(x = "Day", y = "Rainfall") +
  scale_color_viridis_c(name = "Elevation", direction = -1, n.breaks = 4, labels = c("","Low", "", "High")) +
  scale_fill_viridis_c(name = "Elevation", direction = -1, n.breaks = 4, labels = c("","Low", "", "High")) +
  #scale_color_brewer(palette = "BuPu") +
  #scale_color_viridis_c(name = "Elev.", direction = -1, n.breaks = 5, labels = c("", "Low", "", "High", "")) +
  #scale_fill_viridis_c(name = "Elev.", direction = -1, n.breaks = 5, labels = c("", "Low", "", "High", "")) +
  theme_classic() +
  theme(legend.position = c(1,1), #c(1,1),
        #legend.direction = "horizontal",
        legend.justification = c(1,1),
        legend.key.width = unit(5, "pt"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_blank(),
        #legend.text = element_text(size = 8),
        legend.title = element_text(size = 10, vjust = 1),
        #legend.text.align = 0,
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "last")),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "last")),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


ggsave(paste0(figpath, "sdl_break_nov2020.pdf"), width = 3, height = 3, units = "in")



# show 2013 floods
sep2013 <- subset(all_ppt, mon == 9 & yr == 2013) %>%
  # take out timestamp from FRASER
  mutate(local_site = gsub("_[0-9]{4}$", "", local_site)) %>%
  left_join(siteinfo[c("local_site", "elevation", "CD")]) %>%
  mutate(ppt = ifelse(local_site == "sdl", raw, measurement))

ggplot(sep2013, aes(date, measurement, col = elevation)) +
  #ggplot(subset(sep2013, CD == "East"), aes(date, measurement, col = elevation)) +
  geom_line(data = subset(sep2013, local_site == "sdl"), lwd = 2, col = "grey80", alpha = 0.75) +
  #geom_point(data = subset(novppt2020, doy == 317 & local_site == "sdl"), size = 3, col = "grey80", alpha = 0.75) +
  geom_line(aes(col = elevation, group = local_site), alpha = 0.75) +
  geom_point(aes(fill = elevation), col = "grey30", pch = 21, alpha = 0.9) +
  geom_line(data = subset(sep2013, local_site == "sdl")) +
  labs(x = "Day", y = "Rainfall") +
  scale_color_viridis_c(name = "Elevation", direction = -1, n.breaks = 4, labels = c("","Low", "", "High")) +
  scale_fill_viridis_c(name = "Elevation", direction = -1, n.breaks = 4, labels = c("","Low", "", "High")) +
  #scale_x_date(date_labels = "%d %b %Y")
  theme_classic() +
  theme(legend.position = c(1,1), #c(1,1),
        #legend.direction = "horizontal",
        legend.justification = c(1,1),
        legend.key.width = unit(5, "pt"),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_blank(),
        #legend.text = element_text(size = 8),
        legend.title = element_text(size = 10, vjust = 1),
        #legend.text.align = 0,
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "last")),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "last")),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


ggsave(paste0(figpath, "regional_ppt_sep2013.jpg"), width = 3, height = 3, units = "in")




# -- SAME STATION -----
# raw sdl values separated from infilled sdl chart

# -- june 1982 tmax outlier ----
highdate <- with(sdl_chart_temp_qc, doy[grepl("high val", qa_flag)])
sdl_june <- subset(sdl_chart_temp_qc, doy >= (highdate - 13) & doy <= (highdate + 13))

sdl_junelm <- lm(sdl_qatemp ~ cos(2*pi*doy/365) + sin(2*pi*doy/365), data = sdl_june)
test <- data.frame((predict(sdl_junelm, newdata = sdl_june, se.fite = T, level = .99, interval = "prediction"))) %>%
  cbind(sdl_june)

sdl_june_meansd <- group_by(sdl_june, doy) %>%
  summarise(avg_tmax = mean(sdl_temp, na.rm = T),
            sd_tmax = sd(sdl_temp, na.rm = T),
            nobs = sum(!is.na(sdl_temp)))

ggplot() +
  geom_ribbon(data = test, aes(doy, ymin = lwr, ymax = upr), col = "grey30", lty = 2, fill = "grey50", alpha = 0.15) +
  geom_point(data = subset(sdl_june, met == "airtemp_max"), aes(doy, sdl_temp, col = yr), alpha = 0.75) +
  geom_point(data = subset(sdl_june, met == "airtemp_max" & sdl_temp > 24), aes(doy, sdl_temp), size = 4, pch = 1, col = "red") +
  geom_line(data = test, aes(doy, fit), col = "blue", lwd = 1) +
  labs(x = "Day of year", y = "Temperature (°C)") +
  scale_color_viridis_c(name = "Yr", option = "B") +
  theme_classic() +
  theme(#legend.position = c(0.97,0.97), #c(1,1), 
    legend.position = c(1,0.03),
    legend.justification = c("right"),
    legend.direction = "horizontal",
    #legend.justification = c(1,1),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(3, "pt"),
    
    legend.background = element_blank(),
    
    legend.text = element_text(size = 8, vjust = 3),
    legend.title = element_text(size = 10, vjust = 1),
    #legend.text.align = 0,
    axis.line.y = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                   ends = "both")),
    axis.line.x = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                   ends = "last")))


ggsave(paste0(figpath, "sdl_tmax_error_1982_color.pdf"), width = 3, height = 3, units = "in")


# show tmin in feb 2011

sdtlT_good <- subset(sdl_temp,  mon %in% 8 & yr > 2012)

ggplot(sdl_2011, aes(doy, c(NA, diff(raw_airtemp_min)), col = yr, group = yr)) +
  geom_line() +
  geom_point() +
  geom_smooth()

ggplot(subset(sdl_temp, mon <4), aes(doy, airtemp_min_gapfilled)) +
  geom_density_2d_filled()

ggplot(sdl_2011, aes(airtemp_min_gapfilled)) +
  geom_density()

sdl_feblm <- lm(raw_airtemp_avg ~ cos(2*pi*doy/365) + sin(2*pi*doy/365), data = sdl_2011)
test2011 <- data.frame((predict(sdl_feblm, newdata = sdl_2011, se.fite = T, level = .99, interval = "prediction"))) %>%
  cbind(sdl_2011)

sdl_june_meansd <- group_by(sdl_june, doy) %>%
  summarise(avg_tmax = mean(sdl_temp, na.rm = T),
            sd_tmax = sd(sdl_temp, na.rm = T),
            nobs = sum(!is.na(sdl_temp)))

ggplot() +
  geom_line(data = test2011, aes(doy, fit), col = "blue", lwd = 1) +
  geom_ribbon(data = test2011, aes(doy, ymin = lwr, ymax = upr), col = "grey30", lty = 2, fill = "grey50", alpha = 0.15) +
  geom_point(data = subset(sdl_2011), aes(doy, raw_airtemp_avg, col = yr), alpha = 0.8) +
  #geom_point(data = subset(sdl_june, met == "airtemp_max" & sdl_temp > 24), aes(doy, sdl_temp), size = 4, pch = 1, col = "red") +
  labs(x = "Day of year", y = "Temperature (°C)") +
  scale_color_viridis_c(name = "Yrs", option = "B", )+ #breaks = c(2000,2010, 2020), labels = c(2020-2000,2020-2010, 2020-2020)
  theme_classic() +
  theme(#legend.position = c(0.97,0.97), #c(1,1), 
    legend.position = c(0.95,0.95),
    legend.justification = c("right"),
    legend.direction = "horizontal",
    #legend.justification = c(1,1),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(3, "pt"),
    
    legend.background = element_blank(),
    
    legend.text = element_text(size = 8, vjust = 3),
    legend.title = element_text(size = 10, vjust = 1),
    #legend.text.align = 0,
    axis.line.y = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                   ends = "both")),
    axis.line.x = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                   ends = "last")))

ggsave(paste0(figpath, "sdl_tmean_aug_2012on.pdf"), width = 3, height = 3, units = "in")
ggsave(paste0(figpath, "sdl_tmean_aug_2012on_color.jpg"), width = 3, height = 3, units = "in")


# -- good days to day change ----
# show tmin in feb 2011 (extreme temp)
sdl_goodchange <- subset(sdl_temp,  mon < 4, select = c(local_site, logger, date, yr, mon, doy, raw_airtemp_max, raw_airtemp_avg, raw_airtemp_min, source_station))

gather(sdl_goodchange, met, val, raw_airtemp_max:raw_airtemp_min) %>%
  ggplot(aes(doy, val, col = met)) +
  geom_line(aes(group = yr)) +
  geom_point() +
  facet_wrap(~met)

sdl_goodchange <- gather(sdl_goodchange, met, val, raw_airtemp_max:raw_airtemp_min) %>%
  group_by(yr, met) %>%
  mutate(delta = val-lag((val))) %>%
  ungroup() %>%
  # start at 1990 where logger more operational
  subset(yr > 1992) %>%
  # focus on 15 day window around 2/1 (day 32 of year)
  subset(doy >= 32-14 & doy <= 32+14) #%>%
# choose tmin only
subset(grepl("min", met))

ggplot(sdl_goodchange, aes(scale(delta))) +
  geom_histogram() +
  facet_wrap(~met)

sdl_tmindelta_lm <- lm(delta~doy, data = sdl_goodchange)
goodchange_predict <- data.frame(predict(sdl_tmindelta_lm, newdata = sdl_goodchange, se.fit = T, level = .99, interval = "prediction")) %>%
  cbind(sdl_goodchange)

ggplot(sdl_goodchange, aes(doy, delta)) +
  geom_ribbon(data = goodchange_predict, aes(doy, ymax = fit.upr, ymin = fit.lwr), lty = 3, fill = "grey80") +
  geom_point(aes( col = yr), alpha = 0.7) +
  #geom_smooth(method = "lm") +
  scale_color_viridis_c(option = "B")

mean_goodchange <- mean(sdl_goodchange$delta, na.rm = T)
sd_goodchange <- sd(sdl_goodchange$delta, na.rm = T)
ggplot(sdl_goodchange, aes(doy, delta)) +
  #geom_ribbon(data = subset(goodchange_predict), aes(doy, ymax = fit.upr, ymin = fit.lwr)) +
  geom_ribbon(aes(doy, 
                  ymax = mean_goodchange + (4*sd_goodchange), 
                  ymin = mean_goodchange - (4*sd_goodchange)),
              fill = "grey50", alpha = 0.15, col = "grey30", lty = 2) +
  geom_point(aes( col = yr), alpha = 0.7) +
  geom_line(aes(doy, mean_goodchange), lwd = 1, col = "blue") +
  labs(y = "Temperature change from day before", x = "Day of year") +
  #geom_smooth(method = "lm") +
  scale_color_viridis_c(name = "Yrs past", option = "B", breaks = c(2000,2010, 2020), labels = c(2020-2000,2020-2010, 2020-2020)) +
  scale_y_continuous(breaks = c((mean_goodchange - (3*sd_goodchange)), (mean_goodchange + (3*sd_goodchange))),
                     labels = c("-","+"), ) +
  theme_classic() +
  theme(#legend.position = c(0.97,0.97), #c(1,1), 
    legend.position = c(1.02,.98),
    legend.justification = c("right"),
    legend.direction = "horizontal",
    #legend.justification = c(1,1),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(3, "pt"),
    
    legend.background = element_blank(),
    
    legend.text = element_text(size = 8, vjust = 3),
    legend.title = element_text(size = 10, vjust = 1),
    #legend.text.align = 0,
    axis.line.y = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                   ends = "both")),
    axis.line.x = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                   ends = "last")),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks = element_blank())

ggsave(paste0(figpath, "sdlT_goodchange_winter.pdf"), width = 3, height = 3, units = "in")
ggsave(paste0(figpath, "sdlT_goodchange_winter.jpg"), width = 3, height = 3, units = "in")


# -- plot flatline example -----
# 2015-09-2 6+ flatline in tmin but not tmax
# 2000-06-25 mostly tmin but tmax doesn't move for a few days en pares
sdl_flatline <- subset(sdl_chart_temp_qc, mon %in% c(6,7) & yr == 2000, select = -daysflat) %>%
  distinct() %>%
  group_by(met) %>%
  mutate(delta = sdl_temp - lag(sdl_temp)) %>%
  ungroup()
mindoy <- with(sdl_flatline, min(doy[grepl("warning", qa_flag)]))
maxdoy <- with(sdl_flatline, max(doy[grepl("warning", qa_flag)]))

ggplot(subset(sdl_flatline, doy >= (mindoy-7) & doy <= (maxdoy+7)), aes(date, sdl_temp, group = met, col = met)) +
  geom_line(data = subset(sdl_flatline, doy > mindoy & doy <= maxdoy & grepl("min", met)), aes(date, sdl_temp), lwd = 3, col = "grey80", alpha = 0.75) +
  geom_line() +
  geom_point(aes()) +
  geom_text(aes(x = as.Date("2000-06-20"), y = -2), size = 4, label = "Min", col = "blue") +
  geom_text(aes(x = as.Date("2000-06-22"), y = 16), size = 4, label = "Max", col = "red") +
  labs(y = "Daily temperature (°C)", x = NULL) +
  scale_x_date(date_breaks = "4 days", date_labels = "%b %d") +
  theme_classic() +
  theme(legend.position = "none",
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "both")),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "last")))

ggsave(paste0(figpath, "sdlT_flatline.pdf"), width = 3, height = 3, units = "in")


# -- plot D1 climate anomalies for ARose ----
d1_anomT <- subset(d1_temp, select = c(year, mean_temp)) %>%
  group_by(year) %>%
  summarise(annT = mean(mean_temp)) %>%
  ungroup() %>%
  mutate(alldat_anom = annT - mean(annT),
         rollmean = zoo::rollmean(annT, fill = NA, k = 30, align = "right"),
         moving_anom = annT - rollmean,
         baseline_mean = mean(annT[year < 1982]),
         baseline_anom = annT - mean(annT[year < 1982]))

cowplot::plot_grid(
  ggplot(d1_anomT, aes(year, 1)) +
    geom_col(aes(fill = annT)) + 
    scale_fill_distiller(name = "Mean", palette = "RdBu"),
  ggplot(d1_anomT, aes(year, 1)) +
    geom_col(aes(fill = alldat_anom)) + 
    scale_fill_distiller(name = "Anom", palette = "RdBu"),
  ggplot(d1_anomT, aes(year, 1)) +
    geom_col(aes(fill = baseline_anom)) + 
    scale_fill_distiller(name = "Anom", palette = "RdBu"),
  ggplot(d1_anomT, aes(year, 1)) +
    geom_col(aes(fill = moving_anom)) + 
    scale_fill_distiller(name = "Anom", palette = "RdBu"),
  nrow = 4, align = "vh"
)

gather(d1_anomT, met, val, annT:ncol(d1_anomT)) %>%
  subset(!met %in% c("annT", "rollmean")) %>%
  ggplot(aes(year, 1)) +
  geom_col(aes(fill = val)) + 
  scale_fill_distiller(name = "Anom", palette = "RdBu") +
  facet_wrap(~met, nrow = 3)


# -- FIGS FOR NWT MEETING -----

# to make:
# 1. example raw fig w flagged data --> treated data

# show d1 2013:2019
d1_postTK <- subset(d1_temp, year > 2012) %>%
# april 2017 has stuck pen
  subset(month(date) %in% 3:4 & year ==2017, select = c(local_site:min_temp, source_station, t_mean_pvalue, Tmax_QAflag:ncol(.))) %>%
  gather(met, val, max_temp, min_temp, raw_Tmax, raw_Tmin) %>%
  mutate(gapfilled = ifelse(grepl("raw", met), "raw", "final"),
         met = ifelse(grepl("max", met), "tmax", "tmin")) %>%
  spread(gapfilled, val) %>%
  mutate(qaflag = ifelse(met == "tmin", Tmin_QAflag, Tmax_QAflag))

ggplot(d1_postTK, aes(date, raw, group = met)) +
  geom_line(col = "grey30") +
  geom_point(alpha = 0.9) +
  geom_point(data = subset(d1_postTK, !is.na(qaflag)), aes(col = qaflag), alpha = 0.9) +
  labs(y = "D1 raw max, min temp (°C)", x = NULL) +
  scale_x_date(date_labels = "%m-%d-%y") +
  scale_color_manual(values = c("firebrick2","goldenrod2")) +
  scale_y_continuous(limits = c(-24, 9)) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(2, "pt"),
          legend.key.size =  unit(5, "pt"),
        axis.text.x = element_text(hjust = 0.75))
ggsave("nwt_climate/figs/d1_rawTemp_errorEx.jpg", height = 2.88, width = 3.56, units = "in")

# corrected
ggplot(d1_postTK, aes(date, final, group = met)) +
  geom_line(col = "grey30") +
  geom_point(alpha = 0.9) +
  geom_point(data = subset(d1_postTK, !is.na(t_mean_pvalue)), aes(col = source_station), alpha = 0.9) +
  scale_color_manual(values = c("cyan3", "mediumpurple1", "darkorchid3")) +
  labs(y = "D1 gap-filled max, min temp (°C)", x = NULL) +
  scale_y_continuous(limits = c(-24, 9)) +
  scale_x_date(date_labels = "%m-%d-%y") +
  theme(legend.position = c(0.01, 0.99),
        legend.justification = c("left", "top"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size =  unit(5, "pt"),
        legend.spacing.y = unit(2, "pt"),
        #legend.title = element_text(size = 10),
        axis.text.x = element_text(hjust = 0.75))

ggsave("nwt_climate/figs/d1_finalTemp_errorEx.jpg", height = 2.88, width = 3.56, units = "in")

# 2. another example or so of flagged data

# find spike change and comparative deviance
# choose c1 2014-11-25 tmax had comparative deviance and low value (met multiple flags, plot with other stations)

# show 2 weeks before and after, all of nov
c1_tmax_bad <- subset(c1_temp, year == 2014, select = c(local_site:date, max_temp,source_station, t_mean_pvalue, Tmin_QAflag, raw_Tmax)) %>%
  subset(month(date) == 11) %>%
  subset(date >= as.Date("2014-11-15"))
c1_temp_sites <- subset(temp_sites, station_id == "c1_chart") %>%
  arrange(final_rank) %>%
  # choose top 6 sites
  subset(final_rank < 18)
c1_temp_order <- c1_temp_sites$paired_site[order(c1_temp_sites$final_rank)]

# pull other data near c1
snotel_c1 <- subset(snotel_temp, grepl("Niwo|Univ", local_site, fixed = F) & date %in% c1_tmax_bad$date & grepl("max", metric))
nwtlogT <-subset(nwt_logger_temp_qc, grepl("max", metric) & date %in% c1_tmax_bad$date) 
ghcnd_c1 <- subset(ghcnd_temp, local_site %in% c1_temp_sites$paired_site & date %in% c1_tmax_bad$date & grepl("max", metric))
flux_c1 <- subset(ameriflux_temp, date %in% c1_tmax_bad$date & grepl("max", metric))
stack_badT_c1 <- subset(nwtlogT, select = names(nwtlogT)[names(nwtlogT) %in% names(snotel_c1)]) %>%
  rbind(snotel_c1[names(.)]) %>%
  rbind(ghcnd_c1[names(.)]) %>%
  rbind(flux_c1[names(.)]) %>%
  mutate(local_site = factor(local_site, levels = c1_temp_order)) %>%
  subset(!is.na(local_site))

unique(stack_badT_c1$local_site[stack_badT_c1$local_site %in% c1_temp_order])

ggplot(c1_tmax_bad, aes(date, raw_Tmax)) +
  geom_line(data = stack_badT_c1, aes(date, measurement, col = local_site), alpha = 0.8) +
  geom_line(col = "darkseagreen4", lwd = 1, alpha = 0.75) +
  geom_point(col = "darkseagreen4", size = 2) +
  geom_text(data = c1_tmax_bad[1,], col = "darkseagreen4", label = "C1 chart", hjust =0 , nudge_y = 2) +
  geom_point(data = subset(c1_tmax_bad, !is.na(t_mean_pvalue)), pch = 1, size = 5, col = "chocolate2", lwd = 1) +
  scale_color_grey(name = "comparative\nstation") +
  guides(color = guide_legend(ncol = 2)) +
  scale_x_date(date_breaks = "4 days", date_labels = "%m-%d-%y") +
  labs(y = "raw daily max temp (°C)", x = NULL) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size =  unit(8, "pt"),
        legend.spacing.y = unit(2, "pt"),
        axis.text.x = element_text(hjust = 0.75))
ggsave("nwt_climate/figs/c1_comparative_errorEx.jpg", height = 3, width = 3.4, units = "in")



# 3. homogenized temp and gap-filled precip, panel
nwt_temp_mon %>%
  gather(metric, temp, tmin:tmean) %>%
  subset(!grepl("avg", metric)) %>%
  mutate(plotdate = as.Date(paste(yr, mon, 1, sep = "-"))) %>%
  group_by(local_site, metric) %>%
  mutate(rollingtemp = zoo::rollmean(temp, 12, "right", fill = NA)) %>%
  ggplot(aes(plotdate, temp, col = local_site_elev)) +
  geom_point(alpha = 0.25) +
  geom_line(aes(plotdate, rollingtemp), lwd = 1, alpha = 0.9) +
  scale_color_manual(name = NULL, values = c("blue", "purple", "forestgreen")) +
  facet_wrap(~metric, nrow = 3, scales = "free_y")


ggplot(subset(regional_wy_ppt, grepl("d1|c1|sdl", local_site) & !(local_site == "sdl" & wy < 1988)), aes(wy, wy_ppt, col = local_site_elev)) +
  geom_line(alpha = 0.25) +
  geom_line(aes(wy, zoo::rollmean(wy_ppt, 5, align = "right", fill = NA), group = local_site_elev), lwd = 1.5, alpha = 0.75) +
  geom_point(alpha = 0.5) +
  scale_color_manual(name = NULL, values = c("blue", "purple", "forestgreen")) +
  labs(y = "Total precipitation (mm)", x = "Water year (Oct 1 - Sep 30)") +
  scale_fill_viridis_d() +
  theme(legend.position = c(0.05,0.9),
        legend.justification = "left")





# nwt_temp %>%
#   group_by(local_site, yr) %>%
#   summarise(tmax = max(max), 
#             tmin = min(min),
#             avgtmax = mean(max),
#             avgtmin = mean(min),
#             tmean = mean(avg),
#             nobs = length((avg))) %>%
#   subset(nobs > 360, select = -nobs) %>%
#   #subset(yr > 1985) %>%
#   gather(metric, temp, tmax:tmean) %>%
#   mutate(local_site = factor(local_site, levels = c("c1", "sdl", "d1"))) %>%
#   group_by(local_site, metric) %>%
#   mutate(rollingtemp = zoo::rollmean(temp, 5, "right", fill = NA)) %>%
#   ggplot(aes(yr, temp, col = local_site)) +
#   geom_point(alpha = .95) +
#   geom_line(alpha = 0.95) +
#   #geom_line(aes(yr, rollingtemp), lwd = 1, alpha = 0.5) +
#   scale_color_manual(name = NULL, values = c("blue", "purple", "forestgreen")) +
#   facet_wrap(~metric, nrow = 3, scales = "free_y")


wy_met_all <- group_by(mon_met_all, wy, local_site_elev, simple_name, local_site) %>%
  summarise(avgtmin = mean(avgtmin),
            avgtmax = mean(avgtmax),
            tmean = mean(tmean),
            tmax = max(tmax),
            tmin = min(tmin),
            yr_ppt = sum(mon_ppt),
            ctmon = length(mon)) %>%
  ungroup() %>%
  subset(ctmon == 12) %>%
  left_join(allsites_kml[c("simple_name", "elevation")]) %>%
  # convert elev to ft
  mutate(elev_ft = round(elevation* 3.28084,0),
         site_label = paste0(local_site,", ", elev_ft, " ft"),
         site_label = paste0(casefold(substr(site_label, 1, 1), upper = T), substr(site_label,2, nchar(site_label))),
         site_label = gsub("Sdl", "Saddle (NWT LTER, alpine tundra)", site_label),
         site_label = gsub("D1", "D1 (NWT LTER, upper alpine)", site_label),
         site_label = gsub("C1", "C1 (NWT LTER, subalpine forest)", site_label),
         site_label = gsub("Boulder", "City of Boulder", site_label),
         site_label = factor(site_label, levels = unique(site_label[order(local_site_elev)]), ordered = TRUE))


# time series annual temp
yr_met_all %>%
  gather(metric, value, avgtmin:yr_ppt) %>%
  ggplot(aes(yr, value, col = local_site_elev)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.7) +
  facet_wrap(~metric, scales = "free_y")


subset(wy_met_all, select = -yr_ppt) %>%
  gather(metric, value, avgtmin:tmin) %>%
  mutate(local_site_elev2 = factor(simple_name, rev(levels(local_site_elev))),
         metric2 = factor(metric, levels = c("tmax", "avgtmax", "tmean", "avgtmin", "tmin"), labels = c("Max", "Avg. Max", "Avg.", "Avg. Min", "Min"))) %>%
  ggplot(aes(wy, value, col = metric2)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.7) +
  facet_grid(~local_site_elev2, space = "free_x", scales = "free_x")

striplabels <- unique(wy_met_all$site_label)[unique(wy_met_all$local_site_elev)]
anntemp <- subset(wy_met_all, select = -yr_ppt) %>%
  gather(metric, value, avgtmin:tmin) %>%
  # convert to F
  #mutate(value = (value*1.8) +32) %>%
  group_by(local_site, metric) %>%
  mutate(rollingtemp = zoo::rollmean(value, k = 5, fill = NA, align = "right")) %>%
  ungroup() %>%
  mutate(local_site_elev2 = factor(simple_name, rev(levels(local_site_elev))),
         station = paste(casefold(local_site, upper = T), elevation, "m"),
         station = factor(station, levels = c("D1 3734 m", "SDL 3528 m", "C1 3022 m")),
         metric2 = factor(metric, levels = c("tmax", "avgtmax", "tmean", "avgtmin", "tmin"), labels = c("Max", "Avg. Max", "Avg.", "Avg. Min", "Min"))) %>%
  ggplot(aes(wy, value, col = metric2)) +
  geom_line(alpha = 0.5) +
  geom_point(aes(fill = metric2), pch = 21, col = "grey50", alpha = 0.7) +
  geom_line(aes(wy, rollingtemp), lwd = 1, alpha = 0.7) +
  scale_x_continuous(breaks = seq(1880, 2020, 20)) +
  scale_y_continuous(breaks = seq(-40, 100, 20)) +
  scale_color_brewer(name = NULL, palette = "PuOr") +
  scale_fill_brewer(name = NULL, palette = "PuOr") +
  labs(y = "Temperature (°C)", x = "Water year (Oct 1-Sep 30)") +
  #labs(y = "Temperature (°F)", x = "Water year (Oct 1-Sep 30)") +
  facet_wrap(~station, nrow = 3) +
  theme_bw() +
  theme(legend.position = c(0.05, 0.45),
        legend.justification = c("left", "bottom"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(4, "pt"),
        legend.key = element_rect(fill = "transparent"),
        axis.title = element_text(size = 10),
        strip.background = element_rect(fill= "transparent"),
        strip.text = element_text(face = "bold"))


# time series annual ppt
annppt <- subset(wy_met_all, select = c(wy:local_site, elevation, yr_ppt, site_label)) %>%
  # put ppt in inches
  #mutate(yr_ppt = yr_ppt/25.4) %>%
  mutate(local_site_elev2 = factor(simple_name, rev(levels(local_site_elev))),
         station = paste(casefold(local_site, upper = T), elevation, "m"),
         station = factor(station, levels = c("D1 3734 m", "SDL 3528 m", "C1 3022 m"))) %>%
  group_by(local_site) %>%
  mutate(rollingppt = zoo::rollmean(yr_ppt, k = 5, fill = NA, align = "right")) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(wy, yr_ppt), alpha = 0.5, col = "dodgerblue") +
  geom_point(aes(wy, yr_ppt), pch = 21, col ="blue", alpha = 0.7, fill = "dodgerblue") +
  geom_line(aes(wy, rollingppt),lwd = 1, col = "blue", alpha = 0.7) +
  #geom_smooth(method = "gam") +
  labs(y = "Precipitation (mm)", x = "Water year (Oct 1-Sep 30)") +
  scale_x_continuous(breaks = seq(1880, 2020, 20)) +
  facet_wrap(~station, nrow = 4) +
  theme_bw() +
  theme(legend.position = c(0.1,1),
        legend.justification = c(1,1),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        axis.title = element_text(size = 10),
        strip.background = element_rect(fill= "transparent"),
        strip.text = element_text(face = "bold"))

# what does log look like?
subset(wy_met_all, select = c(wy:local_site, yr_ppt)) %>%
  mutate(local_site_elev2 = factor(simple_name, rev(levels(local_site_elev))),
         yr_ppt = log(yr_ppt)) %>%
  group_by(local_site) %>%
  mutate(rollingppt = zoo::rollmean(yr_ppt, k = 10, fill = NA, align = "right")) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(wy, yr_ppt), alpha = 0.5, col = "dodgerblue") +
  geom_point(aes(wy, yr_ppt), pch = 21, col ="blue", alpha = 0.7, fill = "dodgerblue") +
  geom_line(aes(wy, rollingppt), lwd = 1.5, col = "blue", alpha = 0.7) +
  #geom_smooth(method = "gam") +
  scale_x_continuous(breaks = seq(1880, 2020, 20)) +
  facet_wrap(~local_site_elev, nrow = 4) +
  theme_bw()


ts_out <- plot_grid(annppt, anntemp, ncol = 2, 
          align = "vh", rel_widths = c(0.8, 1))
ggsave("nwt_climate/figs/annual_climate_timeseries.jpg", ts_out,
       width = 3.85, height = 5.2, units = "in", scale = 1.3)

ggsave(paste0(figpath, "annual_climate_timeseries.pdf"), width = 8, height = 5.5, units = "in", scale = 1.35)
ggsave(paste0(figpath, "annual_climate_timeseries.png"), width = 8, height = 5.5, units = "in", scale = 1.35)

# 
