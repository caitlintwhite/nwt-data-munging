# this df from gapfill_sdlts.R
sapply(split(hmp_dailysds$ws_scaletemp, hmp_dailysds$metric), function(x) table(abs(x)>=2))
# $airtemp_avg
# 
# FALSE 
# 13127 
# 
# $airtemp_max
# 
# FALSE  TRUE 
# 13126     1 
# 
# $airtemp_min
# 
# FALSE  TRUE 
# 13088    32 


sapply(split(hmp_dailysds$ws_scaledelta, hmp_dailysds$metric), function(x) table(abs(x)>=2))
#         airtemp_avg airtemp_max airtemp_min
# FALSE       13074       13033       13032
# TRUE            3          47          32

sapply(split(hmp_dailysds$xs_scaletemp, hmp_dailysds$station_name), function(x) table(abs(x)>=2))
#         c1 logger d1 logger sdl logger
# FALSE     14544     12770      11963
# TRUE         73        10          6

sapply(split(hmp_dailysds$xs_scaledelta, hmp_dailysds$station_name), function(x) table(abs(x)>=2))
#         c1 logger d1 logger sdl logger
# FALSE     14492     12652      11915
# TRUE        104        35         15


hmp_dailysds2 <- hmp_dailysds

hmp_dailysds2 <- group_by(hmp_dailysds2, station_name, metric) %>%
  mutate(ws_rank = rank(-(abs(ws_scaletemp))))
hmp_dailysds2$flag_deltas <- apply(hmp_dailysds2[grepl("scaledelta", names(hmp_dailysds2))], 1, function(x) sum(abs(x)>=2)==2)
hmp_dailysds2$flag_temp <- apply(hmp_dailysds2[grepl("scaletemp", names(hmp_dailysds2))], 1, function(x) sum(abs(x)>=2)==2)


# calculate diff btwn cr23x adn cr21x for three days of overlap
diffcrs <- subset(nwtlog_qc, yr == 2000 & doy %in% 175:179)
ggplot(diffcrs, aes(date, raw, col = local_site)) +
  geom_line() +
  geom_point() +
  facet_wrap(~metric) +
  scale_x_date(date_label = "%d", date_breaks = "2 day") +
  theme_minimal()
diffcrs <- subset(nwtlog_qc, yr == 2000 & doy %in% 175:179) %>%
  subset(station_id == "sdl", select = -c(flag, local_site, measurement)) %>%
  spread(logger, raw) %>%
  mutate(cr21diffcr23 = cr21x - cr23x)
with(diffcrs, sapply(split(cr21diffcr23, metric), function(x) mean(x, na.rm = T)))
# airtemp_avg airtemp_max airtemp_min 
# -1.00275    -1.33000    -0.95125 


tidycsv

basecsv <- read.csv(paste0(datpath, "publish/sdl_daily_airtemp_gapfilled_ongoing2.csv"))
basecsv2 <- read_csv(paste0(datpath, "publish/sdl_daily_airtemp_gapfilled_ongoing2.csv"))


tidycsv <- read.csv(paste0(datpath, "publish/sdl_daily_airtemp_gapfilled_ongoing.csv"))
tidycsv2 <- read_csv(paste0(datpath, "publish/sdl_daily_airtemp_gapfilled_ongoing.csv"))

checktest <- read_csv(paste0(datpath, "publish/test.csv"))
checktest2 <- read.csv(paste0(datpath, "publish/test.csv"))


ggplot(subset(sdl_ppt, precip > 0 & is.na(rsquared)), aes(year, precip_winteradj, group = year)) +
  geom_violin() +
  labs(subtitle = "sdl precip (raw good values, gap-filled removed), 1981-present, with winter correction factor applied") +
  facet_wrap(~month(date))

ggplot(subset(tkd1_ppt, precip > 0 & is.na(rsquared) & year > 1980), aes(year, precip, group = year)) +
  geom_violin() +
  labs(subtitle = "d1 precip (raw good values, gap-filled removed), 1981-present") +
  facet_wrap(~month(date))

# recrunch infill station breakdown with summer 1981-1987 months added

