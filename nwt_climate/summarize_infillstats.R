# summarize climate dats for methods

library(tidyverse)
library(lubridate)
source("nwt_climate/R/fetch_data_functions.R") # to read in prepped tidy datasets

datpath <- "/Users/scarlet/Documents/nwt_lter/nwt_climate/data/"

# read in saddle dats used for jhux and mo papers
# > qc'd, gapfilled, homogenized/date-cutoff daily datasets

sdl_temp <- readRDS(paste0(datpath, "homogenize/nwt_sdl_homogenized_temperature_draft.rds"))
sdl_ppt <- readRDS(paste0(datpath, "infill/sdlPPT_infilled_draft_Sep1987.rds"))
sdl_ppt_pretty <- read_csv(paste0(datpath, "publish/sdl_daily_precip_gapfilled_ongoing.csv"))
  
# list all rds files in prepped data folder (full path directory)
rdsfiles <- list.files(paste0(datpath, "prep"), pattern = "rds", full.names = T)
# read in prepared temp data
charttemp <- get_tidydat("chartTemp", rdsfiles, c("*")) # use asterisk if want all metrics available
nwtlog <- get_tidydat("nwtlog", rdsfiles, "*")
ameriflux <- get_tidydat("ameri", rdsfiles, "airtemp")
snotel <- get_tidydat("sno", rdsfiles, "airtemp")
ghcnd <- get_tidydat("ghc", rdsfiles, c("temp", "TOBS"))


# specify dates to summarize
start_date <- min(sdl_temp$date)#"2007-09-01" #"1990-01-01"
end_date <- max(sdl_temp$date)#"2021-08-31" #  "2021-12-31"
study_dates <- seq.Date(as.Date(start_date), as.Date(end_date), 1)


# how many raw dats were missing?
# I want to know.. how many max, raw, min, across instruments, as a whole/all togethert
# similarly: how many missing days in sdl
sdl_temp_raw <- subset(sdl_temp, date %in% study_dates) %>%
  subset(select = grepl("date|yr|mon|raw|logger", names(.))) %>%
  subset(select = !grepl("flag", names(.))) %>%
  gather(met, val, raw_airtemp_max:ncol(.)) %>%
  mutate(temp = str_extract(met, "min|max|avg"),
         met = gsub("_max|_avg|_min", "", met)) %>%
  distinct() %>%
  #spread(temp, val) %>%
  subset(!(grepl("hmp", met) & yr < 2018)) %>%
  subset(!(grepl("hmp", logger) & !grepl("hmp", met))) %>%
  arrange(date)


# check missing amounts for study period
temp_missing <- sdl_temp_raw %>%
  mutate(countna = sum(is.na(val)),
         global_nobs = length(val),
    global_missing = sum(is.na(val))/nrow(.)) %>%
  # now by logger
  group_by(logger, met,global_nobs, global_missing, countna) %>%
  summarise(nobsNA = sum(is.na(val)),
            lognobs = length(val),
              pctNA = sum(is.na(val))/length(val)) %>%
  ungroup() %>%
  mutate(relNA = nobsNA/countna)

# ID how many were pulled for flagging in study period
sdl_temp_flagging <- subset(sdl_temp, date %in% study_dates) %>%
  subset(select = grepl("date|yr|mon|logger|flag", names(.)))
# separate qc flagging
sdl_temp_qcflagging <- sdl_temp_flagging %>%
  subset(select = grepl("date|yr|mon|logger|qc", names(.))) %>%
  gather(met, val, airtemp_max_qcflag:ncol(.)) %>%
  mutate(airtemp = str_extract(met, "airtemp_min|airtemp_max|airtemp_avg"),
         met = gsub("^a.*_max_|^a.*_avg_|^a.*_min_", "", met)) %>%
  distinct() %>%
  # need to remove hmps before 2018
  subset(!(grepl("hmp", met) & yr < 2018))

sdl_temp_rawflagging <- sdl_temp_flagging %>%
  subset(select = grepl("date|yr|mon|logger|_flag", names(.))) %>%
  gather(met, val, raw_airtemp_max_flag:ncol(.)) %>%
  mutate(airtemp = str_extract(met, "airtemp_min|airtemp_max|airtemp_avg"),
         met = gsub("^.*_max_|^.*_avg_|^.*_min_", "", met)) %>%
  distinct()  %>%
  # need to remove hmps before 2018
  subset(!(grepl("hmp", met) & yr < 2018))

# check unique flags
unique(sdl_temp_rawflagging$val) # only q would be something i removed
View(subset(sdl_temp, date %in% sdl_temp_rawflagging$date[sdl_temp_rawflagging$val == "q"])) # I added a qc flag if I pulled it, so going by qc flag is fine

# calculate relpct like for missing -- for study period
sdl_temp_qcflagging <- sdl_temp_qcflagging %>%
  mutate(countflagged = sum(!is.na(val)),
         pctglobal_flagged = sum(!is.na(val))/nrow(.),
         pctglobal_flagged = round(pctglobal_flagged*100, 4)) %>%
  # now by logger
  group_by(logger, met, pctglobal_flagged, countflagged) %>%
  summarise(nobsFlagged = sum(!is.na(val)),
            pctFlagged = sum(!is.na(val))/length(val),
            pctFlagged = round(pctFlagged * 100,4)) %>%
  ungroup() %>%
  mutate(relpct_flagged = round((nobsFlagged/countflagged)*100,4))

# source station check for study period
sdl_temp_sources <- subset(sdl_temp, date %in% study_dates) %>%
  subset(select = grepl("date|yr|mon|logger|source", names(.))) %>%
  gather(sourcestat, val, source_station:ncol(.)) %>%
  # remove hmps before 2018
  subset(!(grepl("hmp", sourcestat) & yr < 2018)) %>%
  mutate(global_count = sum(!is.na(val)),
         global_pctinfill = global_count/length(val)) %>%
  # calculate relative contributions
  group_by(global_count, global_pctinfill, val) %>%
  summarise(source_count = length(date)) %>%
  ungroup() %>%
  mutate(source_pct = round((source_count/global_count)*100,4))

with(sdl_temp_sources, sum(source_pct[grepl("^sdl", val)])) # 46%
with(sdl_temp_sources, sum(source_pct[grepl("^d1", val)])) # 36%


missing_dates <- subset(sdl_temp_raw, is.na(val), select = c(logger, met, date, yr, mon)) %>%
    distinct()
  
infill_temp_dates <- sdl_temp[grepl("logger|date|yr|mon|source_station", names(sdl_temp))]  %>%
    gather(sourcestat, val, source_station:ncol(.)) %>%
    distinct() %>%
    subset(!is.na(val))
  
length(unique(missing_dates$date)) #1419 missing dates (257 for JHux study period)
summary(unique(infill_temp_dates$date) %in% unique(missing_dates$date))
  

# -- run with gapfill precip ----
#sdl_ppt_studyperiod <- subset(sdl_ppt, date %in% study_dates) 
sdl_ppt_studyperiod <- subset(sdl_ppt, date >= as.Date("1987-10-01")) %>% rename(compare_qcnote = compare_qcflag) # publishing starting WY 1988 (Oct 01, 1987)
sdl_ppt_studyperiod <- sdl_ppt_pretty # if including summer months JJAS 1981-1987

# how many were missing?
# summing: cells that have no ppt_tot record, cells that are accumulated precip greater than 0, and cells that were infilled by an outdated method
with(sdl_ppt_studyperiod, sum((is.na(raw_ppt_tot) | grepl("outdated", infill_qcnote) | !raw_qdays %in% c(1, NA) ) & flag_2 != "G")/length(raw_ppt_tot))
# using 1981 summer + 1988 wy dataset: 0.08568207; using wy 1988 onwards only: 0.08928143
# > either way, both about 9% rounding

# how many not AA -- some of these are missing and some were flagged
nrow(subset(sdl_ppt_studyperiod, !grepl("SDL", source_station)))/nrow(sdl_ppt_studyperiod) # 0.0908 (9%) adjusted by another station wy 1988; 0.09417512 with early summer
nrow(subset(sdl_ppt_studyperiod, flag_1 == "A" & flag_2 %in% c("G", "H")))/nrow(sdl_ppt_studyperiod) # 0.0025 had 0 accum or accum divided equally; 0.002856069 using pretty

# how many additional cells flagged by QC?
# anything that has a comparative qc note that isn't about post-infilling overcatch only, and that had qdays == 1 (would have been used as is)
# multiply by 100 for percentage
with(sdl_ppt_studyperiod, sum(!is.na(compare_qcnote) & !grepl("post.infilling", compare_qcnote) & raw_qdays == 1)/nrow(sdl_ppt_studyperiod)) * 100 #0.796693, so about 0.8%, 106 days of 13305


# methods
nrow(subset(sdl_ppt_studyperiod, flag_1 %in% c("B", "C")))/nrow(sdl_ppt_studyperiod) # 4.4% infilled via method 1 (short window) [pretty: 0.04163848]
nrow(subset(sdl_ppt_studyperiod, flag_1 %in% c("D", "E")))/nrow(sdl_ppt_studyperiod) # 4.8% infilled via method 2 (historic) [pretty: 0.05253664]

# check D1 -- what I recently infilled vs. what's there
nrow(subset(d1_out_tkctw, !grepl("D1", source_station)))/nrow(d1_out_tkctw) # 0.274 (27%) adjusted by another station
# what about since 1980?
nrow(subset(d1_out_tkctw, !grepl("D1", source_station) & year >= 1980))/nrow(subset(d1_out_tkctw, year >= 1980)) # 8% since 1980
nrow(subset(d1_out_tkctw, flag_1 == "A" & flag_2 %in% c("G", "H")))/nrow(d1_out_tkctw) # 0.017 had 0 accum or accum divided equally

# methods
nrow(subset(d1_out_tkctw, flag_1 %in% c("B", "C")))/nrow(d1_out_tkctw) # 2.3% infilled via method 1 (short)
nrow(subset(d1_out_tkctw, flag_1 %in% c("D", "E")))/nrow(d1_out_tkctw) # 25% infilled via method 2 (historic)
# is there a difference by period? (e.g., earlier trends toward method 2)
nrow(subset(d1_out_tkctw, flag_1 %in% c("B", "C") & year >=1980))/nrow(subset(d1_out_tkctw, year >= 1980)) # 2.7% infilled via method 1 (short)
nrow(subset(d1_out_tkctw, flag_1 %in% c("D", "E") & year >=1980))/nrow(subset(d1_out_tkctw, year >= 1980)) # 7% infilled via method 2 (historic)

# check C1 --
nrow(subset(c1_out_tkctw, !grepl("C1", source_station)))/nrow(c1_out_tkctw) # 0.164 (16%) adjusted by another station
nrow(subset(c1_out_tkctw, flag_1 %in% c("B", "C")))/nrow(c1_out_tkctw) # 0.5% infilled via method 1 (short)
nrow(subset(c1_out_tkctw, flag_1 %in% c("D", "E")))/nrow(c1_out_tkctw) # 15.8% infilled via method 2 (historic)
# is there a difference by period? (e.g., earlier trends toward method 2)
nrow(subset(c1_out_tkctw, flag_1 %in% c("B", "C") & year >=1980))/nrow(subset(c1_out_tkctw, year >= 1980)) # 0.007% infilled via method 1 (short)
nrow(subset(c1_out_tkctw, flag_1 %in% c("D", "E") & year >=1980))/nrow(subset(c1_out_tkctw, year >= 1980)) # 2% infilled via method 2 (historic)


# -- WHAT ARE THE MAIN SOURCES FOR SDL? ---
sort(summary(factor(sdl_ppt_studyperiod$source_station))/nrow(sdl_ppt_studyperiod))*100
# 91% of Saddle data come from Saddle
# 4% infilled from D1, then UniCamp (0.75%), then Sawtooth (.5%), then Grand Lake (53500, 0.474%), then HighLo (0.4%), then Lake Eldo, Niwot, Boulder 14W .. even Estes before C1

# summarise source stations more explicitly
# -- need to count anything: B (predicted from accumulated not 0), I (entirely infilled by another station)
# A = from SDL, G = 0 accumulated, H = SDL accumulated divided by # of days
sdl_ppt_sourcesummary <- mutate(sdl_ppt_studyperiod, informed = (flag_2 %in% c("B", "I")) | (flag_1 != "A" & flag_2 == "A")) %>%
  group_by(informed) %>%
  mutate(countNA = length(precip)) %>%
  group_by(informed, countNA, source_station) %>%
  summarise(nobs = length(precip)) %>%
  ungroup() %>%
  mutate(relpct = (nobs/countNA)*100) %>%
  arrange(desc(nobs))

# how much "missing" data was non-0 accumulated precip vs. true missing?
sdl_missing_breakdown <- sdl_ppt_studyperiod %>%
                                # if flag is A, accumulated ppt is 0, qdays = 1 and pulled for JM note or during QC review, count as present
                                mutate(status = ifelse(flag_1 == "A" | flag_2 == "G", "present",
                                                ifelse(flag_1 != "A" & flag_2 %in% c("B", "H", "I"), "accumulated",
                                                       ifelse(flag_1 != "A" & flag_2 == "A", "true missing", NA))),
                                       # update status so if it was flagged by JM or by me in QC but otherwise present, mark as present
                                       status = ifelse((grepl("JM flagged", infill_qcnote) & raw_qdays == 1) | (!is.na(compare_qcnote) & grepl("^1$", raw_qdays)), "qcremoved", status),
                                totnobs = length(precip)) %>%
  group_by(totnobs, status) %>%
  summarise(statnobs = length(precip)) %>%
  ungroup() %>%
  mutate(statpct = 100*(statnobs/totnobs))

# summarise missing by month of year
sdl_missing_breakdown_month <- sdl_ppt_studyperiod %>%
  # if flag is A, accumulated ppt is 0, qdays = 1 and pulled for JM note or during QC review, count as present
  mutate(status = ifelse(flag_1 == "A" | flag_2 == "G", "present",
                         ifelse(flag_1 != "A" & flag_2 %in% c("B", "H", "I"), "accumulated",
                                ifelse(flag_1 != "A" & flag_2 == "A", "true missing", NA))),
         # update status so if it was flagged by JM or by me in QC but otherwise present, mark as present
         status = ifelse((grepl("JM flagged", infill_qcnote) & raw_qdays == 1) | (!is.na(compare_qcnote) & grepl("^1$", raw_qdays)), "qcremoved", status),
         totnobs = length(precip),
         mon = month(date)) %>%
  group_by(totnobs, mon) %>%
  mutate(monnobs = length(precip)) %>%
  group_by(totnobs, mon, monnobs, status) %>%
  summarise(statnobs = length(precip)) %>%
  ungroup() %>%
  mutate(statpct = 100*(statnobs/monnobs)) %>%
  arrange(status, mon) %>%
  group_by(status) %>%
  mutate(winter_totnobs = sum(monnobs[mon %in% c(10:12, 1:5)]),
         winter_statnobs = sum(statnobs[mon %in% c(10:12, 1:5)]),
         winter_statpct = 100*(winter_statnobs/winter_totnobs)) %>%
  ungroup()

sdl_missing_breakdown_winter <- sdl_ppt_studyperiod %>%
  # if flag is A, accumulated ppt is 0, qdays = 1 and pulled for JM note or during QC review, count as present
  mutate(status = ifelse(flag_1 == "A" | flag_2 == "G", "present",
                         ifelse(flag_1 != "A" & flag_2 %in% c("B", "H", "I"), "accumulated",
                                ifelse(flag_1 != "A" & flag_2 == "A", "true missing", NA))),
         # update status so if it was flagged by JM or by me in QC but otherwise present, mark as present
         status = ifelse((grepl("JM flagged", infill_qcnote) & raw_qdays == 1) | (!is.na(compare_qcnote) & grepl("^1$", raw_qdays)), "qcremoved", status),
         totnobs = length(precip),
         period = ifelse(month(date) %in% 6:9, "JJAS", "winter")) %>%
  group_by(totnobs, period) %>%
  mutate(periodnobs = length(precip)) %>%
  group_by(totnobs, period, periodnobs, status) %>%
  summarise(statnobs = length(precip)) %>%
  ungroup() %>%
  mutate(statpct = 100*(statnobs/totnobs)) %>%
  arrange(period,status)


# how much was from ppt accum? vs. source station indep infill?
nrow(subset(sdl_ppt_studyperiod, flag_2 %in% c("B", "G", "H"))) # 918 rows were backfilled by other station
nrow(subset(sdl_ppt_studyperiod, flag_2 %in% c("B", "G", "H")))/nrow(sdl_ppt_studyperiod) # 6% backfilled
with(subset(sdl_ppt_studyperiod, flag_2 %in% c("B", "G", "H")), round(sort(summary(factor(source_station))/length(date))*100, 5))

nrow(subset(sdl_ppt_studyperiod, flag_1 != "A" & !flag_2 %in% c("B", "G", "H"))) # 485 rows
nrow(subset(sdl_ppt_studyperiod, flag_1 != "A" & !flag_2 %in% c("B", "G", "H")))/nrow(sdl_ppt_studyperiod) # about 3%
with(subset(sdl_ppt_studyperiod, flag_1 != "A" & !flag_2 %in% c("B", "G", "H")), round(sort(summary(factor(source_station))/length(date))*100, 5))
nrow(subset(sdl_ppt_studyperiod, flag_1 != "A")) # to be sure, sumcheck

# what about anything but divide accumulated and 0 accumulated?
with(subset(sdl_ppt_studyperiod, flag_1 != "A" & !flag_2 %in% c("G", "H")), round(sort(summary(factor(source_station))/length(date))*100, 5))

# how much was gap-filled by year or decade?
decade_stats <- mutate(sdl_ppt_studyperiod, decade = paste0(substr(year,1,3),0),
                       summer = month(date) %in% 6:9,
                       infilled = (!grepl("SDL", source_station)), removed = !is.na(compare_qcnote)) %>%
  group_by(decade, year, summer, infilled, removed) %>%
  summarise(nobs = sum(!is.na(pvalue)),
            nobs_check = length(date))

ggplot(subset(decade_stats, infilled), aes(year, nobs, fill = paste(summer, removed))) +
  geom_col(col = "grey30") +
  scale_fill_brewer(palette = "Paired") +
  #facet_wrap(summer~., nrow = 2, scales = "free_y") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0.05,0)) +
  theme_classic()

#d1
round(sort(summary(factor(d1_out_tkctw$source_station))/nrow(sdl_ppt))*100, 5)
# most informative sources:
# Fraser (14%), Grand Lake 7%, C1 5.4%, other Grand Lake 6SSW 5.17%, SDL 4%, Allenspark 3.77% 
round(sort(summary(factor(d1_out_tkctw$source_station[d1_out_tkctw$year >= 1980]))/nrow(sdl_ppt[d1_out_tkctw$year >= 1980,]))*100, 5)
# most informative sources:
# SDL 3.89%, C1 1.36%, Grand Lake 1NW 1.056%, Fraser 0.44%, Sawtooth (0.42%), US NR1 (0.37%)
# relative amts
round(sort(summary(factor(d1_out_tkctw$source_station[d1_out_tkctw$year >= 1980 & !grepl("D1", d1_out_tkctw$source_station)]))/nrow(sdl_ppt[d1_out_tkctw$year >= 1980 & !grepl("D1", d1_out_tkctw$source_station),]))*100, 5)
# sdl infilled about 40%, C1 14%, Grand Lake 10.82%, Fraser 4.5%, Sawtooth 4.27%


# -- review homogenization cutoff ----
# ctw will recommend start at jan 1 1988 for full-year use. could possibly go back to Oct 1 1987

# compare w c1 and d1 to be sure it's at least sensical

otherstations <- subset(alldats, yr > 1980 & !local_site %in% c("sdl", "c1", "d1")) %>%
  # agg on yr if has > 27 days
  group_by(yr, mon, local_site) %>%
  summarise(nobs = length(measurement[!is.na(measurement)]),
            mon_ppt = sum(measurement, na.rm = T)) %>%
  ungroup() %>%
  mutate(mon_ppt = ifelse((mon == 2 & nobs  < 26)| (mon != 2 & nobs < 27), NA, mon_ppt))

nwt_stations <- dplyr::select(sdl_ppt, -precip) %>%
  rename(precip = precip_winteradj) %>%
  subset(select = names(d1_out_tkctw)) %>%
  rbind(d1_out_tkctw, c1_out_tkctw) %>%
  mutate(mon = month(date)) %>%
  subset(year > 1980) %>%
  group_by(local_site, year, mon) %>%
    summarise(mon_ppt = sum(precip)) %>%
  rename(yr = year)

sdl_order <- allsites$paired_site[allsites$station_name == "sdl"]
sdl_order[sdl_order %in% c("sdl", "d1", "c1")] <- casefold(sdl_order[sdl_order %in% c("sdl", "d1", "c1")], upper = T)

all_precip <- rbind(nwt_stations, otherstations[names(nwt_stations)]) %>%
  mutate(location = ifelse(local_site %in% c("C1", "D1", "SDL"), "NWT LTER", "Other"),
         plot_date = as.Date(paste(yr, mon, 01, sep = "-")))

ggplot(all_precip, aes(plot_date, mon_ppt, group = local_site, col = local_site)) +
  geom_line(alpha = 0.7) +
  geom_point(alpha = 0.7) +
  facet_wrap(~location, nrow = 2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  theme_minimal() +
  theme(legend.position = "none")

subset(all_precip, yr < 1990) %>%
  mutate(local_site = factor(local_site, levels = sdl_order)) %>%
  ggplot(aes(plot_date, mon_ppt, group = local_site, col = local_site)) +
  geom_line(aes(linewidth = local_site == "SDL"), alpha = 0.7) +
  # plot sdl on both
  geom_line(data = subset(all_precip, yr < 1990 & local_site == "SDL"), aes(linewidth = local_site == "SDL"), col = "black", alpha = 0.7) +
  #geom_point(alpha = 0.7) +
  facet_wrap(~location, nrow = 2) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%y", expand = c(0,0)) +
  scale_linewidth_discrete(range = c(0.75,2)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))

subset(all_precip, yr %in% c(1986:1996)) %>%
  mutate(local_site = factor(local_site, levels = sdl_order)) %>%
  ggplot(aes(plot_date, mon_ppt)) +
  geom_line(aes(group = local_site, col = local_site), alpha = 0.7) + #linewidth = local_site == "SDL
  # plot sdl on both
  geom_line(data = subset(all_precip, yr %in% c(1986:1996) & local_site == "SDL"), lwd = 1.5, col = "red", alpha = 0.7) + #aes(linewidth = local_site == "SDL"), 
  stat_summary(data =  subset(all_precip, yr %in% c(1986:1996) & location == "Other"), geom = "line", lwd = 1.5, alpha = 0.7) +
  #geom_point(alpha = 0.7) +
  facet_wrap(~location, nrow = 2) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%y", expand = c(0,0)) +
  #scale_linewidth_discrete(range = c(0.75,2)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))

subset(all_precip, plot_date >= as.Date("1987-10-01")) %>%
  mutate(local_site = factor(local_site, levels = sdl_order)) %>%
  ggplot(aes(plot_date, mon_ppt)) +
  geom_line(aes(group = local_site, col = local_site), alpha = 0.7) + #linewidth = local_site == "SDL
  # plot sdl on both
  geom_line(data = subset(all_precip, plot_date >= as.Date("1987-10-01") & local_site == "SDL"), lwd = 1, col = "blue", alpha = 0.7) + #aes(linewidth = local_site == "SDL"), 
  stat_summary(data =  subset(all_precip, plot_date >= as.Date("1987-10-01") & location == "Other"), geom = "line", lwd = 1, alpha = 0.7) +
  #geom_point(alpha = 0.7) +
  facet_wrap(~location, nrow = 2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", expand = c(0,0)) +
  #scale_linewidth_discrete(range = c(0.75,2)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) # I'm gonna say start at Oct 1 1987, that's when it starts getting less problematic

# check by year
season_yr_ppt <- all_precip %>%
  mutate(season = ifelse(mon %in% 6:8, "summer", 
                                        ifelse(mon %in% c(12,1,2), "winter", 
                                               ifelse(mon %in% c(9:11), "fall", "spring"))),
                        ecoyr = ifelse(mon %in% 9:12, yr+1, yr)) %>%
  group_by(ecoyr, local_site) %>%
  mutate(ecoyr_ppt = sum(mon_ppt),
         ecoyr_nobs = length(mon_ppt[!is.na(mon_ppt)]),
         ecoyr_ppt = ifelse(ecoyr_nobs == 12, ecoyr_ppt, NA)) %>%
  group_by(location, local_site, ecoyr, ecoyr_ppt, ecoyr_nobs, season) %>%
  summarise(season_ppt = sum(mon_ppt),
            season_nobs = length(mon_ppt),
            season_ppt = ifelse(season_nobs == 3, season_ppt, NA)) %>%
  ungroup() %>%
  mutate(season = factor(season, levels = c("fall", "winter", "spring", "summer")),
         local_site = factor(local_site, levels = sdl_order))

ggplot(season_yr_ppt, aes(ecoyr, ecoyr_ppt)) +
  geom_vline(aes(xintercept = 1988)) +
  geom_line(aes(col = local_site)) +
  geom_line(data = subset(season_yr_ppt, local_site == "SDL"), col = "blue") +
  stat_summary(data = subset(season_yr_ppt, location == "Other"), geom= "line", lwd = 1.5, alpha = 0.7) +
  facet_wrap(~location, nrow = 2) +
  scale_x_continuous(breaks = seq(1980,2020, 2), expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(season_yr_ppt, aes(ecoyr, season_ppt)) +
  geom_vline(aes(xintercept = 1988)) +
  geom_line(aes(col = local_site)) +
  geom_line(data = subset(season_yr_ppt, local_site == "SDL"), col = "blue") +
  stat_summary(data = subset(season_yr_ppt, location == "Other"), geom= "line", lwd = 1.5, alpha = 0.7) +
  facet_grid(season~location) +
  scale_x_continuous(breaks = seq(1980,2020, 2), expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# out of curiosity, what do things look like if i plot my 1980 infilled fwd d1 and c1 data
ctw_d1c1 <- rbind(d1_out, c1_out) %>%
  mutate(mon = month(date)) %>%
  group_by(local_site, mon, year) %>%
  summarise(mon_ppt = sum(precip)) %>%
  ungroup() %>%
  mutate(ecoyr = ifelse(mon %in% 9:12, year + 1, year),
         season = ifelse(mon %in% 6:8, "summer", 
                         ifelse(mon %in% c(12,1,2), "winter", 
                                ifelse(mon %in% c(9:11), "fall", "spring"))),
         location = "NWT LTER") %>%
  group_by(ecoyr, local_site) %>%
  mutate(ecoyr_ppt = sum(mon_ppt),
         ecoyr_nobs = length(mon_ppt[!is.na(mon_ppt)]),
         ecoyr_ppt = ifelse(ecoyr_nobs == 12, ecoyr_ppt, NA)) %>%
  group_by(location, local_site, ecoyr, ecoyr_ppt, ecoyr_nobs, season) %>%
  summarise(season_ppt = sum(mon_ppt),
            season_nobs = length(mon_ppt),
            season_ppt = ifelse(season_nobs == 3, season_ppt, NA)) %>%
  ungroup() %>%
  mutate(season = factor(season, levels = c("fall", "winter", "spring", "summer")))

# add in to tk dat + sdl plot
ggplot(subset(season_yr_ppt, location == "NWT LTER"), aes(ecoyr, season_ppt)) +
  geom_vline(aes(xintercept = 1988)) +
  geom_line(aes(col = local_site)) +
  geom_line(data = ctw_d1c1, lty = 3) +
  #stat_summary(data = subset(season_yr_ppt, location == "Other"), geom= "line", lwd = 1.5, alpha = 0.7) +
  facet_grid(season~local_site) +
  scale_x_continuous(breaks = seq(1980,2020, 2), expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) # mostly the same

ggplot(subset(season_yr_ppt, location == "NWT LTER"), aes(ecoyr, ecoyr_ppt, col = local_site, group = local_site)) +
  geom_vline(aes(xintercept = 1988)) +
  geom_line() +
  geom_line(data = ctw_d1c1, lty = 4, lwd = 1.5, alpha = 0.6) +
  #stat_summary(data = subset(season_yr_ppt, location == "Other"), geom= "line", lwd = 1.5, alpha = 0.7) +
  #facet_grid(~local_site) +
  scale_x_continuous(breaks = seq(1980,2020, 4), expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) # also mostly the same


# how many data values removed for questionable blowing snow, and how different were estimates from raw?
unique(sdl_ppt_studyperiod$compare_qcnote)
with(subset(sdl_ppt_studyperiod, !is.na(compare_qcnote)), (summary(factor(compare_qcnote)) / nrow(sdl_ppt_studyperiod))*100)
(table(is.na(sdl_ppt_studyperiod$compare_qcnote[!grepl("^snow event", sdl_ppt_studyperiod$compare_qcnote)]))/(nrow(sdl_ppt_studyperiod)))*100 # 0.77% of observations pulled

ggplot(subset(sdl_ppt_studyperiod, !is.na(compare_qcnote)), aes(date, precip_winteradj - raw_ppt_tot, col = compare_qcnote)) +
  geom_hline(aes(yintercept =0), lty = 2, col = "grey50") +
  geom_point() +
  facet_wrap(~month(date), scales = "free_x")

ggplot(subset(sdl_ppt_studyperiod, !is.na(compare_qcnote)), aes(raw_ppt_tot, precip_winteradj, col = compare_qcnote)) +
  geom_hline(aes(yintercept =0), lty = 2, col = "grey50") +
  geom_point() +
  facet_wrap(~month(date), scales = "free")


# -- SUMMARIZE INPUT DATA -----
# double check time periods for loggers and sdl chart

ggplot(subset(nwtlog, local_site == "sdl" & metric == "airtemp_avg" & logger == "cr21x" & yr < 1994), 
       aes(date, is.na(measurement), col = factor(month(date)))) +
  geom_point() +
  facet_wrap(~yr, scales = "free") +
  theme(legend.position = "none")

# compare temp in 1990 between CR21X and SDL chart. Metadata notes comparisons by Connie Woodhouse suggest CR21x several degrees cooler than chart then:
subset(nwtlog, local_site == "sdl" & logger == "cr21x" & yr ==1990) %>%
  rename(log_measurement = measurement) %>%
  left_join(charttemp) %>%
ggplot(aes(date, log_measurement - measurement)) +
  geom_hline(aes(yintercept = 0), col = "red") +
  geom_point(alpha = 0.7) +
  #geom_point(data = subset(charttemp, local_site == "sdl" & yr == 1990), col = "blue", alpha = 0.7) +
  facet_wrap(~metric, nrow = 3, scales = "free")
