# script to prep/compile and standardize all edi-available and unpublished pika data from chris
# author: caitlin.t.white@colorado.edu
# project: NWT LTER temperature synthesis (project PI = Chris Ray)

# script purpose:
# had most of this code in .Rmds, but putting it in its own so compilation doesn't have to be done every time

# notes:
# from Chris re: PIKA-088:
# The two loggers at site PIKA-088 were placed in different years. 
# U was recording good data from 2016-08-16 through 2018-11-07, when it failed, and NWT-06 was recording good data from 2018-08-09 to 2020-08-12. 
# The reason they overlap from 2018-08-09 to 2018-11-07 is the techs could not find U to pull it when they placed NWT-06. 
# Both should have been deployed in similar (standard) placements, at std depth, but they certainly weren't in exactly the same position, or the crew would have found U when placing NWT-06.



# -- SETUP ----
# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)
options(stringsAsFactors = F)

# ctw functions to fetch climate datasets from EDI
source("~/github/nwt-data-munging/nwt_climate/R/fetch_data_functions.R")



# -- 1. FETCH DATA FROM EDI -----
# pika hab occupancy
pikahab <- getTabular(17) # these are plot conditions on day of survey
# there are multiple data tables with dataset, so generic read in function doesn't work. use URL to temp
pika_haboccT <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.17.2&entityid=277e15977751ea77c595c5bb45fb275e")
# pika habitat occupancy locations
pika_habocc_sites <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.17.2&entityid=313b4ae5a8cf0ce9434e24a6ceccbb73")

# pika demography
pika_demoT <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.8.5&entityid=fc3de3c583f5d9207841734ead55b709")
pika_demoT_lut <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.8.5&entityid=c1a7d0d58f5658cffccd829cf6bb6c18")



# -- 2. FETCH UNPUBLISHED DATA FROM CHRIS ----

# set path to pika data
pikadat <- list.dirs("/Users/scarlet/Documents/nwt_lter/temp_synth/unpublished_data")
# make name of subfolder name of element
names(pikadat) <- gsub("^.*temp_synth[/]","",pikadat)
# clean up names for nested folders
names(pikadat) <- gsub("^.*[/]", "", names(pikadat))
# read contents of each folder to list
pikadat_list <- sapply(as.list(pikadat), function(x) list.files(x, full.names = T))


# read in temp data and metadata
# -- cable gate (pika demography dataset) ----
cgpika_temp <- read.delim(pikadat_list$`Cable Gate talus temperatures`[grep(".txt$", pikadat_list$`Cable Gate talus temperatures`)], skip = 2,strip.white = T, blank.lines.skip = T, header = T)
cgpika_meta <- read.csv(pikadat_list$`Cable Gate talus temperatures`[grep(".csv$", pikadat_list$`Cable Gate talus temperatures`)], strip.white = T, blank.lines.skip = T)

summary(cgpika_temp) # need to convert date_time to posix, temps look okay in range


# -- excel workbook temps -----
# get tab names in excel workbook
glvpika_temp_sheets <- readxl::excel_sheets(pikadat_list$unpublished_data[grep("GLV", pikadat_list$unpublished_data)])
# read metadata
glvpika_meta <- read_excel(pikadat_list$unpublished_data[grep("GLV", pikadat_list$unpublished_data)], sheet = "metadata")
# separate glv sites from west knoll sites
glvpika_sites <- glvpika_temp_sheets[glvpika_temp_sheets %in% glvpika_meta$Datalogger[grepl("Green", glvpika_meta$Site)]]
# read in glv logger data
glvpika_list <- lapply(glvpika_sites, function(x) read_excel(pikadat_list$unpublished_data[grep("GLV", pikadat_list$unpublished_data)], sheet = x, col_names = F, col_types = "text")) 
# ^ has boilerplate info with actual data variable lines below, and a section noting high values
# dates and timestamps don't read in correctly, even specifying text values
names(glvpika_list) <- glvpika_sites
# iterate through each tab and note:
# 1. alarm value section (will have "HIGH/LOW" in col 2)
# 2. temp data section (will have "Log" in column 1)
# or could note the format sections
glvpika_temp_master <- data.frame()
for(i in glvpika_sites){
  tempdat <- glvpika_list[[i]]
  tempdat <- data.frame(tempdat)
  tempdat$rowid <- rownames(tempdat)
  # get eat section
  formatlines <- tempdat[grep("Format", tempdat[,1]),]
  names(formatlines)[names(formatlines) != "rowid"] <- letters[1:(ncol(formatlines)-1)]
  # alarm info will be between where alarm starts and histogram starts
  alarmstart <-  as.numeric(with(formatlines, rowid[grepl("HIGH/LOW", b)]))
  alarmend <- as.numeric(with(formatlines, rowid[grepl("Range", c)]))
  
  alarmdat <- read_excel(pikadat_list$unpublished_data[grep("GLV", pikadat_list$unpublished_data)], 
                         sheet = i, 
                         skip = alarmstart-1,
                         n_max = (alarmend-alarmstart-3))
  names(alarmdat)[sapply(alarmdat, function(x) all(grepl("[A-Z]+", x)|is.na(x)))] <- "temp_alarm"
  names(alarmdat)[sapply(alarmdat[2,], function(x) is.POSIXct(x) & !grepl(":",x))] <- c("date")
  names(alarmdat)[sapply(alarmdat[2,], function(x) is.POSIXct(x) & grepl(":",x))] <- c("time")
  alarmdat <- alarmdat[c("date", "time", "temp_alarm")]
  datastart <- as.numeric(with(formatlines, rowid[grepl("^Temp.+Cel", d)]))
  logdat <-  read_excel(pikadat_list$unpublished_data[grep("GLV", pikadat_list$unpublished_data)], 
                        sheet = i, 
                        skip = datastart-1,trim_ws = T)
  # first column should be NAs (remove any col that is all NAs)
  logdat <- logdat[,sapply(logdat, function(x) !all(is.na(x)))]
  # standardize temp name
  names(logdat)[grepl("^Temp.+Cel", names(logdat))]<- "temp_c"
  names(logdat)[sapply(logdat[2,], function(x) is.POSIXct(x) & !grepl(":",x))] <- c("date")
  names(logdat)[sapply(logdat[2,], function(x) is.POSIXct(x) & grepl(":",x))] <- c("time")
  # standardize loggerdat names as lowcase
  names(logdat) <- casefold(names(logdat))
  logdat <- logdat[c("date", "time", "temp_c")]
  # join alarmdat to loggerdat
  logdat <- left_join(logdat, alarmdat)
  logdat <- cbind(Datalogger = i, logdat)
  glvpika_temp_master <- rbind(glvpika_temp_master, logdat)
}

# read in and treat the west knoll pika temps
# separate glv sites from west knoll sites
wkpika_sites <- glvpika_temp_sheets[!glvpika_temp_sheets %in% glvpika_meta$Datalogger[grepl("Green", glvpika_meta$Site)]]
wkpika_sites <- wkpika_sites[wkpika_sites!= "metadata"] # remove metadata 

# read in wk logger data
wkpika_list <- lapply(wkpika_sites, function(x) read_excel(pikadat_list$unpublished_data[grep("GLV", pikadat_list$unpublished_data)], sheet = x, col_names = F, col_types = "text")) 
# ^ has boilerplate info with actual data variable lines below, and a section noting high values
# dates and timestamps don't read in correctly, even specifying text values
names(wkpika_list) <- wkpika_sites
# iterate through each tab and note:
# 1. alarm value section (will have "HIGH/LOW" in col 2)
# 2. temp data section (will have "Log" in column 1)
# or could note the format sections
wkpika_temp_master <- data.frame()
for(i in wkpika_sites){
  tempdat <- wkpika_list[[i]]
  tempdat <- data.frame(tempdat)
  
  # find col and row that has a cell value that == "Date"
  datecol <- sapply(tempdat, function(x) any(grepl("^date$|^date ", x, ignore.case = T)))
  daterow <- grep("^date$|^date ", tempdat[,datecol], ignore.case = T)
  
  # read in dat starting at data header
  logdat <-  read_excel(pikadat_list$unpublished_data[grep("GLV", pikadat_list$unpublished_data)], 
                        sheet = i, 
                        skip = daterow-1,)
  
  # print for troubleshooting
  #print(i)
  #print(str(logdat))
  
  # pull temp units
  temp_units <- names(logdat)[grepl("°|C)|F)|Cels|Far", names(logdat), ignore.case = T)]
  # pull time info
  time_gmt <- names(logdat)[grepl("GMT|hour|hrs", names(logdat), ignore.case = T)]
  
  # > standardize colnames
  # logdat should start date:temp
  datecol <- which(sapply(logdat[3,], function(x) is.POSIXct(x) & !grepl(":", x)))
  names(logdat)[datecol] <- "date"
  # tempcol
  tempcol <- which(sapply(logdat, function(x) any(is.numeric(x) & grepl("[.]", x))))
  names(logdat)[tempcol] <- "temp"
  
  # write check to see if a time col exists
  timecol <- which(sapply(logdat[3,], function(x) grepl("[0-9]:[0-9]", x))) # it may not necessarily be read as a posix, sometimes reads as character 
  if(length(timecol) ==0){
    # if no timecol, create one
    logdat$time <- ""
  }else{
    # rename
    names(logdat)[timecol] <- "time" 
  }
  logdat <- logdat[c("date", "time", "temp")]
  
  logdat$date <- as.Date(logdat$date)
  logdat$time_char <- as.character(logdat$time)
  logdat$time_char <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2} ", "", logdat$time_char)
  logdat$time <- as.POSIXct(logdat$time, format = "%H:%M:%S")
  logdat$time_info <- ifelse(length(time_gmt) == 0, "none", time_gmt)
  logdat$temp_unit <- ifelse(length(temp_units) == 0 , "none", temp_units)
  
  # row-bind
  wkpika_temp_master <- rbind(wkpika_temp_master, cbind(Datalogger = i, logdat))
}

summary(wkpika_temp_master) # max temp is 193..
summary(is.na(wkpika_temp_master))
lapply(wkpika_temp_master[c("Datalogger", "time_char", "time_info", "temp_unit")], unique)
# which data loggers have blank times
unique(wkpika_temp_master$Datalogger[wkpika_temp_master$time_char == ""]) # compared against .xlsx and looks okay
# all loggers have temp unit info..
# which data loggers don't have time info (this doesn't matter quite as much because can count nobs per day and compare)
unique(wkpika_temp_master$Datalogger[wkpika_temp_master$time_info == "none"])
# review temps
ggplot(wkpika_temp_master) +
  geom_histogram(aes(x = temp, fill = grepl("C", temp_unit))) +
  facet_wrap(~Datalogger, scales = "free") # P8_2013_edited has high F range, otherwise other scales look plausible

ggplot(subset(wkpika_temp_master, temp < 100)) +
  geom_boxplot(aes(x = month(date), y = temp, group = month(date), fill = grepl("C", temp_unit))) +
  facet_wrap(~Datalogger, scales = "free") #pm_2013 and p8_2013 are still looking unusual when drop high outliers.. unless winter months have stable temps

# check winter only
ggplot(subset(wkpika_temp_master, temp < 100 & !month(date) %in% 6:9)) +
  geom_boxplot(aes(x = month(date), y = temp, group = month(date), fill = grepl("C", temp_unit))) +
  facet_wrap(~Datalogger, scales = "free")
# split F from C
ggplot(subset(wkpika_temp_master, temp < 100 & !month(date) %in% 6:9)) +
  geom_boxplot(aes(x = Datalogger, y = temp, group = Datalogger, fill = grepl("C", temp_unit))) +
  facet_grid(month(date)~grepl("C", temp_unit), scales = "free") +
  theme(axis.text.x = element_text(angle = 90))
# check summer
ggplot(subset(wkpika_temp_master, temp < 100 & month(date) %in% 6:9)) +
  geom_violin(aes(x = Datalogger, y = temp, group = Datalogger, fill = grepl("C", temp_unit))) +
  facet_grid(month(date)~grepl("C", temp_unit), scales = "free") +
  theme(axis.text.x = element_text(angle = 90))

# check global highs and lows
with(wkpika_temp_master, sapply(split(temp, Datalogger), function(x) tail(sort(x)))) # 137 and 193 in P8_2013 are the warmest
with(wkpika_temp_master, sapply(split(temp, Datalogger), function(x) head(sort(x)))) # global lows okay
# check glv
with(glvpika_temp_master, sapply(split(temp_c, Datalogger), function(x) tail(sort(x))))
with(glvpika_temp_master, sapply(split(temp_c, Datalogger), function(x) head(sort(x)))) # okay

# convert pika temps to celsius, give same name as glv pika
wkpika_temp_master <- wkpika_temp_master %>%
  mutate(
    # subtract F by 32, then divide by 1.8; round to 2 deci places to match precision
    temp_c = ifelse(grepl("F", temp_unit), round((temp-32)/1.8, 2), temp),
    # add index (assume data read in in order, but can double check)
    index = 1:nrow(.)
  )


# -- read in more recent pika occupancy data -----
# pika occupancy temps
unpub_pikaocc_temp <- read.csv(pikadat_list$`temperature-pika-occ-survey-2021-in-situ`[grep(".txt$", pikadat_list$`temperature-pika-occ-survey-2021-in-situ`)], strip.white = T, blank.lines.skip = T, header = T)
unpub_pikaocc_meta <- read.csv(pikadat_list$`temperature-pika-occ-survey-2021-in-situ`[grep(".csv$", pikadat_list$`temperature-pika-occ-survey-2021-in-situ`)], strip.white = T, blank.lines.skip = T, header = T)

# pika survey
unpub_pikasurvey_temp <- read.csv(pikadat_list$`NWT-pika-survey-temperature-metadata-2020-v2`[1])
unpub_pikasurvey_meta <- read.csv(pikadat_list$`NWT-pika-survey-temperature-metadata-2020-v2`[[2]])


# clean up environment
rm(list = ls()[grepl("alarm|^date|^data|^temp|^form|logd|^i$|time", ls())])



# -- 3. COMPILE ALL PIKA DEMOGRAPHY DATA ----- 

# format cable gate so can rbind to pika demography
str(pika_demoT)
str(cgpika_temp)
cgpika_demoT <- cgpika_temp %>%
  mutate(date = substr(Date.Time, 1, 8),
         date = as.Date(date, format = "%m/%d/%y"),
         time = trimws(substr(Date.Time, 9, nchar(Date.Time))),
         time = gsub("[.]0$", "", time),
         date_time = as.POSIXct(paste(date, time)),
         deployment_id = cgpika_meta$Sensor.ID[1]) %>%
  rename_at(grep("Temp", names(.)), function(x) x <- "temperature") %>%
  dplyr::select(names(pika_demoT))


# check if metadata format same as pika dem
str(cgpika_meta)
str(pika_demoT_lut) # no..

cgpika_demoT_lut <- cgpika_meta %>%
  rename_all(casefold) %>%
  rename(deployment_id = sensor.id) %>%
  mutate(date_placed = as.POSIXct(paste(date.placed, time.placed), format = "%m/%d/%Y %H:%M"),
         date_pulled = as.POSIXct(paste(date.retrieved, time.retrieved), format = "%m/%d/%Y %H:%M"),
         years_insitu = paste(year(date_placed), year(date_pulled), sep = "-"),
         true_file_name = "",
         notes = "") %>%
  dplyr::select(names(pika_demoT_lut))

# add cable gate to pika demography
pika_demoT <- rbind(pika_demoT, cgpika_demoT)
pika_demoT_lut <- rbind(pika_demoT_lut, cgpika_demoT_lut)
# pull aspect, slope and elevation from notes in lut (where available)
pika_demoT_lut$aspect <- str_extract(pika_demoT_lut$notes, "ASPECT ?[:digit:]+|aspect ?[:digit:]+")
pika_demoT_lut$slope <-  str_extract(pika_demoT_lut$notes, "SLOPE ?[:digit:]+|SLOPE ?[:digit:]+")
pika_demoT_lut$elev_ft <- str_extract(pika_demoT_lut$notes, "elev ?[:digit:]+|elevation ?[:digit:]|[:digit:]+ft")
pika_demoT_lut <- mutate_at(pika_demoT_lut, c("aspect", "slope", "elev_ft"), parse_number)  


pika_demoT_all <- left_join(pika_demoT, subset(pika_demoT_lut)) %>%
  # make depth in cable gate similar letter casing as other pika demography
  mutate(depth = gsub("std", "Std", depth)) %>%
  subset(select = c(site, easting, northing, slope, aspect, elev_ft, depth, deployment_id:temperature, date_placed, date_pulled))



# -- 4. COMPILE OCCUPANCY DATA ----
# what's on EDI + embargoed for Airy

# choose final easting and northing when present, otherwise plot was not moved or was replaced by a different one
edi_occsites_lut <- mutate(pika_habocc_sites, northing = ifelse(!is.nan(northing_final), northing_final, northing_original),
                           easting = ifelse(!is.nan(easting_final), easting_final, easting_original)) %>%
  data.frame() %>%
  dplyr::select(plot,easting, northing, mdcaty, panel_current) %>%
  mutate(plotnum = as.numeric(gsub("NWT-", "", plot))) %>%
  rename(lut_plot = plot)

edi_pikaocc <- mutate(pika_haboccT, plotnum = as.numeric(gsub("[A-Z]+|-", "", plot))) %>%
  rename(dat_plot = plot) %>%
  left_join(edi_occsites_lut, by = "plotnum")

# unpubdat 1 (2019)
pikaocc_2019 <- left_join(unpub_pikaocc_temp, 
                          subset(unpub_pikaocc_meta, !grepl("double survey", notes, ignore.case = T), select = c(plot.ID, easting.plot, northing.plot)), 
                          by = c("plot" = "plot.ID")) %>%
  mutate(plotnum = as.numeric(gsub("[A-Z]+|-", "", plot))) %>%
  # to be sure location similar
  left_join(edi_occsites_lut) %>%
  mutate(diff_east = easting.plot - easting, diff_north = northing.plot - northing)

summary(pikaocc_2019[c("diff_east", "diff_north")]) # looks fine. only off by 7-6 meters at most.


# unpubdat 2 (2018)
# > this has PIKA-088, which has two loggers that overlap aug - nov 2018. 
# > pika-088 sensor U ran 2016-08-16 12:30 to 2018-11-07 03:00; sensor NWT-06 ran 2018-08-09 12:00 to 2020-08-12 09:00
# > number rows to distinguish the two sensors
pikaocc_2018 <- left_join(mutate(unpub_pikasurvey_temp, plot = trimws(plot)), 
                          distinct(unpub_pikasurvey_meta, plot, easting.sensor, northing.sensor)) %>%
  mutate(plotnum = as.numeric(gsub("[A-Z]+|-", "", plot))) %>%
  # to be sure location similar
  left_join(edi_occsites_lut) %>%
  mutate(diff_east = easting.sensor - easting, diff_north = northing.sensor - northing)

# check plot coords against sensor coords (only coords available, no plot coords)
summary(pikaocc_2018[c("diff_east", "diff_north")]) # looks fine. only off by 7-6 meters at most.
# > NWT-027 is off from plots coords by 80m, but edi metadata says plot moved and these sensor coords are closer to original plot location so maybe moved back?
# > or is typo (e.g., 99 vs 19).. or sensor really was 80m from plot center (but that seems not right?)
# > either way, it's fine. other coord is only 3 off

# stack all -- fix timestamps in unpublished pika occupancy first
stack_unpub <- subset(pikaocc_2018, select = c(plot, lut_plot, plotnum, easting:panel_current, date_time, temperature)) %>%
  rbind(pikaocc_2019[names(.)]) %>%
  arrange(plotnum) %>%
  mutate(date_time = trimws(date_time),
         date = trimws(substr(date_time, 1,11)),
         time = trimws(substr(date_time, 12, nchar(date_time))),
         date_char = nchar(date),
         time_char = nchar(time))
summary(stack_unpub[c("date_char", "time_char")])
unique(stack_unpub$time) # okay. time is to min only
stack_unpub$date_time <- as.POSIXct(stack_unpub$date_time, format = "%Y-%m-%d %H:%M", tz = "UTC")
# check that it looks good
summary(stack_unpub) # okay
# make dat_plot for rbinding (dat_plot in edit separated NWT-082 from NWT-082-WN)
stack_unpub$plot <- gsub("PIKA-", "NWT-", stack_unpub$plot)
# note that these data are unpublished
stack_unpub$source <- "Chris Ray (unpublished)"

pikaocc_all_out <- subset(edi_pikaocc, select = c(dat_plot, lut_plot, plotnum, easting:panel_current, date_time, temperature)) %>%
  rename(plot = dat_plot) %>%
  mutate(source = "edi") %>%
  rbind(stack_unpub[names(.)]) %>%
  #rename(plot = lut_plot) %>%
  arrange(plotnum, date_time) %>%
  distinct() %>%
  # check for any duplicates in data
  group_by(plotnum) %>%
  mutate(duptime = duplicated(date_time),
         dupdate = date_time %in% date_time[duptime])
summary(pikaocc_all_out) # true :(
# NWT-082 and NWT-082-WN conflict (from EDI)
# plot 88 has duplicate dates from unpublished data.. metadata shows two different sensors with overlapping dates
# I don't know which one is correct..spans Aug thru Nov 2018


ggplot(subset(pikaocc_all_out, plotnum == 88 & year(date_time) == 2018 & month(date_time) %in% 7:12)) +
  geom_point(aes(date_time, temperature), alpha = 0.5) +
  facet_wrap(~month(date_time), scales = "free_x")
# what would average vals look like?
ggplot(subset(pikaocc_all_out, plotnum == 88 & year(date_time) == 2018 & month(date_time) %in% 8:11)) +
  geom_point(aes(date_time, temperature), alpha = 0.5) +
  stat_summary(geom = "point", aes(date_time, temperature, group = date_time), fun = "mean", col = "red", alpha = 0.7) +
  facet_wrap(~month(date_time), scales = "free_x")
# that seems fine

# what about 082 and 082-WN (overlap also aug thru nov 2018)
ggplot(subset(edi_pikaocc, plotnum == 82 & year(date_time) == 2018 & month(date_time) %in% 8:11)) +
  geom_line(aes(date_time, temperature, col = dat_plot), alpha = 0.75, linewidth = 1) +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  theme_bw() +
  theme(legend.position = c(0.95, 0.95),
        legend.title = element_blank(),
        legend.justification = c("right", "top"))

# update: Chris says at both of these sites crew couldn't find old sensor so placed new ones. then found sensors later, hence overlap.
# maybe for purposes of analysis, average values at duplicate timestamps? or keep both separate but need to note sensors in unpublished pika data

# specifically:
#The two loggers at site PIKA-088 were placed in different years. U was recording good data from 2016-08-16 through 2018-11-07, when it failed, and NWT-06 was recording good data from 2018-08-09 to 2020-08-12. The reason they overlap from 2018-08-09 to 2018-11-07 is the techs could not find U to pull it when they placed NWT-06. Both should have been deployed in similar (standard) placements, at std depth, but they certainly weren't in exactly the same position, or the crew would have found U when placing NWT-06.

# pull out 82 and 88 to average each by date_time

nwt8288 <- subset(pikaocc_all_out, plotnum %in% c(82, 88)) %>%
  group_by(plotnum) %>%
  mutate(dupdate = date_time %in% date_time[duptime])


# -- 5. COMPILE WEST KNOLL AND GLV DATA -----
# read excel read in times with bogus date. time component of time col is still good
glvpika_temp_master <- mutate(glvpika_temp_master, 
                              clean_time = as.character(time),
                              clean_time = gsub("^[1-9].+-[1-9]{2} ", "", clean_time),
                              clean_date_time = as.POSIXct(paste(date, clean_time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  group_by(Datalogger) %>%
  mutate(dat_index = 1:length(temp_c)) %>%
  ungroup()

# join coords to glvpika, NA any alarms
glvpika_temp_clean <- left_join(glvpika_temp_master, subset(glvpika_meta, select = c(Datalogger:Northing))) %>%
  # drop 1 row with no data
  subset(!is.na(date)) %>%
  rename_all(casefold) %>%
  rename(years_insitu = year) %>%
  subset(select = c(datalogger, site:northing, date, clean_time, clean_date_time, dat_index, temp_c, temp_alarm)) %>%
  mutate(site = "Green Lakes Valley")

# some west knoll sites don't have a time so create index to at least aggregate by day
sort(unique(wkpika_temp_master$time_char)) # need to remove trailing .0, times are at least between 0 and 24 when present

# not all temps are celsius
wkpika_temp_clean <- group_by(wkpika_temp_master, Datalogger) %>%
  mutate(dat_index = 1:length(temp),
         date_check = date - lag(date)) %>%
  ungroup() # there is only 1 date that is wrong.. fix manually
unique(wkpika_temp_clean$date_check)
baddate <- which(wkpika_temp_clean$date < as.Date("1920-01-01"))
wkpika_temp_clean$date[baddate]
wkpika_temp_clean$date[baddate -1]; wkpika_temp_clean$date[baddate +1] # sandwiched between good dates
wkpika_temp_clean$date[baddate] <- wkpika_temp_clean$date[baddate-1]

# proceed with cleaning timestamps and standardizing temps to celsius
wkpika_temp_clean <- wkpika_temp_clean %>%
  mutate(clean_time = gsub("[.]0$", "", time_char),
         clean_time = ifelse(nchar(clean_time)==0, NA, clean_time),
         clean_date_time = paste(as.character(date), clean_time),
         clean_date_time = ifelse(grepl("NA", clean_date_time), NA, clean_date_time),
         temp_c = ifelse(grepl("F", temp_unit), (temp-32)/1.8, temp),
         # NA high values -- two are v clearly outliers/bad vals
         temp_c = ifelse(temp_c > 40, NA, temp_c)) %>%
  # edit logger name for joining w wkpika metadata
  rename(datfile = Datalogger) %>%
  mutate(datalogger = gsub("_|edited", "", datfile)) %>%
  subset(select = c(datalogger, datfile, date, clean_time, clean_date_time, dat_index, temp_c))

# prep wk metadata to join
wkpika_meta <- subset(glvpika_meta, grepl("West", Site), select = c(Datalogger:Northing)) %>% # these are in lat lon
  rename_all(casefold) %>%
  rename(years_insitu = year, lat = northing, lon = easting)

wkpika_temp_clean <- left_join(wkpika_temp_clean, wkpika_meta) %>%
  subset(select = c(datalogger, datfile, site:lat, date:temp_c))
# check for NAs to indicate bad join
summary(is.na(wkpika_temp_clean)) # some present
unique(with(wkpika_temp_clean, datalogger[is.na(site)])) 
# extras/no pair in metadata sheet: [1] "PM2013" "PO2013"
wkpika_meta$datalogger[!wkpika_meta$datalogger %in% wkpika_temp_clean$datalogger] # that's all accounted for
# i don't know what the two mystery sites are. will need to ask chris
wkpika_temp_clean$site <- "West Knoll"
# chris says PM sites are from Montana. Should not be in West Knoll, subset out
wkpika_temp_clean <- subset(wkpika_temp_clean, !is.na(years_insitu))


# -- WRITE OUT -----
# write to output data folder on local drive
datpath <- "/Users/scarlet/Documents/nwt_lter/temp_synth/output/dat/"

# write out data as they are so can work on sub-daily plots for meeting with chris 7/31/23. come back to spruce up later

# prepped demography data
saveRDS(pika_demoT_all, paste0(datpath, "pika_demog_edicg.rdata"))
write_csv()

# demography data: glv and west knoll
saveRDS(glvpika_temp_clean, paste0(datpath, "glvpika_demog.rdata"))
saveRDS(wkpika_temp_clean, paste0(datpath, "wkpika_demog.rdata"))

# prepped occupancy data
saveRDS(pikaocc_all_out, paste0(datpath, "pika_habocc_all.rdata"))
write_csv()

