#' title: Compare precip and temp from chart, Emily's data, and Jennings et al.
#' author: CTW
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' 
#' **Goal**: Figure out how best to infill precip and temp for years 2015, 2016, 2017

#+ script setup, echo = FALSE, message = FALSE, warning = FALSE
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(tidyverse)
library(lubridate)

# read in NWT climate dataset used in NSF proposal
# > read in from EDI data portal
sdl_charttemp <- read_csv("https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-nwt.413.10&entityid=afc48b6fab15649c9f91a9367debd2e0",
                       trim_ws=TRUE, na = c("", "NA", ".", "NaN")) %>%
  mutate(source="chart")

sdl_loggerdat <- read_csv("http://niwot.colorado.edu/data_csvs/sdlcr23x-cr1000.daily.ml.data.csv",
                             na = c("", "NA", "Nan"))
# keep only temp data from logger for this
sdl_loggerdat <- sdl_loggerdat[,1:12] # should keep "LTER_site" through "flag_airtemp_avg 

# read in precip chart data from EDI
# > read in from EDI data portal
sdl_chartpcp <- read_csv("https://portal.lternet.edu/nis/dataviewer?packageid=knb-lter-nwt.416.8&entityid=c1c73287f2e10039c0d89dba8fd44993",
                         trim_ws = TRUE,
                         na = c("", "NA", "NaN")) %>%
  mutate(source = "chart")

# read in C1 and D1 to compare for quick infilling
# all chart data, all from NWT data portal
c1_charttemp <- read_csv("http://niwot.colorado.edu/data_csvs/c-1tdayv.ml.data.csv",
                   na = c("", "NA", "NaN"))
c1_charttemp$source <- "chart"

c1_chartpcp <- read_csv("http://niwot.colorado.edu/data_csvs/c-1pdayv.ml.data.csv",
                     na = c("", "NA", "NaN"))
c1_chartpcp$source <- "chart"

d1_charttemp <- read_csv("http://niwot.colorado.edu/data_csvs/d-1tdayv.ml.data.csv",
                   na = c("", "NA", "NaN"))
d1_charttemp$source <- "chart"

d1_chartpcp <- read_csv("http://niwot.colorado.edu/data_csvs/d-1pdayv.ml.data.csv",
                     na = c("", "NA", "NaN"))
d1_chartpcp$source <- "chart"

# temp and precip data EF used for NSF proposal climate dataset
temp_pcp_ef <- read_csv("~/Dropbox/NWT_data/Saddle_precip_temp_formoisturedeficit.csv")
temp_pcp_ef$source <- "NSF proposal data"

# Keith Jennings et al. infilled *hourly* data
Jennings_infill <- read_csv("http://niwot.colorado.edu/data_csvs/infillclimate_c1_d1_sdl.hourly.kj.data.csv")
Jennings_infill$source <-  "Jennings et al."

# summarise Jennings et al. data to daily totals (ppt) or averages (temp)
Jennings_summarized <- Jennings_infill %>%
  group_by(LTER_site, local_site, year, date,jday, source) %>%
  summarise(ppt_tot = sum(ppt_tot),
            airtemp_max = mean(airtemp_avg),
            airtemp_min = min(airtemp_avg),
            airtemp_avg = mean(airtemp_avg))

# remove hourly infilled dataset bc 65 MB and eats up memory
#rm(Jennings_infill)

#' ### Time ranges of each dataset..
#' 

#+ compare raw datasets
# combine datasets first

# what are structures of each dataset?
glimpse(sdl_chartpcp)
glimpse(c1_chartpcp)
glimpse(d1_chartpcp)
glimpse(sdl_charttemp)
glimpse(c1_charttemp)
glimpse(d1_charttemp)
glimpse(sdl_loggerdat) # only air temperature values kept; wind speed, rel. humid, soil moisture, etc. dropped (not needed for this)
glimpse(Jennings_summarized)
glimpse(temp_pcp_ef)

# correct d1 chart precip ppt_tot name to match others
names(d1_chartpcp)[4] <- "ppt_tot"

pcp_all <- rbind(d1_chartpcp, sdl_chartpcp, c1_chartpcp)
Jennings_summarized$qdays <- 1 #reflects daily sum
Jennings_summarized$flag_ppt_tot <- NA
pcp_all <-  rbind(pcp_all,
                  Jennings_summarized[c("LTER_site", "local_site", "date", "ppt_tot", "qdays", "flag_ppt_tot", "source")])
# give Emily's dataset the same fields as other chart data
temp_pcp_ef <- temp_pcp_ef %>%
  mutate(qdays = 1,
         flag_ppt_tot = NA,
         date = as.Date(paste(Year,Month,Day, sep = "-")),
         LTER_site = "NWT",
         local_site = "sdl") %>%
  dplyr::rename(ppt_tot = PCP)

pcp_all <- rbind(pcp_all, 
                 temp_pcp_ef[c("LTER_site", "local_site", "date", "ppt_tot", "qdays", "flag_ppt_tot", "source")])

# combine temperature data
temp_all <- rbind(d1_charttemp, sdl_charttemp, c1_charttemp)

# give Jennings dataset same fields as chart data
Jennings_summarized$flag_airtemp_max <- NA # lose flag data when summarise hourly values
Jennings_summarized$flag_airtemp_min <- NA # lose flag data when summarise hourly values

# add in Jennings et al. summarized daily temp
temp_all <- rbind(temp_all,
                  Jennings_summarized[c("LTER_site", "local_site", "date", 
                                        "airtemp_max", "flag_airtemp_max", "airtemp_min", "flag_airtemp_min",
                                        "airtemp_avg", "source")])
# add in Emily's dataset
# give Emily's dataset the same fields as other chart data
temp_pcp_ef <- temp_pcp_ef %>%
  mutate(date = as.Date(paste(Year,Month,Day, sep = "-")),
         LTER_site = "NWT",
         local_site = "sdl",
         flag_airtemp_max = NA,
         flag_airtemp_min = NA,
         airtemp_avg = NA) %>%
  dplyr::rename(airtemp_max = TMAX,
                airtemp_min = TMIN)

# add in Emily's data
temp_all <- rbind(temp_all, 
                  temp_pcp_ef[c("LTER_site", "local_site", "date", "airtemp_max", "flag_airtemp_max",
                                "airtemp_min", "flag_airtemp_min", "airtemp_avg", "source")])

# -----------------------
# plot to compare!

# raw data - precip
ggplot(pcp_all) +
  geom_point(aes(date, ppt_tot, fill = source), pch=21, alpha = 0.6) +
  scale_fill_brewer(palette ="Set2") +
  theme_bw() +
  facet_grid(source~local_site)

# data subsetted to period of NSF proposal and corrected for blowing snow
# snow correction: if Jun-Aug, do nothing; otherwise multiple ppt * 0.39
pcp_all %>%
  mutate(yr = year(date),
         mon = month(date),
         ppt_tot = ifelse(source == "chart" & 
                            #local_site == "sdl" & 
                            mon %in% c(1:5,9:12), ppt_tot*0.39, ppt_tot)) %>%
  filter(yr >= 1982 & local_site == "sdl") %>%
  ggplot() +
  geom_point(aes(date, ppt_tot, fill = source), pch=21, alpha = 0.6) +
  scale_fill_brewer(palette ="Set2") +
  theme_bw() +
  facet_grid(source~local_site)

pcp_all %>%
  mutate(yr = year(date),
         mon = month(date),
         ppt_tot = ifelse(source == "chart" & 
                            local_site == "sdl" & 
                            mon %in% c(1:5,9:12), ppt_tot*0.39, ppt_tot)) %>%
  filter(yr >= 1982 & local_site != "sdl") %>%
  ggplot() +
  geom_point(aes(date, ppt_tot, fill = source), pch=21, alpha = 0.6) +
  scale_fill_brewer(palette ="Set2") +
  theme_bw() +
  facet_grid(source~local_site)

pcp_all %>%
  mutate(yr = year(date),
         mon = month(date),
         ppt_tot = ifelse(source == "chart" & 
                            local_site == "sdl" & 
                            mon %in% c(1:5,9:12), ppt_tot*0.39, ppt_tot)) %>%
  filter(yr %in% 2007:2008 & local_site != "sdl") %>%
  ggplot() +
  geom_point(aes(date, ppt_tot, col = source), alpha = 0.6) +
  scale_color_brewer(palette ="Set2") +
  theme_bw() +
  facet_grid(.~local_site)

pcp_all <- pcp_all %>%
  mutate(yr = year(date),
         mon = month(date),
         ppt_tot = ifelse(source == "chart" & 
                            local_site == "sdl" & 
                            mon %in% c(1:5,9:12), ppt_tot*0.39, ppt_tot))

ggplot() +
  geom_point(data = subset(pcp_all,yr %in% 2012:2013 & local_site == "sdl" & source == "chart"),
             aes(date, ppt_tot), col= "dodgerblue", size=2, alpha =0.6) +
  geom_point(data = subset(pcp_all,yr %in% 2012:2013 & local_site == "sdl" & source == "Jennings et al."),
             aes(date, ppt_tot), col = "seagreen", size=1, alpha = 0.6) +
  #geom_point(data = subset(pcp_all,yr %in% 2012 & local_site == "sdl" & source == "NSF proposal data"),
  #           aes(date, ppt_tot, col = source), alpha = 0.6) +
  ggtitle("chart data (blue) vs. Jennings et al. infill (green)") +
  theme_bw()

ggplot() +
  #geom_point(data = subset(pcp_all,yr %in% 2007 & local_site == "sdl" & source == "Jennings et al."),
  #           aes(date, ppt_tot, col = source), alpha = 0.6) +
  geom_point(data = subset(pcp_all,yr %in% 2012:2013 & local_site == "sdl" & source == "chart"),
             aes(date, ppt_tot), col = "dodgerblue", size=2, alpha = 0.6) +
  geom_point(data = subset(pcp_all,yr %in% 2012:2013 & local_site == "sdl" & source == "NSF proposal data"),
             aes(date, ppt_tot), col = "goldenrod", size =1, alpha = 0.6) +
  ggtitle("chart data (blue) vs. NSF proposal data (yellow)") +
  theme_bw()

ggplot() +
  geom_point(data = subset(pcp_all,yr %in% 2012:2013 & local_site == "sdl" & source == "Jennings et al."),
             aes(date, ppt_tot), col="seagreen", size = 2, alpha = 0.6) +
  #geom_point(data = subset(pcp_all,yr %in% 2012:2013 & local_site == "sdl" & source == "chart"),
  #           aes(date, ppt_tot), col = "dodgerblue", size=2, alpha = 0.6) +
  geom_point(data = subset(pcp_all,yr %in% 2012:2013 & local_site == "sdl" & source == "NSF proposal data"),
             aes(date, ppt_tot), col = "goldenrod", size =1, alpha = 0.6) +
  ggtitle("Jennings et al. infill data (green) vs. NSF proposal data (yellow)") +
  theme_bw()
