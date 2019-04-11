#' ---
#' title: Extended summer PCA sensitivity analysis (preliminary)
#' author: CTW
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' ### Sensitivity of extended summer scores and input metrics to source precip/temp dataset
#' 
#' ![Extended Summer PC score over time, by data source](figs/PCAsensitivity_PC1overtime.png)
#' Fig 1. 
#' ![Difference between NWT-derived extended summer score and Jennings-derived extended summer score](figs/PCAsensitivity_deltaPC1_overtime.png)
#' Fig 2.
#' 
#' ![Extended summer year ordination](figs/PCAsensitivity_yrloadings.png)
#' Fig 3.
#' ![Extended summer variable loadings](figs/PCAsensitivity_sploadings.png)
#' Fig 4.
#' 
#' ![Input summer metrics, by data source](figs/PCAsensitivity_summermetrics.png)
#' Fig 5.
#' ![Difference in summer metric derived from NWT NSF data and Jennings et al. infilled data](figs/PCAsensitivity_metricdelta.png)
#' Fig 6.
#' 

#' ### Compare raw source data
#' Some visuals of the raw data to highlight the origin of the any differences above..
#' 
#' Looking at the raw data, precipitation in the two source datasets overlaps well except for a handful of differences in later years. The extended summer analysis strongly considers just summer months' (JJA) precipitation, which overlaps well in all years (not shown).
#' TMIN and TMAX tend to have warmer values in the Jennings infilled dataset. The NWT NSF data have a single max temp and min temp value per day, whereas the Jennings dataset provides hourly average air temperature. I took the max and min of those hourly values, per day, which gives the Jennings TMIN and TMAX values shown here.
#' Daily TMEAN used in the extended summer analysis is the average of daily TMIN and TMAX. 

#+ plot raw jennings vs nwt nsf, message = FALSE, warnings = FALSE, echo = FALSE, fig.width = 10, fig.height = 8
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE, fig.width = 10, fig.height = 8)

# precip and temp, spr, sum and fall months
options(stringsAsFactors = F)
library(ggplot2)
library(tidyr)
theme_set(theme_bw())

# read in raw daily precip and temp (prepped)
jennings_subset <- read.csv("output_data/jennings/hcn_jennings.csv")
nwtnsf_subset <- read.csv("output_data/suding/sensitivity_subset/hcn_suding_19902013.csv")

# plot tmin, tmax, by season, both datasets in one pane with transparency
# assign seasons
jennings_subset$source <- "Jennings et al. 2018" 
nwtnsf_subset$source <- "NWT NSF renewal"
master <- rbind(jennings_subset, nwtnsf_subset)
master$season[master$Month %in% 3:5] <- "Spring"
master$season[master$Month %in% 6:8] <- "Summer"
master$season[master$Month %in% 9:11] <- "Fall"
master$season[master$Month %in% c(1,2,12)] <- "Winter"
master$date <- as.Date(master$date)
master$jday <- lubridate::yday(master$date)
master <- gather(master, met, val, TMIN:PCP)

ggplot(master, aes(date, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Daily total precipitation, TMIN, and TMAX, 1990-2013, by dataset source") +
  facet_grid(met~., scales = "free_y")

#' Looking at temperature by season, the biggest differences are in the summer months (JJA): 
 
#+ summer temperature plots
ggplot(subset(master, season == "Summer" & met == "TMAX"), aes(jday, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Summer (Jun-Aug) TMAX, by source and year") +
  facet_wrap(~Year)

ggplot(subset(master, season == "Summer" & met == "TMIN"), aes(jday, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Summer (Jun-Aug) TMIN, by source and year") +
  facet_wrap(~Year)

#' Spring and fall temperatures in both datasets overlap better compared to the summer months. 
#' For metrics used in the extended summer PCA, spring temperature values are used to calculate warming temperature thresholds ("fivedayrunning5C" and "fivedayrunnings12C").
#' Fall temperature values are used to calculate 3-day running minimum temperature thresholds ("GSLthreedayneg3C").

#+ spring and fall plots
ggplot(subset(master, season == "Spring" & met == "TMAX"), aes(jday, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Spring (Mar-May) TMAX, by source and year") +
  facet_wrap(~Year)

ggplot(subset(master, season == "Spring" & met == "TMIN"), aes(jday, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Spring (Mar-May) TMIN, by source and year") +
  facet_wrap(~Year)


ggplot(subset(master, season == "Fall" & met == "TMAX"), aes(jday, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Fall (Sep-Nov) TMIN, by source and year") +
  facet_wrap(~Year)

ggplot(subset(master, season == "Fall" & met == "TMIN"), aes(jday, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Fall (Sep-Nov) TMIN, by source and year") +
  facet_wrap(~Year)

