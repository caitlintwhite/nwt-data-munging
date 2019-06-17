#' ---
#' title: Extended summer PCA sensitivity analysis (preliminary)
#' author: CTW
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' ### Sensitivity of extended summer scores and input metrics to source precip/temp dataset
#' 
#' Regardless of input dataset used, PC1 explains 50% of the variation in the summer climate data.
#' 
#' 
#' Fig 1. 
#' ![Extended Summer PC score over time, by data source](figs/PCAsensitivity_PC1overtime.png)
#' Fig 2.
#' ![Difference between NWT-derived extended summer score and Jennings-derived extended summer score](figs/PCAsensitivity_deltaPC1_overtime.png)
#' 
#' Fig 3.
#' ![Extended summer year ordination](figs/PCAsensitivity_yrloadings.png)
#' Fig 4.
#' ![Extended summer variable loadings](figs/PCAsensitivity_sploadings.png)
#' 
#' Fig 5.
#' ![Input summer metrics, by data source](figs/PCAsensitivity_summermetrics.png)
#' Fig 6.
#' ![Difference in summer metric derived from NWT NSF data and Jennings et al. infilled data](figs/PCAsensitivity_metricdelta.png)
#' 
#' 

#' ### Compare PCA input data using Saddle chart vs. Saddle logger temperature
#' 
#' The following figure compares Extended Summer PCA input variables derived from different saddle temperature datasets.
#' The NWT renewal panel shows data used in the NWT NSF renewal (1982-2014), with 2015-2017 saddle chart-derived values appeneded and 2018 saddle-chart projected from cr1000 (the Saddle chart temperature data was discontinued after 2017-12-31.)
#' The CR1000 + CR1000 projected panel shows values derived from the CR1000 dat logger (2013-ongoing) + CR1000 projected backwards to 1982 from the saddle chart.
#' The last two panels show PCA input variables derived from the saddle logger datasets (1986-ongoing), with 1982-1986 logger-projected using two different loggers: the current logger (cr1000) and the logger active closest-in-time the 1980s saddle chart data (cr21x).
#' The grey shaded area denotes the period logger temperature was projected backwards from the saddle chart. The dark blue vertical lines denote years when new saddle data loggers were launched (we estimate the sensor used in the logger was the same since 1995/1996 regardless of logger changes).
#' Although shown, values for 2015 and 2016 in the NWT renewal panel should be ignored because temperature data from Mar 2015-Aug 2016 in the saddle chart are bad (update to dataset not done yet).

#' Because backwards-projected logger values using the saddle chart and cr21x logger are most similar to the values using the saddle chart, I recommend using the cr21x logger to backfill if we decide to use the Saddle logger data.
#' 

#+ prep and plot PCA input data, message = FALSE, warnings = FALSE, echo = FALSE, fig.width = 10, fig.height = 7

# precip and temp, spr, sum and fall months
library(tidyverse)
options(stringsAsFactors = F)
theme_set(theme_bw())
          
nsf_input <- read.csv("output_data/suding/allyrs/NWT_sumallPCclimate_19822018.csv") %>%
  mutate(source = "NWT renewal")
cr21x_input <- read.csv("output_data/ctw/crall21x/NWT_sumallPCclimate_19822018.csv") %>%
  mutate(source = "sdl loggers, cr 21x backfill")
cr1000_input <- read.csv("output_data/ctw/crall1000/NWT_sumallPCclimate_19822018.csv") %>%
  mutate(source = "sdl loggers, cr 1000 backfill")
cr1000sdl_input <- read.csv("output_data/ctw/cr1000_sensitivity/NWT_sumallPCclimate_19822018.csv") %>%
  mutate(source = "cr 1000 + cr1000 projected sdl chart")
# stack all PCA input dataset versions
alltog <- rbind(nsf_input, cr21x_input, cr1000_input, cr1000sdl_input) %>%
  dplyr::select(source, eco_year, sumallPC1:ncol(.)) %>%
  #gather all input metrics to panel plot
  gather(met, val, sumallPC1:ncol(.))
alltog$source <- factor(alltog$source, levels = c("NWT renewal", "cr 1000 + cr1000 projected sdl chart",
                                                  "sdl loggers, cr 1000 backfill", "sdl loggers, cr 21x backfill"))
ggplot(subset(alltog, !grepl("iceoff|precip|PC2", met)), aes(eco_year, val, group = met, col = source)) +
  geom_hline(data = subset(alltog, met == "sumallPC1"), aes(yintercept = 0)) +
  geom_ribbon(data = subset(alltog, !grepl("iceoff|precip|PC2", met) & eco_year < 1987), aes(ymin = -Inf, ymax = Inf, group=factor(met)), fill = "grey80", col = "grey80", alpha = 0.5) +
  geom_vline(aes(xintercept= 1986), col = "dodgerblue4", lwd = 1.5, alpha = 0.5) +
  geom_vline(aes(xintercept= 2006), col = "dodgerblue4", lwd = 1.5, alpha = 0.5) +
  geom_vline(aes(xintercept= 2012), col = "dodgerblue4", lwd = 1.5, alpha = 0.5) +
  geom_line() +
  geom_point() +
  #scale_color_manual(name = "Infill method", values = c("darkorange4", "blue3", "blueviolet")) +
  labs(y = "Input value", x = "Year") +
  facet_grid(met~source, scales = "free_y") +
  theme(strip.text.y = element_text(size = 6),
        legend.position = "none")




#' ### Compare raw source data
#' Some visuals of the raw data to highlight the origin of the any differences above..
#' 
#' Looking at the raw data, precipitation in the two source datasets overlaps well except for a handful of differences in later years. The extended summer analysis strongly considers just summer months' (JJA) precipitation, which overlaps well in all years (not shown).
#' TMIN and TMAX tend to have warmer values in the Jennings infilled dataset. The NWT NSF data have a single max temp and min temp value per day, whereas the Jennings dataset provides hourly average air temperature. I took the max and min of those hourly values, per day, which gives the Jennings TMIN and TMAX values shown here.
#' Daily TMEAN used in the extended summer analysis is the average of daily TMIN and TMAX. 

#+ plot raw jennings vs nwt nsf, message = FALSE, warnings = FALSE, echo = FALSE, fig.width = 10, fig.height = 8
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE, fig.width = 10, fig.height = 8)

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
  ggtitle("Fig 7. Daily total precipitation, TMIN, and TMAX, 1990-2013, by dataset source") +
  facet_grid(met~., scales = "free_y")

#' Looking at temperature by season, the biggest differences are in the summer months (JJA): 
 
#+ summer temperature plots
ggplot(subset(master, season == "Summer" & met == "TMAX"), aes(jday, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Fig 8. Summer (Jun-Aug) TMAX, by source and year") +
  facet_wrap(~Year)

ggplot(subset(master, season == "Summer" & met == "TMIN"), aes(jday, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Fig 9. Summer (Jun-Aug) TMIN, by source and year") +
  facet_wrap(~Year)

#' Spring and fall temperatures in both datasets overlap better compared to the summer months. 
#' For metrics used in the extended summer PCA, spring temperature values are used to calculate warming temperature thresholds ("fivedayrunning5C" and "fivedayrunnings12C").
#' Fall temperature values are used to calculate 3-day running minimum temperature thresholds ("GSLthreedayneg3C").

#+ spring and fall plots
ggplot(subset(master, season == "Spring" & met == "TMAX"), aes(jday, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Fig 10. Spring (Mar-May) TMAX, by source and year") +
  facet_wrap(~Year)

ggplot(subset(master, season == "Spring" & met == "TMIN"), aes(jday, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Fig 11. Spring (Mar-May) TMIN, by source and year") +
  facet_wrap(~Year)


ggplot(subset(master, season == "Fall" & met == "TMAX"), aes(jday, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Fig 12. Fall (Sep-Nov) TMAX, by source and year") +
  facet_wrap(~Year)

ggplot(subset(master, season == "Fall" & met == "TMIN"), aes(jday, val, col = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("chocolate2", "dodgerblue2")) +
  ggtitle("Fig 13. Fall (Sep-Nov) TMIN, by source and year") +
  facet_wrap(~Year)


