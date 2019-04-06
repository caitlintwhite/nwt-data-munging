# Temporal trends in discharge for GL4 and Saddle outlets
# caitlin.t.white@colorado.edu


# From WW:
# > In Nel's email (pasted below) and our conversation we spoke about temporal patterns in stream yield, flow initiation, rates, and timing (e.g. late season flows).  
# > Some of this has been published on, but it's likely worth revisiting with more current data.  
# > Also, calibration for the pressure gauges changes with time, making QC important for these data. 
## CTW note: CTW has no idea how to adjust discharge data via QC.. will require someone with expertise in hydrology .. but can get started for now

# ----- SETUP -----
# load libraries
library(tidyverse)
library(lubridate)
library(cowplot)


# read in datasets from EDI
# GL4 streamflow (knb-lter-nwt.105.9)
gl4_q <- read_csv("http://pasta.lternet.edu/package/data/eml/knb-lter-nwt/105/9/3f04604569c43a28142630c784abd99d",
                  na = c("NA", "NaN", " "))

# Saddle streamflow (knb-lter-nwt.74.1)
sdl_q <- read_csv("http://pasta.lternet.edu/package/data/eml/knb-lter-nwt/74/1/b85e6c408c41c80ba59a43a2b02c2d60",
                  na = c("NA", "NaN", " "))

# Martinelli streamflow (knb-lter-nwt.111.9)
mart_q <- read_csv("https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/111/9/56426af5bf09fcdd704fb8b6cc46d0fe",
                   na = c("NA", "NaN", " "))
  
# Albion streamflow (knb-lter-nwt.102.13)
alb_q <- read_csv("https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/102/13/2e23f7287a1c6b929e1e32b8a70e2212",
                  na = c("NA", "NaN", " "))


# read in extended summer PC scores
nwt_climdat <- read_csv("/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/climate_update/NWT_sumallPCclimate_19822017.csv")

# ----- DATA EXPLORE -----
## NOTE! must plot on the water year (Oct 1 - Sep 30)
# what is temporal coverage of gl4?
range(gl4_q$date)
sort(unique(month(gl4_q$date)))

gl4_q <- gl4_q %>%
  mutate(yr = as.factor(year(`date`)), mon = month(`date`))

# plot raw data to see any gaps
gl4_q %>%
  mutate(doy = yday(`date`)) %>%
  ggplot() +
  geom_path(aes(doy, discharge), alpha = 0.5) +
  facet_wrap(~year(`date`)) # date coverage per year not the same
# days covered by year
with(gl4_q, sapply(split(yday(`date`), year(`date`)), function(x) range(x)))

# what does cumulative discharge look like?
gl4_q %>%
  mutate(doy = yday(`date`)) %>%
  group_by(year(`date`)) %>%
  mutate(cum_q = cumsum(discharge)) %>%
  ggplot() +
  geom_path(aes(doy, cum_q, group = year(`date`)), alpha = 0.5) 
  #facet_wrap(~year(`date`)) # 2015 seems odd, flow comes in winter and total q very high..

ggplot(gl4_q) +
  geom_boxplot(aes(as.factor(year(`date`)), discharge))

# subset roughly May 1 to Sep 30
gl4_q %>%
  mutate(doy = yday(`date`),
         WY = ifelse(month(`date`) %in% 10:12, year(`date`)+1, year(`date`))) %>%
  filter(doy %in% 120:273 & WY >1982) %>%
  #fill NAs with 0s so cum sum will run on all years
  mutate(filled_q = ifelse(is.na(discharge), 0, discharge),
         na_q = is.na(discharge)) %>%
  group_by(WY) %>%
  mutate(cum_q = cumsum(filled_q)) %>%
  left_join(nwt_climdat, by = c("WY" = "eco_year")) %>%
  ggplot() +
  geom_point(aes(doy, cum_q, group = WY, col = sumallPC1), alpha = 0.5) +
  scale_color_distiller(name = "Extended\nsummer PC", palette = "RdBu")
  
# subset roughly May 1 to Sep 30
gl4_cumsum <- gl4_q %>%
  mutate(doy = yday(`date`),
         WY = ifelse(month(`date`) %in% 10:12, year(`date`)+1, year(`date`))) %>%
  filter(doy %in% 120:273 & WY >1982) %>%
  #fill NAs with 0s so cum sum will run on all years
  mutate(filled_q = ifelse(is.na(discharge), 0, discharge),
         na_q = is.na(discharge)) %>%
  group_by(WY) %>%
  mutate(cum_q = cumsum(filled_q),
         percentile_q = cum_q/max(cum_q)) %>%
  left_join(nwt_climdat, by = c("WY" = "eco_year"))

gl4_PC1_q <- ggplot(gl4_cumsum) +
  geom_hline(aes(yintercept = 0.5), col = "grey50", lty = 3) +
  geom_point(data = subset(gl4_cumsum, na_q==TRUE), aes(doy, percentile_q, group = WY), col = "black", size = 2) +
  geom_path(aes(doy, percentile_q, group = WY, col = sumallPC1), lwd = 1.5, alpha = 0.5) +
  scale_color_distiller(name = "Extended\nsummer PC", palette = "RdBu") +
  labs(y = "Q percentile", subtitle = "Green Lake 4 outlet, 1982-2017", x = NULL) +
  theme_bw()

# peak flow and date of peak flow
gl4_q %>%
  mutate(doy = yday(`date`),
         WY = ifelse(month(`date`) %in% 10:12, year(`date`)+1, year(`date`))) %>%
  filter(WY != 2013) %>%
  group_by(WY) %>%
  filter(discharge == max(discharge)) %>%
  ggplot(aes(WY, doy)) +
  geom_point() +
  geom_smooth()
  
gl4_maxQ_trend <- gl4_q %>%
  mutate(doy = yday(`date`),
         WY = ifelse(month(`date`) %in% 10:12, year(`date`)+1, year(`date`))) %>%
  group_by(WY) %>%
  filter(discharge == max(discharge)) %>%
  left_join(nwt_climdat, by = c("WY" = "eco_year")) %>%
  #remove 2013 outlier (flood year)
  filter(WY <2010) %>%
  ggplot(aes(WY, doy)) +
  #geom_vline(aes(xintercept = 0), col = "grey50", lty = 3) +
  geom_point() +
  #geom_label(aes(x =1.1, y =200, label = "p = 0.068\nR^2 = 0.082"), size = 3.5) +
  labs(x = NULL, y = "Day of year",
       subtitle = "Green Lake 4 outlet, 1982-2017 (2013 removed)") +
  #scale_y_date(breaks = "10 days", date_labels = "%d-%m") +
  geom_smooth(method = "lm") +
  theme_bw()

# is trend significant?
max_annual_q <- gl4_q %>%
  mutate(doy = yday(`date`),
         WY = ifelse(month(`date`) %in% 10:12, year(`date`)+1, year(`date`))) %>%
  group_by(WY) %>%
  filter(discharge == max(discharge)) %>%
  left_join(nwt_climdat, by = c("WY" = "eco_year")) %>%
  #remove 2013 outlier (flood year)
  filter(WY <2010)

summary(lm(doy ~  sumallPC1, data = max_annual_q))

boxplot(max_annual_q$doy ~ max_annual_q$sumallPC1 > 0)


#faux specific discharge (don't know area so sum all because all should have common denominator)
gl4_late <- filter(gl4_q, month(`date`) %in% 9) %>%
  mutate(yr = year(`date`)) %>%
  group_by(yr) %>%
  summarise(totalQ = sum(discharge, na.rm=TRUE),
            spQ = ((totalQ/(43416/.0207))*1000),
            nobs = length(discharge))

ggplot(subset(gl4_late, yr<2010 & nobs == 30), aes(yr, spQ)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #scale_x_continuous(breaks = seq(1980,2015, 5), minor_breaks = seq(1980,2017,1)) +
  #scale_y_continuous(limits=c(0,360), breaks = seq(0,350,50)) +
  theme_bw()

# early season
filter(gl4_q, month(`date`) %in% 6) %>%
  mutate(yr = year(`date`)) %>%
  group_by(yr) %>%
  summarise(totalQ = sum(discharge, na.rm=TRUE)) %>%
  ggplot(aes(yr, totalQ)) +
  geom_point() +
  geom_smooth(method = "lm")

## ----- SADDLE Q ------
# repeat above but for saddle
# what is temporal coverage of saddle outlet?
range(sdl_q$date)
sort(unique(month(sdl_q$date)))

# plot raw data to see any gaps
sdl_q %>%
  mutate(doy = yday(`date`)) %>%
  ggplot() +
  geom_path(aes(doy, discharge), alpha = 0.5) +
  facet_wrap(~year(`date`)) # date coverage per year not the same
# days covered by year
with(sdl_q, sapply(split(yday(`date`), year(`date`)), function(x) range(x)))

# what does cumulative discharge look like?
sdl_q %>%
  mutate(doy = yday(`date`)) %>%
  group_by(year(`date`)) %>%
  mutate(cum_q = cumsum(discharge)) %>%
  ggplot() +
  geom_path(aes(doy, cum_q, group = year(`date`)), alpha = 0.5) 
#facet_wrap(~year(`date`)) # 2015 seems odd, flow comes in winter and total q very high..

ggplot(sdl_q) +
  geom_boxplot(aes(as.factor(year(`date`)), discharge))

# subset roughly May 1 to Sep 30
sdl_cumsum <- sdl_q %>%
  mutate(doy = yday(`date`)) %>%
  filter(doy %in% 120:273) %>%
  #fill NAs with 0 so cumulative sum will run (just to see how all years plot out)
  mutate(discharge_fill = ifelse(is.na(discharge), 0, discharge)) %>%
  group_by(year(`date`)) %>%
  mutate(cum_q = cumsum(discharge_fill),
         percentile_q = cum_q/max(cum_q),
         q_na = is.na(discharge))

sdl_PC1_q <- sdl_cumsum %>%
  filter(month(`date`) != 10) %>%
  mutate(WY = year(`date`)) %>% # there are no Oct-Dec months in this dataset
  left_join(nwt_climdat, by = c("WY" = "eco_year")) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0.5), col = "grey50", lty = 3) +
  geom_point(data = subset(sdl_cumsum, q_na == TRUE), aes(doy, percentile_q), size = 3) +
  geom_path(aes(x=doy, percentile_q, group = year(`date`), col = sumallPC1), 
             alpha = 0.5, lwd = 1.5) +
  scale_color_distiller(name = "Extended\nsummer PC", palette = "RdBu") +
  labs(title = "Proportion of total streamflow May 1 - Sep 30 (all years available)", 
       subtitle = "Saddle outlet, 1999-2017",
       y = "Q percentile", x = NULL) +
  #scale_x_continuous(labels = strftime(`date`,format="%m-%d")) +
  theme_bw()

# peak flow and date of peak flow
sdl_q %>%
  mutate(doy = yday(`date`),
         WY = ifelse(month(`date`) %in% 10:12, year(`date`)+1, year(`date`))) %>%
  group_by(WY) %>%
  filter(discharge == max(discharge, na.rm=TRUE)) %>%
  ggplot(aes(WY, doy)) +
  geom_point() +
  geom_smooth(method = "lm")

sdl_maxQ_trend <- sdl_q %>%
  mutate(doy = yday(`date`),
         WY = ifelse(month(`date`) %in% 10:12, year(`date`)+1, year(`date`))) %>%
  group_by(WY) %>%
  filter(discharge == max(discharge, na.rm=TRUE)) %>%
  left_join(nwt_climdat, by = c("WY" = "eco_year")) %>%
  #remove 2013 outlier (flood year)
  #filter(WY != 2013) %>%
  ggplot(aes(sumallPC1, doy)) +
  geom_vline(aes(xintercept = 0), col = "grey50", lty = 3) +
  geom_point() +
  geom_label(aes(x =1.1, y =175, label = "p = 0.032\nR^2 = 0.198"), size = 3.5) +
  labs(x = NULL, y = "Day of year",
       title = "Day of peak discharge advances with extended summer (all years available)",
       subtitle = "Saddle outlet, 1999-2017 (2013 included)") +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = range(nwt_climdat$sumallPC1), breaks = seq(-1,1.5,0.5)) +
  theme_bw()

# is trend significant?
sdl_max_annual_q <- sdl_q %>%
  mutate(doy = yday(`date`),
         WY = ifelse(month(`date`) %in% 10:12, year(`date`)+1, year(`date`))) %>%
  group_by(WY) %>%
  filter(discharge == max(discharge, na.rm=TRUE)) %>%
  left_join(nwt_climdat, by = c("WY" = "eco_year"))
  #remove 2013 outlier (flood year)
  #filter(WY != 2013)

summary(lm(doy ~  sumallPC1, data = sdl_max_annual_q))
summary(lm(doy ~  year(`date`), data = sdl_max_annual_q))

boxplot(sdl_max_annual_q$doy ~ sdl_max_annual_q$sumallPC1 > 0)



## ---- ALBION ----
# repeat above but for albion
# what is temporal coverage of saddle outlet?
range(alb_q$date)
sort(unique(month(alb_q$date)))

# plot raw data to see any gaps
alb_q %>%
  mutate(doy = yday(`date`)) %>%
  ggplot() +
  geom_path(aes(doy, discharge), alpha = 0.5) +
  facet_wrap(~year(`date`)) # date coverage per year not the same
# days covered by year
with(alb_q, sapply(split(yday(`date`), year(`date`)), function(x) range(x)))

# what does cumulative discharge look like?
alb_q %>%
  mutate(doy = yday(`date`)) %>%
  group_by(year(`date`)) %>%
  mutate(cum_q = cumsum(discharge)) %>%
  ggplot() +
  geom_path(aes(doy, cum_q, group = year(`date`)), alpha = 0.5) 
#facet_wrap(~year(`date`)) # 2015 seems odd, flow comes in winter and total q very high..

ggplot(alb_q) +
  geom_boxplot(aes(as.factor(year(`date`)), discharge))

# subset roughly May 1 to Sep 30
alb_cumsum <- alb_q %>%
  mutate(doy = yday(`date`)) %>%
  filter(doy %in% 120:273) %>%
  #fill NAs with 0 so cumulative sum will run (just to see how all years plot out)
  mutate(discharge_fill = ifelse(is.na(discharge), 0, discharge)) %>%
  group_by(year(`date`)) %>%
  mutate(cum_q = cumsum(discharge_fill),
         percentile_q = cum_q/max(cum_q),
         q_na = is.na(discharge))

alb_PC1_q <- alb_cumsum %>%
  filter(month(`date`) != 10 & year(`date`) >1982) %>%
  mutate(WY = year(`date`)) %>% # there are no Oct-Dec months in this dataset
  left_join(nwt_climdat, by = c("WY" = "eco_year")) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0.5), col = "grey50", lty = 3) +
  geom_point(data = subset(alb_cumsum, q_na == TRUE), aes(doy, percentile_q), size = 2) +
  geom_path(aes(x=doy, percentile_q, group = year(`date`), col = sumallPC1), 
            alpha = 0.5, lwd = 1.5) +
  scale_color_distiller(name = "Extended\nsummer PC", palette = "RdBu") +
  labs(subtitle = "Albion outlet, 1982-2017",
       y = "Q percentile", x = "Day of year") +
  #scale_x_continuous(labels = strftime(`date`,format="%m-%d")) +
  theme_bw()


# ----- MARTINELLI Q ------
# repeat above but for albion
# what is temporal coverage of saddle outlet?
range(mart_q$date)
sort(unique(month(mart_q$date)))

# plot raw data to see any gaps
mart_q %>%
  mutate(doy = yday(`date`)) %>%
  ggplot() +
  geom_path(aes(doy, discharge), alpha = 0.5) +
  facet_wrap(~year(`date`)) # date coverage per year not the same
# days covered by year
with(mart_q, sapply(split(yday(`date`), year(`date`)), function(x) range(x)))

# what does cumulative discharge look like?
mart_q %>%
  mutate(doy = yday(`date`)) %>%
  group_by(year(`date`)) %>%
  mutate(cum_q = cumsum(discharge)) %>%
  ggplot() +
  geom_path(aes(doy, cum_q, group = year(`date`)), alpha = 0.5) 
#facet_wrap(~year(`date`)) # 2015 seems odd, flow comes in winter and total q very high..

ggplot(mart_q) +
  geom_boxplot(aes(as.factor(year(`date`)), discharge))

# subset roughly May 1 to Sep 30
mart_cumsum <- mart_q %>%
  mutate(doy = yday(`date`)) %>%
  filter(doy %in% 120:273) %>%
  #fill NAs with 0 so cumulative sum will run (just to see how all years plot out)
  mutate(discharge_fill = ifelse(is.na(discharge), 0, discharge)) %>%
  group_by(year(`date`)) %>%
  mutate(cum_q = cumsum(discharge_fill),
         percentile_q = cum_q/max(cum_q),
         q_na = is.na(discharge))

mart_PC1_q <- mart_cumsum %>%
  filter(month(`date`) != 10 & year(`date`) >1982) %>%
  mutate(WY = year(`date`)) %>% # there are no Oct-Dec months in this dataset
  left_join(nwt_climdat, by = c("WY" = "eco_year")) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0.5), col = "grey50", lty = 3) +
  geom_point(data = subset(mart_cumsum, q_na == TRUE), aes(doy, percentile_q), size = 2) +
  geom_path(aes(x=doy, percentile_q, group = year(`date`), col = sumallPC1), 
            alpha = 0.5, lwd = 1.5) +
  scale_color_distiller(name = "Extended\nsummer PC", palette = "RdBu") +
  labs(subtitle = "Martinelli outlet, 1982-2017",
       y = "Q percentile", x = NULL) +
  #scale_x_continuous(labels = strftime(`date`,format="%m-%d")) +
  theme_bw()



## ---- all sites cumulative 
# nwt_q <- rbind(gl4_q[c("site", "date", "discharge")], alb_q[c("site", "date", "discharge")], 
#                mart_q[c("date", "discharge")], alb_q[c("date", "discharge")]) %>%
#            mutate(doy = yday(`date`)) %>%
#   filter(doy %in% 120:273) %>%
#   #fill NAs with 0 so cumulative sum will run (just to see how all years plot out)
#   mutate(discharge_fill = ifelse(is.na(discharge), 0, discharge)) %>%
#   group_by(year(`date`)) %>%
#   mutate(cum_q = cumsum(discharge_fill),
#          percentile_q = cum_q/max(cum_q),
#          q_na = is.na(discharge)) %>%
  


## ----- FIGURES -----
# plot saddle cumulative Q stacked over GL4 cumulative Q
plot_grid(gl4_PC1_q, mart_PC1_q, sdl_PC1_q, alb_PC1_q,
          nrow = 4)

plot_grid(sdl_maxQ_trend, gl4_maxQ_trend,
          nrow = 2)
