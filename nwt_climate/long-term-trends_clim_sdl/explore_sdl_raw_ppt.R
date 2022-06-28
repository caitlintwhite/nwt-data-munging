# explore sdl ppt raw data
# author(s): ctw
# date initiated: 2022-06-14
# questions?: caitlin.t.white@colorado.edu


# note: until otherwise noted, only ctw and sce should work with this scipt and any resulting dataset(s).


# parking lot for ideas:
# oct-may overcatch correction factor
# - USSR dataset has correction factors by month, informed by wind speed and temp at gauge [k2 scale] (groisman et al. 1991)
# > note: USSR dataset measures ppt by volume in bucket not via dipstick
# > note: USSR team made extensive effort to create/reconstruct metadata from physical files for +600 stations.. to the extent possible, a complete station history *is* necessary to address inhomogeneities detected (esp to dist. artificial change from climate-induced)

# compare sdl v d1, break down by month:
# > how often does saddle have >0 ppt, d1 == 0 & vv?
# > do we have daily wind at/near saddle? (e.g., hourly logger dataset?)
# > how do wind and temp relate to raw daily ppt observed at saddle?
# > what is the relationship between sdl and d1 ppt by month (summer/less snowy months should be more reflective of unbiased/less biased relationship?)
# can you find the month in WY 1995 where SDL raw diff from D1 starts increasing? annual ppt 1995 is first year in william et al. 1998 to jump (their fig only goes to 1996, which is also high)


# compare sdl v d1 v sdl nadp ppt
# do ppt event days at sdl nadp and d1 agree?
# notes: when rainfall light and/or winds moderate, gauge may not catch wind
# > blowing snow tends to occur AFTER storm, on clear day with high winds; undercatch may occur during storm
# > kittel et al. 2015 methods est D1 may undercatch by 4-9% in winter
# d1 uses alter-type shield and wyo snow fence (good bc baron 1990 paper says Nipher shield accuracy is variable in places that are not full exposed to wind [aerodyn of shield messed with])

# how does tvan compare? or other GLV station?
# is there a way to approximate williams correction factor with the data?


# references:
# Overcoming Biases of Precipitation Measurement: A History of the USSR Experience Author(s): P. Ya. Groisman, V. V. Koknaeva, T. A. Belokrylova and T. R. Karl Source: Bulletin of the American Meteorological Society, Vol. 72, No. 11 (November 1991), pp. 1725-1733




# -- SETUP -----
library(tidyverse)
library(lubridate)
na_vals <- c(NA, "NA", ".", NaN, "NaN", " ", "")
options(stringsAsFactors = F)
theme_set(theme_test())

# source functions to read in in data from EDI
source("utility_functions/utility_functions_all.R")


# raw sdl dats
# chart ppt
sdl_ppt <- getTabular(edi_id = 416)
sdl_cr <- getTabular(405) # daily cr23x and cr1000, 2000 to ongoing
sdl_cr_cr21x <- getTabular(78) # daily cr logger, 1986-2000
sdl_chart <- getTabular(413) # chart temp 1981 to dec 21 2017
sdl_clim <- getTabular(81)
sdl_dp <- getTabular(80)

# d1 ppt
d1ppt_raw <- getTabular(415)
d1ppt_k <- getTabular(186)
d1clim <- getTabular(73) # raw data, includes wind speed, solrad

# c1 ppt
c1ppt_k <- getTabular(184)

# sdl nadp
# weekly
nadp <- read.csv("clim_sdl/NTN-co02-W-i.csv", strip.white = T, na.strings = c(na_vals, -9.99))
unique(nadp$ppt[nadp$ppt<0])
# sdl monthly
nadp_sdl_monthly <- read.csv("clim_sdl/NTN-co02-i-mgl.csv", strip.white = T, na.strings = c(na_vals, -9.99))
# nwt_se monthly (to compare w c1)
# > if this matches c1 well, then trends in sdl are what they are (weekly nadp @ sdl matches pretty well)
nadp_se_monthly <- read.csv("clim_sdl/NTN-co90-i-mgl.csv", strip.white = T, na.strings = c(na_vals, -9.99))


# jennings et al. infilled hourly dat
jennings <- getTabular(168) 

# read in sdl ppt dat made for nwt 8 renewal just to look for breaks (do they match with rawdat?)
# > this goes into nwt8-renewal repo
sdl_ppt_nwt8 <- read.csv("../nwt8-renewal/c1_d1_sdl_clim/homogenize_climdat/data/sdl_ppt_1981-2020_draft.csv", na.strings = na_vals)


# -- COMPARATIVE PLOTS -----
# sdl ppt aggregated to nadp windows -- how do tallies compare?
# > note: this will include NA vals for SDL

nadp_ppt <- subset(nadp, select= c(dateOn, dateOff, ppt:invalcode)) %>%
  mutate(start_date = as.Date(substr(dateOn, 1, 10)),
         end_date = as.Date(substr(dateOff, 1, 10)),
         dateOn = as.POSIXct(dateOn, tz = "UTC"),
         dateOff = as.POSIXct(dateOff, tz = "UTC"),
         event = as.numeric(rownames(.))) %>%
  distinct()
# change -7 (trace) to 0.5mm and -9.99 to NA
nadp_ppt$ppt[nadp_ppt$ppt < -9] <- NA
nadp_ppt$ppt[nadp_ppt$ppt == -7] <- 0.5

# iterate through saddle dataset and note which event date corresponds to (append time of 23:59 so count first day in interval but not last?)
sdl_ppt$event <- 0
sdl_ppt$datetime <- as.POSIXct(paste(sdl_ppt$date, "20:00:00"), tz = "UTC")
sdl_ppt$date <- as.Date(sdl_ppt$date)
sdl_ppt$rowid <- as.numeric(rownames(sdl_ppt))
for(i in sdl_ppt$rowid[sdl_ppt$datetime >= min(nadp_ppt$dateOn) & sdl_ppt$datetime <= max(nadp_ppt$dateOn)]){
  #tempevent <- nadp_ppt$event[sdl_ppt$datetime[i] >= nadp_ppt$dateOn & sdl_ppt$datetime[i] <= nadp_ppt$dateOff]
  tempevent <- nadp_ppt$event[sdl_ppt$date[i] > nadp_ppt$start_date & sdl_ppt$date[i] <= nadp_ppt$end_date]
  if(length(tempevent)>1){
    tempevent <- max(tempevent)
  }
  sdl_ppt$event[i] <- tempevent
}

# aggregate ppt by event
# being lazy and not backfilling ppt for accumulated days first
weekly_ppt <- group_by(sdl_ppt, event) %>%
  summarise(wkly_ppt = sum(ppt_tot, na.rm = T),
            m_days = sum(is.na(ppt_tot)),
            q_days = str_flatten(unique(qdays), collapse = ", "),
            end_date_ctw = as.Date(max(datetime))) %>%
  subset(event >0)

# plot and compare
#test <- 
left_join(nadp_ppt, weekly_ppt, by = "event") %>%
  mutate(diffNADP_sdl = ppt - wkly_ppt,
         decade = paste0(substr(year(end_date), 1,3),0)) %>%
  ggplot(aes(dateOff, ppt)) +
  #geom_line(aes(dateOff, diffNADP_sdl)) +
  #geom_point(aes(dateOff, diffNADP_sdl, col = month(end_date_ctw) %in% 6:9)) +
  geom_boxplot(aes(decade, diffNADP_sdl, col = month(end_date) %in% 6:9))
  #geom_hline(aes(yintercept = 0), col = "blue") +
  #geom_smooth() +
  #geom_hline(aes(yintercept = mean(diffNADP_sdl, na.rm = T )), col = "green") +
  facet_wrap(~decade, scales = "free_x") 
  coord_flip()

left_join(nadp_ppt, weekly_ppt, by = "event") %>%
  mutate(diffNADP_sdl = ppt - wkly_ppt) %>%
  ggplot(aes(ppt, wkly_ppt)) +
  geom_point(aes(col = year(end_date)), alpha = 0.6) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_color_viridis_c(option = "B") +
  labs(x = "weekly NADP ppt (mm)", y = "weekly raw SDL ppt (mm)") +
  facet_wrap(~month(end_date) %in% 6:9,labeller = as_labeller(c('FALSE' = "winter", 'TRUE' = "summer")), scales = "free_x")

d1_ppt_annual <- group_by(d1ppt_k, year) %>%
  summarise(d1_annual = sum(precip, na.rm = T)) %>%
  subset(year > 1984)
c1_ppt_annual <- group_by(c1ppt_k, year) %>%
  summarise(c1_annual = sum(precip, na.rm = T)) %>%
  subset(year > 1984)

left_join(nadp_ppt, weekly_ppt, by = "event") %>%
  mutate(yr = year(end_date)) %>%
  group_by(yr) %>%
  summarise(annual_sdl = sum(wkly_ppt, na.rm = T),
            annual_nadp = sum(subppt, na.rm = T)) %>%
  ggplot() +
  geom_line(aes(yr, annual_sdl)) +
  geom_line(aes(yr, annual_nadp), col = "orchid") +
  geom_line(data = d1_ppt_annual, aes(year, d1_annual), col = "royalblue2") +
  geom_line(data = c1_ppt_annual, aes(year, c1_annual), col = "forestgreen")


# comparing raw and qdays == 1, how many days was there ppt at d1 but not sdl?
sdl_ppt$mo <- month(sdl_ppt$date)
d1ppt_k$mo <- month(d1ppt_k$date)

compppt <- subset(sdl_ppt, qdays == 1, select = -local_site) %>%
  rename(sdl_ppt = ppt_tot) %>%
  left_join(subset(d1ppt_k, raw_qdays == 1 & is.na(regression_equation), select = -local_site)) %>%
  rename(d1_ppt = precip) %>%
  # drop anything where sdl or d1 is na
  subset(!is.na(sdl_ppt) & !is.na(d1_ppt)) 

summary(with(compppt, sdl_ppt >0 & d1_ppt == 0)) # 1263 days at sdl have ppt when d1 does not
# what is the distribution by month?
sumppt <- group_by(compppt, mo, year) %>%
  summarise(both_dry = length(date[d1_ppt == 0 & sdl_ppt == 0]),
            both_wet = length(date[d1_ppt > 0 & sdl_ppt > 0]),
            sdl_wet = length(date[d1_ppt == 0 & sdl_ppt > 0]),
            d1_wet = sum(d1_ppt > 0 & sdl_ppt == 1),
            totdays = length(date)) %>%
  ungroup() %>%
  mutate(decade = paste0(substr(year,1,3), 0))

sumppt %>%
  gather(met, val, both_dry:d1_wet) %>%
  ggplot(aes(met, val, col = year)) +
  geom_point(position= position_dodge(width = 0.2)) +
  geom_line(aes(group = year)) +
  scale_color_viridis_c(option = "B") +
  facet_wrap(~mo)
  

sumppt %>%
  gather(met, val, both_dry:d1_wet) %>%
  subset(grepl("^sdl|^d1", met)) %>%
  mutate(val = val/totdays) %>%
  # only consider when d1 and sdl mismatched
  # is there any trend by decade?
  ggplot(aes(met, val, group = paste(decade, met), fill = decade, col = year)) +
  geom_boxplot(varwidth = T) +
  geom_point(position = position_jitterdodge(dodge.width = .7, jitter.width = 0.2, jitter.height = 0)) +
  scale_color_viridis_c(option = "B") +
  scale_fill_viridis_d(option = "B") +
  facet_wrap(~mo)

sdlwind <- subset(sdl_cr_cr21x, select = c(logger, date, year, jday, airtemp_avg, ws_max, ws_scal_avg, ws_min, wd_avg)) %>%
  rename(wd =wd_avg, ws_avg = ws_scal_avg) %>%
  rbind(sdl_cr[names(.)])

# what is distrib on rainfall on days where d1 and sdl are mismatched?
compppt <- mutate(compppt, d1_wet = d1_ppt > 0 & sdl_ppt == 0,
                  sdl_wet = d1_ppt == 0 & sdl_ppt > 0) %>%
  # join windinfo available (2000 onwards)
  left_join(sdlwind)

mismatched <- subset(compppt, d1_wet | sdl_wet, select = c(date, year, mo, sdl_wet, d1_wet, sdl_ppt, d1_ppt, airtemp_avg:wd)) %>%
  mutate(condition = ifelse(sdl_wet, "sdl_wet", "d1_wet")) %>%
  dplyr::select(-c(sdl_wet, d1_wet))


mismatched %>%
  # what about days where ppt diff is rel larger
subset(sdl_ppt > 25 | d1_ppt > 25) %>%
gather(met, val, sdl_ppt:wd) %>%
  mutate(decade = paste0(substr(year, 1,3), 0)) %>%
  ggplot(aes(condition, val, group = paste(mo, condition), col = decade)) +
  # stat_summary(geom = "line", fun =range, position = position_dodge(width = 0.3)) +
  # stat_summary(position = position_dodge(width = 0.3)) +
  #geom_boxplot() +
  geom_jitter(alpha = 0.8) +
  #stat_summary(geom = "line", position = position_dodge(width = 0.3)) +
  #scale_color_distiller() +
  scale_color_viridis_d() +
  facet_grid(met~mo, scales = "free_y")

psych::pairs.panels(subset(mismatched, condition == "sdl_wet", select = c(mo, sdl_ppt:wd)), density = T, ellipses = T, method = "pearson")
psych::pairs.panels(subset(mismatched, condition == "d1_wet", select = c(mo, sdl_ppt:wd)), density = T, ellipses = T, method = "pearson")


# when does diff between sdl and d1 start getting bigger?
# > on days when they both have rain
compdiff <- compppt %>%
  #subset(compppt, sdl_ppt > 0 & d1_ppt > 0) %>%
  mutate(diffsdl_d1 = sdl_ppt - d1_ppt,
         decade = paste0(substr(year, 1,3),0),
         season = ifelse(mo %in% 6:9, "summer", "winter"),
         condition = ifelse(d1_wet, "d1_wet", ifelse(sdl_wet, "sdl_wet", "both_same")))

# look at c1 on days that sdl and d1 have rain
compc1 <- subset(c1ppt_k, date %in% compdiff$date[compdiff$condition == "both_same"])
# how many of these dates are infilled?
summary(is.na(compc1$regression_equation)) #231 of 10155 obs infilled

# look for discernable difference in d1/sdl by year
hist(compdiff$diffsdl_d1)
diff_aov <- lm(diffsdl_d1 ~ factor(condition) * factor(year), data = compdiff)
summary(diff_aov) # nothing signif when just subset days where sdl had rain and d1 did not
aov_df <- broom::tidy(summary(diff_aov))
aov_df <- data.frame(anova(diff_aov))
TukeyHSD(diff_aov)

ggplot(subset(compdiff, ws_max <150), aes(date, diffsdl_d1, group = decade)) +
  geom_point(aes(col = ws_max), alpha = 0.6) +
  geom_hline(aes(yintercept = 0), col = "grey30", lty = 2) +
  geom_smooth(col = "black", fill = "orchid", method = "lm") +
  scale_color_distiller(palette= "Spectral") +
  facet_wrap(~mo, scales = "free")

ggplot(subset(compdiff, ws_max <150| is.na(ws_max)), aes(year, diffsdl_d1)) + #scale
  geom_point(aes(col = ws_max), alpha = 0.8) +
  geom_violin(aes(group = year), fill = "transparent") + #outlier.alpha = 0.2
  geom_hline(aes(yintercept = 0), col = "grey30", lty = 2) +
  geom_smooth(aes(group = decade), col = "black", fill = "orchid") +
  #scale_color_distiller(palette= "Spectral") +
  scale_color_viridis_c(option = "B") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~mo) #, scales = "free_x"

# has anything in wind trends been changing over time to suggest why sdl relationship and d1 may change?
sdlwind %>%
  mutate(mo = month(date), decade = paste0(substr(year, 1,3),0)) %>%
  # remove high wind speed outlier
  subset(!ws_max > 100) %>%
  ggplot(aes(date, ws_max,  col = decade)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), col = "grey30", lty = 2) +
  geom_smooth(col = "red", fill = "pink") +
  geom_smooth(aes(group = decade), col = "black", fill = "grey", method = "lm") +
  facet_wrap(~mo, scales = "free_x")
# strong chinook windstorms feb 1999 (and boulder events generally support stronger wind trends in late 1990s-2000)
# https://psl.noaa.gov/boulder/wind.html



# -- DETREND AND COMPARE -----
# if remove seasonality to monthly data, is a shift more discernable?
# only compare good raw data, where all dats have data collected on same day (apples to apples)
# > I did compare detrended/deseasoned ppt dats for *infilled* ppt datasets

# aggregate to monthly
monthly_ppt <- compppt %>%
  group_by(year, mo) %>%
  summarise(d1_monthly = sum(d1_ppt),
            sdl_monthly = sum(sdl_ppt)) %>%
  ungroup() %>%
  mutate(wy = ifelse(mo %in% 10:12, year+1, year),
         wy_mo = ifelse(mo %in% 10:12, mo-9, mo+3),
         diffsdl_d1 = sdl_monthly - d1_monthly) %>%
  group_by(wy) %>%
  mutate(nobs = length(wy_mo))
with(monthly_ppt, unique(wy[nobs != 12])) # just compare 1982-2013 since earliest year with missing month is 2014

# going off of wy, and wy_mon (oct = 1)
sdl_ts <- ts(data = subset(monthly_ppt, wy %in% 1985:2013, select = sdl_monthly), start = c(1985,1), frequency = 12)
d1_ts <- ts(data = subset(monthly_ppt, wy %in% 1985:2013, select = d1_monthly), start = c(1985,1), frequency = 12)
diff_ts <- ts(data = subset(monthly_ppt, wy %in% 1985:2013, select = diffsdl_d1), start = c(1985,1), frequency = 12)
sdl_decomp <- decompose(sdl_ts)
d1_decomp <- decompose(d1_ts)
diff_decomp <- decompose(diff_ts)
plot(sdl_decomp$trend, d1_decomp$trend)
plot(diff_decomp)

# decomp nadp
# > not going to be perfect because weeks span months sometimes
nadp_monthly <- nadp_ppt %>%
  mutate(yr = year(start_date),
         mo = month(start_date),
         wy = ifelse(mo %in% 10:12, yr+1, yr),
         wy_mo = ifelse(mo %in% 10:12, mo-9, mo+3)) %>%
  group_by(wy, wy_mo) %>%
  summarise(nadp_monthly = sum(subppt, na.rm = T)) %>%
  ungroup() %>%
  group_by(wy) %>%
  mutate(nobs = length(wy_mo))
# only 1984 (starting year and 2022 have less than 12 months)
nadp_ts <- ts(data = subset(nadp_monthly, wy %in% 1985:2013, select = nadp_monthly), start = c(1985,1), frequency = 12)
nadp_decomp <- decompose(nadp_ts)
plot(nadp_decomp)

# compare with monthly dataset
comp_nadp <- nadp_sdl_monthly %>%
  mutate(wy = ifelse(seas %in% 10:12, yr+1, yr),
         wy_mo = ifelse(seas %in% 10:12, seas-9, seas+3),
         # monthly nadp is in cm no mm
         ppt = ppt*10) %>%
  dplyr::select(yr, seas, wy, wy_mo, ppt) %>%
  full_join(nadp_monthly[,1:3]) %>%
  arrange(wy, wy_mo) %>%
  mutate(plot_date = as.Date(paste(wy, wy_mo, 1, sep = "-")))

gather(comp_nadp, met, val, ppt, nadp_monthly) %>%
  ggplot(aes(plot_date, val, col = met)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~met, nrow = 2)

nadp_monthly_ts <- ts(data = subset(comp_nadp, wy %in% 1985:2013, select = ppt), start = c(1985,1), frequency = 12)
nadp_monthly_decomp <- decompose(nadp_monthly_ts)
plot(nadp_monthly_decomp)

# stack all
alltog <- subset(comp_nadp, wy %in% 1985:2013) %>%
  left_join(monthly_ppt) %>%
  rename(nadpMo_monthly = ppt) %>%
  dplyr::select(-nobs)

decomp_df <- data.frame(
  cbind(
    sdl_trend = sdl_decomp$trend,
    sdl_random = sdl_decomp$random,
    d1_trend = d1_decomp$trend,
    d1_random = d1_decomp$random,
    nadp_trend = nadp_decomp$trend,
    nadp_random = nadp_decomp$random,
    diffsdld1_trend = diff_decomp$trend,
    diffsdld1_random = diff_decomp$random,
    nadpMo_trend = nadp_monthly_decomp$trend,
    nadpMo_random = nadp_monthly_decomp$random
  )
) %>%
  mutate(
    sdl_des = sdl_trend + sdl_random,
    d1_des = d1_trend + d1_random,
    nadp_des = nadp_trend + nadp_random,
    diffsdld1_des = diffsdld1_trend + diffsdld1_random,
    nadpMo_des = nadpMo_trend + nadpMo_random
  )

alltog_ppt <- cbind(alltog, decomp_df) %>%
  rename(diffsdld1_monthly = diffsdl_d1) %>%
  gather(met, val, nadpMo_monthly, nadp_monthly, d1_monthly:ncol(.)) %>%
  separate(met, c("site", "met"), sep = "_") %>%
  mutate(plot_date = as.Date(paste(wy, wy_mo, 1, sep = "-")))

ggplot(alltog_ppt, aes(plot_date, val, col = site)) +
  geom_line(alpha = 0.6) +
  geom_hline(aes(yintercept = 0)) +
  geom_smooth(aes(fill=site), alpha = 0.5) +
  facet_grid(met~site, scales = "free_y")


ggplot(subset(alltog_ppt, met == "des"), aes(plot_date, val, col = site)) +
  geom_line(alpha = 0.6) +
  geom_smooth(method = "gam") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(~site, nrow = 3)

# d1 vs. sdl
spread(alltog_ppt, site, val) %>%
  ggplot(aes(sdl, d1, col = plot_date)) +
  geom_point() +
  #geom_smooth(method = "gam") +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_colour_viridis_c(trans = "date") +
  facet_wrap(~met, scales = "free")

# nadp weekly to monthly vs. sdl
spread(alltog_ppt, site, val) %>%
  ggplot(aes(sdl, nadp, col = plot_date)) +
  geom_point() +
  #geom_smooth(method = "gam") +
  scale_colour_viridis_c(trans = "date") +
  facet_wrap(~met, scales = "free")

# nadp monthly vs. sdl
spread(alltog_ppt, site, val) %>%
  ggplot(aes(sdl, nadpMo, col = plot_date)) +
  geom_point() +
  #geom_smooth(method = "gam") +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_colour_viridis_c(trans = "date") +
  facet_wrap(~met, scales = "free")

# nadp monthly vs. sdl
spread(alltog_ppt, site, val) %>%
  ggplot(aes(nadp, nadpMo, col = plot_date)) +
  geom_point() +
  #geom_smooth(method = "gam") +
  geom_abline(aes(intercept = 0, slope = 1)) +
  scale_colour_viridis_c(trans = "date") +
  facet_wrap(~met, scales = "free")


# look for changepoints
library(changepoint)
changepoint::cpt.mean(diff_decomp$trend[!is.na(diff_decomp$trend)]) # mean changes at point 100
changepoint::cpt.var(diff_decomp$trend[!is.na(diff_decomp$trend)]) # variance changes at point 29

subset(alltog_ppt, site == "sdl" & met == "trend" & !is.na(val))[100,] # july 1993
subset(alltog_ppt, site == "sdl" & met == "trend" & !is.na(val))[29,] # aug 1987


changepoint::cpt.mean(decomp_df$nadpMo_des[!is.na(decomp_df$nadpMo_des)]) #mean changes at 91, no change in variance
changepoint::cpt.mean(decomp_df$nadp_des[!is.na(decomp_df$nadp_des)]) # mean change at 90 (sep 1992) -- same whether use trend or deseasoned dat
changepoint::cpt.mean(decomp_df$d1_trend[!is.na(decomp_df$d1_trend)]) # mean change at 318 for d1
changepoint::cpt.meanvar(decomp_df$diffsdld1_des[!is.na(decomp_df$diffsdld1_des)]) # mean change at 98 (may 1993), mean and variance change at 103 (sep 1993)
changepoint::cpt.var(decomp_df$diffsdld1_des[!is.na(decomp_df$diffsdld1_des)]) # no change in variance on its own

subset(alltog_ppt, site == "sdl" & met == "trend" & !is.na(val))[91,] # sep 1992
subset(alltog_ppt, site == "d1" & met == "trend" & !is.na(val))[318,] # nov 2011

# look at trends in wind
sdl_wind_monthly <- sdlwind %>%
  mutate(mo = month(date)) %>%
  gather(met, val, airtemp_avg:wd) %>%
  group_by(year, mo, met) %>%
  summarise(mo_avg = mean(val, na.rm = T),
            nobs = length(val[!is.na(val)])) %>%
  #drop anything that doesn't have observation
  subset(nobs > 0) %>%
  group_by(year, met) %>%
  mutate(yr_nobs = length(mo))

# which years are complete (enough?
with(subset(sdl_wind_monthly, yr_nobs != 12, select = c(met, year)), sapply(split(year, met), function(x) sort(unique(x))))
# can do ws_average 1993 onwards
ws_avg <- ts(subset(sdl_wind_monthly, met == "ws_avg" & year > 1992, select = mo_avg),start = c(1993,1), freq = 12)
ws_avg_decomp <- decompose(ws_avg)
plot(ws_avg_decomp)

changepoint::cpt.meanvar(ws_avg_decomp$trend[!is.na(ws_avg_decomp$trend)]) # no step change in mean wind, but meanwind is 72
meanwind_decomp_df <- subset(sdl_wind_monthly, met == "ws_avg" & year > 1992, select = c(year, mo, mo_avg)) %>%
  cbind(data.frame(meanwind_trend = ws_avg_decomp$trend)) %>%
  cbind(data.frame(meanwind_random = ws_avg_decomp$random)) %>%
  rename(meanwind_random = 'x...seasonal') %>%
  mutate(meanwind_des = meanwind_trend + meanwind_random)

alltog_ppt_ws <- left_join(distinct(dplyr::select(alltog_ppt, yr:mo)), meanwind_decomp_df) %>%
  rename(meanwind_monthly = mo_avg) %>%
  gather(met, val, meanwind_monthly:ncol(.)) %>%
  separate(met, c("site", "met"), sep = "_")

alltog_ppt_ws <- rbind(alltog_ppt, alltog_ppt_ws[names(alltog_ppt)])

ggplot(subset(alltog_ppt_ws, site %in% c("sdl", "meanwind", "d1", "nadpMo") & met %in% c("des","trend")), aes(plot_date, val, col = site)) +
  geom_line() +
  geom_smooth(method = "gam") +
  facet_wrap(~site+met, scales = "free_y", nrow = 4)

cor.test(with(alltog_ppt_ws, val[site == "meanwind" & met == "trend"]), with(alltog_ppt_ws, val[site == "sdl" & met == "trend"]), na.action = "na.remove")
# 28% pos corr..


# where are the changepoints with infilled sdl ppt for renewal?
sdl_nwt8_ts <- ts(data = subset(sdl_ppt_nwt8, year %in% 1982:2020, select = winter_adjusted), start = c(1982,1,1), frequency = 365.25)
sdl_nwt8_decomp <- decompose(sdl_nwt8_ts)
sdl_nwt8_decomp_df <- subset(sdl_ppt_nwt8, year %in% 1982:2020, select= c(date, year, winter_adjusted))
sdl_nwt8_decomp_df <- cbind(sdl_nwt8_decomp_df, data.frame(trend = sdl_nwt8_decomp$trend))
sdl_nwt8_decomp_df$date <- as.Date(sdl_nwt8_decomp_df$date)
plot(sdl_nwt8_decomp)
changepoint::cpt.meanvar(sdl_nwt8_decomp$trend[!is.na(sdl_nwt8_decomp$trend)]) #4520, var alone at 6421
subset(sdl_nwt8_decomp_df, year %in% 1982:2020 & !is.na(trend))[6421,] # 11-15-1994 = 4520
ggplot(sdl_nwt8_decomp_df, aes(date, trend)) +
  geom_line() +
  geom_smooth(method = "loess") +
  geom_vline(aes(xintercept = as.Date("1995-11-15")), col = "red") +
  geom_vline(aes(xintercept = as.Date("2000-01-29")), col = "orchid") +
  scale_x_date(date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 90))

changepoint::cpt.var(sdl_ppt_nwt8$winter_adjusted) #3009 = meanvar, 4901 = mean, 2920 = var alone
sdl_ppt_nwt8$date[2920] #1989-10-25 for meanvar, 1994-12-30 for mean change alone, 1989-07-28 for variance, 
changepoint::cpt.mean(d1ppt_k$precip) #11238 for variance and meanvar, 10914 for mean
d1ppt_k$date[10914] #1982-10-07 = 11238, 1981-11-17 for mean change

changepoint::cpt.meanvar(c1ppt_k$precip[c1ppt_k$year %in% c(1982:2020)], method = "BinSeg", class = T, Q = 10) #1884 for mean, 1885 for meanvar and var; 11421 for 1982 onwards variance, 14030 for meanvar
c1ppt_k$date[1884] #1957-02-26
c1ppt_k$date[c1ppt_k$year %in% 1982:2020][11581] #2013-04-08


# -- CLIMATOL TEST -----
# following examples in CRAN guide

# write out table of stations
# c1, d1, sdl ppt
c1_simple <- cbind(station = "c1", dplyr::select(c1ppt_k, date, precip)) %>%
  separate(date, into= c("year", "month", "day"), sep = "-", remove = F)
d1_simple <- cbind(station = "d1", dplyr::select(d1ppt_k, date, precip)) %>%
  separate(date, into= c("year", "month", "day"), sep = "-", remove = F)
sdl_simple <- cbind(station = "sdl", dplyr::select(sdl_ppt_nwt8, date, winter_adjusted)) %>%
  rename(precip = winter_adjusted) %>%
  separate(date, into= c("year", "month", "day"), sep = "-", remove = F)
# add dates from c1 and d1 to sdl with NAs
emptysdl <- subset(c1_simple, date < min(sdl_simple$date)) %>%
  mutate(station = "sdl", precip = NA)
# stack
sdl_simple <- rbind(emptysdl, sdl_simple)
# make sure all dates present at all stations
summary(seq.Date(min(c1_simple$date), max(c1_simple$date), 1) %in% c1_simple$date)
summary(seq.Date(min(d1_simple$date), max(d1_simple$date), 1) %in% d1_simple$date)
summary(seq.Date(min(sdl_simple$date), max(sdl_simple$date), 1) %in% sdl_simple$date) # all there
nwt_stations <- data.frame(file = c("clim_sdl/d1.txt", "clim_sdl/c1.txt", "clim_sdl/sdl.txt"),
                           lon = c(-105.5878198,-105.543657	,-105.5923174),
                           lat = c(40.05490012,40.036902,40.05492794),
                           elev = c(3739, 3022, 3528),
                           station = c("d1", "c1", "sdl"))
write.table(nwt_stations, "clim_sdl/stations.txt", row.names = F, col.names = F)
write.table(d1_simple, "clim_sdl/d1.txt", row.names = F, col.names = F)
write.table(c1_simple, "clim_sdl/c1.txt", row.names = F, col.names = F)
write.table(sdl_simple, "clim_sdl/sdl.txt", row.names = F, col.names = F)
daily2climatol("clim_sdl/stations.txt", stcol = c(1:5,0), datcol = c(3:6),anyi = 1952, anyf = 2020, varcli= "precip")
homogen("precip", 1952, 2020, std = 2)
homogen("precip-mh", 1952, 2020, std = 2)



# use 1982-2020 and raw data values (except where qdays > 1 backfilled)
# > nm bc backfilled days have qdays == NA, no way to quickly ID true missing data
c1_simple_82 <- cbind(station = "c1", dplyr::select(c1ppt_k, date, precip)) %>%
  separate(date, into= c("year", "month", "day"), sep = "-", remove = F) %>%
  subset(year > 1981)
d1_simple_82 <- cbind(station = "d1", dplyr::select(d1ppt_k, date, precip)) %>%
  separate(date, into= c("year", "month", "day"), sep = "-", remove = F) %>%
  subset(year > 1981)
sdl_simple_82 <- cbind(station = "sdl", dplyr::select(sdl_ppt_nwt8, date, winter_adjusted)) %>%
  rename(precip = winter_adjusted) %>%
  separate(date, into= c("year", "month", "day"), sep = "-", remove = F) %>%
  subset(year > 1981)
# write out
write.table(d1_simple_82, "clim_sdl/d1.txt", row.names = F, col.names = F)
write.table(c1_simple_82, "clim_sdl/c1.txt", row.names = F, col.names = F)
write.table(sdl_simple_82, "clim_sdl/sdl.txt", row.names = F, col.names = F)
daily2climatol("clim_sdl/stations.txt", stcol = c(1:5,0), datcol = c(3:6),anyi = 1982, anyf = 2020, varcli= "precip")

homogen("precip", 1982, 2020, std = 2, expl = T)
# calc monthly totals
dd2m('precip', 1982, 2020, valm = 1)
homogen("precip-m", 1982, 2020, std = 2, metad = T, sufbrk = "")
