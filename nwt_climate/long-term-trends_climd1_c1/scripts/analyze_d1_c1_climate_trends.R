# analyze d1 and c1 climate temporal trends

# script purpose:
# try to replicate temporal and monthly trends shown in NWT renewal for C1 and D1
# update analysis with yrs 2011-ongoing appended

# trends/figs shown in renewal:
# (1) annual precip over time, D1 & C1, with trend line with break at 1976/1977 for PDO Regime Shift
# > note: TK et al. 2015 did not show break line for PDO shift at 1976/1977
# > also looking at renewal code, was an annual calendar year ppt not water year ppt
# (2) max T over time, D1 + C1, with trend line
# (3) monthly max T trend, D1 + C1, with asterisks for months with signif trends



# -- SETUP ------
rm(list = ls())
library(tidyverse)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN", "NP")

# set pathway to extended summer analysis data
datpath <- "climate_d1_c1/output_data/"

# functions to read in data from EDI Data Portal by package ID number (version not neeeded)
source("utility_functions/utility_functions_all.R")

# read in data
# tim kittel infilled d1 and c1 temp -- only goes thru dec 31 2010
## from NWT renewal dropbox
tkd1temp <- read.csv("~/Dropbox/NWT_data/d1_infilled_daily_temp.csv", na.strings = na_vals)
tkc1temp <- read.csv("~/Dropbox/NWT_data/c1_infilled_daily_temp.csv", na.strings = na_vals)

# ctw infilled d1 and c1 climate data (2011-ongoing), appended to tk infilled data
## temp
d1temp_all <- read.csv(paste0(datpath, "d1_dailytemp_infilled_1952-2018.csv"), na.strings = na_vals, strip.white = T)
c1temp_all <- read.csv(paste0(datpath, "c1_dailytemp_infilled_1952-2018.csv"), na.strings = na_vals, strip.white = T)
## ppt
d1ppt_all <- read.csv(paste0(datpath, "d1_dailyppt_infilled_1952-2018.csv"), na.strings = na_vals, strip.white = T)
c1ppt_all <- read.csv(paste0(datpath, "c1_dailyppt_infilled_1952-2018.csv"), na.strings = na_vals, strip.white = T)



# -- REPLICATE ANALYSES ON NWT RENEWAL DATA -----
# temporal trend in max T
d1maxT <- tkd1temp %>%
  group_by(month, year) %>%
  summarize(maxT = max(Tmax)) %>%
  ungroup() %>%
  mutate(month = as.factor(month))

monthlm <- lm(maxT ~ year, data = subset(d1maxT, month == 1))
monthlm <- lm(maxT ~ year, data = subset(d1maxT, month == 2))
monthlm <- lm(maxT ~ year, data = subset(d1maxT, month == 7))
monthlm <- lm(maxT ~ year, data = subset(d1maxT, month == 12)) # can confirm analysis was done subsetting month of interest and then regressing year on maxT
summary(monthlm)

d1maxT_annual <- d1maxT %>%
  group_by(year) %>%
  summarize(maxT = max(maxT))

summary(lm(maxT ~ year, d1maxT_annual)) # same results (+0.6C per decade)

ggplot(d1maxT_annual, aes(year, maxT)) +
  geom_line() +
  geom_point() +
  geom_smooth() 


# -- UPDATE FIGURES AND ANALYSES TO 2018 -----
# specify site colors and shapes
site_cols <- c("D1" = "deepskyblue", "C1" = "forestgreen")
month_labels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
site_pch <- c("D1" = 17, "C1" = 16)
#specify annotation size
plottext = 3
# stack datasets for easier paneling
nwttemp <- rbind(d1temp_all, c1temp_all)
nwtppt <- rbind(d1ppt_all, c1ppt_all)

# -- TEMP FIGS -----
# calculate temporal trend in *monthly* max T
monthTmax <- nwttemp %>%
  group_by(local_site, year, month) %>%
  summarize(maxT = max(Tmax.C)) %>%
  ungroup() %>%
  mutate(month = as.factor(month))

# iterate through monthly lms and store results, by site
# initiate data frame for storing results
monthTlms <- data.frame()
for(s in unique(monthTmax$local_site)){
  temp_df <- subset(monthTmax, local_site == s)
  # iterate through months
  for(m in 1:12){
    temp_lm <- lm(maxT ~ year, data = subset(temp_df, month == m))
    results_df <- data.frame(cbind(local_site = s,
                                   month = m,
                                   adj.r2 = summary(temp_lm)$adj.r.squared,
                                   pval = summary(temp_lm)$coefficients[8],
                                   effect = summary(temp_lm)$coefficients[2],
                                   effect_se = summary(temp_lm)$coefficients[4],
                                   eq = paste0(round(summary(temp_lm)$coefficients[1],4), " + ", round(summary(temp_lm)$coefficients[2],4), "x"),
                                   nobs = nrow(temp_lm$model)))
    #append to results
    monthTlms <- rbind(monthTlms, results_df)
  }
}
str(monthTlms)
# make cols that should be numeric numeric
monthTlms[, c(2:6,8)] <- sapply(monthTlms[,c(2:6,8)], as.numeric)
# add significance levels
monthTlms <- mutate(monthTlms, 
                 sig = ifelse(pval <= 0.001, "***",
                              ifelse(pval <= 0.01, "**",
                                     ifelse(pval <= 0.05, "*",
                                            ifelse(pval <= 0.1, ".", "n.s.")))))

# make monthly effects fig
monT_fig <- ggplot(monthTlms, aes(month, effect, col = local_site)) +
  geom_hline(aes(yintercept = 0)) +
  geom_errorbar(aes(ymax = effect + effect_se, ymin = effect - effect_se), width = 0.1, 
                position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(width = 1)) +
  # annotate significance
  geom_text(data = subset(monthTlms, sig != "n.s." & local_site == "C1"), 
            aes(month, effect+effect_se + 0.001, label = sig), show.legend = F, 
            position = position_dodge(width = 0.5, preserve = "single")) +
  geom_text(data = subset(monthTlms, sig != "n.s." & local_site == "D1"), 
            aes(month, effect+effect_se + 0.001, label = sig), show.legend = F, 
            position = position_dodge(width = -0.5, preserve = "single")) +
  scale_color_manual(name = "Site", values = site_cols) +
  scale_x_continuous(breaks = 1:12, labels = month_labels) +
  labs(y = "Tmax trend (°C/yr)",
       x = "Month") +
  theme(legend.position = c(0.9,0.8),
        legend.background = element_rect(color = "black"))


# -- ANNUAL TMAX FIG -----
# summarise on year
yrTmax <- monthTmax %>%
  group_by(local_site, year) %>%
  summarize(maxT = max(maxT))

# gather lm results
# initate yr df
yrTlms <- data.frame()
for(s in unique(yrTmax$local_site)){
  temp_lm <- lm(maxT ~ year, data = subset(yrTmax, local_site == s))
  results_df <- data.frame(cbind(local_site = s,
                                 adj.r2 = summary(temp_lm)$adj.r.squared,
                                 pval = summary(temp_lm)$coefficients[8],
                                 effect = summary(temp_lm)$coefficients[2],
                                 effect_se = summary(temp_lm)$coefficients[4],
                                 eq = paste0(round(summary(temp_lm)$coefficients[1],4), " + ", round(summary(temp_lm)$coefficients[2],4), "x"),
                                 nobs = nrow(temp_lm$model)))
  #append to results
  yrTlms <- rbind(yrTlms, results_df)
}
yrTlms[, c(2:5,7)] <- sapply(yrTlms[,c(2:5,7)], as.numeric)
# add significance levels
yrTlms <- mutate(yrTlms, 
                 sig = ifelse(pval <= 0.001, "***",
                              ifelse(pval <= 0.01, "**",
                                     ifelse(pval <= 0.05, "*",
                                            ifelse(pval <= 0.1, ".", "n.s.")))))

yrT_fig <- ggplot(yrTmax, aes(year, maxT, col = local_site)) +
  geom_line() +
  geom_point() +
  geom_smooth(aes(fill = local_site), method = "lm", show.legend = FALSE) + 
  # annotate c1 equation
  geom_text(data = subset(yrTlms, local_site == "C1"), 
            aes(2000, y = yrTmax$maxT[yrTmax$year == 2000 & yrTmax$local_site == "C1"] - 6, label = paste(eq, sig)),
            hjust = 0.22, family = "Times", size = plottext) +
  # annotate d1 equation
  geom_text(data = subset(yrTlms, local_site == "D1"), 
            aes(2000, y = yrTmax$maxT[yrTmax$year == 2000 & yrTmax$local_site == "D1"] - 2.5, label = paste(eq, sig)),
            hjust = 0.22, family = "Times", size = plottext) +
  scale_color_manual(name = "Site", values = site_cols) +
  scale_fill_manual(values = site_cols) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) + # 2010s still look low, even with adjustment...
  labs(y = "Annual Tmax (°C)",
       x = "Year") +
  theme(legend.position = "none")

# -- PPT FIGS -----
yrppt <- nwtppt %>%
  group_by(local_site, year) %>%
  summarise(ppt_tot = sum(precipitation.mm))

# gather lm results
# initate yr df
yrPlms <- data.frame()
for(s in unique(yrppt$local_site)){
  temp_lm <- lm(ppt_tot ~ year, data = subset(yrppt, local_site == s))
  results_df <- data.frame(cbind(local_site = s,
                                 adj.r2 = summary(temp_lm)$adj.r.squared,
                                 pval = summary(temp_lm)$coefficients[8],
                                 effect = summary(temp_lm)$coefficients[2],
                                 effect_se = summary(temp_lm)$coefficients[4],
                                 eq = paste0(round(summary(temp_lm)$coefficients[1],4), " + ", round(summary(temp_lm)$coefficients[2],4), "x"),
                                 nobs = nrow(temp_lm$model)))
  #append to results
  yrPlms <- rbind(yrPlms, results_df)
}
str(yrPlms)
yrPlms[, c(2:5,7)] <- sapply(yrPlms[,c(2:5,7)], as.numeric)
# add significance levels
yrPlms <- mutate(yrPlms, 
                 sig = ifelse(pval <= 0.001, "***",
                              ifelse(pval <= 0.01, "**",
                                     ifelse(pval <= 0.05, "*",
                                            ifelse(pval <= 0.1, ".", "n.s.")))))

yrlyppt_fig <- ggplot(yrppt, aes(year, ppt_tot, col = local_site)) +
  geom_line() +
  geom_point() +
  # d1 trend signif
  geom_smooth(data = subset(yrppt, local_site == "D1"), aes(fill = local_site), method = "lm", show.legend = FALSE) + 
  # c1 trend not
  geom_smooth(data = subset(yrppt, local_site == "C1"), aes(fill = local_site), method = "lm", show.legend = FALSE, lty = 2) + 
  geom_vline(aes(xintercept = 1976.5), lty = 3) +
  #geom_smooth(data = subset(yrppt, year < 1977), aes(year, ppt_tot, col = local_site, fill = local_site), method = "lm", show.legend = FALSE) + 
  #geom_smooth(data = subset(yrppt, year >= 1977), aes(year, ppt_tot, col = local_site, fill = local_site), method = "lm", show.legend = FALSE) + 
  geom_text(data = subset(yrPlms, local_site == "D1"), 
            aes(x = 1950, y = 1580, label = paste(eq, sig)), hjust = 0, family = "Times", size = plottext) +
  scale_color_manual(name = "Site", values = site_cols) +
  scale_fill_manual(values = site_cols) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  labs(x = "Year", y = "Total precipitation (mm/yr)") +
  theme(legend.position = "none")


# -- also make monthly trends fig to see in which part of yr ppt is increasing at d1 (or c1) -----
# calculate temporal trend in *monthly* max T
monthppt <- nwtppt %>%
  group_by(local_site, year, month) %>%
  summarize(ppt_tot = sum(precipitation.mm)) %>%
  ungroup() %>%
  mutate(month = as.factor(month))

# iterate through monthly lms and store results, by site
# initiate data frame for storing results
monthPlms <- data.frame()
for(s in unique(monthppt$local_site)){
  temp_df <- subset(monthppt, local_site == s)
  # iterate through months
  for(m in 1:12){
    temp_lm <- lm(ppt_tot ~ year, data = subset(temp_df, month == m))
    results_df <- data.frame(cbind(local_site = s,
                                   month = m,
                                   adj.r2 = summary(temp_lm)$adj.r.squared,
                                   pval = summary(temp_lm)$coefficients[8],
                                   effect = summary(temp_lm)$coefficients[2],
                                   effect_se = summary(temp_lm)$coefficients[4],
                                   eq = paste0(round(summary(temp_lm)$coefficients[1],4), " + ", round(summary(temp_lm)$coefficients[2],4), "x"),
                                   nobs = nrow(temp_lm$model)))
    #append to results
    monthPlms <- rbind(monthPlms, results_df)
  }
}
str(monthPlms)
# make cols that should be numeric numeric
monthPlms[, c(2:6,8)] <- sapply(monthPlms[,c(2:6,8)], as.numeric)
# add significance levels
monthPlms <- mutate(monthPlms, 
                    sig = ifelse(pval <= 0.001, "***",
                                 ifelse(pval <= 0.01, "**",
                                        ifelse(pval <= 0.05, "*",
                                               ifelse(pval <= 0.1, ".", "n.s.")))))


# make monthly effects fig
monppt_fig <- ggplot(monthPlms, aes(month, effect, col = local_site)) +
  geom_hline(aes(yintercept = 0)) +
  geom_errorbar(aes(ymax = effect + effect_se, ymin = effect - effect_se), width = 0.1, 
                position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(width = 1)) +
  # annotate significance
  geom_text(data = subset(monthPlms, sig != "n.s." & local_site == "C1"), 
            aes(month, effect+effect_se + 0.01, label = sig), show.legend = F, 
            position = position_dodge(width = 0.5, preserve = "single")) +
  geom_text(data = subset(monthPlms, sig != "n.s." & local_site == "D1"), 
            aes(month, effect+effect_se + 0.01, label = sig), show.legend = F, 
            position = position_dodge(width = -0.5, preserve = "single")) +
  scale_color_manual(name = "Site", values = site_cols) +
  scale_x_continuous(breaks = 1:12, labels = month_labels) +
  #scale_shape_manual(values = site_pch) +
  labs(y = "Precipitation trend (mm/yr)",
       x = "Month") +
  theme(legend.position = "none")


# -- PANEL PLOT ALL -----
panel_fig <- plot_grid(yrT_fig, monT_fig, yrlyppt_fig, monppt_fig,
          nrow = 2, ncol = 2,
          align = "vh")
ggsave("climate_d1_c1/figures/climate_trends_1952-2018.pdf", panel_fig,
       width = 6, height = 4, units = "in", scale = 1.5)
