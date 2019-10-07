# replicate TS analyses
# > *to the extent possible with data available (cover data)


# output (things to replicate):
# Figs 1-2 (Saddle forb vs. grass cover by treatment by year, col plots with SEs)
# Parts of Tables 1-2 (NutNet by year [2013 = all 40 plots, 2017 = 28 plots], richness? entropy? plant cover and hits for sure)



# notes:
# SADDLE
# only 16 dry meadow plots consistently sampled across all three years
# plant cover = % (FIRST hits that were vegetation/ all FIRST hits) [veg = not litter, rock or bare ground]
# rel plant cov = % (number of hits of single species/total vascaular vegetation hits) <-- is this top or all hits?
# 1997 only had top hits; 2012 and 2016 allowed multiple hits per points
# > stat tests:
# > full factorial, randomized, no blocks

# NUTNET
# +P and +P+K not sampled in 2017
# 0.25 cover = present in 1m^2 plot but not hit
# ran Shannon Div on NutNet (including species present but not hit)
# 2 reps of C and +N+P+K so averaged those per block for subsequent calculations
# > stat tests:
# > 2013: all 40 plots, full factorial analysis of N, P and micro effects, randomized + blocked
# > 2017: only 28 plots sampled, one-way ANOVA to eval +N, +K, +N+P+K effects, randomized + blocked



# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(vegan)
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", NA, "NA", "NaN", NaN, ".")


# get data
# plant community data for sdl and nutnet, all yrs
plantcom <- read.csv("alpine_addnuts/output_data/sdl_nutnet_plantcom_allyrs.csv") 
# spp lookup table
spplist <- read.csv("alpine_addnuts/output_data/sdl_nutnet_spplookup.csv")
# sdl site info
sdlplots <- read.csv("alpine_addnuts/output_data/sdl_plot_lookup.csv") 
nnplots <- read.csv("alpine_addnuts/output_data/nutnet_plot_lookup.csv")

# full sdl site info table (troublshooting discrepancies, but good to use for plots 1-16 sampled in 1997-2016 in sdl)
sdlplots_full <- read.csv("alpine_addnuts/output_data/sdl_plots_lookup_trblshoot.csv")


# read in prepped datasets where can pull top hits only
nn17_wvert <- read.csv("alpine_addnuts/output_data/nutnet2017_vertical_sppcomp.csv") 
sdl16_wvert <- read.csv("alpine_addnuts/output_data/sdl2016_vertical_sppcomp.csv") 



# -- RECODE TRTMENTS FOR NUTNET (SIMPLIFY) -----
#ts found no effect of +k, condense +k into other treatments (e.g. n+p+k to n+p; k to control)
# in previous ordination analysis, k plots overlay similar treatments without k (e.g. n+p and n+p+k hulls overlap on nmds) 
nnplots$trt2 <- gsub("[+]K", "", nnplots$trt)
nnplots$trt2 <- gsub("K", "C", nnplots$trt2)

# make trts factor in each site
#nnplots$trt <- factor(nnplots$trt, levels = c(c("C", "K", "N", "P", "P+K", "N+K", "N+P", "N+P+K")))
#nnplots$trt2 <- factor(nnplots$trt2, levels = c("C", "N", "P", "N+P"))
sdlplots$trt <- factor(sdlplots$trt, levels = c("C", "N", "P", "N+P"))



# -- ADD SIMPLE LIFEFORM TO SPPLIST ----
#make simple lifeform grp
spplist <- spplist %>%
  mutate(simple_lifeform = ifelse(Family == "Fabaceae", "N-fixer",
                                  ifelse(grepl("Shrub", Growth_Habit), "Shrub",
                                         ifelse(Growth_Habit == "Graminoid", "Grass", "Forb")))) %>%
  # change Dryas to forb from shrub (is listed as subshrub/shrub/forb)
  mutate(simple_lifeform = ifelse(simple_name == "Dryas octopetala", "Forb", simple_lifeform)) %>%
  # fill in simple lifeform for 2FORB and 2GRAM
  mutate(simple_lifeform = ifelse(clean_code2 == "2FORB", "Forb",
                                  ifelse(clean_code2 == "2GRAM", "Grass", simple_lifeform))) %>%
  # add alternative lifeform group where N-fixer lumped into forb group
  mutate(simple_lifeform2 = ifelse(simple_lifeform == "N-fixer", "Forb", simple_lifeform),
         # add nutnet grouping (Selaginella densa [spikemoss] = ground cover, not forb)
         nutnet_grp = ifelse(clean_code2 == "SEDES", "Ground cover", simple_lifeform2),
         nutnet_grp = ifelse(grepl("^2", clean_code2) & is.na(nutnet_grp), "Ground cover", simple_lifeform2))



# -- Table 3: Relative cover of G. rossii by study by year ----
# rel cov = number hits per species/total number of veg hits in the plot
geum_relcov <- plantcom %>%
  #filter(!grepl("2BA|2LT|2RF", clean_code2)) %>%
  #filter(!clean_code2 %in% unique(spplist$clean_code2[spplist$nutnet_grp == "Ground cover"])) %>%
  filter(clean_code2 %in% sort(unique(spplist$clean_code2[!is.na(spplist$simple_lifeform2)]))) %>%
  # sum total veg hits per site per plot per yr
  group_by(site, yr, plotid) %>%
  mutate(totveg = sum(hits)) %>%
  ungroup() %>%
  # spread out species to fill in 0s where not hit
  spread(clean_code2, hits, fill = 0) %>%
  gather(clean_code2, hits, '2FORB':ncol(.)) %>%
  # subset to geum rossii only
  filter(clean_code2 == "GERO2") %>%
  mutate(relcov = (hits/totveg)*100)

# split data frame into sdl and nutnet, since treated a little differently in prep
sdl_geum <- subset(geum_relcov, site == "sdl") %>%
  # subset to dry meadow plots surveyed across all years only (corresponds to what was surveyed in 1997)
  ## should be plots 1-16
  subset(plotid %in% unique(sdlplots$plot[!is.na(sdlplots$old_plot) & sdlplots$meadow == "Dry"])) %>%
  # join site info
  ## convert plotid from char to numeric to join with sdlplots df (nutnet has alpha chars in plotid)
  mutate(plotid = as.numeric(plotid)) %>%
  left_join(unique(sdlplots[c("plot", "trt")]), by = c("plotid" = "plot"))

# visualize to check what to expect
ggplot(sdl_geum, aes(trt, relcov)) +
  stat_summary() +
  facet_wrap(~yr) # doesn't match tim's.. 1997 N and N+P looks greater here, other yrs could discrep bc summarized on all hits (not just top) 

# calculate table of means and ses
sdl_geum_means <- data.frame(sdl_geum) %>%
  group_by(site, yr, trt) %>%
  summarise(meancov = mean(relcov),
            secov = sd(relcov)/sqrt(length(relcov)),
            nobs = length(relcov)) %>%
  # calculate yrs passed since start of exp
  mutate(post_yrs = yr - 1993) %>%
  as.data.frame()
sdl_geum_means


# nutnet
## need to average N+P and C by block first (2 reps per block)
nn_geum <- subset(geum_relcov, site == "nutnet") %>%
  # join site data
  full_join(nnplots) %>%
  # recode 2017 N+K as N.. (so have 4 N trt plots, as is have 3 N and 1 N+K)
  mutate(trt = ifelse(yr == 2017 & trt == "N+K", "N", trt)) %>%
  # average reps in C and N+P+K by block
  group_by(site, yr, block, trt) %>%
  mutate(relcov2 = mean(relcov),
         # to verify 2 obs per C and N+P+K per block
         nobs = length(relcov)) %>% # yes (checked manually)
  ungroup()


# visualize to check what to expect
ggplot(subset(nn_geum, trt %in% c("C", "N", "N+P", "P")), aes(trt, relcov2)) +
  stat_summary() +
  facet_wrap(~yr) # doesn't match tim's.. 1997 N and N+P looks greater here, other yrs could discrep bc summarized on all hits (not just top) 

# calculate table of means and ses
nn_geum_means <- data.frame(nn_geum) %>%
  dplyr::select(site, yr, block, trt, relcov2) %>%
  distinct() %>%
  group_by(site, yr, trt) %>%
  summarise(meancov = mean(relcov2),
            secov = sd(relcov2)/sqrt(length(relcov2)),
            nobs = length(relcov2)) %>%
  ungroup() %>%
  # calculate yrs passed since start of exp
  mutate(post_yrs = yr - 2008) %>%
  as.data.frame()
nn_geum_means


# stack sdl and nutnet to plot means and ses by yr since trts initiated
all_geum_means <- rbind(sdl_geum_means, nn_geum_means) %>%
  # keep only trts of interest
  subset(trt %in% c("C", "N", "P", "N+P"))
  

# enter sdl data for 1994 and 2005
sdl_geum_9405 <- data.frame(cbind(site = "sdl",
                                  trt = rep(c("C", "N", "P", "N+P"), 2),
                                  yr = c(rep(1994,4), rep(2005,4)),
                                  meancov = c(3.1, 3.3, 2.0, 3.8, 
                                              9.8, 11.5, 4.5, 14.2),
                                  secov = c(0.26, 1.22, 0.98, 1.16,
                                            2.09, 2.95, 1.93, 5.23),
                                  nobs = NA)) %>%
  mutate(yr = as.numeric(yr),
         post_yrs = yr-1993)
                              
all_geum_means <- rbind(all_geum_means, sdl_geum_9405) %>%
  # convert trt to factor
  mutate(trt = factor(trt, levels = c("C", "N", "P", "N+P"))) %>%
  mutate_at(vars("meancov", "secov", "nobs"), as.numeric)

# plot means by time since experiment onset
ggplot(all_means, aes(post_yrs, meancov, group = site, col = site)) +
  geom_errorbar(aes(ymax = meancov + secov, ymin = meancov - secov), width = 0) +
  geom_point(aes(col = site)) +
  #geom_line(aes(col = site)) +
  scale_color_manual(name = "Site", values = c("salmon", "darkred")) +
  labs(y = "Geum rossi mean relative cover (%)",
       x = "Years since experiment onset") +
  facet_wrap(~trt, nrow = 1)



# -- ABUNDANT FORBS IN NUTNET 2017 (Table 4) -----


# -- FORBS VS GRASSES (Figs 1 + 2) ----
# (out of curiosity make similar time since exp onset plot to compare forb shift over time by site by trt)
fgdat <- plantcom %>%
  left_join(distinct(spplist[c("clean_code2", "simple_lifeform2")])) %>%
  subset(!is.na(simple_lifeform2)) %>%
  group_by(site, yr, plotid, simple_lifeform2) %>%
  summarise(hits = sum(hits),
            sppR = length(clean_code2)) %>%
  ungroup() %>%
  # add total hits col
  group_by(site, yr, plotid) %>%
  mutate(tothits = sum(hits)) %>%
  ungroup() %>%
  #calculate relcov
  mutate(relcov = (hits/tothits)*100)

# split out sdl and nutnet so can average C and N+P+K
sdl_fg <- subset(fgdat, site == "sdl") %>%
  # select only dry meadow plots consistently sampled
  # subset to dry meadow plots surveyed across all years only (corresponds to what was surveyed in 1997)
  ## should be plots 1-16
  subset(plotid %in% unique(sdlplots$plot[!is.na(sdlplots$old_plot) & sdlplots$meadow == "Dry"])) %>%
  # drop any shrubs (only in 2 rows, <1% relcov)
  filter(simple_lifeform2 != "Shrub") %>%
  # join site info
  ## convert plotid from char to numeric to join with sdlplots df (nutnet has alpha chars in plotid)
  mutate(plotid = as.numeric(plotid)) %>%
  left_join(unique(sdlplots[c("plot", "trt")]), by = c("plotid" = "plot"))

# visualize to check what to expect
ggplot(sdl_fg, aes(yr, relcov, col = simple_lifeform2)) +
  stat_summary() +
  facet_wrap(~trt)

sdl_fg_means <- data.frame(sdl_fg) %>%
  group_by(site, yr, trt, simple_lifeform2) %>%
  summarise(meancov = mean(relcov),
            secov = sd(relcov)/sqrt(length(relcov)),
            nobs = length(relcov)) %>%
  # calculate yrs passed since start of exp
  mutate(post_yrs = yr - 1993) %>%
  as.data.frame()
sdl_fg_means


# nutnet
## need to average N+P and C by block first (2 reps per block)
nn_fg <- subset(fgdat, site == "nutnet") %>%
  # join site data
  full_join(nnplots) %>%
  # recode 2017 N+K as N.. (so have 4 N trt plots, as is have 3 N and 1 N+K)
  mutate(trt = ifelse(yr == 2017 & trt == "N+K", "N", trt)) %>%
  # average reps in C and N+P+K by block
  group_by(site, yr, block, trt, simple_lifeform2) %>%
  mutate(relcov2 = mean(relcov),
         # to verify 2 obs per C and N+P+K per block
         nobs = length(relcov)) %>% # yes (checked manually)
  ungroup()


# visualize to check what to expect
ggplot(subset(nn_fg, trt %in% c("C", "N", "N+P", "P")), aes(trt, relcov2, col = simple_lifeform2)) +
  stat_summary() +
  facet_wrap(~yr)

# calculate table of means and ses
nn_fg_means <- data.frame(nn_fg) %>%
  dplyr::select(site, yr, block, trt, simple_lifeform2, relcov2) %>%
  distinct() %>%
  group_by(site, yr, trt, simple_lifeform2) %>%
  summarise(meancov = mean(relcov2),
            secov = sd(relcov2)/sqrt(length(relcov2)),
            nobs = length(relcov2)) %>%
  ungroup() %>%
  # calculate yrs passed since start of exp
  mutate(post_yrs = yr - 2008) %>%
  as.data.frame()
nn_fg_means


# stack sdl and nutnet to plot means and ses by yr since trts initiated
all_fg_means <- rbind(sdl_fg_means, nn_fg_means) %>%
  # keep only trts of interest
  subset(trt %in% c("C", "N", "P", "N+P")) %>%
  mutate_at(vars("meancov", "secov", "nobs"), as.numeric)


# plot means by time since experiment onset
ggplot(subset(all_fg_means, simple_lifeform2 == "Forb"), aes(post_yrs, meancov, group = site, col = site)) +
#ggplot(all_fg_means, aes(post_yrs, meancov, group = site, fill = simple_lifeform2)) +
  geom_errorbar(aes(ymax = meancov + secov, ymin = meancov - secov), width = 0) +
  geom_point() +
  #geom_line(aes(col = site)) +
  scale_color_manual(name = "Site", values = c("salmon", "darkred")) +
  labs(y = "Forb mean relative cover (%)",
       x = "Years since experiment onset") +
  scale_y_continuous(breaks = seq(20,80, 10)) +
  facet_grid(.~trt)
