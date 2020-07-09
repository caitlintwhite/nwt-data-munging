# prep codom for FI calculations in python
# caitlin.t.white@colorado.edu
# july 2020

# script purpose:
# for kns proposal.. prep sppcomp data per kns instructions
# 1) read in nwt lter codom dataset dynamically from EDI
# 2) instructions:
# I think 4 year and 8 year windows would be great. One year increments. 
# I understand these better than the size of state, so I think the default is best for now on that one.
# KNS: Agree with removing rare species and unknowns, based on how FI is calculated, 
# KNS: I might make this plot-based, and remove species from analysis if they are absent from a plot >50% of the years. 
# >> CTW: check to see what is avg amt spp appears in a given plot
# KNS: If that sounds ridiculous, a removal cut off less than 1% average in a plot.
# >> CTW: be sure to check for #hits per point
# KNS: I’d like all treatment/plots if possible. I want to look at 4 treatments — xx xn  dn an — in particular, as it seems as if the removals cause differing dynamics in response to added N. 
# I don’t have any plans to analyze site by site, but it’s helpful to run for each plot/site to get some idea of variance within a trt . 
# Plot level calcs not needed though if it adds work for you. 
# Drop the B treatment and careful with site: use the number codes as there should be 7 blocks, not 6 (the edge site has blocks 2 and 3). 
# KSN: Remove rock bare, keep all growth forms. Keep in des and geum even though we removed one some trts. Geum = acomastylis.

# steps:
# > remove rare, remove unknowns (unless consistently there), remove bare + rock, sum all hits, then re-relativize data
# > write out by plot, with site and full treatment in filename for post-python compilation
# > spp = columns, years = rows (IN ORDER!)
# > keep an eye out for plots that were not sampled in all years.. (not sure what python script will do if NA's?-- but should assign NA's for that year)



# notes:
# EDI data package is knb-nwt-lter-6 (currently v2 at time of script)



# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)
theme_set(theme_bw())
source("edi_functions.R")

# read in plant dat
codom <- getTabular2(edi_id = 6)
glimpse(codom)
summary(codom)
sapply(select(codom, year, site:spp, growth_habit, hits), function(x) sort(unique(x)))



# -- SCREEN AND PREP DATA -----
# look at avg presence per species (is 50% reasonable cutoff?)
# remove rare (< 50% in plot all years  or < 1% cover in plot)
# check unknowns (how commonly recorded per plot)
# remove rock, bare, litter.. keep all else (e.g. club moss, moss moss, lichens, shrubs + grams and forbs)

# who are the unknowns?
sort(unique(with(codom, USDA_code[grepl("^2", USDA_code)])))
# 2GENT = Gentian fam sp., 2GP = peren gram
# will want to keep lichen and moss..
removeusda <- c("2BARE", "2RF", "2LTR")

# how many dates per year?
sapply(split(codom$date, codom$year), unique) #hm..
# examine dates per plot per year
unite(codom, plotid, site, plot, remove = F) %>%
  distinct(plotid, plot, year, date, site) %>%
  group_by(plot, year, site) %>%
  summarise(visits = length(date)) %>%
  ungroup() %>%
  ggplot(aes(year, plot, col = as.factor(visits))) +
  geom_point() +
  facet_wrap(~ site, scales = "free_x")
# only 1 visit per plot per year.. carbon trts end after 2015

# are hits already summed per spp? (each spp will have 1 obs per plot if present)
unite(codom, plotid, site, plot, remove = F) %>%
  subset(!USDA_code %in% removespp) %>%
  distinct(plotid, plot, year, spp, site, hits) %>%
  group_by(plot, year, site, spp) %>%
  summarise(nobs = length(hits)) %>%
  ungroup() %>%
  ggplot(aes(year, spp, col = nobs)) +
  geom_point() +
  theme(axis.text.y = element_blank()) +
  facet_grid(plot ~ site, scales = "free_x") # hits already summed.. only 1 observation per spp per plot
# most spp in a plot seem to appear fairly regularly (peren, long-lived spp system, so that is what's expected..)
# looks like 2CAN didn't get sampled one year..


# check temporal frequencies per spp per plot (i.e. are most spp there 50% of time?)
# need to keep bare, rock, and litter to see if any spp accidentally entered in those rows or vv
spptempfreq <- unite(codom, plotid, site, plot, remove = F) %>%
  #subset(!USDA_code %in% removespp) %>%
  group_by(plotid) %>%
  mutate(yrs = length(unique(year))) %>%
  ungroup() %>%
  group_by(site, plot, plotid, yrs, spp) %>%
  summarise(nobs = length(hits),
            meanhits = mean(hits),
            maxhits = max(hits),
            sumhits = sum(hits)) %>%
  ungroup() %>%
  mutate(relfreq = (nobs/yrs)*100)

# look at distribution
ggplot(spptempfreq) +
  geom_boxplot(aes(site, relfreq, group = site)) +
  geom_jitter(aes(site, relfreq, group = site, col = nobs), width = 0.2) +
  facet_wrap(~plot) # maybe cutoff at 25% if lose too much info with 50% cutoff?

# what is the mean # hits by relfreq? ()
gather(spptempfreq, met, val, meanhits:sumhits) %>%
  ggplot() +
  geom_vline(aes(xintercept = 50), col = "red", lty = 2) +
  geom_point(aes(round(relfreq,1), val, col = spp %in% c("litter", "rock", "bare"))) +
  scale_color_manual(values = c("TRUE" = "pink", "FALSE" = "grey10")) +
  scale_x_continuous(breaks = seq(20,100, 20)) +
  labs(x = "Relative temporal frequency (point = species-plot)",
       y = "Plot hits summary value",
  title = "NWT Codom: species max hits (per plot-yr), mean hits (per plot-yr),\ntotal hits (all years) by plot temporal rel. frequency") +
  facet_grid(met ~ ., scales = "free_y")


View(subset(spptempfreq, sumhits == 0.5))
View(subset(spptempfreq, sumhits > 0.5))

# examples of lewpyg-litter mixups:
## > 5 CBX in 2004
## > 7 CXX in 2006


# some logic checks that would be good to do..
# check for interannual spikes in richness
# check for interannual spikes in spp hits
# check for interannual spikes in spp alpha-pairs?
# check for spp that might get mis-ID'd depending on time of season sampled? (per JGS)

# .. spread out spp to infill 0s across all plots, all years; gather and diff interannual vals; flag cutoff or deviation from some spp-plot stat (e.g. median or modal value?)
sppcheck <- select(codom, LTER_site:spp, hits) %>%
  data.frame() %>%
  spread(spp, hits, fill = 0) %>%
  gather(spp, hits, names(.)[grep("nut", names(.))+1]:ncol(.)) %>%
  unite(plotid, site, plot, remove = F) %>%
  arrange(plotid, spp, year) %>%
  group_by(plotid, spp) %>%
  mutate(diffhits = hits - lag(hits),
         flagdiff = abs(diffhits) > 10) %>%
  ungroup()

sppreview <- subset(sppcheck)


# -- COMPILE FI SCORES -----