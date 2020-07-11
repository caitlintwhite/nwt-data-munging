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


# spp comp methods from EDI:
# Plant species composition in each plot was measured annually at peak biomass, using a point-intercept method with a 100-point grid. 
# At each of the 100 grid points, the uppermost plant present was recorded as being hit. 
# Plants found present in the plot without being hit were given a hit value of 0.5. 
# Non-plant hits included bare soil surface or small gravely rock (bare), lichen growing on soil (lichen), and rocks or rock fragments at least 10 cm in diameter or generally under which plant growth is unlikely (rock).
# ‘Litter’ refers to dead plant material that was never alive during the current growing season. 
# When litter was the uppermost hit at a point but live plant material was intercepted lower down, hits were recorded as the live species.

# > CTW: total hits should not sum much over 100 per plot (may if many plants present not hit)
# > litter, and non-veg only recorded if no live, current-season plant below
# > present not hit = 0.5

# target removal moethds from EDI:
# Plots were visited one to three times per season to implement removal treatments.
# Codominant experimental plots – In targeted species removal plots, all aboveground biomass of G. rossii (CA-) or D. cespitosa (CD-) was removed. 
# In random removal plots (CBX), the random biomass removal protocol (Appendix 1) was followed to remove a similar amount of biomass of non-targeted species.
# As the amount of G. rossii and D. cespitosa removed from plots diminished with time, random biomass removal was similarly diminished until eventually just one individual plant was removed at each of three haphazardly selected points inside the plot.





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

# pull out sppLUT for convenience
spplut <- distinct(codom, spp, USDA_code, USDA_name, growth_habit) %>%
  # infill litter, rock and bare as ground cover
  replace_na(list(growth_habit = "ground cover")) %>%
  arrange(spp)


# also plot the things that only occur 1 time with high hits along with whatever occurs next to them alphabetically



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
         # flag anything that's one time only
         onetime = length(year[hits>0]) == 1 & length(hits[!hits %in% c(0,0.5,1)]) == 1,
         twotime = length(year[hits>0]) == 2 & length(hits[!hits %in% c(0,0.5,1)]) == 1) %>%
  ungroup() %>%
  mutate(flagdiff = abs(diffhits) >= 10) %>%
  group_by(plotid, spp) %>%
  mutate(checkspp = any(flagdiff, na.rm = T)) %>%
  ungroup()
  

# calculate veg, moss and lichen total hits as second check (e.g. if spp flux, does overall green cover stay the same?)
greenhits <- subset(sppcheck, !spp %in% c("litter", "rock", "bare")) %>%
  group_by(site, plot, plotid, year) %>%
  summarise(tothits = sum(hits)) %>%
  ungroup

# redo with codom orig to be sure can sum > 100
greenhits2 <- subset(codom, !spp %in% c("litter", "rock", "bare")) %>%
  distinct() %>%
  group_by(site, plot, year) %>%
  summarise(tothits = sum(hits)) %>%
  ungroup

sort(unique(greenhits2$year[greenhits2$tothits > 105])) # only 2002 and 2005 have over 110 cover
length(unique(greenhits$plotid[greenhits$year == 2002]))
length(unique(greenhits$plotid[greenhits$year == 2005]))

# try remove all spp present and see what sums are
greenhits3 <- subset(codom, !spp %in% c("litter", "rock", "bare")) %>%
  subset(hits != 0.5) %>%
  group_by(site, plot, year) %>%
  summarise(tothits = sum(hits)) %>%
  ungroup
# only +100 sum (with spp present removed) in 2011, 2016, 2018 (only three plots--1 per yr..)

# is richness in 2002 and 2005 fairly consistent with other years?
S <- subset(sppcheck, hits > 0) %>%
  distinct(site, plot, plotid, year, spp) %>%
  group_by(site, plot, plotid, year) %>%
  summarise(S = length(unique(spp))) %>%
  ungroup()

# plot for swings
ggplot(S, aes(year, S, col = plot, group = plotid)) +
  geom_line() +
  facet_wrap(~site, scales = "free_y")


# > notes:
# > looked at rawdat for 2002 and 2005.. hi hits are real
# > in 2005, multiple hits recorded (in IA's methods)
# > not sure why 2002 high (i.e. if multiple hits done yr1 .. KNS would have collected data then?), but KNS says to relativize counts in those years
# > 2002 hi counts are all in all types of treatment plots (-A, -D, and X)
# > To Do: relativize non-present-only veg, lichen and moss in 2002 and 2005 and add back in then re-crunch flagging

# clean up env
rm(greenhits, greenhits2, greenhits3, S)



# 1) TREAT 2002, 2005 DAT -----
# check that total cover in other years is around 100 (i.e. should i relativize just veg/moss/lichen or all [litter, bare, rock too]?)
totcovcheck <- mutate(codom, growth_habit = ifelse(USDA_code %in% removeusda, "ground cover", growth_habit)) %>%
  unite(plotid, site, plot, remove = F) %>%
  group_by(year, site, plot, plotid, growth_habit) %>%
  summarize(tothits = sum(hits)) %>%
  ungroup() %>%
  spread(growth_habit, tothits, fill = 0) %>%
  mutate(greenhits = apply(.[grepl("forb|gram|shrub|lich|nonvas|lycop", names(.))], 1, sum),
         allhits = apply(.[grepl("forb|gram|shrub|ground|lich|nonvas|lycop", names(.))], 1, sum))

summary(as.factor(totcovcheck$year))
sort(sapply(split(totcovcheck$year, totcovcheck$plotid), length)) #2 CAN missing a year
unique(totcovcheck$year[totcovcheck$plotid == "2_CAN"]) # ya, 2013.

# lbh curious about <100 tothits..
# who does it affect?
ggplot(subset(totcovcheck, allhits < 100), aes(year, allhits, col = site)) +
  geom_point() +
  facet_wrap(~plot)
# let's say ppl forgot to do a few points or even a row..
ggplot(subset(totcovcheck, allhits < 95), aes(as.factor(site), allhits, col = as.factor(year))) +
  geom_point() +
  facet_wrap(~plot, scales = "free_x")

sppreview <- subset(sppcheck, checkspp) %>%
  # remove nonveg, descae, acoros (and maybe also both plots since those are random removals)
  filter(!spp %in% c("rock", "bare", "litter", "GEUROS", "DESCAE"))


# screen by treatment, because might make sense that certain spp flux in target removal treatments
# control plots should not flux quite as much (esp XX)



ggplot(subset(sppreview, rem == "A"), aes(year, diffhits, col = as.factor(site), group = plotid)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line() +
  facet_wrap(~spp+plot, scales = "free_y")

ggplot(subset(sppreview, rem == "D"), aes(year, diffhits, col = as.factor(site), group = plotid)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line() +
  facet_wrap(~spp+plot, scales = "free_y")

ggplot(subset(sppreview, rem == "X"), aes(year, diffhits, col = as.factor(site), group = plotid)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line() +
  facet_wrap(~spp+plot) #, scales = "free_y"

ggplot(subset(sppreview, rem == "X"), aes(year, diffhits, col = spp, group = spp)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line() +
  facet_wrap(~site+plot, scales = "free_y")


# look at one time only spp -- or 2 if not consecutive years?
infrequent <- sppreview





# -- COMPILE FI SCORES -----