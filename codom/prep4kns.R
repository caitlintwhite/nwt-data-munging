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
library(readxl)
options(stringsAsFactors = F)
theme_set(theme_bw())
source("edi_functions.R")
na_vals <- c(".", " ", "", NA, "NA", NaN, "NaN")
  
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


# read in JGS codom rawdat files (master)
rawdat_all <- read_excel("/Users/scarlet/Documents/nwt_lter/unpub_data/codom/NWT_CoDom_SpComp_data_L0.xlsx", na = na_vals, trim_ws = T)
str(rawdat_all)
summary(rawdat_all)
# what is the NA row?
View(subset(rawdat_all, is.na(CARHET))) #hm.. this is the missing plot (2CAN 2013), but it DOES have some zeros for 3 species.. ?

# only 2014 bc that has different tallies than master
raw2014 <- read_excel("/Users/scarlet/Documents/nwt_lter/unpub_data/codom/codom_yearly_sp_comp_raw_data/Niwot_CoDom_2014_SpComp.xlsx", na = na_vals, trim_ws = T, skip = 10)
str(raw2014)

# read in 2002-2005 raw dat to be sure..
earlydat <- read_excel("/Users/scarlet/Documents/nwt_lter/unpub_data/codom/codom2002_2005/SpeciesCompMaster2002-2005.xls",
                       na = na_vals, trim_ws = T, col_names = F) %>%
  data.frame()


# also plot the things that only occur 1 time with high hits along with whatever occurs next to them alphabetically



# -- SCREEN AND PREP RAW DATA -----
# JGS master file
# check first if this file will solve some of the sub 100 plots in 2003 and 2014
sumcheck <- apply(rawdat_all[,grep("ACHMIL", names(rawdat_all)):ncol(rawdat_all)],1, function(x) sum(x, na.rm = T))
View(rawdat_all[sumcheck< 98,])
sumcheck[sumcheck < 100]
boxplot(sumcheck[sumcheck < 98]) #hm..


# IA early dat raw file
# first two row = headers
IAspp <- earlydat[1:2,]
IAspp <- paste(earlydat[1,], earlydat[2,], sep = "_")
# assign row 3 to first 3 slots
IAspp[1:3] <- as.character(unname(earlydat[3,][1:3]))
IAspp <- gsub("_NA", "", IAspp)
IAspp <- gsub(" ", "_", IAspp)

names(earlydat) <- IAspp
earlydat <- earlydat[4:nrow(earlydat),]
earlydat[,4:ncol(earlydat)] <- sapply(earlydat[,4:ncol(earlydat)], function(x) ifelse(grepl("^P", x), 0.5, x))
sapply(earlydat[,4:ncol(earlydat)], unique)
sapply(earlydat[,4:ncol(earlydat)], function(x) any(grepl("[[:alpha:]]", x)))
earlydat2 <- earlydat
earlydat2[,4:ncol(earlydat)] <- sapply(earlydat[,4:ncol(earlydat)], as.numeric)
sumcheck2 <- apply(earlydat2[,4:ncol(earlydat2)], 1, function(x) sum(x, na.rm = T))
View(earlydat2[sumcheck2 < 100,])
View(earlydat[sumcheck2 < 100,])
apply(earlydat[sumcheck2 < 100,4:ncol(earlydat)], 1, function(x) sort(unique(x))) #hm.. true low values.. 56 of 70 plots in 2003.. 







# -- SCREEN AND PREP CURRENT DATA -----
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
  subset(!USDA_code %in% removeusda) %>%
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




# check that total cover in other years is around 100 (i.e. should i relativize just veg/moss/lichen or all [litter, bare, rock too]?)
totcovcheck <- mutate(codom, growth_habit = ifelse(USDA_code %in% removeusda, "ground cover", growth_habit)) %>%
  unite(plotid, site, plot, remove = F) %>%
  group_by(year, site, plot, plotid, growth_habit) %>%
  summarize(tothits = sum(hits)) %>%
  ungroup() %>%
  spread(growth_habit, tothits, fill = 0) %>%
  mutate(greenhits = apply(.[grepl("forb|gram|shrub|lich|nonvas|lycop", names(.))], 1, sum),
         allhits = apply(.[grepl("forb|gram|shrub|ground|lich|nonvas|lycop", names(.))], 1, sum)) %>%
  # join with JGS rawdat all hits to compare
  left_join(data.frame(cbind(rawdat_all[c("year", "site", "plot")], sumcheck)))

summary(totcovcheck$allhits == totcovcheck$sumcheck) # yup, all the same.. nothing to do about hi or low total hits values other than to know about them

summary(as.factor(totcovcheck$year))
sort(sapply(split(totcovcheck$year, totcovcheck$plotid), length)) #2 CAN missing a year
unique(totcovcheck$year[totcovcheck$plotid == "2_CAN"]) # ya, 2013.

# lb curious about <100 tothits..
# who does it affect?
ggplot(subset(totcovcheck, allhits < 100), aes(year, allhits, col = site)) +
  geom_point() +
  facet_wrap(~plot)
# let's say ppl forgot to do a few points or even a row..
ggplot(subset(totcovcheck, allhits < 95), aes(as.factor(site), allhits, col = as.factor(year))) +
  geom_point() +
  facet_wrap(~plot, scales = "free_x")

# plot tothits to see if can see when all hits vs top hits were used
group_by(totcovcheck, plotid) %>%
  mutate(avghits = mean(allhits)) %>%
  ungroup() %>%
  ggplot(aes(year, allhits, group = plot, col = plot)) +
  geom_hline(aes(yintercept = 100), col = "grey20", lty = 2) +
  geom_hline(aes(yintercept = avghits)) +
  geom_line() +
  facet_grid(plot~site, scales = "free_y")


# clean up env
rm(greenhits, greenhits2, greenhits3, S)



# -- CORRECT 2014 LOW HITS DATA ----
# treat 2014 dat
raw2014_tidy <- gather(raw2014, spp, hits, 4:`unk forb`) %>%
  subset(!is.na(hits)) %>%
  # check total hits
  group_by(Site, Plot) %>%
  mutate(tot2 = sum(hits)) %>%
  ungroup() %>%
  mutate(sumcheck = Total == tot2)

summary(raw2014_tidy$sumcheck) # great

raw2014_tidy <- dplyr::select(raw2014_tidy, -c(Total, tot2, Notes, sumcheck)) %>%
  rename_all(function(x) casefold(x))
# clean up codes
summary(unique(raw2014_tidy$spp) %in% spplut$spp)
unique(raw2014_tidy$spp)[!unique(raw2014_tidy$spp) %in% spplut$spp]
# what spp are in the edi dataset?

# do achlan check with achmil
achmil2014 <- left_join(subset(raw2014_tidy, spp == "ACHLAN"), subset(codom, spp == "ACHMIL" & year == 2014, names(codom) != "date"), by = c("site", "plot"))
# all but 1 yes (0 got lopped off of 10?)
# clean up spp
raw2014_tidy <- mutate(raw2014_tidy,
                       spp = recode(spp, "ACHLAN" = "ACHMIL", "ACOROS" = "GEUROS", "CLERHO" = "RHORHO",
                                    "CARsp" = "CAR_SP1", "Draba, yellow" = "DRABA_SP1", "POAABB" = "POA_SP1",
                                    "SALIXspp" = "SALIX_SP1", "unk forb" = "FORB_SP1")) 
summary(unique(raw2014_tidy$spp) %in% spplut$spp)

# find out what's different
check2014 <- full_join(raw2014_tidy, subset(codom, year == 2014, names(codom) != "date"), by = c("site", "plot", "spp")) %>%
  rename(raw_hits = hits.x, edi_hits = hits.y) %>%
  subset(raw_hits != edi_hits) %>%
  unite(plotid, site, plot, remove = F)
# are these the same plots that have <100 cover in codom?
subset(codom, year == 2014) %>%
  unite(plotid, site, plot, remove = F) %>%
  group_by(site, plot, plotid) %>%
  summarise(allhits = sum(hits)) %>%
  ungroup() %>%
  subset(allhits < 100) %>%
  mutate(check = plotid %in% check2014$plotid) %>%
  distinct(check) %>%
  summary() # yes

# correct hits from check 2014 in edi dataset
for(i in 1:nrow(check2014)){
  #id row to update 
  temprow <- with(codom, which(site == check2014$site[i] & plot == check2014$plot[i] & spp == check2014$spp[i] & year == 2014))
  codom$hits[temprow] <- check2014$raw_hits[i]
}

# are these good now?
check2014 <- full_join(raw2014_tidy, subset(codom, year == 2014, names(codom) != "date"), by = c("site", "plot", "spp")) %>%
  rename(raw_hits = hits.x, edi_hits = hits.y) %>%
  subset(raw_hits != edi_hits) %>%
  unite(plotid, site, plot, remove = F) # sweet

rm(check2014, raw2014, raw2014_tidy)


# -- SCREEN FOR SPP HITS SPIKES -----
# recrunch totcovcheck
# check that total cover in other years is around 100 (i.e. should i relativize just veg/moss/lichen or all [litter, bare, rock too]?)
totcovcheck <- mutate(codom, growth_habit = ifelse(USDA_code %in% removeusda, "ground cover", growth_habit)) %>%
  unite(plotid, site, plot, remove = F) %>%
  group_by(year, site, plot, plotid, growth_habit) %>%
  summarize(tothits = sum(hits),
            # crunch non-present
            #onlyhits = sum(hits[hits != 0.5])
  ) %>%
  ungroup() %>%
  spread(growth_habit, tothits, fill = 0) %>%
  mutate(greenhits = apply(.[grepl("forb|gram|shrub|lich|nonvas|lycop", names(.))], 1, sum),
         allhits = apply(.[grepl("forb|gram|shrub|ground|lich|nonvas|lycop", names(.))], 1, sum)) %>%
  # join with JGS rawdat all hits to compare
  left_join(data.frame(cbind(rawdat_all[c("year", "site", "plot")], sumcheck)))

# replot all hits with mean line
# plot tothits to see if can see when all hits vs top hits were used
group_by(totcovcheck, plotid) %>%
  mutate(avghits = mean(allhits)) %>%
  ungroup() %>%
  ggplot(aes(year, allhits, group = plot, col = plot)) +
  geom_hline(aes(yintercept = 100), col = "grey20", lty = 2) +
  geom_hline(aes(yintercept = avghits)) +
  geom_line() +
  facet_grid(plot~site, scales = "free_y") # better. 2002, 2003 and 2005 are years that need to be corrected.. maybe also any year that doesn't have 100 total hits (e.g. if 97.5 adjust)

tothitscheck <- mutate(codom, growth_habit = ifelse(USDA_code %in% removeusda, "ground cover", growth_habit)) %>%
  unite(plotid, site, plot, remove = F) %>%
  group_by(year, site, plot, plotid, growth_habit) %>%
  summarize(tothits = sum(hits[hits != 0.5]),
            # crunch non-present
            #onlyhits = sum(hits[hits != 0.5])
  ) %>%
  ungroup() %>%
  spread(growth_habit, tothits, fill = 0) %>%
  mutate(greenhits = apply(.[grepl("forb|gram|shrub|lich|nonvas|lycop", names(.))], 1, sum),
         allhits = apply(.[grepl("forb|gram|shrub|ground|lich|nonvas|lycop", names(.))], 1, sum)) %>%
  # join with JGS rawdat all hits to compare
  left_join(data.frame(cbind(rawdat_all[c("year", "site", "plot")], sumcheck)))

# plot to see how many plots have  over/under 100 hits (not counting spp present 0.5) [i.e. how many may need to be relativized]
ggplot(subset(tothitscheck, allhits != 100), aes(year, allhits, group = plot, col = allhits < 100)) +
  geom_hline(aes(yintercept = 100), col = "grey20", lty = 2) +
  #geom_hline(aes(yintercept = avghits)) +
  geom_point() +
  facet_grid(plot~site) # a lot of points..



# -- RELATIVIZE +- 100 hits -----
# leave present be, but relativize crosshair hits for anything that doesn't sum to 100
codom2 <- codom %>%
  group_by(site, plot, year) %>%
  mutate(tothits = sum(hits[hits != 0.5])) %>%
  ungroup() %>%
  mutate(hits100 = ifelse(hits != 0.5, round((hits/tothits)*100, 2), hits),
         # force anything that's < 1 to 1
         hits100 = ifelse(hits != 0.5 & hits100 < 1, 1, hits100)) %>%
  group_by(site, plot, year) %>%
  mutate(sumcheck = sum(hits100[hits100 != 0.5])) %>%
  ungroup() %>%
  #infill growth form with ground cover
  replace_na(list(growth_habit = "ground cover"))

# recrunch totcovcheck
totcovcheck2 <- codom2 %>%
  unite(plotid, site, plot, remove = F) %>%
  group_by(year, site, plot, plotid, growth_habit) %>%
  summarize(tothits = sum(hits100)) %>%
  ungroup() %>%
  spread(growth_habit, tothits, fill = 0) %>%
  mutate(greenhits = apply(.[grepl("forb|gram|shrub|lich|nonvas|lycop", names(.))], 1, sum),
         allhits = apply(.[grepl("forb|gram|shrub|ground|lich|nonvas|lycop", names(.))], 1, sum)) %>%
  # diff forb, gram, and bare total hits
  arrange(plotid, year) %>%
  group_by(plotid) %>%
  mutate(diff_forb = forb - lag(forb),
         diff_gram = graminoid - lag(graminoid),
         diff_ground = `ground cover` - lag(`ground cover`)) %>%
  ungroup()



# recrunch sppchecks since 2014 updates and relativizing
# .. spread out spp to infill 0s across all plots, all years; gather and diff interannual vals; flag cutoff or deviation from some spp-plot stat (e.g. median or modal value?)
sppcheck100 <- select(codom2, LTER_site:spp, hits100) %>%
  rename(hits = hits100) %>%
  data.frame() %>%
  spread(spp, hits, fill = 0) %>%
  gather(spp, hits, names(.)[grep("nut", names(.))+1]:ncol(.)) %>%
  unite(plotid, site, plot, remove = F) %>%
  arrange(plotid, spp, year) %>%
  # join in spp info
  left_join(spplut) %>%
  group_by(plotid, spp) %>%
  mutate(diffhits = hits - lag(hits),
         # flag anything that's one time only
         onetime = length(year[hits>0]) == 1 & length(hits[!hits %in% c(0,0.5,1)]) == 1,
         twotime = length(year[hits>0]) == 2 & length(hits[!hits %in% c(0,0.5,1)]) == 1,
         zerocheck = (hits == 0 & lag(hits) > 4 & lead(hits) > 4)
  )  %>%
  ungroup() %>%
  mutate(flagdiff = ifelse(growth_habit == "graminoid", abs(diffhits) >= 20,abs(diffhits) >= 10),
         # flag any 1x spp that diff > 5
         flagdiff = ifelse(onetime & diffhits > 4.5, TRUE, flagdiff),
         # ignore anything that's geum, deschampsia, or ground cover
         flagdiff = ifelse(growth_habit == "ground cover" | spp %in% c("DESCAE", "GEUROS", "moss"), FALSE, flagdiff),
         # flag anything that is fails zerocheck
         flagdiff = ifelse(zerocheck, TRUE, flagdiff)) %>%
  group_by(plotid, spp) %>%
  mutate(checkspp = any(flagdiff, na.rm = T)) %>%
  ungroup()


# screen by treatment, because might make sense that certain spp flux in target removal treatments
# control plots should not flux quite as much (esp XX)
ggplot(subset(sppcheck100, checkspp & rem == "A"), aes(year, diffhits, col = as.factor(site), group = plotid)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line() +
  geom_point(data = subset(sppcheck100, checkspp & zerocheck & rem == "A"), aes(year, diffhits, col = as.factor(site), group = plotid)) +
  ggtitle("codom qa check: spp spikes in acoros removal plots") +
  facet_wrap(~plot+spp, scales = "free_y")

ggplot(subset(sppcheck100, checkspp & rem == "D"), aes(year, diffhits, col = as.factor(site), group = plotid)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line() +
  geom_point(data = subset(sppcheck100, checkspp & zerocheck & rem == "D"), aes(year, diffhits, col = as.factor(site), group = plotid), size = 2) +
  ggtitle("codom qa check: spp spikes in descae removal plots") +
  facet_wrap(~spp+plot, scales = "free_y")

ggplot(subset(sppcheck100, checkspp & rem == "X"), aes(year, diffhits, col = as.factor(site), group = plotid)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line() +
  geom_point(data = subset(sppcheck100, checkspp & zerocheck & rem == "X"), aes(year, diffhits, col = as.factor(site), group = plotid), size = 2) +
  ggtitle("codom qa check: spp spikes in control plots") +
  facet_wrap(~plot+spp, scales = "free_y")



# notes from talking with jane about above plots:
# > 1) DANINT spike in CAX due to miss-ID with descae in 2009 (jgs says person that year was working on her own, had only worked season prior with someone else the for first time) 
# > 2) AREFEN that only appears one time -- check if STELON or CERARV was ever in that plot, was likely that (Hope trained IS and Marko in 2005 and got AREFEN mixed up so they were confused too)
# > 3) If STELON only appears once, also check is CERARV was there -- STELON and CERARV don't tend to grow in same habitat (CERARV grows in drier places)
# > 4) Change LEWPYG one time, hit hits to litter
# > 5) Check for BISVIV only once when other BISBIS
# > 6) CARSCO shouldn't have been confused with anything else -- if spikes, check to see if increase in ground cover or forbs
# > 7) If forbs spike, check for corresponding spike (up or down) in total gram hits
# > 8) PHLALP spike is probably TRISPI (2006 only year PHLALP there and TRISPI not--a little high, maybe PHLALP = TRISPI + DESCAE)

# add in functional total hits to check if when forb spp increase grams or ground cover decrease
sppcheck100 <- left_join(sppcheck100, dplyr::select(totcovcheck2, year, plotid, diff_forb:diff_ground)) %>%
  mutate(flagdiff2 = ifelse(growth_habit == "graminoid" & flagdiff & abs(diff_ground) > abs(diff_gram), FALSE, flagdiff))


# manual corrections:
# 1) DANINT in 2009, 7_CAX
# sum DANINT and DESCAE, look at relative amounts of each in year prior and before and apportion similarly
Dsum <- subset(codom2, site == 7 & plot == "CAX" & spp %in% c("DESCAE", "DANINT")) %>%
  select(LTER_site:growth_habit, hits100) %>%
  group_by(year) %>%
  mutate(yrsum = sum(hits100)) %>%
  ungroup() %>%
  arrange(year) %>%
  group_by(spp) %>%
  mutate(lagdiff = hits100 - lag(hits100)) %>%
  ungroup() %>%
  mutate(relhits = round((hits100/yrsum)*100,2))

ggplot(Dsum, aes(year, relhits)) +
  geom_line(aes(col = spp)) +
  # plot midpoints
  geom_point(aes(x = 2009, y = mean(c(Dsum$relhits[Dsum$spp == "DANINT" & Dsum$year == 2008],Dsum$relhits[Dsum$spp == "DANINT" & Dsum$year == 2010])))) +
  geom_point(aes(x = 2009, y = mean(c(Dsum$relhits[Dsum$spp == "DESCAE" & Dsum$year == 2008],Dsum$relhits[Dsum$spp == "DESCAE" & Dsum$year == 2010]))))

# > decide take relcov two years prior and 2 after for each and apportion total hits of the two
danint <- subset(Dsum, spp == "DANINT" & year %in% c(2007, 2008, 2010, 2011)) %>%
  select(relhits) %>% sapply(mean)
descae <- subset(Dsum, spp == "DESCAE" & year %in% c(2007, 2008, 2010, 2011)) %>%
  select(relhits) %>% sapply(mean)
round(unique(Dsum$yrsum[Dsum$year == 2009])*(danint/100),0)
round(unique(Dsum$yrsum[Dsum$year == 2009])*(descae/100),0)
# check how those values look in the time series
ggplot(Dsum, aes(year, hits100)) +
  geom_line(aes(col = spp)) +
  # plot midpoints
  geom_point(aes(x = 2009, y = 35, col = "DANINT")) +
  geom_point(aes(x = 2009, y = 20, col = "DESCAE")) # looks reasonable

danintrow <- with(codom2, which(spp == "DANINT" & year == 2009 & site == 7 & plot == "CAX"))
descaerow <- with(codom2, which(spp == "DESCAE" & year == 2009 & site == 7 & plot == "CAX"))
# also update hits col for recrunching hits100
# is original tothits 100? if so, can assign same number as hits100 (never needed to be relativized)
codom2$tothits[danintrow]
codom2[danintrow, c("hits", "hits100")] <- round(unique(Dsum$yrsum[Dsum$year == 2009])*(danint/100),0)
codom2[descaerow, c("hits", "hits100")] <- round(unique(Dsum$yrsum[Dsum$year == 2009])*(descae/100),0)

# clean up
rm(Dsum, danint, descae, danintrow, descaerow)



# 2) LEWPYG to litter (7_CXX 2006, 5_CBX 2004) [6_CDN also has spike but spike real, both litter and lewpyg present throughout ts]
# review records to swap
View(subset(sppcheck100, spp %in% c("litter", "LEWPYG") & plotid %in% c("7_CXX", "5_CBX")))
# id rows to swap hits100 vals (litter not in dataset these plots these years, so just reassign codes/plant names)
cxxlewpyg <- with(codom2, which(site == 7 & plot == "CXX" & spp == "LEWPYG" & year == 2006))
cbxlewpyg <- with(codom2, which(site == 5 & plot == "CBX" & spp == "LEWPYG" & year == 2004))
# to be sure
codom2$hits100[cxxlewpyg]
codom2$hits100[cbxlewpyg] # yes
# need to replace spp through growth_habit
codom2[c(cxxlewpyg,cbxlewpyg), c("spp", "USDA_code", "USDA_name", "growth_habit")] <- subset(spplut, spp == "litter")

# clean up
rm(cxxlewpyg, cbxlewpyg)


# 3) ACHMIL to ARTSCO in 6_CXX 2007
# > only year ARTSCO not there and only year ACHMIL is .. maybe got misentered? (or miss-ID'd?.. )
# review
View(subset(codom2, plot == "CXX" & site == 6 & spp %in% c("ACHMIL", "ARTSCO"))) # yup. only year achmil there and artsco not there
achmilrow <- with(codom2, which(site == 6 & plot == "CXX" & spp == "ACHMIL" & year == 2007))
codom2[achmilrow, c("spp", "USDA_code", "USDA_name", "growth_habit")] <- subset(spplut, spp == "ARTSCO")
# clean up
rm(achmilrow)


# 4) PHLALP to TRISPI (4_CXN)
# review
View(subset(codom2, plot == "CXN" & site == 4 & spp %in% c("PHLALP", "TRISPI"))) # yup. only year phlalp there and trispi not there
phlalprow <- with(codom2, which(site == 4 & plot == "CXN" & spp == "PHLALP" & year == 2006))
codom2[phlalprow, c("spp", "USDA_code", "USDA_name", "growth_habit")] <- subset(spplut, spp == "TRISPI")
# clean up
rm(phlalprow)


# 5) BISVIV to BISBIS when BISVIV only appears once and BISBIS other years
bischeck <- subset(sppcheck100, spp %in% c("BISBIS", "BISVIV")) %>%
  group_by(plotid) %>%
  mutate(nbisbis = length(hits[hits > 0 & spp == "BISBIS"]),
         nbisviv = length(hits[hits > 0 & spp == "BISVIV"])) %>%
  group_by(plotid, year) %>%
  mutate(flagbis = (hits > 0 & spp == "BISVIV") & (hits == 0 & spp == "BISBIS")) %>%
  ungroup()
View(subset(bischeck, plotid %in% unique(plotid[onetime])))
# hm.. every year bisviv there, bisbis also recorded.. keep as is


# 6) final check for 1x STELON, ARFEF, CERARV
forbcheck <- subset(sppcheck100, plotid %in% unique(plotid[onetime & spp %in% c("STELON", "ARFEF", "CERARV")])) %>%
  filter(spp %in% c("STELON", "ARFEF", "CERARV")) %>%
  # clean up a bit to review
  dplyr::select(LTER_site:checkspp) %>%
  subset(hits != 0) %>%
  arrange(plotid, year, spp)
# not going to mess with this one either. plots where spp are one time, others don't show up (usually), and others where a spp is 1x the others are also recorded for that year


# one more stelon check (with sibpro..)
steloncheck <- subset(codom2, spp %in% c("STELON", "SIBPRO")) %>%
  unite(plotid, site, plot) %>%
  arrange(plotid, year, spp) %>%
  group_by(plotid, year) %>%
  mutate(nspp = length(unique(spp)),
         unihits = length(unique(hits))) %>%
  ungroup() %>%
  subset(plotid %in% plotid[nspp == 2])

# look and stelon and sibbaldia in plots that have same hits values in 2004.. is stelon in those plots in other years?
# i.e. which stelon to drop and which to allow (e.g. if only 0.5 or 1 and present in other years? [esp recent])
stelon2004 <- subset(steloncheck, plotid %in% unique(plotid[nspp == 2 & unihits == 1 & year == 2004])) %>%
  arrange(plotid, year, spp) %>%
  # flag any plot where STELON only there in 2004 (or one year)
  group_by(plotid, spp) %>%
  mutate(nyear = length(unique(year)),
         nearly = length(unique(year[year < 2006 & year != 2004])),
         # calculate mean hits excluding 2004
         meanhits100 = mean(hits100[year != 2004]),
         meanearly = mean(hits100[year < 2006 & year != 2004])) %>%
  ungroup() %>%
  mutate(flag2004 = ifelse(year == 2004, hits100 > meanhits100,FALSE),
         diff2004 = hits100-meanhits100,
         flagearly = ifelse(year == 2004, hits100 > meanearly,FALSE),
         diffearly = hits100 - meanearly)

length(unique(stelon2004$plotid))
# 5_CXX .. change to 0.5.. not recorded until 2005 and 2005 was an all hits year. there are some plots in 2004 where tothits under 100, so this will be one of them (but not by much)
# 7_CAC .. maybe change from 4 to 0.5.. was 3 hits in 2002 (but KNS was doing all hits that year, then 1 hit 2005, then present or 1 hit 2007-2014)
# all other STELONs in 2004 should be dropped bc that was the only year they were there and the value matched sibbaldia exactly
stelonplots <- with(stelon2004, plotid[spp == "STELON" & year == 2004])

for(i in stelonplots){
  if(i %in% c("5_CXX", "7_CAC")){
    # change to 0.5
    temprow <- with(codom2, which(site == substr(i,1,1) & plot == substr(i, 3, 5) & year == 2004 & spp == "STELON"))
    codom2$hits[temprow] <- 0.5
    codom2$hits100[temprow] <- 0.5
    next
  }
  #drop from codom2
  temprow <- with(codom2, which(site == substr(i,1,1) & plot == substr(i, 3, 5) & year == 2004 & spp == "STELON"))
  codom2 <- codom2[-temprow,]
}


# recrunch hits100 with updated hits for all
clean_veg <- codom2 %>%
  group_by(site, plot, year) %>%
  mutate(tothits = sum(hits[hits != 0.5])) %>%
  ungroup() %>%
  mutate(hits100 = ifelse(hits != 0.5, round((hits/tothits)*100, 2), hits),
         # force anything that's < 1 to 1
         hits100 = ifelse(hits != 0.5 & hits100 < 1, 1, hits100)) %>%
  group_by(site, plot, year) %>%
  mutate(sumcheck = sum(hits100[hits100 != 0.5])) %>%
  ungroup() %>%
  # make plotid
  unite(plotid, site, plot, remove = F) %>%
  # > select vegdat to use moving forward to FI scores using relativized hits
  dplyr::select(LTER_site:date, plotid, site:growth_habit, hits100) %>%
  rename(hits = hits100)


# check to be sure all sums to about 100 and no funny individuals hits values
boxplot(clean_veg$hits)
summary(clean_veg$hits)
group_by(clean_veg, year, plotid) %>%
  summarise(tothits = sum(hits)) %>%
  summary() #good

# make sure it looks good
# write out for courtney and katie to use for katie's proposal
write_csv(clean_veg, "codom/codom_sppcomp_clean.csv")

# time to move on! (yay!)
# clean up enviro
rm(forbcheck, bischeck, steloncheck, stelon2004, stelonplots, totcovcheck, totcovcheck2, achmil2014,
   tothitscheck, sppcheck100, sppcheck, rawdat_all, codom2, spptempfreq, earlydat, earlydat2, sppreview,
   sumcheck, sumcheck2, IAspp, i, removeusda, temprow)



# -- WRITE OUT VEG MATRICES FOR PYTHON -----
copy <- clean_veg
# note: need to make 2013 infill data for 2_CAN so FI can run for that plot (average values from 2012 and 2014, no missing data allowed)

# re-assess who to drop
spptempfreq <- clean_veg %>%
  subset(growth_habit != "ground cover") %>%
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
  subset(!grepl("B", plotid)) %>%
  subset(nobs>3) %>%
  ggplot() +
  geom_vline(aes(xintercept = 50), col = "red", lty = 2) +
  geom_point(aes(round(relfreq,1), val, col = nobs)) +
  #scale_color_manual(values = c("TRUE" = "pink", "FALSE" = "grey10")) +
  scale_x_continuous(breaks = seq(20,100, 20)) +
  labs(x = "Relative temporal frequency (point = species-plot)",
       y = "Plot hits summary value",
       title = "NWT Codom: species max hits (per plot-yr), mean hits (per plot-yr),\ntotal hits (all years) by plot temporal rel. frequency") +
  facet_grid(met ~ ., scales = "free_y")


# > drop any spp hit 3x or less.. can modify later if Katie wants, but that seems like a reasonable cut looking at the data

# make placeholder dat for 2013 2_CAN
# > STELON is only spp present 1 year in 2012 & 2014 at 0.5; is present in 2_CAN most of 2010s so keep at 0.5 for 2013
# > looked at other CAN plots 2012-2014 and taking average seems like an alright enough  quick strategy (2013 was maybe a bit of a grass year?)
CAN2_1214 <- subset(clean_veg, plotid == "2_CAN"& year %in% c(2012, 2014)) %>%
  group_by(spp) %>%
  mutate(nobs = length(year),
         meanhits = mean(hits)) %>%
  ungroup() %>%
  arrange(spp) %>%
  mutate(year = 2013,
         date = NA) %>%
  select(-hits) %>%
  distinct() %>%
  rename(hits = meanhits)
# manual edits to get sum to 100 (excluding PA)
CAN2_1214$hitsedit <- CAN2_1214$hits
CAN2_1214$hitsedit[CAN2_1214$spp == "bare"] <- 0
CAN2_1214$hitsedit[CAN2_1214$spp == "litter"] <- CAN2_1214$hits[CAN2_1214$spp == "litter"]-0.5
CAN2_1214$hitsedit[CAN2_1214$spp == "TRIPAR"] <- CAN2_1214$hits[CAN2_1214$spp == "TRIPAR"]+0.5
CAN2_1214$hitsedit[CAN2_1214$spp == "CARSCO"] <- CAN2_1214$hits[CAN2_1214$spp == "CARSCO"]+0.5
CAN2_1214$hitsedit[CAN2_1214$spp == "GENALG"] <- CAN2_1214$hits[CAN2_1214$spp == "GENALG"] + 0.5
CAN2_1214$hitsedit[CAN2_1214$spp == "CALLEP"] <- CAN2_1214$hits[CAN2_1214$spp == "CALLEP"] + 0.25
sum(CAN2_1214$hitsedit[CAN2_1214$hitsedit != 0.5]) # great
CAN2_1214 <- select(CAN2_1214, LTER_site:growth_habit, hitsedit) %>% rename(hits = hitsedit) %>% data.frame()

# append to cleanveg
clean_veg <- rbind(clean_veg, CAN2_1214)
# clean up
rm(CAN2_1214)


# notes on file prep:
# > can use same veg matrix for each type of FI Katie wants
# > filename should have plotid
# > remove ground cover and any spp not present 4 yrs or more
# > katie doesn't care about rando removal plots (but could run all the same.. no cost in a for-loop)
# > python code needs year in col 1, spp after that
# > write out with NO headers

for(i in unique(clean_veg$plotid)){
  # subset plot
  tempdat <- subset(clean_veg, plotid == i & growth_habit != "ground cover") %>%
    # calc temporal freq and drop anything 3 yrs or less
    group_by(spp) %>%
    mutate(nobs = length(unique(year))) %>%
    ungroup() %>%
    # keep only those spp that occurred 4 yrs or more
    subset(nobs > 3, c(year, spp, hits)) %>%
    # make wide, infill NA with 0s
    spread(spp, hits, fill = 0) %>%
    # order by year
    arrange(year) %>%
    data.frame()
  
  # write out
  write_csv(tempdat, paste0("~/python/hack4kns/vegdat/", i,"vegdat.csv"), col_names = F) #"codom/forpython/"
  
}

# -- REORG TIME WINDOW FIs -----
# need to run this loop after generating each time-window set of FI scores (e.g. 4yr, 8yr) before running next set
# > note: it takes about 8 seconds to generate FI scores on all 70 plots! crazy

FIfiles <- list.files("~/python/hack4kns/vegdat/") 
# select FI files generated (has FI or _sost)
FIfiles <- FIfiles[grepl("FI|_sost", FIfiles)]
# set window yr for loop below
y <- 8 #4
# run loop to move and clean up files
for(i in FIfiles){
  # if is an FI file, move (if sost, ignore this part)
  if(grepl("_FI", i)){
    file.copy(from = paste0("~/python/hack4kns/vegdat/",i), 
              to = paste0("~/python/hack4kns/vegdat/", y, "yrwin/", y, "yr_",i), overwrite = T)
  }
  # remove from vegdat
  file.remove(paste0("~/python/hack4kns/vegdat/",i))
}



# -- COMPILE FI SCORES -----
# > want to stack all FI scores, being sure to note which time window set they're from

# initate master df
FImaster <- data.frame()
# run loop to read in all FI dats
for(i in c(4,8)){
  tempfiles <- list.files(paste0("~/python/hack4kns/vegdat/", i, "yrwin"), full.names = T)
  for(t in tempfiles){
    tempdat <- read.csv(t, stringsAsFactors = F) %>%
      rename(increment = X) %>%
      mutate(winsize = i,
             plotid = str_extract(t, "[0-9]_[A-Z]{3}(?=vegdat)"))
    FImaster <- rbind(FImaster, tempdat)
  }
}

# pair site info and clean up
FImaster <- left_join(FImaster, distinct(dplyr::select(clean_veg, LTER_site, local_site, plotid:nut))) %>%
  # reorder cols
  dplyr::select(LTER_site, local_site, plotid, site:nut, winsize, increment:ncol(.)) %>%
  rename(timestep = Time_Step,
         smooth3_FI = Smooth_FI) %>%
  data.frame()
str(FImaster)

# plot to be sure looks okay
ggplot(FImaster, aes(increment, FI)) +
  geom_line(aes(group = plotid)) +
  geom_smooth(fill = "dodgerblue2") +
  scale_x_continuous(breaks = seq(0,20,2)) +
  ggtitle("NWT LTER Codom spp comp: FI prelim results, 4- and 8-yr windows") +
  facet_wrap(~plot+winsize)
# write out for kns
ggsave("codom/codom_FIprelim.pdf", width = 7, height = 6, units = "in")

# write out FI dataset
write_csv(FImaster, "codom/codom_FI_4-8yr.csv")
