# prep ts dats for multivariate analysis

# script purpose:
# read in all ts alpine plant comm dats, spp lookup table, perhaps marko trait dataset
# standardize spp names across all datasets
# standardize site descriptive info for plots across years (e.g. all plot 1 at saddle should have same meadow, snow and N/P nut addition info)
# > note: add in 2016 sdl descrip as alternative (ts sent to ctw on 7/5/19)
# create spp matrices for NMDS:
# 1) top hit only -- update: can't do this bc some raw datasets have vert height summed already
# 2) all hits
# write out cleaned up plant comm and site lookup dataset to output_data for analysis

# notes:
# ...



# -- SETUP ----
rm(list = ls())
library(tidyverse)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", NA, "NA", "NaN", NaN, ".")
source("edi_functions.R")

# set pathway to data folder on your local machine
datpath <- "../../Documents/nwt_lter/unpub_data/dry_meado_fert/"
# list data files
datfiles <- list.files(datpath, full.names = T)

# read in datasets
## NutNet
nutnet13 <- read.csv(datfiles[grep("net13", datfiles)], strip.white = T, na.strings = na_vals)
nutnet17 <- read.csv(datfiles[grep("net17[.]", datfiles)], strip.white = T, na.strings = na_vals)
nutnet17raw <- read.csv(datfiles[grep("net17r", datfiles)], strip.white = T, na.strings = na_vals)
# read in block 3 plot 7 2017 (missing from above raw file), ts sent to ctw 7/9/19
nn17_b3p7 <- read.csv(datfiles[grep("b3", datfiles)], strip.white = T, na.strings = na_vals)

## Sdl 
plot_codes <- read.csv(datfiles[grep("codes_99", datfiles)], strip.white = T, na.strings = na_vals)
sdl1997 <- read.csv(datfiles[grep("1997", datfiles)], strip.white = T, na.string = na_vals)
sdl2012 <- read.csv(datfiles[grep("2012", datfiles)], strip.white = T, na.strings = na_vals)
sdl2016 <- read.csv(datfiles[grep("2016", datfiles)], strip.white = T, na.strings = na_vals)
# ts 2016 codes sent 7/5
plots2016 <- read.csv(datfiles[grep("16_codes", datfiles)], strip.white = T, na.string = na_vals)

## spp lookup table
spplt <- read.csv("alpine_addnuts/output_data/sdl_nutnet_spplookup.csv", na.strings = na_vals, strip.white = T)




# -- REVIEW DATA -----
# nutnet
glimpse(nutnet13) # wide form, hits
glimpse(nutnet17) # long-form, summarized (rel and abs cov)
glimpse(nutnet17raw) # long form, raw hits at each grid pt (need to be summed to plot level)
glimpse(nn17_b3p7) # same as nutnet17_raw
# sdl
glimpse(sdl1997) # date read in as integer, long form total hits
glimpse(sdl2012) # no date, total hits, long form
glimpse(sdl2016) # no hits, just presence (spp noted at grid point) -- only top hits

# note: nmds must be reduced to top hit only if comparing trends in all so it's a fair comparison
# to be sure, what is sum of total hits in each plot, per dataset?
with(sdl2012, sapply(split(hits, plot), sum)) #sdl 2012 sums to over 100 hits per plot
with(sdl1997, sapply(split(hits, old_plot), sum))
boxplot(sdl1997$hits) # 999 == not present?
sort(unique(sdl1997$hits))
#do all spp have 999 entered?
sort(unique(sdl1997$species[sdl1997$hits == 999])); sort(unique(sdl1997$species))
# > not all spp have 999 entered..
# update: TS says 999 = present in plot but not "hit" at grid point. 
# not all datasets have spp present (but not hit) in them, so only look at cover for now
# ts says he can send me richness dats, but would have to go digging for those files

# nutnet -- plot not unique, must combine block_plot for unique ID
apply(dplyr::select(nutnet13, LTR:ncol(nutnet13)), 1, sum) # sums to over 100..
# > update: TS says "O" in the nutnet17 set means nothing was hit (O != ORALA)
# sum hits per plot in nutnet17
subset(nutnet17raw, species != "O") %>%
  group_by(block, plot) %>%
  summarise(hit = length(species)) %>%
  print(n = Inf) # sums to over 100
# how many unique points?
unique(nutnet17raw$point) #124?
View(nutnet17raw[nutnet17raw$point ==124,]) # nothing hit anyway.. no importa!

# are there any other codes for present but not hit in the datasets?
gather(nutnet13, code, hit, LTR:ncol(nutnet13)) %>%
  arrange(hit) %>%
  dplyr::select(hit) %>% distinct() # there is: 0.25

sapply(sdl1997, function(x) sort(unique(x))) # no presence recorded, trts are upper case
sapply(sdl2012, function(x) sort(unique(x))) #0.1 for hits, trts are lower case
sapply(sdl2016, function(x) sort(unique(x))) # trt also coded differently

sapply(nutnet13, function(x) sort(unique(x)))
sapply(nutnet17raw, function(x) sort(unique(x)))



# -- NUTNET PREP -----
# 2013
nn13_tidy <- nutnet13 %>%
  rename(K = `K.`) %>%
  gather(code, hits, LTR:ncol(.)) %>%
  # filter out spp not hit or present (0.25) but not hit
  filter(hits >= 1) %>%
  mutate(plotID = paste(Block, Plot, sep = "_"),
         yr = 2013) %>%
  left_join(spplt[c("code", "clean_code2")]) %>%
  dplyr::select(yr, plotID, Block:trt, clean_code2, hits) %>%
  grouped_df(colnames(.)[1:9]) %>%
  summarise(hits = sum(hits)) %>%
  arrange(Block, Plot, clean_code2) %>%
  # lower case all colnames
  rename_all(~ casefold(.)) %>%
  ungroup() %>%
  data.frame()

# build nutnet plot lookup table
nutnet_plots <- distinct(dplyr::select(nn13_tidy,plotid:trt))

# 2017
# append nn17_b3p7 to nutnet17raw
## K colname same as nutnet17raw colname
colnames(nn17_b3p7) <- gsub("[.]", "", colnames(nn17_b3p7))

nn17_tidy <- nutnet17raw %>%
  rbind(nn17_b3p7) %>%
  # capitalize all colnames to match 2013
  rename_all(~casefold(.)) %>%
  #drop O (nothing hit)
  subset(species != "O") %>%
  # create col for hit count, yr
  mutate(hits = 1,
         # add yr
         yr = 2017,
         # clean up block format
         block = paste0("B", block)) %>%
  # join spp lt info
  left_join(spplt[c("code", "clean_code2")], by = c("species" = "code")) %>%
  # join trt col from nn2013
  left_join(distinct(dplyr::select(nn13_tidy, plotid:p, trt))) %>%
  data.frame() %>%
  # arrange by block, plot then spp code
  arrange(block, plot, clean_code2)

# where does 2017 differ in treatments?
trtcheck <- left_join(nutnet_plots, 
                   distinct(dplyr::select(nn17_tidy, block, plot, n, p,k)),
                   by = c("block", "plot")) %>%
  # check for diffences
  mutate(ndiff = n.x != n.y,
         pdiff = p.x != p.y,
         kdiff = k.x != k.y)
View(trtcheck)
# trtment for n, and k not entered consistently within plot B2_7 in 2017
# sometimes it's +n, +p, -k; othertimes -n, -p, +k
# going by 2013, should be +n, +p, -k

# > decision: drop trt cols in 2017 dataset and re-join 2013 trt cols
nn17_tidy <- nn17_tidy %>%
  dplyr::select(-c(n,p,k, plotid, trt)) %>%
  left_join(distinct(dplyr::select(nn13_tidy, plotid:trt))) %>%
  # ms says canopy structure assessed in both 2013 and 2017, so going to assume vert summed in 2013
  dplyr::select(colnames(nn13_tidy)) %>%
  grouped_df(colnames(.)[1:9]) %>%
  summarise(hits = sum(hits)) %>%
  arrange(block, plot, clean_code2) %>%
  ungroup()





# -- SDL DATA PREP -----
# for all of these, keep plot/old_plot, species, and hits, use plot_codes table to crosswalk/standardize all
# lower case plot_codes
colnames(plot_codes) <- casefold(colnames(plot_codes))
# drop count col
plot_codes <- dplyr::select(plot_codes, -count)

# make sdl crosswalk/lookup tble for *all* plots (to preserve data) 
# > bc nutnet trt listed as "N+P", "N", "C", use that type of trtment code as well
# > sdl2016 has old and new codes so start there..
sdl_plots <- dplyr::select(sdl2016, -c(point, species)) %>%
  distinct() %>%
  rename(old_plot = `old.`) %>%
  dplyr::select(plot, old_plot, meadow, sfcode, trt)
# append 16 to colnames after plot to keep track of origin
colnames(sdl_plots)[2:ncol(sdl_plots)] <- paste0(colnames(sdl_plots)[2:ncol(sdl_plots)], 16)
# join july 2019 2016 codes TS sent to CTW
sdl_plots <- left_join(sdl_plots, plots2016) %>%
  #drop empty col in 2016v2 
  dplyr::select(-X) %>%
  rename(notes = X.1)
#id pos that ends with last concatenation
pos <- max(grep("16", colnames(sdl_plots)))
# append "16.2" to cols joined and lowercase colnames
colnames(sdl_plots)[(pos+1):ncol(sdl_plots)] <- paste0(casefold(colnames(sdl_plots[(pos+1):ncol(sdl_plots)])), "16.2")

# join plot_codes lookup data
sdl_plots <- full_join(sdl_plots, plot_codes, by = "plot") 
#id pos that ends with last concatenation
pos <- max(grep("16.2", colnames(sdl_plots)))
# append "LT" to colnames added to keep track of origin
colnames(sdl_plots)[(pos+1):ncol(sdl_plots)] <- paste0(colnames(sdl_plots)[(pos+1):ncol(sdl_plots)], "LT")
# > none of the old_plot code in the plot_code table match up with old_plot in the sdl16 dataset!!
# > BUT! trtment and snow cols match up so that's good. be sure to ignore old. in 2016 dataset when tidying it

# join 2012 plot data
sdl_plots <- full_join(sdl_plots, distinct(dplyr::select(sdl2012, meadow:plot)), by = "plot")
#id pos that ends with last concatenation
pos <- max(grep("LT", colnames(sdl_plots)))
# append 12 to colnames added to keep track of origin
colnames(sdl_plots)[(pos+1):ncol(sdl_plots)] <- paste0(colnames(sdl_plots)[(pos+1):ncol(sdl_plots)], 12)
View(sdl_plots) # there are some plots in the 2012 dataset that aren't in the 2016 dataset..

# join sdl 1997
sdl_plots <- full_join(sdl_plots, dplyr::select(sdl1997, old_plot, site, trt), by = c("old_plotLT" = "old_plot"))
#id pos that ends with last concatenation
pos <- max(grep("12", colnames(sdl_plots)))
# append 97 to colnames added to keep track of origin
colnames(sdl_plots)[(pos+1):ncol(sdl_plots)] <- paste0(colnames(sdl_plots)[(pos+1):ncol(sdl_plots)], 97)

# reorg colnames so cols that should match up are adjacent
sdl_plots <- dplyr::select(sdl_plots,
                           plot, old_plotLT, old_plot16, 
                           meadowLT, meadow12, meadow16, meadow16.2,
                           snowLT, site97, snow12, snow16.2, sfcode16, recovery16.2, notes16.2,
                           trtLT, trt97, fert12, trt16, fert16.2) %>%
  arrange(plot) %>%
  distinct()
  #rm extra old plotnums in the 2016 that pair with plot 14 and aren't 287 since there is a match for that
  # look like "287" got copied down in excel but sequence number went up when really it should have been 287 the entire time
sdl_plots <- sdl_plots %>%
  filter((plot == 14 & old_plot16 == 287) |
           plot != 14 |
           is.na(plot)) %>%
  mutate(old_plot97 = old_plotLT,
         old_plotLT = ifelse(is.na(plot), NA, old_plotLT)) %>%
  dplyr::select(plot, old_plotLT, old_plot97, old_plot16:ncol(.))

View(sdl_plots) # inconsistencies in snow and meadow in 2012 and 2016 yrs.. write out for Tim to look at
write_csv(sdl_plots, "alpine_addnuts/output_data/sdl_plots_lookup_trblshoot.csv")



# -- PREP SDL PLANT DATA WITHOUT TRT INFO IN MATRICES (just plot num) -----
## 1997 -- deal with 999 (present but not hit)
sdl97_tidy <- sdl1997 %>%
  # remove spp present but not hit and spp not hit
  subset(!hits %in% c(0,999)) %>%
  left_join(spplt[c("code", "clean_code2")], by = c("species" = "code")) %>%
  # join treatment info
  left_join(plot_codes) %>%
  mutate(yr = 1997)
View(subset(sdl97_tidy, is.na(snow)))
# which sdl97 old plots not in plot code? and vice versa?
unique(sdl1997$old_plot[!sdl1997$old_plot %in% plot_codes$old_plot])
unique(plot_codes$old_plot[!plot_codes$old_plot %in% sdl1997$old_plot]) # all there.. 
length(unique(plot_codes$old_plot))
length(unique(sdl1997$old_plot)) # i guess 0, 96 and 286 really are extraneous plots? (0 or 96 = typo, but 286 is a mystery plot)
# > drop 286 and press on with reorganizing cols for standardization
# > drop site descrip cols until tim gets back to me on correct site descriptions
sdl97_tidy <- subset(sdl97_tidy, !is.na(snow)) %>%
  dplyr::select(yr, plot, clean_code2, hits) %>%
  # sum hits by clean spp code
  grouped_df(colnames(.)[1:3]) %>%
  summarise(hits = sum(hits)) %>%
  arrange(plot, clean_code2) %>%
  ungroup()


# sdl 2012
sdl12_tidy <- sdl2012 %>%
  # remove spp present only or spp absent
  subset(hits >= 1) %>%
  left_join(spplt[c("code", "clean_code2")], by = c("species" = "code")) %>%
  # add col for yr
  mutate(yr = 2012) %>%
  group_by(yr, plot, clean_code2) %>%
  summarise(hits = sum(hits)) %>%
  ungroup()


# sdl 2016
sdl16_tidy <- sdl2016 %>%
  # remove placeholder for nothing hit
  subset(species != "junk1") %>%
  left_join(spplt[c("code", "clean_code2")], by = c("species" = "code")) %>%
  # add col for hit count
  mutate(hits = 1,
         # add col for yr
         yr = 2016) %>%
  group_by(yr, plot, clean_code2) %>%
  summarise(hits = sum(hits)) %>%
  ungroup()



# -- STANDARDIZE NN + SDL DATS, STACK, AND SEPARATE PLANT FROM SITE INFO -----
# make 2 datasets to write out:
# 1) stack plant community data -- need to add local_site to each dat
# 2) stack site info

master_plant <- rbind(nn13_tidy, nn17_tidy) %>%
  dplyr::select(yr, plotid, clean_code2, hits) %>%
  rename(plot = plotid) %>%
  mutate(site = "nutnet") %>%
  rbind(rbind(cbind(sdl97_tidy, site = "sdl"),
              cbind(sdl12_tidy, site = "sdl"),
              cbind(sdl16_tidy, site = "sdl"))) %>%
  dplyr::select(site, yr:ncol(.)) %>%
  # change plot back to plotid
  rename(plotid = plot) %>%
  ungroup()

# be sure no duplicates in the dataset
checkdups <- master_plant %>%
  grouped_df(colnames(.)[1:4]) %>%
  summarise(nobs = length(hits))
summary(checkdups$nobs) # all 1s -- each spp only has one entry per plot. good! ready to write out



# --- CLEAN UP SITE LOOKUP TABLES -----
# add site to nutnet lookup
nutnet_plots <- mutate(nutnet_plots, site = "nutnet") %>%
  dplyr::select(site, plotid:ncol(.))


# specify dry meadow codes used across sdl datasets
drycodes <- c("d", "Dry", "dry")
wetcodes <- c("w")
mescodes <- c("m", "mesic")
snowcodes <- c("snow", "s", "SD")
nosnow <- c("n", "none", "recovery", "NONE", "ND")


# select plot and treatment from sdl (choose sdl 16.2 bc that is most recent info from TS)
sdl_plots2 <- sdl_plots %>%
  mutate(trt = ifelse(is.na(fert16.2), trt97, fert16.2),
         trt = ifelse(is.na(trt), fert12, trt),
         trt = casefold(trt, upper = T),
         trt = recode(trt, "NN" = "N", "CC" = "C", "CONTROL" = "C",
                      "PP" = "P", "NP" = "N+P"),
         sfcode2016 = ifelse(!is.na(recovery16.2), recovery16.2,
                         ifelse(snow12 == "n", 1, NA)),
         sfcode2016 = ifelse(is.na(sfcode2016), "Unknown", sfcode2016),
         notes2016 = notes16.2,
         notes2016 = ifelse(is.na(notes2016) & sfcode2016 == 1, "never affected", notes2016),
         notes2016 = ifelse(is.na(notes2016), "Unknown", notes2016)) %>%
  rename(old_plot = old_plot97) %>%
  #temp create rowid for grouping by row
  mutate(rowid = row.names(.)) %>%
  group_by(rowid) %>%
  mutate(meadow = ifelse(meadowLT %in% c(drycodes, NA) &
                           meadow12 %in% c(drycodes, NA) &
                           meadow16 %in% c(drycodes, NA), "Dry", "Unknown"),
         meadow = ifelse(meadowLT %in% c(wetcodes, NA) &
                           meadow12 %in% c(wetcodes, NA) &
                           meadow16 %in% c(wetcodes, NA), "Wet", meadow),
         meadow = ifelse(meadowLT %in% c(mescodes, NA) &
                           meadow12 %in% c(mescodes, NA) &
                           meadow16 %in% c(mescodes, NA), "Mesic", meadow),
         #correct for NAs in all
         meadow = ifelse(is.na(meadowLT) & is.na(meadow12) & is.na(meadow16), "Unknown", meadow),
         # specify snow col
         snow = ifelse(snowLT %in% c(snowcodes, NA) &
                         site97 %in% c(snowcodes, NA) &
                         snow12 %in% c(snowcodes, NA) &
                         sfcode16 %in% c(snowcodes, NA), "Snow", "Unknown"),
         snow = ifelse(snowLT %in% c(nosnow, NA) &
                         site97 %in% c(nosnow, NA) &
                         snow12 %in% c(nosnow, NA) &
                         sfcode16 %in% c(nosnow, NA), "No snow", snow),
         #correct for NAs in all
         snow = ifelse(is.na(snowLT) & is.na(site97) & is.na(snow12) & is.na(sfcode16), "Unknown", snow)) %>%
  ungroup() %>%
  # add in alternate meadow and snow cols based on most recent ts 2016 info
  mutate(meadow_alt = ifelse(!is.na(meadow16.2), meadow16.2, meadow),
         snow_alt = ifelse(!is.na(snow16.2), snow16.2, snow),
         # correct code spellings for snow_alt
         snow_alt = gsub("SNOW", "Snow", snow_alt),
         snow_alt = gsub("NONE", "No snow", snow_alt)) %>%
  #retain cols just used for analysis
  #retain reference cols for analysis only
  dplyr::select(plot, old_plot, trt, meadow, meadow_alt, snow, snow_alt, sfcode2016, notes2016) %>%
  data.frame()

# after review, old_plot 96 is probably old_plot 296, so change descriptive info to match that
sdl_plots2[sdl_plots2$old_plot == 96 & !is.na(sdl_plots2$old_plot),c(1,3:ncol(sdl_plots2))] <- sdl_plots2[sdl_plots2$old_plot == 296 & !is.na(sdl_plots2$old_plot),c(1,3:ncol(sdl_plots2))]



# -- PREP DATASETS TO INCLUDE VERTICAL HIT LEVEL ----
# for datasets available: sdl 2016, nutnet2017 (that's it!)
nn17_wvert <- nutnet17raw %>%
  dplyr::select(-date) %>%
  mutate(block = paste0("B", block),
         yr = 2017) %>%
  #join treatment info
  left_join(nutnet_plots) %>%
  dplyr::select(site, yr, plotid, block, plot, n:trt, point, hit, species) %>%
  filter(!species == "O") %>%
  # replace species with USDA_code
  left_join(spplt, by = c("species" = "code")) %>%
  mutate(fxnl_grp = ifelse(grepl("forb", Growth_Habit, ignore.case = T), "Forb",
                           ifelse(grepl("Gram", Growth_Habit), "Grass", "Ground cover")),
         nutnet_grp = ifelse(clean_code2 == "SEDES", "Ground cover",
                             ifelse(grepl("forb", Growth_Habit, ignore.case = T), "Forb",
                                    ifelse(grepl("Gram", Growth_Habit), "Grass", "Ground cover")))) %>%
  dplyr::select(site:hit, clean_code2, simple_name, Common_Name, Category, Family, Growth_Habit, fxnl_grp, nutnet_grp)


sdl16_wvert <- sdl2016 %>%
  group_by(plot, point) %>%
  mutate(hit = seq(1, length(species), 1)) %>%
  ungroup() %>%
  # remove placeholders Tim inserted
  filter(species != "junk1") %>%
  # replace species with USDA_code
  left_join(spplt, by = c("species" = "code")) %>%
  mutate(fxnl_grp = ifelse(grepl("forb", Growth_Habit, ignore.case = T), "Forb",
                           ifelse(grepl("Gram", Growth_Habit), "Grass", "Ground cover")),
         nutnet_grp = ifelse(clean_code2 == "SEDES", "Ground cover",
                             ifelse(grepl("forb", Growth_Habit, ignore.case = T), "Forb",
                                    ifelse(grepl("Gram", Growth_Habit), "Grass", "Ground cover")))) %>%
  # join plot info
  left_join(plots2016) %>%
  mutate(site = "SDL",
         yr = 2016) %>%
  dplyr::select(site, yr, plot, Snow, Meadow, trt, recovery, X.1, point, hit, clean_code2, simple_name, Common_Name, Category, Family, Growth_Habit, fxnl_grp, nutnet_grp) %>%
  # clean up colnames and correct typo in recovery code
  rename(sf_recovery_notes = X.1,
         snow = Snow,
         meadow = Meadow,
         sf_recovery_code = recovery) %>%
  mutate(trt = ifelse(trt == "control", "C", trt),
         sf_recovery_code = ifelse(sf_recovery_code == 3 & grepl("recovery", sf_recovery_notes), 2, sf_recovery_code)) %>%
  arrange(plot, point, hit)



# -- FINISHING -----
# write out cleaned master plant dataset and site lookup datasets
# plant dats
write_csv(master_plant, "alpine_addnuts/output_data/sdl_nutnet_plantcom_allyrs.csv")
# nutnet site lookup
write_csv(nutnet_plots, "alpine_addnuts/output_data/nutnet_plot_lookup.csv")
# sdl site lookup
write_csv(sdl_plots2, "alpine_addnuts/output_data/sdl_plot_lookup.csv")
# cleaned up vertical hits datasets
write_csv(nn17_wvert, "alpine_addnuts/output_data/nutnet2017_vertical_sppcomp.csv")
write_csv(sdl16_wvert, "alpine_addnuts/output_data/sdl2016_vertical_sppcomp.csv")
