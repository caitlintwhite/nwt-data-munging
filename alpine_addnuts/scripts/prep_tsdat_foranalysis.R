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
library(readxl)
options(stringsAsFactors = F, na.strings = na_vals, strip.white = T)
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

# ctw-cleaned nutnet2013 data from nutnet/SCE (not tim)
nn13_sce <- read.csv("alpine_addnuts/output_data/nutnet2013_alldats/NWTnutnet_sppcomp_2013ongoing.csv") 
# extract plot info
nnplots_sce <- distinct(dplyr::select(nn13_sce, Block:FullTreatment))
  
## Sdl 
plot_codes <- read.csv(datfiles[grep("codes_99", datfiles)], strip.white = T, na.strings = na_vals)
sdl1997 <- read.csv(datfiles[grep("1997", datfiles)], strip.white = T, na.string = na_vals)
sdl2012 <- read.csv(datfiles[grep("2012", datfiles)], strip.white = T, na.strings = na_vals)
sdl2016 <- read.csv(datfiles[grep("2016", datfiles)], strip.white = T, na.strings = na_vals)
# ts 2016 codes sent 7/5
plots2016 <- read.csv(datfiles[grep("16_codes", datfiles)], strip.white = T, na.string = na_vals)

## spp lookup table
spplt <- read.csv("alpine_addnuts/output_data/sdl_nutnet_spplookup.csv", na.strings = na_vals, strip.white = T)

# "mystery" files from SCE
scefiles <- list.files(paste0(gsub("dry_meado_fert", "mystery_files", datpath)), full.names = T)
sdl12sppcomp <- read_excel(scefiles[grep("2012", scefiles)],trim_ws = T)
sdl2003 <- read_excel(scefiles[grep("03", scefiles)], trim_ws=T)
sffert_info <- read.csv(scefiles[grep("plotinfo", scefiles)]) #entered by ctw into spreadsheet, from pdf file "Plot locations and tag numbers snowfence fertilization plots"
# add in meadow and nutrient info
colordf <- data.frame(meadow_col = c("green", "blue", "orange"), meadow = c("dry", "mesic", "snowbed"))
fertdf <- data.frame(fert_symbol = c("circle", "plus", "x", "diamond"), ferttrt = c("C", "N", "P", "N+P"))
sffert_info <- left_join(sffert_info, colordf) %>%
  left_join(fertdf)

#sffert richness 1997 (to troubleshoot plots that don't match up)
sdlS97 <- getTabular(138) #not very helpful..



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
glimpse(sdl12sppcomp) # includes presence for richness!.. lots of unknowns too
glimpse(sdl2003) # no ground cover noted, but could use to assess geum rossii in 2003, uses old plot numbers
glimpse(sffert_info)

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
  # create nutnet code to preserve distinct unknown forb and unknown grass (there are 2 each in 2013)
  mutate(nutnet_code = ifelse(code %in% c("FORB1", "FORB2", "GRASS1", "GRASS2"), code, clean_code2)) %>%
  dplyr::select(yr, plotID, Block:trt, nutnet_code, clean_code2, hits) %>%
  grouped_df(colnames(.)[1:10]) %>%
  summarise(hits = sum(hits)) %>%
  arrange(Block, Plot, clean_code2) %>%
  # lower case all colnames
  rename_all(~ casefold(.)) %>%
  ungroup() %>%
  data.frame()

# build nutnet plot lookup table
nutnet_plots <- distinct(dplyr::select(nn13_tidy,plotid:trt))
# is it same as nutnet designations in 2013 data from sce?
summary(nnplots_sce == nutnet_plots[,2:ncol(nutnet_plots)]) #block different bc LH df has "B" before block #, otherwise the same

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
# sometimes it's +n, +p, -k; other times -n, -p, +k
# going by 2013, should be +n, +p, -k
# also from sce nutnet metadata notes, plot 7 across all blocks should have same treatment
unique(nn13_tidy$trt[nn13_tidy$plot == 7]) # correct, should be N+P (no K+)

# > decision: drop trt cols in 2017 dataset and re-join 2013 trt cols
nn17_tidy <- nn17_tidy %>%
  dplyr::select(-c(n,p,k, plotid, trt)) %>%
  left_join(distinct(dplyr::select(nn13_tidy, plotid:trt))) %>%
  # ms says canopy structure assessed in both 2013 and 2017, so going to assume vert summed in 2013
  # add in nutnet_code to preserve distinct unks (there is only unk1 and 2FORB in 2017 entered data.. will assume they are different)
  mutate(nutnet_code = ifelse(species == "unk1", "UNK1", clean_code2)) %>%
  dplyr::select(colnames(nn13_tidy)) %>%
  grouped_df(colnames(.)[1:10]) %>%
  summarise(hits = sum(hits)) %>%
  ungroup() %>%
  arrange(block, plot, clean_code2)





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

View(sdl_plots) # inconsistencies in snow and meadow in 2012 and 2016 yrs..

# finally, append ctw-entered sdl plot info (which should be the correct dataset to use.. SCE gave to ctw 10-9-19)
sdl_plots <- full_join(sdl_plots, sffert_info, by = c("plot" = "plot_num_tag")) %>%
  # append PDF to ctw-entered colnames so know origin of data (pdf of site layout)
  rename_at(vars(colnames(sffert_info)[2:ncol(sffert_info)]), function(x) paste0(x, "_PDF")) %>%
  # reorg colnames so cols that should match up are adjacent
  dplyr::select(plot:old_plot16, plot_ne_tag_PDF, plot_sw_tag_PDF, notes_PDF,
                meadowLT:meadow16.2, meadow_PDF,
                snowLT:notes16.2, snowfence_PDF,
                trtLT, trt97, fert12, trt16, fert16.2, ferttrt_PDF) # PDF meadow data are the best ones, and match up with 2016 data (tim-dbl check plot data also alighn with 2016), 2012 bad data entry

# write out for tim to review
write_csv(sdl_plots, "alpine_addnuts/output_data/sdl_plots_lookup_trblshoot.csv")



# -- PREP SDL PLANT DATA WITHOUT TRT INFO IN MATRICES (just plot num) -----
## 1997 -- deal with 999 (present but not hit), no unknowns
sdl97_tidy <- sdl1997 %>%
  # remove spp present but not hit and spp not hit
  subset(!hits %in% c(0, 999)) %>%
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


# sdl 2012 -- only 1 unk forb and 1 unk grass, so okay to use 2FORB, 2 GRASS
sdl12_tidy <- sdl2012 %>%
  # remove spp present only or spp absent
  subset(hits >= 1) %>%
  left_join(spplt[c("code", "clean_code2")], by = c("species" = "code")) %>%
  # add col for yr
  mutate(yr = 2012) %>%
  group_by(yr, plot, clean_code2) %>%
  summarise(hits = sum(hits)) %>%
  ungroup()


# sdl 2016 -- no unknowns
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
## 2 unk forbs and 2 unk grasses in nn13 data never in same plot so okay to use usda codes only

master_plant <- rbind(dplyr::select(nn13_tidy, -nutnet_code), dplyr::select(nn17_tidy, -nutnet_code)) %>%
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


# select plot and treatment from sdl (choose pdf data, then sdl 16.2 bc that is most recent info from TS)
sdl_plots2 <- sdl_plots %>%
  mutate(trt = ifelse(is.na(ferttrt_PDF), fert16.2, ferttrt_PDF),
         trt = ifelse(is.na(trt), trt97, trt),
         trt = ifelse(is.na(trt), fert12, trt),
         trt = casefold(trt, upper = T),
         trt = recode(trt, "NN" = "N", "CC" = "C", "CONTROL" = "C",
                      "PP" = "P", "NP" = "N+P"),
         snow = recode(snowfence_PDF, impacted = "snow", unimpacted = "no snow"),
         snow = ifelse(is.na(snow), sfcode16, snow),
         snow = ifelse(is.na(snow), site97, snow),
         snow = recode(snow, ND = "no snow", SD = "snow"),
         snow_notes = notes16.2,
         snow_notes = ifelse(is.na(snow_notes) & snow == "no snow", "never affected", snow_notes),
         meadow = ifelse(is.na(meadow_PDF),meadow16, meadow_PDF),
         meadow = ifelse(is.na(meadow), site97, meadow),
         meadow = recode(meadow, ND = "dry", SD = "dry")) %>%
  #retain reference cols for analysis only
  dplyr::select(plot, old_plot97:plot_sw_tag_PDF, trt, meadow, snow, snow_notes) %>%
  # remove 96 and 0 plots because 0s are only for 0 descae, and 96 = 296, will have only mystery 286 plot from 1997
  filter(!old_plot97 %in% c(0, 96)) %>%
  data.frame()
 


# -- PREP DATASETS TO INCLUDE VERTICAL HIT LEVEL ----
# for datasets available: sdl 2016, nutnet2017 (that's it!)
nn17_wvert <- nutnet17raw %>%
  # rbind block 3 plot 7 data
  rbind(nn17_b3p7) %>%
  # remove date
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


sdl16_wvert <- dplyr::select(sdl2016, -c(meadow, trt, sfcode)) %>%
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
  left_join(dplyr::select(sdl_plots2, plot, trt, meadow, snow, snow_notes)) %>%
  mutate(site = "SDL",
         yr = 2016) %>%
  dplyr::select(site, yr, plot, snow, snow_notes, meadow, trt, point, hit, clean_code2, simple_name, Common_Name, Category, Family, Growth_Habit, fxnl_grp, nutnet_grp) %>%
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
