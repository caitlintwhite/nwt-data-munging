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

# email from JGS to CTW 10/13:
# I started by looking for definitely not in DM or FF species but there’s at least one in both plots!!! I highlighted them in yellow in your spreadsheet. 
# It’s primarily Carex scopulorum (CASC12) in plot 11 and Deschampsia caespitosa (DECE) in plot 60.
# So, I looked at my old files and found my REU data!! However, I have different plot numbers and only one as 869 and it’s classified as DM inside the snowfence. 
# (Also super frustrating I have it as 869, 454 (NP) in my species cover file and 869, 954 (NP) in sp richness - I assume I mistook a 4 for a 9 or vice versa… 
# Anyway, maybe these numbers help? If not, l also compared sp comp and my 869,4/954 (NP) seems most similar to your plot 60 due to the presence of STLO2, TRSP2, and TRPAP, all species not present in plot 11. 
# I didn’t find DECE but I did find an unidentified grass sp., which could be DECE. Also, STLO2 appearing in plot 60 over time seems like a sign that it was changing to be more MM/SB-like…
# I’ve attached my sp cover and sp richness data files. Present species that weren’t hit aren’t included in cover but everything present should be listed in sp richness. 
# Also, doing the 100 point cover was taking me too long so I just did a subset. 
# Looking at the total number of hits I suspect I did 50 points and counted multiple hits per point where relevant.

# > jgs 869, 954 should be 869, 454 (looking at sf fert pdf)



# -- SETUP ----
rm(list = ls())
library(tidyverse)
library(readxl)
theme_set(theme_bw())
na_vals <- c("", " ", NA, "NA", "NaN", NaN, ".")
source("edi_functions.R")
options(stringsAsFactors = F, na.strings = na_vals, strip.white = T)

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
sdl12sce <- read_excel(scefiles[grep("2012", scefiles)],trim_ws = T)
sdl2003 <- read_excel(scefiles[grep("03", scefiles)], trim_ws=T)
sffert_info <- read.csv(scefiles[grep("plotinfo", scefiles)]) #entered by ctw into spreadsheet, from pdf file "Plot locations and tag numbers snowfence fertilization plots"
# add in meadow and nutrient info
colordf <- data.frame(meadow_col = c("green", "blue", "orange"), meadow = c("dry", "mesic", "snowbed"))
fertdf <- data.frame(fert_symbol = c("circle", "plus", "x", "diamond"), ferttrt = c("C", "N", "P", "N+P"))
sffert_info <- left_join(sffert_info, colordf) %>%
  left_join(fertdf)
rm(colordf, fertdf)

#sffert richness 1997 (to troubleshoot plots that don't match up)
sdlS97 <- getTabular(138) #colnames don't read in correctly, fix now
names(sdlS97)
names(sdlS97)[grep("X", names(sdlS97))] <- NA
sdlS97[nrow(sdlS97)+1,] <- data.frame(t(names(sdlS97)))
# manually assign names from online metadata
names(sdlS97) <- c("yr", "loc", "trt", "plot", "sppS", "grass_wgt_rep1", "forb_wgt_rep1", "total_rep1", "grass_rep2", "forb_rep2", "total_rep2")
# plot info from knb-lter-nwt.138 metadata (on culter)
sdl97_plotinfo <- read.csv(scefiles[grep("metadata", scefiles)]) %>%
# quick clean up to remove parentheses and other punctuation from tag numbers
  mutate_at(c("plot", "old_plot", "replace_tag_2002"), parse_number) %>%
  arrange(plot)

# jgs 2005 data
jgs_files <- list.files(gsub("dry_meado_fert", "jgs_reu_2005", datpath), full.names = T)
# cover only has 1 data sheet
jgs_cover <- read_excel(jgs_files[grep("cover", jgs_files, ignore.case = T)], col_names = F)
# richness has multiple data sheets
jgs_richness_sheets <- excel_sheets(jgs_files[grep("rich", jgs_files, ignore.case = T)]) 
jgs_rich <- list()
for(i in 1:length(jgs_richness_sheets)){
  jgs_rich[[i]] <- read_excel(jgs_files[grep("rich", jgs_files, ignore.case = T)], sheet = i, col_names = F)
  names(jgs_rich)[i] <- jgs_richness_sheets[i]
}  

# text metadata for 1996/97 ANPP + richness sdl dataset
metadat96 <- readLines("http://nwt.colorado.edu/meta_data/saddferb.ts.meta.txt")




# -- REVIEW DATA -----
# nutnet
glimpse(nutnet13) # wide form, hits
glimpse(nn13_sce) # date is a character
glimpse(nutnet17) # long-form, summarized (rel and abs cov)
glimpse(nutnet17raw) # long form, raw hits at each grid pt (need to be summed to plot level), date as string, no dashes
glimpse(nn17_b3p7) # same as nutnet17_raw, date as string, no dashes
glimpse(sdlS97) # need to correct cols to numeric
# checks to see which cols are truly all numeric
sapply(sdlS97, function(x)sort(unique(x))) # plot has XX1 and XX2
sdlS97 <- mutate_at(sdlS97, vars(colnames(sdlS97)[!grepl("plot|trt|loc", colnames(sdlS97))]), as.numeric) %>%
  arrange(yr, trt, loc, plot)

# correct nn13sce and nn17 dates
nn13_sce$Date <- as.Date(nn13_sce$Date)
nutnet17raw <- mutate(nutnet17raw, date = as.Date(paste(substr(date,1,1), substr(date,2,3), "2017", sep = "-"), format = "%m-%d-%Y"))
nn17_b3p7 <- mutate(nn17_b3p7, date = as.Date(paste(substr(date,1,1), substr(date,2,3), "2017", sep = "-"), format = "%m-%d-%Y"))

# sdl
glimpse(sdl1997) # date read in as numeric string (format is yymmdd), long form total hits
glimpse(sdl2012) # no date, total hits, long form
glimpse(sdl2016) # no hits, just presence (spp noted at grid point) -- only top hits
glimpse(sdl12sce) # includes presence for richness!.. lots of unknowns too
glimpse(sdl2003) # no ground cover noted, but could use to assess geum rossii in 2003, uses old plot numbers
glimpse(sffert_info)
glimpse(jgs_cover)
glimpse(jgs_rich) # date is in second row, 5 sheets total

# correct 1997 date
sdl1997 <- mutate(sdl1997, date = as.Date(paste(substr(date,3,4), substr(date,5,6), "1997", sep = "-"), format = "%m-%d-%Y"))

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
  ungroup() %>%
  # bring in date (strip B from block to join)
  mutate(Block = parse_number(Block)) %>%
  left_join(distinct(nn13_sce[c("Date", "Block", "Plot")])) %>%
  arrange(Block, Plot, clean_code2) %>%
  # add B back to block
  mutate(Block = paste0("B", Block)) %>%
  # lower case all colnames
  rename_all(~ casefold(.)) %>%
  #reorder cols
  dplyr::select(yr, date, plotid:hits) %>%
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
  grouped_df(colnames(.)[!names(.) == "hits"]) %>%
  summarise(hits = sum(hits)) %>%
  ungroup() %>%
  arrange(block, plot, clean_code2)

# add site to nutnet lookup
nutnet_plots <- mutate(nutnet_plots, site = "nutnet") %>%
  dplyr::select(site, plotid:ncol(.))



# -- SDL DATA PREP -----
# -- troubleshoot sdl site info -----
# for all of these, keep plot/old_plot, species, and hits, use plot_codes table to crosswalk/standardize all
# start with datasets Tim sent to CTW
# lower case plot_codes (97 crosswalk codes from Tim)
## > note 4 of tim's codes don't exist in jane's dataset or on PDF (old plots 291, 263, 300 or 285; corresponding new plot numbers have different old plot numbers in EDI dataset)
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
colnames(sdl_plots)[(pos+1):ncol(sdl_plots)] <- paste0(colnames(sdl_plots)[(pos+1):ncol(sdl_plots)], "97LT")
# > none of the old_plot code in the plot_code table match up with old_plot in the sdl16 dataset!!
# > BUT! trtment and snow cols match up so that's good. be sure to ignore old. in 2016 dataset when tidying it

# join 2012 plot data (from tim seastedt, not sce)
sdl_plots <- full_join(sdl_plots, distinct(dplyr::select(sdl2012, meadow:plot)), by = "plot")
#id pos that ends with last concatenation
pos <- max(grep("97LT", colnames(sdl_plots)))
# append 12 to colnames added to keep track of origin
colnames(sdl_plots)[(pos+1):ncol(sdl_plots)] <- paste0(colnames(sdl_plots)[(pos+1):ncol(sdl_plots)], "12TS")
View(sdl_plots) # there are some plots in the 2012 dataset that aren't in the 2016 dataset..


# join sdl 1997 (from tim, not from EDI)
sdl_plots <- full_join(sdl_plots, dplyr::select(sdl1997, old_plot, site, trt), by = c("old_plot97LT" = "old_plot"))
#id pos that ends with last concatenation
pos <- max(grep("12", colnames(sdl_plots)))
# append 97 to colnames added to keep track of origin
colnames(sdl_plots)[(pos+1):ncol(sdl_plots)] <- paste0(colnames(sdl_plots)[(pos+1):ncol(sdl_plots)], 97)


# reorg colnames so cols that should match up are adjacent
sdl_plots <- dplyr::select(sdl_plots,
                           plot, old_plot97LT, old_plot16, 
                           meadow97LT, meadow12TS, meadow16, meadow16.2,
                           snow97LT, site97, snow12TS, snow16.2, sfcode16, recovery16.2, notes16.2,
                           trt97LT, trt97, fert12TS, trt16, fert16.2) %>%
  arrange(plot) %>%
  distinct()
#rm extra old plotnums in the 2016 that pair with plot 14 and aren't 287 since there is a match for that
# look like "287" got copied down in excel but sequence number went up when really it should have been 287 the entire time
sdl_plots <- sdl_plots %>%
  filter((plot == 14 & old_plot16 == 287) |
           plot != 14 |
           is.na(plot)) %>%
  #mutate(old_plot97LT = ifelse(is.na(plot), NA, old_plot97LT)) %>%
  dplyr::select(plot, old_plot97LT, old_plot16:ncol(.))

View(sdl_plots) # inconsistencies in snow and meadow in 2012 and 2016 yrs..

# finally, append ctw-entered sdl plot info (which should be the correct dataset to use.. SCE gave to ctw 10-9-19)
sdl_plots <- full_join(sdl_plots, sffert_info, by = c("plot" = "plot_num_tag")) %>%
  # append PDF to ctw-entered colnames so know origin of data (pdf of site layout)
  rename_at(vars(colnames(sffert_info)[2:ncol(sffert_info)]), function(x) paste0(x, "_PDF")) %>%
  # reorg colnames so cols that should match up are adjacent
  dplyr::select(plot:old_plot16, plot_ne_tag_PDF, plot_sw_tag_PDF, notes_PDF,
                meadow97LT:meadow16.2, meadow_PDF,
                snow97LT:notes16.2, snowfence_PDF,
                trt97LT, trt97, fert12TS, trt16, fert16.2, ferttrt_PDF) # PDF meadow data are the best ones, and match up with 2016 data (tim-dbl check plot data also alighn with 2016), 2012 bad data entry

# write out for tim to review -- [resolved oct 2019]
# write_csv(sdl_plots, "alpine_addnuts/output_data/sdl_plots_lookup_trblshoot.csv")


# after manually checking data, don't trust sdl12 and sdl16 trt info from TS (16.2 okay, 16 not, is inconsistent with PDF)
# old plot 16 has typo in 489 (is 498.. there are two 498s, should match with sw tag)
# seems like old plot 16 should match with sw tag, and old plot 97 should match with ne tag

# add in 1997 metadata
sdl_plots2 <- left_join(sdl_plots, sdl97_plotinfo, by = "plot") %>%
  #append source colname
  rename_at(vars(colnames(sdl97_plotinfo)[!names(sdl97_plotinfo) == "plot"]), function(x) paste0(x, "_edi138"))

# prep 96 plots to add in
sdl96_sites <- distinct(sdlS97[sdlS97$yr == 1996, c("plot", "loc", "trt")]) %>%
  rename_all(function(x) paste0(x, "96")) %>%
  # add col to translate plot nums to 97 codes per metadata (all same except XX plots)
  # also correct typo for plot 283 (should be 083 per metadata and sdl03 info)
  mutate(plot97 = recode(plot96, XX1 = "870", XX2 = "397", `283` = "83"))

# append 1996 plot codes to master site lookup
sdl_plots2 <- merge(sdl_plots2, sdl96_sites, by.x = as.character("old_plot_edi138"), by.y = "plot97", all = T)
  
# how well does 97 metadata match other cols?
split(sdl_plots2[colnames(sdl_plots2)[grepl("meadow|loc", colnames(sdl_plots2)) & !grepl("138", colnames(sdl_plots2))]], sdl_plots2$meadow_edi138) # all same, wet = snowbed = moist
split(sdl_plots2[colnames(sdl_plots2)[grepl("trt", colnames(sdl_plots2)) & !grepl("138", colnames(sdl_plots2))]], sdl_plots2$trt_edi138) # all same
split(sdl_plots2[colnames(sdl_plots2)[grepl("snow|loc", colnames(sdl_plots2)) & !grepl("138", colnames(sdl_plots2))]], sdl_plots2$snow_edi138) # same except for sdl12 data, but don't trust that source anyway

# clean up
# select plot and treatment from sdl (choose edi meta, then, pdf data, then sdl 16.2 bc that is most recent info from TS)
sdl_plots2 <- sdl_plots2 %>%
  mutate(trt = ifelse(is.na(trt_edi138), ferttrt_PDF, trt_edi138),
         trt = ifelse(is.na(trt), fert16.2, trt),
         trt = ifelse(is.na(trt), trt97, trt),
         #trt = ifelse(is.na(trt), trt96, trt),
         trt = recode(trt, "NN" = "N", "CC" = "C", "CONTROL" = "C",
                      "PP" = "P", "NP" = "N+P"),
         snow = ifelse(is.na(snow_edi138), snowfence_PDF, snow_edi138),
         snow = recode(snow, impacted = "snow", unimpacted = "no snow"),
         snow = ifelse(is.na(snow), sfcode16, snow),
         snow = ifelse(is.na(snow), site97, snow),
         snow = recode(snow, ND = "no snow", SD = "snow"),
         snow_notes = notes16.2,
         snow_notes = ifelse(snow == "snowfield", "in snowfield",
                             ifelse(is.na(snow_notes) & snow == "no snow", "never affected", snow_notes)),
         # recode snowfield to snow, since snowfield explanation in snow_notes
         snow = recode(snow, snowfield = "snow"),
         meadow = ifelse(is.na(meadow_edi138), meadow_PDF, casefold(meadow_edi138)),
         meadow = ifelse(is.na(meadow),meadow16, meadow),
         meadow = ifelse(is.na(meadow), site97, meadow),
         meadow = recode(meadow, ND = "dry", SD = "dry")) %>%
  # rename plot96 to old_plot
  rename(old_plot96 = plot96) %>%
  #retain reference cols for analysis only
  dplyr::select(plot, old_plot_edi138, old_plot96, old_plot97LT, replace_tag_2002_edi138, old_plot16:plot_sw_tag_PDF, trt, meadow, snow, snow_notes) %>%
  # remove0 plots because 0s are only for 0 descae, will have only mystery 286 plot from 1997
  filter(!old_plot97LT %in% c(0, 96)) %>%
  # sort by plot
  arrange(plot) %>%
  data.frame()


# evaluate SCE 2012 and EDI 1997 datasets
# join 2012 plot data from SCE
sdl12sce_sites <- dplyr::select(sdl12sce, tmt_code:old_plot_num) %>% distinct()
# which column matches old plot in sdl12 sce best?
sapply(sdl_plots2[,2:7], function(x) summary(x %in% sdl12sce_sites$old_plot_num)) # pairs on edi and sw tag the best
sce12_x_sffert <- left_join(sdl12sce_sites[c("old_plot_num", "plot_num")], sdl_plots2, by = c("old_plot_num" = "old_plot_edi138")) %>%
  mutate(check = plot_num == plot)
summary(sce12_x_sffert$check) # missing: 283 pairs with 30, but 283 not a number in edi metadata..
# is it anywhere?
sapply(sdl_plots2, function(x) summary(x == 283)) #so far no..
sdl_plots2[sdl_plots2$plot == 30,] #edi num is 83.. could be that 283 is typo? is mesic, P, no snow
sdl12sce_sites[sdl12sce_sites$plot_num == 30, ] # yes, also mm, no snow and P

# preserve spatial info for sites, just in case anyone ever wanted it
# id where plot info starts
sfpos <- grep("Dry sites.*Mesic sites", metadat96) # first is non snowfence, second is snowfence
sbedpos <- grep("Wet Meadow", metadat96) + 1 # sites start one slot after descrip

sfen <- metadat96[sfpos[1]:sbedpos]
sfen <- sfen[!grepl("[a-z]", sfen)]
sfen <- strsplit(sfen, "  ")
xydat <- data.frame()
for(i in 1:length(sfen)){
  tempdat <- data.frame(info = sfen[[i]]) %>% filter(!info == "") %>%
  mutate(x = str_extract(info, "[0-9]+,"),
         x = parse_number(x),
         y = str_extract(info, "[0-9]{0,3}.{0,1}[0-9]+[-][0-9]+.{0,1}[0-9]{0,3}"),
         plot = str_extract(info, "[(][0-9]+"),
         plot = parse_number(plot)) %>%
    separate(y, c("ymin", "ymax"), "-") %>%
    mutate(trt = info[1])
  # clean up
  tempdat <- subset(tempdat, info != unique(trt)) %>%
    mutate(trt = recode(trt, NN = "N", PP = "P", CC = "C", NP = "N+P"))
  # add to master
  xydat <- rbind(xydat, tempdat)
}
# clean up problem plots
goodxy <- xydat[complete.cases(xydat),]
problemxy <- anti_join(xydat, goodxy) %>%
  # remove any empty cells
  filter(nchar(info)>1)
# need to pair every two rows
for(i in seq(2,nrow(problemxy),2)){
  problemxy$info[i-1] <- paste(problemxy$info[i-1],problemxy$info[i])
  problemxy$plot[i-1] <- problemxy$plot[i]
}
# clean up
problemxy <- problemxy[complete.cases(problemxy),]
goodxy <- rbind(goodxy, problemxy)
goodxy$info <- trimws(goodxy$info)
goodxy <- mutate_at(goodxy, c("x", "ymin", "ymax", "plot"), as.numeric)
# to be sure, are all extracted numbers in the info col?

# no snowbed spatial info, only new plot number and old plot number
# pair spatial info to master sdl site LT
sdl_plots2 <- left_join(sdl_plots2, goodxy, by = c("plot", "trt")) 


# -- tidy jgs sdl 05 spp comp + richness ----
# cover needs to be split
View(jgs_cover) 
# NAs in col 1 split the different community datasets, spp colnames are the same
# 2nd row within community datasets = name of meadow
comm_pos <- which(is.na(jgs_cover$...1))
# stack spp data
jgs_sppdat <- data.frame()
for(i in 1:length(comm_pos)){
  if(i < length(comm_pos)){
    tempdat <- jgs_cover[comm_pos[i]:(comm_pos[i+1]-1),]
  }else{
    # last one
    tempdat <- jgs_cover[comm_pos[i]:nrow(jgs_cover),]
  }
  colnames(tempdat) <- c("plot", "surveydate", tempdat[1,3:ncol(tempdat)])
  tempdat$meadow <- tempdat$plot[2]
  # remove first two rows (spp names and meadow type) bc stored in colnames or meadow col
  tempdat <- data.frame(tempdat[3:nrow(tempdat),])
  # spp start in 3rd col (plot first, survey date 2nd)
  tempdat <- gather(tempdat, code, hits, 3:(ncol(tempdat)-1)) %>%
    # remove spp not hit
    filter(!is.na(hits)) %>%
    # convert date from numeric string to date
    mutate(surveydate = as.Date(as.numeric(surveydate), origin = "1899-12-30"))
  # rbind to master
  jgs_sppdat <- rbind(jgs_sppdat, tempdat)
}
glimpse(jgs_sppdat)
sapply(jgs_sppdat, function(x) sort(unique(x)))
# clean up site number formatting (inconsistent spacing after commas)
jgs_sppdat$plot <- gsub(",|, ",", ", jgs_sppdat$plot)

# break out plot numbers
jgs_sites <- unique(jgs_sppdat$plot)
jgs_sitelist <- strsplit(unique(jgs_sppdat$plot), " ")
jgs_sites <- cbind(jgs_sites, matrix(nrow = length(jgs_sites), ncol = 10))
for(i in 1:nrow(jgs_sites)){
  temprow <- t(jgs_sitelist[[i]])
  for(r in 1:ncol(temprow)){
    jgs_sites[i,r+1] <- str_extract(temprow[[r]],"[0-9]+")
  }
}
# clean up site info
jgs_sites <- as.data.frame(jgs_sites)
# get rid of any cols that are all NA
jgs_sites <- jgs_sites[apply(jgs_sites, 2, function(x) any(!is.na(x)))]
names(jgs_sites) <- c("plot", "plot_num1", "plot_num2", "plot_num3", "plot_num4")
# convert tag numbers to numeric (to pair with sdl_plots)
jgs_sites[grepl("plot_num", names(jgs_sites))] <- sapply(jgs_sites[grepl("plot_num", names(jgs_sites))], as.numeric)
jgs_sites$trt <- str_extract(jgs_sites$plot, "[:alpha:]+")

# extract richness - ignore dates, just compile spp present
jgs_spppresent <- data.frame()
for(i in jgs_richness_sheets){
  tempdat <- jgs_rich[[i]]
  if(nrow(tempdat) == 0){
    next
  }
  for(co in 1:ncol(tempdat)){
    temppres <- tempdat[,co]
    temppres <- na.omit(temppres)
    names(temppres) <- "code"
    surveyplot <- temppres$code[1]
    temppres$plot <- surveyplot
    temppres <- temppres[2:nrow(temppres),]
    # find date rows
    daterows <- grep("^[0-9]+$", temppres$code)
    temppres$surveydate <- temppres$code[daterows[1]]
    temppres$surveyevent <- 1
    # iterate through each date
    if(length(daterows)>1){
      for(d in 2:length(daterows)){
        temppres$surveydate[daterows[d]:nrow(temppres)] <- temppres$code[daterows[d]] # will just keep updating rows (writing over previous) until there are no more dates, so could work for more than 2 survey dates
        temppres$surveyevent[daterows[d]:nrow(temppres)] <- d
      }
    }
    # clean up
    temppres <- filter(temppres, !code %in% c(unique(temppres$surveydate), NA))
    temppres$surveydate <- as.Date(as.numeric(temppres$surveydate), origin = "1899-12-30")
    temppres$hits = 0.25
    temppres$meadow <- i
    
    # add to master
    jgs_spppresent <- rbind(jgs_spppresent, temppres)
  }
}
# standardize plot names to how entered in spp comp
jgs_spppresent$plot <- gsub(" [/] |,", ", ", jgs_spppresent$plot)

# do all of jane's plots in spp present dataset have match in spp comp dataset?
summary(sort(unique(jgs_spppresent$plot) %in% sort(unique(jgs_sppdat$plot)))) # 1 false..
unique(jgs_spppresent$plot)[!unique(jgs_spppresent$plot) %in% jgs_sppdat$plot] # "869, 954 (NP)" doesn't have match..
# jgs says 954 should be 454
unique(jgs_sppdat$plot)[!unique(jgs_sppdat$plot) %in% jgs_spppresent$plot] # corresponding plot that doesn't match is "869, 454 (NP)"
# correct richness plot to 454
jgs_spppresent$plot <- gsub("869, 954", "869, 454",jgs_spppresent$plot)


# rbind cover and presence data
# check cover only surveyed 1 date
distinct(jgs_sppdat[c("surveydate", "plot")]) %>%
  group_by(plot) %>%
  summarise(nobs = length(surveydate)) %>%
  summary() # only once
jgs_master <- jgs_sppdat %>%
  mutate(surveyevent = 1) %>%
  #reorder cols
  dplyr::select(plot, surveydate, surveyevent, meadow, code, hits) %>%
  rbind(jgs_spppresent[colnames(.)]) %>%
  # add standardized spp code
  left_join(spplt[c("code", "clean_code2")]) %>%
  # sort by plot, date and clean code
  arrange(plot, clean_code2, surveydate) %>%
  mutate(hits = as.numeric(hits),
         dataset = ifelse(hits == 0.25, "richness", "sppcomp"))
# assess spp that don't have match in clean_code2
sort(unique(jgs_master$code[is.na(jgs_master$clean_code2)]))
# periods introduced.. annoying, and dates in others
# correct dates
jgs_master[grepl("2005", jgs_master$code),]
jgs_master$surveydate[jgs_master$plot == "412, 453 (PP)"]
jgs_master$surveydate[jgs_master$plot == "412, 453 (PP)"] <- unique(jgs_master$surveydate[jgs_master$plot == "412, 453 (PP)" & !is.na(jgs_master$surveydate)])
# remove rows with dates as codes
jgs_master <- filter(jgs_master, !grepl("2005", code))
for(i in sort(unique(jgs_master$code[is.na(jgs_master$clean_code2)]))){
  searchcode <- trimws(gsub("[.]", " ", i))
  replacecode <- unique(spplt$clean_code2[grepl(searchcode, gsub("[.]", "", spplt$code), ignore.case = T)])
  print(paste("Swapping", i, "for", replacecode))
  jgs_master$clean_code2[jgs_master$code == i] <- replacecode
}

# review dataset
summary(jgs_master)
sapply(jgs_master, function(x)sort(unique(x)))

# keep whichever cover value is greatest per spp per plot
jgs_master2 <- jgs_master %>%
  distinct() %>% # there are some duplicates for salix richness in one plot?
  arrange(plot, clean_code2, desc(hits)) %>%
  group_by(plot, clean_code2) %>%
  mutate(maxval = ifelse(hits == max(hits), 1, 0)) %>%
  ungroup() %>%
  # only keep max cover val
  filter(maxval == 1) %>%
  # join treatment info
  left_join(jgs_sites) %>%
  # add snow info
  mutate(snow = ifelse(grepl("Snow", meadow, ignore.case = T), "snow", "no snow"),
         meadow = ifelse(grepl("Dry", meadow, ignore.case = T), "dry", "mesic")) %>%
  # remove unneeded cols
  dplyr::select(plot, plot_num1:plot_num4, surveydate, meadow, snow, trt, clean_code2, hits)


# extract site info from jgs dataset to pair with sdl site info
jgs_sites <- left_join(jgs_sites, distinct(jgs_master2[c("plot", "trt", "meadow", "snow")]))

# find out which col #s match the best
sapply(sdl_plots2[,1:7], function(x) summary(jgs_sites$plot_num1[!is.na(jgs_sites$plot_num1)] %in% x))
# num 1 pairs best with edi, then sw tag #, but some pairs across all available numbers to match on
sapply(sdl_plots2[,1:7], function(x) summary(jgs_sites$plot_num2[!is.na(jgs_sites$plot_num2)] %in% x))
# num 2 pairs best with ne tag #, 4 numbers pair with sw tag
sapply(sdl_plots2[,1:7], function(x) summary(jgs_sites$plot_num3[!is.na(jgs_sites$plot_num3)] %in% x))
# 2 matches in ne tag/old 16, 1 match with sw tag
sapply(sdl_plots2[,1:7], function(x) summary(jgs_sites$plot_num4[!is.na(jgs_sites$plot_num4)] %in% x))
# num 4 pairs with nothing

# test pair on edi knb-lter-nwt.138 plot number
jgs_sitematch <- rename(jgs_sites, jgs_plot = plot) %>%
  as.data.frame() %>%
  mutate(trt = recode(trt, NN = "N", NP = "N+P", PP = "P", CC = "C")) %>%
  left_join(sdl_plots2, by= c("plot_num1" = "old_plot_edi138", "trt", "snow", "meadow"))
# does plot_num2 pair with also pair with ne tag for paired sites?
summary(jgs_sitematch$plot_num2 == jgs_sitematch$plot_ne_tag_PDF) # mostly, yes
# what jgs plots don't have pair?
jgs_sitematch$jgs_plot[is.na(jgs_sitematch$plot)] # only 2!
# are plot_num2 in other cols?
sapply(sdl_plots2, function(x) summary(jgs_sitematch$plot_num2[is.na(jgs_sitematch$plot)] %in% x)) # they are in NE tag

for(i in jgs_sitematch$plot_num2[is.na(jgs_sitematch$plot) & !is.na(jgs_sitematch$plot_num2)]){
  sdlne <- subset(sdl_plots2, plot_ne_tag_PDF == i)
  num2 <- subset(jgs_sitematch, plot_num2 == i)
  compare <- all(num2[c("plot_num2", "trt", "meadow", "snow")] == sdlne[c("plot_ne_tag_PDF", "trt", "meadow", "snow")])
  if(compare){
      jgs_sitematch[jgs_sitematch$plot_num2 == i & !is.na(jgs_sitematch$plot_num2), c("plot", "plot_ne_tag_PDF")] <- sdlne[c("plot", "plot_ne_tag_PDF")]
    }
}
summary(jgs_sitematch) # no more NAs in plot


# clean up environment (remove unneeded)
rm(temppres, temprow, tempdat, sdlne, num2)



# -- tidy sdl 2003 spp comp + richness ----
# not sure if suveyor (colin tucker, bill bowman's reu student) truly counted all ground cover classes or not (e.g. see lichen and moss, but no rock or bare ground)
# look at colsums..
apply(sdl2003[,4:64], 2, sum, na.rm = T) # some spp have cols, but never hit..
sdl2003_tidy <- dplyr::select(sdl2003, -sum) %>% # drop sum cover column
  gather(code, hit, 4:ncol(.)) %>%
  # remove spp not hit
  filter(!is.na(hit)) %>%
  left_join(spplt[c("code", "clean_code2")]) %>%
  arrange(plot, clean_code2) %>%
  mutate(treatment = recode(treatment, CC="C", NN = "N", PP = "P", NP = "N+P")) %>%
  rename(plot_2003 = plot,
         trt = treatment)

# try to match sites based on edi meteadata
sdl03_sites <- distinct(sdl2003_tidy[c("plot_2003", "trt")]) %>% arrange(plot_2003) %>%
  left_join(sdl_plots2, by = c("plot_2003" = "old_plot_edi138", "trt"))
# did everything match?
summary(is.na(sdl03_sites$plot)) # 4 didn't
# two in sw tag, one in ne
# are any in jane's?
sapply(jgs_sitematch[,2:5], function(x) summary(sdl03_sites$plot_2003[is.na(sdl03_sites$plot)] %in% x))
# 2 apiece

for(i in sdl03_sites$plot_2003[is.na(sdl03_sites$plot)]){
  jgstemp <- subset(jgs_sitematch, grepl(i, jgs_plot))
  temptrt <- sdl03_sites$trt[sdl03_sites$plot_2003 == i]
  # try pair on plot_num1
  if(i == jgstemp$plot_num1 & temptrt == jgstemp$trt){
     sdl03_sites[sdl03_sites$plot_2003 == i, c("plot", "meadow", "snow")] <- jgs_sitematch[jgs_sitematch$plot_num1 == i & !is.na(jgs_sitematch$plot_num1), c("plot", "meadow", "snow") ]
  }
  # try pair on plot_num2
  if(i == jgstemp$plot_num2 & temptrt == jgstemp$trt){
    sdl03_sites[sdl03_sites$plot_2003 == i, c("plot", "meadow", "snow")] <- jgs_sitematch[jgs_sitematch$plot_num2 == i & !is.na(jgs_sitematch$plot_num2), c("plot", "meadow", "snow")]
  }
}

summary(is.na(sdl03_sites$plot)) # all have pair

# pair current plot to sdl2003_tidy
sdl2003_tidy <- left_join(sdl2003_tidy, sdl03_sites[c("plot_2003", "plot", "trt", "meadow", "snow")]) %>%
  # reorder cols and sort
  dplyr::select(date, plot_2003, plot, trt, meadow, snow, clean_code2, hit)


# -- finalize sdl lookup table -----
sdl_sitesLT <- sdl_plots2 %>%
  full_join(jgs_sitematch[c("plot", "jgs_plot", "plot_num1", "plot_num2", "plot_num3", "plot_num4")]) %>%
  rename_at(names(jgs_sitematch)[grepl("_num[0-9]", names(jgs_sitematch))], function(x) paste0("jgs_",x)) %>%
  # join sdl 2003
  full_join(sdl03_sites[c("plot", "plot_2003")]) %>%
  #reorder cols
  dplyr::select(plot, trt, meadow, snow, snow_notes, plot_sw_tag_PDF, plot_ne_tag_PDF, info, x, ymin, ymax, old_plot_edi138:replace_tag_2002_edi138, plot_2003, jgs_plot:jgs_plot_num4, old_plot16) %>%
  rename(metadata_edi138 = info)
# 286 still needs to be resolved..

# clean up environment
rm(sce12_x_sffert, sdl_plots, plots2016, jgstemp, sdl97_sites, sfen, xydat, trtcheck, problemxy)



# -- PREP SDL PLANT DATA WITHOUT TRT INFO IN MATRICES (just plot num) -----
# > keep survey date info if available!

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

# see if can ID mystery 286 plots based on spp richness data
richness97 <- sdl1997 %>%
  # remove spp present but not hit and spp not hit
  subset(hits != 0) %>%
  #correct plot 96 typo
  mutate(old_plot = ifelse(old_plot == 96, 296, old_plot)) %>%
  left_join(spplt[c("code", "clean_code2")], by = c("species" = "code")) %>%
  # join treatment info
  left_join(plot_codes) %>%
  dplyr::select(plot, old_plot, snow, meadow, trt, species, clean_code2) %>%
  distinct() %>%
  #infill snow and meadow for 286 (SD)
  mutate(snow = ifelse(old_plot == 286, "SNOW", snow),
         meadow = ifelse(old_plot == 286, "Dry", meadow))
S97 <- richness97 %>%
  filter(!grepl("^2", clean_code2)) %>%
  group_by(plot, old_plot, snow, meadow, trt) %>%
  #summarise(S = length(unique(clean_code2))) %>%
  summarise(S = length(species)) %>%
  ungroup() %>%
  mutate(old_plot = as.character(old_plot)) %>%
  left_join(sdlS97[sdlS97$yr == 1997,], by = c("old_plot" = "plot")) 
# .. i have no idea how sppS in the EDI dataset was calculated, i've tried different ways to calcuate (i.e. not as cleanly, including non-veg) and still don't get as high of numbers as in EDI dataset
# i would maybe not post those data?
# still can't figure out what 286 matches up to..
# check which plot codes are missing from 96/97 dataset that match NN trt (286 is supposed to be snow and dry meadow, but maybe-misentered?)
checkNN <- sort(sdl97_plotinfo$old_plot[!sdl97_plotinfo$old_plot %in% S97$old_plot & sdl97_plotinfo$trt == "NN"])
# what are spp abundances in 286?
sdl1997[sdl1997$old_plot == 286,] %>%
  arrange(hits)

# > *keep* 286 and press on with reorganizing cols for standardization
# > drop site descrip cols until tim gets back to me on correct site descriptions
sdl97_tidy <- mutate(sdl97_tidy, plot = ifelse(is.na(plot), old_plot, plot)) %>%
  dplyr::select(yr, date, plot, clean_code2, hits) %>%
  # sum hits by clean spp code
  grouped_df(colnames(.)[1:4]) %>%
  summarise(hits = sum(hits)) %>%
  arrange(plot, clean_code2) %>%
  ungroup()


# sdl 2012 -- only 1 unk forb and 1 unk grass, so okay to use 2FORB, 2 GRASS
sdl12_tidy <- sdl2012 %>%
  # remove spp present only or spp absent
  subset(hits >= 1) %>%
  left_join(spplt[c("code", "clean_code2")], by = c("species" = "code")) %>%
  # add col for yr and date (NA, no info)
  mutate(yr = 2012, 
         date = NA) %>%
  group_by(yr, date, plot, clean_code2) %>%
  summarise(hits = sum(hits)) %>%
  ungroup()

# check sce 2012 data
sdl12sce_tidy <- sdl12sce %>%
  gather(code, hits, `2BARE`:ncol(.)) %>%
  # remove spp that weren't hit
  filter(hits > 0) %>%
  left_join(spplt[c("code", "clean_code2")], by = c("code")) %>%
  rename(yr = `year`,
         plot = plot_num)%>%
  mutate(hits = as.numeric(hits))
# need to manually correct some spp names that didn't match..
needs_usda <- sort(unique(sdl12sce_tidy$code[is.na(sdl12sce_tidy$clean_code2)]))
needs_usda
sdl12sce_tidy$clean_code2[sdl12sce_tidy$code == "2SCATE"] <- "2SCAT"
sdl12sce_tidy$clean_code2[sdl12sce_tidy$code == "DROCH"] <- "DROC"
sdl12sce_tidy$clean_code2[sdl12sce_tidy$code == "CARSCS2"] <- unique(spplt$clean_code2[spplt$code == "CARSCO"])
# everything else can stay the same
sdl12sce_tidy <- mutate(sdl12sce_tidy, clean_code2 = ifelse(is.na(clean_code2), code, clean_code2)) %>%
group_by(yr, plot, old_plot_num, tmt_code, veg_class, snow, clean_code2) %>%
  summarise(hits = sum(hits)) %>%
  ungroup() %>%
  mutate(plot = as.numeric(plot)) %>%
  arrange(plot, clean_code2)

check2012 <- full_join(sdl12sce_tidy, sdl12_tidy, by = c("yr", "plot", "clean_code2")) %>%
  mutate(diff = hits.x - hits.y) %>%
  arrange(plot, clean_code2) # in manual scan, pretty much the same (all unk careces became juncus)
# double check that plot codes match up
check2012_plots <- distinct(sdl12sce[c("plot_num", "old_plot_num")]) %>%
  rename(plot = plot_num, old_plot12 = old_plot_num) %>%
  mutate(plot = as.numeric(plot)) %>%
  left_join(sdl_sitesLT)
# does it match edi 138 numbers?
summary(check2012_plots$old_plot12 == check2012_plots$old_plot_edi138) #1 false.. 
subset(check2012_plots, old_plot12 != old_plot_edi138) # which is 283 (typo for 83), so all okay
# clean up
rm(check2012, check2012_plots)


# sdl 2016 -- no unknowns
sdl16_tidy <- sdl2016 %>%
  # remove placeholder for nothing hit
  subset(species != "junk1") %>%
  left_join(spplt[c("code", "clean_code2")], by = c("species" = "code")) %>%
  # add col for hit count
  mutate(hits = 1,
         # add col for yr and date (NA)
         yr = 2016,
         date = NA) %>%
  group_by(yr, date, plot, clean_code2) %>%
  summarise(hits = sum(hits)) %>%
  ungroup()



# -- CLEAN UP EXTRA SDL 2003 AND 2005 DATASETS ----
jgs_clean <- rename(jgs_master2, jgs_plot = plot, date = surveydate) %>%
  mutate(yr = 2005) %>%
  dplyr::select(jgs_plot, yr, date, clean_code2, hits) %>%
  left_join(sdl_sitesLT[c("plot", "jgs_plot", "trt", "meadow", "snow")]) %>%
  mutate(site = "sdl") %>%
  dplyr::select(site, yr, date, plot, jgs_plot, trt:snow, clean_code2, hits) %>%
  arrange(plot, clean_code2)

sdl03_clean <- sdl2003_tidy %>%
  mutate(site = "sdl", yr = 2013) %>%
  rename(hits = hit) %>%
  dplyr::select(site, yr, date, plot, plot_2003, trt:snow, clean_code2, hits) %>%
  arrange(plot, clean_code2)


# -- STANDARDIZE NN + SDL DATS, STACK, AND SEPARATE PLANT FROM SITE INFO -----
# make 2 datasets to write out:
# 1) stack plant community data -- need to add local_site to each dat
# 2) stack site info
## 2 unk forbs and 2 unk grasses in nn13 data never in same plot so okay to use usda codes only

master_plant <- rbind(dplyr::select(nn13_tidy, -nutnet_code), dplyr::select(nn17_tidy, -nutnet_code)) %>%
  dplyr::select(yr, date, plotid, clean_code2, hits) %>%
  rename(plot = plotid) %>%
  mutate(site = "nutnet") %>%
  rbind(rbind(cbind(sdl97_tidy, site = "sdl"),
              jgs_clean[c(names(sdl97_tidy), "site")],
              sdl03_clean[c(names(sdl97_tidy), "site")],
              cbind(sdl12_tidy, site = "sdl"),
              cbind(sdl16_tidy, site = "sdl"))) %>%
  dplyr::select(site, yr:ncol(.)) %>%
  # change plot back to plotid
  rename(plotid = plot) %>%
  ungroup()

# be sure no duplicates in the dataset
checkdups <- master_plant %>%
  grouped_df(colnames(.)[names(.)!= "hits"]) %>%
  summarise(nobs = length(hits))
summary(checkdups$nobs) # all 1s -- each spp only has one entry per plot. good! ready to write out






# -- PREP DATASETS TO INCLUDE VERTICAL HIT LEVEL ----
# for datasets available: sdl 2016, nutnet2017 (that's it!)
nn17_wvert <- nutnet17raw %>%
  # rbind block 3 plot 7 data
  rbind(nn17_b3p7) %>%
  mutate(block = paste0("B", block),
         yr = 2017) %>%
  #join treatment info
  left_join(nutnet_plots) %>%
  dplyr::select(site, yr, date, plotid, block, plot, n:trt, point, hit, species) %>%
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
         yr = 2016, 
         date = NA) %>%
  dplyr::select(site, yr, date, plot, snow, snow_notes, meadow, trt, point, hit, clean_code2, simple_name, Common_Name, Category, Family, Growth_Habit, fxnl_grp, nutnet_grp) %>%
  arrange(plot, point, hit)


# -- PREP RICHNESS DATASETS -----
# any years where spp presence (not hit) is available:
## sdl 1997 (code = 999) and 2012 (code = 0.1), nutnet 2013 (code = 0.25)
sdlpres97 <- subset(sdl1997, hits == 999) %>%
  # correct 96 to 296
  mutate(old_plot = ifelse(old_plot == 96, 296, old_plot),
         # change presence value to nutnet presence value
         hits = 0.25,
         # add site and year
         site = "sdl",
         yr = 1997) %>%
  #drop trt
  dplyr::select(-trt) %>%
  left_join(sdl_plots2, by = c("old_plot" = "old_plot97LT")) %>%
  left_join(spplt[c("code", "clean_code2")], by = c("species" = "code")) %>%
  # old plot 286 doesn't have a match for more current plot numbers.. preserve that, else keep plot values
  mutate(plotid = ifelse(is.na(plot), old_plot, plot)) %>%
  # same names as master for rbinding
  dplyr::select(colnames(master_plant))
# check for duplicates
summary(duplicated(sdlpres97)) #none

sdlpres12 <- subset(sdl2012, hits <1 & hits>0) %>% #to be sure no typos with 0.1
  # only keep plot, species and hits
  dplyr::select(plot, species, hits) %>%
  # change presence value to nutnet presence value
  mutate(hits = 0.25,
         # add site and year
         site = "sdl",
         yr = 2012,
         date = NA) %>%
  left_join(sdl_plots2) %>%
  left_join(spplt[c("code", "clean_code2")], by = c("species" = "code")) %>%
  # rename plot colname to match master
  rename(plotid = plot) %>% 
  # same names as master for rbinding
  dplyr::select(colnames(master_plant))
# check for duplicates
summary(duplicated(sdlpres12)) #none

nnpres13 <- subset(nn13_sce, Hits == 0.25) %>%
  # remove any "species" that were ground cover (one entry for 2BARE -- ctw looked and no unknown forbs or grasses are present only)
  filter(!is.na(USDA_Growth_Habit)) %>%
  mutate(plotid = paste0("B", Block, "_", Plot)) %>%
  #lowercase colnames
  rename_all(casefold) %>%
  # rename usda_symbol to clean_code2
  rename(clean_code2 = usda_symbol) %>%
  mutate(site = "nutnet",
         yr = 2013) %>%
  dplyr::select(colnames(master_plant))

# check for duplicates
summary(duplicated(nnpres13)) # none

# rbind all to master
master_plant <- rbind(master_plant, sdlpres97, sdlpres12, nnpres13) %>%
  #re-create block and plot to sort by numeric order
  mutate(block = ifelse(site == "nutnet", parse_number(substr(plotid, 2,2)), NA),
         plot = ifelse(site == "nutnet", parse_number(gsub(".*_", "", plotid)), as.numeric(plotid))) %>% #throws a warning message abut NAs but works fine
  arrange(site, yr, block, plot, clean_code2) %>%
  # remove block and plot (only keep plotid)
  dplyr::select(-c(block, plot))
# warning sign that NAs introduced, but no NAs introduces
summary(master_plant) #2012 and 2016 don't have dates, so NAs there okay

# be sure spp present were not also recorded as hit
group_by(master_plant, site, yr, plotid, clean_code2) %>%
  summarise(nobs = length(hits)) %>%
  summary() # obs per sp per site/yr/plot is always 1 (no species recorded as hit and present-only)



# -- FINISHING -----
# write out cleaned master plant dataset and site lookup datasets
# plant dats
write_csv(master_plant, "alpine_addnuts/output_data/sdl_nutnet_plantcom_allyrs.csv")
# nutnet site lookup
write_csv(nutnet_plots, "alpine_addnuts/output_data/nutnet_plot_lookup.csv")
# sdl site lookup
write_csv(sdl_sitesLT, "alpine_addnuts/output_data/sdl_plot_lookup.csv")
# cleaned up vertical hits datasets
write_csv(nn17_wvert, "alpine_addnuts/output_data/nutnet2017_vertical_sppcomp.csv")
write_csv(sdl16_wvert, "alpine_addnuts/output_data/sdl2016_vertical_sppcomp.csv")
# extra 2003 and 2005 datasets
write_csv(jgs_clean, "alpine_addnuts/output_data/sdl_2005_jgs_sppcomp.csv")
write_csv(sdl03_clean, "alpine_addnuts/output_data/sdl_2003_sppcomp.csv")
# write out knb-lter-nwt.138 site info metadata
write_csv(sdl97_plotinfo, "alpine_addnuts/output_data/sffert_knb138_sites.csv")
