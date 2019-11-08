# clean nutnet data

# purpose (this script evolved over time: 
# 1) [original script purpose] review bulk motherlode of 2013 nn files sent to sarah (e.g. are they different? all the same?)
# 2) [update nov 2019] use data cleaned in this script plus other year dat CTW cleaned for TS manuscript to prep/format all nutnet data for EDI publication
# > writes out EDI-ready datasets to Anna and Sarah's GDrive PKG 418 folder using 'googledrive' R package

# nutnet 2013 (sent to Sarah [from Amy C?]):
# ANPP
# richness
# spp comp
## by block, plot, and treatment
## dbl-check ctw spp list against spp list in folder sce received (ts didn't pass on this info to ctw, so spp list made from usda databse, jgs spp list and marko spp list)

# update (2019-10-07): CTW and SCE decided just to post basic, cleaned nutnet data to EDI: anpp (Forb, Grass and Legum), and spp comp
# data users can dervice other metrics (e.g. aggregate cover, species richness and diversity) if they want, but on their own
# don't be fussy about same formatting out nutnet protocol

# update 2019-11-08: SCE and CTW decided to indicates 1s and 0s for trt cols *as applied at time of sampling*
# and to only include original spp codes *IF* they are typo free (CTW fixes them in this script, it's not too bad); standardized USDA codes also provided with USDA Plant DB descriptive info


# final outputs for EDI:
# stacked time series ANPP dataset
# 2013 spp comp dataset (total hits per species per plot, spp present included as 0.25)
# 2017 spp comp dataset (vertical data preserved (e.g. top hit, secondary, terciary, etc.), spp present IDs not available)
# stacked time series spp richness (only way to preserve total spp richness and calculate diversity using spp present for 2017; include 2013 just.. so it's complete i guess, but data users could also calculate 2013 from the sppcomp dataset bc includes spp present)
# > note for richness: 2007 richness exist.. SCE and CTW cannot find them anywhere though. they are published in Adler et al. 2011 science paper on productivity and richness nutnet study



# -- SETUP ----
rm(list = ls())
library(tidyverse)
library(readxl)
library(vegan)
library(googledrive)
options(stringsAsFactors = F)
source("EDI_functions.R")

# read in CTW spp lookup table for nutnet and saddle grid (made for Tim's ms)
sdlnnLUT <- read.csv("alpine_addnuts/output_data/sdl_nutnet_spplookup.csv")
# read in unk USDA codes
unkcodes <- read.csv("https://plants.usda.gov/Data/unknown_plants.txt")

# set pathway to data files
datpath <- "../../Documents/nwt_lter/unpub_data/nutnet2013/"

# list nutnet files in folder from sce
nnfiles <- list.files(datpath)
nnfiles # ignore pptx
nnfiles <- nnfiles[!(grepl("pptx", nnfiles))]
# grab sheets within each file, store in list
nnsheets <- list()
for(i in 1:length(nnfiles)){
  nnsheets[[i]] <- excel_sheets(paste0(datpath, nnfiles[i]))
}

# looking at files manually.. they all seem mostly the same? just named differently.. something to check

# read in all dfs to list
nnlist <- list()
for(i in 1:length(nnfiles)){
  templist <- list()
  for(s in 1:length(nnsheets[[i]])){
    tempdat <- read_excel(paste0(datpath, nnfiles[i]), sheet = nnsheets[[i]][s], trim_ws = TRUE)
    # only append non-empty data frames
    if(nrow(tempdat) > 0){
      templist[[s]] <- tempdat
      names(templist)[s] <- nnsheets[[i]][s]
    }
  }
  nnlist[[i]] <- templist
  names(nnlist)[i] <- nnfiles[i]
}


# read in 2007 nutnet anpp to stack with 2013 anpp (per CTW-SCE discussion on 2019-10-07)
## read in from NWT FTP (to be sure most current df, altho in manual review same as what's on EDI)
nnanpp_2007 <- read.csv("http://nwt.colorado.edu/data_csvs/aboveground_biomass_nutnet.ts.data.csv")

# read in CTW-cleaned 2017 data (vertical), and raw for original spp codes
ctw2017 <- read.csv("alpine_addnuts/output_data/nutnet2017_vertical_sppcomp.csv") 
raw2017 <- read.csv("../../Documents/nwt_lter/unpub_data/dry_meado_fert/nut_net17raw.csv")
# block 3 plot 7 not included in raw
rawb3p7 <- read.csv("../../Documents/nwt_lter/unpub_data/dry_meado_fert/b3_p7.csv")

# read in richness data (already cleaned and verified by CTW for TS ms)
## this is for sdl sffert and nutnet
all_biodiv <- read.csv("alpine_addnuts/output_data/forTS/sdl_nutnet_fxnl_biodiv_anpp_1997-ongoing.csv")




# -- REVIEW ANPP ----
# which dfs are ANPP?
anppdats <- grep("mass", names(nnlist), ignore.case = T) # 3 datasets in folder to compare..

# set 1
anpp1 <- nnlist[[anppdats[1]]] # there are 2 dfs.. are they the same?
anpp1.1 <- data.frame(anpp1[[1]])
anpp1.2 <- data.frame(anpp1[[2]])
glimpse(anpp1.1); glimpse(anpp1.2) # empty first row, names in 2nd row
# remove any rows where all empty
## writing fxn bc will probably use this for other dfs
noNA <- function(xdf){
  xdf <- xdf[apply(xdf,1,function(x) !all(is.na(x))),]
  return(xdf)
}
anpp1.1 <- noNA(anpp1.1)
anpp1.2 <- noNA(anpp1.2)
# set colnames, and remove header row from df
names(anpp1.1) <- anpp1.1[1,]; anpp1.1<- anpp1.1[!grepl("[[:alpha:]]", anpp1.1[[1]]),]
anpp1.1 <- mutate_at(anpp1.1, vars("BLOCK", "PLOT", "BIOMASS"), as.numeric) %>%
  rename(VEGTYPE = `VEG TYPE`) %>%
  arrange(BLOCK, PLOT,VEGTYPE)
# repeat for anpp1.2 and compare
names(anpp1.2) <- anpp1.2[1,]; anpp1.2<- anpp1.2[!grepl("[[:alpha:]]", anpp1.2[[1]]),]
anpp1.2 <- mutate_at(anpp1.2, vars("BLOCK", "PLOT", "BIOMASS"), as.numeric) %>%
  rename(VEGTYPE = `VEG TYPE`) %>%
  arrange(BLOCK, PLOT,VEGTYPE)
summary(anpp1.2 == anpp1.1) # same datasets

# set 2
# what is it?
anpp2 <- nnlist[[anppdats[2]]]
anpp2
# 1) raw data, 2) count of groups with biomass by treatment.. (more of a logic check all there)
# 3) raw data with treatment info, wide form.. split by group and total biomass
# 4) summary stats (e.g. mean with sd.. a user could calc this if they wanted it)

# wide form seems most useful, but check that raw data agree with anpp1.1
anpp2.long <- anpp2[[1]]
anpp2.wide <- anpp2[[3]] # actually the wide is a spreadsheet with everything (yuck)

anpp2.long <- noNA(anpp2.long)
anpp2.wide <- noNA(anpp2.wide)
# set colnames, and remove header row from df
names(anpp2.long) <- anpp2.long[1,]; anpp2.long<- anpp2.long[!grepl("[[:alpha:]]", anpp2.long[[1]]),]
names(anpp2.wide) <- anpp2.wide[1,]; anpp2.wide<- anpp2.wide[!grepl("Block", anpp2.wide[[1]]),]
# clean up long form and compare with anpp1.1
anpp2.long <- mutate_at(anpp2.long, vars("BLOCK", "PLOT", "BIOMASS"), as.numeric) %>%
  rename(VEGTYPE = `VEG TYPE`) %>%
  arrange(BLOCK, PLOT,VEGTYPE)
# does it agree with anpp1.1?
summary(anpp2.long == anpp1.1) # yup


# set 3
anpp3 <- nnlist[[anppdats[3]]]
# what is it?
anpp3 # same as anpp1, ignore

# ANPP review conclusion:
# provide long and wide-form, with treatment info.. wide will have total too


# -- PREP ANPP FOR EDI ----
# extract only biomass data from wide form (ignore summary stats and counts)
anpp2.wide <- anpp2.wide[,1:which(names(anpp2.wide)=="TOTAL")]
anpp2.wide <- mutate_at(anpp2.wide, names(anpp2.wide)[!names(anpp2.wide) %in% c("Block", "tot_treat")], as.numeric) %>%
  arrange(Block, Plot)
# check that TOTAL == sum of forb + grass+ legume
summary(anpp2.wide$TOTAL == apply(anpp2.wide[c("FORB", "GRASS", "LEGUME")], 1, sum)) # .. 1 false..
bad <- which(anpp2.wide$TOTAL != apply(anpp2.wide[c("FORB", "GRASS", "LEGUME")], 1, sum))
anpp2.wide[bad,] # seems correct..
anpp2.wide$TOTAL - apply(anpp2.wide[c("FORB", "GRASS", "LEGUME")], 1, sum) # it's fine, anpp not recorded to that fine of decimal, just wonky R stuff
# clean up colnames/formatting for anpp wide -- keep K+ as is because that is the colname used in NutNet protocols
anpp2.wide <- rename(anpp2.wide, FullTreatment = tot_treat) %>%
  # strip "B" from block vals, convert to numeric for sorting (NutNet doesn't use B in their block values)
  # > note, NutNet uses different colnames but will let SCE/TS handle that if they want to submit data to NutNet
  mutate(Block = as.numeric(gsub("B", "", Block))) %>%
  rename_at(c("FORB", "GRASS", "LEGUME", "TOTAL"), function(x) paste0(substr(x,1,1), casefold(substr(x, 2,nchar(x)))))

#gather wide to long form (since already prepped, and confirm same as anpp1.1)
anpp.long <- gather(anpp2.wide, Group, ANPP_g, Forb:Total) %>%
  arrange(Block, Plot, Group)

# be sure data from wide agrees with data from anpp1.1
# each set is ordered by block, plot, and vegtype so should agree if anpp number is the same
anpp2.wide$Forb == anpp1.1$BIOMASS[anpp1.1$VEGTYPE == "FORB"]
anpp2.wide$Grass == anpp1.1$BIOMASS[anpp1.1$VEGTYPE == "GRASS"]
anpp2.wide$Legume[anpp2.wide$Legume > 0] == anpp1.1$BIOMASS[anpp1.1$VEGTYPE == "LEGUME"] # raw dat doesn't have 0's entered if no legume bmass present

# all agrees, anpp2.wide and anpp.long (made from anpp2.wide) good to write out and dataset for EDI

# clean up env
rm(anpp1, anpp1.1, anpp1.2, anpp2, anpp2.long, anpp3)


# clean up long-form for EDI (per discussion with SCE), standardize biomass, and stack 2007 data
stack_anpp <- subset(anpp.long, Group != "Total") %>%  # remove derived bmass 
  # standardize bmass (is g/0.1m^2) (20x50cm clip)
   mutate(ANPP_g_per_m2 = ANPP_g * 10) %>%
  #append Subplot and Subsubplot to stack with 2007 data
  mutate(Subplot = NA, #no info available
         Subsubplot = NA, #no info available
         DataType = "Post-treatment (Experimental)",
         Date = as.Date("2013-08-02"), # last date of spp comp sampling (but not exactly sure when bmass clipped)
         Site = "NWT NutNet",
          Micro = `K+`,
         # change K+ to 0 since not applied yet in 2013 (added to micro plots in 2016)
         `K+` = 0,
         # recode grass to graminoid
         Group = recode(Group, Grass = "Graminoid")) %>%
  # rename ANPP to reflect area
  rename(ANPPg_0.1m2 = ANPP_g) %>%
  dplyr::select(Site, Date, DataType, Block, Plot, Subplot, Subsubplot,N,P,Micro, `K+`, Group, ANPPg_0.1m2)

anpp2007.long <- nnanpp_2007 %>%
  gather(Group, ANPP_g, live_gram:trifolium) %>%
  # standardize bmass (is g/0.1m^2) (20x50cm clip)
  mutate(ANPP_g_per_m2 = ANPP_g * 10) %>%
  # add treatment columns
  # left_join(distinct(anpp.long[c("Block", "Plot", "N", "P", "Micro", "K+")]), by = c("block" = "Block", "exp_unit" = "Plot")) %>%
  # recode groups
  mutate(Group = recode(Group, live_gram = "Graminoid", live_forb = "Forb",  dead_gram = "Dead graminoid", trifolium = "Legume")) %>%
  rename_at(vars(names(nnanpp_2007)[1:5]), function(x) paste0(casefold(substr(x,1,1), upper = T), substr(x, 2, nchar(x)))) %>%
  rename(Plot = Exp_unit,
         # also rename ANPP
         ANPPg_0.1m2 = ANPP_g) %>%
  mutate(DataType = "Pre-treatment (Baseline)",
         Site = "NWT NutNet",
         # add in 0 data for trt cols since nothing applied yet
         N = 0,
         P = 0,
         Micro = 0,
         `K+` = 0) %>%
  # reorder cols
  dplyr::select(names(stack_anpp))

stack_anpp <- rbind(stack_anpp, anpp2007.long) %>%
  # sort by date, block, plot and group
  arrange(Date, Block, Plot, Group)


# -- REVIEW SPP COMP -----
# in nutnet, 0.25 = present in plot but not hit
# what are the spp comp dats?
sppdats <- names(nnlist)[grepl("spp.comp|sppcomp", names(nnlist), ignore.case = T)]
#.. it's kind of amazing there are 8 datasets of the same thing..
nnlist[[sppdats[1]]] # all tables on sheet 1, spp comp wide form (spp = colnames), spp comp as presence-absence, richness by group with forb:grass ratio, some summary stats tabs
nnlist[[sppdats[2]]] # 2 is same as 1 with a few more tabs at end for stats in tables/figs
nnlist[[sppdats[3]]] # same as 2, but stats tabs appended are unnamed
nnlist[[sppdats[4]]] # block 1 raw data
nnlist[[sppdats[5]]] # block 2 raw data
nnlist[[sppdats[6]]] # block 3 raw data
nnlist[[sppdats[7]]] # block 4 raw data
nnlist[[sppdats[8]]] # same as 1..

# I am going to assume raw block data is what's in spp comp datasets and not check each of those files (each plot is on its own tab.. would be 40 tabs to process + additional (all + notes))
# based on bmass review, also assuming spp comp dats that look the same actually are the same..
# end-products needed for EDI:
# 1) spp comp.. long-form.. add column for presence-absence so data-users can get frequency that way
# 2) richness and forb:grass ratio, long-form
# 3) spp lookup table (more for CTW to verify analyses she did using files tim gave her is good and spp LUT made from that is good)


# start with #1 since everything else seems to be made from that/variations on that
sppcomp1 <- nnlist[[sppdats[1]]]
names(sppcomp1)
sppcomp1.2 <- sppcomp1[[2]] # wide form..
sppcomp1.1 <- sppcomp1[[1]] # can extract spp list/codes from here
sppS <- sppcomp1[[4]] # richness and other aggregate values (e.g. forb cover, grass:forb ratio)

# get to work!
# spp LUT
sppcodes <- sppcomp1.1[,1:2]
sppcodes <- noNA(sppcodes)
names(sppcodes) <- sppcodes[1,]; sppcodes <- sppcodes[!sppcodes[[2]] %in% c("CODE", NA),]
# clean up names (remove _, -, captalize first letter)
sppcodes$Spp <- gsub("_|-", " ", sppcodes$Spp)
sppcodes$Spp <- paste0(casefold(substr(sppcodes$Spp, 1,1), upper = T), substr(sppcodes$Spp, 2, nchar(sppcodes$Spp)))
sppcodes$Spp <- gsub("UNK", "Unk", sppcodes$Spp)

# which of these codes are not in CTW's lookup table?
sppcodes[!sppcodes$CODE %in% sdlnnLUT$code,] # all but 4..
# some species noted as combined.. see nutnet notes spreadsheet:
sppcomp1[["notes"]] # recode trpa5 as trpap, others were not hit or present

# compare names
sppcodes <- left_join(sppcodes, sdlnnLUT, by = c("CODE" = "code"))
# in manual review, everything matches except Noccaea montana (is fenderli in CTW db.. but i remember reviewing that.. taxa name changed?)
# yes, n montana is a synonym for n fendleri.. accepted name is n fendleri, so CTW LUT is good
# good to replace nutnet codes with clean usda codes from CTW LUT


# continue with spp comp
# make sppcomp long form
sppcomp.long <- gather(sppcomp1.2, Code, Hits, (grep("tot_treat", names(sppcomp1.2))+1):ncol(sppcomp1.2)) %>%
  # append presence col for presence/absence
  mutate(Present = ifelse(Hits > 0, 1, 0),
         # strip "B" from block values
         Block = as.numeric(gsub("B", "", Block))) %>%
  # clean up colnames to match biomass
  rename(FullTreatment = tot_treat) %>%
  # join CTW LUT to correct spp codes to USDA codes
  left_join(sdlnnLUT, by = c("Code" = "code")) %>%
  left_join(sppcodes[c("CODE", "Spp")], by = c("Code" = "CODE")) %>%
  dplyr::select(Block:FullTreatment, Code, Spp, clean_code2, Hits:ncol(.))

# # fill in empty common name with unk codes common name for non-vascular veg cover
# needscommon <- unique(sppcomp.long$clean_code2[is.na(sppcomp.long$Common_Name)])
# for(i in needscommon){
#   sppcomp.long$Common_Name[sppcomp.long$clean_code2 == i] <- unkcodes$Common.Name[unkcodes$SYMBOL == i]
# }
# check for NAs
summary(is.na(sppcomp.long$Common_Name)) # nope!


# add in simple functional group
sppcomp.long$Group <- with(sppcomp.long, ifelse(grepl("Fabace", Family), "Legume",
                                                ifelse(grepl("forb", Growth_Habit, ignore.case = T), "Forb",
                                                       ifelse(grepl("grami", Growth_Habit, ignore.case = T), "Grass", 
                                                              # add in non-vascular cover (w.g. moss, lichen, wood, litter, bare ground, rock)
                                                              ifelse(grepl("^2", clean_code2), "Non-vascular ground cover", NA))))) 

# check Accepted_Symbol_x = Symbol (i.e. is it a redundant column in this case?)
summary(sppcomp.long$Symbol == sppcomp.long$Accepted_Symbol_x) # yes, NAs are 2xx codes


# -- PREP SPP COMP FOR EDI -----
# clean up data frame for final product
# i.e. remove redudant colnames.. perhaps indicate certain cols as from USDA (could also indicate in EML to avoid long names)
sppcomp.long.final <- dplyr::select(sppcomp.long,Block:FullTreatment, Code, Spp, Group, Hits, Present, clean_code2, Scientific_Name_x:ncol(sppcomp.long)) %>%
  rename(Name = Spp,
         USDA_Symbol = clean_code2) %>%
  # prefix all cols from USDA plants db with "USDA_"
  rename_at(vars(names(.)[names(.) %in% names(sdlnnLUT)]), function(x) paste0("USDA_", x)) %>%
  arrange(Block, Plot, Code) %>%
  #remove _x from SciName
  rename_all(function(x) gsub("_x$", "", x))
# Code and Name correspond to values used by NutNet, all other descriptive cols are from USDA Plants DB (except Group, specified by CTW)

# > there are a few unk forbs and grasses.. check to see if these were counted separately in richness

# clean up colnames in USDA wide.. preserve NutNet codes as colnames
sppcomp.wide.final <- sppcomp1.2 %>%
  rename(FullTreatment = tot_treat) %>%
  # strip B from block vals
  mutate(Block = as.numeric(gsub("B", "", Block))) %>%
  # reorder spp cols alphabetically
  dplyr::select(Block:FullTreatment, sort(names(.)[7:ncol(.)])) %>%
  arrange(Block, Plot)



# -- REVIEW RICHNESS AND AGGREGATE SPP COMP METRICS -----
# does richness count unk forbs (2) and unk grams (2) separately or as 1 each?
summarize_richness <- subset(sppcomp.long.final, Present == 1 & !grepl("Non-vascular", Group)) %>%
  grouped_df(names(sppcomp.long.final)[c(1:6, 9)]) %>%
  summarize(richness = length(unique(Code)),
            cover = sum(Hits)) %>%
  ungroup()%>%
  gather(met, val, richness:cover) %>%
  unite(cat, Group, met, sep = "_") %>%
  spread(cat, val, fill = 0) %>%
  arrange(Block, Plot) %>%
  mutate(Total_cover = Forb_cover + Grass_cover, Legume_cover,
         Total_richness = Forb_richness + Grass_richness + Legume_richness)

# compare richness and cover btwn ctw calculated and files given to SCE
compare <- rename(sppS, FullTreatment = tot_treat) %>%
  # strip B from block
  mutate(Block = as.numeric(gsub("B", "", Block))) %>%
  left_join(summarize_richness) %>%
  dplyr::select(Block:FullTreatment, GRASS_RICH, Grass_richness, FORB_RICH, Forb_richness, Legume_richness, SPP_RICH, Total_richness,
                GRASS_COVER, Grass_cover, FORB_COVER, Forb_cover, Legume_cover, TOTAL_COVER, Total_cover, FORB_GRASS_RATIO)
compare$GRASS_RICH-compare$Grass_richness # no diff
compare$FORB_RICH-compare$Forb_richness # ctw over by 1
compare$SPP_RICH-compare$Total_richness # ctw over by 1 most times
# check out spp list for 2nd row (ctw forb richness over by 1 .. is it bc of unk forbs?)
sort(sppcomp.long.final$Code[sppcomp.long.final$Block == 1  & sppcomp.long.final$Plot == 2]) # has forb1 and forb2, grass 1 and grass 2.. (but grass richness was okay for that one)
# try another
sort(sppcomp.long.final$Code[sppcomp.long.final$Block == 1 & sppcomp.long.final$Plot == 3]) # has forb1 and forb2, grass 1 and grass 2.. (but grass richness was okay for that one)
# idk.. even their own grass and forb cover sums to more than total, and I can't explain discrepancies in richness counts
# my calcs agree with theirs for grass cover and grass richness, but differs on forbs and idk what they did for legumes.. (maybe they classed some spp as subshrubs and not forbs??)
# their forb_grass_ratio numbers are also incorrect.. maybe it's just better to post spp comp/presence absence data and let data users calculate richness or aggregate cover on their own?


# > CTW opened up one of the workbooks, looked at formulas used and found this:
# 1) Selaginella densa (spikemoss) is NOT counted in veg cover (even tho is vascular plant)
# 2) forbs 1 and 2 and unk grasses 1 and 2 are counted separately (for richness), legumes are counted in with forb cover and richness, when remove Selaginella, forb numbers are correct
# 3) Total cover != grass + forb cover; it equals 100- sum(non-veg cover).. which isn't really total veg cover and is not even rel cov necessarily (don't know if non-veg recorded under veg hits)
# NutNet protocol says total cover should = grass + forb, but then NutNet uses modified Daubenmire not hits so idk..
# 4) forb:grass ratio is not actually forb cover/grass cover (or forb S/grass S). It's grass S/total S, so rel grass richness


# .. for now, follow what NutNet did for output datasets (i.e. Selaginella = non-plant cover, not counted in spp richness or Shannon div)
# do count distinct unk forbs and grams separately, and do fold legumes into forb richness and cover

# recode selaginella's group, then recalc richness, diversity, total grass and forb cover
# change "Non-vascular cover" to "Ground cover"
sppcomp.long.final$Group[sppcomp.long.final$Code == "SEDES"] <- "Ground cover"
sppcomp.long.final$Group[grepl("Non-vascular", sppcomp.long.final$Group)] <- "Ground cover"

# clean up environment
rm(sppcomp.long, bad)


# -- PREP AGGREGATE AND BIODIVERSITY FOR EDI -----
# need: total richness, grass richness, forb richness, total cover (actual total cover), grass cover, forb cover, and shannon div
# ctw will calculate then compare to nutnet data to be sure
biodiv <- subset(sppcomp.long.final, Present == 1 & Group != "Ground cover") %>%
  grouped_df(names(sppcomp.long.final)[c(1:6, 9)]) %>%
  summarize(richness = length(unique(Code)),
            hits = sum(Hits)) %>%
  ungroup()%>%
  gather(met, val, richness:hits) %>%
  unite(cat, Group, met, sep = "_") %>%
  spread(cat, val, fill = 0) %>%
  arrange(Block, Plot) %>%
  mutate(AllForb_hits = Forb_hits + Legume_hits,
         AllForb_richness = Forb_richness + Legume_richness,
         AllSpecies_hits = AllForb_hits + Grass_hits,
         AllSpecies_richness = AllForb_richness + Grass_richness)
# check agrees with nutnet-calculated numbers
biodiv$AllForb_hits - sppS$FORB_COVER # yay
biodiv$AllSpecies_richness - sppS$SPP_RICH # agrees

# add in plant cover as 100-sum(Ground cover)
groundcover <-  subset(sppcomp.long.final, Group == "Ground cover") %>%
  grouped_df(names(sppcomp.long.final)[c(1:6)]) %>%
  summarize(Ground_hits = sum(Hits)) %>%
  ungroup()%>%
  mutate(Plant_cover = 100-Ground_hits) %>%
  arrange(Block, Plot)
# append plant cover
biodiv <- left_join(biodiv, dplyr::select(groundcover, -Ground_hits)) %>%
  #rearrange Grass cols before Forb
  dplyr::select(Block:FullTreatment, Grass_hits, Grass_richness, Forb_hits:ncol(.))
# check plant cov agrees
biodiv$Plant_cover - sppS$TOTAL_COVER # agrees

# calculate and append Shannon diversity
# make relative cover dataset on all non-ground cover cols
plantdat <- sppcomp.wide.final[!names(sppcomp.wide.final) %in% unique(sppcomp.long.final$Code[sppcomp.long.final$Group == "Ground cover"])] %>%
  arrange(Block, Plot)
relcov <- vegan::decostand(plantdat[(grep("FullTreatment", names(plantdat))+1):ncol(plantdat)], method = "total")
relcov <- as.matrix(relcov)
rownames(relcov) <- paste0("B", plantdat$Block, "_", plantdat$Plot)

# to be sure relcov the same as nutnet calculated, compare
nnrelcov <- nnlist[[8]][[9]]
nnrelcov <- nnrelcov[c(names(nnrelcov)[1:6], sort(names(nnrelcov)[7:ncol(nnrelcov)]))]
summary(relcov - nnrelcov[,7:ncol(nnrelcov)]) # same
# calculate shannon div
Sdiv <- diversity(relcov)
Sdiv # same order as biodiv, so can append values directly
# append diversity to biodiv df
biodiv$Shannon_diversity <- Sdiv


# -- SIMPLIFY LONG-FORM SPP COMP -----
# simplify long-form dataset for SCE (remove Nutnet grouping) + add site info and dates
sppcomp.simple <- sppcomp.long.final %>%
  mutate(Site = "NWT NutNet") %>%
  dplyr::select(Site, Block:Name, Hits:ncol(.))

# each block-workbook has the survey dates for spp comp.. need to iterate through and scrap those
blockdats <- grep("_block", names(nnlist))
datedat <- data.frame()
for(i in blockdats){
  templist <- nnlist[[i]]
  # get position of data tabs that correspond to plot spp comp (have the date)
  blocksheets <- grep("block", nnsheets[[i]])
  for(b in blocksheets){
    worksheet <- nnlist[[i]][[b]]
    datepos <- grep("DATE", worksheet[[1]], ignore.case = T)
    if(length(datepos) != 0){
      tempdate <- worksheet[[2]][[datepos]] 

      tempdat <- data.frame(item = nnsheets[[i]][[b]],
                            datenum = tempdate,
                            Date = as.Date(as.numeric(tempdate), origin = "1904-01-01"),
                            Block = worksheet[[2]][[grep("BLOCK", worksheet[[1]], ignore.case = T)]],
                            Plot = worksheet[[2]][[grep("PLOT", worksheet[[1]], ignore.case = T)]])
      
    } else{
      # search for date in colnames
      tempdate <- names(worksheet)[grep("DATE", names(worksheet))]
      tempdate <- gsub("DATE: ", "", tempdate)
      blockplot <- worksheet[[grep("Block", worksheet, ignore.case = T)]]
      tempdat <- data.frame(item = nnsheets[[i]][[b]],
                            datenum = tempdate,
                            Date = as.Date(tempdate, format = "%B %d, %Y"),
                            Block = parse_number(blockplot[[grep("BLOCK:", blockplot, ignore.case = T)]]),
                            Plot = parse_number(blockplot[[grep("PLOT:", blockplot, ignore.case = T)]]))
    }
    
    datedat <- rbind(datedat, tempdat)
  }
}

# triage missing date data
datedat <- datedat %>%
  mutate(Plot = ifelse(is.na(Plot),parse_number(gsub("^.+plot ", "", datedat$item)),Plot),
         Block = ifelse(is.na(Block),parse_number(gsub(" plot.*$", "", datedat$item)),Block))

# get block 3 plot 8 9 10 dates
block3 <- nnlist[[grep("block3", names(nnlist))]]
for(i in 8:10){
  worksheet <- block3[[grep(paste("plot", i), names(block3))]]
  # date is in colnames
  tempdate <- names(worksheet)[grep("DATE", names(worksheet))]
  tempdate <- gsub("DATE: ", "", tempdate)
  datedat$datenum[datedat$Block == 3 & datedat$Plot == i] <- tempdate
  datedat$Date[datedat$Block == 3 & datedat$Plot == i] <- as.Date(tempdate, format = "%B %d, %Y")
}

# manually correct B1 7
datedat$Date[datedat$Block == 1 & datedat$Plot == 7] <- as.Date(datedat$datenum[datedat$Block == 1 & datedat$Plot == 7], format = "%B %d %Y")
# subset datedat to max date per block-plot
datedat <- distinct(datedat[c("Block", "Plot", "Date")]) %>%
  group_by(Block, Plot) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  mutate_at(c("Block", "Plot"), as.numeric)

# now, infill missing Dates
sppcomp.simple <- sppcomp.simple %>%
  left_join(datedat) %>%
  dplyr::select(Site, Date, Block:ncol(.)) %>%
  arrange(Block, Plot, Code)

# clean up "(ERIMEL?)" in spp Name
sppcomp.simple <- mutate(sppcomp.simple, Name = ifelse(grepl("Erig.* melano", Name),
                                                       str_extract(unique(sppcomp.simple$Name), "^.* melanocephalus"),Name),
                         # remove extra double space from Carex spp
                         Name = trimws(gsub("  ", " ", Name)))
# check names
sort(unique(sppcomp.simple$Name))
# check codes
sort(unique(sppcomp.simple$Code))
View(distinct(sppcomp.simple[c("Code", "Name", "USDA_Symbol")]))


# clean up 2013 for EDI 
# correct trt cols (micro, K+), remove spp not 0 and presence/absence col
sppcomp.2013.final <- as.data.frame(sppcomp.simple) %>%
  ungroup() %>%
  mutate(Micro = `K+`,
         `K+` = 0) %>%
  filter(Hits > 0) %>%
  dplyr::select(Site:P, Micro, `K+`, Code:Hits, USDA_Symbol:ncol(.)) %>%
  # drop nativity
  dplyr::select(-USDA_Native_Status)


# -- PREP 2017 VERTICAL SPP COMP FOR EDI -----
# stack tim's rawdat and remove no hits
rawstack <- rename(rawb3p7, K = `K.`) %>%
  rbind(raw2017) %>%
  mutate(species = trimws(species)) %>%
  # assign vert hits
  group_by(block, plot, point) %>%
  mutate(hit = seq(1,length(species), 1)) %>%
  ungroup() %>%
  filter(nchar(species) > 2)

# view tim's codes, compare with Eve's to see if can use her names
sort(unique(rawstack$species))
summary(unique(rawstack$species) %in% sppcomp.2013.final$Code) #oye..
# what agrees?
unique(rawstack$species)[unique(rawstack$species) %in% sppcomp.2013.final$Code]
# 2017 codes not in 2013
sort(unique(rawstack$species)[!unique(rawstack$species) %in% sppcomp.2013.final$Code])
# 2013 not in 2017
sort(unique(sppcomp.2013.final$Code)[!unique(sppcomp.2013.final$Code) %in% unique(rawstack$species)])

# > ctw decides, not going to match name because codes do not agree between years; tim didn't provide name for codes so not including
# just going to clean up tim codes so no typos
View(data.frame(code = unique(rawstack$species)) %>% left_join(sdlnnLUT))
# the rule is, if first 4 letter matches another code, keep the longer code (e.g. has a number as final character)
clean_ts_codes <- data.frame(code = unique(rawstack$species)) %>% left_join(sdlnnLUT[c("code", "clean_code2")]) %>%
  mutate(check = substr(code,1,4),
         chars = nchar(code),
         final_ts = NA)
# iterate through to assign final code based on nchar (can't figure out how to do in dplyr)  
for(i in unique(clean_ts_codes$clean_code2)){
  tempname <- subset(clean_ts_codes, clean_code2 == i) %>%
    filter(chars == max(chars))
  clean_ts_codes$final_ts[clean_ts_codes$clean_code2 == i] <- tempname$code[1]
}
View(clean_ts_codes) # looks good

# now replace codes in tsrawdat, then append to ctw cleaned vert data
rawstack <- left_join(rawstack, clean_ts_codes, by = c("species" = "code")) %>%
  left_join(distinct(sppcomp.2013.final[c("Code", "Name")]), by = c("final_ts" = "Code")) %>%
  mutate(block = paste0("B", block))
# join with ctw cleaned as check to be sure clean codes match up properly

sppcomp.2017.final <- left_join(ctw2017, dplyr::select(rawstack, block, plot, point, hit, clean_code2, species, final_ts, Name))
summary(is.na(sppcomp.2017.final)) # yay everything matched, just need to fill in nutnet names
# can I fill in with simple name?
needsmatch <- subset(sppcomp.2017.final, is.na(Name)) %>%
       dplyr::select(clean_code2, simple_name, final_ts, Name) %>%
       distinct() # everything in simple name is appropriate for infilling
# OR pull Names using clean code in 2013 dataset (duh)
names2013 <- dplyr::select(sppcomp.2013.final, USDA_Symbol, Name, Code) %>%distinct()
# infill with 2013 names
for(i in needsmatch$clean_code2){
  # remove any numbers if it was an unk forb or grass
  infillval <- unique(gsub("[0-9]+", "", names2013$Name[names2013$USDA_Symbol == i])) %>% trimws()
  # infill ts 2017
  if(length(infillval) > 0){
  needsmatch$Name[needsmatch$clean_code2 == i] <- infillval
  }
}
# infill the rest with the simple name
needsmatch <- mutate(needsmatch, Name = ifelse(is.na(Name), simple_name, Name))
# now infill main set then clean up
for(i in needsmatch$final_ts){
  sppcomp.2017.final$Name[sppcomp.2017.final$final_ts == i] <- needsmatch$Name[needsmatch$final_ts == i]
}

summary(is.na(sppcomp.2017.final)) # nothing NA anymore
sppcomp.2017.final <- rename(sppcomp.2017.final, Code = final_ts,
                             USDA_Symbol = clean_code2,
                             Vertical = hit) %>%
  rename_all(function(x) paste0(casefold(substr(x,1,1),upper = T), substr(x,2,nchar(x)))) %>%
  mutate(Micro = K,
         Hits = 1,
         Site = unique(sppcomp.2013.final$Site),
         Block = parse_number(Block)) %>%
  rename(`K+` = K) %>%
  dplyr::select(Site, Date, Block:P, Micro, `K+`, Point, Vertical, Code, Name, Hits, USDA_Symbol) %>%
  arrange(Block, Plot, Point, Vertical) %>%
  # join usda data (don't include nativity)
  left_join(distinct(dplyr::select(sdlnnLUT, clean_code2, Scientific_Name_x:Growth_Habit)), by = c("USDA_Symbol" = "clean_code2")) %>%
  # prefix USDA_ to usda dats  
  rename_at(vars(names(.)[names(.) %in% names(sdlnnLUT)]), function(x) paste0("USDA_",x))
# remove _x from SciName
names(sppcomp.2017.final) <- gsub("_x", "", names(sppcomp.2017.final))
  
# final check that N thu Micro trts = N thru Micro treats for 2013
trt2017 <- distinct(dplyr::select(sppcomp.2017.final, Block:Micro)) %>%
  # if everything matches, can join a column from sppcomp 2013 based on all cols and there won't be NAs
  left_join(distinct(dplyr::select(sppcomp.2013.final, Block:Micro, Site)))
summary(is.na(trt2017)) # yay! everything joined well


# append survey to code and name so data users know those were Eve/Tim's taxa units
sppcomp.2013.final <- rename_at(sppcomp.2013.final, vars("Code", "Name"), function(x)paste0("Field",x))
sppcomp.2017.final <- rename_at(sppcomp.2017.final, vars("Code", "Name"), function(x)paste0("Field",x))

# final check
glimpse(sppcomp.2013.final)
glimpse(sppcomp.2017.final)


# -- PREP RICHNESS DATS -----
# provide 2013 for convenience I guess?
# 2017 total richness is all that currently exists.. ts did not provide spp IDs for spp present, so only have tallies
# use prepped sdl nutnet richness dat (from ms) and append sampling dates, fix trt cols
nnS <-subset(all_biodiv, site == "nutnet") %>%
  dplyr::select(site:plot, trt, S) %>%
  mutate(block = parse_number(block),
         site = unique(sppcomp.2013.final$Site)) %>%
  rename_all(function(x)paste0(casefold(substr(x,1,1), upper = T), substr(x, 2, nchar(x)))) %>%
  left_join(distinct(dplyr::select(sppcomp.2013.final, Date, Block:`K+`))) %>%
  #reorder cols
  dplyr::select(Site, Yr, Date, Block, Plot, N:`K+`, S) %>%
  rename(N_spp_present = S,
         Date2013 = Date) %>%
  # change K+ to 1 if yr = 2017
  mutate(`K+` = ifelse(Yr == 2017, Micro, `K+`)) %>%
  # join 2017 dates
  left_join(distinct(dplyr::select(sppcomp.2017.final, Date, Block:`K+`))) %>%
  rename(Date2017 = Date) %>%
  # make character bc otherwise will not play nicely with 2017 dates (R converts all to numeric. boo!)
  mutate(Date2013 = as.character(Date2013)) %>%
  # choose final date .. if 2013, 2013 date, if 2017.. 2017 date..
  mutate(Date = ifelse(Yr == 2013, Date2013, Date2017)) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  dplyr::select(Site, Date, Block:N_spp_present)




# -- WRITE OUT FINAL DATASETS -----
# specify pathway for writing out final datasets
outpath <- "alpine_addnuts/output_data/nutnet2013_alldats/"

# anpp, long and wide form + stacked for EDI
write.csv(anpp.long, paste0(outpath, "nutnet2013_anpp_long.csv"), row.names = F)
write.csv(anpp2.wide, paste0(outpath, "nutnet2013_anpp_wide.csv"), row.names = F)
write.csv(stack_anpp, paste0(outpath, "NWTnutnet_anpp_2007ongoing.csv"), row.names = F)

# spp comp, long and wide form, and simplified long-form spp comp
write.csv(sppcomp.long.final, paste0(outpath, "nutnet2013_sppcomp_long.csv"), row.names = F)
write.csv(sppcomp.wide.final, paste0(outpath, "nutnet2013_sppcomp_wide.csv"), row.names = F)
write.csv(sppcomp.2013.final, paste0(outpath, "NWTnutnet_sppcomp2013_forEDI.csv"), row.names = F)
write.csv(sppcomp.2017.final, paste0(outpath, "NWTnutnet_sppcomp2017_forEDI.csv"), row.names = F)

# richness (no 2007 available)
write.csv(nnS, paste0(outpath, "NWTnutnet_sppS_2013ongoing_forEDI.csv"), row.names = F)

# aggregate metrics, long form only <-- these aggs mark selaginella dense as ground cover in order to verify 2013 richness and diversity numbers in TS's first ms draft
write.csv(biodiv, paste0(outpath, "nutnet2013_aggregate_and_biodiversity.csv"), row.names = F)

# write to google drive for Anna
## get 418 folder
gdrive418 <- drive_find(pattern = "PKG_418", n = 30) %>% drive_ls()
# write anpp dataset
# rename anpp dat to datname on EDI (aboveground_biomass_nutnet_baseline.ts.data.csv -- but SCE says take out baseline)
drive_upload(media = paste0(outpath, "NWTnutnet_anpp_2007ongoing.csv"),
             path = gdrive418[grep("clean", gdrive418$name),], 
             name = "aboveground_biomass_nutnet.ts.data.csv", overwrite = T)
# write spp comp
# give it name similar to anpp dataset
## > write 2013 data separately bc in different data format than 2017 data
drive_upload(media = paste0(outpath, "NWTnutnet_sppcomp2013_forEDI.csv"),
             path = gdrive418[grep("clean", gdrive418$name),], 
             name = "sppcomp2013_nutnet.ts.data.csv", overwrite = T)
## > write 2017 sppcomp data separately (has vertical info)
drive_upload(media = paste0(outpath, "NWTnutnet_sppcomp2017_forEDI.csv"),
             path = gdrive418[grep("clean", gdrive418$name),], 
             name = "sppcomp2017_nutnet.ts.data.csv", overwrite = T)
# write richness
# give it name similar to anpp dataset
drive_upload(media = paste0(outpath, "NWTnutnet_sppS_2013ongoing_forEDI.csv"),
             path = gdrive418[grep("clean", gdrive418$name),], 
             name = "spprichness_nutnet.ts.data.csv", overwrite = T)

