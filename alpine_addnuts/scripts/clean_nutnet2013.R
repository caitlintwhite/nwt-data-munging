# clean nutnet data

# nutnet 2013:
# ANPP
# richness
# spp comp
## by block, plot, and treatment
## dbl-check ctw spp list against spp list in folder sce received (ts didn't pass on this info to ctw, so spp list made from usda databse, jgs spp list and marko spp list)


# -- SETUP ----
library(tidyverse)
library(readxl)
options(stringsAsFactors = F)

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
# clean up colnames for anpp wide
anpp2.wide <- rename(anpp2.wide, K = `K+`,
                     Treatment = tot_treat) %>%
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
sppcomp <- sppcomp1[[2]] # wide form..
sppcomp.1 <- sppcomp1[[1]] # can extract spp list/codes from here
sppS <- sppcomp1[[4]] # richness and other aggregate values (e.g. forb cover, grass:forb ratio)

# get to work!
# spp LUT
sppcodes <- sppcomp.1[,1:2]
sppcodes <- noNA(sppcodes)
names(sppcodes) <- sppcodes[1,]; sppcodes <- sppcodes[!sppcodes[[2]] %in% c("CODE", NA),]
# clean up names (remove _, -, captalize first letter)
sppcodes$Spp <- gsub("_|-", " ", sppcodes$Spp)
sppcodes$Spp <- paste0(casefold(substr(sppcodes$Spp, 1,1), upper = T), substr(sppcodes$Spp, 2, nchar(sppcodes$Spp)))
sppcodes$Spp <- gsub("UNK", "Unk", sppcodes$Spp)

# which of these codes are not in CTW's lookup table?
sppcodes[!sppcodes$CODE %in% sdlnnLUT$code,] # all but 5..
# wood and some species noted as combined.. see nutnet notes spreadsheet:
sppcomp1[["notes"]] # recode trpa5 as trpap, others were not hit or present, wood = 2LTRWS (Litter, woody, <2.5cm) [in CTW LUT]

# compare names
sppcodes <- left_join(sppcodes, sdlnnLUT, by = c("CODE" = "code"))
# in manual review, everything matches except Noccaea montana (is fenderli in CTW db.. but i remember reviewing that.. taxa name changed?)
# yes, n montana is a synonym for n fendleri.. accepted name is n fendleri, so CTW LUT is good
# good to replace nutnet codes with clean usda codes from CTW LUT


# continue with spp comp
# make sppcomp long form
sppcomp.long <- gather(sppcomp, Code, Hits, (grep("tot_treat", names(sppcomp))+1):ncol(sppcomp)) %>%
  # append presence col for presence/absence
  mutate(Present = ifelse(Hits > 0, 1, 0)) %>%
  # clean up colnames to match biomass
  rename(K = `K+`,
         Treatment = tot_treat) %>%
  # recode 2WOOD with something in CTW LUT that will match to correct USDA code
  mutate(Code = gsub("2WOOD", "WOOD", Code)) %>%
  # join CTW LUT to correct spp codes to USDA codes
  left_join(sdlnnLUT, by = c("Code" = "code")) %>%
  dplyr::select(Block:Treatment, clean_code2, Hits:ncol(.))

# fill in empty common name with unk codes common name for non-vascular veg cover
needscommon <- unique(sppcomp.long$clean_code2[is.na(sppcomp.long$Common_Name)])
for(i in needscommon){
  sppcomp.long$Common_Name[sppcomp.long$clean_code2 == i] <- unkcodes$Common.Name[unkcodes$SYMBOL == i]
}
# check for NAs
summary(is.na(sppcomp.long$Common_Name)) # nope!
