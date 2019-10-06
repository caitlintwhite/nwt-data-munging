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
sppcomp.long <- gather(sppcomp1.2, Code, Hits, (grep("tot_treat", names(sppcomp1.2))+1):ncol(sppcomp1.2)) %>%
  # append presence col for presence/absence
  mutate(Present = ifelse(Hits > 0, 1, 0)) %>%
  # clean up colnames to match biomass
  rename(K = `K+`,
         Treatment = tot_treat) %>%
  # recode 2WOOD with something in CTW LUT that will match to correct USDA code
  mutate(Code = gsub("2WOOD", "WOOD", Code)) %>%
  # join CTW LUT to correct spp codes to USDA codes
  left_join(sdlnnLUT, by = c("Code" = "code")) %>%
  # recode 2WOOD back to original code to join nutnet names
  mutate(Code = gsub("WOOD", "2WOOD", Code)) %>%
  left_join(sppcodes[c("CODE", "Spp")], by = c("Code" = "CODE")) %>%
  dplyr::select(Block:Treatment, Code, Spp, clean_code2, Hits:ncol(.))

# fill in empty common name with unk codes common name for non-vascular veg cover
needscommon <- unique(sppcomp.long$clean_code2[is.na(sppcomp.long$Common_Name)])
for(i in needscommon){
  sppcomp.long$Common_Name[sppcomp.long$clean_code2 == i] <- unkcodes$Common.Name[unkcodes$SYMBOL == i]
}
# check for NAs
summary(is.na(sppcomp.long$Common_Name)) # nope!

# clean up Growth Habit values for unk forbs and grams
sppcomp.long$Growth_Habit[sppcomp.long$clean_code2 == "2FORB"] <- "Forb/herb"
sppcomp.long$Growth_Habit[sppcomp.long$clean_code2 == "2GRAM"] <- "Graminoid"

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
sppcomp.long.final <- dplyr::select(sppcomp.long,Block:Treatment, Code, Spp, Group, Hits, Present, clean_code2, Scientific_Name_x:ncol(sppcomp.long)) %>%
  rename(USDA_Symbol = clean_code2,
         Scientific_Name = Scientific_Name_x,
         Name = Spp) %>%
  arrange(Block, Plot, Code)
# Code and Name correspond to values used by NutNet, all other descriptive cols are from USDA Plants DB (except Group, specified by CTW)

# > there are a few unk forbs and grasses.. check to see if these were counted separately in richness

# clean up colnames in USDA wide.. preserve NutNet codes as colnames
sppcomp.wide.final <- sppcomp1.2 %>%
  rename(K = `K+`,
         Treatment = tot_treat) %>%
  # reorder spp cols alphabetically
  dplyr::select(Block:Treatment, sort(names(sppcomp.wide.final)[7:ncol(.)])) %>%
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
compare <- rename(sppS, K = 'K+', Treatment = tot_treat) %>%
  left_join(summarize_richness) %>%
  dplyr::select(Block:Treatment, GRASS_RICH, Grass_richness, FORB_RICH, Forb_richness, Legume_richness, SPP_RICH, Total_richness,
                GRASS_COVER, Grass_cover, FORB_COVER, Forb_cover, Legume_cover, TOTAL_COVER, Total_cover, FORB_GRASS_RATIO)
compare$GRASS_RICH-compare$Grass_richness # no diff
compare$FORB_RICH-compare$Forb_richness # ctw over by 1
compare$SPP_RICH-compare$Total_richness # ctw over by 1 most times
# check out spp list for 2nd row (ctw forb richness over by 1 .. is it bc of unk forbs?)
sort(sppcomp.long.final$Code[sppcomp.long.final$Block == "B1" & sppcomp.long.final$Plot == 2]) # has forb1 and forb2, grass 1 and grass 2.. (but grass richness was okay for that one)
# try another
sort(sppcomp.long.final$Code[sppcomp.long.final$Block == "B1" & sppcomp.long.final$Plot == 3]) # has forb1 and forb2, grass 1 and grass 2.. (but grass richness was okay for that one)
# idk.. even their own grass and forb cover sums to more than total, and I can't explain discrepancies in richness counts
# my calcs agree with theirs for grass cover and grass richness, but differs on forbs and idk what they did for legumes.. (maybe they classed some spp as subshrubs and not forbs??)
# their forb_grass_ratio numbers are also incorrect.. maybe it's just better to post spp comp/presence absence data and let data users calculate richness or aggregate cover on their own?


# > CTW opened up one of the workbooks, looked at formulas used and found this:
# 1) Selaginella densa (spikemoss) is NOT counted in veg cover (even tho is vascular plant)
# 2) forbs 1 and 2 and unk grasses 1 and 2 are counted separately (for richness), legumes are counted in with forb cover and richness, when remove Selaginella, forb numbers are correct
# 3) Total cover != grass + forb cover; it equals 100- sum(non-veg cover).. which isn't really total veg cover and is not even rel cov necessarily (don't know if non-veg recorded under veg hits)
# NutNet protocol says total cover should = grass + forb, but then NutNet uses modified Daubenmire not hits so idk..
# 4) forb:grass ratio is not actually forb cover/grass cover (or forb S/grass S). It's grass S/total S, so rel grass richness
