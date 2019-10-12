# compile ts et al data

# script purpose:
# read in alpine nutnet and sdl community comp data from ts
# make spp lookup table for standardizing codes across all plant cover datasets (write out spp lookup)
# run the following in nmds:
# (1)
# (2)



# notes from TS on species codes:
# TS says "junk1" = nothing (placeholder "species" for nothing hit), and
# "O" also = nothing hit (placeholder)
# ERS6 = fairly safe to assume that's ERSI3...  Erigeron simplex  (maybe we found two of them stuck together (3+3=)
# SOL  = Solidago radiata [ctw: multiradiata]
# PrAu     
# DEADSE    probably dead sedum but let's call this "litter" [ctw: DEADKO also equals "litter" then]
# DRBA  =  Draba
# Fz.Gr = fuzzy graminoid = unknown grama
# R.J.       unknown Juncus
# UnCr3 (also 4 and 5 .. unk carex?)  Carex sp. (unk Carex)
# UnCrWL   Unknown 
# BIOB2   = probalbly MIOB2 = Minuartia obtusiloba
# DW (wood?)  = probably dead wood = litter (ctw: there is a usda code for small wood)
# ERTC =  probably ERSI3 = Erigeron simplex
# GRDAZ = I'm clueless = unknown
# LATER =  my guess is that's "litter"

# 9/6/19: from reviewing nutnet2013 data from SCE and looking at JGS list, ORAL should be ORALA (alpina ssp.)

# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(readxl)
library(request) # to access USDA plants api
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

## Sdl 
plot_codes <- read.csv(datfiles[grep("Saddle_codes", datfiles)], strip.white = T, na.strings = na_vals)
sdl1997 <- read.csv(datfiles[grep("1997", datfiles)], strip.white = T, na.strings = na_vals)
sdl2012 <- read.csv(datfiles[grep("2012", datfiles)], strip.white = T, na.strings = na_vals)
sdl2016 <- read.csv(datfiles[grep("2016", datfiles)], strip.white = T, na.strings = na_vals)
# extra 2003 dataset
sdl2003 <- read_excel("../../Documents/nwt_lter/unpub_data/mystery_files/colin03a.xls")
## NWT datasets on EDI
# MSpaso sdl spp trait dataset
sdltraits <- getTabular(500) %>% data.frame()
# sdl spp comp (has 6-letter spp codes and USDA codes)
sdlcomp <- getTabular(93) %>% data.frame()

# JGS NWT spp list
jgslist <- read_excel("/Users/serahsierra/Documents/nwt_lter/unpub_data/pspecies.mw.data_JGS.xlsx", sheet = 1, na = na_vals, trim_ws = T)

#usda unknown codes
usda_unk <- read.csv("https://plants.usda.gov/Data/unknown_plants.txt")


# functions
getUSDAplants <- function(searchfield){
  # grab usda plant codes -- 5000 is max number of records retrievable
  startUSDA <- api("https://plantsdb.xyz") %>%
    api_path(search) %>% api_query_(fields = eval(searchfield), limit = 5000)
  # number of times needed to iterate through to scrape all codes
  runs <- round(startUSDA$count/5000)
  USDAdf <- startUSDA$data 
  for(i in 1:runs){
    print(paste("Retrieving records", (i*5000), "through", (i*5000)+5000))
    temp_df <- api("https://plantsdb.xyz") %>%
      api_path(search) %>% api_query_(fields = eval(searchfield), limit = 5000, offset = i*5000)
    # append next batch of codes to USDA codes string
    USDAdf <- rbind(USDAdf, temp_df$data)
  }
  # once done, clean up scraped data
  USDAdf <- distinct(USDAdf)
  print(paste(nrow(USDAdf), "unique USDA records retrieved!"))
  return(USDAdf)
}





# -- REVIEW/PREP DATA -----
# review data
glimpse(nutnet13) #wide-form spp matrix, total plot hits or 0.25 if present but not hit
glimpse(nutnet17) #long-form community comp, already summarized into rel + abs cov (no hit data)
glimpse(nutnet17raw) # long-form, dates didn't read in correctly; species codes have "O"?
sort(unique(nutnet17raw$species)) # O, ORAL, ORALA..
sort(unique(nutnet17$species)) # no O species, but ORALA

glimpse(plot_codes)
glimpse(sdl1997) #long-form, total plot hits
glimpse(sdl2012) #long-form, total plot hits
glimpse(sdl2016) #long-form spp-presence dataset (need to transform to hit data with 0 infilled); what is sfcode?
sapply(sdl2016, unique)
glimpse(sdl2003) #wide form, sum column at end -- remove
sdl2003 <- sdl2003[!colnames(sdl2003) == "sum"] # remove sum column
glimpse(sdltraits)
sort(unique(sdltraits$USDA.Code)) #137 unique codes.. see what matches in tim's data..
# note: lowest common denom measurement = rel_cov or abs_cov, so compare trends in nutnet and sdl using that
# to run nmds, will want wide-form spp matrix for each dataset, and eventually compiled by site
# also build spp list with spp descriptive info (maybe Marko's trait dataset has that?)


# -- COMPILE MASTER SPP LIST  -----
# get USDA plant codes
USDAcodes <- getUSDAplants("Symbol")

# compare spp codes in ts datasets..
sort(unique(nutnet17$species)) # looks mostly like USDA codes (except unk1, and UNKF)
sort(unique(nutnet17raw$species)) # mostly USDA codes, but some misspellings/inconsistencies
sort(colnames(nutnet13[,7:ncol(nutnet13)])) # some USDA codes, but some user-defined (e.g. GRASS1,VIOLET, WOOD)
sort(unique(sdl1997$species)) # 6-letter codes
sort(unique(sdl2012$species)) # some USDA codes, some user-defined codes.. and typos/inconsistent casing
sort(unique(sdl2016$species)) # mostly USDA codes, except "junk1" (juncus?)
sort(colnames(sdl2003[,4:ncol(sdl2003)])) # 6-letter codes with some multi-spp guesses
spplist_master <- rbind(data.frame(set = "NutNet 2013", code = sort(colnames(nutnet13[,7:ncol(nutnet13)]))),
                        data.frame(set = "NutNet 2017", code = sort(unique(nutnet17$species))),
                        data.frame(set = "NutNet 2017 raw", code = sort(unique(nutnet17raw$species))),
                        data.frame(set = "Saddle 1997", code = sort(unique(sdl1997$species))),
                        data.frame(set = "Saddle 2003", code = sort(colnames(sdl2003)[4:ncol(sdl2003)])),
                        data.frame(set = "Saddle 2012", code = sort(unique(sdl2012$species))),
                        data.frame(set = "Saddle 2016", code = sort(unique(sdl2016$species)))) %>%
  # try removing dataset
  dplyr::select(-set) %>%
  distinct() %>%
  arrange(code)

# manual corrections to start
correctcodes <- c(junk1 = "No hit",
                  BIOB2 = "MIOB2",
                  O = "No hit",
                  ERS6 = "ERSI3",
                  ERTC = "ERSI3",
                  SOL = "SOMU", #solidago multiradiata
                  PrAu = "2FORB",
                  DEADKO = "2LTR",
                  DEADSE = "2LTR",
                  LATER = "2LTR",
                  DRBA  =  "DRABA", #usda draba sp
                  `Fz.Gr` = "2GRAM",
                  `R.J.` = "JUNCU", #usda juncus sp
                  UkCr3 = "JUNCU", 
                  UkCr4 = "JUNCU",
                  UkCr5 = "JUNCU",
                  UkCrWL = "JUNCU")

spplist_master$clean_code <- NA 
for(i in names(correctcodes)){
  spplist_master$clean_code[spplist_master$code == i] <- correctcodes[names(correctcodes) == i]
}

spplist_master <- left_join(spplist_master, distinct(sdltraits[c("Species", "USDA.Code")]), by = c("code" = "Species")) %>%
  rename(marko_usda = USDA.Code) %>%
  mutate(marko_usda = ifelse(code %in% unique(sdltraits$USDA.Code), code, marko_usda),
         clean_code = ifelse(!is.na(marko_usda) & is.na(clean_code), marko_usda, clean_code)) %>%
  left_join(distinct(jgslist[c("USDA_code", "corrected_NWT_code")]), by = c("code" = "corrected_NWT_code")) %>%
  rename(jgs_usda = USDA_code) %>%
  mutate(clean_code = ifelse((is.na(clean_code) & !is.na(jgs_usda)), jgs_usda, clean_code)) %>%
  dplyr::select(-jgs_usda) %>%
  #try rejoining jane's usda codes using her code instead of the corrected_nwt_code
  left_join(distinct(jgslist[c("USDA_code", "JGS_code")]), by = c("code" = "JGS_code")) %>%
  rename(jgs_usda = USDA_code) %>%
  mutate(clean_code = ifelse((is.na(clean_code) & !is.na(jgs_usda)), jgs_usda, clean_code)) %>%
  #drop marko and jane code cols
  dplyr::select(-c(marko_usda, jgs_usda)) %>%
  # try searching usda_unk for accepted code
  mutate(clean_code = ifelse(code %in% usda_unk$SYMBOL, code, clean_code)) %>%
  arrange(code)

# check for inconsistent code entries (all same but number, e.g. CASC vs CASC12) 
spplist_master$code_typo <- NA
spplist_master$alt_code <- NA
for(i in unique(spplist_master$code[is.na(spplist_master$clean_code)])){
  check <- sort(unique(spplist_master$code[!spplist_master$code == i]))
  if(sum(grepl(casefold(i), casefold(check)))>0){
    spplist_master$code_typo[spplist_master$code == i] <- TRUE
    altcodes <- check[grepl(casefold(i), casefold(check))]
    spplist_master$alt_code[spplist_master$code == i] <- str_flatten(altcodes, collapse = "_")
  }
}

# iterate through codes with no usda match so far and search for string matches within code and clean_code
needsusda <- sort(unique(spplist_master$code[is.na(spplist_master$clean_code)]))
# create temp col for troubleshooting codes with missing usda match
spplist_master$trbl_usda <- NA
for(n in needsusda){
  nlength <- nchar(n)
  # subset that matches all chars but last or the string with numbers removed
  temp_df <- subset(spplist_master, grepl(substr(n,1,nlength-1), code, ignore.case = T) | grepl(gsub("[0-9]+", "", n), code, ignore.case = T))
  missingcodes <- unique(temp_df$code[is.na(temp_df$clean_code)])
  replace <- str_flatten(unique(temp_df$clean_code[!is.na(temp_df$clean_code)]), collapse = "_")
  # if replacement not empty, fill, otherwise, NA
  spplist_master$trbl_usda[spplist_master$code == n] <- ifelse(length(replace) > 0, replace, NA) 
}

# if code typo and trbl match, use that for clean code
spplist_master$clean_code2 <- spplist_master$clean_code
spplist_master <- mutate(spplist_master, 
                         clean_code2 = ifelse(is.na(clean_code2) & alt_code == trbl_usda, alt_code, clean_code2))
                         
# remove suggested alternate code and trbl_usda codes if more than one
spplist_master[c("alt_code", "trbl_usda")] <- sapply(spplist_master[c("alt_code", "trbl_usda")],function(x)ifelse(grepl("_", x), NA, x))

# manually check suggested usda codes for anything that doesn't have a clean code yet
View(subset(spplist_master, is.na(clean_code2) & !is.na(trbl_usda)))
# usda code of CAHEE is incorrect for code CAREX, make NA
spplist_master$trbl_usda <- gsub("CAHEE", NA, spplist_master$trbl_usda)
spplist_master <- mutate(spplist_master,
                         clean_code2 = ifelse(alt_code %in% c(USDAcodes$Symbol, usda_unk$SYMBOL) & is.na(clean_code2),alt_code, 
                                              ifelse(trbl_usda %in% c(USDAcodes$Symbol, usda_unk$SYMBOL) & is.na(clean_code2), trbl_usda, clean_code2)),
                         clean_code2 = ifelse(code %in% clean_code2 & is.na(clean_code2), code, clean_code2))
# match anything that's already in the clean_code2 col

# check codes that don't have a clean code still against clean_code2 for character match as above
# first drop clean_code, alt_codes, and trbl_usda since already used 
spplist_master <- dplyr::select(spplist_master, code, clean_code2)

# finish with manual corrections..
needsusda <- sort(unique(spplist_master$code[is.na(spplist_master$clean_code2)]))
# check if code had a match in scraped usda codes list
correctdf <- data.frame(code = needsusda, clean_code2 = ifelse(needsusda %in% USDAcodes$Symbol, needsusda, NA))
# manual correct unknowns..
correctdf$clean_code2[grepl("lich|2lch|chn", correctdf$code, ignore.case = T)] <- usda_unk$SYMBOL[usda_unk$Common.Name == "Lichen"]
correctdf$clean_code2[grepl("^rf|rock", correctdf$code, ignore.case = T)] <- "2RF" #rock fragment
correctdf$clean_code2[grepl("unk", correctdf$code, ignore.case = T)] <- "2FORB" #unk forb
correctdf$clean_code2[grepl("lit", correctdf$code, ignore.case = T)] <- "2LTR" #litter
correctdf$clean_code2[grepl("grass", correctdf$code, ignore.case = T)] <- "2GRAM" #unk grass
correctdf$clean_code2[grepl("bare", correctdf$code, ignore.case = T)] <- "2BARE" #unk grass
correctdf$clean_code2[grepl("moss", correctdf$code, ignore.case = T)] <- "2MOSS" #unk grass
correctdf$clean_code2[grepl("DW|WOOD", correctdf$code)] <- usda_unk$SYMBOL[grepl("woody, <2.5", usda_unk$Common.Name)]
correctdf$clean_code2[correctdf$code == "elk.P"] <- "2SCAT" # there's no usda code for scat.. if want code can either be 2BARE or 2LTR?

                                                          
# check codes that pattern match codes in master list even tho have match in USDA codes (could differ by numeric characters)
correctdf$flag <- sapply(correctdf$code, function(x) sum(grepl(x, spplist_master$code, ignore.case = T))>1)
View(subset(correctdf, flag == TRUE)) #ARFE, CASC, and CEAR need to be adjusted
correctdf$clean_code2[correctdf$code == "ARFE"] <- unique(spplist_master$clean_code2[grepl("ARFE", spplist_master$code) & !is.na(spplist_master$clean_code2)])
correctdf$clean_code2[correctdf$code == "CASC"] <- unique(spplist_master$clean_code2[grepl("CASC", spplist_master$code) & !is.na(spplist_master$clean_code2)])
correctdf$clean_code2[correctdf$code == "CEAR"] <- unique(spplist_master$clean_code2[grepl("CEAR", spplist_master$code) & !is.na(spplist_master$clean_code2)])

# typo corrections
correctdf$clean_code2[correctdf$code == "CAR02"] <- unique(spplist_master$clean_code2[spplist_master$code == "CARO2"])
correctdf$clean_code2[correctdf$code == "TEAGR"] <- unique(spplist_master$clean_code2[spplist_master$code == "TEGR"])

# partial matches in USDA
correctdf$clean_code2[correctdf$code == "ELYMUS"] <- USDAcodes$Symbol[grepl("^ELYM", USDAcodes$Symbol)]
correctdf$clean_code2[correctdf$code == "VIOLET"] <- USDAcodes$Symbol[grepl("^VIOL", USDAcodes$Symbol)]

# manually correct codes from 2003 sdl data
correctdf$clean_code2[correctdf$code == "POT"] <- "POTEN"
correctdf$clean_code2[correctdf$code == "CAR SSP"] <- "CAREX"
correctdf$clean_code2[correctdf$code == "STELMON"] <- jgslist$USDA_code[jgslist$corrected_NWT_code == "STEMON" & !is.na(jgslist$corrected_NWT_code)]
correctdf$clean_code2[correctdf$code == "MINBIF/OBTBIF/LIDOBT"] <- jgslist$USDA_code[jgslist$corrected_NWT_code == "MINBIF" & !is.na(jgslist$corrected_NWT_code)]
correctdf$clean_code2[correctdf$code == "LEWPYG/OREPYG"] <- jgslist$USDA_code[jgslist$corrected_NWT_code == "LEWPYG" & !is.na(jgslist$corrected_NWT_code)]
correctdf$clean_code2[correctdf$code == "THLMON/NOCMON"] <- jgslist$USDA_code[jgslist$corrected_NWT_code == "NOCMON" & !is.na(jgslist$corrected_NWT_code)]

# make everything else an unk forb (only GRDAZ left, which TS says is unknown)
correctdf$clean_code2[is.na(correctdf$clean_code2)] <- "2FORB"

# fill in spplist_master with manually corrected codes
for(i in correctdf$code){
  spplist_master$clean_code2[spplist_master$code == i] <- correctdf$clean_code2[correctdf$code == i]
}

# check to make sure no missing usda codes
summary(is.na(spplist_master$clean_code2)) # none. huzzah!
# finally, manually correct ORAL to ORALA
spplist_master$clean_code2[spplist_master$clean_code2 == "ORAL"] <- "ORALA"
# check how 2003 names paired (ctw added them in after script developed)
check2003 <- subset(spplist_master, code %in% colnames(sdl2003)) %>% left_join(jgslist, by = c("code" = "corrected_NWT_code"))
# 2003 looks good
# clean up environment
rm(correctdf,temp_df, altcodes, check, correctcodes, i, missingcodes, n, needsusda, nlength, replace, check2003)



# --APPEND USDA PLANTS DATA -----
# specify vars desired from usda plants database (there are 134)
usda_plantvars <- c("Symbol","Accepted_Symbol_x","Scientific_Name_x","Common_Name",
                    "Category","Family","Family_Common_Name","Duration","Growth_Habit","Native_Status")

# pull distinct codes with empty USDA colnames
store_USDA <- spplist_master %>%
  # flag unknowns to ignore when scraping usda database
  mutate(unknown = ifelse(clean_code2 %in% usda_unk$SYMBOL | grepl("^2", clean_code2),
                          TRUE,FALSE)) %>%
  # add in cols for usda database data
  cbind(data.frame(matrix(nrow = nrow(.), ncol=length(usda_plantvars)))) %>%
  data.frame()
colnames(store_USDA)[which(colnames(store_USDA) == "X1"):ncol(store_USDA)] <- usda_plantvars
# run usda plants api query to scrape species info
# NOTE!!: this will throw an error ["Client error: (400) Bad Request"] if a species is spelled incorrectly in the cover data (a good QA check)
# loop will issue warnings about cbind command providing more variables to replace than there are in 

joinUSDAplants <- function(spp, joinfield, searchfield){  
  for(p in spp){
    print(paste("Checking records for", p))
    # check if code exists in USDA plants database or not
    if(p %in% USDAcodes[[searchfield]]){
      print(paste(p, "found! Fetching..."))
      # specify search
      searchlist <- list(p)
      names(searchlist) <- searchfield
      # fetch data
      templist <- api("https://plantsdb.xyz") %>%
        api_path(search) %>%
        api_query_(.dots = searchlist)
      
      # isolate desired cols
      temp_df <- templist$data[1,colnames(templist$data) %in% usda_plantvars]
      # rematch to updated name if accepted symbol doesn't match symbol
      if(temp_df$Symbol != temp_df$Accepted_Symbol_x){
        templist2 <- api("https://plantsdb.xyz") %>%
          api_path(search) %>%
          api_query_(Symbol = eval(temp_df$Accepted_Symbol_x))
        update_df <- templist2$data[1,colnames(templist2$data) %in% usda_plantvars]
        temp_df[,which(colnames(temp_df)=="Common_Name"):ncol(temp_df)] <- update_df[,which(colnames(update_df)=="Common_Name"):ncol(update_df)]
      }
      # cleanup empty cells
      temp_df[temp_df==""] <- NA
      # append to master data frame
      #usdaplants_df <- rbind(usdaplants_df, temp_df)
      # add to spplist_master
      store_USDA[store_USDA[[joinfield]] == p,usda_plantvars] <- as.data.frame(temp_df)
    }
  }
  return(store_USDA)
}


spplist_master <- joinUSDAplants(spp = unique(store_USDA$clean_code2[store_USDA$unknown==FALSE]), joinfield = "clean_code2", searchfield = "Symbol")
rm(store_USDA)


# -- CHECK FOR MULTIPLE CODES FOR ONE SPECIES -----
# go through species appended by scientific name and check for redundant species with different USDA codes
spplist_master2 <- spplist_master %>%
  dplyr::select(Symbol:Native_Status) %>%
  distinct() %>%
  subset(!is.na(Symbol)) %>%
  arrange(Scientific_Name_x) %>%
  # extract just genus and species epithet
  mutate(simple_name = str_extract(Scientific_Name_x, "[A-Za-z]+ [A-Za-z]+")) %>%
  group_by(simple_name) %>%
  mutate(code_ct = length(unique(Symbol))) %>%
  ungroup() %>%
  # keep only those redundant species that have different codes
  subset(code_ct > 1) %>%
  # mark if exists in various datasets
  mutate(in_jgs = Symbol %in% jgslist$USDA_code,
         in_ts = Symbol %in% spplist_master$code,
         # create empty col for storing spp number
         spnum = NA,
         # create empty col for storing which code to kick out
         spexclude = NA) %>%
  # reorganize
  dplyr::select(spnum, spexclude, simple_name:in_ts, Symbol:ncol(.))

# number species to keep track of pairs
for(i in unique(spplist_master2$simple_name)){
  spplist_master2$spnum[spplist_master2$simple_name == i] <- which(unique(spplist_master2$simple_name) == i)
}
  
spplist_master2$spexclude <- !(spplist_master2$in_jgs & spplist_master2$in_ts)
# manually correct geum rossii (should only have 1 code, even tho in both datasets)
# > choose GERO2 (used in Marko's trait dataset, is most general)
spplist_master2$spexclude[spplist_master2$Symbol == "GEROT"] <- TRUE
# manually correct carex heteroneura (both are in jgs dataset, are different ssp of carex h.)
spplist_master2 <- subset(spplist_master2, simple_name != "Carex heteroneura")

# replace codes and descriptive info in spp_master with codes to keep in spplist_master2
for(i in spplist_master2$Symbol[spplist_master2$spexclude == TRUE]){
  # pull correct data
  simple_name <- spplist_master2$simple_name[spplist_master2$Symbol == i]
  temp_df <- spplist_master2[spplist_master2$simple_name == simple_name & !spplist_master2$spexclude, 7:ncol(spplist_master2)]
  
  # replace usda info
  spplist_master[spplist_master$clean_code2 == i,4:ncol(spplist_master)] <- temp_df
  # correct usda code
  spplist_master$clean_code2[spplist_master$clean_code2 == i] <- temp_df$Symbol
}


# manual checks: correct codes that were misspelled in ts entered data or otherwise got paired incorrectly
#SIAC v SAIC -- SAIL is a misspell (ids as lichen in USDA database. should be SIAC2)
spplist_master[spplist_master$clean_code2 == "SAIC",4:ncol(spplist_master)] <- spplist_master[spplist_master$code == "SIACS2",4:ncol(spplist_master)]
spplist_master$clean_code2[spplist_master$clean_code2 == "SAIC"] <- "SIACS2"

#NOMO2 v. NOMOM -- jgs notes noccaea montana is old, should be noccaea fendleri. marko's dataset uses noccaea fendleri, but is older than jane's list
# > go with NOFEN
spplist_master[spplist_master$clean_code2 == "NOMO2",4:ncol(spplist_master)] <- spplist_master[spplist_master$code == "NOMOM",4:ncol(spplist_master)] 
spplist_master$clean_code2[spplist_master$clean_code2 == "NOMO2"] <- "NOFEG"

# add a simple latin name
spplist_master$simple_name <- str_extract(spplist_master$Scientific_Name_x, "[A-Za-z]+ [A-Za-z]+") 
# switch "L." to sp. for plants IDd to genus only
spplist_master$simple_name <- gsub(" L$", " sp.", spplist_master$simple_name) 

#reorganize cols
spplist_master <- dplyr::select(spplist_master, code:unknown, simple_name, Symbol:ncol(spplist_master))



# -- INFILL MISSING DESCRIPTIVE INFO ----
# infill missing species descriptive info for plants only IDd to genus (e.g. Juncus L., Viola L.)
# all plants in Tim's dataset should be native and probably perennial
# will match to whatever species are in same genera and IDd to spp level
# also cross-check with jane's spp list

# which codes have missing descriptive info -- ignore unknowns and no hit codes
needsinfill <- sort(unique(spplist_master$clean_code2[is.na(spplist_master$Growth_Habit) & !grepl("^2|No hit", spplist_master$clean_code2)]))
# check which cols have missing info
with(spplist_master, spplist_master[clean_code2 %in% needsinfill, ])
# > just Duration, Growth_Habit, and Native_Satus are NA

# > notws: 
# TS says Juncus should only be perennial at NWT
# all violets in jgs spp list are perennial (looked up codes that don't have info on usda plants db)
# all Poa in jgs spp list are perennial
# all potentilla in jgs perennial
# elymus is perennial in usda plants db
# .. going to infill all as perennial, native and then whatever is most common growth_habit for that genus in ts spplist

# infill duration
spplist_master$Duration[spplist_master$clean_code2 %in% needsinfill] <- "Perennial"
# infill native status
spplist_master$Native_Status[spplist_master$clean_code2 %in% needsinfill] <- "L48 (N)" #most conservative
# infill poa, rush and carex sp. as graminoid (follows usda plants db convention for poa, rush or carex ID'd to spp level)
spplist_master$Growth_Habit[spplist_master$clean_code2 %in% needsinfill & grepl("Cyper|Poac|Junca", spplist_master$Family)] <- "Graminoid"
# infill forb growth habitat
spplist_master$Growth_Habit[spplist_master$clean_code2 == "POTEN"] <- unique(spplist_master$Growth_Habit[grepl("Potenti", spplist_master$simple_name) & !is.na(spplist_master$Growth_Habit)])
spplist_master$Growth_Habit[spplist_master$clean_code2 == "DRABA"] <- unique(spplist_master$Growth_Habit[grepl("Draba", spplist_master$simple_name) & !is.na(spplist_master$Growth_Habit)])
spplist_master$Growth_Habit[spplist_master$clean_code2 == "VIOLA"] <- unique(spplist_master$Growth_Habit[grepl("Viola", spplist_master$simple_name) & !is.na(spplist_master$Growth_Habit)])

# review infilling
spplist_master[spplist_master$clean_code2 %in% needsinfill,] # looks good

# change unknown for no-hit to NA since not applicable
spplist_master$unknown[spplist_master$clean_code2 == "No hit"] <- NA


# fill in unknowns/ground cover common_name (i.e. symbols that start with "2") and usda symbols
for(i in unique(spplist_master$clean_code2[spplist_master$clean_code2 %in% usda_unk$SYMBOL])){
  spplist_master$Common_Name[spplist_master$clean_code2 == i] <- usda_unk$Common.Name[usda_unk$SYMBOL == i]
  spplist_master$Symbol[spplist_master$clean_code2 == i] <- i
  spplist_master$Accepted_Symbol_x[spplist_master$clean_code2 == i] <- i
}
# fill in simple name
spplist_master <- spplist_master %>%
  mutate(simple_name = ifelse(is.na(simple_name), Common_Name, simple_name),
         # clean up certain unk simple names for ground cover/unks
         simple_name = ifelse(clean_code2 == "2FORB", "Forb sp.",
                              ifelse(clean_code2 == "2GRAM", "Grass sp.",
                                     ifelse(clean_code2 == "2LTRWS", "Woody litter", simple_name))),
         # update lifeform for unk forb and grass, and duration
         Duration = ifelse(grepl("2FOR|2GRA", clean_code2), "Perennial", Duration),
         Growth_Habit = ifelse(clean_code2 == "2FORB", "Forb/herb",
                               ifelse(clean_code2 == "2GRAM", "Graminoid", Growth_Habit)))

# because I know it exists from cleaning nutnet2013 data, add row for 2WOOD code, copy values for code = WOOD
woodrow <- spplist_master[spplist_master$code == "WOOD",] 
woodrow$code <- "2WOOD"
# rbind to master
spplist_master <- rbind(spplist_master, woodrow)
# sort by code
spplist_master <- arrange(spplist_master, code)


# -- FINISHING -----
# note which spp
# spplist compiled! write out for reference
write_csv(spplist_master, "alpine_addnuts/output_data/sdl_nutnet_spplookup.csv")
