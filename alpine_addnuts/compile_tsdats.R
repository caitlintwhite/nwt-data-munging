# compile ts et al data

# script purpose:
# read in alpine nutnet and sdl community comp data from ts
# 

# notes from TS on species codes:
# TS says "junk1" = nothing (placeholder "species" for nothing hit), and "O" == "ORALA" (Oreoxis alpina)
# ERS6 = fairly safe to assume that's ERSI3...  Erigeron simplex  (maybe we found two of them stuck together (3+3=)
# SOL  = Solidago radiata [ctw: multiradiata]
# PrAu     
# DEADSE    probably dead sedum but let's call this "litter" [ctw: DEADKO also equals "litter" then]
# DRBA  =  Draba
# Fz.Gr = fuzzy graminoid = unknown grama
# R.J.       unknown Juncus
# UnCr3 (also 4 and 5 .. unk carex?)  Carex sp. (unk Carex)
# UnCrWL   Unknown 


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
datpath <- "../../Documents/nwt_lter/unpub_data/dry_meado_fert"
# list data files
datfiles <- list.files(datpath, full.names = T)

# read in datasets
## NutNet
nutnet13 <- read.csv(datfiles[grep("net13", datfiles)], strip.white = T, na.strings = na_vals)
nutnet17 <- read.csv(datfiles[grep("net17[.]", datfiles)], strip.white = T, na.strings = na_vals)
nutnet17raw <- read.csv(datfiles[grep("net17r", datfiles)], strip.white = T, na.strings = na_vals)

## Sdl 
plot_codes <- read.csv(datfiles[grep("codes", datfiles)], strip.white = T, na.strings = na_vals)
sdl1997 <- read.csv(datfiles[grep("1997", datfiles)], strip.white = T, na.strings = na_vals)
sdl2012 <- read.csv(datfiles[grep("2012", datfiles)], strip.white = T, na.strings = na_vals)
sdl2016 <- read.csv(datfiles[grep("2016", datfiles)], strip.white = T, na.strings = na_vals)

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
glimpse(sdltraits)
sort(unique(sdltraits$USDA.Code)) #137 unique codes.. see what matches in tim's data..
# note: lowest common denom measurement = rel_cov or abs_cov, so compare trends in nutnet and sdl using that
# to run nmds, will want wide-form spp matrix for each dataset, and eventually compiled by site
# also build spp list with spp descriptive info (maybe Marko's trait dataset has that?)


# -- PREP SPP LIST  -----
# get USDA plant codes
USDAcodes <- getUSDAplants("Symbol")

# compare spp codes in ts datasets..
sort(unique(nutnet17$species)) # looks mostly like USDA codes (except unk1, and UNKF)
sort(unique(nutnet17raw$species)) # mostly USDA codes, but some misspellings/inconsistencies
sort(colnames(nutnet13[,7:ncol(nutnet13)])) # some USDA codes, but some user-defined (e.g. GRASS1,VIOLET, WOOD)
sort(unique(sdl1997$species)) # 6-letter codes
sort(unique(sdl2012$species)) # some USDA codes, some user-defined codes.. and typos/inconsistent casing
sort(unique(sdl2016$species)) # mostly USDA codes, except "junk1" (juncus?)

spplist_master <- rbind(data.frame(set = "NutNet 2013", code = sort(colnames(nutnet13[,7:ncol(nutnet13)]))),
                        data.frame(set = "NutNet 2017", code = sort(unique(nutnet17$species))),
                        data.frame(set = "NutNet 2017 raw", code = sort(unique(nutnet17raw$species))),
                        data.frame(set = "Saddle 1997", code = sort(unique(sdl1997$species))),
                        data.frame(set = "Saddle 2012", code = sort(unique(sdl2012$species))),
                        data.frame(set = "Saddle 2016", code = sort(unique(sdl2016$species)))) %>%
  # try removing dataset
  dplyr::select(-set) %>%
  distinct() %>%
  arrange(code)

# manual corrections to start
correctcodes <- c(junk1 = "No hit",
               O = "ORAL",
               ERS6 = "ERSI3",
               SOL = "SOMU", #solidago multiradiata
               PrAu = "2FORB",
               DEADKO = "2LTR",
               DEADSE = "2LTR",
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
                         #clean_code2 = ifelse(is.na(clean_code2) & code %in% USDAcodes$Symbol, code, clean_code2))
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

# some manual corrections..
needsusda <- sort(unique(spplist_master$code[is.na(spplist_master$clean_code2)]))
correctdf <- data.frame(code = needsusda, clean_code2 = ifelse(needsusda %in% USDAcodes$Symbol, needsusda, NA))




# -- NUT NET DATA PREP -----
# build and compile rel cov, all years; create all yrs spp lookup table
# create nutnet plot lookup table
nutnet_sites <- dplyr::select(nutnet13, Block:trt) %>% distinct %>%
  rename(K = `K.`) # remove period from K col

# tidy nutnet 2013 dataset and convert to rel_cov
nn13_long <- nutnet13 %>%
  gather(species, hits, LTR:ncol(.)) %>%
  rename(K = `K.`) %>%
  # add sampling year
  mutate(yr = 2013)

# how many nut net spp in mspaso spp?
nutnet_spp <- sort(unique(c(unique(nn13_long$species), unique(nutnet17$species))))
summary(nutnet_spp %in% traitspp$USDA.Code) #44 yes, 41 no...
# what's missing?
nutnet_spp[!nutnet_spp %in% traitspp$USDA.Code] # some unknowns or non-plant codes, but run USDA plants database scrape script..


# -- COMPILE SPP LIST, APPEND USDA PLANTS DATA -----
# specify vars desired from usda plants database (there are 134)
usda_plantvars <- c("Symbol","Accepted_Symbol_x","Scientific_Name_x","Common_Name",
                    "Category","Family","Family_Common_Name","Duration","Growth_Habit","Native_Status")

# pull distinct codes with empty USDA colnames
store_USDA <- distinct(dplyr::select(spplist_master, code:clean_code)) %>%
  arrange(code) %>%
  cbind(data.frame(matrix(nrow = nrow(.), ncol=length(usda_plantvars)))) %>%
  as.data.frame()
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
}

store_USDA <- getUSDAplants(store_USDA$clean_code, joinfield = "clean_code", searchfield = "Symbol")

#how many still missing?
summary(is.na(store_USDA$Symbol)) #77...
nomatchcodes <- store_USDA$clean_code[is.na(store_USDA$Symbol)] 
nomatchcodes
sapply(jgslist, function(x) summary(nomatchcodes %in% x)) # present in 3 cols.. what are they?
nomatchcodes[nomatchcodes %in% jgslist$USDA_code]
nomatchcodes[nomatchcodes %in% jgslist$corrected_NWT_code]   
nomatchcodes[nomatchcodes %in% jgslist$JGS_code] 

# -- SDL DATA PREP -----
#

