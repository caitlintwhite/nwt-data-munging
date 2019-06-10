# compile ts et al data

# script purpose:
# read in alpine nutnet and sdl community comp data from ts
# 



# -- SETUP -----
library(tidyverse)
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
nutnet17 <- read.csv(datfiles[grep("net17", datfiles)], strip.white = T, na.strings = na_vals)
## Sdl 
plot_codes <- read.csv(datfiles[grep("codes", datfiles)], strip.white = T, na.strings = na_vals)
sdl1997 <- read.csv(datfiles[grep("1997", datfiles)], strip.white = T, na.strings = na_vals)
sdl2012 <- read.csv(datfiles[grep("2012", datfiles)], strip.white = T, na.strings = na_vals)
sdl2016 <- read.csv(datfiles[grep("2016", datfiles)], strip.white = T, na.strings = na_vals)

## NWT datasets on EDI
# MSpaso sdl spp trait dataset
sdltraits <- getTabular(500)
# sdl spp comp (has 6-letter spp codes and USDA codes)
sdlcomp <- getTabular(93)

# review data
glimpse(nutnet13) #wide-form spp matrix, total plot hits or 0.25 if present but not hit
glimpse(nutnet17) #long-form community comp, already summarized into rel + abs cov (no hit data)
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
# compare spp codes in ts datasets..
sort(unique(nutnet17$species)) # looks mostly like USDA codes (except unk1, and UNKF)
sort(colnames(nutnet13[,7:ncol(nutnet13)])) # some USDA codes, but some user-defined (e.g. GRASS1,VIOLET, WOOD)
sort(unique(sdl1997$species)) # 6-letter codes
sort(unique(sdl2012$species)) # some USDA codes, some user-defined codes.. and typos/inconsistent casing
sort(unique(sdl2016$species)) # mostly USDA codes, except "junk1" (juncus?)

spplist_master <- rbind(data.frame(set = "NutNet 2013", code = sort(colnames(nutnet13[,7:ncol(nutnet13)]))),
                        data.frame(set = "NutNet 2017", code = sort(unique(nutnet17$species))),
                        data.frame(set = "Saddle 1997", code = sort(unique(sdl1997$species))),
                        data.frame(set = "Saddle 2012", code = sort(unique(sdl2012$species))),
                        data.frame(set = "Saddle 2016", code = sort(unique(sdl2016$species))))
spplist_master$clean_code <- NA
spplist_master <- left_join(spplist_master, distinct(sdltraits[c("Species", "USDA.Code")]), by = c("code" = "Species")) %>%
  rename(marko_code = USDA.Code) %>%
  mutate(marko_code = ifelse(code %in% unique(sdltraits$USDA.Code), code, marko_code),
         clean_code = ifelse(!is.na(marko_code), marko_code, code))

nutnetspp <- cbind(colnames(nutnet13))

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
sdlspp <- sort(unique(c(sdl1997$species, sdl2012$species, sdl2016$species)))
spplist_master <- sort(unique(c(nutnet_spp, sdlspp)))

# specify vars desired from usda plants database (there are 134)
usda_plantvars <- c("Symbol","Accepted_Symbol_x","Scientific_Name_x","Common_Name","State_and_Province",
                    "Category","Family","Family_Common_Name","Duration","Growth_Habit","Native_Status")

spplist_master <- cbind(spplist_master, data.frame(matrix(nrow = length(spplist_master), ncol=length(usda_plantvars))))
colnames(spplist_master)[which(colnames(spplist_master) == "X1"):ncol(spplist_master)] <- usda_plantvars
# run usda plants api query to scrape species info
# NOTE!!: this will throw an error ["Client error: (400) Bad Request"] if a species is spelled incorrectly in the cover data (a good QA check)
# loop will issue warnings about cbind command providing more variables to replace than there are in 
for(p in spplist_master$species[spplist_master$unknown == 0]){
  print(paste("Pulling USDA Plants data for",p))
  temp_genus <- spplist_master$genus[spplist_master$species == p]
  temp_epithet <- spplist_master$epithet[spplist_master$species == p] 
  
  # grab usda plants data
  if(grepl("ssp.", p)){
    temp_susbp <- gsub("^[A-Z].+ ssp. ", "", p)
    temp_epithet <- gsub(" .*", "", temp_epithet)
    templist <- api("https://plantsdb.xyz") %>%
      api_path(search) %>%
      api_query_(Genus = eval(temp_genus), Species = eval(temp_epithet), Subspecies = eval(temp_susbp))
  }
  # special case for medusahead (any hyphenated species epithet is a special case, search function bonks with hyphen)
  if(grepl("Taen", p)){
    templist <- api("https://plantsdb.xyz") %>%
      api_path(search) %>%
      api_query(Genus = Taeniatherum, Species = `caput-medusae`)
  }
  if(!grepl(" ssp[.]|-", p)){
    templist <- api("https://plantsdb.xyz") %>%
      api_path(search) %>%
      api_query_(Genus = eval(temp_genus), Species = eval(temp_epithet))
  }
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
  spplist_master[spplist_master$species == p,usda_plantvars] <- as.data.frame(temp_df)
}

# -- SDL DATA PREP -----
#

