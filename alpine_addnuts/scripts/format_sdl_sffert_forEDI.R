# clean SDL SFfert for EDI

# read in CTW prepped spp comp and anpp data and tweak cols for publication EDI
## EDI package ID knb-lter-nwt.138
# write out to Anna and SCE's Google Drive folder


# NOTE!
# edi package id naming convention for this dataset is: saddfer[x].ts.data.ext (where x is the what, e.g. b = biomass)



# -- SETUP ----
rm(list = ls())
library(tidyverse)
library(googledrive)
library(readxl)
options(stringsAsFactors = F, strip.white = T, na.strings = c("NA", NA, "NaN", NaN, "."))
theme_set(theme_bw())
source("EDI_functions.R")

# read in data CTW cleaned for TS 2019 ms
alldat <- list.files("alpine_addnuts/output_data/forTS/", full.names = T)
all_sppcomp <- read.csv(alldat[grep("sppcomp", alldat)])
all_biodiv <- read.csv(alldat[grep("biodiv", alldat)])

# other dats that might be helpful..
## 2016 vertical spp comp (includes grid point and vertical hit order)
vert2016 <- read.csv("alpine_addnuts/output_data/sdl2016_vertical_sppcomp.csv")
## sdl plot lookup tbl
sdlsites <- read.csv("alpine_addnuts/output_data/sdl_plot_lookup.csv")
# read in orig 2012 plotnums to confirm other numbers to drop (e.g. if PDF number doesn't match anything in datasets [not actually used to match] can drop)
sffert2012 <- read_excel("../../Documents/nwt_lter/unpub_data/mystery_files/saddfert.2012.ts.data.xlsx") %>%
  dplyr::select(plot_num, old_plot_num) %>%
  rename(old_plot2012 = old_plot_num) %>%
  mutate_all(as.numeric)

# saddle sffert anpp 1996, 1997 data (on EDI)
anpp <- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.138.3&entityid=754715ae8d14748faaa98f48e178fb83",
                 na.strings = c("NA", NA, NaN, "NaN"), header = FALSE)
anpp_meta <- readLines("http://nwt.colorado.edu/meta_data/saddferb.ts.meta.txt")
anpp_2014 <- read.csv("http://nwt.colorado.edu/data_csvs/saddferb.ts.data.csv", na.strings = c("NaN", NA, "NA", NaN))


# check that everything read in as expected
str(anpp)
str(anpp_2014)
str(all_sppcomp)
str(all_biodiv)

# infill plot number for XX controls in anpp 2014 dataset
anpp_2014$plot_num[anpp_2014$old_plot_num == "XX1"] <- 17 # from metadata
anpp_2014$plot_num[anpp_2014$old_plot_num == "XX2"] <- 29 # from metadata

# read in metadata with old plots numbers
oldsites <- readLines("http://nwt.colorado.edu/meta_data/saddfert.ts.meta.txt")
# metadata with current plot numbers
oldsites_current <- readLines("http://nwt.colorado.edu/meta_data/saddferb.ts.meta.txt")


# -- PREP SF SITES ----
#wet meadow = no snow, recode snow and snowfence recovery to binary
# plot 76 should NOT be a snowfence recovery plot (entered incorrectly)
sdlLT <- sdlsites %>%
  mutate(snow = ifelse(meadow == "wet", "no snow", snow),
         snow = recode(snow, `no snow` = 0, snow = 1),
         meadow = recode(meadow, dry = "DM", wet = "WM", mesic ="MM"),
         trt = recode(trt, N="NN", C ="CC", P = "PP", `N+P` = "NP"),
         snow_notes = ifelse(plot == 76, "in snowfence area", snow_notes),
         snow_recovery = ifelse(grepl("recovery", snow_notes), 1, 0),
         # add in site info
         LTER_site = "Niwot Ridge LTER",
         local_site = "SDL snowfence") %>%
  rename(fert = trt,
         veg_class = meadow,
         plot_num = plot) %>%
  #drop plot 286 from 1997
  filter(plot_num != 286) %>%
  left_join(sffert2012)


# -- review snowfence sites -----
# extract old tags in anpp metadata on server to be sure all info in there
sitepos <- grep("^Treatment", oldsites)
sfsites <- data.frame(dat =oldsites[sitepos[1]:(sitepos[1]+16)]) %>%
  separate(dat, into = paste0("col", 1:15), sep = " +")
names(sfsites) <- c("fert", "dry_x", "dry_y", "dry", "mesic_x", "mesic_y", "mesic")
#drop any cols that are all NA
sfsites <- sfsites[2:nrow(sfsites),!sapply(sfsites, function(x) all (is.na(x)))]
sfsites <- separate(sfsites, dry_y, c("dry_ymin", "dry_ymax"), sep = "[-]") %>%
  separate(mesic_y, c("mesic_ymin", "mesic_ymax"), sep = "[-]") %>%
  mutate(dry_2004 = str_extract(dry, "[0-9]{3}[+]"),
         mesic_2004 = str_extract(mesic, "[0-9]{3}[+]"),
         dry = substr(dry, 2, 4),
         mesic = substr(mesic, 2, 4)) %>%
  # make all that should be numeric, numeric (remove non-number chars)
  mutate_at(vars(names(.)[2:ncol(.)]), parse_number)
# split up
drysf <- sfsites[,grep("fert|dry", names(sfsites))] %>%
  mutate(veg_class = "DM",
         snow = 1) %>%
  rename_all(function(x) gsub("dry_", "", x)) %>%
  rename(tag_sw_orig = dry,
         tag_sw_2004 = `2004`)
mesicsf <- sfsites[,grep("fert|mes", names(sfsites))] %>%
  mutate(veg_class = "MM",
         snow = 1) %>%
  rename_all(function(x) gsub("mesic_", "", x)) %>%
  rename(tag_sw_orig = mesic,
         tag_sw_2004 = `2004`)
# stack snowfence sites
sfsites_meta <- rbind(drysf, mesicsf) # snowfence metadata don't have any NE tags
rm(drysf, mesicsf, sfsites)

# check against sites lookup from summer data stitching 
checkLUTsf <- left_join(sdlLT, sfsites_meta) %>%
  filter(snow == 1) %>%
  # add in ne tag cols
  mutate(tag_ne_orig = NA) %>%
  # reorg cols
  dplyr::select(LTER_site, local_site, plot_num, fert, veg_class, snow, snow_recovery, x, ymin, ymax, 
                #sw plots
                metadata_edi138, old_plot_edi138, old_plot96, old_plot97LT, plot_sw_tag_PDF, tag_sw_orig, tag_sw_2004,
                #ne plots
                jgs_plot:jgs_plot_num4, tag_ne_orig, plot_ne_tag_PDF, plot_2003, old_plot2012, old_plot16) %>%
  # remove 6 control plots added in 2016
  filter(plot_num <= 80)
# check cols
for(i in names(checkLUTsf)[!names(checkLUTsf) %in% c("tag_ne_orig", "tag_sw_orig")]){
  # remove any cols from other sources that match metadata SW tag perfectly or are all NA
  if(all(checkLUTsf[i] == checkLUTsf$tag_sw_orig) | all(is.na(checkLUTsf[i]))){
    checkLUTsf <- dplyr::select(checkLUTsf, -i)
    print(paste("Removed", i))
  }
}
# screen PDF plot tags (if aren't found in any other tags cols, ditch [don't trust entry, e.g. 30.11])
# decide orig will = first number issued for that corner, whether in 1996 or 2004
checkLUTsf <- mutate(checkLUTsf, 
                     tag_ne_orig = ifelse(plot_ne_tag_PDF == jgs_plot_num2, plot_ne_tag_PDF, tag_ne_orig),
                     # check against metadata
                     tag_ne_orig = ifelse((tag_ne_orig == tag_sw_2004) | (!is.na(tag_ne_orig) & is.na(tag_sw_2004)), tag_ne_orig, NA),
                     # if PDF ne tag == 2016 number, assign that
                     tag_ne_orig = ifelse(is.na(tag_ne_orig) & (plot_ne_tag_PDF == old_plot16), plot_ne_tag_PDF, tag_ne_orig))

# go through tag cols and eliminate any numbers that are accounted for
tagcols <- c("plot_sw_tag_PDF","tag_sw_2004", "jgs_plot","jgs_plot_num2","jgs_plot_num3","plot_ne_tag_PDF","old_plot16")
for(i in tagcols){
  # iterate through each row
  for(n in 1:nrow(checkLUTsf)){
    if(checkLUTsf[n,i] %in% checkLUTsf[n, c("tag_sw_orig", "tag_ne_orig")]){  #tagcols[tagcols!=i],   
      checkLUTsf[n,i] <- NA
    }
  }
}
# there are some clear typos in the data (e.g. 451 for 459, 412 for 462).. manual corrections where needed
# assign 462 as ne tag
checkLUTsf$tag_ne_orig[grepl(462, checkLUTsf$jgs_plot) & checkLUTsf$old_plot16 == 462] <- 462
# assign 475 ne tag.. not used in any other year, but nearby plots have NE tag in that range.. idk why it wouldn't be true
checkLUTsf$tag_ne_orig[grepl(475, checkLUTsf$jgs_plot) & checkLUTsf$jgs_plot_num2 == 475] <- 475
# the other numbers outstanding (e.g. PDF diagram numbers) don't match up with anything in any dataset so ditch
# or maybe keep possible numbers? as other/backup tags
checkLUTsf <- mutate(checkLUTsf, tag_other = ifelse(tag_sw_orig %in% c(883, 885, 417), gsub("NA, ", "", paste(jgs_plot_num2, plot_sw_tag_PDF, sep = ", ")), jgs_plot_num3))
checkLUTsf$date_est <- as.Date("1993-08-17") #17 August 1993
# remove anything that's all na
checkLUTsf <- checkLUTsf[,!sapply(checkLUTsf, function(x) all(is.na(x)))]
# clean up and add in 6 control plots in SF recovery from 2016
new_controls <- subset(sdlLT, snow == 1 & snow_recovery == 1 & is.na(x)) %>%
  mutate(tag_ne_orig = old_plot16,
         tag_sw_orig = plot_num,
         date_est = as.Date("2016-06-16"),
         tag_other =NA) %>%
  dplyr::select(names(checkLUTsf))
# fill in spatial info from info in metadata
# Three moist meadow recovery control plots:   
# (locations are distance east of the plot, location along the 60 m snow fence (the last 30 m of which are now snow recovery)
# plot 81 (back-up # 97)  12 E, 51 N 
# plot 82 (back-up # 97) 20 E, 52 N 
# plot 83 (back-up tag # 99)  16 E, 45.5 N 
# Three Dry meadow recovery control plots: 
# plot 84 (back-up tag # 100) 41.3 E, 54 N   
# plot 85 (back-up tag # 225) 41.5 E, 52 N  
# plot 86 (back up tag # 874) 42 E 42 N

new_controls[new_controls$plot_num == 81, c("x", "ymin")] <- c(12, 51)
new_controls[new_controls$plot_num == 82, c("x", "ymin")] <- c(20, 52)
new_controls[new_controls$plot_num == 83, c("x", "ymin")] <- c(16, 45.5)
new_controls[new_controls$plot_num == 84, c("x", "ymin")] <- c(41.3, 54)
new_controls[new_controls$plot_num == 85, c("x", "ymin")] <- c(41.5, 52)
new_controls[new_controls$plot_num == 86, c("x", "ymin")] <- c(42, 42)
new_controls$ymax <- new_controls$ymin + 2

#join in other cleaned up snowfence sites
sdlLT_sf_final <- rbind(checkLUTsf, new_controls) %>%
  # add in spatial cols for wet meadow grid
  mutate(wm_x = NA, wm_y = NA) %>%
  #reorder cols
  dplyr::select(LTER_site:plot_num, date_est, fert:ymax, wm_x, wm_y, tag_sw_orig, tag_ne_orig, tag_other) %>%
  #rename spatial cols to reflect snowfence spatial
  rename_at(vars("x", "ymin", "ymax"), function(x) paste0("snow_",x))

# clean up environment
rm(drysf, mesicsf, sfsites_meta, sfsites, new_controls, checkLUTsf)



# -- review non snowfence ----
nsfsites <- data.frame(dat = oldsites[sitepos[2]:(sitepos[2]+16)]) %>%
  separate(dat, into = paste0("col", 1:15), sep = " +") %>%
  #clean up whitespace
  mutate_all(function(x) ifelse(x == "", NA, x))
names(nsfsites) <- c("fert", "dry_x", "dry_y", "dry", "mesic_x", "mesic_y", "mesic")
#drop any cols that are all NA
nsfsites <- nsfsites[2:nrow(nsfsites),!sapply(nsfsites, function(x) all (is.na(x)))]
# correct a few errors from lack of space
nsfsites$mesic[is.na(nsfsites$mesic)] <- nsfsites$mesic_y[is.na(nsfsites$mesic)]
nsfsites$mesic_y[nsfsites$mesic_y == nsfsites$mesic] <- str_extract(nsfsites$mesic_x[grepl("-", nsfsites$mesic_x)], ",[0-9].+")
nsfsites$mesic_x <- gsub(",.+", "", nsfsites$mesic_x)
nsfsites <- separate(nsfsites, dry_y, c("dry_ymin", "dry_ymax"), sep = "[-]") %>%
  separate(mesic_y, c("mesic_ymin", "mesic_ymax"), sep = "[-]") %>%
  separate(dry, c("dry", "dry_2002"), sep = ",") %>%
  mutate(mesic_tag_sw_orig = substr(mesic, 2,5),
         mesic_tag_ne_orig = str_extract(mesic, ",[0-9]{3}[*]{0,1}[)]"),
         mesic_tag_sw_2002 = ifelse(grepl("[)][(][0-9]{3},", mesic), str_extract(mesic, "[)][(][0-9]{3},"), str_extract(mesic, ",[0-9]{3},")),
         mesic_tag_sw_2004 = str_extract(mesic, "[(][0-9]{3}[+]."),
         mesic_tag_ne_2004 = str_extract(mesic, ",[0-9]{3}[+][+]"))


# split up
drynsf <- nsfsites[,grep("fert|dry", names(nsfsites))] %>%
  mutate(veg_class = "DM",
         snow = 0) %>%
  rename_all(function(x) gsub("dry_", "", x)) %>%
  rename(tag_sw_orig = dry,
         # 2002 tags are actually NE tags
         tag_ne_orig = `2002`) %>%
  # make all that should be numeric, numeric (remove non-number chars)
  mutate_at(vars("x", "ymin", "ymax", "tag_sw_orig", "tag_ne_orig"), parse_number) %>%
  #add tag_sw_2004
  mutate(tag_sw_2004 = NA,
         tag_sw_2002 = NA,
         tag_ne_2004 = NA)
mesicnsf <- nsfsites[,grep("fert|mes", names(nsfsites))] %>%
  mutate(veg_class = "MM",
         snow = 0) %>%
  rename_all(function(x) gsub("mesic_", "", x)) %>%
  # make all that should be numeric, numeric (remove non-number chars)
  mutate_at(vars("x", "ymin", "ymax", names(.)[grepl("tag", names(.))]), parse_number) %>%
  # drop mesic
  dplyr::select(-mesic)

# stack non snowfence sites
nsfsites_meta <- rbind(drynsf[,names(mesicnsf)], mesicnsf) 
rm(drynsf, mesicnsf, nsfsites)

# compare against saddle LUT from stitching datasets together
checkLUTnsf <- left_join(sdlLT, nsfsites_meta) %>%
  filter(snow == 0 & veg_class != "WM") %>%
  # reorg cols
  dplyr::select(LTER_site, local_site, plot_num, fert, veg_class, snow, snow_recovery, x, ymin, ymax, 
                #sw plots
                metadata_edi138, old_plot_edi138, old_plot96, old_plot97LT, plot_sw_tag_PDF, tag_sw_orig, tag_sw_2002, tag_sw_2004,
                #ne plots
                jgs_plot:jgs_plot_num4, tag_ne_orig, tag_ne_2004, plot_ne_tag_PDF, plot_2003, old_plot2012, old_plot16)

# set aside XX1 and XX2 plots for now to clean up the rest
checkLUT_xx <- filter(checkLUTnsf, old_plot96 %in% c("XX1", "XX2", NA))
checkLUTnsf <- filter(checkLUTnsf, !old_plot96 %in% c("XX1", "XX2", NA))

# check cols
for(i in names(checkLUTnsf)[!names(checkLUTnsf) %in% c("tag_ne_orig", "tag_ne_2004", "tag_sw_orig", "tag_sw_2002", "tag_sw_2004")]){
  # remove any cols from other sources that match metadata SW tag perfectly or are all NA
  if(all(checkLUTnsf[i] == checkLUTnsf$tag_sw_orig) | all(is.na(checkLUTnsf[i]))){
    checkLUTnsf <- dplyr::select(checkLUTnsf, -i)
    print(paste("Removed", i))
  }
}
# only jgs_num1 matched with tag_sw_orig col

# infill original ne tags, if PSF tag matches last old plot num (2016) then assign
checkLUTnsf <- mutate(checkLUTnsf, 
                      tag_ne_orig = ifelse(is.na(tag_ne_orig) & plot_ne_tag_PDF == old_plot16, plot_ne_tag_PDF, tag_ne_orig),
                      # use jgs plotnum2 for ne tag if number used
                      tag_ne_orig = ifelse(is.na(tag_ne_orig) & tag_sw_2004 == jgs_plot_num2, jgs_plot_num2, tag_ne_orig))

# go through tag cols and eliminate any numbers that are accounted for
tagcols <- c("old_plot_edi138", "old_plot96", "old_plot97LT", "plot_sw_tag_PDF", "tag_sw_2004", "jgs_plot","jgs_plot_num2","jgs_plot_num3", "jgs_plot_num4","plot_ne_tag_PDF","plot_2003","old_plot2012", "old_plot16")
for(i in tagcols){
  # iterate through each row
  for(n in 1:nrow(checkLUTnsf)){
    if(checkLUTnsf[n,i] %in% checkLUTnsf[n, c("tag_sw_orig", "tag_ne_orig","tag_sw_2002", "tag_ne_2004")]){
      checkLUTnsf[n,i] <- NA
    }
  }
}

# remove any cols that are all NA
checkLUTnsf <- checkLUTnsf[,!sapply(checkLUTnsf, function(x)all(is.na(x)))]
# paste any numbers that weren't accounted for in tag_other
## can pull from edi_metadata138, old_plot96 and old_plot97LT and plot_sw_tag
checkLUTnsf <- mutate(checkLUTnsf, tag_other = gsub("NA, |, NA|NA", "", paste(old_plot_edi138, old_plot97LT, plot_sw_tag_PDF, sep = ", "))) %>%
  mutate(tag_other = ifelse(tag_other == "", NA, tag_other))
# infill date est:  
# 26 August 1993, except 397 which was established in the summer of 1997. 
# The control plots were established in the summer of 1996, exception of 870 which was established in the summer of 1997
checkLUTnsf$date_est <- as.Date("1993-08-26") #26 August 1993
# remove cols not needed anymore
checkLUTnsf <- dplyr::select(checkLUTnsf, LTER_site:plot_num, date_est, fert:ymax, tag_sw_orig:tag_sw_2002, tag_ne_orig, tag_ne_2004, tag_other) %>%
  # add notes col to rbind with control below
  mutate(notes = NA)


# triage control plots (XX1, XX2)
# metadata say XX1 and XX2 were relocated to new plots.. not sure why they're described as associated with 17 and 29 if originally went in the same spot
# "Plot XX1 was located 17 meters east of the snowfence, with a minimum distance of 84 meters and a maximum distance of 86 meters from the southern terminus of the snowfence in a northerly direction."
# "Plot XX2 was located 15 meters east of the snowfence, with a minimum distance of 77 meters and a maximum distance of 79 meters from the southern terminus of the snowfence in a northerly direction."
# collapse 29.5 and 29, remove XX1 and XX2 from 17 and 29 as associated old names and make new rows for XX1 and XX2
checkLUT_xx[checkLUT_xx$plot_num == 29.5, names(checkLUT_xx)[!names(checkLUT_xx) %in% c("LTER_site", "local_site", "veg_class", "snow", "snow_recovery")]] <- NA
# na old_plot96 XX1 and XX2
checkLUT_xx$old_plot96[1:2] <- NA
# add another row
checkLUT_xx[4, ] <- checkLUT_xx[3, ]
checkLUT_xx[3,c("plot_num", "old_plot96", "fert", "x", "ymin", "ymax")] <- c("XX1", "XX1", "CC", 17, 84, 86)
checkLUT_xx[4,c("plot_num", "old_plot96", "fert", "x", "ymin", "ymax")] <- c("XX2", "XX2", "PP", 15, 77, 79)
# add date col
checkLUT_xx$date_est <-  as.Date(c("1997-08-01", "1997-08-01","1993-08-26", "1993-08-26")) # assume XX plots were still established in 1993 with the others since once is a phosphorus plot
# add notes col
checkLUT_xx$notes <- c(rep("established summer 1997, exact date unknown, estimate provided",2), "relocated to current plot 17 in 1997 (different location)", "relocated to current plot 29 in 1997 (different location)")
# finally, clean up numbers for plots 17 and 29
# remove any instances of 872, 875, 876 in the control plot set (PDF wrong)
checkLUT_xx <- data.frame(sapply(checkLUT_xx, function(x) ifelse(x %in% c(872, 875, 876), NA, x)))
# only correction needed in ne tag for plot 17
checkLUT_xx <- mutate(checkLUT_xx, tag_ne_orig = ifelse(is.na(tag_ne_orig), tag_sw_2004, tag_ne_orig))

# go through tag cols and eliminate any numbers that are accounted for
tagcols <- c("old_plot_edi138", "old_plot96", "old_plot97LT", "plot_sw_tag_PDF", "tag_sw_2004", "jgs_plot","jgs_plot_num1", "jgs_plot_num2","jgs_plot_num3", "jgs_plot_num4","plot_ne_tag_PDF","plot_2003","old_plot2012", "old_plot16")
for(i in tagcols){
  # iterate through each row
  for(n in 1:nrow(checkLUT_xx)){
    if(checkLUT_xx[n,i] %in% checkLUT_xx[n, c("plot_num", "tag_sw_orig", "tag_ne_orig","tag_sw_2002", "tag_ne_2004")]){
      checkLUT_xx[n,i] <- NA
    }
  }
}
# remove any cols that are all NA
checkLUT_xx <- checkLUT_xx[,!sapply(checkLUT_xx, function(x)all(is.na(x)))]
# add colnames in nonsnowfence data frame to rbind
checkLUT_xx <- mutate(checkLUT_xx, tag_ne_2004 = NA, tag_other = NA)

#combine all non-snowfence plots
sdlLT_nsf_final <- rbind(checkLUTnsf,checkLUT_xx[,names(checkLUTnsf)]) %>%
  # add in spatial cols for wet meadow grid
  mutate(wm_x = NA, wm_y = NA) %>%
  #reorder cols
  dplyr::select(LTER_site:plot_num, date_est, fert:ymax, wm_x, wm_y, tag_sw_orig, tag_ne_orig, tag_sw_2002, tag_ne_2004, tag_other, notes) %>%
  #rename spatial cols to reflect snowfence spatial
  rename_at(vars("x", "ymin", "ymax"), function(x) paste0("snow_",x))

# clean up environment
rm(tagcols,checkLUTnsf, checkLUT_xx)



# -- review wet meadow ----
# get wet meadow
wm <- oldsites[grep("Wet Meadow", oldsites)] %>%
  str_extract(., "[0-9]{3}[(].*") %>%
  str_split(.,",") %>% unlist() %>% trimws()
wm_sites <- data.frame(plot_tag_orig = wm, veg_class = "WM", snow = 0) %>%
  mutate(fert = str_extract(plot_tag_orig, "[A-Z]{2}"),
         plot_tag_orig = parse_number(plot_tag_orig)) 

# read in old metadata with current numbers
oldnew_pos <- grep("^Treatment", oldsites_current)
wm_oldnew <- oldsites_current[(grep("^Wet Meadow", oldsites_current)+1):(grep("^Wet Meadow", oldsites_current)+16)] %>% 
  as.data.frame() %>% rename(fert='.') %>%
  mutate(plot_num = parse_number(str_extract(fert, "[0-9]{2},")),
         plot_tag_orig = parse_number(str_extract(fert, ", [0-9]{3}")),
         fert = str_extract(fert, "[A-Z]{2}"),
         veg_class = "WM",
         snow = 0)
#check old and oldnew line up
wm_sites <- left_join(wm_sites, wm_oldnew) # yes, no NAs

checkLUTwm <- left_join(sdlLT, wm_sites) %>%
  filter(veg_class == "WM") %>% # numbers from PDF don't match up with some.. maybe they were tags later replaced in 2004?
  # add in N and E directions (based on description in metadata and orientation in PDF)
  mutate(wm_y = ifelse(plot_num %in% c(33,46,47,39), 1,
                       ifelse(plot_num %in% c(37, 42, 38, 36), 2,
                              ifelse(plot_num %in% c(45, 34, 48, 40), 3, 4))),
         wm_x = ifelse(plot_num %in% c(33,37,45,41), 1,
                       ifelse(plot_num %in% c(46, 42, 34, 43), 2,
                              ifelse(plot_num %in% c(47, 38, 48, 35), 3, 4))))
# winnow down
wm_final <- checkLUTwm %>%
  #rename x and ymin ymax to indicate for snowfence
  rename(tag_sw_orig = plot_tag_orig) %>%
  dplyr::select(LTER_site, local_site, plot_num, fert, veg_class, snow, snow_recovery, x, ymin, ymax, wm_x, wm_y, plot_sw_tag_PDF:ncol(.))
# remove any cols that don't have values
wm_final <- wm_final[,!sapply(wm_final, function(x) all(is.na(x)))]
# manually reviewed.. all SW numbers the same except for 240 as PDF sw tag.. will leave in as backup SW number. No NEs numbers ever used, but exist on PDF
wm_final <- wm_final %>%
  # add snow x, ymin and ymax back in (have no value bc wet meadow sites not in snowfence area)
  mutate(snow_x = NA, snow_ymin = NA, snow_ymax = NA,
         # add date established (26 August 1993)
         date_est = as.Date("1993-08-26")) %>%
  dplyr::select(LTER_site:plot_num, date_est, fert:snow_recovery, snow_x, snow_ymin, snow_ymax, wm_x, wm_y, tag_sw_orig, plot_ne_tag_PDF, plot_sw_tag_PDF) %>%
  rename(tag_ne_orig = plot_ne_tag_PDF,
         tag_other = plot_sw_tag_PDF) %>%
  mutate(tag_other = ifelse(tag_other != tag_sw_orig, tag_other, NA))
# clean up
rm(checkLUTwm, wm_sites, wm_oldnew)



# -- combine final for write-out -----
# combine dm, mm, and wm site LUTs
names(wm_final)
names(sdlLT_sf_final)
names(sdlLT_nsf_final)
# first two are the same, can bind those, add in cols from third that are needed then bind third
sdlLT_final <- rbind(wm_final, sdlLT_sf_final) %>%
  mutate(tag_sw_2002= NA, tag_ne_2004 = NA, notes = NA) %>%
  dplyr::select(names(sdlLT_nsf_final)) %>%
  rbind(sdlLT_nsf_final) %>%
  mutate_at(vars(names(.)[grepl("snow|_sw|_ne", names(.))]), as.numeric) %>%
  mutate(plot_sort = factor(plot_num, levels = c("XX1", "XX2", 1:86))) %>%
  arrange(plot_sort) %>%
  dplyr::select(-plot_sort)



# -- PREP SPP COMP (ALL HITS SUMMED PER SPP PER PLOT) ----
sdlcomp <- subset(all_sppcomp, site == "sdl") %>%
  # drop plotid and block and other remnant cols from analysis
  dplyr::select(-c(plotid, block, trt2, simple_lifeform, simple_lifeform2)) %>%
  # convert codes to match anpp and richness dats
  mutate(meadow = recode(meadow, dry = "DM", wet = "WM", mesic = "MM"),
         trt = recode(trt, C = "CC", N = "NN", P = "PP", `N+P` = "NP"),
         # change wet meadow to no snow
         snow = ifelse(meadow == "WM", "no snow", snow),
         # recode snow and recovery field
         snow = recode(snow, `no snow` = 0, snow = 1),
         # correct plot 76 for recovery -- Tim coded as recovered but isn't
         snow_notes = ifelse(plot == 76, "in snowfence area",snow_notes),
         # code recovery as yr 2016 and later, if recovery noted
         snow_notes = ifelse((grepl("recov", snow_notes)& yr >= 2016), 1, 0),
         site = "SDL snowfence") %>%
  rename(USDA_Symbol = clean_code2,
         fert = trt,
         veg_class = meadow,
         snow_recovery = snow_notes,
         local_site = site,
         collect_date = date,
         plot_num = plot,
         year = yr)  %>%
  rename_at(grep("Scien", names(.)):ncol(.), function(x) paste0("USDA_", x)) %>%
  #rename_all(function(x) paste0(casefold(substr(x,1,1), upper = T), substr(x, 2, nchar(x)))) %>%
  #remove _x from SciName  
  rename_all(function(x) gsub("_x", "", x)) %>%
  # add in LTER site
  mutate(LTER_site = "Niwot Ridge LTER") %>%
  dplyr::select(LTER_site, local_site:ncol(.)) %>%
  # add m to collect_date for missing
  mutate(collect_date = ifelse(is.na(collect_date), "m", collect_date),
         # change 0.25 present value to 0.1
         hits = ifelse(hits == 0.25, 0.1, hits)) %>%
  # drop 286 from 1997 since can't match it to anything and supposedly only dry meadow non-snowfence sampled anyway
  filter(plot_num != 286) %>%
  # reorder cols
  dplyr::select(LTER_site:plot_num, fert, veg_class:ncol(.))

# to be sure recovered coded correctly
sapply(split(sdlcomp$snow_recovery, sdlcomp$year), unique) # looks good
sapply(split(sdlcomp$plot_num, sdlcomp$snow_recovery), function(x) length(unique(x))) # looks good
# final check
sapply(sdlcomp, function(x) sort(unique(x)))
# infill 2Scat with  elk scat
sdlcomp$simple_name[sdlcomp$USDA_Symbol == "2SCAT"] <- "Scat"



# -- PREP VERT SPP COMP ----
# for 2016 only
sffert_vert2016 <- mutate(vert2016, LTER_site = "Niwot Ridge LTER",
                          site = "SDL snowfence",
                          # recode meadow and fertilization
                          meadow = recode(meadow, dry = "DM", mesic = "MM", wet = "WM"),
                          trt = recode(trt, N = "NN", P = "PP", C = "CC", `N+P`= "NP"),
                          date = "m") %>%
  # drop functional groups -- rejoin USDA plant info bc sciname missing
  dplyr::select(LTER_site, site:simple_name) %>%
  rename(local_site = site,
         snow_recovery = snow_notes,
         veg_class = meadow,
         fert = trt,
         plot_num = plot,
         vertical = hit,
         year = yr,
         collect_date = date,
         USDA_Symbol = clean_code2) %>%
  # recode
  ## change plot 76 to no snow recovery
  ## wet meadow needs to be no snow
  mutate(snow_recovery = ifelse(plot_num == 76, "in snowfence area", snow_recovery),
         #wm is no snow (but also not sampled in 2016)
         snow = ifelse(veg_class == "WM", "no snow", snow),
         snow = recode(snow, `no snow` = 0, snow = 1),
         snow_recovery = ifelse(grepl("recov", snow_recovery), 1, 0)) %>%
  # prefix USDA_ to usda plants db cols
  left_join(distinct(dplyr::select(sdlcomp, USDA_Symbol, USDA_Scientific_Name:USDA_Growth_Habit))) %>%
  # reorder cols to be in same order as other dats
  dplyr::select(LTER_site:plot_num, fert, veg_class, snow:ncol(.)) %>%
  # be sure ordered by plot, point, vertical order
  arrange(plot_num, point, vertical)

# double check plots match sdlLT
vertcheck <- distinct(dplyr::select(sffert_vert2016, plot_num:snow_recovery)) %>% 
  mutate(plot_num = as.character(plot_num)) %>%
  left_join(sdlLT_final) # everything checks out (manually looked)

# double check final vals
sapply(sffert_vert2016, function(x)sort(unique(x)))



# -- PREP ANPP ----
# write anpp separately from richness..
# update 2019-11-18: TS says 2014 data are actually from 2012. Need to change 2014 in date and year column to 2012.
# also ANPP in 2012 was collected in 20x50cm frames, so convert 1996 and 1997 and 2012 to g per meter squared

sffert_anpp <- dplyr::select(anpp_2014, -spp_rich) %>%
  # specify 0 for gram and total weight where forb weight present
  mutate(wt_gr1 = ifelse(!is.na(wt_fb1) & is.na(wt_gr1), 0, wt_gr1),
         wt_tot1 = ifelse(!is.na(wt_fb1) & is.na(wt_tot1), wt_fb1, wt_tot1)) %>%
  gather(group, anpp_g, wt_gr1:ncol(.)) %>%
  filter(!is.na(anpp_g)) %>%
  mutate(rep = parse_number(group),
         group = gsub("wt_|[0-9]", "", group),
         group = recode(group, fb = "Forb", gr = "Graminoid", tot = "Total"),
         snow = recode(snow, no_snow = 0, snow = 1)) %>%
  rename(fert = fert_tmt) %>%
  # add site
  mutate(LTER_site = "Niwot Ridge LTER", local_site = "SDL snowfence",
         snow_recovery = 0) %>%
  #reorder cols
  dplyr::select(LTER_site, local_site, year, collect_date, plot_num, old_plot_num, veg_class, fert, snow, snow_recovery, group, rep, anpp_g) %>%
  # replace XX1x plot_nums with XXx
  mutate(plot_num = ifelse(grepl("XX", old_plot_num), old_plot_num, plot_num),
         # correct 29.5 to 29 (based on 397)
         plot_num = recode(plot_num, `29.5` = "29"),
         # change 2014 to 2012
         year = ifelse(year == 2014, 2012, year),
         collect_date = ifelse(is.na(collect_date), "m", gsub("2014", "2012", as.character(collect_date))))

# double check plot designations
anpp_plots <- distinct(dplyr::select(sffert_anpp, plot_num:snow)) %>%
  left_join(sdlLT_final)
# check that no snow_notes cols are NA (shouldn't be if everything matched correctly)
summary(is.na(anpp_plots$snow_recovery))
# manually checked that all plot nums match up with various cols in CTW LT and do

# plot out to check numbers (2014 seems high compared to earlier years)
ggplot(subset(sffert_anpp, group == "Total" & rep == 1), aes(as.factor(year), anpp_g, group = plot_num)) +
  geom_line() +
  geom_point() +
  facet_grid(fert~snow)
# still seems a bit high...
subset(sffert_anpp, group == "Total") %>%
  mutate(snow = recode(snow, '1' = "Snow", '0' = "No snow")) %>%
  ggplot(aes(as.factor(plot_num), anpp_g, col = as.factor(year), shape = veg_class)) +
  geom_point(size = 2, alpha = 0.7) +
  ggtitle("SDL snowfence ANPP, by snow and fertilization treatment, colored by year, symbol by meadow") +
  labs(y = "g per 0.04 m^2", x = "plot") +
  scale_color_brewer(palette = "Set2", name = "Year") +
  scale_shape(name = "Meadow") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(size = 11.5)) +
  facet_grid(fert~ snow, scales = "free_x")
# write out plot for Tim to review
ggsave("alpine_addnuts/figures/SFFERT_review_anpp.pdf", width = 8, height = 6)


# convert all biomass to g per m2 and look at values
clip97 <- 0.2*0.2 #96 and 97 were 20x20cm per NWT metadata
clip12 <- 0.2*0.5 #tim thinks 2012 was 20x50cm bc that's what they used for nutnet

sffert_anpp <- mutate(sffert_anpp, 
                      g_per_m2 = ifelse(year == 2012, anpp_g/clip12, anpp_g/clip97))
# plot out again to compare..
# still seems a bit high...
subset(sffert_anpp, group == "Total") %>%
  mutate(snow = recode(snow, '1' = "Snow", '0' = "No snow")) %>%
  ggplot(aes(as.factor(plot_num), g_per_m2, col = as.factor(year), shape = veg_class)) +
  geom_point(size = 2, alpha = 0.7) +
  ggtitle("SDL snowfence ANPP, by snow and fertilization treatment, colored by year, symbol by meadow") +
  labs(y = "g per 0.04 m^2", x = "plot") +
  scale_color_brewer(palette = "Set2", name = "Year") +
  scale_shape(name = "Meadow") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(size = 11.5)) +
  facet_grid(fert~ snow, scales = "free_x") # i don't think 20x50 is right in this case.. suggests trt had no effect. going with what's written in metadata (20x20cm all years) and leaving raw data as they are


# remove converted anpp and reorder cols by yr, plot, rep and group)
sffert_anpp <- mutate(sffert_anpp,
                      # make sorting col for plot since not numeric
                      plot_sort = factor(plot_num, levels = c("XX1", "XX2", 1:80))) %>%
  arrange(year, plot_sort, rep, group) %>%
  # rearrange cols
  dplyr::select(LTER_site:old_plot_num, fert, veg_class, snow:anpp_g) %>%
  # sounds like tim doesn't like anpp (said it's standing crop mass not true aboveground biomass..)
  rename(mass_g = anpp_g)

# plot forb and gram only just to check for any funny values
subset(sffert_anpp, group == "Forb") %>%
  mutate(snow = recode(snow, '1' = "Snow", '0' = "No snow")) %>%
  ggplot(aes(as.factor(plot_num), mass_g, col = as.factor(year), shape = veg_class)) +
  geom_point(size = 2, alpha = 0.7) +
  ggtitle("SDL snowfence forb ANPP, by snow and fertilization treatment, colored by year, symbol by meadow") +
  labs(y = "g per 0.04 m^2", x = "plot") +
  scale_color_brewer(palette = "Set2", name = "Year") +
  scale_shape(name = "Meadow") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(size = 11.5)) +
  facet_grid(fert~ snow, scales = "free_x") 

subset(sffert_anpp, group == "Graminoid") %>%
  mutate(snow = recode(snow, '1' = "Snow", '0' = "No snow")) %>%
  ggplot(aes(as.factor(plot_num), mass_g, col = as.factor(year), shape = veg_class)) +
  geom_point(size = 2, alpha = 0.7) +
  ggtitle("SDL snowfence forb ANPP, by snow and fertilization treatment, colored by year, symbol by meadow") +
  labs(y = "g per 0.04 m^2", x = "plot") +
  scale_color_brewer(palette = "Set2", name = "Year") +
  scale_shape(name = "Meadow") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(size = 11.5)) +
  facet_grid(fert~ snow, scales = "free_x") 

# check anpp values
sapply(sffert_anpp, function(x) sort(unique(x))) # looks fine



# -- PREP 1996-1997 RICHNESS DATA (from nwt website) ----
# compare richness in 1997 to ctw-calculated richness
richness97 <- full_join(subset(all_biodiv, yr == 1997), subset(anpp_2014, year < 1998), by = c("plot" = "plot_num", "yr" ="year")) %>%
  filter(veg_class == "DM" & snow.y == "no_snow") %>%
  dplyr::select(plot, yr, S, spp_rich) %>%
  gather(met, val, S:spp_rich)
# infill richness for 96 so can plot connecting lines
richness97$val[richness97$met == "S" & richness97$yr == 1996] <- richness97$val[richness97$met == "spp_rich" & richness97$yr == 1996]
richness97 <- mutate(richness97, met = recode(met, S = "CTW 97 richness", spp_rich = "EDI 97 richness"))

compareyrs <- ggplot(richness97, aes(as.factor(yr), val)) +
  geom_line(aes(group = plot), col = "lightsalmon", alpha = 0.8) +
  geom_point(col = "indianred3", size = 2) +
  geom_point(data = subset(richness97, grepl("CTW", met) & yr == 1997), aes(as.factor(yr), val), col = "black", size = 2) +
  #geom_point(data = subset(richness97, !grepl("CTW", met) & yr == 1997), aes(as.factor(yr), val), col = "mediumpurple2") +
  scale_y_continuous(breaks = seq(6,28, 4), limits = c(6,28)) +
  labs(y = "Spp richness", x = "Year") +
  facet_wrap(~met)

comparetime <- subset(all_biodiv, plot <17 & site == "sdl" & !is.na(S)) %>%
  mutate(met = "All years, red = EDI, black = CTW") %>%
  ggplot(aes(yr, S)) +
  geom_line(aes(group = plotid), col = "grey50", alpha = 0.8) +
  geom_point(size = 2) +
  labs(y = NULL, x = "Year") +
  #geom_line(data = subset(anpp_2014, plot_num < 17), aes(year, spp_rich, group = plot_num), col = "orchid", alpha = 0.5, position = position_dodge(width = 0.1)) +
  geom_point(data = subset(anpp_2014, plot_num < 17), aes(year, spp_rich), col = "indianred3", alpha = 0.6, position = position_dodge(width = 0.1), size =2) +
  scale_y_continuous(breaks = seq(6,28, 4), limits = c(6,28)) +
  scale_x_continuous(breaks = seq(1996, 2012,4)) +
  facet_wrap(~met)

cowplot::plot_grid(compareyrs, comparetime)
# write out plot for Tim to decide what to do with spp richness in 96,97

# tim says still post richness data from 1996, 1997, we'll just add a note about count differences with rest of dataset
# format 96, 97 richness data for EDI
## bc plot codes in ANPP match, okay to go for richness
sffert_rich <- dplyr::select(anpp_2014, year:collect_date, spp_rich) %>%
  filter(!is.na(spp_rich)) %>%
  mutate(snow = recode(snow, no_snow = 0, snow = 1),
         # recode XXxand 29.5  plots
         plot_num = ifelse(grepl("XX", old_plot_num), old_plot_num, plot_num),
         # correct 29.5 to 29 (based on 397)
         plot_num = recode(plot_num, `29.5` = "29"),
         # add site
         LTER_site = "Niwot Ridge LTER", local_site = "SDL snowfence",
         # nothing in recovery yet
         snow_recovery = 0) %>%
  rename(fert = fert_tmt) %>%
  # sort by year and plot num (add sort col then remove)
  mutate(plot_sort = factor(plot_num, levels = c("XX1", "XX2", 1:80))) %>%
  arrange(year, plot_sort) %>%
  #reorder cols
  dplyr::select(LTER_site, local_site, year, plot_num, old_plot_num, fert, veg_class, snow, snow_recovery, spp_rich)

# double check plot designations
rich_plots <- distinct(dplyr::select(sffert_rich, plot_num:snow)) %>%
  left_join(sdlLT_final) # all checks out

# final check
sapply(sffert_rich, function(x) sort(unique(x)))


# -- WRITE OUT DATA -----
# specify outpath for writing data
outpath <- "alpine_addnuts/output_data/forEDI/"
write.csv(sdlcomp, paste0(outpath, "sffert_sppcomp_1997ongoing_forEDI.csv"), row.names = F)
write.csv(sffert_vert2016, paste0(outpath, "sffert_sppcomp_2016vert_forEDI.csv"), row.names = F)
write.csv(sffert_anpp, paste0(outpath, "sffert_anpp_1996ongoing_forEDI.csv"), row.names = F)
write.csv(sffert_rich, paste0(outpath, "sffert_spprich_19961997_forEDI.csv"), row.names = F)
write.csv(sdlLT_final, paste0(outpath, "sffert_plotlookup_forEDI.csv"), row.names = F)


# write to gdrive for Anna
# write to google drive for Anna
## get 418 folder
gdrive138 <- drive_find(pattern = "PKG_138", n = 30) %>% drive_ls()
# write anpp dataset
# rename anpp dat to datname on EDI
drive_upload(media = paste0(outpath, "sffert_anpp_1996ongoing_forEDI.csv"),
             path = gdrive138[grep("clean", gdrive138$name, ignore.case = T),], 
             name = "saddferb.ts.data.csv", overwrite = T)
# write spp comp
## > write all years, hits summed by spp by plot
drive_upload(media = paste0(outpath, "sffert_sppcomp_1997ongoing_forEDI.csv"),
             path = gdrive138[grep("clean", gdrive138$name, ignore.case = T),], 
             name = "saddfert.ts.data.csv", overwrite = T)
## > write 2016 vertical sppcomp data 
drive_upload(media = paste0(outpath, "sffert_sppcomp_2016vert_forEDI.csv"),
             path = gdrive138[grep("clean", gdrive138$name, ignore.case = T),], 
             name = "saddferv.ts.data.csv", overwrite = T)
# write spp richness
# give it name similar to other datasets
drive_upload(media = paste0(outpath, "sffert_spprich_19961997_forEDI.csv"),
             path = gdrive138[grep("clean", gdrive138$name, ignore.case = T),], 
             name = "saddferr.ts.data.csv", overwrite = T)
# write plot lookup table
# give it name similar to other datasets
drive_upload(media = paste0(outpath, "sffert_plotlookup_forEDI.csv"),
             path = gdrive138[grep("clean", gdrive138$name, ignore.case = T),], 
             name = "saddfert.ts.sitelut.csv", overwrite = T)
