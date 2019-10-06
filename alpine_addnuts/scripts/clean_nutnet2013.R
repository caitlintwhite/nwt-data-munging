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
# each set it 
anpp2.wide$Forb == anpp1.1$BIOMASS[anpp1.1$VEGTYPE == "FORB"]
anpp2.wide$Grass == anpp1.1$BIOMASS[anpp1.1$VEGTYPE == "GRASS"]
anpp2.wide$Grass == anpp1.1$BIOMASS[anpp1.1$VEGTYPE == "GRASS"]
