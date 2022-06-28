# screen prepped tempdats for anomalous values within same station record
# > restrictive to instrument (e.g. not yet comparing chart to loggers)


# notes:
# temp boundaries for d1 are:
# temp boundaries for sdl are:
# temp boundaries for c1 are:

# -- SETUP -----
rm(list = ls())
library(tidyverse)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", ".", NA, NaN, "NA", "NaN", -6999)

# set path to climdat folder
datpath <- "c1_d1_sdl_clim/homogenize_climdat/"
# functions to qa temp
source(paste0(datpath,"scripts/flagging_functions.R"))
source(paste0(datpath,"scripts/oldscripts_sceadaptedfor2020/qa_functions.R"))

# read in prepped datasets as list
datfiles <- sort(list.files(paste0(datpath, "data/prep_data/ready_for_qa"), full.names = T))
datlist <- list()
for(d in 1:length(datfiles)){
  datlist[[d]] <- read.csv(datfiles[d], na.strings = na_vals)
  # make date col Date class
  datlist[[d]][["date"]] <- as.Date(datlist[[d]][["date"]],format = "%Y-%m-%d")
  names(datlist)[d] <- str_extract(datfiles[d],"(?<=qa[/]).*(?=.csv)")
}
View(datlist) # manually inspect
lapply(datlist, str)
names(datlist)



