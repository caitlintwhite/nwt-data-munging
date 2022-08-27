#' ---
#' title: Prepare temperature data 
#' author: CTW  
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#' 
#' ### QC checks on precip pre gap-filling
#' 
#' Test report to:
#' 1. Read in NWT and neighbor station temperature datasets
#' 2. Check qualifying days don't overlap a non-NA day for NWT precip
#' 3. Check for breaks in time series
#' 4. Wide-format data for gap filling
#' 
#' All code will be displayed to show procedure and work out bugs


# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)

source("nwt_climate/R/ppt_qaqc_functions.R")
source("nwt_climate/R/fetch_data_functions.R") # to read in prepped tidy datasets

# set path to prepared data
datpath <- "~/Documents/nwt_lter/nwt_climate/data/"
#csvfiles <- list.files(paste0(datpath, "prep"), pattern = "csv", full.names = T)
# list all rds files in prepped data folder (full path directory)
rdsfiles <- list.files(paste0(datpath, "prep"), pattern = "rds", full.names = T)


# read in prepared precip data
chartppt <- get_tidydat("chartPPT", rdsfiles, "ppt")
c1logppt <- get_tidydat("c1log", rdsfiles, "ppt")
ameriflux <- get_tidydat("ameri", rdsfiles, "ppt")
snotel <- get_tidydat("sno", rdsfiles, "ppt")
ghcnd <- get_tidydat("ghc", rdsfiles, "ppt")


# -- REVIEW NWT -----
for(u in unique(chartppt$local_site)){
  check_qdays(subset(chartppt, local_site == u))
}

test <- qc_qdays(chartppt, "local_site")

reviewdat <- subset(test, grepl("ppt value falls", qdays_qcnote))

d1date <- reviewdat$date[reviewdat$local_site == "c1"]

ndays <- 15
subdat <- subset(test, date < d1date + ndays  & date > d1date - ndays)
ggplot(subdat, aes(date, raw, col = local_site)) +
  geom_path(alpha = 0.8) +
  geom_point(data = subset(subdat, grepl("ppt value falls", qdays_qcnote)), col = "black", pch = 1, size = 2) +
  geom_text(data = subset(subdat, qdays > 1), aes(label = qdays), size = 2)
  facet_wrap(~local_site, nrow = 3)
  
visual_qcreview(reviewdat, alldat = test, groupvar = "local_site")
do.call(gridExtra::grid.arrange, testplot)
ggplot(testplot)
