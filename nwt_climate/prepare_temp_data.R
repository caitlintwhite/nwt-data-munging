#' ### Prepare temperature data 
#' 
#' Test report to:
#' 1. Read in NWT and neighbor station temperature datasets
#' 2. Show raw data (removing any infilled or flagged data that we wish to ignore for processing)
#' 3. Show datasets prepared for QC procedure
#' 
#' All code will be displayed to show procedure and work out bugs


# -- SETUP -----
# libraries needed
library(tidyverse)
library(lubridate)


# source scripts (this will change for package)
source("nwt_climate/R/fetch_data_functions.R")
source("nwt_climate/R/prep_data_functions.R")
source("nwt_climate/R/dataviz_functions.R")

# specify path to data directory (can be wherever, but raw input files should live in data/raw)
datpath <- "~/Documents/nwt_lter/nwt_climate/data/"
fluxpath <- "~/Documents/nwt_lter/nwt_climate/data/raw/AmeriFlux"
ghcndpath <- "~/Documents/nwt_lter/nwt_climate/data/raw/GHCNd"

# create subfolders for data out if they don't exist
for(i in c("qc", "infill", "homogenize")){
  if(!i %in% list.files(datpath)){
    dir.create(paste0(datpath, i))
  }
  rm(i) # clean up environment
}

# other way to write
sapply(list("qc", "infill", "homogenize"), function(x) 
  if(!x %in% list.files(datpath)){
    dir.create(paste0(datpath, x))
  }
)


snotel <- getSnotelNeighbors()
nwtchart <- getNWTcharts()
nwtchart_infilled <- getNWTchartsinfilled()
nwtlogger <- getNWTdailyloggers()
ameriflux <- getAmeriflux(fluxpath)
ghcnd <- getGHCND(ghcndpath)



# -- PREP DATA -----
ameriflux_prepped <- lapply(ameriflux, prepAmeriflux)
# review NAs in datasets
lapply(ameriflux_prepped, function(x) summary(is.na(x)))
purrr::map(ameriflux_prepped,  summary(is.na(.y)))


# viz data available for ameriflux, sdl
plot_all(listobject =nwtchart, timecol = "date", mets = c("airtemp", "ppt"))
plot_all(listobject =nwtlogger, timecol = "date", mets = c("airtemp"), plotNA = F)

plot_all_groups(snotel2[,!grepl("Flag", names(snotel2))], timecol = "Date", mets = c("Pre.*", "Air.*"), 
                groupvars = "Station.Name", plotNA = F)


ghcndattr <- names(ghcnd)[grepl("ATTR", names(ghcnd))]
ghcndmets <- gsub("_A.*$", "", ghcndattr)
plot_all_groups(ghcnd, timecol = "DATE", mets = ghcndmets, groupvars = "NAME", allvars = F)
plot_all_groups(ghcnd, timecol = "DATE", mets = ghcndmets, groupvars = "NAME", allvars = F, plotNA = F)

