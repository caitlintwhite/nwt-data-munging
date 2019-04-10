# extended summer sensitivity analysis

# script purpose:
# compare nwt extended summer pca metric using different input temp and precip datasets 


# -- SETUP -----
rm(list = ls())
source("extended_summer/AET_functions.R")
source("extended_summer/calculate_climate_functions.R")
options(stringsAsFactors = F)

# read in data:
nwt_met_allyrs <- read.csv("extended_summer/output_data/suding/allyrs/hcn_suding.csv")
nwt_met_subset <- read.csv("extended_summer/output_data/suding/sensitivity_subset/hcn_suding_19902013.csv")
jennings_met <- read.csv("extended_summer/output_data/jennings/hcn_jennings.csv")
snow <- read.csv("http://niwot.colorado.edu/data_csvs/saddsnow.dw.data.csv",
                 na.strings = c("NA", " ", "NaN"),
                 strip.white = TRUE,
                 stringsAsFactors = F)


# review
str(nwt_met_allyrs)
str(nwt_met_subset)
str(jennings_met) # needs date column

# set lapse rate for aet function
lr<-c(0,0,0,0,0,0,0,0,0,0,0,0) # <-- CTW: used by EF for NWT renewal 2015

nwt_allyrs <- aet(station.elev=3528, site.elev=3528, lapse=lr, fc=75, latitude=40.05, m = "metric")
nwt_allyrs_aet <- AETecoyear(nwt_allyrs, "extended_summer/output_data/suding/allyrs/extsum_pca_input/")

nwt_subset <- aet(station.elev=3528, site.elev=3528, lapse=lr, fc=75, latitude=40.05, m = "metric")
nwt_subset_aet <- AETecoyear(nwt_subset, "extended_summer/output_data/suding/sensitivity_subset/extsum_pca_input/")

jennings_subset <- aet(station.elev=3528, site.elev=3528, lapse=lr, fc=75, latitude=40.05, m = "metric")
jennings_subset_aet <- AETecoyear(jennings_subset, "extended_summer/output_data/jennings/extsum_pca_input/")


plot(1:12,aet_results75[31,14:25])

par(mfrow = c(round(nrow(aet_results75)/8),round(nrow(aet_results75)/4)))
par(mar=c(1,1,1,1))
for(i in 1:nrow(aet_results75)){ 
  plot(1:12,aet_results75[i, 
                          # DEF from Jan to Dec
                          which(colnames(aet_results75) == "DEF.Jan"):which(colnames(aet_results75) == "DEF.Dec")])
  text(3, max(aet_results75[i, which(colnames(aet_results75) == "DEF.Jan"):which(colnames(aet_results75) == "DEF.Dec")]), 
       labels = paste0(aet_results75[i], "\nDEF"),
       pos = 1, offset = 0.5, vfont = NULL,
       cex = 1, col = "red")
}
dev.off()

boxplot(aet_results75[,which(colnames(aet_results75) == "AET.Jan"):which(colnames(aet_results75) == "AET.Dec")])
boxplot(aet_results75[,which(colnames(aet_results75) == "DEF.Jan"):which(colnames(aet_results75) == "DEF.Dec")])
boxplot(aet_results75[,which(colnames(aet_results75) == "SUR.Jan"):which(colnames(aet_results75) == "SUR.Dec")])
boxplot(aet_results75[,which(colnames(aet_results75) == "PET.Jan"):which(colnames(aet_results75) == "PET.Dec")])

# calculate climate
dat <- read.csv("extended_summer/output_data/suding/allyrs/hcn_suding.csv", 
                stringsAsFactors = FALSE)
tmin <- "TMIN"
tmax <- "TMAX"
tmean <- "no"
date <- "date"

pptdat <- "dat"
precip <- "PCP"
correction <- FALSE

