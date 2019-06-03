# extended summer sensitivity analysis

# script purpose:
# compare nwt extended summer pca metric using different input temp and precip datasets 


# -- SETUP -----
rm(list = ls())
source("extended_summer/AET_functions.R")
source("extended_summer/calculate_climate_functions.R")
source("extended_summer/generate_extended_summerPCA.R")
library(ggplot2)
library(dplyr)
options(stringsAsFactors = F)
theme_set(theme_bw())


# -- FUNCTIONS TO READ IN EDI DATASETS -----
# SCE + CTW code to determine most recent version of package ID and read in current dataset on EDI
#function to determine current version of data package on EDI
getCurrentVersion<-function(edi_id){
  versions=readLines(paste0('https://pasta.lternet.edu/package/eml/knb-lter-nwt/', edi_id), warn=FALSE)
  currentV <- max(as.numeric(versions))
  return(currentV)
}
# function to get entity ID for current version
getEntityId <- function(edi_id, version){
  entID <- readLines(paste0('https://pasta.lternet.edu/package/eml/knb-lter-nwt/', edi_id, "/", version, "/"), warn=FALSE)[1]
  entID <- gsub(paste0("http.*/",edi_id,"/",version,"/"), "", entID) # remove all chars except what comes after last /
  return(entID)
}
# reads in tabular dataset for data package that has only one csv data file (could make more generic with read table, but should know what you're reading in to use)
getTabular <- function(edi_id, na_vals = c("", "NA", NA, NaN, ".", "NaN", " ")){
  v <- getCurrentVersion(edi_id)
  id <- getEntityId(edi_id, v)
  dat <- read.csv(paste0("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.", edi_id, ".", v, 
                         "&entityid=", id),
                  strip.white =TRUE, na.strings = na_vals)
  print(paste0("Reading in knb-lter-nwt.", edi_id, ".", v))
  return(dat)
}



# set lapse rate for aet function
lr<-c(0,0,0,0,0,0,0,0,0,0,0,0) # <-- CTW: used by EF for NWT renewal 2015

# read in data:
nwt_met_allyrs <- read.csv("extended_summer/output_data/suding/allyrs/hcn_suding.csv")
nwt_met_subset <- read.csv("extended_summer/output_data/suding/sensitivity_subset/hcn_suding_19902013.csv")
jennings_met <- read.csv("extended_summer/output_data/jennings/hcn_jennings.csv")
ctw_met <- read.csv("extended_summer/output_data/ctw/hcn_ctw.csv")
snow <- getTabular(31)
lakedat <- getTabular(106)

# review
str(nwt_met_allyrs)
str(nwt_met_subset)
str(jennings_met)

# summarize datasets that aren't dependent on temp or precip (i.e. can be used in any sensitvity analysis for NWT extended summer PCA)
nwt_allyrs_snowmelt <- summarizeSnowmelt(snow, outpath = "extended_summer/output_data/")
nwt_allyrs_ice <- summarizeIceoff(lakedat, outpath = "extended_summer/output_data/")

# summarize datasets sensitive to input daily temp and precip

# --- NWT NSF REWNEWAL + CTW INFILL, ALL YEARS -----
# nwt, all years, renewal data + ctw 2015-2017 infilled temp and precip data for ASM 2019
nwt_allyrs <- aet(nwt_met_allyrs, station.elev=3528, site.elev=3528, lapse=lr, fc=75, latitude=40.05)
nwt_allyrs_aet <- AETecoyear(nwt_allyrs, "extended_summer/output_data/suding/allyrs/extsum_pca_input/")
nwt_allyrs_temp <- summarizeTemp(nwt_met_allyrs, date = "date", tmin = "TMIN", tmax = "TMAX", outpath = "extended_summer/output_data/suding/allyrs/extsum_pca_input/")
nwt_allyrs_ppt <- summarizePrecip(nwt_met_allyrs,date = "date", precip = "PCP", outpath = "extended_summer/output_data/suding/allyrs/extsum_pca_input/")
nwt_allyrs_climate <- compileClimate(temp = nwt_allyrs_temp, precip = nwt_allyrs_ppt, AET = nwt_allyrs_aet, snowmelt = nwt_allyrs_snowmelt, ice = nwt_allyrs_ice, outpath = "extended_summer/output_data/suding/allyrs/extsum_pca_input/")
nwt_allyrs_PCA <- extendedSummer(nwt_allyrs_climate, "eco_year", outpath = "extended_summer/output_data/suding/allyrs/")


# -- NWT RENEWAL DATA, SUBSET TO JENNINGS ET AL YEARS-----
nwt_subset <- aet(nwt_met_subset, station.elev=3528, site.elev=3528, lapse=lr, fc=75, latitude=40.05)
nwt_subset_aet <- AETecoyear(nwt_subset, "extended_summer/output_data/suding/sensitivity_subset/extsum_pca_input/")
nwt_subset_temp <- summarizeTemp(nwt_met_subset, date = "date", tmin = "TMIN", tmax = "TMAX", outpath = "extended_summer/output_data/suding/sensitivity_subset/extsum_pca_input/")
nwt_subset_ppt <- summarizePrecip(nwt_met_subset,date = "date", precip = "PCP", outpath = "extended_summer/output_data/suding/sensitivity_subset/extsum_pca_input/")
nwt_subset_climate <- compileClimate(temp = nwt_subset_temp, precip = nwt_subset_ppt, AET = nwt_subset_aet, snowmelt = nwt_allyrs_snowmelt, ice = nwt_allyrs_ice, outpath = "extended_summer/output_data/suding/sensitivity_subset/extsum_pca_input/")
nwt_subset_PCA <- extendedSummer(nwt_subset_climate, "eco_year", outpath = "extended_summer/output_data/suding/sensitivity_subset/")


# -- JENNINGS ET AL. 2018 INFILLED DATA -----
jennings_subset <- aet(jennings_met, station.elev=3528, site.elev=3528, lapse=lr, fc=75, latitude=40.05, m = "metric")
jennings_subset_aet <- AETecoyear(jennings_subset, "extended_summer/output_data/jennings/extsum_pca_input/")
jennings_subset_temp <- summarizeTemp(jennings_met, date = "date", tmin = "TMIN", tmax = "TMAX", outpath = "extended_summer/output_data/jennings/extsum_pca_input/")
jennings_subset_ppt <- summarizePrecip(jennings_met,date = "date", precip = "PCP", outpath = "extended_summer/output_data/jennings/extsum_pca_input/")
jennings_subset_climate <- compileClimate(temp = jennings_subset_temp, precip = jennings_subset_ppt, AET = jennings_subset_aet, snowmelt = nwt_allyrs_snowmelt, ice = nwt_allyrs_ice, outpath = "extended_summer/output_data/jennings/extsum_pca_input/")
jennings_subset_PCA <- extendedSummer(jennings_subset_climate, "eco_year", outpath = "extended_summer/output_data/jennings/")



# --- CTW INFILL ONLY, ALL YEARS -----
# nwt, all years, ctw 2010-2018 infilled temp and precip data (following chart metadata methods, no nsf data)
nwt_raw_allyrs <- aet(ctw_met, station.elev=3528, site.elev=3528, lapse=lr, fc=75, latitude=40.05)
nwt_raw_aet <- AETecoyear(nwt_raw_allyrs, "extended_summer/output_data/ctw/extsum_pca_input/")
nwt_raw_temp <- summarizeTemp(ctw_met, date = "date", tmin = "TMIN", tmax = "TMAX", outpath = "extended_summer/output_data/ctw/extsum_pca_input/")
nwt_raw_ppt <- summarizePrecip(ctw_met,date = "date", precip = "PCP", outpath = "extended_summer/output_data/ctw/extsum_pca_input/")
nwt_raw_climate <- compileClimate(temp = nwt_raw_temp, precip = nwt_raw_ppt, AET = nwt_raw_aet, snowmelt = nwt_allyrs_snowmelt, ice = nwt_allyrs_ice, outpath = "extended_summer/output_data/ctw/extsum_pca_input/")
nwt_raw_PCA <- extendedSummer(nwt_raw_climate, "eco_year", outpath = "extended_summer/output_data/ctw/")


# -- COMPARE VISUALLY -----
jennings_subset_PCA$source <- "Jennings et al. 2018"
nwt_subset_PCA$source <- "NSF renewal"
nwt_allyrs_PCA$source <- "NSF renewal + CTW"
nwt_raw_PCA$source <- "Raw + CTW"

masterPCA <- rbind(jennings_subset_PCA, nwt_subset_PCA, nwt_allyrs_PCA, nwt_raw_PCA)
masterPCA$source <- factor(masterPCA$source, levels = c("Raw + CTW","NSF renewal + CTW",
                                                        "NSF renewal", "Jennings et al. 2018"))
PC1time_fig <- ggplot(masterPCA, aes(eco_year, sumallPC1)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  labs(y = "Extended Summer PC score",
       x = "Year",
       title = "Extended summer PC1 by daily temp and precip data source",
       subtitle = "top down: raw Saddle chart + CTW infilled 2010-2018 (following published dataset methods) (1982-2018); NWT NSF renewal\ndata + CTW infilled 2015-2018 (1982-2018); NSF renewal data (1991-2013); Jennings infilled (1991-2013)") +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  facet_grid(source~., labeller = label_wrap_gen(width = 9))

ggsave("extended_summer/figs/PCAsensitivity_PC1overtime.png", PC1time_fig)

deltaPC1time_fig <- jennings_subset_PCA %>%
  rename(sumallPC1.jennings = sumallPC1) %>%
  left_join(nwt_subset_PCA[c("eco_year", "sumallPC1")], by = "eco_year") %>%
  mutate(deltaPC1 = sumallPC1 - sumallPC1.jennings) %>%
  ggplot(aes(eco_year, deltaPC1)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 2008), col = "chocolate2", lwd = 2, alpha = .6) +
  geom_point() +
  ggtitle("Difference in NSF rewnewal PC1 and Jennings et al. derived PC1 has negative temporal trend") +
  annotate(geom = "text", x = 2008.5, y = 0.75, label = "sdl chart infilling\nends 2008", hjust = 0, col = "red") +
  geom_smooth(method = "lm") +
  labs(y = "NWT PC1 - Jennings PC1",
       x = "Year") +
  theme(plot.title = element_text(size = 10))
  
ggsave("extended_summer/figs/PCAsensitivity_deltaPC1_overtime.png", deltaPC1time_fig,
       units = "in", scale = 0.8)

# read in scores to plot
nwt_allyrs_loadings <- read.csv("extended_summer/output_data/suding/allyrs/NWT_sumallPCVarout_19822018.csv")
nwt_subset_loadings <- read.csv("extended_summer/output_data/suding/sensitivity_subset/NWT_sumallPCVarout_19912013.csv")
jennings_loadings <- read.csv("extended_summer/output_data/jennings/NWT_sumallPCVarout_19912013.csv")
ctw_loadings <- read.csv("extended_summer/output_data/ctw/NWT_sumallPCVarout_19822018.csv")


nwt_allyrs_loadings$source <- "NSF renewal + CTW infill 2015-2018"
nwt_subset_loadings$source <- "NWT NSF renewal"
jennings_loadings$source <- "Jennings et al. 2018"
ctw_loadings$source <- "Raw + CTW infill 2010-2018"

masterloadings <- rbind(nwt_allyrs_loadings,
                        nwt_subset_loadings,
                        jennings_loadings,
                        ctw_loadings)
masterloadings$source <- factor(masterloadings$source, levels = c("Raw + CTW infill 2010-2018","NSF renewal + CTW infill 2015-2018",
                                                        "NWT NSF renewal", "Jennings et al. 2018"))

sploadings_fig <- ggplot(masterloadings, aes(PC1, PC2)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_label(aes(label = variable, col = variable), fontface = "bold", size = 3) +
  #coord_equal() +
  #scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  labs(title = "PCA 'species' loadings comparison: Raw/CTW 1982-2018, NSF/CTW 1982-2018, NSF 1991-2013, Jennings 1991-2013") +
  #scale_color_brewer(palette = "Set1") +
  facet_grid(.~source) +
  theme(legend.position = "none",
        plot.title = element_text(size = 12))

ggsave("extended_summer/figs/PCAsensitivity_sploadings.png", sploadings_fig,
       width = 10,
       height = 4,
       units = "in")


yrloadings_fig <- masterPCA %>%
  mutate(decade = ifelse(eco_year < 2000, "1980/1990s",
                         ifelse(eco_year < 2010, "2000s", "2010s"))) %>%
  ggplot(aes(sumallPC1, sumallPC2)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_label(aes(label = eco_year, col = as.factor(eco_year)), fontface = "bold", size = 3) +
  #coord_equal() +
  #scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  labs(title = "PCA year loadings comparison: Raw/CTW 1982-2018, NSF/CTW 1982-2018, NSF 1991-2013, Jennings 1991-2013",
       subtitle = "CTW infilled Saddle chart missing data following published dataset methods") +
  labs(x = "NWT summer climate PC1", 
       y = "NWT summer climate PC2") +
  facet_grid(decade~source) +
  theme(legend.position = "none",
        plot.title = element_text(size = 9),
        plot.subtitle = element_text(size = 9))

ggsave("extended_summer/figs/PCAsensitivity_yrloadings.png", yrloadings_fig,
       width = 8,
       height = 8,
       units = "in", scale = 0.9)

# plot differences by input variables
jennings_long <- jennings_subset_PCA %>%
  select(-iceoff_GL4) %>%
  gather(var, jennings_val, sumallPC1:GSLthreedayneg3C)

nwt_subset_long <- nwt_subset_PCA %>% 
  select(-iceoff_GL4) %>%
  gather(var, nsf_val, sumallPC1:GSLthreedayneg3C)

long_compare <- left_join(nwt_subset_long, jennings_long, by = c("eco_year", "var")) %>%
  mutate(delta = nsf_val - jennings_val)

deltaviolin_fig <- ggplot(long_compare, aes(0, delta)) +
  geom_violin() +
  geom_hline(aes(yintercept = 0), col = "grey30", lty = 2, alpha = 0.6) +
  geom_jitter(aes(fill = eco_year), size = 2, pch = 21, alpha = 0.6, width = 0.2) +
  labs(y = "delta: NSF climate data - Jennings climate data",
       x = NULL) +
  coord_flip() +
  scale_fill_distiller(name = "Year", palette = "BrBG") +
  ggtitle("Difference in NWT Saddle summer climate metrics used in PCA, by metric") +
  facet_wrap(~var, scales = "free_x") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position  = c(0.9, 0.1),
        legend.background = element_rect(color = "black"))

ggsave("extended_summer/figs/PCAsensitivity_metricdelta.png", deltaviolin_fig, 
       width = 6, height = 5, units = "in", scale = 1.4)

climatemetrics_raw_fig <- ggplot(long_compare, aes(eco_year, nsf_val)) +
  geom_line(col = "dodgerblue2", alpha = 0.5) +
  geom_point(col = "dodgerblue2", alpha = 0.5) +
  geom_line(data = long_compare, aes(eco_year, jennings_val), color = "chocolate4", alpha = 0.5) +
  geom_point(data = long_compare, aes(eco_year, jennings_val), color = "chocolate4", alpha = 0.5) +
  labs(title = "NWT summer climate metrics used in PCA and PC1 and PC2 scores, by data source",
       subtitle = "Sources: blue = NWT NSF renewal data (1991-2013), brown = Jennings et al. 2018 (1991-2013)",
       y = "NWT summer climate value",
       x = "Year") +
  facet_wrap(~var, scales = "free_y")
  
ggsave("extended_summer/figs/PCAsensitivity_summermetrics.png", climatemetrics_raw_fig)

# plot(1:12,aet_results75[31,14:25])
# 
# par(mfrow = c(round(nrow(aet_results75)/8),round(nrow(aet_results75)/4)))
# par(mar=c(1,1,1,1))
# for(i in 1:nrow(aet_results75)){ 
#   plot(1:12,aet_results75[i, 
#                           # DEF from Jan to Dec
#                           which(colnames(aet_results75) == "DEF.Jan"):which(colnames(aet_results75) == "DEF.Dec")])
#   text(3, max(aet_results75[i, which(colnames(aet_results75) == "DEF.Jan"):which(colnames(aet_results75) == "DEF.Dec")]), 
#        labels = paste0(aet_results75[i], "\nDEF"),
#        pos = 1, offset = 0.5, vfont = NULL,
#        cex = 1, col = "red")
# }
# dev.off()
# 
# boxplot(aet_results75[,which(colnames(aet_results75) == "AET.Jan"):which(colnames(aet_results75) == "AET.Dec")])
# boxplot(aet_results75[,which(colnames(aet_results75) == "DEF.Jan"):which(colnames(aet_results75) == "DEF.Dec")])
# boxplot(aet_results75[,which(colnames(aet_results75) == "SUR.Jan"):which(colnames(aet_results75) == "SUR.Dec")])
# boxplot(aet_results75[,which(colnames(aet_results75) == "PET.Jan"):which(colnames(aet_results75) == "PET.Dec")])


