# script to grab elevation data for all nwt lter dataset points of interest
# author: caitlin.t.white@colorado.edu
# project: NWT LTER temperature synthesis (project PI = Chris Ray)

# script purpose: 
# not all plots have elevation, helpful for grouping sites for comparison in cluster analysis
# also not all coords in same projection (e.g., lat-lon vs. UTM N13)
# names not consistent across dataset (e.g., sometimes NWT-### and sometimes PIKA-###)
# standardize all coords, and site names so can have single look up table for reference/no to run this each time
# can source LUT

# notes:
# UTM format: 6 digits for easting, 7 digits for northing
# lat long epsg is EPSG:6269
# UTM that should be used for NWT is EPSG:26913 (NAD 1983 UTM 13N)



# -- SETUP ----
# load libraries needed
library(tidyverse)
library(sf)
#library(terra)
library(elevatr)

# source functions for reading in datasets
source("~/github/nwt-data-munging/nwt_climate/R/fetch_data_functions.R")
# ctw personal prefs for environemnt and display
options(stringsAsFactors = F)
theme_set(theme_bw())


# -- PREP SITE DATA -----
# need to download nwt plots from NWT LTER website, not available on EDI
nwtplots <- read_sf("/Users/scarlet/Documents/nwt_lter/temp_synth/lter_plots_2022_12_08/lter_plots.shp")
# utms present in a column but epsg is 6269 (lat lon)
st_crs(nwtplots) # epsg 6269, GCS unknown
str(nwtplots) # elevation is character
nwtplots$GPS_ELE <- as.numeric(nwtplots$GPS_ELE)

# -- 1. sites in nwt lter plots shp for temp synth ----
# pull any plot of interest for temperature:
# any snow, any lake or stream point, black sand, climate stations, all chris ray plots
nwtplots_tempsynth <- subset(nwtplots, PI == "Chris Ray" |
                               # climate stations
                               grepl("(D1|SAD|C1)-SSCREEN", SITECOD) & grepl("Losleben", PI, fixed = F) |
                               # gl4 just in case want to use met data from there
                               grepl("GL4BEL|GL4MET", SITECOD) |
                               # tvan east and west
                               grepl("Blanken", PI, fixed = F) |
                               # catchment sensor array
                               grepl("SN_", SITECOD) |
                               # saddle grid
                               grepl("PTQUAD", SITECOD) |
                               # aquatic sites in GLV
                               (PI == "Jen Morse" & grepl("GL4", SITECOD)) | # gl4 stream gauge
                               grepl("GL4_A", SITECOD) | # Hannah's point in lake (closer to some of pika occupancy sites)
                               PI == "Kelly Loria" |  # GL5, 3 and 2 sites
                               SITECOD == "GL1" |
                               grepl("RockGlacier", SITECOD)|  # GL1 stream gauge for Nel (close enough to lake)
                               grepl("Black Sand", PROJECT, fixed = F) | # black sand
                               # closest locations to gl4 inlet, outlet and lake
                               grepl("GL4 UPHILL NA|^GL4_A$|GL4 GAUG", SITECOD)
                               ) %>%
  distinct()

# check crs
st_crs(nwtplots_tempsynth) # is epsg 6269 (ellipse: GRS 1980, NAD 1983), could be set to epsg 4269
# > leave as is (epsg 6269)

# subset plots that need elevation 
nwtplots_needselev <- subset(nwtplots_tempsynth, is.nan(GPS_ELE))


# -- 2. unpublished pika data from chris -----
# pika study plots not in nwt plots
# --> sites in GLV and sites near west knoll
# set path to pika data
pikadat <- list.dirs("/Users/scarlet/Documents/nwt_lter/temp_synth/unpublished_data")
# make name of subfolder name of element
names(pikadat) <- gsub("^.*temp_synth[/]","",pikadat)
# clean up names for nested folders
names(pikadat) <- gsub("^.*[/]", "", names(pikadat))
# read contents of each folder to list
pikadat_list <- sapply(as.list(pikadat), function(x) list.files(x, full.names = T))

# cable gate
cgpika_meta <- read.csv(pikadat_list$`Cable Gate talus temperatures`[grep(".csv$", pikadat_list$`Cable Gate talus temperatures`)], strip.white = T, blank.lines.skip = T)
# glv and west knoll sites
# get tab names in excel workbook
glvpika_temp_sheets <- readxl::excel_sheets(pikadat_list$unpublished_data[grep("GLV", pikadat_list$unpublished_data)])
glvpika_meta <- read_excel(pikadat_list$unpublished_data[grep("GLV", pikadat_list$unpublished_data)], sheet = "metadata")
# > split glvpika into glv and westknolls because coord types are different

# pika habitat occupancy locations
pika_habocc_sites <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.17.2&entityid=313b4ae5a8cf0ce9434e24a6ceccbb73")
# pika demography locations
pika_demoT_lut <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.8.5&entityid=c1a7d0d58f5658cffccd829cf6bb6c18")

# unpublished pika
unpub_pikaobb_meta <- read.csv(pikadat_list$`temperature-pika-occ-survey-2021-in-situ`[grep(".csv$", pikadat_list$`temperature-pika-occ-survey-2021-in-situ`)], strip.white = T, blank.lines.skip = T, header = T)
unpub_pikasurvey_meta <- read.csv(pikadat_list$`NWT-pika-survey-temperature-metadata-2020-v2`[[2]])



# -- 3. other nwt locations that aren't in nwt plots shp ----
# gl4 outlet and inlet continuous logger
gl4in_coords <-  data.frame(SITECOD = "GL4 hobo inlet", lat = 40.055639, lon = -105.617102, PI = "Piet Johnson")
gl4out_coords <- data.frame(SITECOD = "GL4 hobo outlet",lat = 40.053809, lon = -105.622255, PI = "Piet Johnson")
gl4buoy <- data.frame(SITECOD = "GL4 buoy", lat = 40.05545, lon = -105.62091, PI = "Piet Johnson")


# -- 4. prep latlon-only dataframe for elev ----- 
# pull chris' glv lat lon sites
wk_pika_latlon <- subset(glvpika_meta, Site == "WestKnoll", select = c(Datalogger,Site,Easting,Northing)) %>%
  mutate(PI = "Chris Ray", Project = "Pika Demography") %>%
  rename(lat = Northing, lon = Easting, SITECOD = Datalogger)
# plot with nwtplots to compare range in location
ggplot() + 
  geom_point(data = wk_pika_latlon, aes(lon, lat)) + 
  geom_sf(data = subset(nwtplots_needselev, grepl("West|Mitch|Cabl", SITECOD)), aes(col = SITECOD))
# > these are spread out enough that may be worth it to pull elevation

# create a lat long dataset with same colnames
latlon_needselev <- rbind(gl4in_coords, gl4out_coords, gl4buoy) %>%
  mutate(Site = "GL4", Project = "Continuous monitoring GL4") %>%
  rbind(wk_pika_latlon[names(.)])
latlong_needs_elev_sf <- st_as_sf(latlon_needselev, coords = c("lon", "lat"))
latlong_needs_elev_sf <- st_set_crs(latlong_needs_elev_sf, st_crs(nwtplots))
st_crs(latlong_needs_elev_sf)


# -- 5. prep utms-only dataframe for elev -----
# create sf for pika demography sites since those move around (in UTMS)
utm_pikademo_sites_needselev <- subset(glvpika_meta, grepl("Green", Site), select = c(Datalogger:Northing)) %>% #pika_demoT_lut
  rename_all(function(x) casefold(x)) %>%
  rename(deployment_id = datalogger, years_insitu = year) %>%
  rbind(pika_demoT_lut[names(.)]) %>%
  # drop anything where coords are TBD
  subset(easting != "TBD") %>%
  mutate_at(c("easting", "northing"), as.numeric)

utm_pikademo_sites_needselev_sf <- st_as_sf(utm_pikademo_sites_needselev, coords = c("easting", "northing")) 
utm_pikademo_sites_needselev_sf <- st_set_crs(utm_pikademo_sites_needselev_sf, st_crs(26913))
st_crs(utm_pikademo_sites_needselev_sf)



#  -- PULL ELEVATION ----- 
# pull elevation data from USGS
nwtplots_needselev_eqps <- get_elev_point(nwtplots_noelev, prj = "EPSG:6269", src = "epqs")
# assign elevation (fetched in same order given so can do a simple assignment)
nwtplots_needselev$GPS_ELE <- nwtplots_needselev_eqps$elevation

# for latlon not in nwt plots
latlong_needs_elev_eqps <- get_elev_point(latlong_needs_elev_sf, prj = "EPSG:6269", src = "epqs")
latlon_needselev <- cbind(elev_m = latlong_needs_elev_eqps$elevation,latlon_needselev)
# for utms not in nwt plots
utm_pikademo_sites_needselev_eqps <- get_elev_point(utm_pikademo_sites_needselev_sf, prj = "EPSG:26913", src = "epqs")
utm_pikademo_sites_needselev <- cbind(elev_m = utm_pikademo_sites_needselev_eqps$elevation, utm_pikademo_sites_needselev)



# -- COMPILE ALL LOCATIONS -----
# standardize coords so that all have latlon and UTMs

# convert utm to latlong
utm_pika_tolatlon <- st_transform(utm_pikademo_sites_needselev_sf, st_crs(nwtplots))
st_crs(utm_pika_tolatlon)
# convert latlong to utm
latlon_to_utm <- st_transform(latlong_needs_elev_sf, st_crs(utm_pikademo_sites_needselev_sf))
st_crs(latlon_to_utm)

# pull utms for data frame, write sf with latlon out
pika_utms <- st_coordinates(utm_pikademo_sites_needselev_sf) %>%
  data.frame() %>%
  rename(northing = Y, easting = X)
pika_latlon <- st_coordinates(utm_pika_tolatlon) %>%
  data.frame() %>%
  rename(lat = Y, lon = X)
pika_coords <- cbind(pika_utms, pika_latlon)
pika_out <- cbind(pika_coords,elev_m = utm_pikademo_sites_needselev_eqps$elevation, utm_pika_tolatlon)

# other nwt sites with lat lon and utms
othernwt_utms <- st_coordinates(latlon_to_utm) %>%
  data.frame() %>%
  rename(northing = Y, easting = X)
othernwt_latlon <- st_coordinates(latlong_needs_elev_sf) %>%
  data.frame() %>%
  rename(lat = Y, lon = X)
othernwt_coords <- cbind(othernwt_utms, othernwt_latlon)
othernwt_out <- cbind(othernwt_coords, elev_m = latlong_needs_elev_eqps$elevation, latlong_needs_elev_sf)

# check names in all datasets
names(pika_out)
# [1] "easting"       "northing"      "lon"           "lat"           "elev_m"        "deployment_id" "site"  "years_insitu" "geometry"
names(othernwt_out)
#  [1] "easting"  "northing" "lon"      "lat"      "elev_m"   "SITECOD"  "PI"       "Site"     "Project"  "geometry"
names(nwtplots_tempsynth)
# [1] "SITECOD"  "UTM_E"    "UTM_N"    "GPS_ELE"  "LOC_TYP"  "PI"       "PROJECT"  "geometry"


# 1. prep sites not in nwt lter shp (pika and other nwt)
pika_out <- pika_out %>%
  mutate(PI = "Chris Ray", project = "Pika demography study") %>%
  # since already have code developed using SITECOD from nwt lter plots, use that
  rename(SITECOD = deployment_id) %>%
  dplyr::select(SITECOD, site, years_insitu, easting, northing, lat, lon, elev_m, PI, project, geometry)

# prep other sites then rbind pika_out
pikaother_out <- left_join(othernwt_out, glvpika_meta[c("Datalogger", "Site", "Year", "Easting")],
                          by = c("SITECOD" = "Datalogger", "lon" = "Easting", "Site")) %>%
  rename(site = Site, project = Project, years_insitu = Year) %>%
  dplyr::select(names(pika_out)) %>%
  rbind(pika_out)

# 2. nwt plots (shp) for temp synth: combine plots that neede elev with plots that already had it
nwtplots_tempsynth_out <- subset(nwtplots_tempsynth, !is.nan(GPS_ELE)) %>%
  rbind(nwtplots_needselev) %>%
  distinct() %>%
  arrange(PI, PROJECT, SITECOD) %>%
  #reformat names for rbinding
  rename(easting = UTM_E, northing = UTM_N, elev_m = GPS_ELE, project = PROJECT) %>%
  # add years_insitu to rbind with other sites
  mutate(years_insitu = NA,
         # need site col
         site = ifelse(PI %in% c("Mark Losleben", "Peter Blanken"), str_extract(SITECOD, "D1|C1|SAD|Tvan|GL4"),
                       ifelse(grepl("Black", SITECOD, ignore.case = T), str_extract(SITECOD, "Trough|Soddie|Lefty|EastKnoll|Audubon"),
                              ifelse(grepl("SN_", SITECOD), "Sensor node catchment",
                                     ifelse(grepl("PTQUAD", SITECOD), "Saddle grid", 
                                            ifelse(PI == "Chris Ray", SITECOD, str_extract(SITECOD, "GL."))
                                            )
                                     )
                              )
                       ))

# nwt plots shp lat lon
nwtplots_tempsynth_latlon <- st_coordinates(nwtplots_tempsynth_out) %>%
  data.frame() %>%
  rename(lon = X, lat = Y)
nwtplots_tempsynth_out <- cbind(nwtplots_tempsynth_latlon, nwtplots_tempsynth_out) %>%
  dplyr::select(names(pikaother_out))

# 3. combine both for a single sites dataset
# have cols for UTMS, cols for latlon and geometry in lat long (altho.. doesn't have to be in sf form)

allsites_out <- rbind(pikaother_out, nwtplots_tempsynth_out) %>%
  arrange(PI, project, site) %>%
  # infill missing project for Nel, Mark and Peter Blanken
  mutate(project = ifelse(grepl("Mark L", PI), "Climate",
                          ifelse(grepl("Tvan", SITECOD), "Ameriflux",
                                 ifelse(grepl("Nel", PI), "Stream sampling", project)
                          ))) %>%
  st_as_sf()

# see how it all looks before writing out
ggplot(allsites_out) +
  geom_sf()

plot(allsites_out)
# look at glv area to see PI nearness
ggplot(subset(allsites_out, grepl("Green|^GL", site))) +
  geom_sf(aes(color = PI)) # don't need both Kurt and Hannah's points since have Piet's coords for GL4 inlet and outlet
allsites_out <- subset(allsites_out, !grepl("Jen M|Hannah|Kurt", PI) & SITECOD != "GL4 GAUGE")
# okay to write out


# -- WRITE OUT -----
datpath <- "/Users/scarlet/Documents/nwt_lter/temp_synth/output/dat/"
saveRDS(allsites_out, paste0(datpath, "tempsynth_allsites_sf"))


