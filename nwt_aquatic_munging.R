#'---
#' title: "NWT data munging: All lakes and stream data"
#' author: CTW
#' date: "`r format(Sys.Date())`"
#' output: github_document
#'---
#'
#' ## Script purpose
#' Summarize data availability and test temporal trends figures for a "core" aquatic dataset (primarily focused on Green Lake 4).
#' See GL4_WQ_waterchem_datamunging.md in this GitHub repository for preliminary data munging steps.
#' This script reads in and combines all aquatic-related datasets (13 total) from the NWT data portal that are currently featured as "signature" datasets on the website:
#'
#' * Lake water quality (1)
#' * Green Lake 4 phytoplankton (1)
#' * Lake and stream water chemistry (8)
#' * Stream discharge (3)


# -----------------
#+ script setup, echo = FALSE, message = FALSE, warning = FALSE, include = FALSE
# "include = FALSE" hides figures

#load needed libraries
library(tidyverse)
library(lubridate)

# assign elevation
elevation <- data_frame(site = c("ARIKAREE", "NAVAJO", "GREEN LAKE 5", "GREEN LAKE 4", 
                                 "MARTINELLI", "SADDLE STREAM 007", "GREEN LAKE 3", "GREEN LAKE 1", "ALBION"),
                        elevation_m = c(3785, 3725, 3615, 3550, 
                                        3415, 3400, 3436, 3431, 3360))
# note: can't find saddle gauge elevation in the metadata or anywhere else, made up number by eyeballing location 
# respective to martinelli gauge (isn't for analysis, just for ordering sites by elevations in plots)

# Green1 40.0508, -105.6056, 3431 m
# Green2 40.0494, -105.6108, 3411 m
# Green3 40.0519, -105.6158, 3436 m
# Green4 40.0553, -105.6203, 3566 m
# Green5 40.0522, -105.6306, 3625 m
# Albion   40.0464, -105.6036, 3360 m
# Silver    40.0294, -105.5825, 3141 m

# read in phytoplankton data
GL4_phytoplankton <- read_csv("http://niwot.colorado.edu/data_csvs/gl4_flowcam_phytocomp.dm.data.csv")

phytodat <- GL4_phytoplankton[c("lake", "date")] %>%
  distinct() %>%
  mutate(lake = "GREEN LAKE 4",
         location = "Lake",
         yr = year(date),
         dataset = "phytoplankton") %>%
  rename(site = lake) %>%
  dplyr::select(site, location, yr, date, dataset)

# water quality dataset
GLV_WQ <- read_csv("http://niwot.colorado.edu/data_csvs/water_quality_GLV.dm.data.csv")

wqdat <- GLV_WQ %>%
  filter(grepl("Inlet|0m|0.05m|Surface|3m|9m|Air", `depth/loc`)==TRUE) %>%
  mutate(location = ifelse(grepl("Inlet", `depth/loc`)==TRUE, "Inlet",
                           ifelse(`depth/loc`=="Outlet", "Outlet", "Lake")),
         local_site = gsub("GL", "GREEN LAKE ", local_site),
         local_site = gsub("ALB", "ALBION", local_site),
         dataset = "water quality",
         yr = year(date)) %>%
  rename(site = local_site) %>%
  dplyr::select(site, location, yr, date, dataset) %>% distinct()

wqdat %>%
  group_by(yr, site, location) %>%
  summarise(nobs = length(date)) %>%
  mutate(ID = paste(site, "_", location)) %>%
  ggplot(aes(yr, ID, fill = nobs)) +
  geom_tile() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal()  
  
# read in waterchemistry data
McKnight_GLV_waterchem <- read_csv("http://niwot.colorado.edu/data_csvs/glvwatsolu.dm.data.csv")
Caine_GL4_waterchem <- read_csv("http://niwot.colorado.edu/data_csvs/gre4solu.nc.data.csv")
Caine_ARI_waterchem <- read_csv("http://niwot.colorado.edu/data_csvs/ariksolu.nc.data.csv")
Caine_GL5_waterchem <- read_csv("http://niwot.colorado.edu/data_csvs/gre5solu.nc.data.csv")
Caine_MAR_waterchem <- read_csv("http://niwot.colorado.edu/data_csvs/martsolu.nc.data.csv")
Caine_ALB_waterchem <- read_csv("http://niwot.colorado.edu/data_csvs/albisolu.nc.data.csv")
Caine_SAD_waterchem <- read_csv("http://niwot.colorado.edu/data_csvs/sad007solu.mw.data.csv")
Caine_NAV_waterchem <- read_csv("http://niwot.colorado.edu/data_csvs/navasolu.nc.data.csv")

# date in year column in Nel's Albion dataset
missing_date <- Caine_ALB_waterchem$year[which(is.na(Caine_ALB_waterchem$date))]
# problem: there aren't 31 days in June.. assume it's June 30?
Caine_ALB_waterchem$date[which(is.na(Caine_ALB_waterchem$date))] <- as.Date("1993-06-30")

# pare down datasets to just location, year, and date
Caine_GL4_waterchem <- Caine_GL4_waterchem[c("samp_loc", "date")] %>% distinct()
Caine_ARI_waterchem <- Caine_ARI_waterchem[c("samp_loc", "date")] %>% distinct()
Caine_GL5_waterchem <- Caine_GL5_waterchem[c("local_site", "date")] %>% distinct() %>% rename(samp_loc = local_site)
Caine_MAR_waterchem <- Caine_MAR_waterchem[c("samp_loc", "date")] %>% distinct()
Caine_ALB_waterchem <- Caine_ALB_waterchem[c("samp_loc", "date")] %>% distinct()
Caine_SAD_waterchem <- Caine_SAD_waterchem[c("samp_loc", "date")] %>% distinct()
Caine_NAV_waterchem <- Caine_NAV_waterchem[c("samp_loc", "date")] %>% distinct()

Caine_GL4_waterchem$samp_loc <- "GREEN LAKE 4"
Caine_NAV_waterchem$samp_loc <- "NAVAJO"

Caine_waterchem <- rbind(Caine_GL4_waterchem, Caine_ARI_waterchem, 
                         Caine_SAD_waterchem, Caine_GL5_waterchem, 
                         Caine_MAR_waterchem, Caine_ALB_waterchem, Caine_NAV_waterchem) %>% 
  distinct() %>% 
  mutate(location = ifelse(month(date) %in% c(1:5, 11, 12) & grepl("GREEN LAKE",samp_loc)==TRUE, "Lake", "Outlet"),
         yr = year(date),
         dataset = "water chemistry") %>% 
  rename(site = samp_loc) %>%
  dplyr::select(site, location, yr, date, dataset)

McK_GLV_waterchem <- McKnight_GLV_waterchem %>%
  mutate(local_site = gsub("GL", "GREEN LAKE ", local_site),
         local_site = gsub("ALB", "ALBION", local_site)) %>%
  rename(site = local_site) %>%
  filter(`depth/loc` %in% c("Inlet", "Outlet", "Surface", "0m", "0.5m", "3m", "9m")) %>%
  mutate(`depth/loc` = ifelse(`depth/loc` %in% c("Inlet", "Outlet"), `depth/loc`, "Lake"),
         yr = year(date),
         dataset = "water chemistry") %>%
  dplyr::select(site, `depth/loc`, yr, date, dataset) %>%
  distinct() %>%
  rename(location = `depth/loc`)

NWT_waterchem <- rbind(Caine_waterchem, McK_GLV_waterchem) %>% distinct()


NWT_waterchem %>%
  group_by(yr, site, location) %>%
  summarise(nobs = length(date)) %>%
  mutate(ID = paste(site, "_", location)) %>%
  ggplot(aes(yr, ID, fill = nobs)) +
  geom_tile() +
  scale_fill_distiller(palette = "PuBu", direction = 1) +
  theme_minimal()

# read in discharge data
flow_ALB <- read_csv("http://niwot.colorado.edu/data_csvs/albdisch.nc.data.csv",
                     na = c("", "NA", "NaN"))
#flow_ALB_edi <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.102.12&entityid=bf6b1a1d40dfe37ff2d9f84990787dc9")
flow_GL4 <- read_csv("http://niwot.colorado.edu/data_csvs/gl4disch.nc.data.csv",
                     na = c("", "NA", "NaN"))
flow_MAR <- read_csv("http://niwot.colorado.edu/data_csvs/mardisch.nc.data.csv",
                     na = c("", "NA", "NaN"),
                     trim_ws = TRUE)

# assign locations
flow_ALB$site <- "ALBION"
flow_GL4$site <- "GREEN LAKE 4"
flow_MAR$site <- "MARTINELLI"

# row-bind all together for master discharge dataset
flow_NWT <- rbind(flow_ALB, flow_GL4, flow_MAR)  
flow_NWT$location <- "Outlet"
flow_NWT$ID <- paste(flow_NWT$site, flow_NWT$location)
flow_NWT$ID <- factor(flow_NWT$ID, levels = c("ALBION Outlet", "MARTINELLI Outlet", "GREEN LAKE 4 Outlet"))

#ggplot(flow_NWT, aes(date, log(discharge), col=site)) + geom_point(na.rm=TRUE, alpha=0.5) + theme_minimal() + theme(legend.position = "none")

#------------------
#' ## Data availability
#' 
#+ data availability figures, echo = FALSE, warning = FALSE, message =FALSE, fig.width = 8, fig.height = 6

# heat map of flow data
filter(flow_NWT, !is.na(discharge)) %>%
  mutate(yr = year(date),
         ID = gsub(" O", "\nO", ID), 
         ID = factor(ID, levels = c("ALBION\nOutlet","MARTINELLI\nOutlet", "GREEN LAKE 4\nOutlet"))) %>%
  group_by(yr, site, location, ID) %>%
  summarise(nobs = length(date)) %>%
  ggplot() +
  geom_tile(aes(yr, ID, fill=nobs), col="grey50") +
  scale_fill_distiller(name = "# days", palette = "PuBu", direction = 1) +
  scale_x_continuous(breaks = seq(1980, 2017, 4), expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  labs(x = "Year", y = "Sampling location\n(by elevation, lowest to highest)",
       title = paste("1. NWT LTER daily stream discharge: data availability,", min(year(flow_NWT$date)), "-", max(year(flow_NWT$date))),
       subtitle = "Colored by number of days per year with discharge data") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

filter(flow_NWT, !is.na(discharge)) %>%
  mutate(yr = year(date)) %>%
  group_by(yr, site, location, ID) %>%
  summarise(nobs = length(date)) %>%
  ggplot() +
  geom_point(aes(yr, nobs), col="blue") +
  geom_line(aes(yr, nobs)) +
  scale_y_continuous(breaks = seq(0, 365, 50)) +
  scale_x_continuous(breaks = seq(1980, 2016, 4)) +
  labs(x="Year", y = "Total days with discharge data",
       title = paste("2. Alternative plot: NWT stream discharge data availability,", min(year(flow_NWT$date)), "-", max(year(flow_NWT$date))),
       subtitle = "Arrayed by sampling location (lowest elevation to highest elevation)") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  facet_grid(.~ID)

# put everything together minus discharge
nwt_aquatic <- rbind(phytodat, wqdat, NWT_waterchem) %>% 
  as.data.frame() %>%
  merge(elevation) %>%
  mutate(site = gsub("SADDLE STREAM 007", "SADDLE STREAM", site),
    location_rank = ifelse(location == "Inlet", 2,
                                ifelse(location == "Lake", 1, 0)),
         site_order = elevation_m + location_rank,
         ID = paste(site, location))
nwt_aquatic$ID <- factor(nwt_aquatic$ID, levels = unique(nwt_aquatic$ID[order(nwt_aquatic$site_order)]), ordered=TRUE)

nwt_aquatic %>%
  dplyr::select(site, ID, yr, date) %>%
  distinct() %>%
  group_by(site, ID, yr) %>%
  summarise(nobs = length(date)) %>%
  ggplot(aes(yr, ID, fill=nobs)) + 
  geom_tile(col="grey50") +
  scale_fill_distiller(name="# days", palette= "Blues", direction = 1) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1980, 2016,4)) +
  scale_y_discrete(expand = c(0,0)) +
  labs(x="Year", y="Sampling location (by elevation, highest to lowest)",
       title = "3. NWT LTER aquatic data availability: annual sampling frequency, 1981 - 2017",
       subtitle = "Colored by total number of dates sampled per year") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=12),
        legend.title = element_text(size=10))

nwt_aquatic %>%
  dplyr::select(dataset, site, ID, yr, date) %>%
  distinct() %>%
  mutate(dataset = gsub("phytoplankton", "phyto-\nplankton", dataset)) %>%
  group_by(dataset, site, ID, yr) %>%
  summarise(nobs = length(date)) %>%
  ggplot(aes(yr, ID, fill=nobs)) + 
  geom_tile(col="grey50") +
  scale_fill_distiller(name = "# days", palette= "Blues", direction = 1) +
  scale_x_continuous(expand = c(0,0), breaks=seq(1980,2016, 4)) +
  scale_y_discrete(expand=c(0,0)) +
  labs(x="Year", y="Sampling location (by elevation, highest to lowest)",
       title = "4. NWT LTER aquatic data availability: annual sampling frequency, 1981 - 2017",
       subtitle = "Colored by total number of dates sampled per year, split by dataset") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        #axis.text.y = element_text(size=10),
        plot.title = element_text(size=12),
        legend.title = element_text(size=10)) +
  facet_grid(.~dataset, scales="free_x", space = "free_x")
