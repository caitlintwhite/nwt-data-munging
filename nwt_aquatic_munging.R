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

# metadata from Nel's ice-phenology dataset
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
         dataset = "Phytoplankton") %>%
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
         dataset = "Water quality",
         yr = year(date)) %>%
  rename(site = local_site) %>%
  dplyr::select(site, location, yr, date, dataset) %>% distinct()

# a visual
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

# put all Caine water chemistry datasets together
Caine_waterchem <- rbind(Caine_GL4_waterchem, Caine_ARI_waterchem, 
                         Caine_SAD_waterchem, Caine_GL5_waterchem, 
                         Caine_MAR_waterchem, Caine_ALB_waterchem, Caine_NAV_waterchem) %>% 
  distinct() %>% 
  mutate(location = ifelse(month(date) %in% c(1:5, 11, 12) & grepl("GREEN LAKE",samp_loc)==TRUE, "Lake", "Outlet"),
         yr = year(date),
         dataset = "Water chemistry") %>% 
  rename(site = samp_loc) %>%
  dplyr::select(site, location, yr, date, dataset)

McK_GLV_waterchem <- McKnight_GLV_waterchem %>%
  mutate(local_site = gsub("GL", "GREEN LAKE ", local_site),
         local_site = gsub("ALB", "ALBION", local_site)) %>%
  rename(site = local_site) %>%
  filter(`depth/loc` %in% c("Inlet", "Outlet", "Surface", "0m", "0.5m", "3m", "9m")) %>%
  mutate(`depth/loc` = ifelse(`depth/loc` %in% c("Inlet", "Outlet"), `depth/loc`, "Lake"),
         yr = year(date),
         dataset = "Water chemistry") %>%
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
  mutate(dataset = gsub("Phytoplankton", "Phyto-\nplankton", dataset)) %>%
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

# ----------------
#' ## Temporal trends 
#' 
#' Some examples from Green Lake 4 ...
#' 

#+ read in and transform full GL4 water chemistry datasets (just at "core" depths of surface, 3m and 9m), message = FALSE, warning = FALSE, echo = FALSE
# same code as in GL4_WQ_waterchem_munging.R

# Nel Caine GL4 water chemistry dataset, 1982 through 2014
Caine_GL4_waterchem <- read_csv("http://niwot.colorado.edu/data_csvs/gre4solu.nc.data.csv",
                                trim_ws = TRUE,
                                na = c("NaN", "DNS",  "EQCL", "N/A", "NP", "NSS", "NV", "u", "QNS", NA, " ", ""))

# Diane McKnight water chemistry dataset, 1998 through 2016
# issues: year is wrong (has 1905 for 2014 and 2015 dates), TDP, IP and PO4 have "<" with some numeric values, forcing numeric columns as character in data import
# also "<" creates an ambiguous value 
McKnight_GLV_waterchem <- read_csv("http://niwot.colorado.edu/data_csvs/glvwatsolu.dm.data.csv",
                                   trim_ws = TRUE,
                                   na = c("NaN", "DNS",  "EQCL", "N/A", "NP", "NSS", "NV", "u", "QNS", NA, " ", ""))

# Caine waterchem dataset --
# remove isotopic standard deviation variables, gather all other variables to make long form
Caine_GL4 <- dplyr::select(Caine_GL4_waterchem, -contains("sdev")) %>%
  subset(samp_loc == "GREEN LAKE 4") %>%
  gather(metric, value, pH:POC) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value))

# break out isotopic standard deviation variables, gather to make long form
Caine_sdev <- dplyr::select(Caine_GL4_waterchem, samp_loc:time, contains("sdev")) %>%
  subset(samp_loc == "GREEN LAKE 4") %>%
  gather(sdev, sd_value, d18O_sdev:T_sdev) %>%
  mutate(metric = gsub("_sdev", "", sdev)) %>%
  filter(!is.na(sd_value))
# rename st deviation variables to match how they're named in main dataset
Caine_sdev$metric <- gsub("dD", "dDeut", Caine_sdev$metric)
Caine_sdev$metric <- gsub("T", "Trit", Caine_sdev$metric)

# join main dataset and standard deviation variables into one data frame, make long form
Caine_long <- left_join(Caine_GL4, Caine_sdev) %>%
  mutate(doy = yday(date),
         # metatadata says June-Oct (summer conditions) samples taken from outlet, Nov-May under ice from lake
         location = ifelse(month(date) %in% 6:10, "Outlet", "Lake"),
         depth = ifelse(month(date) %in% 6:10, NA, 0),
         source = "Caine") %>%
  dplyr::select(-samp_loc)
# remove intermediate datasets, not needed anymore
rm(Caine_GL4, Caine_sdev)


# clean up McKnight dataset
# fix year
McKnight_GLV_waterchem$year <- year(McKnight_GLV_waterchem$date)
# assign NA to any "<" value, convert field to numeric
McKnight_GLV_waterchem$TDP <- as.numeric(with(McKnight_GLV_waterchem, ifelse(grepl("<", TDP), NA, TDP))) # if true has "<"
McKnight_GLV_waterchem$IP <- as.numeric(with(McKnight_GLV_waterchem, ifelse(grepl("<", IP), NA, IP)))
McKnight_GLV_waterchem$'PO4---' <- as.numeric(ifelse(grepl("<", McKnight_GLV_waterchem$`PO4---`), NA, McKnight_GLV_waterchem$'PO4---')) 

# break out main chemistry metric measured (exclude standard deviation)
McKnight_GL4 <- dplyr::select(McKnight_GLV_waterchem, -contains("sdev")) %>%
  subset(local_site == "GL4") %>%
  gather(metric, value, pH:POC) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value))

# break out isoptic standard deviation fields and make long-form
McKnight_sdev <- dplyr::select(McKnight_GLV_waterchem, LTER_site:`depth/loc`, contains("sdev")) %>%
  subset(local_site == "GL4") %>%
  gather(sdev, sd_value, d18O_sdev:T_sdev) %>%
  mutate(metric = gsub("_sdev", "", sdev)) %>%
  filter(!is.na(sd_value))
McKnight_sdev$metric <- gsub("dD", "dDeut", McKnight_sdev$metric)
McKnight_sdev$metric <- gsub("T", "Trit", McKnight_sdev$metric)

## core dataset, selecting only for surface, 3m, 6m and 9m lake depths, inlet and outlet ("core" set per KL)
McKnight_long <- left_join(McKnight_GL4, McKnight_sdev) %>%
  mutate(doy = yday(date)) %>%
  dplyr::select(-LTER_site, -local_site, -comments) %>%
  # keep only these sample depths/locations for now
  filter(`depth/loc` %in% c("0m", "3m", "6m", "9m", "Surface", "Inlet", "Outlet")) %>%
  # split depth from location, and make depth numeric
  mutate(`depth/loc` = gsub("Surface", "0m", `depth/loc`),
         location = ifelse(grepl("m",`depth/loc`), "Lake",
                           ifelse(`depth/loc` == "Inlet", "Inlet", "Outlet")),
         depth = ifelse(location == "Lake", parse_number(`depth/loc`), NA),
         source = "McKnight") %>%
  dplyr::select(-`depth/loc`)

# join both McKnight and Caine Green Lake 4 water chemistry datasets
# this contains core data from McKnight water chemistry dataset: Lake (0, 3, 6 and 9m) Inlet and Outlet
GL4_waterchem <- rbind(Caine_long, McKnight_long)

# --------------------
#+ GL4 temporal example, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 8
# load library for panel plotting
library(cowplot)

# discharge figure, panel across year
summer_GL4_flow <- filter(flow_GL4, month(date) %in% 6:9 & year(date) >1984) %>%
  mutate(location = "Outlet",
         doy = yday(date)) %>%
  ggplot(aes(doy, discharge)) +
  geom_path(aes(group=year(date)), col="cadetblue") +
  #geom_point(col="steelblue", alpha=0.2, pch=1) +
  #scale_x_date(limits = c(as.Date("2000-06-01"), NA)) +
  scale_y_continuous(breaks = seq(0,88500, 20000), expand = c(0,1)) +
  scale_x_continuous(breaks=seq(150,275, 30)) +
  labs(x = "Year", 
       #y = "Daily Q (cubic meters)"
       y = expression(paste("Q (",m^3, day^-1,")"))
       ) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        panel.spacing.x = unit(0.1, "lines"),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0,0.25,1,0.25), "lines"),
        strip.text.x = element_text(angle = 25, size = 7),
        strip.text.y = element_text(face ="bold")) +
  facet_grid(location~year(date), switch = "x")

# each year its own line, with temporal mean line
alt_Q <- filter(flow_GL4, month(date) %in% 6:9 & year(date) >1999) %>%
  mutate(location = "Outlet",
         doy = yday(date)) %>%
  ggplot(aes(doy, discharge)) +
  geom_path(aes(group=year(date), col=year(date)), alpha=0.6) +
  #geom_point(aes(y=zoo::rollmean(discharge, 15, na.pad=TRUE))) +
  stat_summary(fun.y = "mean", geom="line") +
  #geom_smooth() +
  #scale_x_date(limits = c(as.Date("2000-06-01"), NA)) +
  scale_x_continuous(breaks=seq(150,275, 30), expand = c(0.02,0.02)) +
  scale_color_distiller(name="Year", palette="PuBuGn", direction = 1) +
  labs(x = "Day of year", y = expression(paste("Q (",m^3, day^-1,")"))) +
  theme_light() +
  theme(legend.position ="bottom") +
  guides(color = guide_colorbar(barwidth = 25)) +
  facet_grid(location~.)
  #facet_grid(location~year(date), switch = "x")

# nutrient figure (panel across years)
summer_GL4_NO3 <- filter(GL4_waterchem, month(date) %in% 6:9 & metric == "NO3-" & year(date) >1984) %>%
  group_by(date, metric, doy, location) %>%
  summarise(mean_value = mean(value, na.rm=TRUE)) %>%
  ggplot(aes(doy, mean_value, fill=location)) +
  geom_line(col="grey50") +
  geom_point(size=1, alpha=0.7, pch=21) +
  #scale_color_brewer(name="Location", palette ="Set2") +
  scale_fill_brewer(name="Location", palette ="Blues") +
  scale_x_continuous(breaks=seq(150,275, 30)) +
  #scale_x_date(limits = c(as.Date("2000-06-01"), NA)) +
  labs(x=NULL, y = expression(paste("Mean nitrate concentration (mEq ", L^-1, ")")),
       title = "5. Green Lake 4 temporal trends: summer daily NO3 and discharge, 1985 - 2016",
       subtitle = "Sample dates: 01 Jun - 30 Sep; arrayed by sampling location") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        #strip.text.x = element_blank(),
        panel.spacing.x = unit(0.1, "lines"),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1,0.25,0,0.25), "lines"),
        strip.text.x = element_text(angle = 25, size = 7),
        strip.text.y = element_text(face ="bold")) +
  facet_grid(location~year(date), scales = "free_y", switch="x")

# alternate nutrient figure (each year its own line)
alt_NO3 <- filter(GL4_waterchem, month(date) %in% 6:9 & metric == "NO3-" & year(date) >1999) %>%
  group_by(date, metric, doy, location) %>%
  summarise(mean_value = mean(value, na.rm=TRUE)) %>%
  ggplot() +
  #geom_point(alpha=0.5) +
  geom_path(aes(doy, mean_value, group=year(date), col=year(date)), lwd = 1, alpha=0.5) +
  #geom_point(aes(doy, y=zoo::rollmean(mean_value, 7, na.pad=TRUE))) +
  #stat_summary(fun.y="median", geom="point") +
  #geom_smooth(se=F) +
  scale_color_distiller(palette = "PuBuGn", direction = 1) + 
  scale_x_continuous(breaks=seq(150,275, 30), expand = c(0.02,0.02)) +
  labs(x=NULL, y = expression(paste("Mean nitrate concentration (mEq ", L^-1, ")")),
       title = "6. Alternate trends figure: summer daily NO3 and discharge, 2000 - 2016",
       subtitle = "01 Jun -30 Sep; lines colored by year; discharge temporal mean in black; arrayed by sampling location") +
  theme_light() +
  theme(legend.position = "none",
        axis.text.x = element_blank()) +
  facet_grid(location~.)

plot_grid(summer_GL4_NO3, summer_GL4_flow,
          nrow=2,
          align = "v")

plot_grid(alt_NO3, alt_Q,
          nrow=2,
          align = "v")

# ---------------
#+ more trends, echo=FALSE, warning = FALSE, message = FALSE, fig.height = 10, fig.width=8 

Q_summary <- filter(flow_GL4, month(date) %in% 6:9 & year(date) >1999) %>%
  mutate(location = "Outlet",
         mon = month(date),
         yr = year(date)) %>%
  ggplot(aes(as.factor(yr), discharge, group = mon*yr, fill = as.factor(mon))) +
  geom_boxplot() +
  scale_fill_brewer(name="Month", palette="Reds") +
  labs(x=NULL, 
       y = expression(paste("Q (",m^3, day^-1,")"))) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,.1,0), "lines")) +
  facet_grid(location~.)

Q_cv <- filter(flow_GL4, month(date) %in% 6:9 & year(date) >1999) %>%
  mutate(location = "Outlet",
         mon = month(date),
         yr = year(date)) %>%
  group_by(yr, location) %>%
  summarise(cv = mean(discharge, na.rm=TRUE)/sd(discharge, na.rm = TRUE)) %>%
  ggplot(aes(as.factor(yr), cv)) +
  geom_point() +
  labs(x="Year", y= "Q CV") +
  theme_light() +
  theme(plot.margin = unit(c(0,0,.5,0), "lines")) +
  facet_grid(location~.)


month_trends <- filter(GL4_waterchem, month(date) %in% 6:9 & year(date) >1999 &
         metric %in% c("pH", "Ca++", "ANC", "SO4--")) %>%
  mutate(mon = month(date),
         yr = year(date)) %>%
  ggplot(aes(as.factor(yr), value, group = mon*yr, col = as.factor(mon))) +
  geom_point(alpha=0.5) +
  facet_grid(metric~location, scales = "free_y")

GL4_summary <- filter(GL4_waterchem, month(date) %in% 6:9 & year(date) >1999 &
         metric %in% c("pH", "Ca++", "ANC", "SO4--")) %>%
  mutate(mon = month(date),
         yr = year(date)) %>%
  filter(!is.na(value)) %>%
  group_by(yr, metric, location) %>%
  summarise(mean_value = mean(value, na.rm=TRUE),
            se = sd(value)/sqrt(length(value))) %>%
  ggplot(aes(as.factor(yr), mean_value)) +
  #geom_line(aes(group=mon)) +
  geom_errorbar(aes(ymax=mean_value +se, ymin = mean_value-se, group=location), width=0.1, position = position_dodge(width =0.25)) +
  geom_point(aes(fill=location), position = position_dodge(width =0.25), pch=21) +
  #geom_smooth(method="lm", col="black") +
  scale_fill_brewer(name="Location", palette ="Blues") +
  #scale_x_continuous(breaks=seq(2000,2016,4)) +
  labs(x=NULL, y="Seasonal mean",
       title ="7. GL4 trends: ANC, pH, Ca+ and SO4, discharge, and CV of discharge, 2000-2016",
       subtitle = "01 Jun - 30 Sep; water chemistry colored by sampling location, outlet disharge colored by month") +
  theme_light() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        plot.margin = unit(c(1,0,.1,0), "lines")) +
  facet_grid(metric~., scales = "free_y")
  
plot_grid(GL4_summary, Q_summary, Q_cv,
          nrow=3,
          align = "v",
          rel_heights = c(1,.7,.3))
 

# ----------------
#+ interactive discharge plot, eval = FALSE, echo=FALSE, message = FALSE, warning = FALSE, include = FALSE 

# interactive html figure for time series stream discharge (not included in github markdown, doesn't support html renderings)
# made for NWT staff mtg on 20180402

# load package for interactive figure
library(dygraphs)
library(xts)

# make time-series dataset wide-format for plotting
flow_NWT_wide <- flow_NWT[c("date", "discharge", "site")] %>%
  mutate(discharge = as.numeric(discharge)) %>%
  spread(site, discharge) %>%
  # left_join(flow_ALB[c("date", "notes")]) %>%
  # rename(ALB_notes = notes) %>%
  # left_join(flow_MAR[c("date", "notes")]) %>%
  # rename(MAR_notes = notes) %>%
  # left_join(flow_GL4[c("date", "notes")]) %>%
  # rename(ML4_notes = notes) %>%
  as.data.frame()
flow_NWT_wide <- xts(flow_NWT_wide[,2:4], order.by = flow_NWT_wide$date)

  
# plot
dygraph(flow_NWT_wide, 
        main = "NWT LTER daily stream discharge, 1981-2016",
        ylab = "Q (cubic meters per day)") %>%
  # dySeries("mdeaths", label = "Male") %>%
  # dySeries("fdeaths", label = "Female") %>%
  dyOptions(fillGraph = TRUE) %>%
  dyRangeSelector()
        
        