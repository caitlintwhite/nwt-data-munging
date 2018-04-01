#' ---
#' title: Water quality and chemistry data munging for Green Lake 4
#' author: CTW
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'
#' ## Script purpose
#' NWT LTER has a decades-long record of data collected annually or nearly so for lakes and streams in the Green Lakes Valley.
#' A challenge in working with such a wealth of data is how to make sense of so much data: How to summarize? At what scale or frequency? What to summarize? What descriptive summary statistics to calculate (e.g. mean, max, min, metrics of variance)? and What should be related to what?
#' This script is a first attempt in wrangling one NWT LTER's long term "core" datasets (actually two related datasets): water quality and water chemistry from Green Lakes Valley.
#' A long-term data goal for NWT LTER is to make its data more accessible and intelligible to researchers and the public, to promote visibility of NWT LTER itself, the long-history of research conducted there and the core datasets in particular, as well as spur novel uses of the core datasets.
#' A strategy for achieving this goal is to automate simple dataset clean up, preparation (transformation) needed for summarizing, and data visualization.
#' 
#' As such, the purposes of this script are to:
#' 
#' * Read in relevant datasets directly from the NWT LTER website (these are more up to date than what's on the EDI data portal):
#'     + **Green Lake 4 water chemistry** (PI: Nel Caine)
#'     + **Green Lake Valley water chemistry** (PI: Diane McKnight)
#'     + **Green Lake Valley water quality** (PI: Diane McKnight)
#'     + **Lake ice clearance and formation** (PI: Nel Caine)
#' * Visually assess each dataset for metrics, temporal and spatial range, and sampling frequency within those ranges
#' * Asses which metrics and sites overlap between datasets
#' * Focus on Green Lake 4 and combine water chemistry and water quality datasets to create master long-term aquatic ecology dataset for GL4
#' * Test data summarizing and visualization (this will be a work in progress as CTW receives feedback)  
#'
#' I focus on Green Lake 4 because that is a site with greatest consistency of sampling through time (per Kelly Loria, and it shows in the data).
#' With some exposure to NWT LTER from past field work and data tasks, and academic training and work experience in freshwater ecology, my experience going through these data and challenges incurred should be fairly representative of an outside researcher or someone with ecological knowledge interested in aquatic data (e.g. environmental science for the City of Boulder).
#'
#' For comments or collaborations, email caitlin.t.white@colorado.edu, login to GitHub to add issues or items to the project board for this repository, or fork the repository to edit code and submit pull requests.    
#'
#' ## Setup

# -----------------
#+ setup, message=FALSE, warning = FALSE
# Load packages needed for reading in, transforming, and visualizing the data
library(tidyverse) # multiple handy packages bundled in one (e.g. ggplot, readr, tidyr, dplyr)
library(lubridate) # for dealing with time objects

# -----------------
# Read in water quality and water chemistry datasets on GL4, and ice phenology data

# --NA codes from online metadata--
# u=Undetected
# DNS=Data Not Submitted
# EQCL=Exceeds Quality Control Limits
# N/A=Not Applicable
# NP=Not Performed
# NSS=No Sample Submitted
# NV=Not Valid
# QNS=Quantity Not Sufficient
# NA=Not available

#+ read in data, message=FALSE, warning = FALSE
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

# Diane McKnight water quality dataset, 2000 through 2017
McKnight_GLV_WQdat <- read_csv("http://niwot.colorado.edu/data_csvs/water_quality_GLV.dm.data.csv", 
                      trim_ws = TRUE,
                      na = c("NaN", NA, "NA ", " ", ""))

## subset GL4 data
McKnight_GL4_WQdat <- McKnight_GLV_WQdat[McKnight_GLV_WQdat$local_site=="GL4",]

# read in GLV lake ice on and ice off dates, 1981 - 2015
GLV_icephenology <- read_csv("http://niwot.colorado.edu/data_csvs/glakeice.nc.data.csv",
                             trim_ws = TRUE,
                             na = c("NaN", NA, "NA ", " ", ""))

# -------------------
#' ## Clean up and prep data
#' 
#' Here I transform datasets to long-form "tidy" data, which is better for data visualization in ggplot.
#' Code is not shown unless I make a decision about data transformation (see R script for code).
#' Something to note that I consistently do is break out the "depth/loc" field into two fields, "depth" and "location", so there aren't a mix of data types in one field (e.g. character and numeric).
#' As demonstration, here is the structure of the raw data for the water quality dataset and unique values for the "depth/loc" field: 

# fields in raw water quality dataset
names(McKnight_GLV_WQdat)
# structure of raw data
head(McKnight_GLV_WQdat)
# unique values of depth-location
unique(McKnight_GLV_WQdat$`depth/loc`)

#' There are `r length(unique(McKnight_GLV_WQdat$"depth/loc"))` different possible values for depth-location, some of which are actual lake depths and some not.
#' For an outside researcher potentially interested in using the water quality dataset, this is confusing. For example, the online metadata describe the sampling location as the middle of the lake (http://niwot.colorado.edu/meta_data/water_quality_GLV.dm.meta.txt).
#' What then is the difference between a "depth/loc" value of "1m" and "Middle lake at 1m"? What depths are "Inlet", "Outlet", and "Air"?
#' Was the "Air" sample taken in the middle of the lake? Inlet? Outlet? These are some questions I can imagine a person who'd like use GLV water quality data having, 
#' and for that reason it's cleaner to separate depth from location, and is something that should be considered for how data are recorded and entered.
#' While there are `r length(unique(McKnight_GLV_WQdat$"depth/loc"))` unique "depth/loc" values in the water quality dataset, there are **`r length(unique(McKnight_GLV_waterchem$"depth/loc"))`** unique values for "depth/loc" in the McKnight water chemistry dataset,
#' even though that dataset only includes 1 additional site (Green Lake 3) compared to the McKnight water quality dataset: 

# sites sampled in the water quality dataset
unique(McKnight_GLV_WQdat$local_site)
# sites sampled in the water chemistry dataset
unique(McKnight_GLV_waterchem$local_site)

#' Kelly Loria has suggested breaking out the "core" water quality and chemistry data (data collected consistently through time) from the McKnight (ongoing)
#' water quality and water chemistry datasets, and keeping infrequently-sampled water quality and water chemistry data in an auxiliary or experimental dataset. 
#' For example, some of the "depth/loc" values in the water chemistry dataset are from single-season REU projects (email communication from DMK to KL). 
#' In my experience trying to summarize these datasets, and figure out which values are appropriate for summarizing, having a true "core" dataset separate from an auxiliary or experimental dataset makes sense to me. 

#+ tidy and make ice phenology dataset long, echo = FALSE, warning = FALSE, message = FALSE
# Ice phenology dataset -- 
# break out day of year columns and gather
GLV_ice_doy <- GLV_icephenology[c("year", "lake", "break_jday", "clear_jday", "form_jday")] %>%
  gather(event, event_doy, break_jday:form_jday) %>%
  mutate(event = gsub("_jday", "", event))

# break out date columns and gather
GLV_ice_date <- GLV_icephenology[c("year", "lake", "break_date", "clear_date", "form_date")] %>%
  gather(event, event_date, break_date:form_date) %>%
  mutate(event = gsub("_date", "", event))

# join day of year and date columns back in one dataset
GLV_ice_long <- left_join(GLV_ice_doy, GLV_ice_date) %>%
  filter(!is.na(event_doy))
# remove intermediate data frames from environment, no longer needed
rm(GLV_ice_doy, GLV_ice_date)

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

#' **QA Notes**
#' Working through the McKnight water chemistry dataset, there are a few other issues to mention in the raw data. I show code to illustrate:
#' 
#' * 1905 erroneously entered for the year 2015 in the "year" column 

#+ McKnight water chemistry dataset, echo=TRUE  
# McKnight water chemistry dataset --
# show problem with year (1905 entered instead of 2015)
## all unique "year" values in dataset
unique(McKnight_GLV_waterchem$year)
## all unique "year" values when filtering the dataset for dates from the year 2015
unique(McKnight_GLV_waterchem$year[year(McKnight_GLV_waterchem$date)==2015])
# fix year
McKnight_GLV_waterchem$year <- year(McKnight_GLV_waterchem$date)

#' + Three numeric fields (TDP, IP, and PO4) have "<" entered, coercing the entire field in R as a character instead of numeric when it's read in. 
#' + "<" is also an ambiguous value. I know it doesn't exceed a certain level, but I can't say whether "<5" is 4 or 1 which could make a difference for averaging or determining minimum values. 
#'      + Since I'm not sure of the true value, I exclude anything with "<" for summarizing.
#' 

# remove any values with "<" since not sure of value in context of other values
## TDP ##
# how many values have "<"?
summary(with(McKnight_GLV_waterchem, grepl("<", TDP)))
# assign NA to any "<" value, convert field to numeric
McKnight_GLV_waterchem$TDP <- as.numeric(with(McKnight_GLV_waterchem, ifelse(grepl("<", TDP), NA, TDP))) # if true has "<"

## IP ##
# how many values have "<"?
summary(with(McKnight_GLV_waterchem, grepl("<", IP))) # if true has "<"
# assign NA to any "<" value, convert field to numeric
McKnight_GLV_waterchem$IP <- as.numeric(with(McKnight_GLV_waterchem, ifelse(grepl("<", IP), NA, IP)))

## PO4 ## 
# how many values have "<"?
summary(grepl("<", McKnight_GLV_waterchem$'PO4---')) # if true has "<"
# assign NA to any "<" value, convert field to numeric
McKnight_GLV_waterchem$'PO4---' <- as.numeric(ifelse(grepl("<", McKnight_GLV_waterchem$`PO4---`), NA, McKnight_GLV_waterchem$'PO4---')) 

#' Exluding these "<" values also seems reasonable because they represent a small fraction (2% or less) of all observations within each field.
#' 

#+ resume McKnight waterchem data prep with code hidden, echo = FALSE, warning = FALSE, message = FALSE
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

# combine main and std deviation datasets into long form
# create long dataset, preserve all depths and locations to show full data availability
McKnight_long_alldepths <- left_join(McKnight_GL4, McKnight_sdev) %>%
  mutate(doy = yday(date)) %>%
  dplyr::select(-LTER_site, -local_site, -comments) %>%
  # keep only these sample depths/locations for now
  filter(grepl("m|Surface|Inlet|Outlet|waterfall", `depth/loc`)) %>%
  # split depth from location, and make depth numeric
  mutate(`depth/loc` = gsub("Surface", "0m", `depth/loc`),
         location = ifelse(grepl("m",`depth/loc`), "Lake",
                           ifelse(`depth/loc` == "Inlet", "Inlet", 
                                  ifelse(`depth/loc` == "Outlet", "Outlet", "Waterfall"))),
         depth = ifelse(location == "Lake", parse_number(`depth/loc`), NA),
         source = "McKnight") %>%
  dplyr::select(-`depth/loc`)

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


# ----- 
#' ## Visualize data availability
#' The following figures show data availability, temporal and depth ranges, and sampling frequency within for the tidied long-form datasets.
#' The reason I include lake ice phenology data is to give some context to when summer season lake data was collected. 
#' The metata data for lake water quality and water chemistry (McKnight) states samples were collected approximately 1 week after ice-off, so I wanted to check against Nel Caine's ice phenology data.
#' 
#' Things I'd like feedback on include:
#' 
#' + For presenting data availability, which figures are most compelling?
#' + Given data availability (e.g. inconsistencies in sampling frequency by site and depth), how to best summarize data for a usable long-term summary dataset of lake trends
#' 
#'  Showing the ice phenology data availability via raw data values is easy because it's a relatively simple dataset (3 variables x site x time).
#'  Showing data availability via actual data values with either water quality or chemistry data is trickier because there are more data (more variables x different depths x location (lake, inlet, outlet) x site x time).  
#'
#' **All lakes ice phenology**

#+ plot data availability, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6

# ice phenology dataset--
GLV_ice_long %>%
  mutate(lake = factor(lake, levels = c("Silver", "Albion", "Green1",
                                           "Green2", "Green3", "Green4",
                                           "Green5", "Arikaree"))) %>%
  ggplot(aes(event_doy, year, col=event)) + 
  geom_path(aes(group=year)) + 
  geom_point(alpha=0.6) + 
  labs(y= "Year", x="Event day of year", 
       title = "1. Green Lakes Valley lake ice break, clearance, and formation dates, 1981-2015",
       subtitle = "Sites ordered lowest elevation (Silver Lake, 3141m) to highest (Arikaree Glacier, 3785m)") +
  scale_color_brewer(name = "Event", palette = "Paired") +
  theme_minimal() +
  facet_wrap(~lake)

#' **Green Lake 4 water chemistry data availability**

#+ data availablity for water chem, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 8
# Caine GL4 water chemistry data--
Caine_long %>%
  #filter(location != "Lake") %>%
  group_by(year, location, metric) %>%
  summarise(nobs = length(metric)) %>%
  ggplot() +
  geom_col(aes(year, nobs, fill=location), col="gray50", position = "identity", alpha=0.3) +
  labs(x = "Year", y = "Number of observations",
       title = paste("2. Sampling frequency per year: Green Lake 4 water chemistry (PI: Nel Caine),",
                     min(Caine_long$year), "-", max(Caine_long$year)),
       subtitle = "Samples collected below ice from lakes approx Nov - May; from lake outlet Jun - Oct, during 'summer' conditions") +
  scale_fill_brewer(name = "Sampling\nlocation", palette = "Set2") +
  scale_x_continuous(breaks=seq(1980, 2015, 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~metric)

# show McKnight and Caine inlet and outlet water chem sample combined frequency
# inlet and outlet only
GL4_waterchem %>%
  filter(location != "Lake") %>% # excludes summer McKnight lake samples and winter lake Caine samples
  group_by(source, year, location, depth, metric) %>%
  summarise(nobs = length(metric)) %>%
  mutate(grouping = factor(paste(source, location, sep="_"), levels = c("Caine_Outlet", "McKnight_Outlet", "McKnight_Inlet"))) %>%
  ggplot() +
  geom_area(aes(year, nobs, fill=grouping), col = "gray50", position = "identity", alpha=0.4) +
  #geom_line(aes(year, nobs, col=grouping), alpha=0.7) +
  #geom_point(aes(year, nobs, col=grouping), alpha=0.7) +
  labs(x = "Year", y = "Number of observations",
       title = paste("3. Annual sampling frequency: Green Lake 4 water chemistry", min(GL4_waterchem$year), "-", max(GL4_waterchem$year))) +
       #subtitle = "Bars stacked to show total number of observations per year, segments colored by data source") +
  scale_fill_brewer(name = "Data source", palette="Paired") +
  scale_color_brewer(name = "Data source", palette="Paired") +
  scale_x_continuous(breaks=seq(1980, 2015, 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~metric)

# McKnight GL4 water chemistry data--
# what is sampling frequency by depth over time?
# lake only
McKnight_long_alldepths %>% # all lake depths (when it could be determined)
  filter(month(date) %in% 6:9, # summer months only
         location == "Lake") %>% # excludes inlet and outlet samples
  group_by(year, location, depth, metric) %>%
  summarise(nobs = length(metric)) %>%
  ggplot() +
  geom_vline(aes(xintercept=0), col="dodgerblue2", lwd=1) +
  geom_point(aes(depth, year, group=depth,  col=nobs, size = nobs/4), alpha=0.4) +
  labs(y = "Year", x = "Lake depth (m)", 
       title = paste("4. Annual sampling frequency: Green Lake 4 water chemistry (PI: McKnight),", min(McKnight_long_alldepths$year), "-", max(McKnight_long_alldepths$year)),
       subtitle = "Points colored and sized by number of observations") +
  scale_color_distiller(name = "# obs", palette = "Set2", breaks=seq(0,12,3)) +
  scale_size_continuous(guide="none") +
  scale_x_reverse(expand = c(0.1,0), breaks=seq(0, 12, 3)) +
  #scale_y_continuous(breaks=seq(1980, 2015, 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
  facet_wrap(~metric)

#' This figure shows just the "core" depths (0, 3 and 9). Symobology and coloring adjusted per CR's suggestion (compare with symbology in Fig. 4).
#' 

#+ McKnight lake chemistry core, echo=FALSE, fig.width = 8, fig.height = 6

# plot lake chemistry data availability Jun - Sep
# what is sampling frequency by depth over time?
GL4_waterchem %>% # core data: lake depths only at 0, 3 and 9m
  filter(month(date) %in% 6:9, # effectively makes it McKnight data only
         location == "Lake") %>%
  group_by(year, location, depth, metric) %>%
  summarise(nobs = length(metric)) %>%
  ggplot() +
  #geom_vline(aes(xintercept=0), col="dodgerblue2", lwd=1) +
  # "size=nobs/2" include in aesthetics for geom_point if want to size by observations
  geom_point(aes(depth, year, group=depth, fill=nobs), size=2, pch = 21, col = "gray50", alpha=0.5) +
  labs(y = "Year", x = "Lake depth (m)", 
       title = paste("5. Annual sampling frequency: Green Lake 4 water chemistry (PI: McKnight),", min(McKnight_long_alldepths$year), "-", max(McKnight_long_alldepths$year)),
       subtitle = "Core data only, points colored by number of observations") +
  scale_fill_distiller(palette = "PuBu", direction = 1) +
  #scale_size_continuous(guide = "none") +
  scale_x_reverse(breaks=seq(0, 9, 3)) +
  #scale_y_continuous(breaks=seq(1980, 2015, 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
  facet_wrap(~metric)

# -------------------
#' **Green Lake 4 water quality data availability**
#' 
#' 
#' **QA note**: There really are that many samples for outlet chlorophyll-a in 2007. 
#' Chris Ray suggests remaking this as broken plot (see example here: https://joergsteinkamp.wordpress.com/2016/01/22/broken-axis-with-ggplot2/).
#' If do this, will need to add code to automate detecting outliers and breaking y-axis as necessary.

#+ water quality data availability, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 4 
GL4_WQ_long <- McKnight_GL4_WQdat %>%
  dplyr::select(-comments) %>%
  gather(metric, value, chl_a:DOC) %>%
  filter(!is.na(value)) %>%
  mutate(doy = yday(date),
         yr = year(date),
         depth = ifelse(`depth/loc`== "Surface", 0, 
                        ifelse(grepl("m",`depth/loc`)==FALSE, NA, parse_number(`depth/loc`))),
         location = ifelse(`depth/loc`== "Inlet", "Inlet",
                           ifelse(`depth/loc`== "Outlet", "Outlet", "Lake")))

GL4_WQ_long$location[is.na(GL4_WQ_long$location)] <- "Lake" # fix NA value, corresponds to secchi depth
GL4_WQ_long$depth[is.na(GL4_WQ_long$depth)] <- -1 # assign depth of -1 for anything measured in air or not in lake


# inlet and outlet sampling frequency
GL4_WQ_long %>%
  filter(location != "Lake") %>%
  group_by(yr, location, depth, metric) %>%
  summarise(nobs = length(metric)) %>%
  ggplot() +
  geom_col(aes(yr, nobs, fill=location), width=0.7) +
  labs(x = "Year", y = "Number of observations", 
       title = paste("6. Annual sampling frequency: Green Lake 4 water quality data (PI: McKnight),", min(GL4_WQ_long$yr), "-", max(GL4_WQ_long$yr))) +
  scale_fill_manual(guide = "none", values = c("#B2DF8A","#1F78B4")) +
  scale_x_continuous(breaks = seq(2004, 2016, 4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(location~metric, scales = "free_y")

#' **QA note**: In the raw data, secchi depths values are filled down for all depths (including "air"). In practice, there is only one secchi value per day/time for the entire water column.

#+ lake water quality data availability, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6 
# what is sampling frequency by depth over time?
# lake only
GL4_WQ_long %>%
  filter(location == "Lake") %>%
  group_by(yr, location, depth, metric) %>%
  summarise(nobs = length(metric)) %>%
  ggplot() +
  #geom_vline(aes(xintercept=0), col="dodgerblue2", lwd=1) +
  # "size=nobs/2" add to aesthetics if want to size by nobs 
  geom_point(aes(depth, yr, group=depth, fill=nobs, size = nobs/4), pch = 21, col="gray50", alpha=0.4) +
  labs(y = "Year", x = "Lake depth (m)", 
       title = paste("7. Annual sampling frequency: Green Lake 4 water quality (PI: McKnight),", min(GL4_WQ_long$yr), "-", max(GL4_WQ_long$yr)),
       subtitle = "Points colored and sized by number of observations; secchi and light attenuation values recorded at 'air' ") +
  scale_fill_distiller(name = "# obs", palette = "PuBu", breaks=seq(2,12,2), direction = 1) +
  scale_size_continuous(guide = "none") +
  scale_x_reverse(breaks=seq(0,12,3)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
  facet_grid(.~metric)


# -------------------
#' **Alternative sampling frequency figures**
#' 
#' An alternative to showing sampling frequency as bar charts is to show it as point data by date by year.
#' Instead of emphasizing the total number of observations per year, the focus is on when samples were taken and an interranual comparison of those dates. 
#' These figures capture the sampling range and specific dates sampled, but don't reflect frequency by depth.

#+ waterchem date frequency, echo = FALSE, fig.height = 8, fig.width = 8
## Date frequency
GL4_waterchem %>%
  dplyr::select(year, date, doy, location, source) %>%
  distinct() %>%
  #mutate(term = ifelse(month(date) %in% 6:10, "Summer", "Winter")) %>%
  group_by(year, location, source) %>%
  mutate(nobs = length(doy)) %>%
  #filter(source == "McKnight") %>%
  ggplot(aes(doy, year)) +
  geom_path(aes(col=nobs, group=year)) +
  geom_point(aes(fill=nobs), col="gray50", pch= 21, alpha=0.8) +
  labs(y="Year", x="Day of year", 
       title = "8. Annual sampling frequency: Green Lake 4 water chemistry, by data source and location",
       subtitle = "Points on date collected, line shows temporal range of data per year") +
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"),
                     breaks = yday(c("2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01", 
                                     "2018-05-01", "2018-06-01", "2018-07-01", "2018-08-01", 
                                     "2018-09-01", "2018-10-01", "2018-11-01", "2018-12-01"))) +
  scale_y_continuous(breaks = seq(1980,2016,4)) +
  scale_color_distiller(name = "# obs/yr", palette= "PuBu", breaks=seq(0,25,5), direction = 1) +
  scale_fill_distiller(name = "# obs/yr", palette= "PuBu", breaks=seq(0,25,5), direction = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=0.75, hjust=0.85)) +
  facet_grid(location~source)

#+ water quality date frequency, echo = FALSE, fig.height = 6, fig.width = 8
GL4_WQ_long %>%
  dplyr::select(yr, date, doy, location) %>%
  distinct() %>%
  #mutate(term = ifelse(month(date) %in% 6:10, "Summer", "Winter")) %>%
  group_by(yr, location) %>%
  mutate(nobs = length(doy)) %>%
  #filter(source == "McKnight") %>%
  ggplot(aes(doy, yr)) +
  geom_point(data= subset(GLV_ice_long, year > 1999 & lake == "Green4" & event == "clear"), aes(event_doy, year), col="black", pch = 5, alpha= 0.75) +
  geom_point(data= subset(GLV_ice_long, year > 1999 & lake == "Green4" & event == "break"), aes(event_doy, year), col="black", pch = 4, alpha = 0.75) +
  geom_path(aes(col=nobs, group=yr)) +
  geom_point(aes(col=nobs), alpha=0.8) +
  labs(y="Year", x="Day of year", 
       title = "9. Annual sampling frequency: Green Lake 4 water quality (PI: McKnight)",
       subtitle = "GL4 ice phenology: ice break (x) and ice clearance (diamond)") +
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"),
                     breaks = yday(c("2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01",
                                     "2018-05-01", "2018-06-01", "2018-07-01", "2018-08-01",
                                     "2018-09-01", "2018-10-01", "2018-11-01", "2018-12-01"))) +
  scale_y_continuous(breaks = seq(1980,2016,4)) +
  scale_color_distiller(name = "# obs", palette= "PuBu", breaks=seq(0,15, 3), direction = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=0.75, hjust=0.85)) +
  facet_grid(location~.)

# alterative heat map
GL4_waterchem %>%
  dplyr::select(year, date, location) %>%
  unique() %>%
  group_by(year, location) %>%
  summarise(nobs = length(date)) %>%
  ggplot(aes(year, location, fill=nobs)) +
  geom_tile(col="grey50") +
  scale_x_continuous(expand = c(0,0), breaks=seq(1980, 2017, 5)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_distiller(palette= "OrRd", direction = 1) +
  theme_classic()

#' **QA note**: Plotting ice off with summer lake sampling date (above), I noticed a slight trend in delay of lake sampling over time. A delayed ice off trend wasn't what I remembered from the NWT renewal/the Preston et al. 2016 paper, and so I plotted first sample, ice break and ice off by year. 
#' Since lake sampling started in 1998, there is a significant trend in lake first sampling date with time (ice break and ice off no). 
#' The metadata note lake sampling each year begins roughly 1 week after ice off. Any consequence of missing this window is worth considering when summarizing data and comparing vaues interannually.    

#+ first date QA check, echo = FALSE
# plot first date sampled in lakes each year, just to see if trend overtime in timing
GL4_WQ_long %>%
  dplyr::select(yr, date, doy, location) %>%
  filter(location == "Lake") %>%
  distinct() %>%
  #mutate(term = ifelse(month(date) %in% 6:10, "Summer", "Winter")) %>%
  group_by(yr, location) %>%
  filter(date == min(date)) %>%
  ggplot(aes(yr,doy)) +
  #geom_path(aes(col=nobs, group=yr)) +
  geom_point(data= subset(GLV_ice_long, year > 1999 & event == "clear" & lake == "Green4"), aes(year, event_doy), col="dodgerblue", alpha=0.8) +
  geom_point(data= subset(GLV_ice_long, year > 1999 & event == "break" & lake == "Green4"), aes(year, event_doy), col="lightblue", alpha=0.8) +
  geom_point() +
  geom_smooth(method = "lm", col="grey50") +
  #geom_smooth(data= subset(GLV_ice_long, year > 1999 & event == "clear" & lake == "Green4"), aes(year, event_doy), col="dodgerblue", method="lm") +
  labs(x="Year", y="Day of year", 
       title = "10. QA check: first date water quality sampled at GL4 by year",
       subtitle ="Ice phenology shown in blue: ice break (light blue), ice clearance (dark blue)") +
  scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"),
                     breaks = yday(c("2018-05-01", "2018-05-15", "2018-06-01", "2018-06-15", 
                                     "2018-07-01", "2018-07-15", "2018-08-01"))) +
  scale_x_continuous(breaks = seq(1980,2016,4)) +
  #scale_color_distiller(name = "# obs/yr", palette= "Set2", breaks=seq(0,15, 3)) +
  theme_minimal()
  #theme(axis.text.x = element_text(angle=45)) +
  #facet_grid(location~.)



# --------------
#' ## Data comparison for combining water chemistry datasets
#' Because of overlap in analytes, sites and location within sites, it would be great to merge the Caine and McKnight water chemistry datasets for continuity in time. This prompts are few questions:
#' 
#'  + How do values compare?
#'  + Are these datasets appropriate to join?
#'  + Is it appropriate to average values across datasets in years where there is sampling overlap? 
#'
#' The outlet samples were collected by both groups in the summer months (Jun - Oct). The Caine lake samples are ones collected from just below the ice in winter months (Nov - May), whereas the McKnight lake samples were only collected in summer and at various depths (see above).
#' 

#+ plot actual data values, echo = FALSE, fig.width = 8, fig.height = 8 
# Compare outlet values by source
ggplot(subset(GL4_waterchem, location == "Outlet")) +
  geom_point(aes(date, value, col=source), pch= 1, alpha=0.5) +
  labs(x="Date", y ="Value", title = "11. GL4 water chemistry samples from outlet") +
  theme_light() +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(~metric, scales = "free_y")

# How many samples are there by month?
GL4_waterchem %>%
  mutate(mon = month(date, label = TRUE, abbr=TRUE)) %>%
  filter(location == "Outlet") %>%
  ggplot() +
  geom_boxplot(aes(mon, value, group=mon), col="gray50") +
  geom_point(aes(mon, value, col=source), alpha=0.2) +
  labs(x="Month", y = "Value", 
       title = "12. GL4 water chemistry values, by month, from outlet") +
  #scale_color_discrete(name = "Source", palette = "Set2")
  theme_minimal() +
  theme(axis.text.x = element_text(size=8, angle=90)) +
  facet_wrap(~metric, scales = "free_y")


ggplot(subset(GL4_waterchem, location == "Lake" & depth == 0)) +
  geom_point(aes(date, value, col=source), alpha=0.3) +
  labs(x="Date", y ="Value", 
       title = "13. GL4 water chemistry samples from lake surface") +
  theme_light() +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(~metric, scales = "free_y")

GL4_waterchem %>%
  mutate(mon = month(date, label=TRUE, abbr = TRUE)) %>%
  filter(location == "Lake") %>%
  ggplot() +
  geom_point(aes(mon, value, fill=depth), col="gray90", pch=21, alpha = 0.4) +
  scale_fill_distiller(name = "Depth (m)", palette= "PuBu", direction = -1, trans = "reverse") +
  labs(x="Month", y="Value", 
       title= "14. GL4 lake water chemistry by month, colored by lake depth where collected",
       subtitle = "Lake samples collected Nov-May by N. Caine, Jun-Sep by D. McKnight") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle=90)) +
  facet_wrap(~metric, scales = "free_y")  

GL4_waterchem %>%
  mutate(mon = month(date, label=TRUE, abbr = TRUE)) %>%
  filter(location == "Lake") %>%
  ggplot() +
  geom_boxplot(aes(mon, value, group=mon), col="gray50") +
  geom_point(aes(mon, value, col=year), alpha = 0.3) +
  scale_color_distiller(palette= "BrBG", direction = 1) +
  labs(x="Month", y="Value", 
       title= "15. Alternate version: GL4 lake water chemistry by month, colored by year") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle=90)) +
  facet_wrap(~metric, scales = "free_y")


#' ## Data summary
#' 
#' Below are some summary figures. See CTW for an interactive NO3 time series figure (HTML does not render well in GitHub).

#+ Summarize GL4 outlet data, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.heigh = 6

GL4_waterchem %>%
  mutate(mon = month(date, label=TRUE, abbr = TRUE)) %>%
  filter(location == "Outlet") %>%
  group_by(mon, metric, source) %>%
  summarise(mean_value = mean(value, na.rm=TRUE), 
            se = sd(value, na.rm=TRUE)/sqrt(length(value)),
            nobs = length(value)) %>%
  ggplot() +
  geom_errorbar(aes(mon, ymax = mean_value +se, ymin = mean_value-se, col=source), width = 0.25, alpha=0.7) +
  geom_point(aes(mon, mean_value, col=source), alpha=0.4) +
  ggtitle("16. GL4 outlet water chemistry mean values (+/- 1se) by month") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle=90)) +
  facet_wrap(~metric, scales = "free_y")



GL4_waterchem %>%
  mutate(mon = month(date, label=TRUE, abbr = TRUE)) %>%
  filter(location == "Lake") %>%
  group_by(mon, depth, metric) %>%
  summarise(mean_value = mean(value, na.rm=TRUE), 
            se = sd(value, na.rm=TRUE)/sqrt(length(value)),
            nobs = length(value)) %>%
  ggplot() +
  geom_errorbar(aes(mon, ymax=mean_value+se, ymin=mean_value-se, col=as.factor(depth)), width=0.25) +
  geom_point(aes(mon, mean_value, fill=as.factor(depth)), col="gray50", pch=21, alpha = 0.4) +
  scale_fill_brewer(name="Depth", palette= "PuBu", direction = 1) +
  scale_color_brewer(name= "Depth", palette= "PuBu", direction = 1) +
  labs(y = "Mean value", x = "Month", 
       title = paste("17. Green Lake 4 water chemistry mean values (+/- 1 se)", min(GL4_waterchem$year), "-", max(GL4_waterchem$year)))+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle=90)) +
  facet_wrap(~metric, scales = "free_y")


GL4_WQ_long %>%
  mutate(mon = month(date, label = TRUE, abbr = TRUE)) %>%
  filter(location !="Lake") %>%
  ggplot() +
  geom_boxplot(aes(mon, value, group= mon), col= "gray50", alpha=0.4) +
  geom_point(aes(mon, value, fill= yr), col= "gray50", pch=21, alpha=0.4) +
  scale_fill_distiller(palette = "BrBG", direction = 1) +
  ggtitle("18. GL4 water quality by month at inlet and outlet, colored by year") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle= 90)) +
  facet_wrap(location~metric, scales = "free_y", ncol=8)

GL4_WQ_long %>%
  mutate(mon = month(date, label = TRUE, abbr = TRUE)) %>%
  mutate(core_depth = ifelse(depth %in% 0:0.9, 0, depth)) %>%
  filter(location == "Lake" & core_depth %in% c(0, 3, 9)) %>%
  group_by(mon, metric, depth) %>%
  summarise(mean_value = mean(value, na.rm=TRUE), 
            se = sd(value, na.rm=TRUE)/sqrt(length(value)),
            nobs = length(value)) %>%
  ggplot() +
  geom_point(aes(mon, mean_value, fill=as.factor(depth)), col="gray50", pch=21, position = position_dodge(width=0.2), alpha=0.7) +
  geom_errorbar(aes(mon, ymax = mean_value +se, ymin = mean_value-se, col=as.factor(depth)), width = 0.25, alpha=0.7, position = position_dodge(width=0.2)) +
  scale_color_brewer(name= "Depth (m)", palette = "PuBu", direction = 1) +
  scale_fill_brewer(name= "Depth (m)", palette = "PuBu", direction = 1) +
  ggtitle("19. GL4 water quality mean values (+/- 1se), colored by lake depth") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle= 90)) +
  facet_wrap(~metric, scales = "free_y")

#+ NO3 only, test interactive ts plot with dygraph, echo = FALSE, eval =FALSE
library(dygraphs)
library(xts)

NO3 <- GL4_waterchem %>%
  subset(metric == "NO3-" & location == "Outlet") %>%
  dplyr::select(year, date, time, metric, value, source) %>%
  group_by(year, date, source) %>%
  summarise(value = mean(value)) %>%
  spread(source, value) %>%
  as.data.frame()

Caine_NO3 <- xts(NO3$Caine, order.by=NO3$date)
McKnight_NO3 <- xts(NO3$McKnight, order.by=NO3$date)
NO3_ts <- cbind(Caine_NO3, McKnight_NO3)
dygraph(NO3_ts, main = "NO3 concentration at GL4 outlet, 1981-2016") %>% 
  dyOptions(drawPoints = TRUE, fillGraph = TRUE) %>%
  dyRangeSelector()
