Water quality and chemistry data munging for Green Lake 4
================
CTW
2018-03-18

Script purpose
--------------

NWT LTER has a decades-long record of data collected annually or nearly so for lakes and streams in the Green Lakes Valley. A challenge in working with such a wealth of data is how to make sense of so much data: How to summarize? At what scale or frequency? What to summarize? What descriptive summary statistics to calculate (e.g. mean, max, min, metrics of variance)? and What should be related to what? This script is a first attempt in wrangling one NWT LTER's long term "core" datasets (actually two related datasets): water quality and water chemistry from Green Lakes Valley. A long-term data goal for NWT LTER is to make its data more accessible and intelligible to researchers and the public, to promote visibility of NWT LTER itself, the long-history of research conducted there and the core datasets in particular, as well as spur novel uses of the core datasets. A strategy for achieving this goal is to automate simple dataset clean up, preparation (transformation) needed for summarizing, and data visualization.

As such, the purposes of this script are to:

-   Read in relevant datasets directly from the NWT LTER website (these are more up to date than what's on the EDI data portal):
    -   **Green Lake 4 water chemistry** (PI: Nel Caine)
    -   **Green Lake Valley water chemistry** (PI: Diane McKnight)
    -   **Green Lake Valley water quality** (PI: Diane McKnight)
    -   **Lake ice clearance and formation** (PI: Nel Caine)
-   Visually assess each dataset for metrics, temporal and spatial range, and sampling frequency within those ranges
-   Asses which metrics and sites overlap between datasets
-   Focus on Green Lake 4 and combine water chemistry and water quality datasets to create master long-term aquatic ecology dataset for GL4
-   Test data summarizing and visualization (this will be a work in progress as CTW receives feedback)

I focus on Green Lake 4 because that is a site with greatest consistency of sampling through time (per Kelly Loria, and it shows in the data). With some exposure to NWT LTER from past field work and data tasks, and academic training and work experience in freshwater ecology, my experience going through these data and challenges incurred should be fairly representative of an outside researcher or someone with ecological knowledge interested in aquatic data (e.g. environmental science for the City of Boulder).

For comments or collaborations, email <caitlin.t.white@colorado.edu>, login to GitHub to add issues or items to the project board for this repository, or fork the repository to edit code and submit pull requests.

Setup
-----

``` r
# Load packages needed for reading in, transforming, and visualizing the data
library(tidyverse) # multiple handy packages bundled in one (e.g. ggplot, readr, tidyr, dplyr)
library(lubridate) # for dealing with time objects
```

``` r
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
```

``` r
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
```

Clean up and prep data
----------------------

Here I transform datasets to long-form "tidy" data, which is better for data visualization in ggplot. Code is not shown unless I make a decision about data transformation (see R script for code). Something to note that I consistently do is break out the "depth/loc" field into two fields, "depth" and "location", so there aren't a mix of data types in one field (e.g. character and numeric). As demonstration, here is the structure of the raw data for the water quality dataset and unique values for the "depth/loc" field:

``` r
# fields in raw water quality dataset
names(McKnight_GLV_WQdat)
```

    ##  [1] "LTER_site"   "local_site"  "depth/loc"   "date"        "time"       
    ##  [6] "chl_a"       "pH"          "temp"        "std_conduct" "conduct"    
    ## [11] "DO"          "sat"         "secchi"      "light_att"   "surf_light" 
    ## [16] "depth_light" "DOC"         "comments"

``` r
# structure of raw data
head(McKnight_GLV_WQdat)
```

    ## # A tibble: 6 x 18
    ##   LTER_site local_site `depth/loc`       date     time chl_a    pH  temp
    ##       <chr>      <chr>       <chr>     <date>   <time> <dbl> <dbl> <dbl>
    ## 1       NWT        GL4       0.05m 2000-07-06 01:00:00    NA    NA    NA
    ## 2       NWT        GL4          3m 2000-07-06 01:00:00    NA    NA    NA
    ## 3       NWT        GL4          9m 2000-07-06 01:00:00    NA    NA    NA
    ## 4       NWT        GL4       0.05m 2000-07-06       NA  1.34    NA    NA
    ## 5       NWT        GL4          3m 2000-07-06       NA  1.34    NA    NA
    ## 6       NWT        GL4          9m 2000-07-06       NA  3.34    NA    NA
    ## # ... with 10 more variables: std_conduct <dbl>, conduct <dbl>, DO <dbl>,
    ## #   sat <dbl>, secchi <dbl>, light_att <dbl>, surf_light <dbl>,
    ## #   depth_light <dbl>, DOC <dbl>, comments <chr>

``` r
# unique values of depth-location
unique(McKnight_GLV_WQdat$`depth/loc`)
```

    ##  [1] "0.05m"                 "3m"                   
    ##  [3] "9m"                    "8.1m"                 
    ##  [5] "9.3m"                  "9.5m"                 
    ##  [7] "10.4m"                 "10.5m"                
    ##  [9] "10.3m"                 "9.8m"                 
    ## [11] "10.6m"                 "10.7m"                
    ## [13] "8.5m"                  "10.9m"                
    ## [15] "12.3m"                 "11.7m"                
    ## [17] "9.2m"                  "12.1m"                
    ## [19] "12.2m"                 "Surface"              
    ## [21] "1m"                    "2m"                   
    ## [23] "4m"                    "5m"                   
    ## [25] "6m"                    "7m"                   
    ## [27] "8m"                    "Middle lake at 1m"    
    ## [29] "Inlet"                 "Outlet"               
    ## [31] "Second inlet"          "Inlet at rock glacier"
    ## [33] "Middle lake at 0.05m"  "Middle lake at 3m"    
    ## [35] "Middle lake at 6m"     "3.75m"                
    ## [37] "1.75m"                 "10m"                  
    ## [39] "11m"                   "12m"                  
    ## [41] "13m"                   NA                     
    ## [43] "0m"                    "Air"                  
    ## [45] "14m"

There are 45 different possible values for depth-location, some of which are actual lake depths and some not. For an outside researcher potentially interested in using the water quality dataset, this is confusing. For example, the online metadata describe the sampling location as the middle of the lake (<http://niwot.colorado.edu/meta_data/water_quality_GLV.dm.meta.txt>). What then is the difference between a "depth/loc" value of "1m" and "Middle lake at 1m"? What depths are "Inlet", "Outlet", and "Air"? Was the "Air" sample taken in the middle of the lake? Inlet? Outlet? These are some questions I can imagine a person who'd like use GLV water quality data having, and for that reason it's cleaner to separate depth from location, and is something that should be considered for how data are recorded and entered. While there are 45 unique "depth/loc" values in the water quality dataset, there are **122** unique values for "depth/loc" in the McKnight water chemistry dataset, even though that dataset only includes 1 additional site (Green Lake 3) compared to the McKnight water quality dataset:

``` r
# sites sampled in the water quality dataset
unique(McKnight_GLV_WQdat$local_site)
```

    ## [1] "GL4" "GL5" "GL1" "ALB"

``` r
# sites sampled in the water chemistry dataset
unique(McKnight_GLV_waterchem$local_site)
```

    ## [1] "GL3" "GL4" "GL1" "ALB" "GL5"

Kelly Loria has suggested breaking out the "core" water quality and chemistry data (data collected consistently through time) from the McKnight (ongoing) water quality and water chemistry datasets, and keeping infrequently-sampled water quality and water chemistry data in an auxiliary or experimental dataset. For example, some of the "depth/loc" values in the water chemistry dataset are from single-season REU projects (email communication from DMK to KL). In my experience trying to summarize these datasets, and figure out which values are appropriate for summarizing, having a true "core" dataset separate from an auxiliary or experimental dataset makes sense to me.

**QA Notes** Working through the McKnight water chemistry dataset, there are a few other issues to mention in the raw data. I show code to illustrate:

-   1905 erroneously entered for the year 2015 in the "year" column

``` r
# McKnight water chemistry dataset --
# show problem with year (1905 entered instead of 2015)
## all unique "year" values in dataset
unique(McKnight_GLV_waterchem$year)
```

    ##  [1] 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011
    ## [15] 2012 2013 1905 2016

``` r
## all unique "year" values when filtering the dataset for dates from the year 2015
unique(McKnight_GLV_waterchem$year[year(McKnight_GLV_waterchem$date)==2015])
```

    ## [1] 1905

``` r
# fix year
McKnight_GLV_waterchem$year <- year(McKnight_GLV_waterchem$date)
```

-   Three numeric fields (TDP, IP, and PO4) have "&lt;" entered, coercing the entire field in R as a character instead of numeric when it's read in.
-   "&lt;" is also an ambiguous value. I know it doesn't exceed a certain level, but I can't say whether "&lt;5" is 4 or 1 which could make a difference for averaging or determining minimum values.
    -   Since I'm not sure of the true value, I exclude anything with "&lt;" for summarizing.

``` r
# remove any values with "<" since not sure of value in context of other values
## TDP ##
# how many values have "<"?
summary(with(McKnight_GLV_waterchem, grepl("<", TDP)))
```

    ##    Mode   FALSE    TRUE    NA's 
    ## logical     908      13       0

``` r
# assign NA to any "<" value, convert field to numeric
McKnight_GLV_waterchem$TDP <- as.numeric(with(McKnight_GLV_waterchem, ifelse(grepl("<", TDP), NA, TDP))) # if true has "<"

## IP ##
# how many values have "<"?
summary(with(McKnight_GLV_waterchem, grepl("<", IP))) # if true has "<"
```

    ##    Mode   FALSE    TRUE    NA's 
    ## logical     902      19       0

``` r
# assign NA to any "<" value, convert field to numeric
McKnight_GLV_waterchem$IP <- as.numeric(with(McKnight_GLV_waterchem, ifelse(grepl("<", IP), NA, IP)))

## PO4 ## 
# how many values have "<"?
summary(grepl("<", McKnight_GLV_waterchem$'PO4---')) # if true has "<"
```

    ##    Mode   FALSE    TRUE    NA's 
    ## logical     902      19       0

``` r
# assign NA to any "<" value, convert field to numeric
McKnight_GLV_waterchem$'PO4---' <- as.numeric(ifelse(grepl("<", McKnight_GLV_waterchem$`PO4---`), NA, McKnight_GLV_waterchem$'PO4---')) 
```

Exluding these "&lt;" values also seems reasonable because they represent a small fraction (2% or less) of all observations within each field.

Visualize data availability
---------------------------

The following figures show data availability, temporal and depth ranges, and sampling frequency within for the tidied long-form datasets. The reason I include lake ice phenology data is to give some context to when summer season lake data was collected. The metata data for lake water quality and water chemistry (McKnight) states samples were collected approximately 1 week after ice-off, so I wanted to check against Nel Caine's ice phenology data.

Things I'd like feedback on include:

-   For presenting data availability, which figures are most compelling?
-   Given data availability (e.g. inconsistencies in sampling frequency by site and depth), how to best summarize data for a usable long-term summary dataset of lake trends

Showing the ice phenology data availability via raw data values is easy because it's a relatively simple dataset (3 variables x site x time). Showing data availability via actual data values with either water quality or chemistry data is trickier because there are more data (more variables x different depths x location (lake, inlet, outlet) x site x time).

**All lakes ice phenology**

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/plot%20data%20availability-1.png)

**Green Lake 4 water chemistry data availability**

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/data%20availablity%20for%20water%20chem-1.png)![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/data%20availablity%20for%20water%20chem-2.png)![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/data%20availablity%20for%20water%20chem-3.png)

This figure shows just the "core" depths (0, 3 and 9). Symobology and coloring adjusted per CR's suggestion (compare with symbology in Fig. 4).

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/McKnight%20lake%20chemistry%20core-1.png)

**Green Lake 4 water quality data availability**

**QA note**: There really are that many samples for outlet chlorophyll-a in 2007. Chris Ray suggests remaking this as broken plot (see example here: <https://joergsteinkamp.wordpress.com/2016/01/22/broken-axis-with-ggplot2/>). If do this, will need to add code to automate detecting outliers and breaking y-axis as necessary.

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/water%20quality%20data%20availability-1.png)

**QA note**: In the raw data, secchi depths values are filled down for all depths (including "air"). In practice, there is only one secchi value per day/time for the entire water column.

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/lake%20water%20quality%20data%20availability-1.png)

**Alternative sampling frequency figures**

An alternative to showing sampling frequency as bar charts is to show it as point data by date by year. Instead of emphasizing the total number of observations per year, the focus is on when samples were taken and an interranual comparison of those dates. These figures capture the sampling range and specific dates sampled, but don't reflect frequency by depth.

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/waterchem%20date%20frequency-1.png) ![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/water%20quality%20date%20frequency-1.png)

**QA note**: Plotting ice off with summer lake sampling date (above), I noticed a slight trend in delay of lake sampling over time. A delayed ice off trend wasn't what I remembered from the NWT renewal/the Preston et al. 2016 paper, and so I plotted first sample, ice break and ice off by year. Since lake sampling started in 1998, there is a significant trend in lake first sampling date with time (ice break and ice off no). The metadata note lake sampling each year begins roughly 1 week after ice off. Any consequence of missing this window is worth considering when summarizing data and comparing vaues interannually.

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/first%20date%20QA%20check-1.png)

Data comparison for combining water chemistry datasets
------------------------------------------------------

Because of overlap in analytes, sites and location within sites, it would be great to merge the Caine and McKnight water chemistry datasets for continuity in time. This prompts are few questions:

-   How do values compare?
-   Are these datasets appropriate to join?
-   Is it appropriate to average values across datasets in years where there is sampling overlap?

The outlet samples were collected by both groups in the summer months (Jun - Oct). The Caine lake samples are ones collected from just below the ice in winter months (Nov - May), whereas the McKnight lake samples were only collected in summer and at various depths (see above).

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/plot%20actual%20data%20values-1.png)![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/plot%20actual%20data%20values-2.png)

Data summary
------------

``` r
# How many samples are there by month?
GL4_waterchem %>%
  mutate(mon = month(date, label=TRUE, abbr = TRUE)) %>%
  filter(location == "Outlet") %>%
  ggplot() +
  geom_point(aes(mon, value, col=source), alpha=0.4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(~metric, scales = "free_y")
```

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/Summarize%20GL4%20outlet%20data-1.png)

``` r
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
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(~metric, scales = "free_y")
```

    ## Warning: Removed 29 rows containing missing values (geom_errorbar).

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/Summarize%20GL4%20outlet%20data-2.png)

``` r
GL4_waterchem %>%
  mutate(mon = month(date, label=TRUE, abbr = TRUE)) %>%
  filter(location == "Lake") %>%
  ggplot() +
  geom_point(aes(mon, value, col=year), alpha = 0.4) +
  scale_color_distiller(palette= "BrBG") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(~metric, scales = "free_y")
```

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/Summarize%20GL4%20outlet%20data-3.png)

``` r
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
       title = paste("Green Lake 4 water chemistry", min(GL4_waterchem$year), "-", max(GL4_waterchem$year)))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90)) +
  facet_wrap(~metric, scales = "free_y")
```

    ## Warning: Removed 83 rows containing missing values (geom_errorbar).

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/Summarize%20GL4%20outlet%20data-4.png)

``` r
GL4_WQ_long %>%
  mutate(mon = month(date, label = TRUE, abbr = TRUE)) %>%
  filter(location !="Lake") %>%
  ggplot() +
  geom_boxplot(aes(mon, value, group= mon), col= "gray50", alpha=0.4) +
  geom_point(aes(mon, value, fill= yr), col= "gray50", pch=21, alpha=0.4) +
  scale_fill_distiller(palette = "BrBG", direction = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle= 90)) +
  facet_wrap(location~metric, scales = "free_y", ncol=8)
```

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/Summarize%20GL4%20outlet%20data-5.png)

``` r
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
  scale_color_brewer(name= "Depth", palette = "PuBu", direction = 1) +
  scale_fill_brewer(name= "Depth", palette = "PuBu", direction = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle= 90)) +
  facet_wrap(~metric, scales = "free_y")
```

    ## Warning: Removed 29 rows containing missing values (geom_errorbar).

![](GL4_WQ_waterchem_datamunging_files/figure-markdown_github/Summarize%20GL4%20outlet%20data-6.png)
