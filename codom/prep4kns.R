# prep codom for FI calculations in python
# caitlin.t.white@colorado.edu
# july 2020

# script purpose:
# for kns proposal.. prep sppcomp data per kns instructions
# 1) read in nwt lter codom dataset dynamically from EDI
# 2) instructions:
# I think 4 year and 8 year windows would be great. One year increments. 
# I understand these better than the size of state, so I think the default is best for now on that one.
# Agree with removing rare species and unknowns, based on how FI is calculated, 
# I might make this plot-based, and remove species from analysis if they are absent from a plot >50% of the years. 
# >> CTW: check to see what is avg amt spp appears in a given plot
# If that sounds ridiculous, a removal cut off less than 1% average in a plot.
# >> CTW: be sure to check for #hits per point

# notes:
# EDI data package is knb-nwt-lter-6 (currently v2 at time of script)



# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)
theme_set(theme_bw())
source("edi_functions.R")

# read in plant dat
codom <- getTabular2(edi_id = 6)
glimpse(codom)
summary(codom)
sapply(select(codom, site:spp, growth_habit, hits), function(x) sort(unique(x)))
