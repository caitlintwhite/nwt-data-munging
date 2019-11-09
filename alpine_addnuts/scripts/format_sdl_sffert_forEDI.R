# clean SDL SFfert for EDI

# read in CTW prepped spp comp and anpp data and tweak cols for publication EDI
## EDI package ID knb-lter-nwt.138
# write out to Anna and SCE's Google Drive folder



# -- SETUP ----
rm(list = ls())
library(tidyverse)
library(googledrive)
options(stringsAsFactors = F, strip.white = T)

# read in data CTW cleaned for TS 2019 ms
alldat <- list.files("alpine_addnuts/output_data/forTS/", full.names = T)
all_sppcomp <- read.csv(alldat[grep("sppcomp", alldat)])
all_biodiv <- read.csv(alldat[grep("biodiv", alldat)])

# other dats that might be helpful..
## 2016 vertical spp comp (includes grid point and vertical hit order)
vert2016 <- read.csv("alpine_addnuts/output_data/sdl2016_vertical_sppcomp.csv")
## sdl plot lookup tbl
sdlsites <- read.csv("alpine_addnuts/output_data/sdl_plot_lookup.csv")


# -- PREP SPP COMP (ALL HITS SUMMED PER SPP PER PLOT) ----

