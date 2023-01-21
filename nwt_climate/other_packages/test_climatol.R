#climatol_check part 2 (run program on prepped data)

# -- SETUP -----
# prep data for SDL, D1, and C1 as references
library(tidyverse)
library(lubridate)
library(climatol)
theme_set(theme_test())

source("/Users/scarlet/github/nwt-data-munging/nwt_climate/other_packages/climhomog_ctw.R")

# set path to all data prepped for climatol
preppath <- "/Users/scarlet/Documents/nwt_lter/nwt_climate/other_packages/Climatol/prepped_data/"

# climatol requires setting working directory to where data live
gitpath <- "/Users/scarlet/github/nwt-data-munging"
setwd(preppath)
list.files(getwd())


# -- TEST PRECIP ----
# check dailies for outliers first
homogen("DLYppt", 1965, 2021, std = 2, expl = T)
homogen("sdlDLYppt", 1981, 2021, expl = T,
        std = 2, # divide data by mean rather than mean-center data
        swa = 365*3, # 3 year staggered window for SHNT checks 
        dz.max = 10, # allow up to 10sd before flagging possible outlies
        snht1 = 30, # default snht threshold is 25
        wd = c(0, 60, 25)) # set distance weighting to no weights first pass, within 100km, then within 25km
homogen("sdlDLYppt", 1988, 2021, expl = 2,
        std = 2, # divide data by mean rather than mean-center data
        trf = 1, # apply log 1 + 1 transformation 
        vmin = 0,
        swa = 365*3, # 3 year staggered window for SHNT checks 
        dz.max = 10, # allow up to 10sd before flagging possible outlies
        snht1 = 30, # default snht threshold is 25
        wd = c(0, 60, 25))

# no outliers present, try monthly analyses
dd2m("sdlDLYppt", 1981, 2021, valm = 1, namax = 5)
homogen("sdlDLYppt-m", 1981, 2021, expl = 2,
        #suf = "-m", # monthly data
        std = 2, # divide data by mean rather than mean-center data
        #trf = 1, # apply log 1 + 1 transformation 
        vmin = 0,
        swa = 365*3, # 3 year staggered window for SHNT checks 
        dz.max = 10, # allow up to 10sd before flagging possible outlies
        snht1 = 30, # default snht threshold is 25
        wd = c(0, 60, 25))

# no outliers present, try monthly analyses
dd2m("sdlDLYppt", 1988, 2021, valm = 1, namax = 5)
homogen("sdlDLYppt-m", 1988, 2021, #expl = 2,
        #suf = "-m", # monthly data
        std = 2, # divide data by mean rather than mean-center data
        #trf = 1, # apply log 1 + 1 transformation 
        vmin = 0,
        #swa = 365*3, # 3 year staggered window for SHNT checks 
        dz.max = 7, # allow up to 7sd before flagging possible outlies
        snht1 = 30, # default snht threshold is 25
        wd = c(0, 60, 25))

# then summarize monthlies to explore inhomogenieties
homogen("sdlMONppt", 1981, 2021, std = 2, expl = T)
homogen("sdlMONppt", 1988, 2021, std = 2, expl = T)
homogen("sdlMONppt", 1981, 2021, std = 2)
homogen("sdlMONppt", 1988, 2021, std = 2)
homogen("sdld1MONppt", 1981, 2021, std = 2, expl = T)


load("sdlMONppt_1981-2021.rda")
load("sdld1MONppt_1981-2021.rda")
load("sdlDLYppt_1981-2021.rda")


homogen("Ttest", 1981, 2000, expl = T)
load("Ttest_1981-2000.rda")

ttest_example <- read.table("Ttest_1981-2000.dat")
