# clean SDL SFfert for EDI

# read in CTW prepped spp comp and anpp data and tweak cols for publication EDI
## EDI package ID knb-lter-nwt.138
# write out to Anna and SCE's Google Drive folder


# NOTE!
# edi package id naming convention for this dataset is: saddfer[x].ts.data.ext (where x is the what, e.g. b = biomass)



# -- SETUP ----
rm(list = ls())
library(tidyverse)
library(googledrive)
options(stringsAsFactors = F, strip.white = T)
theme_set(theme_bw())
source("EDI_functions.R")

# read in data CTW cleaned for TS 2019 ms
alldat <- list.files("alpine_addnuts/output_data/forTS/", full.names = T)
all_sppcomp <- read.csv(alldat[grep("sppcomp", alldat)])
all_biodiv <- read.csv(alldat[grep("biodiv", alldat)])

# other dats that might be helpful..
## 2016 vertical spp comp (includes grid point and vertical hit order)
vert2016 <- read.csv("alpine_addnuts/output_data/sdl2016_vertical_sppcomp.csv")
## sdl plot lookup tbl
sdlsites <- read.csv("alpine_addnuts/output_data/sdl_plot_lookup.csv")

# saddle sffert anpp 1996, 1997 data (on EDI)
anpp <- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.138.3&entityid=754715ae8d14748faaa98f48e178fb83",
                 na.strings = c("NA", NA, NaN, "NaN"), header = FALSE)
anpp_meta <- readLines("http://nwt.colorado.edu/meta_data/saddferb.ts.meta.txt")
anpp_2014 <- read.csv("http://nwt.colorado.edu/data_csvs/saddferb.ts.data.csv")


# check that everything read in as expected
str(anpp)
str(anpp_2014)
str(all_sppcomp)
str(all_biodiv)



# -- PREP SPP COMP (ALL HITS SUMMED PER SPP PER PLOT) ----
sdlcomp <- subset(all_sppcomp, site == "sdl") %>%
  # drop plotid and block and other remnant cols from analysis
  dplyr::select(-c(plotid, block, trt2, simple_lifeform, simple_lifeform2)) %>%
  # convert codes to match anpp and richness dats
  mutate(meadow = recode(meadow, dry = "DM", wet = "WM", mesic = "MM"),
         trt = recode(trt, C = "CC", N = "NN", P = "PP", `N+P` = "NP")) %>%
  rename(USDA_Symbol = clean_code2,
         SnowNotes2016 = snow_notes,
         SimpleName = simple_name) %>%
  rename_at(grep("Scien", names(.)):ncol(.), function(x) paste0("USDA_", x)) %>%
  rename_all(function(x) paste0(casefold(substr(x,1,1), upper = T), substr(x, 2, nchar(x)))) %>%
  #remove _x from SciName  
  rename_all(function(x) gsub("_x", "", x))



# -- PREP ANPP AND RICHNESS DATA (from nwt website) ----
# compare richness in 1997 to ctw-calculated richness
richness97 <- full_join(subset(all_biodiv, yr == 1997), subset(anpp_2014, year < 1998), by = c("plot" = "plot_num", "yr" ="year")) %>%
  filter(veg_class == "DM" & snow.y == "no_snow") %>%
  dplyr::select(plot, yr, S, spp_rich) %>%
  gather(met, val, S:spp_rich)
  # infill richness for 96 so can plot connecting lines
richness97$val[richness97$met == "S" & richness97$yr == 1996] <- richness97$val[richness97$met == "spp_rich" & richness97$yr == 1996]
richness97 <- mutate(richness97, met = recode(met, S = "CTW 97 richness", spp_rich = "EDI 97 richness"))

compareyrs <- ggplot(richness97, aes(as.factor(yr), val)) +
  geom_line(aes(group = plot), col = "lightsalmon", alpha = 0.8) +
  geom_point(col = "indianred3", size = 2) +
  geom_point(data = subset(richness97, grepl("CTW", met) & yr == 1997), aes(as.factor(yr), val), col = "black", size = 2) +
  #geom_point(data = subset(richness97, !grepl("CTW", met) & yr == 1997), aes(as.factor(yr), val), col = "mediumpurple2") +
  scale_y_continuous(breaks = seq(6,28, 4), limits = c(6,28)) +
  labs(y = "Spp richness", x = "Year") +
  facet_wrap(~met)

comparetime <- subset(all_biodiv, plot <17 & site == "sdl" & !is.na(S)) %>%
  mutate(met = "All years, red = EDI, black = CTW") %>%
  ggplot(aes(yr, S)) +
  geom_line(aes(group = plotid), col = "grey50", alpha = 0.8) +
  geom_point(size = 2) +
  labs(y = NULL, x = "Year") +
  #geom_line(data = subset(anpp_2014, plot_num < 17), aes(year, spp_rich, group = plot_num), col = "orchid", alpha = 0.5, position = position_dodge(width = 0.1)) +
  geom_point(data = subset(anpp_2014, plot_num < 17), aes(year, spp_rich), col = "indianred3", alpha = 0.6, position = position_dodge(width = 0.1), size =2) +
  scale_y_continuous(breaks = seq(6,28, 4), limits = c(6,28)) +
  scale_x_continuous(breaks = seq(1996, 2012,4)) +
  facet_wrap(~met)

cowplot::plot_grid(compareyrs, comparetime)
# write out plot for Tim to decide what to do with spp richness in 96,97


# write anpp separately..
sffert_anpp <- dplyr::select(anpp_2014, year:wt_tot2) %>%
  # infill obs that had a wgt for forb or grass but not the other and total (should be 0) -- happens in 2014
  mutate(wt_gr1 = ifelse(!is.na(wt_fr1) & wt_gr1 == "NaN", 0, wt_gr1))
  gather(Group, ANPP_g_per_0.4m2, wt_gr1:ncol(.))
  

# -- WRITE OUT DATA -----
write.csv(sdlcomp, "alpine_addnuts/output_data/sdl_sppcomp_1997ongoing_forEDI.csv", row.names = F)

