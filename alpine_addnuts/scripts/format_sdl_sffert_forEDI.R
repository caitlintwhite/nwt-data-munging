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
options(stringsAsFactors = F, strip.white = T, na.strings = c("NA", NA, "NaN", NaN, "."))
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
anpp_2014 <- read.csv("http://nwt.colorado.edu/data_csvs/saddferb.ts.data.csv", na.strings = c("NaN", NA, "NA", NaN))


# check that everything read in as expected
str(anpp)
str(anpp_2014)
str(all_sppcomp)
str(all_biodiv)

# infill plot number for XX controls in anpp 2014 dataset
anpp_2014$plot_num[anpp_2014$old_plot_num == "XX1"] <- 17 # from metadata
anpp_2014$plot_num[anpp_2014$old_plot_num == "XX2"] <- 29 # from metadata

# -- PREP SF SITES ----
#wet meadow = no snow, recode snow and snowfence recovery to binary
# plot 76 should NOT be a snowfence recovery plot (entered incorrectly)

sdlLT <- sdlsites %>%
  mutate(snow = ifelse(meadow == "wet", "no snow", snow),
         snow = recode(snow, `no snow` = 0, snow = 1),
         meadow = recode(meadow, dry = "DM", wet = "WM", mesic ="MM"),
         trt = recode(trt, N="NN", C ="CC", P = "PP", `N+P` = "NP"),
         snow_notes = ifelse(plot == 76, "in snowfence area", snow_notes),
         snow_recovery = ifelse(grepl("recovery", snow_notes), 1, 0)) %>%
  rename(fert = trt,
         veg_class = meadow,
         plot_num = plot) %>%
  #drop plot 286 from 1997
  filter(!is.na(plot_num))



# -- PREP SPP COMP (ALL HITS SUMMED PER SPP PER PLOT) ----
sdlcomp <- subset(all_sppcomp, site == "sdl") %>%
  # drop plotid and block and other remnant cols from analysis
  dplyr::select(-c(plotid, block, trt2, simple_lifeform, simple_lifeform2)) %>%
  # convert codes to match anpp and richness dats
  mutate(meadow = recode(meadow, dry = "DM", wet = "WM", mesic = "MM"),
         trt = recode(trt, C = "CC", N = "NN", P = "PP", `N+P` = "NP"),
         # change wet meadow to no snow
         snow = ifelse(meadow == "WM", "no snow", snow),
         # recode snow and recovery field
         snow = recode(snow, `no snow` = 0, snow = 1),
         # correct plot 76 for recovery -- Tim coded as recovered but isn't
         snow_notes = ifelse(plot == 76, "in snowfence area",snow_notes),
         # code recovery as yr 2016 and later, if recovery noted
         snow_notes = ifelse((grepl("recov", snow_notes)& yr >= 2016), 1, 0),
         site = "SDL snowfence") %>%
  rename(USDA_Symbol = clean_code2,
         fert = trt,
         veg_class = meadow,
         snow_recovery = snow_notes,
         local_site = site,
         collect_date = date,
         plot_num = plot,
         year = yr)  %>%
  rename_at(grep("Scien", names(.)):ncol(.), function(x) paste0("USDA_", x)) %>%
  #rename_all(function(x) paste0(casefold(substr(x,1,1), upper = T), substr(x, 2, nchar(x)))) %>%
  #remove _x from SciName  
  rename_all(function(x) gsub("_x", "", x)) %>%
  # add in LTER site
  mutate(LTER_site = "Niwot Ridge LTER") %>%
  dplyr::select(LTER_site, local_site:ncol(.))

# to be sure recovered coded correctly
sapply(split(sdlcomp$snow_recovery, sdlcomp$year), unique) # looks good
sapply(split(sdlcomp$plot_num, sdlcomp$snow_recovery), function(x) length(unique(x))) # looks good
# final check
sapply(sdlcomp, function(x) sort(unique(x)))
# infill 2Scat with  elk scat
sdlcomp$simple_name[sdlcomp$USDA_Symbol == "2SCAT"] <- "Scat"


# -- PREP VERT SPP COMP ----
# for 2016 only
sffert_vert2016 <- mutate(vert2016, LTER_site = "Niwot Ridge LTER",
                   site = "SDL Snowfence",
                   # recode meadow and fertilization
                   meadow = recode(meadow, dry = "DM", mesic = "MM", wet = "WM"),
                   trt = recode(trt, N = "NN", P = "PP", C = "CC", `N+P`= "NP"),
                   date = "m") %>%
  # drop functional groups -- rejoin USDA plant info bc sciname missing
  dplyr::select(LTER_site, site:simple_name) %>%
  rename(local_site = site,
         snow_recovery = snow_notes,
         veg_class = meadow,
         fert = trt,
         plot_num = plot,
         vertical = hit,
         year = yr,
         collect_date = date,
         USDA_Symbol = clean_code2) %>%
  # recode
  ## change plot 76 to no snow recovery
  ## wet meadow needs to be no snow
  mutate(snow_recovery = ifelse(plot_num == 76, "in snowfence area", snow_recovery),
         #wm is no snow (but also not sampled in 2016)
         snow = ifelse(veg_class == "WM", "no snow", snow),
         snow = recode(snow, `no snow` = 0, snow = 1),
         snow_recovery = ifelse(grepl("recov", snow_recovery), 1, 0)) %>%
  # prefix USDA_ to usda plants db cols
  left_join(distinct(dplyr::select(sdlcomp, USDA_Symbol, USDA_Scientific_Name:USDA_Growth_Habit))) %>%
  # be sure ordered by plot, point, vertical order
  arrange(plot_num, point, vertical)

# double check plots match sdlLT
vertcheck <- distinct(dplyr::select(sffert_vert2016, plot_num:fert)) %>%
  left_join(sdlLT) # everything checks out (manually looked)



# -- PREP ANPP ----
# write anpp separately from richness..
sffert_anpp <- dplyr::select(anpp_2014, -spp_rich) %>%
  # specify 0 for gram and total weight where forb weight present
  mutate(wt_gr1 = ifelse(!is.na(wt_fb1) & is.na(wt_gr1), 0, wt_gr1),
         wt_tot1 = ifelse(!is.na(wt_fb1) & is.na(wt_tot1), wt_fb1, wt_tot1)) %>%
  gather(group, anpp_g, wt_gr1:ncol(.)) %>%
  filter(!is.na(anpp_g)) %>%
  mutate(rep = parse_number(group),
         group = gsub("wt_|[0-9]", "", group),
         group = recode(group, fb = "Forb", gr = "Graminoid", tot = "Total"),
         snow = recode(snow, no_snow = 0, snow = 1)) %>%
  rename(fert = fert_tmt) %>%
  # add site
  mutate(LTER_site = "Niwot Ridge LTER", local_site = "SDL snowfence",
         snow_recovery = 0) %>%
  #reorder cols
  dplyr::select(LTER_site, local_site, year, collect_date, plot_num, old_plot_num, veg_class, fert, snow, snow_recovery, group, rep, anpp_g) %>%
  arrange(year, plot_num, rep, group)
 
# double check plot designations
anpp_plots <- distinct(dplyr::select(sffert_anpp, plot_num:snow)) %>%
  left_join(sdlLT, by = c("plot_num" = "plot", "snow", "fert", "veg_class"))
# check that no snow_notes cols are NA (shouldn't be if everything matched correctly)
summary(is.na(anpp_plots$snow_notes))
# manually checked that all plot nums match up with various cols in CTW LT and do

# plot out to check numbers (2014 seems high compared to earlier years)
ggplot(subset(sffert_anpp, group == "Total" & rep == 1), aes(as.factor(year), anpp_g, group = plot_num)) +
  geom_line() +
  geom_point() +
  facet_grid(fert~snow)
# still seems a bit high...
subset(sffert_anpp, group == "Total") %>%
  mutate(snow = recode(snow, '1' = "Snow", '0' = "No snow")) %>%
  ggplot(aes(as.factor(plot_num), anpp_g, col = as.factor(year), shape = veg_class)) +
  geom_point(size = 2, alpha = 0.7) +
  ggtitle("SDL snowfence ANPP, by snow and fertilization treatment, colored by year, symbol by meadow") +
  labs(y = "g per 0.04 m^2", x = "plot") +
  scale_color_brewer(palette = "Set2", name = "Year") +
  scale_shape(name = "Meadow") +
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(size = 11.5)) +
  facet_grid(fert~ snow, scales = "free_x")
# write out plot for Tim to review
ggsave("alpine_addnuts/figures/SFFERT_review_anpp.pdf", width = 8, height = 6)

# check anpp values
sapply(sffert_anpp, function(x) sort(unique(x))) # looks fine



# -- PREP 1996-1997 RICHNESS DATA (from nwt website) ----
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

# tim says still post richness data from 1996, 1997, we'll just add a note about count differences with rest of dataset
# format 96, 97 richness data for EDI
## bc plot codes in ANPP match, okay to go for richness
sffert_rich <- dplyr::select(anpp_2014, year:collect_date, spp_rich) %>%
  filter(!is.na(spp_rich)) %>%
  mutate(snow = recode(snow, no_snow = 0, snow = 1)) %>%
  rename(fert = fert_tmt) %>%
  # add site
  mutate(LTER_site = "Niwot Ridge LTER", local_site = "SDL snowfence",
         snow_recovery = 0) %>%
  #reorder cols
  dplyr::select(LTER_site, local_site, year, plot_num, old_plot_num, veg_class, fert, snow, snow_recovery, spp_rich) %>%
  arrange(year, plot_num)

# double check plot designations
anpp_plots <- distinct(dplyr::select(sffert_anpp, plot_num:snow)) %>%
  left_join(sdlLT, by = c("plot_num" = "plot", "snow", "fert", "veg_class"))



# -- WRITE OUT DATA -----
# specify outpath for writing data
outpath <- "alpine_addnuts/output_data/forEDI/"
write.csv(sdlcomp, paste0(outpath, "sffert_sppcomp_1997ongoing_forEDI.csv"), row.names = F)
write.csv(sffert_vert2016, paste0(outpath, "sffert_sppcomp_2016vert_forEDI.csv"), row.names = F)
write.csv(sffert_anpp, paste0(outpath, "sffert_anpp_1996ongoing_forEDI.csv"), row.names = F)
write.csv(sffert_rich, paste0(outpath, "sffert_spprich_19961997_forEDI.csv"), row.names = F)
write.csv(sdlLT, paste0(outpath, "sffert_plotlookup_forEDI.csv"), row.names = F)


# write to gdrive for Anna
# write to google drive for Anna
## get 418 folder
gdrive138 <- drive_find(pattern = "PKG_138", n = 30) %>% drive_ls()
# write anpp dataset
# rename anpp dat to datname on EDI
drive_upload(media = paste0(outpath, "sffert_anpp_1996ongoing_forEDI.csv"),
             path = gdrive138[grep("clean", gdrive138$name, ignore.case = T),], 
             name = "saddferb.ts.data.csv", overwrite = T)
# write spp comp
## > write all years, hits summed by spp by plot
drive_upload(media = paste0(outpath, "sffert_sppcomp_1997ongoing_forEDI.csv"),
             path = gdrive138[grep("clean", gdrive138$name, ignore.case = T),], 
             name = "saddfert.ts.data.csv", overwrite = T)
## > write 2016 vertical sppcomp data 
drive_upload(media = paste0(outpath, "sffert_sppcomp_2016vert_forEDI.csv"),
             path = gdrive138[grep("clean", gdrive138$name, ignore.case = T),], 
             name = "saddferv.ts.data.csv", overwrite = T)
# write spp richness
# give it name similar to other datasets
drive_upload(media = paste0(outpath, "sffert_spprich_19961997_forEDI.csv"),
             path = gdrive138[grep("clean", gdrive138$name, ignore.case = T),], 
             name = "saddferr.ts.data.csv", overwrite = T)
# write plot lookup table
# give it name similar to other datasets
drive_upload(media = paste0(outpath, "sffert_plotlookup_forEDI.csv"),
             path = gdrive138[grep("clean", gdrive138$name, ignore.case = T),], 
             name = "saddfert.ts.sitelut.csv", overwrite = T)
