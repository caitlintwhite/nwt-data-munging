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

# read in metadata with old plots numbers
oldsites <- readLines("http://nwt.colorado.edu/meta_data/saddfert.ts.meta.txt")
# metadata with current plot numbers
oldsites_current <- readLines("http://nwt.colorado.edu/meta_data/saddferb.ts.meta.txt")


# -- PREP SF SITES ----
#wet meadow = no snow, recode snow and snowfence recovery to binary
# plot 76 should NOT be a snowfence recovery plot (entered incorrectly)

sdlLT <- sdlsites %>%
  mutate(snow = ifelse(meadow == "wet", "no snow", snow),
         snow = recode(snow, `no snow` = 0, snow = 1),
         meadow = recode(meadow, dry = "DM", wet = "WM", mesic ="MM"),
         trt = recode(trt, N="NN", C ="CC", P = "PP", `N+P` = "NP"),
         snow_notes = ifelse(plot == 76, "in snowfence area", snow_notes),
         snow_recovery = ifelse(grepl("recovery", snow_notes), 1, 0),
         # add in site info
         LTER_site = "Niwot Ridge LTER",
         local_site = "SDL snowfence") %>%
  rename(fert = trt,
         veg_class = meadow,
         plot_num = plot) %>%
  #drop plot 286 from 1997
  filter(plot_num != 286)

# extract old tags in anpp metadata on server to be sure all info in there
sitepos <- grep("^Treatment", oldsites)
sfsites <- data.frame(dat =oldsites[sitepos[1]:(sitepos[1]+16)]) %>%
  separate(dat, into = paste0("col", 1:15), sep = " +")
names(sfsites) <- c("fert", "dry_x", "dry_y", "dry", "mesic_x", "mesic_y", "mesic")
#drop any cols that are all NA
sfsites <- sfsites[2:nrow(sfsites),!sapply(sfsites, function(x) all (is.na(x)))]
sfsites <- separate(sfsites, dry_y, c("dry_ymin", "dry_ymax"), sep = "[-]") %>%
  separate(mesic_y, c("mesic_ymin", "mesic_ymax"), sep = "[-]") %>%
  mutate(dry_2004 = str_extract(dry, "[0-9]{3}[+]"),
         mesic_2004 = str_extract(mesic, "[0-9]{3}[+]"),
         dry = substr(dry, 2, 4),
         mesic = substr(mesic, 2, 4)) %>%
  # make all that should be numeric, numeric (remove non-number chars)
  mutate_at(vars(names(.)[2:ncol(.)]), parse_number)
# split up
drysf <- sfsites[,grep("fert|dry", names(sfsites))] %>%
  mutate(veg_class = "DM",
         snow = 1) %>%
  rename_all(function(x) gsub("dry_", "", x)) %>%
  rename(tag_sw_orig = dry,
         tag_sw_2004 = `2004`)
mesicsf <- sfsites[,grep("fert|mes", names(sfsites))] %>%
  mutate(veg_class = "MM",
         snow = 1) %>%
  rename_all(function(x) gsub("mesic_", "", x)) %>%
  rename(tag_sw_orig = mesic,
         tag_sw_2004 = `2004`)
# stack snowfence sites
sfsites_meta <- rbind(drysf, mesicsf) 
rm(drysf, mesicsf, sfsites)

# non snowfence
nsfsites <- data.frame(dat =oldsites[sitepos[2]:(sitepos[2]+16)]) %>%
  separate(dat, into = paste0("col", 1:15), sep = " +") %>%
  #clean up whitespace
  mutate_all(function(x) ifelse(x == "", NA, x))
names(nsfsites) <- c("fert", "dry_x", "dry_y", "dry", "mesic_x", "mesic_y", "mesic")
#drop any cols that are all NA
nsfsites <- nsfsites[2:nrow(nsfsites),!sapply(nsfsites, function(x) all (is.na(x)))]
# correct a few errors from lack of space
nsfsites$mesic[is.na(nsfsites$mesic)] <- nsfsites$mesic_y[is.na(nsfsites$mesic)]
nsfsites$mesic_y[nsfsites$mesic_y == nsfsites$mesic] <- str_extract(nsfsites$mesic_x[grepl("-", nsfsites$mesic_x)], ",[0-9].+")
nsfsites$mesic_x <- gsub(",.+", "", nsfsites$mesic_x)
nsfsites <- separate(nsfsites, dry_y, c("dry_ymin", "dry_ymax"), sep = "[-]") %>%
  separate(mesic_y, c("mesic_ymin", "mesic_ymax"), sep = "[-]") %>%
  separate(dry, c("dry", "dry_2002"), sep = ",") %>%
  mutate(mesic_tag_sw_orig = substr(mesic, 2,5),
         mesic_tag_ne_orig = str_extract(mesic, ",[0-9]{3}[*]{0,1}[)]"),
         mesic_tag_sw_2002 = ifelse(grepl("[)][(][0-9]{3},", mesic), str_extract(mesic, "[)][(][0-9]{3},"), str_extract(mesic, ",[0-9]{3},")),
         mesic_tag_sw_2004 = str_extract(mesic, "[(][0-9]{3}[+]."),
         mesic_tag_ne_2004 = str_extract(mesic, ",[0-9]{3}[+][+]"))
        
  
# split up
drynsf <- nsfsites[,grep("fert|dry", names(nsfsites))] %>%
  mutate(veg_class = "DM",
         snow = 0) %>%
  rename_all(function(x) gsub("dry_", "", x)) %>%
  rename(tag_sw_orig = dry,
         tag_sw_2002 = `2002`) %>%
  # make all that should be numeric, numeric (remove non-number chars)
  mutate_at(vars("x", "ymin", "ymax", "tag_sw_orig", "tag_sw_2002"), parse_number) %>%
  #add tag_sw_2004
  mutate(tag_sw_2004 = NA,
         tag_ne_orig = NA,
         tag_ne_2004 = NA)
mesicnsf <- nsfsites[,grep("fert|mes", names(nsfsites))] %>%
  mutate(veg_class = "MM",
         snow = 0) %>%
  rename_all(function(x) gsub("mesic_", "", x)) %>%
  # make all that should be numeric, numeric (remove non-number chars)
  mutate_at(vars("x", "ymin", "ymax", names(.)[grepl("tag", names(.))]), parse_number) %>%
  # drop mesic
  dplyr::select(-mesic)
         
# stack snowfence sites
nsfsites_meta <- rbind(drynsf[,names(mesicnsf)], mesicnsf) 
rm(drynsf, mesicnsf, nsfsites)

# stack all sites
sffert_meta <- sfsites_meta %>%
  # add colnames in meta non snowfence sites
  mutate(tag_ne_orig = NA, tag_sw_2002 = NA, tag_ne_2004 = NA) %>%
  dplyr::select(names(nsfsites_meta)) %>%
  rbind(nsfsites_meta)

# review non-wet meadow sites
checkLUTnwm <- left_join(sdlLT, sffert_meta) %>%
  filter(veg_class != "WM") %>%
  # reorg cols
  dplyr::select(LTER_site, local_site, plot_num, fert, veg_class, snow, snow_recovery, x, ymin, ymax, 
                plot_sw_tag_PDF:ncol(.))





# -- review wet meadow ----
# get wet meadow
wm <- oldsites[grep("Wet Meadow", oldsites)] %>%
  str_extract(., "[0-9]{3}[(].*") %>%
  str_split(.,",") %>% unlist() %>% trimws()
wm_sites <- data.frame(plot_tag_orig = wm, veg_class = "WM", snow = 0) %>%
  mutate(fert = str_extract(plot_tag_orig, "[A-Z]{2}"),
         plot_tag_orig = parse_number(plot_tag_orig)) 

# read in old metadata with current numbers
oldnew_pos <- grep("^Treatment", oldsites_current)
wm_oldnew <- oldsites_current[(grep("^Wet Meadow", oldsites_current)+1):(grep("^Wet Meadow", oldsites_current)+16)] %>% 
  as.data.frame() %>% rename(fert='.') %>%
  mutate(plot_num = parse_number(str_extract(fert, "[0-9]{2},")),
         plot_tag_orig = parse_number(str_extract(fert, ", [0-9]{3}")),
         fert = str_extract(fert, "[A-Z]{2}"),
         veg_class = "WM",
         snow = 0)
#check old and oldnew line up
wm_sites <- left_join(wm_sites, wm_oldnew) # yes, no NAs

checkLUTwm <- left_join(sdlLT, wm_sites) %>%
  filter(veg_class == "WM") %>% # numbers from PDF don't match up with some.. maybe they were tags later replaced in 2004?
  # add in N and E directions (based on description in metadata and orientation in PDF)
  mutate(wm_y = ifelse(plot_num %in% c(33,46,47,39), 1,
                       ifelse(plot_num %in% c(37, 42, 38, 36), 2,
                       ifelse(plot_num %in% c(45, 34, 48, 40), 3, 4))),
         wm_x = ifelse(plot_num %in% c(33,37,45,41), 1,
                       ifelse(plot_num %in% c(46, 42, 34, 43), 2,
                              ifelse(plot_num %in% c(47, 38, 48, 35), 3, 4))))
# winnow down
wm_final <- checkLUTwm %>%
  #rename x and ymin ymax to indicate for snowfence
  rename(tag_sw_orig = plot_tag_orig) %>%
  dplyr::select(LTER_site, local_site, plot_num, fert, veg_class, snow, snow_recovery, x, ymin, ymax, wm_x, wm_y, plot_sw_tag_PDF:ncol(.))
# remove any cols that don't have values
wm_final <- wm_final[,!sapply(wm_final, function(x) all(is.na(x)))]
# manually reviewed.. all SW numbers the same except for 240 as PDF sw tag.. will leave in as backup SW number. No NEs numbers ever used, but exist on PDF
wm_final <- wm_final %>%
  # add snow x, ymin and ymax back in (have no value bc wet meadow sites not in snowfence area)
  mutate(snow_x = NA, snow_ymin = NA, snow_ymax = NA) %>%
  dplyr::select(LTER_site:snow_recovery, snow_x, snow_ymin, snow_ymax, wm_x, wm_y, tag_sw_orig, plot_ne_tag_PDF, plot_sw_tag_PDF) %>%
  rename(tag_ne_orig = plot_ne_tag_PDF,
         tag_sw_backup = plot_sw_tag_PDF) %>%
  mutate(tag_sw_backup = ifelse(tag_sw_backup != tag_sw_orig, tag_sw_backup, NA))
# clean up
rm(checkLUTwm, wm_sites, wm_oldnew)


# -- combine final for write-out -----
# combine dm, mm, and wm site LUTs





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
  dplyr::select(LTER_site, local_site:ncol(.)) %>%
  # add m to collect_date for missing
  mutate(collect_date = ifelse(is.na(collect_date), "m", collect_date)) %>%
  # drop 286 from 1997 since can't match it to anything and supposedly only dry meadow non-snowfence sampled anyway
  filter(plot_num != 286)
  

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
                   site = "SDL snowfence",
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

# double check final vals
sapply(sffert_vert2016, function(x)sort(unique(x)))



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

# final check
sapply(sffert_rich, function(x) sort(unique(x)))


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
