# replicate TS analyses
# > *to the extent possible with data available (cover data)


# output (things to replicate):
# Figs 1-2 (Saddle forb vs. grass cover by treatment by year, col plots with SEs)
# Parts of Tables 1-2 (NutNet by year [2013 = all 40 plots, 2017 = 28 plots], richness? entropy? plant cover and hits for sure)



# notes:
# SADDLE
# only 16 dry meadow plots consistently sampled across all three years
# plant cover = % (FIRST hits that were vegetation/ all FIRST hits) [veg = not litter, moss, lichen, rock or bare ground]
# > but when look at nutnet spreadsheets, it's actually 100-sum(non-veg cover) bc i think the protocol is to only record non-veg if no live veg above
# rel plant cov = % (number of hits of single species/total vascaular vegetation hits) <-- this ALL HITS!
# 1997 only had top hits; 2012 and 2016 allowed multiple hits per points
# > stat tests:
# > full factorial, randomized, no blocks

# edi published metadata link to sdl 97 anpp: http://nwt.colorado.edu/meta_data/saddferb.ts.meta.txt

# NUTNET
# +P and +P+K not sampled in 2017
# 0.25 cover = present in 1m^2 plot but not hit
# in nutnet 2013, selaginella = non-veg cover by nutnet analysis, meaning they wouldn't record selaginella if under any other plant (tim did in 2017, but only happened 2x. usually selaginella is the only species if it's at a point)
# ran Shannon Div on NutNet (including species present but not hit)
# 2 reps of C and +N+P+K so averaged those per block for subsequent calculations
# > stat tests:
# > 2013: all 40 plots, full factorial analysis of N, P and micro effects, randomized + blocked
# > 2017: only 28 plots sampled, one-way ANOVA to eval +N, +K, +N+P+K effects, randomized + blocked



# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(vegan)
library(cowplot)
source("edi_functions.R")
na_vals <- c("", " ", NA, "NA", "NaN", NaN, ".")
options(stringsAsFactors = F, strip.white = T, na.strings = na_vals)
theme_set(theme_bw())


# get data
# plant community data for sdl and nutnet, all yrs
plantcom <- read.csv("alpine_addnuts/output_data/sdl_nutnet_plantcom_allyrs.csv") # includes spp present (as 0.25) for yrs available
# get nn 2017 richness data from TS (sent 10/22/2019)
nn17_richness <- read.csv("../../Documents/nwt_lter/unpub_data/dry_meado_fert/summary_edit.csv")
# spp lookup table
spplist <- read.csv("alpine_addnuts/output_data/sdl_nutnet_spplookup.csv")
# sdl site info
sdlplots <- read.csv("alpine_addnuts/output_data/sdl_plot_lookup.csv") 
nnplots <- read.csv("alpine_addnuts/output_data/nutnet_plot_lookup.csv")

# read in prepped datasets where can pull top hits only
nn17_wvert <- read.csv("alpine_addnuts/output_data/nutnet2017_vertical_sppcomp.csv") 
sdl16_wvert <- read.csv("alpine_addnuts/output_data/sdl2016_vertical_sppcomp.csv") 
# cleaned up 2013 nutnet (all spp hit and present, ctw cleaned up for SCE)
nn13_clean <- read.csv("alpine_addnuts/output_data/nutnet2013_alldats/NWTnutnet_sppcomp_2013ongoing.csv") 
nn_anpp <- read.csv("alpine_addnuts/output_data/nutnet2013_alldats/NWTnutnet_anpp_2007ongoing.csv")


# sdl 97 anpp from EDI
#sffert richness 1997 (to troubleshoot plots that don't match up)
sdlS97 <- getTabular(138) #colnames don't read in correctly, fix now
names(sdlS97)
names(sdlS97)[grep("X", names(sdlS97))] <- NA
sdlS97[nrow(sdlS97)+1,] <- data.frame(t(names(sdlS97)))
# manually assign names from online metadata
names(sdlS97) <- c("yr", "loc", "trt", "plot", "sppS", "grass_wgt_rep1", "forb_wgt_rep1", "total_rep1", "grass_rep2", "forb_rep2", "total_rep2")


# review dats
glimpse(plantcom)
sapply(plantcom, function(x) sort(unique(x))) # looks good
glimpse(spplist)
glimpse(sdlplots)
glimpse(nnplots)



# -- PREP SITE DATA + RECODE TRTMENTS FOR NUTNET (SIMPLIFY) -----
# id nutnet plots and sdl plots sampled across all yrs
# sdl limited by sdl dry meadow non-snofence plots surveyed in 1997
# nutnet limitd by plots surveyed in 2017 (fewer than in 2013)
sdl_common <- unique(sdlplots$plot[!is.na(sdlplots$old_plot97LT) & sdlplots$snow == "no snow"])
nn_common <- unique(plantcom$plotid[plantcom$site == "nutnet" & plantcom$yr == 2017])
#to be sure, are these plots in 2013?
summary(nn_common %in% unique(plantcom$plotid[plantcom$site == "nutnet" & plantcom$yr == 2013])) # yup
# pull out P plots from nn too
nn_Pplots <- unique(nnplots$plotid[nnplots$trt == "P"])

#ts found no effect of +k, condense +k into other treatments (e.g. n+p+k to n+p; k to control)
# in previous ordination analysis, k plots overlay similar treatments without k (e.g. n+p and n+p+k hulls overlap on nmds) 
nnplots$trt2 <- gsub("[+]K", "", nnplots$trt)
nnplots$trt2 <- gsub("K", "C", nnplots$trt2)

# make trts factor in each site
#nnplots$trt <- factor(nnplots$trt, levels = c(c("C", "K", "N", "P", "P+K", "N+K", "N+P", "N+P+K")))
#nnplots$trt2 <- factor(nnplots$trt2, levels = c("C", "N", "P", "N+P"))
#sdlplots$trt <- factor(sdlplots$trt, levels = c("C", "N", "P", "N+P"))



# -- ADD SIMPLE LIFEFORM TO SPPLIST ----
#make simple lifeform grp
spplist <- spplist %>%
  mutate(simple_lifeform = ifelse(Family == "Fabaceae", "N-fixer",
                                  ifelse(grepl("Shrub", Growth_Habit), "Shrub",
                                         ifelse(Growth_Habit == "Graminoid", "Grass", 
                                                ifelse(grep("Forb", Growth_Habit), "Forb", "Ground cover")))),
         # change Dryas to forb from shrub (is listed as subshrub/shrub/forb)
         simple_lifeform = ifelse(simple_name == "Dryas octopetala", "Forb", simple_lifeform),
         # fill in simple lifeform for 2FORB and 2GRAM
         simple_lifeform = ifelse(clean_code2 == "2FORB", "Forb",ifelse(clean_code2 == "2GRAM", "Grass", simple_lifeform)),
         # ground cover doesn't assign (maybe won't work on NA values of growth)
         simple_lifeform = ifelse(grepl("^2", clean_code2) & is.na(simple_lifeform), "Ground cover", simple_lifeform)) %>%
  # add alternative lifeform group where N-fixer lumped into forb group
  mutate(simple_lifeform2 = ifelse(simple_lifeform == "N-fixer", "Forb", simple_lifeform),
         # add nutnet grouping (Selaginella densa [spikemoss] = ground cover, not forb)
         nutnet_grp = ifelse(clean_code2 == "SEDES", "Ground cover", simple_lifeform2))


# -- QA check: does NN-provided 2013 data, presence-only-removed, match NN 2013 cover data Tim sent?----
check2013 <- subset(nn13_clean, Hits > 0.25) %>%
  dplyr::select(Block, Plot, USDA_Symbol, Hits) %>%
  arrange(Block, Plot, USDA_Symbol)
plantcom2013 <- subset(plantcom,yr == 2013 & hits > 0.25) %>%
  mutate(block = substr(plotid, 2,2), 
         plot = as.numeric(gsub("B[1-4]_", "", plotid))) %>%
  dplyr::select(block, plot, clean_code2, hits) %>%
  arrange(block, plot, clean_code2)

summary(check2013[c("Block", "Plot", "USDA_Symbol", "Hits")] == plantcom2013[c("block", "plot", "clean_code2", "hits")])
#same
rm(check2013, plantcom2013)


# -- PREP ABUNDANCE-ONLY DATASETS, VEG AND NON-VEG ----
abundance <- subset(plantcom, hits > 0.25) %>%
  left_join(distinct(spplist[c("clean_code2", "simple_lifeform", "simple_lifeform2")]), by = "clean_code2")

# prep wide-form non veg, plant total hits, and plant cover (100-nonveg cover) data frame
coarse_cover <- abundance %>%
  # grp by Forb, Grass, Shrub (SEDES = Forb)
  group_by(site, yr, plotid, simple_lifeform2) %>%
  summarise(cover = sum(hits)) %>%
  ungroup() %>%
  mutate(simple_lifeform2 = recode(simple_lifeform2, `Ground cover` = "NonVeg")) %>%
  spread(simple_lifeform2, cover, fill = 0) %>%
  mutate(TotalHits = Forb + Grass + Shrub + NonVeg,
         PlantCov = 100-NonVeg,
         VegHits = Forb + Grass + Shrub,
         Forb_rel = (Forb/VegHits)*100,
         Grass_rel = (Grass/VegHits)*100,
         F2G_ratio = Forb/Grass) %>%
  # NA plant cover for 2003 and 2005 bc methods not consistent
  ## 2003: unclear whether Colin Tucker recorded all non-veg cover
  ## 2005: jane didn't survey 100 points per plot, not sure how to best calculate plant cover
  mutate(PlantCov = ifelse(yr %in% c(2003, 2005), NA, PlantCov))

# geum rossii
geum_cover <- subset(abundance, clean_code2 == "GERO2") %>%
  group_by(site, yr, plotid) %>%
  summarise(geum_hits = sum(hits)) %>%
  ungroup() %>%
  full_join(coarse_cover[c("site", "yr", "plotid", "VegHits")]) %>%
  # infill 0 for geum if NA
  mutate(geum_hits = ifelse(is.na(geum_hits), 0, geum_hits),
         geum_rel = (geum_hits/VegHits)*100) %>%
  arrange(site, yr, plotid)

# append geum hits and rel cov to coarse_cover
coarse_cover <- left_join(coarse_cover, geum_cover[c("site", "yr", "plotid", "geum_hits", "geum_rel")]) %>%
  rename(Geum_hits = geum_hits, Geum_rel = geum_rel)


# -- PREP BIODIVERSITY DATASETS (where richness available) -----
# nutnet 2013 and sdl 1997, 2005, and 2012 include spp present only (hits = 0.25)
## nn 2017: spp present not recorded, just raw total richness (calculate separately below)
plants <- subset(plantcom, yr %in% unique(yr[hits == 0.25])) %>%
  # okay to remove date
  dplyr::select(-date) %>%
  # remove ground cover
  filter(!clean_code2 %in% unique(spplist$clean_code2[spplist$simple_lifeform == "Ground cover" & !is.na(spplist$simple_lifeform)])) %>%
  unite(rowid, site, yr, plotid, sep = "_", remove = F)

richness <- group_by(plants, rowid, site, yr, plotid) %>%
  summarise(S = length(clean_code2)) %>%
  ungroup()

# shannon diversity
sppmatrix <- spread(plants, clean_code2, hits, fill = 0) %>% as.data.frame()
rownames(sppmatrix) <- sppmatrix$rowid
relmatrix <- decostand(sppmatrix[,(grep("plotid", names(sppmatrix))+1):ncol(sppmatrix)], method = "total")
H <- data.frame(H = diversity(relmatrix),
                rowid = rownames(relmatrix))

# put it all together
biodiv <- left_join(richness, H)

# run nn 2017 separately (tim didn't record species present, just total count of richness)
# need to first diff overall richness from spp abundance richness, then add in 0.25 per spp for extra spp present but not found
nn17hitsdat <- subset(plantcom, site == "nutnet" & yr == 2017) %>%
  dplyr::select(-date) %>%
  # remove rows for non-plants
  filter(!clean_code2 %in% unique(spplist$clean_code2[spplist$simple_lifeform2 == "Ground cover"])) %>%
  group_by(plotid) %>%
  mutate(S = length(unique(clean_code2)),
         # sum hits to check against tim's
         veghits = sum(hits), 
         # add block and plot so can join tim's richness dat
         block = as.numeric(substr(plotid, 2,2)),
         plot = as.numeric(gsub("B[1-4]_", "", plotid))) %>%
  ungroup() %>%
  # join richness dat from tim
  left_join(nn17_richness[c("block", "plot", "richness", "veg.hits")]) %>%
  # diff tim's plot richness from spp hits richness as logic check (diff should always be >= 0)
  mutate(checkS = richness-S,
         checkvhits = veghits - veghits)
# check
summary(nn17hitsdat[c("checkS", "checkvhits")]) # good
# max number of present spp is 7, so need to add 7 extra spp cols to nn17 abundance matrix

nn17wide <- dplyr::select(nn17hitsdat, site:hits) %>%
  spread(clean_code2, hits, fill = 0)
# iterate through and add in spp present  
present17 <- distinct(dplyr::select(nn17hitsdat, site:plotid, checkS))
# add max spp present (7) diff to nn17wide
nn17wide <- cbind(nn17wide, matrix(nrow = nrow(nn17wide), ncol = max(present17$checkS), dimnames = list(NULL, paste0("spp", 1:max(present17$checkS))))) %>%
  # make unique rowid col
  unite(rowid, site:plotid, sep = "_", remove = F)
for(i in 1:nrow(nn17wide)){
  # store number of spp present to infill
  fillcols <- present17$checkS[i]
  # infill number of extra spp
  nn17wide[i,paste0("spp", 1:7)] <- c(rep(0.25, fillcols), rep(0,max(present17$checkS)-fillcols))
}

# calculate diversity
rownames(nn17wide) <- nn17wide$rowid
relmatrix17 <- decostand(nn17wide[,(grep("plotid", names(nn17wide))+1):ncol(nn17wide)], method = "total")
# try relmatrix without additional spp to see if matches tim's numbers better
relmatrix17.2 <- decostand(nn17wide[,(grep("plotid", names(nn17wide))+1):(grep("spp1", names(nn17wide))-1)], method = "total")
H17 <- data.frame(H = diversity(relmatrix17),
                  # diversity on spp hit only
                  H2 = diversity(relmatrix17.2),
                rowid = rownames(relmatrix17)) %>%
  #join plot info to rbind with biodiv dat
  left_join(nn17wide[c("rowid", "site", "yr", "plotid")]) %>%
  # then join tim's richness data
  left_join(distinct(nn17hitsdat[c("plotid", "richness")])) %>%
  #rename richness to S
  rename(S = richness)

# how different are these diversity numbers from tim's?
Hcheck <- left_join(distinct(nn17hitsdat[c("plotid", "block", "plot", "checkS")]), H17[c("plotid", "H", "H2")]) %>%
  left_join(nn17_richness[c("block", "plot", "diversity")])
# numbers are about the same where no additional species were present, meaning diversity run on abundance data only in tim's dat (i think)
plot(Hcheck$checkS, Hcheck$diversity - Hcheck$H) # hm.. idk why have 3 additional present spp would make that big of a difference in H
plot(Hcheck$checkS, Hcheck$diversity - Hcheck$H2) #hm.. also off even if use spp hit only..actually looks more off

# stack H17 with biodiv
biodiv <- rbind(biodiv, H17[names(biodiv)])

# join biodiv and coarse_cover to coarse summary where info available
coarse_summary <- left_join(coarse_cover, dplyr::select(biodiv, -rowid))



# -- PREP ANPP ----
# prep 2013 anpp
nn13_anpp <- subset(nn_anpp, grepl("2013", as.character(Date))) %>%
  # crunch total
  spread(Group, ANPP_g_per_m2, fill = 0) %>%
  mutate(Total = Forb + Grass + Legume,
         plotid = paste0("B", Block, "_", Plot),
         site = "nutnet",
         yr = 2013,
         # add forb and legume since can't separate for saddle
         forb2 = Forb + Legume) %>%
  dplyr::select(site, yr, plotid, FullTreatment:Total, forb2) %>%
  rename_all(casefold) %>%
  rename(trt = fulltreatment) %>%
  dplyr::select(site, yr, plotid, trt, forb2, grass, total) %>%
  rename(forb = forb2)

# from metadata: Aboveground biomass measurements were taken by clip harvesting a 20x20 cm subplot within each plot.
area97 <- (20/100)*(20/100) # convert to m
sdl97_anpp <- subset(sdlS97, yr == 1997) %>%
  # drop richness
  dplyr::select(-sppS) %>%
  # gather all anpp to take average per plot
  gather(grp, anpp_g, grass_wgt_rep1:ncol(.)) %>%
  filter(!is.na(anpp_g)) %>%
  mutate(site = "sdl",
         grp = gsub("_.+$", "", grp),
         # convert all to anpp to g/m2
         anpp_g_m2 = as.numeric(anpp_g)/area97,
         old_plot = as.numeric(plot)) %>%
  group_by(site, yr, loc, trt, old_plot, grp) %>%
  summarise(mean_anpp_g_m2 = mean(anpp_g_m2)) %>%
  ungroup() %>%
  spread(grp, mean_anpp_g_m2) %>%
  mutate(trt = recode(trt, CC = "C", NN = "N", PP = "P", NP = "N+P")) %>%
  left_join(sdlplots[c("plot", "old_plot_edi138", "trt", "meadow", "snow")], by = c("old_plot"="old_plot_edi138", "trt")) %>%
  mutate(plotid = as.character(plot))
# 283 didn't pair (should be 83 not 283 to match), matches by sdl96 column
sdl97_anpp[sdl97_anpp$old_plot == 283, c("meadow", "snow", "plot")] <- sdlplots[sdlplots$old_plot96 == 283 & !is.na(sdlplots$old_plot96), c("meadow", "snow", "plot")]

# stack anpp then join to coarse summary
anpp_stack <- rbind(nn13_anpp, sdl97_anpp[colnames(nn13_anpp)]) %>%
  mutate(yr = as.numeric(yr)) %>%
  dplyr::select(-trt)

# add to summary df
coarse_summary <- left_join(coarse_summary, anpp_stack)


# -- WRITE OUT DATASETS FOR TS ----
# join site info, plant info to plantcom and coarse_summary
allsites <- dplyr::select(nnplots, -c(n,p,k)) %>%
  mutate(meadow = "dry",
         snow = "no snow",
         snow_notes = "never affected") %>%
  dplyr::select(site, plotid, block, plot, meadow, snow, snow_notes, trt, trt2)
# prep sdl to rbind
sdlsites <- mutate(sdlplots, plot = ifelse(is.na(plot), old_plot97LT, plot),
                   plotid = as.character(plot),
                   block = NA,
                   site = "sdl",
                   trt2 = trt) %>%
  dplyr::select(names(allsites))
allsites <- rbind(allsites, sdlsites) %>% distinct()

# specify spp info cols to keep
plantcomTS <- left_join(plantcom, allsites) %>%
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  dplyr::select(site:plotid, block:trt2,clean_code2, hits, simple_name,
                Scientific_Name_x:Growth_Habit, simple_lifeform, simple_lifeform2)
  

coarse_summaryTS <- right_join(allsites, coarse_summary) %>%
  arrange(site, yr, block, plot) %>%
  dplyr::select(site, yr, plotid:ncol(.)) %>%
  rename_at(vars("Forb", "Grass", "NonVeg", "Shrub"), function(x) paste0(x, "Hits")) %>%
  rename_at(vars("forb", "grass", "total"), function(x) str_to_sentence(paste0(x, "_g_m2")))

glimpse(coarse_summaryTS)
glimpse(plantcomTS)


# write out
# write_csv(plantcomTS, "alpine_addnuts/output_data/forTS/sdl_nutnet_sppcomp_1997-ongoing.csv")
# write_csv(coarse_summaryTS, "alpine_addnuts/output_data/forTS/sdl_nutnet_fxnl_biodiv_anpp_1997-ongoing.csv")


# -- Fig 1 and 2: FORBS VS GRASSES (Figs 1 + 2) ----
# (out of curiosity make similar time since exp onset plot to compare forb shift over time by site by trt)
# split out sdl and nutnet so can average C and N+P+K
sdl_coarse <- subset(coarse_summary, site == "sdl") %>%
  # select only dry meadow plots consistently sampled
  # subset to dry meadow plots surveyed across all years only (corresponds to what was surveyed in 1997)
  ## should be plots 1-16
  subset(plotid %in% sdl_common) %>%
  # join site info
  ## convert plotid from char to numeric to join with sdlplots df (nutnet has alpha chars in plotid)
  mutate(plotid = as.numeric(plotid)) %>%
  arrange(yr, plotid) %>%
  left_join(unique(sdlplots[c("plot", "trt", "meadow", "snow")]), by = c("plotid" = "plot")) %>%
  # make treatment a factor
  mutate(trt = factor(trt, levels = c("C", "N", "P", "N+P"))) %>%
  dplyr::select(site, yr, plotid, trt:snow, Forb:ncol(.))

# stack for anova
sdl_coarse_tall <- gather(sdl_coarse, met, val, Forb: ncol(sdl_coarse))

# run anova
fg_anova_global <- aov(val ~ trt * met * yr, data = subset(sdl_coarse_tall, met %in% c("Forb_rel", "Grass_rel")))
summary(fg_anova_global)
TukeyHSD(fg_anova_global)

fg_anova97 <- aov(val ~ trt * met, data = subset(sdl_coarse_tall, met %in% c("Forb_rel", "Grass_rel") & yr == 1997))
fg_anova03 <- aov(val ~ trt * met, data = subset(sdl_coarse_tall, met %in% c("Forb_rel", "Grass_rel") & yr == 2003))
fg_anova05 <- aov(val ~ trt * met, data = subset(sdl_coarse_tall, met %in% c("Forb_rel", "Grass_rel") & yr == 2005))
fg_anova12 <- aov(val ~ trt * met, data = subset(sdl_coarse_tall, met %in% c("Forb_rel", "Grass_rel") & yr == 2012))
fg_anova16 <- aov(val ~ trt * met, data = subset(sdl_coarse_tall, met %in% c("Forb_rel", "Grass_rel") & yr == 2016))

summary(fg_anova97)
TukeyHSD(fg_anova97)
summary(fg_anova03)
TukeyHSD(fg_anova03)
summary(fg_anova05)
TukeyHSD(fg_anova05)
summary(fg_anova12)
TukeyHSD(fg_anova12)
summary(fg_anova16)
TukeyHSD(fg_anova16)

# average by treatment
sdl_coarse_means <- dplyr::select(sdl_coarse, site, yr, plotid, trt, meadow, snow, Forb_rel, Grass_rel, Geum_rel) %>%
  gather(grp, relcov, Forb_rel:Geum_rel) %>%
  group_by(site, yr, trt, meadow, snow, grp) %>%
  summarise(meancov = mean(relcov),
            secov = sd(relcov)/sqrt(length(relcov)),
            nobs = length(relcov)) %>%
  ungroup() %>%
  # calculate yrs passed since start of exp
  mutate(post_yrs = yr - 1993)
sdl_coarse_means

# visualize
ggplot(data= subset(sdl_coarse_means, grp != "Geum_rel"), aes(trt, meancov, fill = grp)) +
  geom_errorbar(aes(ymax = meancov + secov, ymin = meancov-secov, col = grp), width = 0.1, position = position_dodge(width = 0.6)) +
  geom_col(color = "grey30", position = position_dodge(width = 0.6), width = 0.5) +
  scale_color_grey() +
  scale_fill_grey() +
  facet_wrap(~yr, nrow = 2)


# nutnet
## need to average N+P and C by block first (2 reps per block)
nn_coarse_blockmeans <- subset(coarse_summary, site == "nutnet") %>% 
  # select only plots consistently sampled both years
  #subset(plotid %in% c(nn_common, nn_Pplots)) %>% 
  # join site data
  left_join(nnplots) %>%
  # modify N+K plot to N in 2017 so block 4 has a 4th N trtment
  mutate(trt = ifelse(yr == 2017 & grepl("B4", plotid) & trt == "N+K", "N", trt)) %>%
  gather(met, val, Forb:Geum_rel) %>%
  # average reps in C and N+P+K by block
  group_by(site, yr, block, trt, trt2, met) %>%
  summarise(blockmean = mean(val),
            # to verify 2 obs per C and N+P+K per block
            nobs = length(val)) %>% # yes (checked manually)
  ungroup()

# subset to just C, N and N+P for fig2 and anova
nn_fg_dat <- subset(nn_coarse_blockmeans, met %in% c("Forb_rel", "Grass_rel", "Geum_rel") & trt %in% c("C", "N", "P", "N+P")) %>%
  mutate(trt2 = factor(trt2, levels = c("C", "N", "P", "N+P")))

# run anova of forbs and grass rel cov, using C, N and N+P only (so 4 reps per trt) [i.e. ignore K, N+P+K]
nn_fg_anova_global <- aov(blockmean ~ trt2 * met * yr, data = subset(nn_fg_dat, met != "Geum_rel" & trt2 != "P"))
summary(nn_fg_anova_global)
TukeyHSD(nn_fg_anova_global)

nn_fg_anova13 <- aov(blockmean ~ trt2 * met, data = subset(nn_fg_dat, yr == 2013 & met != "Geum_rel"))
nn_fg_anova17 <- aov(blockmean ~ trt2 * met, data = subset(nn_fg_dat, yr == 2017 & met != "Geum_rel" & trt2 != "P"))

summary(nn_fg_anova13)
TukeyHSD(nn_fg_anova13)
summary(nn_fg_anova17)
TukeyHSD(nn_fg_anova17)

# calculate table of means and ses
nn_fg_means <- data.frame(nn_fg_dat) %>%
  group_by(site, yr, trt2, met) %>%
  summarise(meancov = mean(blockmean),
            secov = sd(blockmean)/sqrt(length(blockmean)),
            nobs = length(blockmean)) %>%
  ungroup() %>%
  # calculate yrs passed since start of exp
  mutate(post_yrs = yr - 2008) %>%
  as.data.frame()
nn_fg_means

# visualize
ggplot(data= subset(nn_fg_means, met != "Geum_rel"), aes(trt2, meancov, fill = met)) +
  geom_errorbar(aes(ymax = meancov + secov, ymin = meancov-secov, col = met), width = 0.1, position = position_dodge(width = 0.6)) +
  geom_col(color = "grey30", position = position_dodge(width = 0.6), width = 0.5) +
  scale_color_grey() +
  scale_fill_grey() +
  facet_wrap(~yr, nrow = 3)

# stack sdl and nutnet to plot means and ses by yr since trts initiated
all_fg_means <- rename(nn_fg_means, trt = trt2, grp = met) %>%
  rbind(dplyr::select(sdl_coarse_means, -c(meadow, snow)))


# plot means by time since experiment onset
mutate(all_fg_means, grp = factor(grp, levels = c("Forb_rel", "Grass_rel", "Geum_rel"), labels = c("All forbs", "All grasses", "G. rossii"))) %>%
  ggplot(aes(post_yrs, meancov, col = site)) +
  geom_errorbar(aes(ymax = meancov + secov, ymin = meancov - secov), width = 0) +
  geom_point() +
  #geom_line(aes(col = site)) +
  scale_color_manual(name = "Site", values = c("salmon", "darkred")) +
  labs(y = "Mean relative cover (%)",
       x = "Years since experiment onset") +
  #scale_y_continuous(breaks = seq(20,80, 10)) +
  facet_grid(grp~trt, scales = "free_y")


# -- Table 3: Relative cover of G. rossii by study by year ----
# enter sdl data for 1994 and 2005
sdl_geum_9405 <- data.frame(cbind(site = "sdl",
                                  trt = rep(c("C", "N", "P", "N+P"), 2),
                                  yr = c(rep(1994,4), rep(2005,4)),
                                  meancov = c(3.1, 3.3, 2.0, 3.8, 
                                              9.8, 11.5, 4.5, 14.2),
                                  secov = c(0.26, 1.22, 0.98, 1.16,
                                            2.09, 2.95, 1.93, 5.23),
                                  nobs = NA)) %>%
  mutate(yr = as.numeric(yr),
         post_yrs = yr-1993)

# can remake this using sdl coarse means and nn coarse means, remove 2005 from sdl_geum9405
# all_geum_means <- rbind(all_geum_means, sdl_geum_9405) %>%
#   # convert trt to factor
#   mutate(trt = factor(trt, levels = c("C", "N", "P", "N+P"))) %>%
#   mutate_at(vars("meancov", "secov", "nobs"), as.numeric)
# 
# # plot means by time since experiment onset
# ggplot(all_means, aes(post_yrs, meancov, group = site, col = site)) +
#   geom_errorbar(aes(ymax = meancov + secov, ymin = meancov - secov), width = 0) +
#   geom_point(aes(col = site)) +
#   #geom_line(aes(col = site)) +
#   scale_color_manual(name = "Site", values = c("salmon", "darkred")) +
#   labs(y = "Geum rossi mean relative cover (%)",
#        x = "Years since experiment onset") +
#   facet_wrap(~trt, nrow = 1)



# -- Table 1: NutNet 2013 biodiversity, biomass, and vertical complexity ----
# only plots used in 2017??, Tim pooled +micro additions with otherwise similar (c, c+micro)
# K+ only added in 2016, not added for 2013 surveys.. so in 2013 +micro is just +micro, and in 2017 those +micro plots are +micro+K+ (K as K2SO4)
# the way to get Tim's table results are Selaginella = non-plant -- but everywhere else Selaginella is plant, so updating 2013 with selaginella as plant cover



# collapse K (micro) into otherwise similar treatments (n = 8 per group)
nn_biodiv_blockmeans <- subset(biodiv, site == "nutnet") %>%
  # join biomass data
  left_join(nn13_anpp) %>%
  # join site data
  left_join(nnplots) %>%
  gather(met, val, S:H, forb:total) %>%
  # average reps in C and N+P+K by block
  group_by(site, yr, block, trt, trt2, met) %>%
  summarise(blockmean = mean(val),
            # to verify 2 obs per C and N+P+K per block
            nobs = length(val)) %>% # yes (checked manually)
  ungroup() %>%
  # append abundance-based bockmeans (2013 and 2017)
  rbind(nn_coarse_blockmeans) 


# summarise 2013
nn_biodiv13_means <- subset(nn_biodiv_blockmeans, yr == 2013) %>%
  group_by(site, yr, trt2, met) %>%
  summarise(meancov = mean(blockmean),
            secov = sd(blockmean)/sqrt(length(blockmean)),
            nobs = length(blockmean)) %>%
  ungroup() %>%
  # calculate yrs passed since start of exp
  mutate(post_yrs = yr - 2008) %>%
  as.data.frame()

# visualize
ggplot(nn_biodiv13_means, aes(trt2, meancov)) +
  geom_errorbar(aes(ymax = secov + meancov, ymin = meancov - secov), width = 0.1) +
  geom_point() +
  facet_wrap(~met, scales = "free_y")

# -- Table 2: NutNet 2017 biodiversity and vertical complexity ----
# no anpp for nutnet 2017
# can, for now, calculate total cover, all hits
# set up richness and diversity

unique(nn17_wvert$hit[nn17_wvert$fxnl_grp == "Ground cover"]) # ground cover only recorded if top hit
unique(nn17_wvert$hit[nn17_wvert$clean_code2 == "SEDES"]) # Selaginella recorded at multiple heights
# how many times is SEDES not top hit?
nn17_wvert$hit[nn17_wvert$clean_code2 == "SEDES" & nn17_wvert$hit > 1] #5 times.. could always remove from below
# is anything below SEDES? (don't expect yes)
sedes_plots <- distinct(nn17_wvert[nn17_wvert$clean_code2 == "SEDES" & nn17_wvert$hit == 1, c("plotid", "point")])
View(left_join(sedes_plots, nn17_wvert))
# 4 times: grasses recorded under SEDES

# i think it can work either way (SEDES as ground cover or not, just need to pick an option and stick with it)
# for now, counting SEDES as plant cover


nn17_nonveg <- subset(nn17_wvert, fxnl_grp == "Ground cover") %>%
  group_by(yr, plotid) %>%
  summarise(AllHits = sum(hit),
            nobs = length(unique(clean_code2))) %>%
  ungroup() %>%
  mutate(plant_cover = 100-AllHits)

nn17_biodiv <- nn17_wvert %>%
  mutate(CoverType = ifelse(fxnl_grp == "Ground cover", "NonVascular", "Vascular"),
         CoverType2 = ifelse(Code == "SEDES", "NonVascular", CoverType)) %>%
  group_by(yr, Block, Plot, N, P, `K.`, FullTreatment, CoverType) %>%
  summarise(AllHits = sum(Hits[Hits>0.25]), # don't count species present
            S = length(unique(clean_code2))) %>% # do count species present
  ungroup() %>%
  gather(met, val, AllHits, S) %>%
  unite(cat, CoverType, met) %>%
  spread(cat, val, fill = 0) %>%
  mutate(Plant_cover = 100-NonVascular_AllHits)

sort(unique(nn13_clean$USDA_Symbol[nn13_clean$Block == 4 & nn13_clean$Plot == 6 & nn13_clean$Hits >= 1]))
sort(unique(nn17_wvert$clean_code2[nn17_wvert$plotid == "B4_6"]))
sort(unique(nn13_clean$USDA_Symbol[nn13_clean$Block == 4 & nn13_clean$Plot == 7 & nn13_clean$Hits >= 1]))



# -- ABUNDANT FORBS IN NUTNET 2017 (Table 4) -----


