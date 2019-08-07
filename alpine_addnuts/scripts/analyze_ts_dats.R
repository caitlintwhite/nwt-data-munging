# analyze ts dats (multivariate analysis)

# script purpose:
# read in ctw cleaned + prepped datasets:
# 1) ts nutnet + sdl plant comp tidy data
# 2) tidy site data (nutnet, sdl)
# 3) nutnet-sdl spplist
# run all plant comm in nmdsm, permanova, over time, with treatment data
# > looking for shift to forb dominance in dry meadow plots with +n+p additions
# > also geom rossii dominance in +n+p plots in sdl

# specific plots/analyses to do:
# per KNS--
## 1) NMDS: last time point, all trts, all plots, with natural log forb:grass ration env vector
## > a) Saddle
## > b) NutNet
# per TS--
## 2) NMDS: dry meadow plots commonly samples in all yrs
## > a) Saddle (16 plots)
## > b) NutNet (28 plots)
## ? can add arrow to show movement over time
# ctw ideas --
## betadisper on N+P plots (does N+P expand more than other treatments? i.e. shift more? but if richness declines maybe it wouldn't)
## indicator spp analysis on all trts (is geum rossii an indicator spp for n+p? camp rotund in NutNet N+P?)


# notes:
# ts only looked at plots 1-16 in saddle because those are the only plots that were sampled in all yrs
# kns says nutnet dry meadow plots super different from saddle dry meadow plots, not appropriate to compare

# email from ts:
# Ok…the complication is that there are 16 ‘dry’, 16 ‘mesic’ and 16 wet plots.  
# In addition, however, there also are 16 dry plots in the snow fence and 16 historically mesic sites in the snowfence.
# What’s missing is 16 wet plots in the snowfence (we couldn’t do that one given where the snow fence was placed…)
# So…a dry meadow plot in the snowfence area is a different animal than a dry meadow plot in a dry meadow.
# For the current paper, I’ve stayed out of the snowfence with all dry plots.
# 


# for indicator analysis...
# (from indicspecies vignettes by Caceres)
# A = prob. that site belongs to that target site given species x found there (i.e. specificity or positive predictive value of species as indicator to that site group)
# B = prob. of finding the species in sites belonging to that site group (i.e. fidelity or sensitivity of the species to that site group)




# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(vegan)
library(indicspecies)
#library(ggvegan)# devtools::install_github("gavinsimpson/ggvegan")
library(cowplot)
options(stringsAsFactors = F)
theme_set(theme_bw())
na_vals <- c("", " ", NA, "NA", "NaN", NaN, ".")
source("edi_functions.R")


# get data
# plant community data for sdl and nutnet, all yrs
plantcom <- read.csv("alpine_addnuts/output_data/sdl_nutnet_plantcom_allyrs.csv") 
# spp lookup table
spplist <- read.csv("alpine_addnuts/output_data/sdl_nutnet_spplookup.csv")
# sdl site info
sdlplots <- read.csv("alpine_addnuts/output_data/sdl_plot_lookup.csv") 
nnplots <- read.csv("alpine_addnuts/output_data/nutnet_plot_lookup.csv")

# full sdl site info table (troublshooting discrpancies, but good to use for plots 1-16 sampled in 1997-2016 in sdl)
sdlplots_full <- read.csv("alpine_addnuts/output_data/sdl_plots_lookup_trblshoot.csv")


# marko and soren's trait dataset -- will just consider saddle spp
traitdat <- getTabular(500) %>% data.frame()



# -- REVIEW DATA -----
glimpse(plantcom)
glimpse(spplist)
glimpse(sdlplots)
glimpse(nnplots)
glimpse(sdlplots_full)
# all looks fine.. "control" in sdl site info not coded similarly as "C" in nutnet site info

# trait data
glimpse(traitdat)



# -- ADD SIMPLE LIFEFORM TO SPPLIST ----
#make simple lifeform grp
spplist <- spplist %>%
  mutate(simple_lifeform = ifelse(Family == "Fabaceae", "N-fixer",
                                ifelse(grepl("Shrub", Growth_Habit), "Shrub",
                                       ifelse(Growth_Habit == "Graminoid", "Grass", "Forb")))) %>%
  # fill in simple lifeform for 2FORB and 2GRAM
  mutate(simple_lifeform = ifelse(clean_code2 == "2FORB", "Forb",
                                  ifelse(clean_code2 == "2GRAM", "Grass", simple_lifeform)))



# -- PREP FUNCTIONAL GROUPING ABUNDANCE DATA FRAME -----
plantcom_fg <- plantcom %>%
  #join spp info
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  #drop non-veg
  filter(!is.na(simple_lifeform)) %>%
  filter(hits >= 1) %>%
  # calculate total simple lifeform hits by plot by yr
  group_by(site, yr, plotid, simple_lifeform) %>%
  summarise(hits = sum(hits)) %>%
  ungroup() %>%
  #spread out total hits by lifeform to calculate ratios
  spread(simple_lifeform, hits, fill = 0)
# add total veg hits, forb:grass ratio, and ln (f2g)
plantcom_fg$veghits <- apply(plantcom_fg[,grep("Forb", colnames(plantcom_fg)):ncol(plantcom_fg)], 1, sum)
plantcom_fg$f2g <- plantcom_fg$Forb/plantcom_fg$Grass
plantcom_fg$lnf2g <- log(plantcom_fg$f2g)

  
# -- SUBSET TS SUMMARIZED NN 2017 DATA FOR MISSING PLOT ----
# just need block 3 plot 7 from 2017
# treat abs_cov as tothits
b3p7 <- subset(nn17sum, block == 3 & plot == 7) %>%
  #trim any whitespace in spp codes
  mutate(species = trimws(species)) %>%
  # append correct code names
  left_join(spplist, by = c("species" = "code")) %>%
  # add site, yr, and plotid cols to be consistent with other datasets
  mutate(site = "nutnet",
         yr = 2017,
         plotid = paste0("B", block, "_", plot)) %>%
  # retain same cols as in main plantcom dataset + simple_lifeform and sum rel cov by species codes to be sure
  group_by(site, yr, plotid, clean_code2, simple_lifeform) %>%
  summarise(hits = sum(abs_cov)) %>%
  ungroup() %>%
  # remove any spp not present
  filter(hits > 0) 

# check relcov sums to 100..
sapply(b3p7[c("rel_cov", "abs_cov")], sum) #rel cov > 100.. need to re-relativize
b3p7$rel_cov <- (b3p7$abs_cov/(sum(b3p7$abs_cov))) *100
sum(b3p7$rel_cov) #good

# summarize by functional group & append to fg dataset
b3p7_fg <- b3p7 %>%
  group_by(site, yr, plotid, simple_lifeform) %>%
  summarise(hits = sum(hits)) %>%
  ungroup()
  
# calculate forb:grass ratio
b3p7_fg$f2g <- b3p7_fg$rel_cov[b3p7_fg$simple_lifeform == "Forb"]/b3p7_fg$rel_cov[b3p7_fg$simple_lifeform == "Forb"]
  

# -- EXPLORE DIFFERENT NMDS VARIATIONS -----
# KNS: Last point in time plots (2016 sdl, 2017 nutnet)

# -- Matrix 1: 2016 Saddle plots, last time point sampled -----
matrix1 <- subset(plantcom, yr == 2016 & site == "sdl") %>%
  # keep only plots that were never in snowfence area
  filter(plotid %in% unique(sdlplots$plot[sdlplots$notes2016 == "never affected"])) %>%
  # remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

# calculate forb to grass ratio
fgrat1 <- subset(plantcom_fg, plotid %in% matrix1$plotid & yr == 2016) %>%
  # coerce plotid to number to joins with sitematrix
  mutate(plotid = as.numeric(plotid))
  

sitematrix1 <- matrix1[,1:4] %>%
  mutate(plotid = as.numeric(plotid)) %>%
  left_join(distinct(sdlplots[colnames(sdlplots) != "old_plot"]), by = c("plotid" = "plot")) %>%
  # join grass forb ratio
  left_join(fgrat1[c("site", "yr", "plotid", "f2g", "lnf2g")])
sitematrix1$trt <- factor(sitematrix1$trt, levels = c("C", "N", "P", "N+P"))
sitematrix1$meadow_alt <- as.factor(sitematrix1$meadow_alt) #dry = 1, mesic = 2

row.names(matrix1) <- matrix1$rowid
matrix1 <- matrix1[!colnames(matrix1) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix1_rel <- decostand(matrix1, method = "total")

# run nmds
nmds1 <- metaMDS(matrix1_rel, k = 2, trymax = 50)
nmds1
plot(nmds1, type = "t")

# add environmental fit of forb to grass ratio
fit1 <- envfit(nmds1, sitematrix1[c("trt", "meadow_alt", "lnf2g")], perm = 999)
fit1 # meadow p << 0.001, trt < 0.05 (but isn't if specify trt as strata.. not sure if should be strata?)
# f2g ratio not signif when all lumped, or when stratified by treatment

ordiplot(nmds1, type="n", main = "Saddle 2016, all treatments, no snowfence influence")
with (sitematrix1, ordiellipse(nmds1, trt, kind="se", conf=0.95, col=1:4, lwd = 2))
with (sitematrix1, ordiellipse(nmds1, meadow_alt, kind="se", conf=0.95, col="dodgerblue4", lwd=2))
plot(fit1, col = 1:6)
orditorp (nmds1, display="species", col="black", air=0.01)

# plot forb:grass by trtment
sdlboxplot <- ggplot(sitematrix1, aes(trt, lnf2g, col = meadow_alt)) +
  geom_boxplot() +
  # dry pts with red color
  geom_point(data = subset(sitematrix1, meadow_alt =="Dry"), aes(trt, lnf2g), 
             alpha = 0.5, col = "#F8766D", position = position_nudge(x = -0.19)) +
  # mesic point with blue color
  geom_point(data = subset(sitematrix1, meadow_alt =="Mesic"), aes(trt, lnf2g), 
             alpha = 0.5, col = "#00BFC4", position = position_nudge(x = 0.19)) +
  scale_color_discrete(name = "Meadow") +
  labs(y = "ln Forb hits/Grass hits") +
  ggtitle("Saddle 2016")+
  theme(legend.position = c(0.35,0.8),
        legend.background = element_blank())


# try indicator species analysis to look for geum rossii in n+p plots
sdl_ind = multipatt(matrix1_rel, sitematrix1$trt, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(sdl_ind, alpha = 0.1, indvalcomp = TRUE)
# are any significant?
subset(sdl_ind$sign, p.adjust(sdl_ind$sign$p.value, method = "holm") < 0.1) #only CASC12 in N+P
# try combining trt and meadow to make more explicit
sitematrix1$group = paste(sitematrix1$meadow_alt, sitematrix1$trt)
sdl_ind_comb = multipatt(matrix1_rel, sitematrix1$group, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(sdl_ind_comb, alpha = 0.1, indvalcomp = TRUE)
# are any significant with p-vals corrected for multiple testing?
subset(sdl_ind_comb$sign, p.adjust(sdl_ind_comb$sign$p.value, method = "holm") < 0.1) # only CASC12 in Mesic N+P (but depends on permutations, sometimes is, sometimes isn't)

# test species associations (correlations) with certain habitats
# transform abundance to PA
matrix1_pa = as.data.frame(ifelse(matrix1>0,1,0))
# trt only
matrix1_phi = multipatt(matrix1_pa, sitematrix1$trt, func = "r.g", duleg = T, control = how(nperm=999))
summary(matrix1_phi, alpha = 0.1, indvalcomp = T)
summary(p.adjust(matrix1_phi$sign$p.value, method = "holm") < 0.1) #nothing signif, 1 spp is a generalist/no site pref (NA val)
# trt x meadow
matrix1_phi_comb = multipatt(matrix1_pa, sitematrix1$group, func = "r.g", duleg = T, control = how(nperm=999))
summary(matrix1_phi_comb, alpha = 0.1, indvalcomp = T)
summary(p.adjust(matrix1_phi_comb$sign$p.value, method = "holm") < 0.1) #nothing signif, 1 spp is a generalist/no site pref (NA val)
# who is the generalist?
rownames(matrix1_phi_comb$sign)[is.na(p.adjust(matrix1_phi_comb$sign$p.value, method = "holm"))] # Geum rossii! (!!)
# > acoros has no site preference despite increase in dry meadow n+p
# > maybe n+p in dry allowed it to move in when was previously limited there by enviro factors??


# analysis of similiarities/mrpp/permanova
## calculate CB distance
matrix1_rel_bray <- vegdist(matrix1_rel)
summary(anosim(matrix1_rel_bray, grouping = sitematrix1$trt, strata = sitematrix1$meadow_alt, permutations = 999))
mrpp(matrix1_rel,  grouping = sitematrix1$trt, strata = sitematrix1$meadow_alt, distance = "bray")
adonis(matrix1_rel ~ trt * meadow_alt * lnf2g, data = sitematrix1, permutations = 999, method = "bray")
# change order of variables
adonis(matrix1_rel ~ trt * lnf2g * meadow_alt, data = sitematrix1, permutations = 999, method = "bray")
adonis(matrix1_rel ~ lnf2g * trt * meadow_alt, data = sitematrix1, permutations = 999, method = "bray")


# last yr, dry meadow plots only
matrix1_dry_rel <- matrix1_rel[which(sitematrix1$meadow_alt == "Dry"),]
# remove any spp that aren't present in any plot
matrix1_dry_rel <- matrix1_dry_rel[,apply(matrix1_dry_rel, 2, sum) > 0]
nmds1_dry <- metaMDS(matrix1_dry_rel, k = 2, trymax = 50)
plot(nmds1_dry, type = "t")

fit1_dry <- envfit(nmds1_dry, sitematrix1[which(sitematrix1$meadow_alt == "Dry"),c("trt","lnf2g")], perm = 999)
fit1_dry #trt not signif if trt = strata, but is signif if treatment not strata (don't think it should bc trt is the experimental condition applied, not secondary grouping factor)
#f2g signif

ordiplot(nmds1_dry, type="n", main = "Saddle 2016, dry meadow only, no snowfence, all treatments")
with (sitematrix1[which(sitematrix1$meadow_alt == "Dry"),], 
      ordiellipse(nmds1_dry, trt, kind="se", conf=0.95, col=1:4))
with(sitematrix1[which(sitematrix1$meadow_alt == "Dry"),], 
      ordisurf(nmds1_dry, lnf2g, col="grey50", add = TRUE))
plot(fit1_dry, col = 1:5)
orditorp (nmds1_dry, display="species", col="black", air=0.01)

# indicator species analysis on dry meadow plots only
sdl_ind_dry = multipatt(matrix1_dry_rel, sitematrix1$trt[sitematrix1$meadow_alt == "Dry"], 
                        func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(sdl_ind_dry, indvalcomp = T)
# any significant with adjusted p vals?
summary(p.adjust(sdl_ind_dry$sign$p.value, method = "holm")< 0.1) #nothing significant

# same results with spp associations?
matrix1_rel_dry_pa <- as.data.frame(ifelse(matrix1_dry_rel>0, 1,0))
matrix1_phi_dry = multipatt(matrix1_rel_dry_pa, sitematrix1$trt[sitematrix1$meadow_alt == "Dry"], 
                        func = "r.g", control = how(nperm = 999))
summary(matrix1_phi_dry, indvalcomp = T) # nothing prefers any treatment, but there are species that occur in a combination of treatments
# any significant with adjusted p vals?
summary(p.adjust(matrix1_phi_dry$sign$p.value, method = "holm") <= 0.1) #CARUD signficant marginally (0.1, but not at 0.05 )
# who is the generalist?
subset(matrix1_phi_dry$sign, is.na(matrix1_phi_dry$sign$p.value))
# Carex rupestris is the only signif species, prefers dry meadow Control or +N (doesn't like +P?)
subset(matrix1_phi_dry$sign, p.adjust(matrix1_phi_dry$sign$p.value, method = "holm") < 0.1)


#last year, all non-dry meadow
matrix1_wet_rel <- matrix1_rel[which(sitematrix1$meadow_alt != "Dry"),]
# remove any spp that don't occur in the wet sites
matrix1_wet_rel <- matrix1_wet_rel[,apply(matrix1_wet_rel, 2, sum)>0]
nmds1_wet <- metaMDS(matrix1_wet_rel, k = 2, trymax = 50)
plot(nmds1_wet, type = "t")

fit1_wet <- envfit(nmds1_wet, sitematrix1[which(sitematrix1$meadow_alt != "Dry"),c("trt","lnf2g")], perm = 999)
fit1_wet #trt signif, forb:grass signif

ordiplot(nmds1_wet, type="n", main = "Saddle 2016, mesic only, no snowfence plots, all treatments")
with (sitematrix1[which(sitematrix1$meadow_alt != "Dry"),], 
      ordiellipse(nmds1_wet, trt, kind="se", conf=0.95, col=1:4))
with(sitematrix1[which(sitematrix1$meadow_alt != "Dry"),], 
     ordisurf(nmds1_wet, lnf2g, col="grey50", add = TRUE))
plot(fit1_wet, col = 1:5)
orditorp (nmds1_wet, display="species", col="grey30", air=0.01)

# indicator species
sdl_ind_wet = multipatt(matrix1_wet_rel, sitematrix1$trt[sitematrix1$meadow_alt == "Mesic"], 
                        func = "IndVal.g", duleg = T, control = how(nperm = 2000))
summary(sdl_ind_wet, alpha = 0.1, indvalcomp = T)
summary(p.adjust(sdl_ind_wet$sign$p.value, method = "holm")<0.1)
#Carex scopulorum indicates n+p in mesic (can vary by number of permutations, sometimes is signif, sometimes not)
subset(sdl_ind_wet$sign, p.adjust(sdl_ind_wet$sign$p.value, method = "holm")<0.1)



# -- Matrix 2: NutNet last time point only -----
matrix2 <- subset(plantcom, yr == 2017 & site == "nutnet") %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

# calculate forb to grass ratio
fgrat2 <- subset(plantcom_fg, plotid %in% matrix2$plotid & yr == 2017)

sitematrix2 <- matrix2[,1:4] %>%
  left_join(nnplots) %>%
  # make trtment factor
  mutate(trt = factor(trt, levels = c("C", "K", "N", "N+K", "N+P", "N+P+K"))) %>%
  left_join(fgrat2)

row.names(matrix2) <- matrix2$rowid
matrix2 <- matrix2[!colnames(matrix2) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix2_rel <- vegan::decostand(matrix2, method = "total")

# run nmds
nmds2 <- metaMDS(matrix2_rel, k = 2, trymax = 50)
nmds2
plot(nmds2, type = "t")

# environmental fit grass to forb
fit2 <- envfit(nmds2, sitematrix2[c("trt", "lnf2g")], strata = sitematrix2$block, perm = 999)
fit2 #trt is signif, forb:grass signif

ordiplot(nmds2, type="n", main = "NutNet 2017, all treatments")
with (sitematrix2, ordiellipse(nmds2, trt, kind="se", conf=0.95, col=1:6))
with (sitematrix2, ordisurf(nmds2, lnf2g, col="grey50", add = T))
plot(fit2, col = 1:7)
orditorp (nmds2, display="species", col="grey30", air=0.01)

nnboxplot <- ggplot(sitematrix2, aes(trt, lnf2g)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5, width = 0.1) +
  labs(y = NULL) +
  ggtitle("Nutnet 2017")

# plot boxplots together for kns
plot_grid(sdlboxplot, nnboxplot,
          nrow = 1)

# analysis of similiarities/mrpp/permanova
## calculate CB distance
matrix2_rel_bray <- vegdist(matrix2_rel)
summary(anosim(matrix2_rel_bray, grouping = sitematrix2$trt, strata = sitematrix2$block, permutations = 999))
mrpp(matrix2_rel,  grouping = sitematrix2$trt, strata = sitematrix2$block, distance = "bray")
adonis(matrix2_rel ~ trt * lnf2g, data = sitematrix2, strata = sitematrix2$block, permutations = 999, method = "bray")
# change order of variables
adonis(matrix2_rel ~ lnf2g * trt, data = sitematrix2, strata = sitematrix2$block, permutations = 999, method = "bray")
# either order of explanatory vars, forb:grass ration and trtment is distinct, but there is no interaction


# indicator species for nutnet plots
# indicator species
nn2_ind = multipatt(matrix2_rel, sitematrix2$trt, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(nn2_ind, alpha = 0.1)
# check signif of adjust pvals (for multiple spp comparisons)
summary(p.adjust(nn2_ind$sign$p.value, method = "holm") < 0.1) #nothing signif

# does any spp associate with a particular treatment?
matrix2_rel_pa <- as.data.frame(ifelse(matrix2_rel>0,1,0))
nn2_phi <- multipatt(matrix2_rel_pa, sitematrix2$trt, duleg = T, func = "r.g", control = how(nperm = 999))
summary(nn2_phi)
# check signif for multiple spp comparisons
summary(p.adjust(nn2_phi$sign$p.value, method = "holm")<0.1) #nada
#check with combos allowed
nn2_phi_comb <- multipatt(matrix2_rel_pa, sitematrix2$trt, func = "r.g", control = how(nperm = 999))
summary(nn2_phi_comb)
# check signif for multiple spp comparisons
subset(nn2_phi_comb$sign, p.adjust(nn2_phi_comb$sign$p.value, method = "holm")<0.1) # trispi marginally signif for +n+k+p, but not at 0.05




# -- TS ANALYSIS: ALL YEARS, ONLY PLOTS COMMONLY SAMPLED ACROSS ALL -----
# id common plots
sdlcommon <- with(plantcom, plantcom[site == "sdl", c("plotid", "yr")]) %>% 
  distinct() %>%
  group_by(plotid) %>%
  summarise(nobs = length(yr)) %>%
  ungroup() %>%
  subset(nobs == 3)

nncommon <- subset(plantcom, site == "nutnet") %>%
  dplyr::select(plotid, yr) %>%
  distinct() %>%
  group_by(plotid) %>%
  summarise(nobs = length(yr)) %>%
  ungroup() %>%
  subset(nobs == 2)


# -- Matrix 3: Saddle 1997 -----
matrix3 <- subset(plantcom, yr == 1997 & plotid %in%  sdlcommon$plotid) %>%
  # remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame() %>%
  # coerce plotid to numeric
  mutate(plotid = as.numeric(plotid)) %>%
  arrange(plotid)

# calculate forb to grass ratio
fgrat3 <- subset(plantcom_fg, plotid %in% matrix3$plotid & yr == 1997) %>%
  # coerce plotid to number to joins with sitematrix
  mutate(plotid = as.numeric(plotid))


sitematrix3 <- matrix3[,1:4] %>%
  mutate(plotid = as.numeric(plotid)) %>%
  left_join(distinct(sdlplots[colnames(sdlplots) != "old_plot"]), by = c("plotid" = "plot")) %>%
  # join grass forb ratio
  left_join(fgrat3[c("site", "yr", "plotid", "f2g", "lnf2g")])
sitematrix3$trt <- factor(sitematrix3$trt, levels = c("C", "N", "P", "N+P"))


row.names(matrix3) <- matrix3$rowid
matrix3 <- matrix3[!colnames(matrix3) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix3_rel <- decostand(matrix3, method = "total")

# run nmds
nmds3 <- metaMDS(matrix3_rel, k = 2, trymax = 50)
nmds3
stressplot(nmds3)
plot(nmds3, type = "t")

# add environmental fit of forb to grass ratio
fit3 <- envfit(nmds3, sitematrix3[c("trt", "lnf2g")], perm = 999)
fit3 # trt not signif, f2g is..

ordiplot(nmds3, type="n", main = "Saddle 1997, dry meadow plots 1-16")
with (sitematrix3, ordiellipse(nmds3, trt, kind="se", conf=0.95, col=1:4, lwd = 2))
with (sitematrix3, ordisurf(nmds3, lnf2g, col = "grey50", add = T))
plot(fit3, col = 1:5)
orditorp (nmds3, display="species", col="grey30", air=0.01)


# dissimilarity analyses (anosim, mrpp, permanova)
# analysis of similiarities/mrpp/permanova
## calculate CB distance
matrix3_rel_bray <- vegdist(matrix3_rel)
summary(anosim(matrix3_rel_bray, grouping = sitematrix3$trt, permutations = 999))
mrpp(matrix3_rel,  grouping = sitematrix3$trt, distance = "bray")
adonis(matrix3_rel ~ trt * lnf2g, data = sitematrix3, permutations = 999, method = "bray")
# change order of variables
adonis(matrix3_rel ~ lnf2g * trt, data = sitematrix3, permutations = 999, method = "bray")
# either order of explanatory vars, forb:grass ratio and treattment is signif, but there is no interaction


# indicator species analysis for 1997 saddle plots
# indicator species
matrix3_ind = multipatt(matrix3_rel, sitematrix3$trt, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(matrix3_ind, alpha = 0.1)
# check signif of adjust pvals (for multiple spp comparisons)
subset(matrix3_ind$sign, p.adjust(matrix3_ind$sign$p.value, method = "holm") < 0.1) #CARUD in control sometimes signif depending on permutations

# does any spp associate with a particular treatment?
matrix3_rel_pa <- as.data.frame(ifelse(matrix3_rel>0,1,0))
matrix3_phi <- multipatt(matrix3_rel_pa, sitematrix3$trt, duleg = T, func = "r.g", control = how(nperm = 999))
summary(matrix3_phi, alpha = 1) # nothing is significant

#check with trt combos allowed
matrix3_phi_comb <- multipatt(matrix3_rel_pa, sitematrix3$trt, func = "r.g", control = how(nperm = 999))
summary(matrix3_phi_comb)
# check signif for multiple spp comparisons
subset(matrix3_phi_comb$sign, p.adjust(matrix3_phi_comb$sign$p.value, method = "holm")<0.1) # nothing signif






# -- Matrix 4: Saddle 2012 dry meadow plots -----
matrix4 <- subset(plantcom, yr == 2012 & plotid %in% sdlcommon$plotid) %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame() %>%
  #coerce plotid to numeric
  mutate(plotid = as.numeric(plotid)) %>%
  arrange(plotid)

# calculate forb to grass ratio
fgrat4 <- subset(plantcom_fg, plotid %in% matrix4$plotid & yr == 2012) %>%
  # coerce plotid to number to joins with sitematrix
  mutate(plotid = as.numeric(plotid))


sitematrix4 <- matrix4[,1:4] %>%
  mutate(plotid = as.numeric(plotid)) %>%
  left_join(distinct(sdlplots[colnames(sdlplots) != "old_plot"]), by = c("plotid" = "plot")) %>%
  # join grass forb ratio
  left_join(fgrat4[c("site", "yr", "plotid", "f2g", "lnf2g")])
sitematrix4$trt <- factor(sitematrix4$trt, levels = c("C", "N", "P", "N+P"))

row.names(matrix4) <- matrix4$rowid
matrix4 <- matrix4[!colnames(matrix4) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix4_rel <- decostand(matrix4, method = "total")

# run nmds
nmds4 <- metaMDS(matrix4_rel, k = 2, trymax = 50)
nmds4
stressplot(nmds4)
plot(nmds4, type = "t")

# add environmental fit of forb to grass ratio
fit4 <- envfit(nmds4, sitematrix4[c("trt", "lnf2g")], perm = 999)
fit4 # trt p << 0.001, f2g marginally signif

ordiplot(nmds4, type="n", main = "2012 Saddle, dry meadow only")
with (sitematrix4, ordiellipse(nmds4, trt, kind="se", conf=0.95, col=1:4, lwd=2))
with (sitematrix4, ordisurf(nmds4, lnf2g, col="grey50", add = T))
plot(fit4, col = 1:5)
orditorp (nmds4, display="species", col="grey30", air=0.02)


# dissimilarity analyses (anosim, mrpp, permanova)
# analysis of similiarities/mrpp/permanova
## calculate CB distance
matrix4_rel_bray <- vegdist(matrix4_rel)
summary(anosim(matrix4_rel_bray, grouping = sitematrix4$trt, permutations = 999))
mrpp(matrix4_rel,  grouping = sitematrix4$trt, distance = "bray")
adonis(matrix4_rel ~ trt * lnf2g, data = sitematrix4, permutations = 999, method = "bray")
# change order of variables
adonis(matrix4_rel ~ lnf2g * trt, data = sitematrix4, permutations = 999, method = "bray")
# trt always signif, interactions always marginally signif, lnf2g only signif if lnf2g first

# indicator species analysis for 1997 saddle plots
# indicator species
matrix4_ind = multipatt(matrix4_rel, sitematrix4$trt, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(matrix4_ind, alpha = 0.1, indvalcomp = T)
# check signif of adjust pvals (for multiple spp comparisons)
summary(p.adjust(matrix4_ind$sign$p.value, method = "holm") < 0.1) #nothing signif

# does any spp associate with a particular treatment?
matrix4_rel_pa <- as.data.frame(ifelse(matrix4_rel>0,1,0))
matrix4_phi <- multipatt(matrix4_rel_pa, sitematrix4$trt, duleg = T, func = "r.g", control = how(nperm = 999))
summary(matrix4_phi, alpha = 0.1) # a few signif for control, marginal for P
# check signif with holm's test
# check signif for multiple spp comparisons
subset(matrix4_phi$sign, p.adjust(matrix4_phi$sign$p.value, method = "holm")<0.1) # nothing signif


#check with trt combos allowed
matrix4_phi_comb <- multipatt(matrix4_rel_pa, sitematrix4$trt, duleg = F, func = "r.g", control = how(nperm = 999))
summary(matrix4_phi_comb)
# check signif for multiple spp comparisons
subset(matrix4_phi_comb$sign, p.adjust(matrix4_phi_comb$sign$p.value, method = "holm")<0.1) # nothing signif

# no species align with any particular treatment, as a predictor or associatively 
# no spp are generalists tho (no NAs in any of the indic analysis)


# -- Matrix 6: NutNet 2013, plots from 2016 only -----
matrix6 <- subset(plantcom, yr == 2013 & plotid %in% nncommon$plotid) %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

# calculate forb to grass ratio
fgrat6 <- subset(plantcom_fg, plotid %in% matrix6$plotid & yr == 2013)

sitematrix6 <- matrix6[,1:4] %>%
  left_join(nnplots) %>%
  # make trtment factor
  mutate(trt = factor(trt, levels = c("C", "K", "N", "N+K", "N+P", "N+P+K"))) %>%
  left_join(fgrat6)

row.names(matrix6) <- matrix6$rowid
matrix6 <- matrix6[!colnames(matrix6) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix6_rel <- vegan::decostand(matrix6, method = "total")

# run nmds
nmds6 <- metaMDS(matrix6_rel, k = 2, trymax = 50)
nmds6
plot(nmds6, type = "t")

# environmental fit grass to forb
fit6 <- envfit(nmds6, sitematrix6[c("trt", "lnf2g")], strata = sitematrix6$block, perm = 999)
fit6 #trt is signif, forb:grass signif

ordiplot(nmds6, type="n", main = "NutNet 2013, all treatments")
with (sitematrix6, ordiellipse(nmds6, trt, kind="se", conf=0.95, col=1:6))
with (sitematrix6, ordisurf(nmds6, lnf2g, col="grey50", add = T))
plot(fit6, col = 1:7)
orditorp (nmds6, display="species", col="grey30", air=0.01)


# analysis of similiarities/mrpp/permanova
## calculate CB distance
matrix6_rel_bray <- vegdist(matrix6_rel)
summary(anosim(matrix6_rel_bray, grouping = sitematrix6$trt, strata = sitematrix6$block, permutations = 999))
mrpp(matrix6_rel,  grouping = sitematrix6$trt, strata = sitematrix6$block, distance = "bray")
adonis(matrix6_rel ~ trt * lnf2g, data = sitematrix6, strata = sitematrix6$block, permutations = 999, method = "bray")
# change order of variables
adonis(matrix6_rel ~ lnf2g * trt, data = sitematrix6, strata = sitematrix6$block, permutations = 999, method = "bray")
# either order of explanatory vars, forb:grass ration and trtment is distinct, but there is no interaction


# indicator species for nutnet plots
# indicator species
nn6_ind = multipatt(matrix6_rel, sitematrix6$trt, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(nn6_ind, alpha = 0.1)
# check signif of adjust pvals (for multiple spp comparisons)
summary(p.adjust(nn6_ind$sign$p.value, method = "holm") < 0.1) #nothing signif

# does any spp associate with a particular treatment?
matrix6_rel_pa <- as.data.frame(ifelse(matrix6_rel>0,1,0))
nn6_phi <- multipatt(matrix6_rel_pa, sitematrix6$trt, duleg = T, func = "r.g", control = how(nperm = 999))
summary(nn6_phi)
# check signif for multiple spp comparisons
summary(p.adjust(nn6_phi$sign$p.value, method = "holm")<0.1) #nothing signig, 2 NAs
# who are the generalists?
subset(nn6_phi$sign, is.na(nn6_phi$sign$p.value)) # carex rupestris and kobresia...
#check with combos allowed
nn6_phi_comb <- multipatt(matrix6_rel_pa, sitematrix6$trt, func = "r.g", control = how(nperm = 999))
summary(nn6_phi_comb)
# check signif for multiple spp comparisons
subset(nn6_phi_comb$sign, p.adjust(nn6_phi_comb$sign$p.value, method = "holm")<0.1) # nothing signif



# -- Visualize results -----
# specify plotting colors for all treatments
alltrts <- sort(unique(c(as.character(sitematrix2$trt), as.character(sitematrix1$trt))))
trtcols <- viridis::viridis(n = length(alltrts))
names(trtcols) <- alltrts

# specify plotting cols for lifeform
plantcols <- c("N-fixer" = "chocolate4", "Forb" = "grey30", "Grass" = "seagreen4", "Shrub" = "orchid")

# specify species text size
plottext <- 3



# last time point, sdl (2016)
plot_df1 <- data.frame(nmds1$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix1)

spp_df1 <- data.frame(nmds1$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)]))

# capture hulls
grp1.np <- plot_df1[plot_df1$trt == "N+P", ][chull(plot_df1[plot_df1$trt == "N+P", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp1.n <- plot_df1[plot_df1$trt == "N", ][chull(plot_df1[plot_df1$trt == "N", c("MDS1", "MDS2")]), ]  # hull values for grp n
grp1.p <- plot_df1[plot_df1$trt == "P", ][chull(plot_df1[plot_df1$trt == "P", c("MDS1", "MDS2")]), ]  # hull values for grp p
grp1.c <- plot_df1[plot_df1$trt == "C", ][chull(plot_df1[plot_df1$trt == "C", c("MDS1", "MDS2")]), ]  # hull values for grp control
# stack grp dfs
grpdf1 <- rbind(grp1.np, grp1.n, grp1.p, grp1.c)
grp1.dry <- plot_df1[plot_df1$meadow_alt == "Dry", ][chull(plot_df1[plot_df1$meadow_alt == "Dry", c("MDS1", "MDS2")]), ]  # hull values for dry meadow plots
grp1.other <- plot_df1[plot_df1$meadow_alt != "Dry", ][chull(plot_df1[plot_df1$meadow_alt != "Dry", c("MDS1", "MDS2")]), ]  # hull values for non-dry plots


sdlfig <- ggplot(spp_df1, aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf1, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  # geom_polygon(data = grp1.np, aes(MDS1, MDS2), alpha = 0.5, fill = "aquamarine") +
  # geom_polygon(data = grp1.n, aes(MDS1, MDS2), alpha = 0.5, fill = "steelblue2") +
  # geom_polygon(data = grp1.p, aes(MDS1, MDS2), alpha = 0.5, fill = "goldenrod") +
  # geom_polygon(data = grp1.c, aes(MDS1, MDS2), alpha = 0.5, fill = "orchid") +
  geom_polygon(data = grp1.dry, aes(MDS1, MDS2), alpha = 0.5, fill = NA, col = "black", lty = 2) +
  geom_polygon(data = grp1.other, aes(MDS1, MDS2), alpha = 0.5, fill = NA, col = "black", lty = 3) +
  #geom_text(aes(color = simple_lifeform, label = clean_code2)) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "N-fixer"), aes(label = clean_code2), col = "chocolate4", fontface = "bold", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Forb"), aes(label = clean_code2), col = "grey30", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Grass"), aes(label = clean_code2), col = "seagreen4", fontface = "bold", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Shrub"), aes(label = clean_code2), col = "orchid", fontface = "bold", size = plottext) +
  geom_point(aes(MDS1, MDS2, col = simple_lifeform), alpha = 0.6, pch = 8) +
  geom_point(data = plot_df1, aes(MDS1, MDS2,fill = trt), pch = 21) +
  #scale_color_discrete(name = "Lifeform") +
  scale_color_manual(name = "Lifeform", values = plantcols) +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols) +
  #guides(guide_legend(override.aes = list(pch =21))) +
  #scale_shape_manual(values = c("Dry" = 21, "Mesic" = 24, "Unknown" = 22)) +
  ggtitle("Saddle plots 2016") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")



# -- last time point, nutnet (2017) -------
plot_df2 <- data.frame(nmds2$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix2)
spp_df2 <- data.frame(nmds2$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)]))
plot_df2$trt <- factor(plot_df2$trt, levels = alltrts)

grp2.np <- plot_df2[plot_df2$trt == "N+P", ][chull(plot_df2[plot_df2$trt == "N+P", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp2.npk <- plot_df2[plot_df2$trt == "N+P+K", ][chull(plot_df2[plot_df2$trt == "N+P+K", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp2.nk <- plot_df2[plot_df2$trt == "N+K", ][chull(plot_df2[plot_df2$trt == "N+K", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp2.c <- plot_df2[plot_df2$trt == "C", ][chull(plot_df2[plot_df2$trt == "C", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp2.n <- plot_df2[plot_df2$trt == "N", ][chull(plot_df2[plot_df2$trt == "N", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp2.k <- plot_df2[plot_df2$trt == "K", ][chull(plot_df2[plot_df2$trt == "K", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
# stack treatments
grpdf2 <- rbind(grp2.c, grp2.n, grp2.k, grp2.nk, grp2.np, grp2.npk)

nn2017_fig <- ggplot(spp_df2, aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf2, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  # geom_polygon(data = grp2.np, aes(MDS1, MDS2), alpha = 0.5, fill = "aquamarine") +
  # geom_polygon(data = grp2.npk, aes(MDS1, MDS2), alpha = 0.5, fill = "lightgreen") +
  # geom_polygon(data = grp2.c, aes(MDS1, MDS2), alpha = 0.5, fill = "orchid") +
  # geom_polygon(data = grp2.n, aes(MDS1, MDS2), alpha = 0.5, fill = "steelblue2") +
  # geom_polygon(data = grp2.k, aes(MDS1, MDS2), alpha = 0.5, fill = "darkslateblue") +
  # geom_text(data = subset(spp_df2, simple_lifeform == "N-fixer"), aes(label = clean_code2), col = "chocolate4", fontface = "bold", size = plottext) +
  # geom_text(data = subset(spp_df2, simple_lifeform == "Forb"), aes(label = clean_code2), col = "grey30", size = plottext) +
  # geom_text(data = subset(spp_df2, simple_lifeform == "Grass"), aes(label = clean_code2), col = "seagreen4", fontface = "bold", size = plottext) +
  geom_point(aes(MDS1, MDS2, col = simple_lifeform), alpha = 0.6, pch = 8) +
  geom_point(data = plot_df2, aes(MDS1, MDS2,fill = trt), pch = 21) +
  scale_color_manual(name = "Lifeform", values = plantcols) +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols, drop = F) +
  ggtitle("NutNet plots 2017") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())



# -- sdl 2016, dry meadow only -----
plot_df1_dry <- data.frame(nmds1_dry$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix1)

spp_df1_dry <- data.frame(nmds1_dry$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)]))

# capture hulls
grpdf1_dry <- rbind(data.frame(plot_df1_dry[plot_df1$trt == "N+P", ][chull(plot_df1_dry[plot_df1_dry$trt == "N+P", c("MDS1", "MDS2")]),]),# hull values for grp n+p
  data.frame(plot_df1_dry[plot_df1_dry$trt == "N", ][chull(plot_df1_dry[plot_df1_dry$trt == "N", c("MDS1", "MDS2")]),]), # hull values for grp n
  data.frame(plot_df1_dry[plot_df1_dry$trt == "P", ][chull(plot_df1_dry[plot_df1_dry$trt == "P", c("MDS1", "MDS2")]), ]),  # hull values for grp p
  data.frame(plot_df1_dry[plot_df1_dry$trt == "C", ][chull(plot_df1_dry[plot_df1_dry$trt == "C", c("MDS1", "MDS2")]), ])) # hull values for grp control 
#fix NA row for N+P
grpdf1_dry[is.na(grpdf1_dry$MDS1),]  <- plot_df1_dry[plot_df1_dry$plotid ==9,]

sdl2016_fig <- ggplot(spp_df1_dry, aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf1_dry, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  #geom_text(aes(color = simple_lifeform, label = clean_code2)) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "N-fixer"), aes(label = clean_code2), col = "chocolate4", fontface = "bold", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Forb"), aes(label = clean_code2), col = "grey30", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Grass"), aes(label = clean_code2), col = "seagreen4", fontface = "bold", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Shrub"), aes(label = clean_code2), col = "orchid", fontface = "bold", size = plottext) +
  geom_point(aes(MDS1, MDS2, col = simple_lifeform), alpha = 0.6, pch = 8) +
  geom_point(data = plot_df1_dry, aes(MDS1, MDS2,fill = trt), pch = 21) +
  #scale_color_discrete(name = "Lifeform") +
  scale_color_manual(name = "Lifeform", values = plantcols) +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols) +
  #guides(guide_legend(override.aes = list(pch =21))) +
  #scale_shape_manual(values = c("Dry" = 21, "Mesic" = 24, "Unknown" = 22)) +
  ggtitle("Saddle plots 2016, dry meadow") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())



# -- sdl 1997, dry meadow only ----
plot_df3 <- data.frame(nmds3$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix3)

spp_df3 <- data.frame(nmds3$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)]))

# capture hulls
grpdf3 <- rbind(data.frame(plot_df3[plot_df3$trt == "N+P", ][chull(plot_df3[plot_df3$trt == "N+P", c("MDS1", "MDS2")]),]),# hull values for grp n+p
                    data.frame(plot_df3[plot_df3$trt == "N", ][chull(plot_df3[plot_df3$trt == "N", c("MDS1", "MDS2")]),]), # hull values for grp n
                    data.frame(plot_df3[plot_df3$trt == "P", ][chull(plot_df3[plot_df3$trt == "P", c("MDS1", "MDS2")]), ]),  # hull values for grp p
                    data.frame(plot_df3[plot_df3$trt == "C", ][chull(plot_df3[plot_df3$trt == "C", c("MDS1", "MDS2")]), ])) # hull values for grp control 
   

sdl1997_fig <- ggplot(spp_df3, aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf3, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  #geom_text(aes(color = simple_lifeform, label = clean_code2)) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "N-fixer"), aes(label = clean_code2), col = "chocolate4", fontface = "bold", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Forb"), aes(label = clean_code2), col = "grey30", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Grass"), aes(label = clean_code2), col = "seagreen4", fontface = "bold", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Shrub"), aes(label = clean_code2), col = "orchid", fontface = "bold", size = plottext) +
  geom_point(aes(MDS1, MDS2, col = simple_lifeform), alpha = 0.6, pch = 8) +
  geom_point(data = plot_df3, aes(MDS1, MDS2,fill = trt), pch = 21) +
  #scale_color_discrete(name = "Lifeform") +
  scale_color_manual(name = "Lifeform", values = plantcols) +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols) +
  #guides(guide_legend(override.aes = list(pch =21))) +
  #scale_shape_manual(values = c("Dry" = 21, "Mesic" = 24, "Unknown" = 22)) +
  ggtitle("Saddle plots 1997, dry meadow") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")




# ---- sdl 2012, dry meadow only ----
plot_df4 <- data.frame(nmds4$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix4)

spp_df4 <- data.frame(nmds4$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)]))

# capture hulls
grpdf4 <- rbind(data.frame(plot_df4[plot_df4$trt == "N+P", ][chull(plot_df4[plot_df4$trt == "N+P", c("MDS1", "MDS2")]),]),# hull values for grp n+p
                data.frame(plot_df4[plot_df4$trt == "N", ][chull(plot_df4[plot_df4$trt == "N", c("MDS1", "MDS2")]),]), # hull values for grp n
                data.frame(plot_df4[plot_df4$trt == "P", ][chull(plot_df4[plot_df4$trt == "P", c("MDS1", "MDS2")]), ]),  # hull values for grp p
                data.frame(plot_df4[plot_df4$trt == "C", ][chull(plot_df4[plot_df4$trt == "C", c("MDS1", "MDS2")]), ])) # hull values for grp control 


sdl2012_fig <- ggplot(spp_df4, aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf4, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  #geom_text(aes(color = simple_lifeform, label = clean_code2)) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "N-fixer"), aes(label = clean_code2), col = "chocolate4", fontface = "bold", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Forb"), aes(label = clean_code2), col = "grey30", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Grass"), aes(label = clean_code2), col = "seagreen4", fontface = "bold", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Shrub"), aes(label = clean_code2), col = "orchid", fontface = "bold", size = plottext) +
  geom_point(aes(MDS1, MDS2, col = simple_lifeform), alpha = 0.6, pch = 8) +
  geom_point(data = plot_df4, aes(MDS1, MDS2,fill = trt), pch = 21) +
  #scale_color_discrete(name = "Lifeform") +
  scale_color_manual(name = "Lifeform", values = plantcols) +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols) +
  #guides(guide_legend(override.aes = list(pch =21))) +
  #scale_shape_manual(values = c("Dry" = 21, "Mesic" = 24, "Unknown" = 22)) +
  ggtitle("Saddle plots 2012, dry meadow") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none")




# ---- nutnet 2013, common plots only -----
plot_df6 <- data.frame(nmds6$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix6)

spp_df6 <- data.frame(nmds6$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)]))

# capture hulls
grpdf6 <- rbind(data.frame(plot_df6[plot_df6$trt == "N+P", ][chull(plot_df6[plot_df6$trt == "N+P", c("MDS1", "MDS2")]),]),# hull values for grp n+p
                data.frame(plot_df6[plot_df6$trt == "N+P+K", ][chull(plot_df6[plot_df6$trt == "N+P+K", c("MDS1", "MDS2")]),]), # hull values for grp n+p+k
                data.frame(plot_df6[plot_df6$trt == "N+K", ][chull(plot_df6[plot_df6$trt == "N+K", c("MDS1", "MDS2")]),]),# hull values for grp n+p
                data.frame(plot_df6[plot_df6$trt == "N", ][chull(plot_df6[plot_df6$trt == "N", c("MDS1", "MDS2")]),]), # hull values for grp n
                data.frame(plot_df6[plot_df6$trt == "K", ][chull(plot_df6[plot_df6$trt == "K", c("MDS1", "MDS2")]), ]),  # hull values for grp k
                data.frame(plot_df6[plot_df6$trt == "C", ][chull(plot_df6[plot_df6$trt == "C", c("MDS1", "MDS2")]), ])) # hull values for grp control 



nn2013_fig <- ggplot(spp_df6, aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf6, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  #geom_text(aes(color = simple_lifeform, label = clean_code2)) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "N-fixer"), aes(label = clean_code2), col = "chocolate4", fontface = "bold", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Forb"), aes(label = clean_code2), col = "grey30", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Grass"), aes(label = clean_code2), col = "seagreen4", fontface = "bold", size = plottext) +
  #geom_text(data = subset(spp_df1, simple_lifeform == "Shrub"), aes(label = clean_code2), col = "orchid", fontface = "bold", size = plottext) +
  geom_point(aes(MDS1, MDS2, col = simple_lifeform), alpha = 0.6, pch = 8) +
  geom_point(data = plot_df6, aes(MDS1, MDS2,fill = trt), pch = 21) +
  #scale_color_discrete(name = "Lifeform") +
  scale_color_manual(name = "Lifeform", values = plantcols) +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols) +
  #guides(guide_legend(override.aes = list(pch =21))) +
  #scale_shape_manual(values = c("Dry" = 21, "Mesic" = 24, "Unknown" = 22)) +
  ggtitle("NutNet 2013, common plots") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")



# -- make panel plots ----
# (1) last time point: sdl 2016 non snowfence, nutnet 2017
lasttime_fig <- plot_grid(sdlfig, nn2017_fig, nrow = 1,
                          align = "h", rel_widths = c(1,1.3))
lasttime_fig
ggsave(plot = lasttime_fig, 
       filename = "alpine_addnuts/figures/lasttime_nmds.pdf", scale = 1.3)


# (2) nutnet2013, 2017, common sampled only
nn_common_fig <- plot_grid(nn2013_fig, nn2017_fig, nrow = 1,
                           align = "h", rel_widths = c(1,1.3))
nn_common_fig
ggsave(plot = nn_common_fig, 
       filename = "alpine_addnuts/figures/nncommon_nmds.pdf", scale = 1.3)

# (3) sdl 1997, 2012, 2016, common sampled dry meadow plots only
sdl_common_fig <- plot_grid(sdl1997_fig, sdl2012_fig, sdl2016_fig, nrow = 1,
                            align = "h", rel_widths = c(1,1, 1.3))
sdl_common_fig
ggsave(plot = sdl_common_fig, 
       filename = "alpine_addnuts/figures/sdlcommon_nmds.pdf", scale = 1.3)






# -- TRAIT PCA -----
# review unique vals
sapply(dplyr::select(traitdat, Latin.name:Year_Collected), function(x) sort(unique(x)))

# subset dry meadow spp
traits_dm <- subset(traitdat, Exp == "SAD" & TRT == "DRY") %>%
  arrange(Year_Collected, USDA.Code, Rep)
# what are the unique spp in this dataset?
sapply(dplyr::select(traits_dm, Latin.name:Year_Collected), function(x) print(sort(unique(x))))
summary(traits_dm)

# how do traits vary inter- and intraspecifically?
# boxplots
traits_dm %>%
  dplyr::select(-c(Year_Collected:Time)) %>%
  gather(met, val, VegHeight:ncol(.)) %>%
  filter(!is.na(val)) %>%
  ggplot(aes(USDA.Code, val, fill = Life.Form)) +
  geom_boxplot() +
  facet_wrap(~met, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90))

# mean and sd
traits_dm %>%
  dplyr::select(-c(Year_Collected:Time)) %>%
  gather(met, val, VegHeight:ncol(.)) %>%
  filter(!is.na(val)) %>%
  group_by(USDA.Code, Life.Form, met) %>%
  summarise(meanval = mean(val),
            sd = sd(val)) %>%
  ggplot(aes(USDA.Code, meanval, fill = Life.Form)) +
  geom_errorbar(aes(ymax = meanval + sd, ymin = meanval - sd), width = 0) +
  geom_point(pch = 21) +
  facet_wrap(~met, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90))

# what is the distribution of each variable?
traits_dm %>%
  dplyr::select(-c(Year_Collected:Time)) %>%
  gather(met, val, VegHeight:ncol(.)) %>%
  filter(!is.na(val)) %>%
  ggplot() +
  geom_histogram(aes(val)) +
  facet_wrap(~met, scales = "free")

# average dm trait vals and run through pca just to see..
meantraits <- traits_dm %>%
  dplyr::select(-c(Year_Collected:Time)) %>%
  gather(met, val, VegHeight:ncol(.)) %>%
  filter(!is.na(val)) %>%
  group_by(USDA.Code, met) %>%
  summarise(meanval = mean(val)) %>%
  # drop CC1, CC2 and CC3 since CCAvg present
  filter(!met %in% c("CC1", "CC2", "CC3")) %>%
  spread(met, meanval) %>%
  as.data.frame()

# specify spp as rownames
rownames(meantraits) <- meantraits$USDA.Code
# center and standardize trait vals
scaletraits <- scale(meantraits[,2:ncol(meantraits)])
# run pca
traitpc <- rda(scaletraits)
summary(traitpc)
biplot(traitpc, scaling = 2)


# extract pc scores to plot with sdl2016 nmds above
sppscores <- data.frame(scores(traitpc)$sites) %>%
  mutate(USDA.Code = rownames(.)) %>%
  #bring in latin names from marko and soren's dataset
  left_join(distinct(traits_dm[c("USDA.Code", "Latin.name")])) %>%
  #join spp list data
  left_join(distinct(spplist[,2:ncol(spplist)]), by = c("Latin.name" = "simple_name")) %>%
  #clarify USDA Code
  rename(Marko.Code = USDA.Code)



# replot sdl2016_dry nmds with pc scores for spp where exists
spp_df1_dry %>%
  left_join(sppscores[c("clean_code2", "Marko.Code", "Latin.name", "PC1", "PC2")]) %>%
  left_join(meantraits, by = c("Marko.Code" = "USDA.Code")) %>%
  # drop any spp without a score
  filter(!is.na(PC1)) %>%# tetraneuris grandifolia not in sdl 2016 dataset, other 13 spp are
  ggplot(aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf1_dry, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  geom_text(aes(MDS1, MDS2, col = (PC1>=0), label = `Latin.name`)) +
  geom_point(data = plot_df1_dry, aes(MDS1, MDS2,fill = trt), pch = 21) +
  #scale_color_discrete(name = "Lifeform") +
  #scale_color_viridis_c(option = "B") +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols) +
  #guides(guide_legend(override.aes = list(pch =21))) +
  #scale_shape_manual(values = c("Dry" = 21, "Mesic" = 24, "Unknown" = 22)) +
  ggtitle("Saddle plots 2016, dry meadow") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())


# replot nutnet2017 with PC1 scores for species that overlap
spp_df2 %>%
  left_join(sppscores[c("clean_code2", "Marko.Code", "Latin.name", "PC1", "PC2")]) %>%
  left_join(meantraits, by = c("Marko.Code" = "USDA.Code")) %>%
  # drop any spp without a score
  filter(!is.na(PC1)) %>% # deschampsia not in nutnet 2017 dataset
  ggplot(aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf2, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  geom_text(aes(MDS1, MDS2, col = PC1>= 0, label = Latin.name)) +
  geom_point(data = plot_df2, aes(MDS1, MDS2,fill = trt), pch = 21) +
  #scale_color_manual(name = "Lifeform", values = plantcols) +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols, drop = F) +
  ggtitle("NutNet plots 2017") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())



# try pca with all spp rep traits (raw data) and see if traits still load in similar way as when using average values
trait_matrix <- traits_dm %>%
  dplyr::select(-c("SoilMoisture", "Time", "Stomatal.Conductance")) %>%
  # remove any rep missing info
  na.omit() %>%
  as.data.frame()
# assign rownames (concat spp code and rep no)
rownames(trait_matrix) <- trait_matrix$Code

# mean center and scale trait vars
scaletraits_raw <- scale(trait_matrix[,13:ncol(trait_matrix)])

# run pca on centered and scaled trait vars
traitpc_raw <- rda(scaletraits_raw)
summary(traitpc_raw)
biplot(traitpc_raw, display = "species") # traits still loads similarly, just flipped on PC 1

# extra raw scores and color points by species to see how everything clusters
sppscores_raw <- data.frame(scores(traitpc_raw)$sites) %>%
  mutate(Code = rownames(.)) %>%
  #bring in latin names from marko and soren's dataset
  left_join(distinct(traits_dm[c("Code", "Rep", "USDA.Code", "Latin.name")])) %>%
  #join spp list data
  left_join(distinct(spplist[,2:ncol(spplist)]), by = c("Latin.name" = "simple_name")) %>%
  #clarify USDA Code
  rename(Marko.Code = USDA.Code)

ggplot(sppscores_raw, aes(PC1, PC2, col = clean_code2)) +
  geom_text(aes(label = clean_code2)) # spp generally cluster in their own space


# how do mean of PC scores compare to PC scores using averaged trait data?
meanscores_raw <- sppscores_raw %>%
  grouped_df(c(colnames(sppscores_raw)[5:ncol(sppscores_raw)])) %>%
  summarise(meanPC1 = mean(PC1),
            meanPC2 = mean(PC2))

ggplot(meanscores_raw, aes(meanPC1, meanPC2, col = clean_code2)) +
  geom_text(aes(label = clean_code2)) # pretty similar






# ----- old code below -----



# # -- Matrix 5: Sites in Tim's study only (16 plots in Saddle + NutNet plots commonly sampled) ----
# 
# # plots 1-16 for sdl, 28 plots common in nutnet in 2013 and 2017
# matrix5 <-  subset(plantcom, (site == "sdl" & plotid %in% c(1:16)) |
#                      (site == "nutnet" & plotid %in% nnplots1317$plotid)) %>%
#   #remove unknowns and non-veg
#   subset(!grepl("^2", clean_code2)) %>%
#   mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
#   spread(clean_code2, hits, fill = 0) %>%
#   dplyr::select(rowid, site:ncol(.)) %>%
#   as.data.frame()
# 
# sitematrix5 <- matrix5[,1:4]
# # add trtment info
# sitematrix5 <- left_join(sitematrix5, nnplots[c("plotid", "trt")])
# for(i in sitematrix5$plotid[sitematrix5$site == "sdl"]){
#   sitematrix5$trt[sitematrix5$plotid == i] <- sdlplots$trt[sdlplots$plot == i & !is.na(sdlplots$plot)]
# }
# 
# row.names(matrix5) <- matrix5$rowid
# matrix5 <- matrix5[!colnames(matrix5) %in% c("rowid", "site", "yr", "plotid")]
# 
# # relativize data
# matrix5_rel <- vegan::decostand(matrix5, method = "total")
# 
# # run nmds
# nmds5 <- metaMDS(matrix5_rel, k = 2, trymax = 50)
# plot(nmds5, type = "t")
# 
# 
# ordiplot(nmds5, type="n", main = "all sites, all yrs, common plots")
# with (sitematrix5, ordiellipse(nmds5, yr, kind="se", conf=0.95, col="blue", lwd=2,
#                                label=TRUE))
# with (sitematrix5, ordiellipse(nmds5, trt, kind="se", conf=0.95, col="red", lwd=2,
#                                label=TRUE))
# orditorp (nmds5, display="species", col="black", air=0.01)
# 
# 
# ordiplot(nmds5, type="n", main = "all sites, all yrs, common plots")
# with (sitematrix5, ordihull(nmds5, yr, col="blue", lwd=2,
#                             label=TRUE))
# with (sitematrix5, ordihull(nmds5, trt, col="red", lwd=2,
#                             label=TRUE))
# orditorp (nmds5, display="species", col="black", air=0.01)
# 
# 
# 
# 
# 
# 
# 
# 
# # -- Matrix 1: nutnet and sdl; C, N, P and N+P only; all together -----
# # (1) all sites, all yrs -- keep only C, N, P, and N+P plots
# keep_nnplots <- nnplots$plotid[nnplots$trt %in% c("C", "N", "P", "N+P")]
# matrix1 <- subset(plantcom, plotid %in% c(keep_nnplots, sdlplots$plot)) %>%
#   #remove unknowns and non-veg
#   subset(!grepl("^2", clean_code2))
# 
# # check to see how many spp in nutnet sites overlap with spp in sdl sites
# summary(unique(matrix1$clean_code2[plantcom$site == "nutnet"]) %in%
#           unique(matrix1$clean_code2[plantcom$site == "sdl"]))
# summary(unique(matrix1$clean_code2[plantcom$site == "sdl"]) %in%
#           unique(matrix1$clean_code2[plantcom$site == "nutnet"]))
# # pull out spp that overlap
# nnspp <- sort(unique(matrix1$clean_code2[plantcom$site == "nutnet"]))
# sdlspp <- sort(unique(matrix1$clean_code2[plantcom$site == "sdl"]))
# overlapspp <- nnspp[nnspp %in% sdlspp]
# 
# # keep overlapping spp only
# matrix1_overlap <- subset(matrix1, clean_code2 %in% overlapspp) %>%
#   mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
#   spread(clean_code2, hits, fill = 0) %>%
#   dplyr::select(rowid, site:ncol(.)) %>%
#   as.data.frame()
# 
# sitematrix1 <- matrix1_overlap[,1:4]
# # add trtment info
# sitematrix1 <- left_join(sitematrix1, nnplots[c("plotid", "trt")])
# for(i in sitematrix1$plotid[sitematrix1$site == "sdl"]){
#   sitematrix1$trt[sitematrix1$plotid == i] <- sdlplots$trt[sdlplots$plot == i & !is.na(sdlplots$plot)]
# }
# 
# row.names(matrix1_overlap) <- matrix1_overlap$rowid
# matrix1_overlap <- matrix1_overlap[!colnames(matrix1_overlap) %in% c("rowid", "site", "yr", "plotid")]
# 
# # relativize data
# matrix1_overlap_rel <- vegan::decostand(matrix1_overlap, method = "total")
# 
# # run nmds
# nmds1 <- metaMDS(matrix1_overlap_rel, k = 3, trymax = 50)
# plot(nmds1, type = "t")
# ordiplot()
# 
# ordiplot(nmds1, type="n", main = "all sites, all yrs, common spp")
# with (sitematrix1, ordiellipse(nmds1, yr, kind="se", conf=0.95, col="blue", lwd=2,
#                                label=TRUE))
# with (sitematrix1, ordiellipse(nmds1, trt, kind="se", conf=0.95, col="red", lwd=2,
#                                label=TRUE))
# orditorp (nmds1, display="species", col="black", air=0.01)
# 
# 
# 
# # -- Matix 2: NutNet only, all treatments, all years -----
# # check for no effect of +K and micro relative to N+P, N, P, C..
# # all trts, nutnet only plots..
# matrix2 <- subset(plantcom, site == "nutnet") %>%
#   #remove unknowns and non-veg
#   subset(!grepl("^2", clean_code2)) %>%
#   mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
#   spread(clean_code2, hits, fill = 0) %>%
#   dplyr::select(rowid, site:ncol(.)) %>%
#   as.data.frame()
# 
# sitematrix2 <- matrix2[,1:4] %>%
#   left_join(nnplots)
# 
# row.names(matrix2) <- matrix2$rowid
# matrix2 <- matrix2[!colnames(matrix2) %in% c("rowid", "site", "yr", "plotid")]
# matrix2_rel <- decostand(matrix2, method = "total")
# 
# nmds2 <- metaMDS(matrix2_rel, k = 2, trymax = 50)
# plot(nmds2, type = "t")
# 
# ordiplot(nmds2, type="n", main = "nutnet sites, all yrs, all spp")
# with (sitematrix2, ordiellipse(nmds2, yr, kind="se", conf=0.95, col="blue", lwd=2,
#                                label=TRUE))
# with (sitematrix2, ordiellipse(nmds2, trt, kind="se", conf=0.95, col="red", lwd=2,
#                                label=TRUE))
# orditorp (nmds2, display="species", col="black", air=0.02)
# 
# #N+P and N+P+K separate out from the rest. 2017 community has shifted from 2013 but still close
# 
# 
# # -- Matrix 3: TRY SDL 2012 and 2016 ONLY (since 1997 sticks out) -----
# # all trts, nutnet only plots..
# matrix3 <- subset(plantcom, site == "sdl" & yr > 1997) %>%
#   #remove unknowns and non-veg
#   subset(!grepl("^2", clean_code2)) %>%
#   mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
#   spread(clean_code2, hits, fill = 0) %>%
#   dplyr::select(rowid, site:ncol(.)) %>%
#   as.data.frame()
# 
# sitematrix3 <- matrix3[,1:4] %>%
#   mutate(plotid = as.numeric(plotid)) %>%
#   left_join(sdlplots, by = c("plotid" = "plot"))
# 
# row.names(matrix3) <- matrix3$rowid
# matrix3 <- matrix3[!colnames(matrix3) %in% c("rowid", "site", "yr", "plotid")]
# matrix3_rel <- decostand(matrix3, method = "total")
# 
# nmds3 <- metaMDS(matrix3_rel, k = 2, trymax = 50)
# plot(nmds3, type = "t")
# 
# ordiplot(nmds3, type="n", main = "sdl sites, 2012 and 2016 only, all spp")
# with (sitematrix3, ordiellipse(nmds3, yr, kind="se", conf=0.95, col="blue", lwd=2,
#                                label=TRUE))
# with (sitematrix3, ordiellipse(nmds3, trt, kind="se", conf=0.95, col="red", lwd=2,
#                                label=TRUE))
# orditorp (nmds2, display="species", col="black", air=0.02)
# 
# ordiplot(nmds3, type="n", main = "sdl sites, 2012 and 2016 only, all spp")
# with(sitematrix3, ordisurf(nmds3, yr, col="blue", add = TRUE))
# with(sitematrix3, ordiellipse(nmds3, trt, kind = "se", conf = 0.95, col="red", lwd=2,
#                               label=TRUE))
# orditorp (nmds2, display="species", col="black", air=0.02)
# 
# 
# 
# # -- Matrix 6: Control plots only -----
# controlplots <- sort(c(sdlplots_alt$plot[sdlplots_alt$trt == "C" & sdlplots_alt$meadow == "Dry" & sdlplots_alt$snow == "No snow"],
#                        nnplots$plotid[nnplots$trt == "C"]))
# matrix6 <- subset(plantcom, plotid %in% controlplots) %>%
#   #remove unknowns and non-veg
#   subset(!grepl("^2", clean_code2)) %>%
#   mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
#   spread(clean_code2, hits, fill = 0) %>%
#   dplyr::select(rowid, site:ncol(.)) %>%
#   as.data.frame()
# 
# sitematrix6 <- matrix6[,1:4]
# row.names(matrix6) <- matrix6$rowid
# matrix6 <- matrix6[!colnames(matrix6) %in% c("rowid", "site", "yr", "plotid")]
# 
# # relativize data
# matrix6_rel <- vegan::decostand(matrix6, method = "total")
# 
# # run nmds
# nmds6 <- metaMDS(matrix6_rel, k = 2, trymax = 50)
# plot(nmds6, type = "t")
# 
# 
# ordiplot(nmds6, type="n", main = "all sites, all yrs, control plots only")
# with (sitematrix6, ordiellipse(nmds6, yr, kind="se", conf=0.95, col="blue", lwd=2,
#                                label=TRUE))
# orditorp (nmds6, display="species", col="black", air=0.01)
# 
# ordiplot(nmds6, type="n", main = "all sites, all yrs, control plots only")
# with (sitematrix6, ordihull(nmds6, yr, col="blue", lwd=2,
#                             label=TRUE))
# orditorp (nmds6, display="species", col="black", air=0.01)
# 
# 
# 
# 
# 
# # -- REPLOT IN GGPLOT ----
# plot_df <- data.frame(nmds4$points) %>%
#   mutate(rowid = row.names(.)) %>%
#   left_join(sitematrix4)
# spp_df <- data.frame(nmds4$species) %>%
#   mutate(clean_code2 = row.names(.)) %>%
#   left_join(distinct(spplist[,2:ncol(spplist)]))
# 
# ggplot(spp_df, aes(MDS1, MDS2)) + 
#   geom_text(aes(color = Growth_Habit, label = clean_code2)) +
#   geom_text(data = subset(plot_df, trt == "N+P"), aes(MDS1, MDS2, label = plotid)) +
#   geom_text(data = subset(plot_df, trt == "N"), aes(MDS1, MDS2, label = plotid), col = "blue") +
#   geom_text(data = subset(plot_df, trt == "C"), aes(MDS1, MDS2, label = plotid), col = "purple") +
#   geom_text(data = subset(plot_df, trt == "P"), aes(MDS1, MDS2, label = plotid), col = "brown")
#   
# 
# 
# 
