# figures and tables for TS NWT review poster and ms

# made after considering results from script analyze_ts_dats.R
# copying over code for figs and tables desired by TS for figs

# specific output:
# 1) time series NMDS panel of saddle treatments (dry plots only, only spp common across all yrs) + f:g ratio envfit
# 2) time series NMDS panel of nutnet treatments (dry plots only, only spp common across all yrs) + f:g ratio envfit
# 3) trait PCA (reduced variables -- remove redundant/highly correlated variables)
# > 1) take mean trait value per species
# > 2) run PCA and extract scores
# ** CTW ran all spp and their reps in PCA and looked at mean PC1, mean PC2 val per spp and doesn't change results in where spp ordinate relative to one another
# 4) statistical results:
# treatment effects: PERMANOVA, distinct centroids, betadisper
# 5) table:
# spp table with code, latin name, common name, functional group



# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(vegan)
library(indicspecies)
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



# -- ADD SIMPLE LIFEFORM TO SPPLIST ----
#make simple lifeform grp
spplist <- spplist %>%
  mutate(simple_lifeform = ifelse(Family == "Fabaceae", "N-fixer",
                                  ifelse(grepl("Shrub", Growth_Habit), "Shrub",
                                         ifelse(Growth_Habit == "Graminoid", "Grass", "Forb")))) %>%
  # change Dryas to forb from shrub (is listed as subshrub/shrub/forb)
  mutate(simple_lifeform = ifelse(simple_name == "Dryas octopetala", "Forb", simple_lifeform)) %>%
  # fill in simple lifeform for 2FORB and 2GRAM
  mutate(simple_lifeform = ifelse(clean_code2 == "2FORB", "Forb",
                                  ifelse(clean_code2 == "2GRAM", "Grass", simple_lifeform))) %>%
  # add alternative lifeform group where N-fixer lumped into forb group
  mutate(simple_lifeform2 = ifelse(simple_lifeform == "N-fixer", "Forb", simple_lifeform))



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
# add alternative forb group (nfixer groups in forb) and recalc ratios
plantcom_fg$ForbNFix <- with(plantcom_fg, Forb + `N-fixer`)
plantcom_fg$f2gFNF <- plantcom_fg$ForbNFix/plantcom_fg$Grass
plantcom_fg$lnf2gFNF <- log(plantcom_fg$f2gFNF)



# -- NUTNET PLOTS -----


# -- SDL DRY PLOTS -----



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
  geom_histogram(aes(val)) + # plot using log(val) and scale(val) to see how affects distributions
  facet_wrap(~met, scales = "free")

# look at correlations of variables with one another
pairs(traits_dm[,c(15:20, 22:ncol(traits_dm))])

# pct N nonlinearly related to CN_ratio
# SLA and LMA nonlinearly related
# leaf area, wet wgt and dry wgt all linearly related
# LMDC and LWC linearly related

# > decide to log transform physical measurements: wgt measurements, leaf area, height measurements
# > also, after lit review and looking at marko + soren's metadata, decide to use the  

# specify traits to use
trts <- colnames(traits_dm)[c(20, 25, 27, 29:33)] # 16 = Oheight, 23 = dry weight -- not keeping bc using logged vars
trts 
 
# average dm trait vals and run through pca just to see..
meantraits <- traits_dm %>%
  # log transform physical traits
  mutate(ln_ohgt = log(OHeight),
         ln_drywgt = log(DryWeight)) %>%
  dplyr::select(Latin.name:Code, trts, ln_ohgt, ln_drywgt) %>%
  gather(met, val, CCAvg:ncol(.)) %>%
  filter(!is.na(val)) %>%
  group_by(USDA.Code, met) %>%
  summarise(meanval = mean(val)) %>%
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


