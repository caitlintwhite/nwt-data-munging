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


# -- RECODE TRTMENTS FOR NUTNET (SIMPLIFY) -----
#ts found no effect of +k
nnplots$trt2 <- gsub("[+]K", "", nnplots$trt)
nnplots$trt2 <- gsub("K", "C", nnplots$trt2)

# make trts factor in each site
nnplots$trt <- factor(nnplots$trt, levels = c(c("C", "K", "N", "N+K", "N+P", "N+P+K")))
nnplots$trt2 <- factor(nnplots$trt2, levels = c("C", "N", "N+P"))
sdlplots$trt <- factor(sdlplots$trt, levels = c("C", "N", "P", "N+P"))



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
summary(traitpc) # copy paste results in console into excel spreadsheet for tim
biplot(traitpc, scaling = 2)


# extract pc scores to plot in treatment nmds plots 
sppscores <- data.frame(scores(traitpc)$sites) %>%
  mutate(USDA.Code = rownames(.)) %>%
  #bring in latin names from marko and soren's dataset
  left_join(distinct(traits_dm[c("USDA.Code", "Latin.name")])) %>%
  #join spp list data
  left_join(distinct(spplist[,2:ncol(spplist)]), by = c("Latin.name" = "simple_name")) %>%
  #clarify USDA Code
  rename(Marko.Code = USDA.Code) %>%
  mutate(resource_grp = ifelse(PC1 <= 0, "Conservative", "Acquisitive"))

varscores <- data.frame(scores(traitpc, display = "species"))
varscores$varnames <- rownames(varscores)
# assign abbreviated intelligible varnames for plot
varscores <- mutate(varscores, abbr = varnames) %>%
  mutate(abbr = recode(abbr,
                       CCAvg = "Chl",
                       CN_ratio = "C:N",
                       ln_drywgt = "Dry weight",
                       ln_ohgt = "Height",
                       Percent_C = "%C",
                       Percent_N = "%N"))



# -- TS ANALYSIS: ALL YEARS, ONLY PLOTS COMMONLY SAMPLED ACROSS ALL -----
# id common plots sampled across all yrs
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




# -- NUTNET PLOTS -----
# time series panels of just plots sampled over all three years (lowest common denom = last yr sampled)

# -- NUTNET 2013 -----
matrix_nn2013 <- subset(plantcom, yr == 2013 & plotid %in% nncommon$plotid) %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  # create unique row id
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

# calculate forb to grass ratio
fgrat_nn2013 <- subset(plantcom_fg, plotid %in% matrix_nn2013$plotid & yr == 2013)

sitematrix_nn2013 <- matrix_nn2013[,1:4] %>%
  left_join(nnplots) %>%
  left_join(fgrat_nn2013)

row.names(matrix_nn2013) <- matrix_nn2013$rowid
matrix_nn2013 <- matrix_nn2013[!colnames(matrix_nn2013) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix_nn2013_rel <- vegan::decostand(matrix_nn2013, method = "total")

# run nmds
nmds_nn2013 <- metaMDS(matrix_nn2013_rel, k = 2, trymax = 50)
nmds_nn2013
plot(nmds_nn2013, type = "t")

# environmental fit grass to forb
# original trts
fitnn2013 <- envfit(nmds_nn2013, sitematrix_nn2013[c("trt", "lnf2gFNF")], strata = sitematrix_nn2013$block, perm = 999)
fitnn2013 #trt is signif, forb:grass signif

#original trts
ordiplot(nmds_nn2013, type="n", main = "NutNet 2013, all treatments")
with (sitematrix_nn2013, ordiellipse(nmds_nn2013, trt, kind="se", conf=0.95, col=1:6))
with (sitematrix_nn2013, ordisurf(nmds_nn2013, lnf2gFNF, col="grey50", add = T))
plot(fitnn2013, col = 1:7)
orditorp (nmds_nn2013, display="species", col="grey30", air=0.01)


# analysis of similiarities/mrpp/permanova
## calculate CB distance
matrix_nn2013_rel_bray <- vegdist(matrix_nn2013_rel)
summary(anosim(matrix_nn2013_rel_bray, grouping = sitematrix_nn2013$trt, strata = sitematrix_nn2013$block, permutations = 999))
mrpp(matrix_nn2013_rel,  grouping = sitematrix_nn2013$trt, strata = sitematrix_nn2013$block, distance = "bray")
adonis(matrix_nn2013_rel ~ trt * lnf2gFNF, data = sitematrix_nn2013, strata = sitematrix_nn2013$block, permutations = 999, method = "bray")
# change order of variables
adonis(matrix_nn2013_rel ~ lnf2gFNF * trt, data = sitematrix_nn2013, strata = sitematrix_nn2013$block, permutations = 999, method = "bray")
# either order of explanatory vars, forb:grass ration and trtment is distinct, but there is no interaction

# test for homogeneity of variances
nn2013_disper <- betadisper(matrix_nn2013_rel_bray, sitematrix_nn2013$trt)
nn2013_disper
anova(nn2013_disper) 
permutest(nn2013_disper) 
TukeyHSD(nn2013_disper)

# indicator species for nutnet plots
# indicator species
nn2013_ind = multipatt(matrix_nn2013_rel, sitematrix_nn2013$trt, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(nn2013_ind, alpha = 0.1)
# check signif of adjust pvals (for multiple spp comparisons)
summary(p.adjust(nn2013_ind$sign$p.value, method = "holm") < 0.1) #nothing signif

# does any spp associate with a particular treatment?
matrix_nn2013_rel_pa <- as.data.frame(ifelse(matrix_nn2013_rel>0,1,0))
nn2013_phi <- multipatt(matrix_nn2013_rel_pa, sitematrix_nn2013$trt, duleg = T, func = "r.g", control = how(nperm = 999))
summary(nn2013_phi)
# check signif for multiple spp comparisons
summary(p.adjust(nn2013_phi$sign$p.value, method = "holm")<0.1) #nothing signig, 2 NAs
# who are the generalists?
subset(nn2013_phi$sign, is.na(nn2013_phi$sign$p.value)) # carex rupestris and kobresia...
#check with combos allowed
nn2013_phi_comb <- multipatt(matrix_nn2013_rel_pa, sitematrix_nn2013$trt, func = "r.g", control = how(nperm = 999))
summary(nn2013_phi_comb)
# check signif for multiple spp comparisons
subset(nn2013_phi_comb$sign, p.adjust(nn2013_phi_comb$sign$p.value, method = "holm")<0.1) # nothing signif


summary(anosim(matrix_nn2013_rel_bray, grouping = sitematrix_nn2013$trt, strata = sitematrix_nn2013$block, permutations = 999))
mrpp(matrix_nn2013_rel,  grouping = sitematrix_nn2013$trt, strata = sitematrix_nn2013$block, distance = "bray")
adonis(matrix_nn2013_rel ~ trt * lnf2gFNF, data = sitematrix_nn2013, strata = sitematrix_nn2013$block, permutations = 999, method = "bray")
# change order of variables
adonis(matrix_nn2013_rel ~ lnf2gFNF * trt, data = sitematrix_nn2013, strata = sitematrix_nn2013$block, permutations = 999, method = "bray")
# either order of explanatory vars, forb:grass ration and trtment is distinct, but there is no interaction



# -- NUTNET 2013, SIMPLIFIED TREATMENTS -----
# simplified trts
fitnn2013.simple <- envfit(nmds_nn2013, sitematrix_nn2013[c("trt2", "lnf2gFNF")], 
                           strata = sitematrix_nn2013$block, perm = 999)
fitnn2013.simple

#simplified trts
ordiplot(nmds_nn2013, type="n", main = "NutNet 2013, collapsed treatments")
with (sitematrix_nn2013, ordiellipse(nmds_nn2013, trt2, kind="se", conf=0.95, col=1:6))
with (sitematrix_nn2013, ordisurf(nmds_nn2013, lnf2gFNF, col="grey50", add = T))
plot(fitnn2013.simple, col = 1:7)
orditorp (nmds_nn2013, display="species", col="grey30", air=0.01)

# indicator species analysis
nn2013_ind.simple = multipatt(matrix_nn2013_rel, sitematrix_nn2013$trt2, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(nn2013_ind.simple, alpha = 0.1)
# check signif of adjust pvals (for multiple spp comparisons)
summary(p.adjust(nn2013_ind.simple$sign$p.value, method = "holm") < 0.1) #2 true
nn2013_ind.simple$sign[(p.adjust(nn2013_ind.simple$sign$p.value, method = "holm") < 0.1),]

# gather results
nn2013_ind.simple_df <- cbind(nn2013_ind.simple$A, nn2013_ind.simple$B, nn2013_ind.simple$sign,
                              holm.pval = p.adjust(nn2013_ind.simple$sign$p.value, method = "holm")) 
colnames(nn2013_ind.simple_df)[1:3] <- paste0("a.",colnames(nn2013_ind.simple_df)[1:3])
colnames(nn2013_ind.simple_df)[4:6] <- paste0("b.",colnames(nn2013_ind.simple_df)[4:6])
nn2013_ind.simple_df$clean_code2 <- rownames(nn2013_ind.simple_df)
nn2013_ind.simple_df <- arrange(nn2013_ind.simple_df, p.value) %>%
  # recode index
  mutate(index = ifelse(index == 1, "C",
                        ifelse(index == 2, "N", "N+P"))) %>%
  # join latin name
  left_join(distinct(spplist[c("clean_code2","simple_name")]))

# does any spp associate with a particular treatment?
matrix_nn2013_rel_pa <- as.data.frame(ifelse(matrix_nn2013_rel>0,1,0))
nn2013_phi.simple <- multipatt(matrix_nn2013_rel_pa, sitematrix_nn2013$trt2, duleg = T, func = "r.g", control = how(nperm = 999))
summary(nn2013_phi.simple)
# check signif for multiple spp comparisons
summary(p.adjust(nn2013_phi.simple$sign$p.value, method = "holm")<0.1) #nothing signig, 2 NAs
# who are the generalists?
subset(nn2013_phi.simple$sign, is.na(nn2013_phi.simple$sign$p.value)) # carex rupestris and kobresia...

# analysis of similarity and permanovas
summary(anosim(matrix_nn2013_rel_bray, grouping = sitematrix_nn2013$trt2, strata = sitematrix_nn2013$block, permutations = 999))
mrpp(matrix_nn2013_rel,  grouping = sitematrix_nn2013$trt2, strata = sitematrix_nn2013$block, distance = "bray")
adonis(matrix_nn2013_rel ~ trt2 * lnf2gFNF, data = sitematrix_nn2013, strata = sitematrix_nn2013$block, permutations = 999, method = "bray")
# change order of variables
adonis(matrix_nn2013_rel ~ lnf2gFNF * trt2, data = sitematrix_nn2013, strata = sitematrix_nn2013$block, permutations = 999, method = "bray")
# either order of explanatory vars, forb:grass ration and trtment is distinct, but there is no interaction


# test for homogeneity of variances
nn2013_disper.simple <- betadisper(matrix_nn2013_rel_bray, sitematrix_nn2013$trt2)
nn2013_disper.simple
anova(nn2013_disper.simple) # no difference in homogeneity of dispersion
TukeyHSD(nn2013_disper.simple)
boxplot(nn2013_disper.simple)




# -- NUTNET 2017 -----
matrix_nn2017 <- subset(plantcom, yr == 2017 & site == "nutnet") %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

# calculate forb to grass ratio
fgrat_nn2017 <- subset(plantcom_fg, plotid %in% matrix_nn2017$plotid & yr == 2017)

sitematrix_nn2017 <- matrix_nn2017[,1:4] %>%
  left_join(nnplots) %>%
  left_join(fgrat_nn2017)

row.names(matrix_nn2017) <- matrix_nn2017$rowid
matrix_nn2017 <- matrix_nn2017[!colnames(matrix_nn2017) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
nn2017_rel <- vegan::decostand(matrix_nn2017, method = "total")

# run nmds
nmds_nn2017 <- metaMDS(nn2017_rel, k = 2, trymax = 50)
nmds_nn2017
plot(nmds_nn2017, type = "t")

# environmental fit grass to forb
fitnn2017 <- envfit(nmds_nn2017, sitematrix_nn2017[c("trt", "lnf2gFNF")], strata = sitematrix_nn2017$block, perm = 999)
fitnn2017 #trt is signif, forb:grass signif

# simplified trts
fitnn2017.simple <- envfit(nmds_nn2017, sitematrix_nn2017[c("trt2", "lnf2gFNF")], strata = sitematrix_nn2017$block, perm = 999)
fitnn2017.simple

#original trts
ordiplot(nmds_nn2017, type="n", main = "NutNet 2017, all treatments")
with (sitematrix_nn2017, ordiellipse(nmds_nn2017, trt, kind="se", conf=0.95, col=1:6))
with (sitematrix_nn2017, ordisurf(nmds_nn2017, lnf2gFNF, col="grey50", add = T))
plot(fitnn2017, col = 1:7)
orditorp (nmds_nn2017, display="species", col="grey30", air=0.01)

# collapsed trts
ordiplot(nmds_nn2017, type="n", main = "NutNet 2017, collapsed treatments")
with (sitematrix_nn2017, ordiellipse(nmds_nn2017, trt2, kind="ehull", conf=0.95, col=1:6))
with (sitematrix_nn2017, ordisurf(nmds_nn2017, lnf2gFNF, col="grey50", add = T))
plot(fitnn2017.simple, col = 1:7)
orditorp (nmds_nn2017, display="species", col="grey30", air=0.01)

nn2017boxplot <- ggplot(sitematrix_nn2017, aes(trt2, lnf2gFNF)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5, width = 0.1) +
  labs(y = NULL) +
  ggtitle("Nutnet 2017")
nn2017boxplot

# plot boxplots together for kns
plot_grid(sdlboxplot, nn2017boxplot,
          nrow = 1)

# analysis of similiarities/mrpp/permanova -- ORIGINAL TREATMENTS
## calculate CB distance
nn2017_rel_bray <- vegdist(nn2017_rel)
summary(anosim(nn2017_rel_bray, grouping = sitematrix_nn2017$trt, strata = sitematrix_nn2017$block, permutations = 999))
mrpp(nn2017_rel,  grouping = sitematrix_nn2017$trt, strata = sitematrix_nn2017$block, distance = "bray")
adonis(nn2017_rel ~ trt * lnf2gFNF, data = sitematrix_nn2017, strata = sitenmatrix_n2017$block, permutations = 999, method = "bray")
# change order of variables
adonis(nn2017_rel ~ lnf2gFNF * trt, data = sitematrix_nn2017, strata = sitematrix_nn2017$block, permutations = 999, method = "bray")
# either order of explanatory vars, forb:grass ration and trtment is distinct, but there is no interaction


# indicator species for nutnet plots
# indicator species
nn2017_ind <- multipatt(nn2017_rel, sitematrix_nn2017$trt, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(nn2017_ind, alpha = 0.1)
# check signif of adjust pvals (for multiple spp comparisons)
summary(p.adjust(nn2017_ind$sign$p.value, method = "holm") < 0.1) #nothing signif

# does any spp associate with a particular treatment?
nn2017_rel_pa <- as.data.frame(ifelse(nn2017_rel>0,1,0))
nn2017_phi <- multipatt(nn2017_rel_pa, sitematrix_nn2017$trt, duleg = T, func = "r.g", control = how(nperm = 999))
summary(nn2017_phi)
# check signif for multiple spp comparisons
summary(p.adjust(nn2017_phi$sign$p.value, method = "holm")<0.1) #nada
#check with combos allowed
nn2017_phi_comb <- multipatt(nn2017_rel_pa, sitematrix_nn2017$trt, func = "r.g", control = how(nperm = 999))
summary(nn2017_phi_comb)
# check signif for multiple spp comparisons
subset(nn2017_phi_comb$sign, p.adjust(nn2017_phi_comb$sign$p.value, method = "holm")<0.1) # trispi marginally signif for +n+k+p, but not at 0.05


summary(anosim(nn2017_rel_bray, grouping = sitematrix_nn2017$trt, strata = sitematrix_nn2017$block, permutations = 999))
mrpp(nn2017_rel,  grouping = sitematrix_nn2017$trt, strata = sitematrix_nn2017$block, distance = "bray")
adonis(nn2017_rel ~ trt * lnf2gFNF, data = sitematrix_nn2017, strata = sitenmatrix_n2017$block, permutations = 999, method = "bray")
# change order of variables
adonis(nn2017_rel ~ lnf2gFNF * trt, data = sitematrix_nn2017, strata = sitematrix_nn2017$block, permutations = 999, method = "bray")
# either order of explanatory vars, forb:grass ration and trtment is distinct, but there is no interaction

# with collapsed treatment
# indicator species for nutnet plots
# indicator species
nn2017_ind.simple <- multipatt(nn2017_rel, sitematrix_nn2017$trt2, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(nn2017_ind.simple, alpha = 0.1)
# check signif of adjust pvals (for multiple spp comparisons)
summary(p.adjust(nn2017_ind.simple$sign$p.value, method = "holm") < 0.1) #4 true!
nn2017_ind.simple$sign[(p.adjust(nn2017_ind.simple$sign$p.value, method = "holm") < 0.1),]

# gather results
nn2017_ind.simple_df <- cbind(nn2017_ind.simple$A, nn2017_ind.simple$B, nn2017_ind.simple$sign,
                              holm.pval = p.adjust(nn2017_ind.simple$sign$p.value, method = "holm"))
colnames(nn2017_ind.simple_df)[1:3] <- paste0("a.",colnames(nn2017_ind.simple_df)[1:3])
colnames(nn2017_ind.simple_df)[4:6] <- paste0("b.",colnames(nn2017_ind.simple_df)[4:6])
nn2017_ind.simple_df$clean_code2 <- rownames(nn2017_ind.simple_df)
nn2017_ind.simple_df <- arrange(nn2017_ind.simple_df, p.value) %>%
  # recode index
  mutate(index = ifelse(index == 1, "C",
                        ifelse(index == 2, "N", "N+P"))) %>%
  # join latin name
  left_join(distinct(spplist[c("clean_code2","simple_name")]))

# does any spp associate with a particular treatment?
nn2017_phi.simple <- multipatt(nn2017_rel_pa, sitematrix_nn2017$trt2, duleg = T, func = "r.g", control = how(nperm = 999))
summary(nn2017_phi.simple)
# check signif for multiple spp comparisons
summary(p.adjust(nn2017_phi.simple$sign$p.value, method = "holm")<0.1) #1 true
nn2017_phi.simple$sign[(p.adjust(nn2017_phi.simple$sign$p.value, method = "holm") < 0.1),]

# test for homogeneity of variances
nn2017_disper.simple <- betadisper(nn2017_rel_bray, sitematrix_nn2017$trt2)
nn2017_disper.simple
anova(nn2017_disper.simple) # no difference in homogeneity of dispersion
TukeyHSD(nn2017_disper.simple)
boxplot(nn2017_disper.simple)

# try PCoA just to see..
nn2017.b.pcoa<-cmdscale(nn2017_rel_bray, eig=TRUE)
nn2017.b.pcoa
testdf <- data.frame(scores(nn2017.b.pcoa))
testdf$rowid <- rownames(testdf)
testdf <- left_join(testdf, sitematrix_nn2017)
ggplot(testdf, aes(Dim1, Dim2, col = trt2)) +
  geom_point() +
  ggtitle("NutNet 2017 PCoA")
# > N+P and N+P+K separate out from the others, as in the NMDS



########################
# -- SDL DRY PLOTS -----
# time series panels of just plots sampled over all three years (lowest common denom = first yr sampled)

# -- SDL 1997, DRY MEADOW PLOTS -----
matrix_sdl1997 <- subset(plantcom, yr == 1997 & plotid %in%  sdlcommon$plotid) %>%
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
fgrat_sdl1997 <- subset(plantcom_fg, plotid %in% matrix_sdl1997$plotid & yr == 1997) %>%
  # coerce plotid to number to joins with sitematrix
  mutate(plotid = as.numeric(plotid))


sitematrix_sdl1997 <- matrix_sdl1997[,1:4] %>%
  mutate(plotid = as.numeric(plotid)) %>%
  left_join(distinct(sdlplots[colnames(sdlplots) != "old_plot"]), by = c("plotid" = "plot")) %>%
  # join grass forb ratio
  left_join(fgrat_sdl1997)


row.names(matrix_sdl1997) <- matrix_sdl1997$rowid
matrix_sdl1997 <- matrix_sdl1997[!colnames(matrix_sdl1997) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
sdl1997_rel <- decostand(matrix_sdl1997, method = "total")

# run nmds
nmds_sdl1997 <- metaMDS(sdl1997_rel, k = 2, trymax = 50)
nmds_sdl1997
stressplot(nmds_sdl1997)
plot(nmds_sdl1997, type = "t")

# add environmental fit of forb to grass ratio
fitsdl1997 <- envfit(nmds_sdl1997, sitematrix_sdl1997[c("trt", "lnf2gFNF")], perm = 999)
fitsdl1997 # trt not signif

ordiplot(nmds_sdl1997, type="n", main = "Saddle 1997, dry meadow plots 1-16")
with (sitematrix_sdl1997, ordiellipse(nmds_sdl1997, trt, kind="se", conf=0.95, col=1:4, lwd = 2))
with (sitematrix_sdl1997, ordisurf(nmds_sdl1997, lnf2g, col = "grey50", add = T))
plot(fitsdl1997, col = 1:5)
orditorp (nmds_sdl1997, display="species", col="grey30", air=0.01)


# dissimilarity analyses (anosim, mrpp, permanova)
# analysis of similiarities/mrpp/permanova
## calculate CB distance
sdl1997_rel_bray <- vegdist(sdl1997_rel)
summary(anosim(sdl1997_rel_bray, grouping = sitematrix_sdl1997$trt, permutations = 999))
mrpp(sdl1997_rel,  grouping = sitematrix_sdl1997$trt, distance = "bray")
adonis(sdl1997_rel ~ trt * lnf2gFNF, data = sitematrix_sdl1997, permutations = 999, method = "bray")
# change order of variables
adonis(sdl1997_rel ~ lnf2gFNF * trt, data = sitematrix_sdl1997, permutations = 999, method = "bray")
# either order of explanatory vars, forb:grass ratio and treattment is signif, but there is no interaction


# indicator species analysis for 1997 saddle plots
# indicator species
sdl1997_ind = multipatt(sdl1997_rel, sitematrix_sdl1997$trt, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(sdl1997_ind, alpha = 0.1)
# check signif of adjust pvals (for multiple spp comparisons)
subset(sdl1997_ind$sign, p.adjust(sdl1997_ind$sign$p.value, method = "holm") < 0.1) #CARUD in control sometimes signif depending on permutations

# specify index groups from ind spp analysis
sdl_ind_grps <- c(`1` ="C", `2`="N", `3` = "P", `4` = "N+P") 
# gather results
sdl1997_ind_df <- cbind(sdl1997_ind$A, sdl1997_ind$B, sdl1997_ind$sign,
                              holm.pval = p.adjust(sdl1997_ind$sign$p.value, method = "holm")) 
colnames(sdl1997_ind_df)[1:3] <- paste0("a.",colnames(sdl1997_ind_df)[1:3])
colnames(sdl1997_ind_df)[4:6] <- paste0("b.",colnames(sdl1997_ind_df)[4:6])
sdl1997_ind_df$clean_code2 <- rownames(sdl1997_ind_df)
sdl1997_ind_df <- arrange(sdl1997_ind_df, p.value) %>%
  mutate(index = ifelse(index == 1, "C", 
                        ifelse(index == 2, "N",
                        ifelse(index == 3, "P", "N+P")))) %>%
  # join latin name
  left_join(distinct(spplist[c("clean_code2","simple_name")]))


# does any spp associate with a particular treatment?
sdl1997_rel_pa <- as.data.frame(ifelse(sdl1997_rel>0,1,0))
sdl1997_phi <- multipatt(sdl1997_rel_pa, sitematrix_sdl1997$trt, duleg = T, func = "r.g", control = how(nperm = 999))
summary(sdl1997_phi, alpha = 1) # nothing is significant

#check with trt combos allowed
sdl1997_phi_comb <- multipatt(sdl1997_rel_pa, sitematrix_sdl1997$trt, func = "r.g", control = how(nperm = 999))
summary(sdl1997_phi_comb)
# check signif for multiple spp comparisons
subset(sdl1997_phi_comb$sign, p.adjust(sdl1997_phi_comb$sign$p.value, method = "holm")<0.1) # nothing signif



# -- SDL 2012, DRY MEADOW PLOTS -----
matrix_sdl2012 <- subset(plantcom, yr == 2012 & plotid %in% sdlcommon$plotid) %>%
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
fgrat_sdl2012 <- subset(plantcom_fg, plotid %in% matrix_sdl2012$plotid & yr == 2012) %>%
  # coerce plotid to number to joins with sitematrix
  mutate(plotid = as.numeric(plotid))


sitematrix_sdl2012 <- matrix_sdl2012[,1:4] %>%
  mutate(plotid = as.numeric(plotid)) %>%
  left_join(distinct(sdlplots[colnames(sdlplots) != "old_plot"]), by = c("plotid" = "plot")) %>%
  # join grass forb ratio
  left_join(fgrat_sdl2012)
sitematrix_sdl2012$trt <- factor(sitematrix_sdl2012$trt, levels = c("C", "N", "P", "N+P"))

row.names(matrix_sdl2012) <- matrix_sdl2012$rowid
matrix_sdl2012 <- matrix_sdl2012[!colnames(matrix_sdl2012) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix_sdl2012_rel <- decostand(matrix_sdl2012, method = "total")

# run nmds
nmds_sdl2012 <- metaMDS(matrix_sdl2012_rel, k = 2, trymax = 50)
nmds_sdl2012
stressplot(nmds_sdl2012)
plot(nmds_sdl2012, type = "t")

# add environmental fit of forb to grass ratio
fitsdl2012 <- envfit(nmds_sdl2012, sitematrix_sdl2012[c("trt", "lnf2gFNF")], perm = 999)
fitsdl2012 # trt p << 0.001, f:g ratio not signif

ordiplot(nmds_sdl2012, type="n", main = "2012 Saddle, dry meadow only")
with (sitematrix_sdl2012, ordiellipse(nmds_sdl2012, trt, kind="se", conf=0.95, col=1:4, lwd=2))
with (sitematrix_sdl2012, ordisurf(nmds_sdl2012, lnf2g, col="grey50", add = T))
plot(fitsdl2012, col = 1:5)
orditorp (nmds_sdl2012, display="species", col="grey30", air=0.02)


# dissimilarity analyses (anosim, mrpp, permanova)
# analysis of similiarities/mrpp/permanova
## calculate CB distance
matrix_sdl2012_rel_bray <- vegdist(matrix_sdl2012_rel)
summary(anosim(matrix_sdl2012_rel_bray, grouping = sitematrix_sdl2012$trt, permutations = 999))
mrpp(matrix_sdl2012_rel,  grouping = sitematrix_sdl2012$trt, distance = "bray")
adonis(matrix_sdl2012_rel ~ trt * lnf2g, data = sitematrix_sdl2012, permutations = 999, method = "bray")
# change order of variables
adonis(matrix_sdl2012_rel ~ lnf2g * trt, data = sitematrix_sdl2012, permutations = 999, method = "bray")
# trt always signif, interactions always marginally signif, lnf2g only signif if lnf2g first

# indicator species analysis for 1997 saddle plots
# indicator species
matrix_sdl2012_ind = multipatt(matrix_sdl2012_rel, sitematrix_sdl2012$trt, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(matrix_sdl2012_ind, alpha = 0.1, indvalcomp = T)
# check signif of adjust pvals (for multiple spp comparisons)
summary(p.adjust(matrix_sdl2012_ind$sign$p.value, method = "holm") < 0.1) #nothing signif

# gather results
sdl2012_ind_df <- cbind(matrix_sdl2012_ind$A, matrix_sdl2012_ind$B, matrix_sdl2012_ind$sign,
                        holm.pval = p.adjust(matrix_sdl2012_ind$sign$p.value, method = "holm")) 
colnames(sdl2012_ind_df)[1:3] <- paste0("a.",colnames(sdl2012_ind_df)[1:3])
colnames(sdl2012_ind_df)[4:6] <- paste0("b.",colnames(sdl2012_ind_df)[4:6])
sdl2012_ind_df$clean_code2 <- rownames(sdl2012_ind_df)
sdl2012_ind_df <- arrange(sdl2012_ind_df, p.value) %>%
  # recode index
  mutate(index = ifelse(index == 1, "C", 
                        ifelse(index == 2, "N",
                               ifelse(index == 3, "P", "N+P")))) %>%
  # join latin name
  left_join(distinct(spplist[c("clean_code2","simple_name")]))



# does any spp associate with a particular treatment?
matrix_sdl2012_rel_pa <- as.data.frame(ifelse(matrix_sdl2012_rel>0,1,0))
matrix_sdl2012_phi <- multipatt(matrix_sdl2012_rel_pa, sitematrix_sdl2012$trt, duleg = T, func = "r.g", control = how(nperm = 999))
summary(matrix_sdl2012_phi, alpha = 0.1) # a few signif for control, marginal for P
# check signif with holm's test
# check signif for multiple spp comparisons
subset(matrix_sdl2012_phi$sign, p.adjust(matrix_sdl2012_phi$sign$p.value, method = "holm")<0.1) # nothing signif


#check with trt combos allowed
matrix_sdl2012_phi_comb <- multipatt(matrix_sdl2012_rel_pa, sitematrix_sdl2012$trt, duleg = F, func = "r.g", control = how(nperm = 999))
summary(matrix_sdl2012_phi_comb)
# check signif for multiple spp comparisons
subset(matrix_sdl2012_phi_comb$sign, p.adjust(matrix_sdl2012_phi_comb$sign$p.value, method = "holm")<0.1) # nothing signif

# no species align with any particular treatment, as a predictor or associatively 
# no spp are generalists tho (no NAs in any of the indic analysis)


# -- SDL 2016, DRY MEADOW PLOTS ----
# last yr, dry meadow plots only
matrix_sdl2016 <- subset(plantcom, yr == 2016 & plotid %in% sdlcommon$plotid) %>%
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
fgrat_sdl2016 <- subset(plantcom_fg, plotid %in% matrix_sdl2016$plotid & yr == 2016) %>%
  # coerce plotid to number to joins with sitematrix
  mutate(plotid = as.numeric(plotid))

sitematrix_sdl2016 <- matrix_sdl2016[,1:4] %>%
  mutate(plotid = as.numeric(plotid)) %>%
  left_join(distinct(sdlplots[colnames(sdlplots) != "old_plot"]), by = c("plotid" = "plot")) %>%
  # join grass forb ratio
  left_join(fgrat_sdl2016)

row.names(matrix_sdl2016) <- matrix_sdl2016$rowid
matrix_sdl2016 <- matrix_sdl2016[!colnames(matrix_sdl2016) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix_sdl2016_rel <- decostand(matrix_sdl2016, method = "total")

# nmds
nmds_sdl2016 <- metaMDS(matrix_sdl2016, k = 2, trymax = 50)
plot(nmds_sdl2016, type = "t")

fit_sdl2016 <- envfit(nmds_sdl2016, sitematrix_sdl2016[c("trt","lnf2gFNF")], perm = 999)
fit_sdl2016 #trt signif, forb:grass signif

ordiplot(nmds_sdl2016, type="n", main = "Saddle 2016, dry meadow only, no snowfence, all treatments")
with (sitematrix_sdl2016, 
      ordiellipse(nmds_sdl2016, trt, kind="se", conf=0.95, col=1:4))
with(sitematrix_sdl2016, 
     ordisurf(nmds_sdl2016, lnf2g, col="grey50", add = TRUE))
plot(fit_sdl2016, col = 1:5)
orditorp (nmds_sdl2016, display="species", col="black", air=0.01)

# indicator species analysis on dry meadow plots only
sdl2016_ind_dry = multipatt(matrix_sdl2016_rel, sitematrix_sdl2016$trt, 
                            func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(sdl2016_ind_dry, indvalcomp = T)
# any significant with adjusted p vals?
summary(p.adjust(sdl2016_ind_dry$sign$p.value, method = "holm")< 0.1) #nothing significant

# gather results
sdl2016_ind_df <- cbind(sdl2016_ind_dry$A, sdl2016_ind_dry$B, sdl2016_ind_dry$sign,
                        holm.pval = p.adjust(sdl2016_ind_dry$sign$p.value, method = "holm")) 
colnames(sdl2016_ind_df)[1:3] <- paste0("a.",colnames(sdl2016_ind_df)[1:3])
colnames(sdl2016_ind_df)[4:6] <- paste0("b.",colnames(sdl2016_ind_df)[4:6])
sdl2016_ind_df$clean_code2 <- rownames(sdl2016_ind_df)
sdl2016_ind_df <- arrange(sdl2016_ind_df, p.value) %>%
  # recode index
  mutate(index = ifelse(index == 1, "C", 
                        ifelse(index == 2, "N",
                               ifelse(index == 3, "P", "N+P")))) %>%
  # join latin name
  left_join(distinct(spplist[c("clean_code2","simple_name")]))

# same results with spp associations?
sdl2016_rel_pa <- as.data.frame(ifelse(matrix_sdl2016_rel>0, 1,0))
sdl2016_phi_dry = multipatt(sdl2016_rel_pa, sitematrix_sdl2016$trt, 
                            func = "r.g", control = how(nperm = 999)) #duleg = TRUE)
summary(sdl2016_phi_dry, indvalcomp = T) # nothing prefers any treatment, but there are species that occur in a combination of treatments
# any significant with adjusted p vals?
summary(p.adjust(sdl2016_phi_dry$sign$p.value, method = "holm") <= 0.1) #CARUD signficant marginally [epending on permuation] (0.1, but not at 0.05 )
# who is the generalist?
subset(sdl2016_phi_dry$sign, is.na(sdl2016_phi_dry$sign$p.value)) #geum rossii
# Carex rupestris is the only signif species, prefers dry meadow Control or +N (doesn't like +P?)
subset(sdl2016_phi_dry$sign, p.adjust(sdl2016_phi_dry$sign$p.value, method = "holm") < 0.1)

# try PCoA just to see..
sdl2016.bray<-as.matrix(vegdist(matrix_sdl2016_rel)) 
sdl2016.b.pcoa<-cmdscale(sdl2016.bray, eig=TRUE)
sdl2016.b.pcoa
testdf <- data.frame(scores(sdl2016.b.pcoa))
testdf$rowid <- rownames(testdf)
testdf <- left_join(testdf, sitematrix_sdl2016)
ggplot(testdf, aes(Dim1, Dim2, col = trt)) +
  geom_point() +
  ggtitle("sdl 2016 PCoA")
# shows the same thing but no pts for species bc based on community diss matrix



# -- FIGURES -----
# 1) trait pca
# choose color scheme here to color in spp in nmds points with..
traitcols <- c("Acquisitive" = "deeppink2", "Conservative" = "darkred") #springgreen3, slateblue3

pcfig <- ggplot() +
  geom_vline(aes(xintercept = 0), lty = 2, col = "grey") +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey") +
  geom_segment(data=varscores,aes(x=0,xend=PC1,y=0,yend=PC2),
               arrow = arrow(length = unit(0.25, "cm")),colour="grey40") + 
  geom_point(data = sppscores, aes(PC1, PC2, col = resource_grp), size = plottext) +
  geom_text(data = subset(varscores, PC1 <=0 & abbr != "LDMC"), aes(PC1, PC2, label = abbr), check_overlap = T, nudge_x = -0.15, size = plottext) +
  geom_text(data = subset(varscores, PC1 >0 & abbr != "Dry weight"), aes(PC1, PC2, label = abbr), check_overlap = T, nudge_x = 0.15, size = plottext) +
  geom_text(data = subset(varscores, abbr %in% c("LDMC", "Dry weight")), aes(PC1, PC2, label = abbr), check_overlap = T, nudge_x = 0.02, nudge_y = 0.1, size = plottext) +
  labs(x = "PC1 (41% variance explained)",
       y = "PC2 (20% variance explained)") +
  # describe axes
  geom_segment(aes(x=0.1,xend=2,y=(min(sppscores$PC2)-0.18),yend=(min(sppscores$PC2)-0.18)),
               arrow = arrow(length = unit(0.25, "cm")),colour="grey40", alpha = 0.4, lwd = 1) +
  geom_segment(aes(x=-0.1,xend=-1.5,y=(min(sppscores$PC2)-0.18),yend=(min(sppscores$PC2)-0.18)),
               arrow = arrow(length = unit(0.25, "cm")),colour="grey40", alpha = 0.4, lwd = 1) +
  geom_text(aes(2, (min(sppscores$PC2)-0.08), label = "Resource acquisitive"), fontface = "italic", col = "grey20", size = plottext, hjust = 1) +
  geom_text(aes(-1.5, (min(sppscores$PC2)-0.08), label = "Resource conservative"), fontface = "italic", col = "grey20", size = plottext, hjust = 0) +
  coord_fixed() +
  scale_color_manual(values = traitcols, guide = FALSE)

pcfig
ggsave(plot = pcfig, 
       filename = "alpine_addnuts/figures/pcfig.pdf")


# 2) time series NMDS plots
# specify plotting colors for all treatments
# original treatments
alltrts <- sort(unique(c(as.character(sitematrix_nn2017$trt), as.character(sitematrix_sdl2016$trt))))
trtcols <- viridis::viridis(n = length(alltrts))
names(trtcols) <- alltrts

# simplified treatments
simpletrts <- sort(unique(c(as.character(sitematrix_nn2017$trt2), as.character(sitematrix_sdl2016$trt))))
simplecols <- viridis::viridis(n = length(simpletrts))
names(simplecols) <- simpletrts


# specify plotting cols for lifeform
plantcols <- c("N-fixer" = "chocolate4", "Forb" = "grey30", "Grass" = "seagreen4", "Shrub" = "orchid")
aqcol <- "deeppink2"
concol <- "darkred"

# specify species text size
plottext <- 5


# -- sdl 2016, dry meadows ----
plot_df1 <- data.frame(nmds_sdl2016$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix_sdl2016)

spp_df1 <- data.frame(nmds_sdl2016$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  # add trait PC scores
  left_join(sppscores[c("clean_code2", "PC1", "PC2", "resource_grp")]) %>%
  # attach indicator spp results
  left_join(sdl2016_ind_df[c("clean_code2", "index", "p.value", "holm.pval")])

# capture hulls
grp1.np <- plot_df1[plot_df1$trt == "N+P", ][chull(plot_df1[plot_df1$trt == "N+P", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp1.n <- plot_df1[plot_df1$trt == "N", ][chull(plot_df1[plot_df1$trt == "N", c("MDS1", "MDS2")]), ]  # hull values for grp n
grp1.p <- plot_df1[plot_df1$trt == "P", ][chull(plot_df1[plot_df1$trt == "P", c("MDS1", "MDS2")]), ]  # hull values for grp p
grp1.c <- plot_df1[plot_df1$trt == "C", ][chull(plot_df1[plot_df1$trt == "C", c("MDS1", "MDS2")]), ]  # hull values for grp control
# stack grp dfs
grpdf1 <- rbind(grp1.np, grp1.n, grp1.p, grp1.c)

# capture envfit
vec.sdl2016<-as.data.frame(scores(fit_sdl2016, display = "vectors")) #$vectors$arrows*sqrt(fit_sdl2016$vectors$r))
vec.sdl2016$species<-rownames(vec.sdl2016)

#sdl2016_fig <- 
spp_df1$resource_grp[is.na(spp_df1$resource_grp)] <- "Unknown"
  ggplot(spp_df1, aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf1, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  geom_segment(data=vec.sdl2016,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),colour="black") + 
  # add label to envfit arrow
  geom_text(data=vec.sdl2016,aes(x=NMDS1+0.02,y=NMDS2, label = "Forb:\nGrass"), col = "black", size = plottext, fontface = "italic", hjust = 0) +
  #geom_point(aes(MDS1, MDS2, shape = simple_lifeform), col = "grey30", size = 3) + #pch = 8
  #geom_text(data = subset(spp_df1, is.na(resource_grp)), aes(MDS1, MDS2, label = substr(simple_lifeform,1,1)), col = "grey50", size = plottext) +
  # manually add in trait data species so can specify color
  #geom_point(data = subset(spp_df6, resource_grp == "Acquisitive"), aes(MDS1, MDS2), col = aqcol, size = 3, alpha  = 0.8) +
  #geom_point(data = subset(spp_df6, resource_grp == "Conservative"), aes(MDS1, MDS2), col = concol, size = 3, alpha  = 0.8) +
  geom_point(data = subset(spp_df1, !is.na(resource_grp)), aes(MDS1, MDS2, shape = resource_grp)) +
  #geom_point(data = plot_df1, aes(MDS1, MDS2,fill = trt), pch = 21) +
  geom_point(data = plot_df1, aes(MDS1, MDS2,fill = trt), pch = 21) +
  #scale_color_discrete(name = "Lifeform") +
  scale_color_manual(name = "Resource group", values = traitcols) +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols) +
  scale_shape_discrete(solid = FALSE) +
  # add figure label in top corner
  geom_text(aes(min(MDS1), max(MDS2), label = "SDL 2016"), hjust = 0, vjust = 1, col = "black") +
  # annotate any species that is a significant indicator
  geom_text(data = subset(spp_df1, holm.pval <= 0.1), aes(MDS1, MDS2, label = simple_name), nudge_y = 0.1, fontface = "italic") +
  coord_fixed() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")


# -- nutnet 2017, common plots -------
# original treatments
plot_df2 <- data.frame(nmds_nn2017$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix_nn2017)
spp_df2 <- data.frame(nmds_nn2017$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  # join trait PC scores
  left_join(sppscores[c("clean_code2", "PC1", "PC2", "resource_grp")]) %>%
  # attach indicator spp results
  left_join(nn2017_ind.simple_df[c("clean_code2", "index", "p.value", "holm.pval")])



# original treatment overlays..
grp2.np <- plot_df2[plot_df2$trt == "N+P", ][chull(plot_df2[plot_df2$trt == "N+P", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp2.npk <- plot_df2[plot_df2$trt == "N+P+K", ][chull(plot_df2[plot_df2$trt == "N+P+K", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp2.nk <- plot_df2[plot_df2$trt == "N+K", ][chull(plot_df2[plot_df2$trt == "N+K", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp2.c <- plot_df2[plot_df2$trt == "C", ][chull(plot_df2[plot_df2$trt == "C", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp2.n <- plot_df2[plot_df2$trt == "N", ][chull(plot_df2[plot_df2$trt == "N", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp2.k <- plot_df2[plot_df2$trt == "K", ][chull(plot_df2[plot_df2$trt == "K", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
# stack treatments
grpdf2 <- rbind(grp2.c, grp2.n, grp2.k, grp2.nk, grp2.np, grp2.npk)

# capture envfit
vec.nn2017<-as.data.frame(scores(fitnn2017, display = "vectors")) #$vectors$arrows*sqrt(fit_sdl2016$vectors$r))
vec.nn2017$species<-rownames(vec.nn2017)


# original treatment figure
nn2017_fig <- ggplot(spp_df2, aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf2, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  geom_segment(data=vec.nn2017,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),colour="black") + 
  # add label to envfit arrow
  geom_text(data=vec.nn2017,aes(x=NMDS1-0.02,y=NMDS2, label = "Forb:\nGrass"), col = "black", size = plottext, fontface = "italic", hjust = 1) +
  #geom_point(aes(MDS1, MDS2, col = simple_lifeform), alpha = 0.6, pch = 8) +
  geom_text(data = subset(spp_df2, is.na(resource_grp)), aes(MDS1, MDS2, label = substr(simple_lifeform,1,1)), color = "grey30", size = plottext) +
  geom_text(data = subset(spp_df2, !is.na(resource_grp)), aes(MDS1, MDS2, label = substr(simple_lifeform,1,1), col = resource_grp), fontface = "bold", size = plottext) +
  geom_point(data = plot_df2, aes(MDS1, MDS2,fill = trt), pch = 21) +
  scale_color_manual(name = "Resource", values = traitcols) +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols, drop = F) +
  coord_fixed() +
  # add figure label
  geom_text(aes(min(MDS1), max(MDS2), label = "NutNet 2017"), hjust = 0, vjust = 1, col = "black") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")


# simplified treatment
# simplified treatment overlays
grp2sim.np <- plot_df2[plot_df2$trt2 == "N+P", ][chull(plot_df2[plot_df2$trt2 == "N+P", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp2sim.c <- plot_df2[plot_df2$trt2 == "C", ][chull(plot_df2[plot_df2$trt2 == "C", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp2sim.n <- plot_df2[plot_df2$trt2 == "N", ][chull(plot_df2[plot_df2$trt2 == "N", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
# stack treatments
grpdf2sim <- rbind(grp2sim.c, grp2sim.n,grp2sim.np)

# capture envfit
vec.nn2017.sim<-as.data.frame(scores(fitnn2017.simple, display = "vectors")) #$vectors$arrows*sqrt(fit_sdl2016$vectors$r))
vec.nn2017.sim$species<-rownames(vec.nn2017.sim)


nn2017.simple_fig <- ggplot(spp_df2, aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf2sim, aes(MDS1, MDS2, fill = trt2, col = trt2), alpha = 0.4) +
  # envfit arrow
  geom_segment(data=vec.nn2017.sim,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),colour="black", lwd = 1) + 
  # add label to envfit arrow
  geom_text(data=vec.nn2017.sim,aes(x=NMDS1-0.02,y=NMDS2, label = "F:G***"), col = "black", size = plottext, fontface = "italic", hjust = 1) +
  # add species points
  geom_point(data = subset(spp_df2, is.na(resource_grp)), aes(MDS1, MDS2), col = "grey30", alpha = 0.6, size = 3, pch = 1) +
  # manually add in trait data species so can specify color
  geom_point(data = subset(spp_df2, resource_grp == "Acquisitive"), aes(MDS1, MDS2), col = aqcol, size = 3, alpha  = 0.8) +
  geom_point(data = subset(spp_df2, resource_grp == "Conservative"), aes(MDS1, MDS2), col = concol, size = 3, alpha  = 0.8) +
  # annotate any species that is a significant indicator
  geom_text(data = subset(spp_df2, holm.pval <= 0.1), aes(MDS1, MDS2, label = stringr::str_wrap(simple_name, 10), col = index), 
            fontface = "bold.italic", family = "Times New Roman", size = 4, lineheight = 0.5, nudge_y = 0.05) +
  # add figure label
  annotate(geom = "text", x= -1.2, y = 1.2, label = "NutNet 2017", hjust = 0, vjust = 1, size = plottext) +
  # add NMDS stress in upper right corner
  annotate(geom = "text", x = 1.2, y = 1.2, label = paste("Stress:", round(nmds_nn2017$stress,3)), vjust = 1, hjust = 1, size = plottext) +
  # designate treatment colors
  scale_fill_manual(name = "Plot\ntreatment", values = simplecols, drop = F) +
  scale_color_manual(name = "Plot\ntreatment", values = simplecols, drop = F) +
  coord_fixed() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")



# -- sdl 1997, dry meadow only ----
plot_df3 <- data.frame(nmds_sdl1997$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix_sdl1997)
plot_df3$trt <- factor(plot_df3$trt, levels = alltrts)

spp_df3 <- data.frame(nmds_sdl1997$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)]))%>%
  # add trait PC scores
  left_join(sppscores[c("clean_code2", "PC1", "PC2", "resource_grp")])


# capture hulls
grpdf3 <- rbind(data.frame(plot_df3[plot_df3$trt == "N+P", ][chull(plot_df3[plot_df3$trt == "N+P", c("MDS1", "MDS2")]),]),# hull values for grp n+p
                data.frame(plot_df3[plot_df3$trt == "N", ][chull(plot_df3[plot_df3$trt == "N", c("MDS1", "MDS2")]),]), # hull values for grp n
                data.frame(plot_df3[plot_df3$trt == "P", ][chull(plot_df3[plot_df3$trt == "P", c("MDS1", "MDS2")]), ]),  # hull values for grp p
                data.frame(plot_df3[plot_df3$trt == "C", ][chull(plot_df3[plot_df3$trt == "C", c("MDS1", "MDS2")]), ])) # hull values for grp control 

# capture envfit
vec.sdl1997<-as.data.frame(scores(fitsdl1997, display = "vectors")) #$vectors$arrows*sqrt(fit_sdl2016$vectors$r))
vec.sdl2016$species<-rownames(vec.sdl1997)

sdl1997_fig <- ggplot(spp_df3, aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf3, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  geom_segment(data=vec.sdl1997,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),colour="black") + 
  # add label to envfit arrow
  geom_text(data=vec.sdl1997,aes(x=NMDS1+0.02,y=NMDS2, label = "Forb:\nGrass"), col = "black", size = plottext, fontface = "italic", hjust = 0) +
  geom_text(data = subset(spp_df3, Symbol != "STLO2" & is.na(resource_grp)), aes(MDS1, MDS2, label = substr(simple_lifeform,1,1)), col = "grey30", size = plottext) +
  # plot STLO2 separately bc overplots with grass in top right corner of plot
  geom_text(data = subset(spp_df3, Symbol == "STLO2"), aes(MDS1, MDS2, label = substr(simple_lifeform,1,1)), col = "grey30", size = plottext, nudge_y = -0.1) +
  # add text for spp with PC score
  geom_text(data = subset(spp_df3, !is.na(resource_grp)), aes(MDS1, MDS2, label = substr(simple_lifeform,1,1), col = resource_grp), size = plottext, fontface = "bold") +
  geom_point(data = plot_df3, aes(MDS1, MDS2,fill = trt), pch = 21) +
  #scale_color_discrete(name = "Lifeform") +
  scale_color_manual(name = "Resource", values = traitcols, guide = FALSE) +
  scale_fill_manual(name = "Treatment", values = trtcols) +
  coord_fixed() +
  #guides(guide_legend(override.aes = list(pch =21))) +
  #scale_shape_manual(values = c("Dry" = 21, "Mesic" = 24, "Unknown" = 22)) +
  geom_text(aes(min(MDS1), max(MDS2),label = "SDL 1997"), hjust = 0) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        # add legend here
        legend.position = c(0.89,0.19),
        legend.background = element_rect(color = "black", fill = NULL),
        legend.title = element_text(size = 9))

sdl1997_fig


# ---- sdl 2012, dry meadow only ----
plot_df4 <- data.frame(nmds_sdl2012$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix_sdl2012)

spp_df4 <- data.frame(nmds_sdl2012$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  # add trait PC scores
  left_join(sppscores[c("clean_code2", "PC1", "PC2", "resource_grp")])

# capture hulls
grpdf4 <- rbind(data.frame(plot_df4[plot_df4$trt == "N+P", ][chull(plot_df4[plot_df4$trt == "N+P", c("MDS1", "MDS2")]),]),# hull values for grp n+p
                data.frame(plot_df4[plot_df4$trt == "N", ][chull(plot_df4[plot_df4$trt == "N", c("MDS1", "MDS2")]),]), # hull values for grp n
                data.frame(plot_df4[plot_df4$trt == "P", ][chull(plot_df4[plot_df4$trt == "P", c("MDS1", "MDS2")]), ]),  # hull values for grp p
                data.frame(plot_df4[plot_df4$trt == "C", ][chull(plot_df4[plot_df4$trt == "C", c("MDS1", "MDS2")]), ])) # hull values for grp control 


sdl2012_fig <- ggplot(spp_df4, aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf4, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  geom_text(data = subset(spp_df4, is.na(resource_grp)), aes(MDS1, MDS2, label = substr(simple_lifeform,1,1)), col = "grey30", size = plottext) +
  geom_text(data = subset(spp_df4, !is.na(resource_grp)), aes(MDS1, MDS2, label = substr(simple_lifeform,1,1), col  = resource_grp), size = plottext, fontface = "bold") +
  geom_point(data = plot_df4, aes(MDS1, MDS2,fill = trt), pch = 21) +
  #scale_color_discrete(name = "Lifeform") +
  scale_color_manual(name = "Resource", values = traitcols) +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols) +
  #guides(guide_legend(override.aes = list(pch =21))) +
  #scale_shape_manual(values = c("Dry" = 21, "Mesic" = 24, "Unknown" = 22)) +
  geom_text(aes(min(MDS1), max(MDS2), label = "SDL 2012"), hjust = 0, nudge_y = -0.05) +
  coord_fixed() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none")




# ---- nutnet 2013, common plots only -----
plot_df6 <- data.frame(nmds_nn2013$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix_nn2013)
plot_df6$trt <- factor(plot_df6$trt, levels = alltrts)
plot_df6$trt2 <- factor(plot_df6$trt2, levels = simpletrts)
spp_df6 <- data.frame(nmds_nn2013$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  # join trait PC scores
  left_join(sppscores[c("clean_code2", "PC1", "PC2", "resource_grp")]) %>%
  # join indicator analysis results
  left_join(nn2013_ind.simple_df[c("clean_code2", "index", "p.value", "holm.pval")])

# capture hulls
grpdf6 <- rbind(data.frame(plot_df6[plot_df6$trt == "N+P", ][chull(plot_df6[plot_df6$trt == "N+P", c("MDS1", "MDS2")]),]),# hull values for grp n+p
                data.frame(plot_df6[plot_df6$trt == "N+P+K", ][chull(plot_df6[plot_df6$trt == "N+P+K", c("MDS1", "MDS2")]),]), # hull values for grp n+p+k
                data.frame(plot_df6[plot_df6$trt == "N+K", ][chull(plot_df6[plot_df6$trt == "N+K", c("MDS1", "MDS2")]),]),# hull values for grp n+p
                data.frame(plot_df6[plot_df6$trt == "N", ][chull(plot_df6[plot_df6$trt == "N", c("MDS1", "MDS2")]),]), # hull values for grp n
                data.frame(plot_df6[plot_df6$trt == "K", ][chull(plot_df6[plot_df6$trt == "K", c("MDS1", "MDS2")]), ]),  # hull values for grp k
                data.frame(plot_df6[plot_df6$trt == "C", ][chull(plot_df6[plot_df6$trt == "C", c("MDS1", "MDS2")]), ])) # hull values for grp control 

# capture envfit
vec.nn2013<-as.data.frame(scores(fitnn2013, display = "vectors")) #$vectors$arrows*sqrt(fit_sdl2016$vectors$r))
vec.nn2013$species<-rownames(vec.nn2013)


# simplified treatments
grpdf6.simple <- rbind(data.frame(plot_df6[plot_df6$trt2 == "N+P", ][chull(plot_df6[plot_df6$trt2 == "N+P", c("MDS1", "MDS2")]),]),# hull values for grp n+p
                data.frame(plot_df6[plot_df6$trt2 == "N", ][chull(plot_df6[plot_df6$trt2 == "N", c("MDS1", "MDS2")]),]), # hull values for grp n
                data.frame(plot_df6[plot_df6$trt2 == "C", ][chull(plot_df6[plot_df6$trt2 == "C", c("MDS1", "MDS2")]), ])) # hull values for grp control 

# capture envfit
vec.nn2013.simple <-as.data.frame(scores(fitnn2013.simple, display = "vectors")) #$vectors$arrows*sqrt(fit_sdl2016$vectors$r))
vec.nn2013$species<-rownames(vec.nn2013.simple)


# original figure
nn2013_fig <- ggplot(spp_df6, aes(MDS1, MDS2)) + 
  #stat_ellipse(data = grpdf6, aes(MDS1, MDS2, col = trt)) +
  geom_polygon(data = grpdf6, aes(MDS1, MDS2, fill = trt), alpha = 0.5) +
  #geom_point(aes(MDS1, MDS2, col = simple_lifeform), alpha = 0.6, pch = 8) +
  geom_text(data = subset(spp_df6, is.na(resource_grp)), aes(MDS1, MDS2, label = substr(simple_lifeform,1,1)), col = "grey30", size = plottext) +
  geom_text(data = subset(spp_df6, !is.na(resource_grp)), aes(MDS1, MDS2, label = substr(simple_lifeform,1,1), col = resource_grp), size = plottext, fontface = "bold") +
  geom_point(data = plot_df6, aes(MDS1, MDS2,fill = trt), pch = 21) +
  geom_segment(data=vec.nn2013,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),colour="black") + 
  # add label to envfit arrow
  geom_text(data=vec.nn2013,aes(x=NMDS1-0.02,y=NMDS2, label = "Forb:\nGrass"), col = "black", size = plottext, fontface = "italic", hjust = 1) +
  #scale_color_discrete(name = "Lifeform") +
  scale_color_manual(name = "Resource", values = traitcols, guide = FALSE) +
  scale_fill_manual(name = "Treatment", values = trtcols) +
  geom_text(aes(min(MDS1), max(MDS2), label = "NutNet 2013"), hjust = 0) +
  coord_fixed() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.87, 0.26),
        legend.background = element_rect(color = "black", fill = NULL),
        legend.title = element_text(size = 9))


# simplified treatments fig
#nn2013.simple_fig <- 
  ggplot(spp_df6, aes(MDS1, MDS2)) + 
  geom_polygon(data = grpdf6.simple, aes(MDS1, MDS2, fill = trt2, col = trt2), alpha = 0.4) +
  # envfit arrow
  geom_segment(data=vec.nn2013.simple,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),colour="black", lwd = 1) + 
  # add label to envfit arrow
  geom_text(data=vec.nn2013.simple,aes(x=NMDS1-0.02,y=NMDS2, label = "F:G***"), col = "black", size = plottext, fontface = "italic", hjust = 1) +
  # add species points
  geom_point(data = subset(spp_df6, is.na(resource_grp)), aes(MDS1, MDS2), col = "grey30", alpha = 0.6, size = 3, pch = 1) +
  # manually add in trait data species so can specify color
  geom_point(data = subset(spp_df6, resource_grp == "Acquisitive"), aes(MDS1, MDS2), col = aqcol, size = 3, alpha  = 0.8) +
  geom_point(data = subset(spp_df6, resource_grp == "Conservative"), aes(MDS1, MDS2), col = concol, size = 3, alpha  = 0.8) +
  # annotate any species that is a significant indicator
  geom_text(data = subset(spp_df6, holm.pval <= 0.1), aes(MDS1, MDS2, label = stringr::str_wrap(simple_name, 10), col = index), 
            fontface = "bold.italic", family = "Times New Roman", size = 4, lineheight = 0.5, nudge_y = 0.05) +
  # add figure label
  annotate(geom = "text", x= -1.2, y = 1.2, label = "NutNet 2013", hjust = 0, vjust = 1, size = plottext) +
  # add NMDS stress in upper right corner
  annotate(geom = "text", x = 1.2, y = 1.2, label = paste("Stress:", round(nmds_nn2013$stress,3)), vjust = 1, hjust = 1, size = plottext) +
  # designate treatment colors
  scale_fill_manual(name = "Treatment", values = simplecols, drop = F) +
  scale_color_manual(name = "Treatment", values = simplecols, drop = F) +
  coord_fixed() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.87, 0.15),
        legend.background = element_rect(color = "black", fill = NULL),
        legend.title = element_text(size = 9))



# -- full panel plot -----
# 6-panel plot of everything
full_panel <- plot_grid(pcfig, nn2013_fig, nn2017_fig,
                        sdl1997_fig, sdl2012_fig, sdl2016_fig,
                        nrow = 2,
                        align = "vh")
full_panel
ggsave(plot = full_panel, 
       filename = "alpine_addnuts/figures/pca_nmds_fullpanel.pdf", scale = 2,
       width = 6, heigh = 4, units = "in")
