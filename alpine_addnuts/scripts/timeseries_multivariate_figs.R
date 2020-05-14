# reworked multivariate analyses, and cleaned up figs for ms round 2 (may 2020)

# script purpose:
# plot full time series, per site, in their own NMDS figures, showing trait PCA values for species where available
# specifically:
# NutNet directional trends NMDS (2013 and 2017, commonly sampled plots only)
# NutNet spp ordination trends with treatment ellipses
# SDL directional trends NMDS (1997, 2003, 2005, 2012, 2016, commonly sampled dry meadow plots)
# SDL spp ordination trends with treatment ellipses

# also remake PERMANOVA models, permuting time to account for temporal dependence in observations
# > PERMANOVA as is will freely permute observations, without preserving temporal dependence in observations
# because PERMANOVA sensitive to heterogeneity among groups when design unbalanced, need to drop N+P+K and other K treatments
# > only want 4 reps per treat
# > also because only 1 treatment plot per block (usually, except for Control in NutNet, can ignore block)

# can also, if desired, redo indicator analysis by treatment, all years.. (don't feel super excited about this, but maybe)


# adopt code from .Rmd used to generate initial figures for manuscript submitted to Plant Ecology


# -- SETUP ----
rm(list = ls())
# setup environment
library(tidyverse)
library(vegan)
library(cowplot)
source("edi_functions.R")
na_vals <- c("", " ", NA, "NA", "NaN", NaN, ".")
options(stringsAsFactors = F, strip.white = T, na.strings = na_vals)
theme_set(theme_bw())


# read in needed datasets
coarse_summary <- read.csv("alpine_addnuts/output_data/forTS/sdl_nutnet_fxnl_biodiv_anpp_1997-ongoing.csv")
plantcom <- read.csv("alpine_addnuts/output_data/forTS/sdl_nutnet_sppcomp_1997-ongoing.csv")
sdlplots <- read.csv("alpine_addnuts/output_data/sdl_plot_lookup.csv") 
nnplots <- read.csv("alpine_addnuts/output_data/nutnet_plot_lookup.csv")
spplist <- read.csv("alpine_addnuts/output_data/sdl_nutnet_spplookup.csv")

# read in corrections for lumping spp (per JGS)
sppcorrect <- read.csv("alpine_addnuts/output_data/sdlnn_spplist_jgs_corrections.csv", na.strings = na_vals) %>%
  filter(!is.na(clean_code2)) %>%
  mutate(lump_code = ifelse(is.na(lump_code), clean_code2, lump_code))
# check all lump codes in clean code (no typos)
summary(sppcorrect$lump_code %in% sppcorrect$clean_code2) # yes

# marko and soren's trait dataset -- will just consider saddle spp
traitdat <- getTabular(500) %>% data.frame()

# review read in as expected
glimpse(coarse_summary)
glimpse(plantcom)

# prep site info
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


# prep spp list
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



# specify plotting specs
# specify plotting colors for all treatments
# original treatments
alltrts <- sort(unique(nnplots$trt))
trtcols <- viridis::viridis(n = length(alltrts))
names(trtcols) <- alltrts

# simplified treatments
simpletrts <- sort(unique(c(as.character(sdlplots$trt))))
## cols for trts
simplecols <- viridis::viridis(n = length(simpletrts))
names(simplecols) <- simpletrts
## line types for simple trts
simplelines <- c("C" = 1 , "N" = 2, "N+P"= 3, "P"= 4)
# 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash

# choose color scheme here to color in spp in nmds points with.. subset viridis cols
#traitcols <- c("Acquisitive" = "deeppink2", "Conservative" = "darkred", "Unknown" = "grey50")
#traitcols <- c("Acquisitive" = "grey20", "Conservative" = "grey80", "Unknown" = "black")
traitcols <- c("#FCA636FF", "chocolate4", "grey10") # add dark color for outline of unknown resource spp
names(traitcols) <- c("Acquisitive", "Conservative", "Unknown")

# create plotting them for journal (target = Plant Ecol)
j_theme <- theme(axis.text = element_text(size = 10),
                 axis.title = element_text(size = 12),
                 legend.text = element_text(size = 10),
                 legend.title = element_text(size = 10),
                 strip.text = element_text(size = 10),
                 panel.grid = element_blank(),
                 strip.background = element_rect(fill = "transparent"))


# -- TRAIT PCA -----
# subset dry meadow spp
traits_dm <- subset(traitdat, Exp == "SAD" & TRT == "DRY") %>%
  arrange(Year_Collected, USDA.Code, Rep)

# specify traits to use
trts <- colnames(traits_dm)[c(20, 25, 27:33)] # 16 = Oheight, 23 = dry weight -- not keeping bc using logged vars
trts 

# average dm trait vals and run through pca just to see..
meantraits <- traits_dm %>%
  # log transform physical traits
  mutate(ln_ohgt = log(OHeight),
         ln_drywgt = log(DryWeight),
         ln_leafarea = log(LeafArea)) %>%
  dplyr::select(Latin.name:Code, trts, ln_ohgt, ln_drywgt, ln_leafarea) %>%
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
                       ln_leafarea = "Leaf area",
                       Percent_C = "%C",
                       Percent_N = "%N"))


# -- PREP COVER DAT -----
# split out sdl and nutnet so can average C and N+P+K
sdl_coarse <- subset(coarse_summary, site == "sdl") %>%
  # select only dry meadow plots consistently sampled
  # subset to dry meadow plots surveyed across all years only (corresponds to what was surveyed in 1997)
  ## should be plots 1-16
  subset(plotid %in% sdl_common) %>%
  # make treatment a factor
  mutate(trt = factor(trt, levels = c("C", "N", "P", "N+P")))

# average by treatment -- average all mets, then subset for figs and tables as needed
sdl_coarse_means <- sdl_coarse %>%
  gather(grp, val, ForbHits:ncol(.)) %>%
  group_by(site, yr, trt, meadow, snow, grp) %>%
  summarise(meanval = mean(val),
            seval = sd(val)/sqrt(length(val)),
            nobs = length(val)) %>%
  ungroup() %>%
  # calculate yrs passed since start of exp
  mutate(post_yrs = yr - 1993)


# nutnet
## need to average N+P and C by block first (2 reps per block)
nn_coarse_blockmeans <- subset(coarse_summary, site == "nutnet") %>% 
  # select only plots consistently sampled both years
  #subset(plotid %in% c(nn_common, nn_Pplots)) %>% 
  # modify N+K plot to N in 2017 so block 4 has a 4th N trtment
  mutate(trt = ifelse(yr == 2017 & grepl("B4", plotid) & trt == "N+K", "N", trt)) %>%
  gather(met, val, ForbHits:ncol(.)) %>%
  # average reps in C and N+P+K by block
  group_by(site, yr, block, trt, trt2, met) %>%
  summarise(blockmean = mean(val),
            # to verify 2 obs per C and N+P+K per block
            nobs = length(val)) %>% # yes (checked manually)
  ungroup()

# subset to just C, N and N+P for fig2 and anova
nn_fg_dat <- subset(nn_coarse_blockmeans, met %in% c("Forb_rel", "Grass_rel", "Geum_rel", "ForbHits", "GrassHits") & trt %in% c("C", "N", "P", "N+P")) %>%
  mutate(trt2 = factor(trt2, levels = c("C", "N", "P", "N+P")))

# calculate table of means and ses
nn_fg_means <- data.frame(nn_fg_dat) %>%
  group_by(site, yr, trt2, met) %>%
  summarise(meancov = mean(blockmean),
            secov = sd(blockmean)/sqrt(4),
            nobs = length(blockmean)) %>%
  ungroup() %>%
  # calculate yrs passed since start of exp
  mutate(post_yrs = yr - 2008) %>%
  as.data.frame()

# prep abundance
abundance <- subset(plantcom, hits > 0.25 & simple_lifeform != "Ground cover") %>%
  dplyr::select(site, yr, plotid, block, plot, trt, trt2, clean_code2, hits)

# make wide form
abundance_wide <- spread(abundance, clean_code2, hits, fill = 0) %>%
  unite(rowid, site, yr, plotid, sep = "_", remove = F)

# relativize -- some sites will have extra spp because spread out all plantcom data from both sites
relabundance <- abundance_wide
rownames(relabundance) <- abundance_wide$rowid
# plantdat begins after trt2
relabundance <- decostand(relabundance[,(which(names(relabundance) == "trt2")+1):ncol(relabundance)], "total")
# multiple by 100 to get % relative cover value
relabundance <- relabundance*100


# per KNS suggestion, screen for common/easy to ID species
common_abundance <- subset(plantcom, hits > 0.25 & simple_lifeform != "Ground cover") %>%
  dplyr::select(site, yr, plotid, block, plot, trt, trt2, clean_code2, simple_name, hits) %>%
  # keep just commonly surveyed sites for nmds and permanova
  filter(plotid %in% c(sdl_common, nn_common))

# tally spp by site and year
commonspp <- common_abundance %>%
  group_by(site, yr, clean_code2) %>%
  mutate(site_yr = length(hits)) %>%
  ungroup() %>%
  # how many years?
  group_by(site, clean_code2) %>%
  mutate(nyrs = length(unique(yr))) %>%
  ungroup() %>%
  group_by(site, trt2, clean_code2) %>%
  mutate(site_trt = length(hits)) %>%
  ungroup() %>%
  select(site, yr, clean_code2, simple_name, nyrs, site_yr, site_trt) %>%
  distinct()



# who are the most commonly occuring spp at sdl?
keep <- sort(unique(commonspp$simple_name[commonspp$nyrs > 2]))

# after looking at data..
# collapse campanula rotundiflora and uniflora
# collapse all poa spp
# also there's only 1 type of festuca so make festuca sp. brachyphylla
# collapse all viola
# juncus was only found in 2012 at saddle.. should probable be carex
# also need to screen for spp within genera that were only found once while other spp in same genus were found in other years


# -- STANDARDIZE SPP DAT -----
# want for loop that iterates through every plot and every spp
# first, lump spp per jgs suggestion
common_abundance <- left_join(common_abundance, sppcorrect[c("clean_code2", "lump_code")]) %>%
  # fill in NAs for lump_code and re-join simple names
  mutate(lump_code = ifelse(is.na(lump_code), clean_code2, lump_code)) %>%
  dplyr::select(-c(clean_code2, simple_name)) %>%
  rename(clean_code2 = lump_code) %>%
  left_join(distinct(spplist[c("clean_code2", "simple_name")])) %>%
  # break out genus and spp
  mutate(genus = trimws(str_extract(simple_name, "[A-Z][a-z]+ ")),
         species = trimws(str_extract(simple_name, " [a-z]+$"))) %>%
  data.frame() %>%
  # sum by spp ID
  grouped_df(names(.)[!grepl("hits", names(.))]) %>%
  summarize(hits = sum(hits)) %>%
  ungroup()

# now, go through each plot and spp, and for genera where multiple spp exist, if 1 only appears once and otherwise consistent, change to most commonly occurring spp
# id the genera with multiple names
common_genera <- distinct(common_abundance[c("genus", "species")]) %>%
  group_by(genus) %>%
  mutate(nspp = length(unique(species))) %>%
  ungroup() %>%
  filter(nspp > 1)

# these three spp fall near the center line for resource grpings..
boxplot(plantcom$hits[plantcom$clean_code2 == "DECE"] ~ plantcom$meadow[plantcom$clean_code2 == "DECE"]) 
boxplot(plantcom$hits[plantcom$clean_code2 == "CARUD"] ~ plantcom$meadow[plantcom$clean_code2 == "CARUD"])
boxplot(plantcom$hits[plantcom$clean_code2 == "TEAC"] ~ plantcom$meadow[plantcom$clean_code2 == "TEAC"])

# looks like it's really just the carices that are potentially problematic
# subset carex from spp comp to troubleshooot then add back in
carexcomp <- subset(common_abundance, genus == "Carex")
# pull unique carex
carexspp <- sort(unique(carexcomp$clean_code2))
# look at just saddle, since that would be the problematic site
View(subset(carexcomp, site == "sdl"))
# it actually sees fine.. there's enough consistency within plots across years i'm not going to fuss with anything
# proceed with common_abundnace file

# re-tally common spp if need to subset nmds
# tally spp by site and year
commonspp <- common_abundance %>%
  dplyr::select(-trt) %>%
  distinct() %>%
  group_by(site, yr, clean_code2) %>%
  mutate(plots_yr = length(hits)) %>%
  ungroup() %>%
  # how many years?
  group_by(site, clean_code2) %>%
  mutate(nyrs = length(unique(yr))) %>%
  ungroup() %>%
  group_by(site, trt2, clean_code2) %>%
  mutate(plots_trt = length(hits)) %>%
  ungroup() %>%
  select(site, yr, trt2, clean_code2, simple_name, nyrs, plots_yr, plots_trt) %>%
  distinct()



# -- MULTIPANEL MEAN COVER FIGURE ----
# stack sdl and nutnet to plot means and ses by yr since trts initiated
all_fg_means <- rename(nn_fg_means, trt = trt2, grp = met, meanval = meancov, seval = secov) %>%
  rbind(dplyr::select(sdl_coarse_means, -c(meadow, snow))) %>%
  filter(grp %in% c("Forb_rel", "Grass_rel", "Geum_rel"))


# plot means by time since experiment onset
allcov_fig <- mutate(all_fg_means, grp = factor(grp, levels = c("Forb_rel", "Grass_rel", "Geum_rel"), labels = c("All forbs", "All grasses", "G. rossii"))) %>%
  ggplot(aes(post_yrs, meanval, grp = site)) +
  geom_errorbar(aes(ymax = meanval + seval, ymin = meanval - seval), width = 0) +
  geom_point(aes(shape = site), fill = "white") +
  #geom_line(aes(col = site)) +
  scale_y_continuous(breaks = seq(0,80, 10)) +
  scale_shape_manual(name = NULL, values = c(21,19), labels = c("NutNet", "Saddle")) +
  labs(y = "Mean relative cover (%)",
       x = "Years since experiment onset") +
  theme(legend.position = c(0.001, 0.313),
        legend.justification = c(0,1),
        legend.margin = margin(0.1,2.5,1.5,2, "pt"),
        legend.background = element_rect(color = "black"),
        legend.key = element_blank(),
        legend.key.size = unit(9, "pt")) +
  j_theme + 
  facet_grid(grp~trt, scales = "free_y")

allcov_fig

# write out as tiff
ggsave("alpine_addnuts/figures/journal_figs/all_meancov.tiff", allcov_fig, dpi = 320,
       width = 6, height = 4, units = "in")

# write out as pdf
ggsave("alpine_addnuts/figures/journal_figs/all_meancov2.pdf", allcov_fig, dpi = 320,
       width = 6, height = 4, units = "in")


# -- PCA FIGURE (MAIN AND SUPPLEMENT) ----
# gather proportional importance for axes labels
PCexplain <- data.frame(summary(traitpc)$cont$importance) %>%
  rownames_to_column()

# specify species text size
plottext <- 3.5
pointsize <- 3

pcfig <- ggplot() +
  geom_vline(aes(xintercept = 0), lty = 2, col = "grey") +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey") +
  geom_segment(data=varscores,aes(x=0,xend=PC1,y=0,yend=PC2),
               arrow = arrow(length = unit(0.25, "cm")),colour="grey40") + 
  geom_point(data = sppscores, aes(PC1, PC2, col = resource_grp), size = pointsize) +
  geom_text(data = subset(varscores, PC1 <= 0), aes(PC1, PC2, label = abbr), check_overlap = F, hjust = 1, nudge_x = -0.02, size = plottext) +
  geom_text(data = subset(varscores, PC1 > 0 & !grepl("%N|Leaf|Dry", abbr)), aes(PC1, PC2, label = abbr), check_overlap = F, hjust = 0, nudge_x = 0.02, size = plottext) +
  geom_text(data = subset(varscores, PC1 > 0 & grepl("Dry", abbr)), aes(PC1, PC2, label = abbr), check_overlap = F, hjust = 0, nudge_y = +0.05, size = plottext) +
  geom_text(data = subset(varscores, PC1 > 0 & grepl("Leaf", abbr)), aes(PC1, PC2, label = abbr), check_overlap = F, hjust = 0, nudge_y = -0.06,nudge_x = 0.02, size = plottext) +
  geom_text(data = subset(varscores,abbr == "%N"), aes(PC1, PC2, label = abbr), check_overlap = T, hjust = 0.5, nudge_y = -0.1, size = plottext) +
  # add axis labels with % variance displayed
  labs(x = paste0("PC1 (",round(PCexplain$PC1[grepl("^Prop", PCexplain$rowname)],2)*100, "% variance explained)"),
       y = paste0("PC2 (",round(PCexplain$PC2[grepl("^Prop", PCexplain$rowname)],2)*100, "% variance explained)")) +
  # describe axes
  geom_segment(aes(x=0.1,xend=2,y=(min(sppscores$PC2)-0.18),yend=(min(sppscores$PC2)-0.18)),
               arrow = arrow(length = unit(0.25, "cm")),colour="grey40", alpha = 0.4, lwd = 0.75) +
  geom_segment(aes(x=-0.1,xend=-1.5,y=(min(sppscores$PC2)-0.18),yend=(min(sppscores$PC2)-0.18)),
               arrow = arrow(length = unit(0.25, "cm")),colour="grey40", alpha = 0.4, lwd = 0.75) +
  geom_text(aes(2, (min(sppscores$PC2)-0.08), label = "Resource\nacquisitive"), fontface = "italic", col = "grey20", size = plottext, hjust = 1, vjust = 0, lineheight = 0.75) +
  geom_text(aes(-1.5, (min(sppscores$PC2)-0.08), label = "Resource\nconservative"), fontface = "italic", col = "grey20", size = plottext, hjust = 0, vjust = 0, lineheight = 0.75) +
  #coord_fixed() +
  theme(panel.grid = element_blank()) +
  j_theme +
  scale_color_manual(values = traitcols, guide = FALSE)


pcfig
# write out as tiff
ggsave("alpine_addnuts/figures/journal_figs/pca_nospp.tiff", pcfig, dpi = 320,
       width = 4, height = 4, units = "in")
# write out as pdf
ggsave("alpine_addnuts/figures/journal_figs/pca_nospp.pdf", pcfig, dpi = 320,
       width = 4, height = 4, units = "in")


## PCA with spp labels
# specify transparency for non-spp labels
fade <- 0.3
plottext_pca <- 2.8

pcfig_wspp <- ggplot() +
  geom_vline(aes(xintercept = 0), lty = 2, col = "grey") +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey") +
  geom_segment(data=varscores,aes(x=0,xend=PC1,y=0,yend=PC2),
               arrow = arrow(length = unit(0.25, "cm")),colour="grey40", alpha = fade) + 
  geom_point(data = sppscores, aes(PC1, PC2, col = resource_grp)) +
  # add spp labels
  ## spp on positive x, positive y
  geom_text(data = subset(sppscores, PC1 >0 & PC2 > 0 & !grepl("Tetra", Latin.name)), aes(PC1, PC2, label = str_wrap(Latin.name, width = 10)), size = plottext_pca, fontface = "italic", color = "black", lineheight = 0.75, vjust = -0.3) +
  geom_text(data = subset(sppscores, PC1 >0 & PC2 > 0 & grepl("Tetra", Latin.name)), aes(PC1, PC2, label = str_wrap(Latin.name, width = 10)), size = plottext_pca, fontface = "italic", color = "black", lineheight = 0.75, vjust = -0.3, nudge_x = -0.1) +
  ## spp on positive x, negative y
  geom_text(data = subset(sppscores, PC1 >0 & PC2 <= 0 & grepl("Poly", Latin.name)), aes(PC1, PC2, label = str_wrap(Latin.name, width = 10)), size = plottext_pca, fontface = "italic", color = "black", lineheight = 0.75, vjust = 1.2, nudge_x = -0.17) +
  ## plot ARTSCO separately (+,-)
  geom_text(data = subset(sppscores, PC1 >0 & PC2 <= 0 & grepl("Artem", Latin.name)), aes(PC1, PC2, label = str_wrap(Latin.name, width = 10)), size = plottext_pca, fontface = "italic", color = "black", lineheight = 0.75, hjust = 0, vjust = -0.3) +
  ## plot TRIDAS (+,-) separately
  geom_text(data = subset(sppscores, grepl("dasyph", Latin.name)), aes(PC1, PC2, label = str_wrap(Latin.name, width = 10)), size = plottext_pca, fontface = "italic", color = "black", lineheight = 0.75, hjust = 0, vjust = 1.3) +
  ## spp on neg x, pos y
  geom_text(data = subset(sppscores, PC1 <= 0 & PC2 > 0 & !grepl("Festuca|Kobre", Latin.name)), aes(PC1, PC2, label = str_wrap(Latin.name, width = 10)), size = plottext_pca, fontface = "italic", color = "black", lineheight = 0.75, hjust = 1, nudge_x = -0.04) +
  ## plot Festuca separately (-,+)
  geom_text(data = subset(sppscores, PC1 <= 0 & PC2 > 0 & grepl("Festuca", Latin.name)), aes(PC1, PC2, label = str_wrap(Latin.name, width = 10)), size = plottext_pca, fontface = "italic", color = "black", lineheight = 0.75, vjust = -0.3, hjust = 0.2, nudge_x = -0.01) +
  ## plot Kobresia separately (-,+)
  geom_text(data = subset(sppscores, PC1 <= 0 & PC2 > 0 & grepl("Kobre", Latin.name)), aes(PC1, PC2, label = str_wrap(Latin.name, width = 10)), size = plottext_pca, fontface = "italic", color = "black", lineheight = 0.75, vjust = -0.3, hjust = 1, nudge_x = 0.01) +
  ## spp on negative x, neg y
  geom_text(data = subset(sppscores, PC1 <= 0 & PC2 < 0 & !grepl("Silene|Lloyd|Tetra", Latin.name)), aes(PC1, PC2, label = str_wrap(Latin.name, width = 10)), size = plottext_pca, fontface = "italic", color = "black", lineheight = 0.75, vjust = 1.3) +
  ## adjust Lloydi and Tetraneuris left (-,-)
  geom_text(data = subset(sppscores, PC1 <= 0 & PC2 < 0 & grepl("Tetr", Latin.name)), aes(PC1, PC2, label = str_wrap(Latin.name, width = 10)), size = plottext_pca, fontface = "italic", color = "black", lineheight = 0.75, hjust = 1, nudge_x = -0.06) +
  geom_text(data = subset(sppscores, PC1 <= 0 & PC2 < 0 & grepl("Lloy", Latin.name)), aes(PC1, PC2, label = str_wrap(Latin.name, width = 10)), size = plottext_pca, fontface = "italic", color = "black", lineheight = 0.75, hjust = 1, nudge_x = -0.1) +
  ## plot silene (-,-) separately
  geom_text(data = subset(sppscores, PC1 <= 0 & PC2 < 0 & grepl("Silene", Latin.name)), aes(PC1, PC2, label = str_wrap(Latin.name, width = 10)), size = plottext_pca, fontface = "italic", color = "black", lineheight = 0.75, vjust = -0.3) +
  # label traits
  geom_text(data = subset(varscores, PC1 <= 0), aes(PC1, PC2, label = abbr), check_overlap = F, hjust = 1, nudge_x = -0.02, size = plottext_pca, alpha = fade) +
  geom_text(data = subset(varscores, PC1 > 0 & !grepl("%N|Leaf|Dry", abbr)), aes(PC1, PC2, label = abbr), check_overlap = F, hjust = 0, nudge_x = 0.02, size = plottext_pca, alpha = fade) +
  geom_text(data = subset(varscores, PC1 > 0 & grepl("Dry", abbr)), aes(PC1, PC2, label = abbr), check_overlap = F, hjust = 0, nudge_y = +0.05, size = plottext_pca, alpha = fade) +
  geom_text(data = subset(varscores, PC1 > 0 & grepl("Leaf", abbr)), aes(PC1, PC2, label = abbr), check_overlap = F, hjust = 0, nudge_y = -0.06,nudge_x = 0.02, size = plottext_pca, alpha = fade) +
  geom_text(data = subset(varscores,abbr == "%N"), aes(PC1, PC2, label = abbr), check_overlap = T, hjust = 0.5, nudge_y = -0.1, size = plottext_pca, alpha = fade) +
  # add axis labels with % variance displayed
  labs(x = paste0("PC1 (",round(PCexplain$PC1[grepl("^Prop", PCexplain$rowname)],2)*100, "% variance explained)"),
       y = paste0("PC2 (",round(PCexplain$PC2[grepl("^Prop", PCexplain$rowname)],2)*100, "% variance explained)")) +
  # describe axes
  geom_segment(aes(x=0.1,xend=2,y=(min(sppscores$PC2)-0.18),yend=(min(sppscores$PC2)-0.18)),
               arrow = arrow(length = unit(0.25, "cm")),colour="grey40", alpha = 0.4, lwd = 0.75) +
  geom_segment(aes(x=-0.1,xend=-1.5,y=(min(sppscores$PC2)-0.18),yend=(min(sppscores$PC2)-0.18)),
               arrow = arrow(length = unit(0.25, "cm")),colour="grey40", alpha = 0.4, lwd = 0.75) +
  geom_text(aes(2, (min(sppscores$PC2)-0.08), label = "Resource\nacquisitive"), col = "grey20", size = 3, hjust = 1, vjust = 0, lineheight = 0.75) +
  geom_text(aes(-1.5, (min(sppscores$PC2)-0.08), label = "Resource\nconservative"), col = "grey20", size = 3, hjust = 0, vjust = 0, lineheight = 0.75) +
  coord_fixed() +
  theme(panel.grid = element_blank()) +
  j_theme +
  scale_color_manual(values = traitcols, guide = FALSE)


pcfig_wspp
# write out as tiff
ggsave("alpine_addnuts/figures/journal_figs/pca_wspp_supp.tiff", pcfig_wspp, dpi = 320,
       width = 4, height = 4, units = "in")
# write out as pdf
ggsave("alpine_addnuts/figures/journal_figs/pca_wspp_supp.pdf", pcfig_wspp,dpi = 320,
       width = 4, height = 4, units = "in")


# -- MULTIVAR PREP ----
# create function for storing ellipse (from: https://stackoverflow.com/questions/13794419/plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-created-in-ggplo)
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100){
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

# procedure:
## subset data to common plots only so can compare across yrs
## sdl data can be run through as they are (C, N, P, N+P)
## nutnet data need to be block-averaged (2 reps of C and N+P+K in each block)
## then different iterations of nutnet run:
## 1) C, N, P and N+P only
## 2) all trts (+K)
## 3) K-collapsed trts (e.g. N+P+K becomes N+P) [this pertains to 2013 in particular bc no K added then, just micronuts]
# prep abundance data to feed through for loop by site, add on a col to keep track of iteration, then feed through for-loop

# relativize abundance in common plots
# make wide form
commabundance_wide <- common_abundance %>%
  # drop plant descriptive cols
  dplyr::select(-c(simple_name, genus, species)) %>%
  spread(clean_code2, hits, fill = 0) %>%
  unite(rowid, site, yr, plotid, sep = "_", remove = F)

# do by site
##  1) NutNet NMDS and PERMANOVA ----
nn_wide <- subset(commabundance_wide) %>%
  filter(site == "nutnet")
# tally non-zero counts by sp
nn_sppcounts <- sapply(nn_wide[names(nn_wide)[names(nn_wide) %in% spplist$clean_code2]], function(x) sum(x>0))
sort(nn_sppcounts)
# but also hits
nn_hits <- sapply(nn_wide[names(nn_wide)[names(nn_wide) %in% spplist$clean_code2]], function(x) sum(x))
sort(nn_sppcounts)
# what is 5% of all obs?
ceiling(nrow(nn_wide)*.05) # at least 3x in record
# some spp may contribute a lot of cover tho, even if infrequently encountered
# nutnet rule: appears fewer than 3 times AND total cover less than 10 hits
nn_exclude <- names(nn_sppcounts[nn_sppcounts < ceiling(nrow(nn_wide)*.05)])
nn_exclude <- nn_exclude[nn_exclude %in% names(nn_hits[nn_hits < 10])]
# also add unknowns
nn_exclude <- c(nn_exclude, names(nn_sppcounts)[grep("^2", names(nn_sppcounts))]) %>% unique()

nn_relabundance <- dplyr::select(nn_wide, -c(nn_exclude))
# pull site info
nn_siteinfo <- nn_relabundance[names(nn_relabundance)[!names(nn_relabundance) %in% spplist$clean_code2]]
nn_relabundance <- dplyr::select(nn_relabundance, c("rowid", names(nn_relabundance)[names(nn_relabundance) %in% spplist$clean_code2]))

common_relabundance <- commabundance_wide %>% as.data.frame()
rownames(common_relabundance) <- commabundance_wide$rowid
# plantdat begins after trt2
common_relabundance <- decostand(common_relabundance[,(which(names(common_relabundance) == "trt2")+1):ncol(common_relabundance)], "total")
# multiple by 100 to get % relative cover value
common_relabundance <- common_relabundance*100

# pull F2G_ratio from summary table to be sure have correct common plots
nn_common_F2G <- subset(coarse_summary, plotid %in% nn_common) %>%
  dplyr::select(site:plot, trt, trt2, F2G_ratio) %>%
  grouped_df(names(.)[!names(.) %in% c("plotid", "plot", "F2G_ratio")]) %>%
  summarize(F2Gmean = mean(F2G_ratio),
            nobs = length(F2G_ratio)) %>%
  ungroup() %>%
  # crunch ln F2g
  mutate(lnF2G = log(F2Gmean))

# make block-averaged nn abundance dat (just commonly surveyed plots since this will be a time-series)
nnabundance <- nn_wide %>%
  dplyr::select(-nn_exclude) %>%
  gather(clean_code2, hits, colnames(.)[colnames(.) %in% unique(spplist$clean_code2)]) %>%
  group_by(site, yr, block, trt, trt2, clean_code2) %>%
  summarise(rcblockmean = mean(hits),
            nobs = length(hits)) %>%
  ungroup() %>%
  # join F2G ratio from coarse means
  left_join(subset(nn_common_F2G)) %>%
  # recode 2013 and 2017 N+K to N so has 4 N reps that yr
  mutate(trt = ifelse(trt == "N+K", "N", trt)) %>%
  # change block to id and drop nobs
  rename(id = block,
         F2G_ratio = F2Gmean) %>%
  #make wideform
  spread(clean_code2, rcblockmean) %>%
  # add rowid
  unite(rowid, site, yr, id, trt, remove = F)


# relativize abundance data for NMDS and make bray curtis for multivariate
nn_envmatrix <- nnabundance[names(nnabundance)[!names(nnabundance) %in% spplist$clean_code2]]
nn_relabundance <- nnabundance[c("rowid", names(nnabundance)[names(nnabundance) %in% spplist$clean_code2])] %>%
  as.data.frame()
row.names(nn_relabundance) <- nn_relabundance$rowid
nn_relabundance <- decostand(nn_relabundance[, 2:ncol(nn_relabundance)], method = "total")

# -- 1a) NutNet Simple NMDS -----
# subset relabundance to C, N and N+P only
nn_relabundance_simple <- nn_relabundance[!grepl("K", row.names(nn_relabundance)),]
# drop any spp that are all 0s
nn_relabundance_simple <- nn_relabundance_simple[,!sapply(nn_relabundance_simple, function(x) all(x == 0))]
nn_envmatrix_simple <- data.frame(rowid = row.names(nn_relabundance_simple)) %>%
  left_join(nn_envmatrix) %>%
  # add trtyr col for grouping
  mutate(yrtrt = paste0(yr, trt2))

# simplified NMDS
nmds_nnall_simple<- metaMDS(nn_relabundance_simple, k = 2, trymax = 100)
nmds_nnall_simple
plot(nmds_nnall_simple)


# envfit
env_nnall_simple <- envfit(nmds_nnall_simple, nn_envmatrix_simple[c("trt", "yr", "yrtrt", "lnF2G")], perm = 10000)
env_nnall_simple

# extract centroids
nncentroids_simple <- scores(env_nnall_simple, "factors") %>%
  data.frame() %>%
  rownames_to_column(var = "rowid") %>%
  mutate(yr = parse_number(rowid),
         trt = str_extract(rowid, "[A-Z].*$"),
         site = "nutnet")

# extract vectors
nnvectors_simple <- scores(env_nnall_simple, "vectors") %>%
  as.data.frame() %>%
  rownames_to_column("factor") %>%
  mutate(r=env_nnall_simple$vectors$r,
         pval = env_nnall_simple$vectors$pvals,
         symbol = ifelse(pval < 0.001, "***", ifelse(pval < 0.01,"**", ifelse(pval < 0.05, "*",
                                                                              ifelse(pval < 0.1, ".", "n.s.")))),
         plotlbl = paste(factor, symbol))

plot_df_nnall_simple <- data.frame(nmds_nnall_simple$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(nn_envmatrix_simple)

spp_df_nnall_simple <- data.frame(nmds_nnall_simple$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  # add trait PC scores
  left_join(sppscores[c("clean_code2", "PC1", "PC2", "resource_grp")]) %>%
  mutate(resource_grp = ifelse(is.na(resource_grp), "Unknown", resource_grp))

# ordiellipse for trt t
nnell_simple <- data.frame()
for(y in unique(plot_df_nnall_simple$yr)){
  tempdf <- subset(plot_df_nnall_simple, yr == y)
  for(t in unique(plot_df_nnall_simple$trt)){
    nnell_simple <- rbind(nnell_simple, cbind(as.data.frame(with(tempdf[tempdf$trt==t,],
                                                     veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),length(MDS1)))$cov,center=c(mean(MDS1),mean(MDS2)))))
                                  ,trt=t, yr = y, site = "nutnet"))
  }
}

# ID min and max y ans x axis points
xmin_nn_simple <- min(c(spp_df_nnall_simple$MDS1, plot_df_nnall_simple$MDS1))
xmax_nn_simple <- max(c(spp_df_nnall_simple$MDS1, plot_df_nnall_simple$MDS1))
ymin_nn_simple <- min(c(spp_df_nnall_simple$MDS2, plot_df_nnall_simple$MDS2))
ymax_nn_simple <- max(c(spp_df_nnall_simple$MDS2, plot_df_nnall_simple$MDS2))

nnplottrend_simple <- ggplot(spp_df_nnall_simple[1,], aes(MDS1, MDS2)) + 
  geom_polygon(data = subset(nnell_simple, yr %in% max(nnell_simple$yr)), aes(MDS1, MDS2, fill = trt, subgroup = as.factor(yr), lty = trt),col = "black", alpha = 0.4) +
  geom_path(data = subset(plot_df_nnall_simple), aes(MDS1, MDS2, group = paste(id, trt), lty = trt, col = trt), arrow = grid::arrow(length = unit(10, "pt")), show.legend = F) +
  geom_point(data = subset(plot_df_nnall_simple), aes(MDS1, MDS2, fill = trt), size = 2, pch = 24, alpha = 0.65) +
  # annotate nmds stress
  geom_text(aes(x = xmin_nn_simple, y = ymin_nn_simple, label = paste("Stress:",  round(nmds_nnall_simple$stress, 3))), hjust = 0) +
  scale_x_continuous(breaks = seq(-1,1, 0.5), limits = c(xmin_nn_simple-0.1,xmax_nn_simple+0.1)) +
  scale_y_continuous(breaks = seq(-1,1, 0.5), limits = c(ymin_nn_simple-0.1, ymax_nn_simple+0.1)) +
  scale_fill_manual(values = trtcols) +
  scale_color_manual(values = trtcols) +
  # remove x title
  xlab(NULL) +
  j_theme +
  theme(legend.position = "none")+
  facet_wrap(~"NutNet, 2013-2017")



nnspptrend_simple <- ggplot(spp_df_nnall_simple[1,], aes(MDS1, MDS2)) + 
  geom_segment(data = nnvectors_simple, aes(x=0, xend = NMDS1, y = 0, yend= NMDS2, group = factor),  arrow = grid::arrow(length = unit(5, "pt")), col = "grey30", alpha = 0.75, lwd = 1) +
  geom_path(data = subset(nncentroids_simple, !is.na(yr)), aes(NMDS1, NMDS2, group = trt, lty = trt), arrow = grid::arrow(length = unit(10, "pt"))) +
  geom_polygon(data = subset(nnell_simple, yr %in% max(nnell_simple$yr)), aes(MDS1, MDS2, fill = trt, subgroup = as.factor(yr), lty = trt),col = "black", alpha = 0.4) +
  #geom_text(data= spp_df_nnall_simple, aes(MDS1, MDS2, col = resource_grp, label = clean_code2)) +
  geom_point(data= subset(spp_df_nnall_simple), aes(MDS1, MDS2, col = resource_grp, shape = simple_lifeform2), size = 2) +#size = pointsize, pch = 21
  geom_text(data= subset(spp_df_nnall_simple, clean_code2 == "GERO2"), aes(MDS1, MDS2, col = resource_grp, label = "Geum rossi"), fontface = "bold.italic", family = "Times", vjust = 1.1, show.legend = FALSE, size = 3) +
  # add vector annotations
  geom_text(data = subset(nnvectors_simple, factor == "yr"), aes(NMDS1, NMDS2, label = paste(factor, symbol)), col = "grey30", vjust = 1.2, hjust = 0.3) +
  geom_text(data = subset(nnvectors_simple, factor == "lnF2G"), aes(NMDS1, NMDS2, label = paste(factor, symbol)), col = "grey30", vjust = 1.3, hjust = 0) +
  scale_x_continuous(breaks = seq(-1,1, 0.5), limits = c(xmin_nn_simple-0.1,xmax_nn_simple+0.1)) +
  scale_y_continuous(breaks = seq(-1,1, 0.5), limits = c(ymin_nn_simple-0.1, ymax_nn_simple+0.1)) +
  scale_fill_manual(name = "Treatment", values = trtcols) +
  scale_linetype(name = "Treatment") +
  scale_color_manual(name = "Resource\ngroup", values = c(traitcols)) +
  scale_shape_manual(name = "Lifeform", values = c("Forb" = 4, "Grass" = 15, "Shrub" = 8)) +
  # remove x title
  xlab(NULL) +
  j_theme +
  labs(y = NULL) +
  theme(axis.text.y = element_blank(),
        legend.position = "none") +
  facet_wrap(~"NutNet, 2013-2017")

nn_simple_panel <- plot_grid(nnplottrend_simple, nnspptrend_simple, ncol = 2,
          rel_widths = c(1, 0.85),
          align = "h")

# PERMANOVA
adonis2(nn_relabundance_simple ~ trt * yr* lnF2G, data = nn_envmatrix_simple, strata = nn_envmatrix_simple$id, ermutations = 1000, method = "bray")
adonis2(nn_relabundance_simple ~ yr * trt *lnF2G, data = nn_envmatrix_simple, strata = nn_envmatrix_simple$id, permutations = 1000, method = "bray")
adonis2(nn_relabundance_simple ~ trt * yr* lnF2G, data = nn_envmatrix_simple, strata = id/yrtrt, permutations = 1000, method = "bray")
adonis2(nn_relabundance_simple ~ yr * trt * lnF2G, data = nn_envmatrix_simple, strata = id/yrtrt, permutations = 1000, method = "bray")

adonis2(nn_relabundance_simple ~ trt * lnF2G, data = nn_envmatrix_simple, strata = id, permutations = 1000, method = "bray")
adonis2(nn_relabundance_simple ~ trt * lnF2G, data = nn_envmatrix_simple, strata = id/yrtrt, permutations = 1000, method = "bray")


# trt and ln F2G signif no matter how run (p < 0.001), trt = 27%, lnF2G = 18% .. if remove yr, resids around about 45% like at saddle
# these results match up with envfit

# PERMDISP
# test for homogeneity of variances
nn_disper <- betadisper(vegdist(nn_relabundance_simple), nn_envmatrix_simple$yrtrt)
nn_disper
anova(nn_disper) # no difference in homogeneity of dispersion
TukeyHSD(nn_disper)
boxplot(nn_disper)



# 1b) NutNet with K NMDS, common -----
nmds_nnall<- metaMDS(nn_relabundance, k = 2, trymax = 100)
nmds_nnall
plot(nmds_nnall)

# environmental fit grass to forb
# original trts
fitnnall <- envfit(nmds_nnall, nn_envmatrix[c("trt2", "F2G_ratio", "yr")], strata = nn_envmatrix$id, perm = 999)
fitnnall #trt is signif, forb:grass signif

#original trts
ordiplot(nmds_nnall, type="n", main = "NutNet all years, all treatments")
with (nn_envmatrix, ordiellipse(nmds_nnall, trt2, kind="se", conf=0.95, col=1:6))
with (nn_envmatrix, ordisurf(nmds_nnall, F2G_ratio, col="grey50", add = T))
plot(fitnnall, col = 1:7)
orditorp (nmds_nnall, display="species", col="grey30", air=0.01)


plot_df_nnall <- data.frame(nmds_nnall$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(nn_envmatrix)
spp_df_nnall <- data.frame(nmds_nnall$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  # join trait PC scores
  left_join(sppscores[c("clean_code2", "PC1", "PC2", "resource_grp")]) %>%
  replace_na(list(resource_grp = "Unknown"))

ggplot(spp_df_nnall, aes(MDS1, MDS2)) + 
  #geom_polygon(data = grpdf_nn17.simple, aes(MDS1, MDS2, fill = trt2, col = trt2), alpha = 0.4) +
  geom_path(data = plot_df_nnall, aes(MDS1, MDS2, lty = trt2, group = paste(id,trt)), arrow = grid::arrow()) +
  geom_point(data = plot_df_nnall, aes(MDS1, MDS2, fill = trt2), pch = 24, alpha = 0.65) +
  geom_text(data= subset(spp_df_nnall, resource_grp != "Unknown" & clean_code2 != "GERO2"), aes(MDS1, MDS2, col = resource_grp, label = clean_code2)) +
  geom_point(data= subset(spp_df_nnall, resource_grp == "Unknown"), aes(MDS1, MDS2, col = resource_grp), alpha = 0.5, pch = 7) +#size = pointsize, pch = 21
  geom_text(data= subset(spp_df_nnall, clean_code2 == "GERO2"), aes(MDS1, MDS2, col = resource_grp, label = clean_code2), fontface ="bold", show.legend = FALSE) +
  scale_color_manual(values = traitcols) +
  scale_fill_manual(values = trtcols) +
  scale_linetype_manual(values= c(1,3, 2))
labs(title = "NWT NutNet plots (consistenly sampled), all years (PRELIM FIG)",
     subtitle = "Rescource aquisitive spp align with N+P,conservative with other trts; N+P comp shifts in\nmostly consistent direction, C plots shift in same direction as N+P more modestly, N different")
ggsave("alpine_addnuts/figures/prelim_figs/nutnetcommon_allyears.png",
       width = 8, height = 7, units = "in")



# 2) Saddle NMDS and PERMANOVA -----
sdl_wide <- subset(commabundance_wide) %>%
  filter(site == "sdl")
# tally non-zero counts by sp
sdl_sppcounts <- sapply(sdl_wide[names(sdl_wide)[names(sdl_wide) %in% spplist$clean_code2]], function(x) sum(x>0))
sort(sdl_sppcounts)
# but also hits
sdl_hits <- sapply(sdl_wide[names(sdl_wide)[names(sdl_wide) %in% spplist$clean_code2]], function(x) sum(x))
sort(sdl_sppcounts)
# what is 5% of all obs?
ceiling(nrow(sdl_wide)*.05) # at least 3x in record
# some spp may contribute a lot of cover tho, even if infrequently encountered
# nutnet rule: appears fewer than 3 times AND total cover less than 10 hits
sdl_exclude <- names(sdl_sppcounts[sdl_sppcounts < ceiling(nrow(sdl_wide)*.05)])
sdl_exclude <- sdl_exclude[sdl_exclude %in% names(sdl_hits[sdl_hits < 10])]
# also add unknowns
sdl_exclude <- c(sdl_exclude, names(sdl_sppcounts)[grep("^2", names(sdl_sppcounts))]) %>% unique()



# relativize abundance data for NMDS and make bray curtis for multivariate
sdl_envmatrix <- sdl_wide %>%
  dplyr::select(names(.)[!names(.) %in% spplist$clean_code2]) %>%
  #drop block and trt2 since only apply to nutnet
  dplyr::select(-c(block, trt2)) %>%
  #join forb2grass ration and take natural log
  left_join(coarse_summary[c("site", "yr", "plotid", "F2G_ratio")]) %>%
  mutate(lnF2G = log(F2G_ratio),
         yrs_onset = yr - 1993,
         #create yr-trt group
         yrtrt = paste0(yr, trt))

sdl_relabundance <- sdl_wide %>%
  dplyr::select("rowid", names(.)[names(.) %in% spplist$clean_code2]) %>%
  as.data.frame() %>%
  # drop spp to exclude
  dplyr::select(-sdl_exclude)
# set rownames
row.names(sdl_relabundance) <- sdl_relabundance$rowid
# relativize
sdl_relabundance <- decostand(sdl_relabundance[, 2:ncol(sdl_relabundance)], method = "total")


# NMDS
nmds_sdlall<- metaMDS(sdl_relabundance, k = 2, trymax = 100)
nmds_sdlall
plot(nmds_sdlall)

# envfit
env_sdlall <- envfit(nmds_sdlall, sdl_envmatrix[c("trt", "yr", "yrtrt", "lnF2G")], perm = 10000)
env_sdlall

# extract centroids
sdlcentroids <- scores(env_sdlall, "factors") %>%
  data.frame() %>%
  rownames_to_column(var = "rowid") %>%
  mutate(yr = parse_number(rowid),
         trt = str_extract(rowid, "[A-Z].*$"),
         site = "sdl")
  
# extract vectors
sdlvectors <- scores(env_sdlall, "vectors") %>%
  as.data.frame() %>%
  rownames_to_column("factor") %>%
  mutate(r=env_sdlall$vectors$r,
         pval = env_sdlall$vectors$pvals,
         symbol = ifelse(pval < 0.001, "***", ifelse(pval < 0.01,"**", ifelse(pval < 0.05, "*",
                                                                             ifelse(pval < 0.1, ".", "ns")))),
         plotlbl = paste(factor, symbol))

plot_df_sdlall <- data.frame(nmds_sdlall$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sdl_envmatrix)

spp_df_sdlall <- data.frame(nmds_sdlall$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  # add trait PC scores
  left_join(sppscores[c("clean_code2", "PC1", "PC2", "resource_grp")]) %>%
  mutate(resource_grp = ifelse(is.na(resource_grp), "Unknown", resource_grp))

# ordiellipse for trt t
sdlell <- data.frame()
for(y in unique(plot_df_sdlall$yr)){
  tempdf <- subset(plot_df_sdlall, yr == y)
  for(t in unique(plot_df_sdlall$trt)){
    sdlell <- rbind(sdlell, cbind(as.data.frame(with(tempdf[tempdf$trt==t,],
                                                     veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),length(MDS1)))$cov,center=c(mean(MDS1),mean(MDS2)))))
                                  ,trt=t, yr = y, site = "sdl"))
  }
}


# ID min and max y ans x axis points
xmin_sdl <- min(c(spp_df_sdlall$MDS1, plot_df_sdlall$MDS1))
xmax_sdl <- max(c(spp_df_sdlall$MDS1, plot_df_sdlall$MDS1))
ymin_sdl <- min(c(spp_df_sdlall$MDS2, plot_df_sdlall$MDS2))
ymax_sdl <- max(c(spp_df_sdlall$MDS2, plot_df_sdlall$MDS2))

sdlplottrend <- ggplot(spp_df_sdlall[1,], aes(MDS1, MDS2)) + #spp_df_sdlall, aes(MDS1, MDS2)
  geom_polygon(data = subset(sdlell, yr %in% c(2016)), aes(MDS1, MDS2, fill = trt, subgroup = as.factor(yr), lty = trt),col = "black", alpha = 0.4) +
  geom_path(data = subset(plot_df_sdlall), aes(MDS1, MDS2, group = as.factor(plotid), lty = trt, col = trt), arrow = grid::arrow(length = unit(7, "pt")), show.legend = F) +
  geom_point(data = subset(plot_df_sdlall), aes(MDS1, MDS2, fill = trt), pch = 24, alpha = 0.65) +
  # annotate nmds stress
  geom_text(aes(x = xmin_sdl, y = ymin_sdl,  label = paste("Stress:",  round(nmds_sdlall$stress, 3))), hjust = 0) +
  scale_x_continuous(breaks = seq(-1.5,1, 0.5), limits = c(xmin_sdl-0.1,xmax_sdl+0.1)) +
  scale_y_continuous(breaks = seq(-1,1, 0.5), limits = c(ymin_sdl-0.1,ymax_sdl+0.1)) +
  scale_fill_manual(values = trtcols) +
  scale_color_manual(values = trtcols) +
  j_theme +
  theme(legend.position = "none") +
  facet_wrap(~"Saddle, 1997-2016")

sdlspptrend_legend <- ggplot(spp_df_sdlall, aes(MDS1, MDS2)) + 
    geom_segment(data = sdlvectors, aes(x=0, xend = NMDS1, y = 0, yend= NMDS2, group = factor),  arrow = grid::arrow(length = unit(5, "pt")), col = "grey30", alpha = 0.75, lwd = 1) +
  geom_path(data = subset(sdlcentroids, !is.na(yr)), aes(NMDS1, NMDS2, group = trt, lty = trt), arrow = grid::arrow(length = unit(10, "pt"))) +
  geom_polygon(data = subset(sdlell, yr %in% c(2016)), aes(MDS1, MDS2, fill = trt, subgroup = as.factor(yr), lty = trt),col = "black", alpha = 0.4) +
  #geom_text(data= spp_df_sdlall, aes(MDS1, MDS2, col = resource_grp, label = clean_code2)) +
  geom_point(data= subset(spp_df_sdlall), aes(MDS1, MDS2, col = resource_grp, shape = simple_lifeform2), size = 2) +#size = pointsize, pch = 21
  geom_text(data= subset(spp_df_sdlall, clean_code2 == "GERO2"), aes(MDS1, MDS2, col = resource_grp, label = "Geum rossi"), fontface = "bold.italic", family = "Times", vjust = 1.1, show.legend = FALSE) +
  # add vector annotations
  geom_text(data = sdlvectors, aes(NMDS1, NMDS2, label = paste(factor, symbol)), col = "grey30", hjust = -0.1) +
  scale_x_continuous(breaks = seq(-1.5,1, 0.5), limits = c(xmin_sdl-0.1,xmax_sdl+0.1)) +
  scale_y_continuous(breaks = seq(-1,1, 0.5), limits = c(ymin_sdl-0.1,ymax_sdl+0.1)) +
  scale_fill_manual(name = "Treatment", values = trtcols) +
  scale_linetype(name = "Treatment") +
  scale_color_manual(name = "Resource\ngroup", values = c(traitcols)) +
  scale_shape_manual(name = "Lifeform", values = c("Forb" = 4, "Grass" = 15, "Shrub" = 8)) +
  j_theme +
  labs(y = NULL) +
  theme(axis.text.y = element_blank()) +
  facet_wrap(~"Saddle, 1997-2016")

nmdslegend_simple <- cowplot::get_legend(sdlspptrend_legend)

sdlspptrend <- ggplot(spp_df_sdlall[1,], aes(MDS1, MDS2)) + 
  geom_segment(data = sdlvectors, aes(x=0, xend = NMDS1, y = 0, yend= NMDS2, group = factor),  arrow = grid::arrow(length = unit(5, "pt")), col = "grey30", alpha = 0.75) +
  geom_path(data = subset(sdlcentroids, !is.na(yr)), aes(NMDS1, NMDS2, group = trt, lty = trt), arrow = grid::arrow(length = unit(7, "pt"))) +
  geom_polygon(data = subset(sdlell, yr %in% c(2016)), aes(MDS1, MDS2, fill = trt, subgroup = as.factor(yr), lty = trt),col = "black", alpha = 0.4) +
  #geom_text(data= spp_df_sdlall, aes(MDS1, MDS2, col = resource_grp, label = clean_code2)) +
  geom_point(data= subset(spp_df_sdlall), aes(MDS1, MDS2, col = resource_grp, shape = simple_lifeform2)) +#size = pointsize, pch = 21
  geom_text(data= subset(spp_df_sdlall, clean_code2 == "GERO2"), aes(MDS1, MDS2, col = resource_grp, label = "Geum rossi"), fontface = "bold.italic", family = "Times", vjust = 1.1, size = 3, show.legend = FALSE) +
  # add vector annotations
  geom_text(data = sdlvectors, aes(NMDS1, NMDS2, label = paste(factor, symbol)), col = "grey30", hjust = -0.1) +
  scale_x_continuous(breaks = seq(-1.5,1, 0.5), limits = c(xmin_sdl-0.1,xmax_sdl+0.1)) +
  scale_y_continuous(breaks = seq(-1,1, 0.5), limits = c(ymin_sdl-0.1,ymax_sdl+0.1)) +
  scale_fill_manual(name = "Treatment", values = trtcols) +
  scale_linetype(name = "Treatment") +
  scale_color_manual(name = "Resource\ngroup", values = c(traitcols)) +
  scale_shape_manual(name = "Lifeform", values = c("Forb" = 4, "Grass" = 15, "Shrub" = 8)) +
  j_theme +
  labs(y = NULL) +
  theme(axis.text.y = element_blank(),
        legend.position = "none") +
  facet_wrap(~"Saddle, 1997-2016")

sdl_panel <- plot_grid(sdlplottrend, sdlspptrend, ncol = 2,
          rel_widths = c(1, 0.85),
          align = "h")


# PERMANOVA
adonis2(sdl_relabundance ~ trt * yr* lnF2G, data = sdl_envmatrix, permutations = 1000, method = "bray")
adonis2(sdl_relabundance ~ yr * trt *lnF2G, data = sdl_envmatrix, permutations = 1000, method = "bray")
adonis2(sdl_relabundance ~ trt * yr* lnF2G, data = sdl_envmatrix, strate = sdl_envmatrix$yrtrt, permutations = 1000, method = "bray")
adonis2(sdl_relabundance ~ yr * trt * lnF2G, data = sdl_envmatrix, strate = sdl_envmatrix$yrtrt, permutations = 1000, method = "bray")

# yr accounts for abou 5% of variation, trt almost 30%, lnF2g 10%, yr x trt 5%, trt * lnF2g 5%, resids = 45%
# signif no matter how you run. direct vars p <0.001, trt x ln `p < 0.001, trt x yr < 0.01`
# these results match up with envfit

# PERMDISP
# test for homogeneity of variances
sdl_disper <- betadisper(vegdist(sdl_relabundance), sdl_envmatrix$yrtrt)
sdl_disper
anova(sdl_disper) # no difference in homogeneity of dispersion
TukeyHSD(sdl_disper)
boxplot(sdl_disper)


# EXTRACT CENTROID DISTANCES



# -- FINAL NMDS FIGS ----
main_nmdspanel <- plot_grid(nn_simple_panel, sdl_panel,
          nrow = 2,
          rel_heights = c(0.9, 1))
main_nmdspanel_wlegend <- plot_grid(main_nmdspanel, nmdslegend_simple, ncol = 2,
          rel_widths = c(1, 0.2))

ggsave("alpine_addnuts/figures/journal_figs/main_nmds_panel.pdf",
       main_nmdspanel_wlegend,
       width = 8, height = 7, units = "in")
ggsave("alpine_addnuts/figures/journal_figs/main_nmds_panel.png",
       main_nmdspanel_wlegend,
       width = 8, height = 7, units = "in")


# -- NUTNET MULTIVARIATE -----
nnabundance2 <- nnabund_comm[,grep("2FORB", names(nnabund_comm)):ncol(nnabund_comm)] %>% as.data.frame()
rownames(nnabundance2) <- nnabund_comm$rowid
nnabundance2 <- nnabundance2[,!sapply(nnabundance2, function(x) sum(x)==0)]
# NMDS
nmds_nnall<- metaMDS(nnabundance2, k = 2, trymax = 100)
nmds_nnall
plot(nmds_nnall)

plot_df_nnall <- data.frame(nmds_nnall$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(envmatrix)
spp_df_nnall <- data.frame(nmds_nnall$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  # join trait PC scores
  left_join(sppscores[c("clean_code2", "PC1", "PC2", "resource_grp")]) %>%
  replace_na(list(resource_grp = "Unknown"))

ggplot(spp_df_nnall, aes(MDS1, MDS2)) + 
  #geom_polygon(data = grpdf_nn17.simple, aes(MDS1, MDS2, fill = trt2, col = trt2), alpha = 0.4) +
  geom_path(data = plot_df_nnall, aes(MDS1, MDS2, lty = trt, group = paste(id,trt)), arrow = grid::arrow()) +
  geom_point(data = plot_df_nnall, aes(MDS1, MDS2, fill = trt), pch = 24, alpha = 0.65) +
  geom_text(data= subset(spp_df_nnall, resource_grp != "Unknown" & clean_code2 != "GERO2"), aes(MDS1, MDS2, col = resource_grp, label = clean_code2)) +
  geom_point(data= subset(spp_df_nnall, resource_grp == "Unknown"), aes(MDS1, MDS2, col = resource_grp), alpha = 0.5, pch = 7) +#size = pointsize, pch = 21
  geom_text(data= subset(spp_df_nnall, clean_code2 == "GERO2"), aes(MDS1, MDS2, col = resource_grp, label = clean_code2), fontface ="bold", show.legend = FALSE) +
  scale_color_manual(values = traitcols) +
  scale_fill_manual(values = trtcols) +
  labs(title = "NWT NutNet plots (consistenly sampled), all years (PRELIM FIG)",
       subtitle = "Rescource aquisitive spp align with N+P,conservative with other trts; N+P comp shifts in\nmostly consistent direction, C plots shift in same direction as N+P more modestly, N different")
ggsave("alpine_addnuts/figures/prelim_figs/nutnetcommon_allyears.png",
       width = 8, height = 7, units = "in")


# PERMANOVA



# PERDISP



# EXTRACT CENTROID DISTANCES

