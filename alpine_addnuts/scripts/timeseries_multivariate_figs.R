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
library(knitr)
library(DescTools) # to test greyscale color printing
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


# check
sdl_relabundance <- subset(abundance, site == "sdl") %>%
  filter(plotid %in% sdl_common) %>%
  spread(clean_code2, hits, fill = 0) %>%
  unite(rowid, site, yr, plotid, sep = "_", remove = F)
rownames(sdl_relabundance) <- sdl_relabundance$rowid
# plantdat begins after trt2
sdl_relabundance <- decostand(sdl_relabundance[,(which(names(sdl_relabundance) == "trt2")+1):ncol(sdl_relabundance)], "total")
# multiple by 100 to get % relative cover value
sdl_relabundance <- sdl_relabundance*100

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

# make block-averaged nn abundance dat
nnabundance <- relabundance[grepl("nutnet", rownames(relabundance)),] %>%
  rownames_to_column("rowid") %>%
  left_join(dplyr::select(abundance_wide, rowid:trt2)) %>%
  dplyr::select(rowid,site:trt2, colnames(.)[colnames(.) %in% unique(spplist$clean_code2)]) %>%
  gather(clean_code2, relcov, colnames(.)[colnames(.) %in% unique(spplist$clean_code2)]) %>%
  group_by(site, yr, block, trt, trt2, clean_code2) %>%
  summarise(rcblockmean = mean(relcov),
            nobs = length(relcov)) %>%
  ungroup() %>%
  # add iteration
  mutate(iter = "suppl") %>%
  # recode 2017 N+K to N so has 4 N reps that yr
  mutate(trt = ifelse(yr == 2017 & trt == "N+K", "N", trt)) %>%
  # join F2G ratio from coarse means
  left_join(subset(nn_coarse_blockmeans, met == "F2G_ratio")) %>%
  # change block to id and drop nobs
  rename(id = block,
         F2G_ratio = blockmean) %>%
  dplyr::select(-c(trt2, met)) %>%
  #make wideform
  spread(clean_code2, rcblockmean) %>%
  # add rowid
  unite(rowid, site, yr, id, trt, iter, remove = F)

#subset common plots nn abundance
nnabund_comm <- relabundance[grepl("nutnet", rownames(relabundance)),] %>%
  rownames_to_column("rowid") %>%
  left_join(dplyr::select(abundance_wide, rowid:trt2)) %>%
  filter(plotid %in% nn_common) %>%
  dplyr::select(rowid,site:trt2, colnames(.)[colnames(.) %in% unique(spplist$clean_code2)]) %>%
  gather(clean_code2, relcov, colnames(.)[colnames(.) %in% unique(spplist$clean_code2)]) %>%
  group_by(site, yr, block, trt, trt2, clean_code2) %>%
  summarise(rcblockmean = mean(relcov),
            nobs = length(relcov)) %>%
  ungroup() %>%
  #recode N+K to N so have 4 points 
  mutate(trt = recode(trt, `N+K` = "N"),
         # add iteration
         iter = "main") %>%
  # recode 2017 N+K to N so has 4 N reps that yr
  mutate(trt = ifelse(yr == 2017 & trt == "N+K", "N", trt)) %>%
  # join F2G ratio from coarse means
  left_join(subset(nn_coarse_blockmeans, met == "F2G_ratio")) %>%
  # change block to id and drop nobs
  rename(id = block,
         F2G_ratio = blockmean) %>%
  dplyr::select(-c(trt2, met)) %>%
  spread(clean_code2, rcblockmean) %>%
  # retain only plots in N, P, C, or N+P
  filter(trt %in% c("C", "N", "P", "N+P")) %>%
  # add rowid
  unite(rowid, site, yr, id, trt, iter, remove = F)


# get all relativized sdl data
sdlabundance <- relabundance[grepl("sdl", rownames(relabundance)),] %>%
  mutate(rowid = rownames(.)) %>%
  # join trt info
  left_join(dplyr::select(abundance_wide, rowid:plotid, trt)) %>%
  # select plots commonly sampled across yrs
  filter(plotid %in% sdl_common) %>%
  # add iteration
  mutate(iter = "main",
         nobs = 1) %>%
  #join F2G ratio
  left_join(dplyr::select(coarse_summary, site:plotid, F2G_ratio)) %>%
  #reorder cols (drop rowid to remake)
  dplyr::select(site:F2G_ratio, colnames(.)[colnames(.) %in% unique(spplist$clean_code2)]) %>%
  rename(id = plotid) %>%
  # add rowid
  unite(rowid, site, yr, id, trt, iter, remove = F)

# stack all
relstack <- rbind(nnabundance, nnabund_comm, sdlabundance) %>% as.data.frame() %>%
  # unite yr and iteration to id iterations in for loop
  unite(tracker, iter,yr, remove = F)
rownames(relstack) <- relstack$rowid
envmatrix <- relstack[!colnames(relstack) %in% unique(spplist$clean_code2)] %>%
  # add natural log transformed F2G
  mutate(lnF2G = log(F2G_ratio))



# -- SDL MULTIVARIATE ---
# check distibution of dissimilarity matrix based on transformation
# first, remove cols where spp abundances always 0
sdlabundance2 <- sdlabundance[sdlabundance$yr != 2005,grep("2FORB", names(sdlabundance)):ncol(sdlabundance)]
rownames(sdlabundance2) <- sdlabundance$rowid[sdlabundance$yr != 2005]
sdlabundance2 <- sdlabundance2[,!sapply(sdlabundance2, function(x) sum(x)==0)]
# count number of times each spp present in record
countpres <- sapply(sdlabundance2,function(x) sum(x>0))
sort(countpres)

sdlabundance3 <- sdlabundance2[,countpres>= (nrow(sdlabundance2)*0.05)]

# NMDS
nmds_sdlall<- metaMDS(sdl_relabundance, k = 2, trymax = 100)
nmds_sdlall
plot(nmds_sdlall)

plot_df_sdlall <- data.frame(nmds_sdlall$points) %>%
  mutate(rowid = row.names(.)) %>%
  separate(rowid, c("site", "yr", "id"), sep = "_") %>%
  mutate_at(c("yr"), as.numeric) %>%
  left_join(envmatrix)

spp_df_sdlall <- data.frame(nmds_sdlall$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  # add trait PC scores
  left_join(sppscores[c("clean_code2", "PC1", "PC2", "resource_grp")]) %>%
  mutate(resource_grp = ifelse(is.na(resource_grp), "Unknown", resource_grp))


ggplot(spp_df_sdlall, aes(MDS1, MDS2)) + 
  #geom_polygon(data = grpdf_nn17.simple, aes(MDS1, MDS2, fill = trt2, col = trt2), alpha = 0.4) +
  geom_path(data = subset(plot_df_sdlall), aes(MDS1, MDS2, lty = trt, group = as.factor(id)), arrow = grid::arrow(length = unit(10, "pt"))) +
  geom_point(data = subset(plot_df_sdlall), aes(MDS1, MDS2, fill = trt), size = 2, pch = 24, alpha = 0.65) +
  geom_text(data= subset(spp_df_sdlall, resource_grp != "Unknown" & clean_code2 != "GERO2"), aes(MDS1, MDS2, col = resource_grp, label = clean_code2)) +
  geom_point(data= subset(spp_df_sdlall, resource_grp == "Unknown"), aes(MDS1, MDS2, col = resource_grp), alpha = 0.5, pch = 7) +#size = pointsize, pch = 21
  geom_text(data= subset(spp_df_sdlall, clean_code2 == "GERO2"), aes(MDS1, MDS2, col = resource_grp, label = clean_code2), fontface ="bold", show.legend = FALSE) +
  scale_fill_manual(values = trtcols) +
  scale_color_manual(values = c(traitcols)) +
  labs(title = "Saddle snowfence dry meadow fertilization (plots 1-16), all years (PRELIM FIG)",
       subtitle = "Rescource aquisitive spp align with N+P,conservative with other trts;\nN+P comp shifts in consistent direction, N fluctuates largely, C changes little, P moderately")
ggsave("alpine_addnuts/figures/prelim_figs/sdlcommon_allyears.png",
       width = 8, height = 7, units = "in")


# PERMANOVA


# PERMDISP



# EXTRACT CENTROID DISTANCES




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

