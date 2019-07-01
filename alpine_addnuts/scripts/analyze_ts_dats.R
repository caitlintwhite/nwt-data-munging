# analyze ts dats (multivariate analysis)

# script purpose:
# read in ctw cleaned + prepped datasets:
# 1) ts nutnet + sdl plant comp tidy data
# 2) tidy site data (nutnet, sdl)
# 3) nutnet-sdl spplist
# run all plant comm in nmdsm, permanova, over time, with treatment data
# > looking for shift to forb dominance in dry meadow plots with +n+p additions
# > also geom rossii dominance in +n+p plots in sdl

# notes:
# ts only looked at plots 1-16 in saddle because those are the only plots that were sampled in all yrs

# NOTE: recheck SIAC vs SAIC (typo?)


# -- SETUP -----
rm(list = ls())
library(tidyverse)
library(vegan)
#library(ggvegan) # devtools::install_github("gavinsimpson/ggvegan")
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



# -- REVIEW DATA -----
glimpse(plantcom)
glimpse(spplist)
glimpse(sdlplots)
glimpse(nnplots)
# all looks fine.. "control" in sdl site info not coded similarly as "C" in nutnet site info


# -- PREP SDL SITE INFO FULL (i.e. get snow and meadow info) ----
# specify dry meadow codes used across datasets
drycodes <- c("d", "Dry", "dry")
wetcodes <- c("w")
mescodes <- c("m", "mesic")
snowcodes <- c("snow", "s", "SD")
nosnow <- c("n", "none", "recovery", "NONE", "ND")

sdlplots_alt <- sdlplots %>%
  left_join(subset(sdlplots_full, !is.na(plot))) %>%
  #temp create rowid for grouping by row
  mutate(rowid = row.names(.)) %>%
  group_by(rowid) %>%
  mutate(meadow = ifelse(meadowLT %in% c(drycodes, NA) &
                           meadow12 %in% c(drycodes, NA) &
                           meadow16 %in% c(drycodes, NA), "Dry", "Unknown"),
         meadow = ifelse(meadowLT %in% c(wetcodes, NA) &
                           meadow12 %in% c(wetcodes, NA) &
                           meadow16 %in% c(wetcodes, NA), "Wet", meadow),
         meadow = ifelse(meadowLT %in% c(mescodes, NA) &
                           meadow12 %in% c(mescodes, NA) &
                           meadow16 %in% c(mescodes, NA), "Mesic", meadow),
         #correct for NAs in all
         meadow = ifelse(is.na(meadowLT) & is.na(meadow12) & is.na(meadow16), "Unknown", meadow),
         # specify snow col
         snow = ifelse(snowLT %in% c(snowcodes, NA) &
                         site97 %in% c(snowcodes, NA) &
                         snow12 %in% c(snowcodes, NA) &
                         sfcode16 %in% c(snowcodes, NA), "Snow", "Unknown"),
         snow = ifelse(snowLT %in% c(nosnow, NA) &
                          site97 %in% c(nosnow, NA) &
                          snow12 %in% c(nosnow, NA) &
                          sfcode16 %in% c(nosnow, NA), "No snow", snow),
         #correct for NAs in all
         snow = ifelse(is.na(snowLT) & is.na(site97) & is.na(snow12) & is.na(sfcode16), "Unknown", snow)) %>%
  ungroup() %>%
  #retain reference cols for analysis only
  dplyr::select(plot, old_plot, meadow, snow, trt)



# -- PREP DATA FOR NMSD -----
# (1) all sites, all yrs -- keep only C, N, P, and N+P plots
keep_nnplots <- nnplots$plotid[nnplots$trt %in% c("C", "N", "P", "N+P")]
matrix1 <- subset(plantcom, plotid %in% c(keep_nnplots, sdlplots$plot)) %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2))

# check to see how many spp in nutnet sites overlap with spp in sdl sites
summary(unique(matrix1$clean_code2[plantcom$site == "nutnet"]) %in%
          unique(matrix1$clean_code2[plantcom$site == "sdl"]))
summary(unique(matrix1$clean_code2[plantcom$site == "sdl"]) %in%
          unique(matrix1$clean_code2[plantcom$site == "nutnet"]))
# pull out spp that overlap
nnspp <- sort(unique(matrix1$clean_code2[plantcom$site == "nutnet"]))
sdlspp <- sort(unique(matrix1$clean_code2[plantcom$site == "sdl"]))
overlapspp <- nnspp[nnspp %in% sdlspp]

# keep overlapping spp only
matrix1_overlap <- subset(matrix1, clean_code2 %in% overlapspp) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

sitematrix1 <- matrix1_overlap[,1:4]
# add trtment info
sitematrix1 <- left_join(sitematrix1, nnplots[c("plotid", "trt")])
for(i in sitematrix1$plotid[sitematrix1$site == "sdl"]){
  sitematrix1$trt[sitematrix1$plotid == i] <- sdlplots$trt[sdlplots$plot == i & !is.na(sdlplots$plot)]
}

row.names(matrix1_overlap) <- matrix1_overlap$rowid
matrix1_overlap <- matrix1_overlap[!colnames(matrix1_overlap) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix1_overlap_rel <- vegan::decostand(matrix1_overlap, method = "total")


# -- NMDS ----
nmds1 <- metaMDS(matrix1_overlap_rel, k = 3, trymax = 50)
plot(nmds1, type = "t")
ordiplot()

ordiplot(nmds1, type="n", main = "all sites, all yrs, common spp")
with (sitematrix1, ordiellipse(nmds1, yr, kind="se", conf=0.95, col="blue", lwd=2,
                               label=TRUE))
with (sitematrix1, ordiellipse(nmds1, trt, kind="se", conf=0.95, col="red", lwd=2,
                               label=TRUE))
orditorp (nmds1, display="species", col="black", air=0.01)



# -- TRY NUTNET ONLY -----
# all trts, nutnet only plots..
matrix2 <- subset(plantcom, site == "nutnet") %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

sitematrix2 <- matrix2[,1:4] %>%
  left_join(nnplots)

row.names(matrix2) <- matrix2$rowid
matrix2 <- matrix2[!colnames(matrix2) %in% c("rowid", "site", "yr", "plotid")]
matrix2_rel <- decostand(matrix2, method = "total")

nmds2 <- metaMDS(matrix2_rel, k = 2, trymax = 50)
plot(nmds2, type = "t")

ordiplot(nmds2, type="n", main = "nutnet sites, all yrs, all spp")
with (sitematrix2, ordiellipse(nmds2, yr, kind="se", conf=0.95, col="blue", lwd=2,
                               label=TRUE))
with (sitematrix2, ordiellipse(nmds2, trt, kind="se", conf=0.95, col="red", lwd=2,
                               label=TRUE))
orditorp (nmds2, display="species", col="black", air=0.02)


# -- TRY SDL 2012 and 2016 ONLY (since 1997 sticks out) -----
# all trts, nutnet only plots..
matrix3 <- subset(plantcom, site == "sdl" & yr > 1997) %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

sitematrix3 <- matrix3[,1:4] %>%
  mutate(plotid = as.numeric(plotid)) %>%
  left_join(sdlplots, by = c("plotid" = "plot"))

row.names(matrix3) <- matrix3$rowid
matrix3 <- matrix3[!colnames(matrix3) %in% c("rowid", "site", "yr", "plotid")]
matrix3_rel <- decostand(matrix3, method = "total")

nmds3 <- metaMDS(matrix3_rel, k = 2, trymax = 50)
plot(nmds3, type = "t")

ordiplot(nmds3, type="n", main = "sdl sites, 2012 and 2016 only, all spp")
with (sitematrix3, ordiellipse(nmds3, yr, kind="se", conf=0.95, col="blue", lwd=2,
                               label=TRUE))
with (sitematrix3, ordiellipse(nmds3, trt, kind="se", conf=0.95, col="red", lwd=2,
                               label=TRUE))
orditorp (nmds2, display="species", col="black", air=0.02)

ordiplot(nmds3, type="n", main = "sdl sites, 2012 and 2016 only, all spp")
with(sitematrix3, ordisurf(nmds3, yr, col="blue", add = TRUE))
with(sitematrix3, ordiellipse(nmds3, trt, kind = "se", conf = 0.95, col="red", lwd=2,
                              label=TRUE))
orditorp (nmds2, display="species", col="black", air=0.02)


# try just sites common in both 2012 and 2016
common1216 <- with(plantcom, plantcom[site == "sdl", c("plotid", "yr")]) %>% 
  distinct() %>%
  group_by(plotid) %>%
  mutate(nobs = length(yr)) %>%
  subset(nobs == 3) %>%
  dplyr::select(plotid) %>% distinct()


# all trts, nutnet only plots..
matrix4 <- subset(plantcom, site == "sdl" & plotid %in% common1216$plotid) %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

sitematrix4 <- matrix4[,1:4] %>%
  mutate(plotid = as.numeric(plotid)) %>%
  #left_join(sdlplots, by = c("plotid" = "plot")) %>%
  left_join(sdlplots_alt, by = c("plotid" = "plot"))

row.names(matrix4) <- matrix4$rowid
matrix4 <- matrix4[!colnames(matrix4) %in% c("rowid", "site", "yr", "plotid")]
matrix4_rel <- decostand(matrix4, method = "total")

nmds4 <- metaMDS(matrix4_rel, k = 2, trymax = 100)
plot(nmds4, type = "t")

ordiplot(nmds4, type="n", main = "sdl sites, all yrs, common plots")
with (sitematrix4, ordiellipse(nmds4, yr, kind="se", conf=0.95, col="blue", lwd=2,
                               label=TRUE))
with (sitematrix4, ordiellipse(nmds4, trt, kind="se", conf=0.95, col="red", lwd=2,
                               label=TRUE))
orditorp (nmds4, display="species", col="black", air=0.02)

ordiplot(nmds4, type="n", main = "sdl sites, all yrs, common plots")
with(sitematrix4, ordihull(nmds4, yr, col="blue", label = TRUE))
with(sitematrix4, ordiellipse(nmds4, trt, kind = "se", conf = 0.95, col="red", lwd=2,
                              label=TRUE))
orditorp (nmds4, display="species", col="black", air=0.02)



# -- TRY SITES IN TS STUDY ONLY ----
nnplots1317 <- subset(plantcom, site == "nutnet") %>%
  dplyr::select(plotid, yr) %>%
  distinct() %>%
  group_by(plotid) %>%
  mutate(nobs = length(yr))

# plots 1-16 for sdl, 28 plots common in nutnet in 2013 and 2017
matrix5 <-  subset(plantcom, (site == "sdl" & plotid %in% c(1:16)) |
                     (site == "nutnet" & plotid %in% nnplots1317$plotid[nnplots1317$plotid %in% keep_nnplots])) %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

sitematrix5 <- matrix5[,1:4]
# add trtment info
sitematrix5 <- left_join(sitematrix5, nnplots[c("plotid", "trt")])
for(i in sitematrix5$plotid[sitematrix5$site == "sdl"]){
  sitematrix5$trt[sitematrix5$plotid == i] <- sdlplots$trt[sdlplots$plot == i & !is.na(sdlplots$plot)]
}

row.names(matrix5) <- matrix5$rowid
matrix5 <- matrix5[!colnames(matrix5) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix5_rel <- vegan::decostand(matrix5, method = "total")

# run nmds
nmds5 <- metaMDS(matrix5_rel, k = 2, trymax = 50)
plot(nmds5, type = "t")


ordiplot(nmds5, type="n", main = "all sites, all yrs, common plots")
with (sitematrix5, ordiellipse(nmds5, yr, kind="se", conf=0.95, col="blue", lwd=2,
                               label=TRUE))
with (sitematrix5, ordiellipse(nmds5, trt, kind="se", conf=0.95, col="red", lwd=2,
                               label=TRUE))
orditorp (nmds5, display="species", col="black", air=0.01)


ordiplot(nmds5, type="n", main = "all sites, all yrs, common plots")
with (sitematrix5, ordihull(nmds5, yr, col="blue", lwd=2,
                            label=TRUE))
with (sitematrix5, ordihull(nmds5, trt, col="red", lwd=2,
                            label=TRUE))
orditorp (nmds5, display="species", col="black", air=0.01)



# -- CONTROL PLOTS ONLY FOR COMPARISON -----
controlplots <- sort(c(sdlplots_alt$plot[sdlplots_alt$trt == "C" & sdlplots_alt$meadow == "Dry" & sdlplots_alt$snow == "No snow"],
                       nnplots$plotid[nnplots$trt == "C"]))
matrix6 <- subset(plantcom, plotid %in% controlplots) %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

sitematrix6 <- matrix6[,1:4]
row.names(matrix6) <- matrix6$rowid
matrix6 <- matrix6[!colnames(matrix6) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix6_rel <- vegan::decostand(matrix6, method = "total")

# run nmds
nmds6 <- metaMDS(matrix6_rel, k = 2, trymax = 50)
plot(nmds6, type = "t")


ordiplot(nmds6, type="n", main = "all sites, all yrs, control plots only")
with (sitematrix6, ordiellipse(nmds6, yr, kind="se", conf=0.95, col="blue", lwd=2,
                               label=TRUE))
orditorp (nmds6, display="species", col="black", air=0.01)

ordiplot(nmds6, type="n", main = "all sites, all yrs, control plots only")
with (sitematrix6, ordihull(nmds6, yr, col="blue", lwd=2,
                            label=TRUE))
orditorp (nmds6, display="species", col="black", air=0.01)



# -- LAST TIME-POINT PLOTS -----
matrix7_sdl <- subset(plantcom, yr %in% c(2016, 2017) & site == "sdl") %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

sitematrix7_sdl <- matrix7_sdl[,1:4] %>%
  mutate(plotid = as.numeric(plotid)) %>%
  left_join(sdlplots_alt, by = c("plotid" = "plot"))

row.names(matrix7_sdl) <- matrix7_sdl$rowid
matrix7_sdl <- matrix7_sdl[!colnames(matrix7_sdl) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix7_sdl_rel <- vegan::decostand(matrix7_sdl, method = "total")

# run nmds
nmds7 <- metaMDS(matrix7_sdl_rel, k = 2, trymax = 50)
plot(nmds7, type = "t")


ordiplot(nmds7, type="n", main = "sdl only, last year, all treatments")
with (sitematrix7_sdl, ordiellipse(nmds7, trt, kind="se", conf=0.95, col="blue", lwd=2,
                               label=TRUE))
with (sitematrix7_sdl, ordiellipse(nmds7, meadow, kind="se", conf=0.95, col="green", lwd=2,
                                   label=TRUE))
with (sitematrix7_sdl, ordiellipse(nmds7, snow, kind="se", conf=0.95, col="purple", lwd=2,
                                   label=TRUE))
orditorp (nmds7, display="species", col="black", air=0.01)



# -- NutNet last time point only ---
matrix8 <- subset(plantcom, yr %in% c(2016, 2017) & site == "nutnet") %>%
  #remove unknowns and non-veg
  subset(!grepl("^2", clean_code2)) %>%
  mutate(rowid = paste(site, yr, plotid, sep = ".")) %>%
  spread(clean_code2, hits, fill = 0) %>%
  dplyr::select(rowid, site:ncol(.)) %>%
  as.data.frame()

sitematrix8 <- matrix8[,1:4] %>%
  left_join(nnplots)

row.names(matrix8) <- matrix8$rowid
matrix8 <- matrix8[!colnames(matrix8) %in% c("rowid", "site", "yr", "plotid")]

# relativize data
matrix8_rel <- vegan::decostand(matrix8, method = "total")

# run nmds
nmds8 <- metaMDS(matrix8_rel, k = 2, trymax = 50)
plot(nmds8, type = "t")


ordiplot(nmds8, type="n", main = "nutnet only, last year, all treatments")
with (sitematrix8, ordiellipse(nmds8, trt, kind="se", conf=0.95, col="blue", lwd=2,
                                   label=TRUE))
orditorp (nmds8, display="species", col="black", air=0.01)




# -- REPLOT IN GGPLOT ----
plot_df <- data.frame(nmds4$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix4)
spp_df <- data.frame(nmds4$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)]))

ggplot(spp_df, aes(MDS1, MDS2)) + 
  geom_text(aes(color = Growth_Habit, label = clean_code2)) +
  geom_text(data = subset(plot_df, trt == "N+P"), aes(MDS1, MDS2, label = plotid)) +
  geom_text(data = subset(plot_df, trt == "N"), aes(MDS1, MDS2, label = plotid), col = "blue") +
  geom_text(data = subset(plot_df, trt == "C"), aes(MDS1, MDS2, label = plotid), col = "purple") +
  geom_text(data = subset(plot_df, trt == "P"), aes(MDS1, MDS2, label = plotid), col = "brown")
  



# specify plotting colors for all treatments
alltrts <- sort(unique(c(sitematrix8$trt, sitematrix7_sdl$trt)))
trtcols <- viridis::viridis(n = length(alltrts))
names(trtcols) <- alltrts

# specify species text size
plottext <- 3

# last time point, sdl
# grab ordihull
data.scores7 <- as.data.frame(scores(nmds7))
grp7.np <- data.scores[data.scores$trt == "N+P", ][chull(data.scores[data.scores$grp == 
                                                                   "A", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp7.n <- data.scores[data.scores$grp == "B", ][chull(data.scores[data.scores$grp == 
                                                                   "B", c("NMDS1", "NMDS2")]), ]  # hull values for grp B

plot_df7 <- data.frame(nmds7$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix7_sdl)
spp_df7 <- data.frame(nmds7$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  #make simple lifeform grp
  mutate(simple_lifeform = ifelse(Family == "Fabaceae", "N-fixer",
                                  ifelse(grepl("Shrub", Growth_Habit), "Shrub",
                                         ifelse(Growth_Habit == "Graminoid", "Grass", "Forb"))))

grp7.np <- plot_df7[plot_df7$trt == "N+P", ][chull(plot_df7[plot_df7$trt == "N+P", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp7.dry <- plot_df7[plot_df7$meadow == "Dry", ][chull(plot_df7[plot_df7$meadow == "Dry", c("MDS1", "MDS2")]), ]  # hull values for dry meadow plots
grp7.other <- plot_df7[plot_df7$meadow != "Dry", ][chull(plot_df7[plot_df7$meadow != "Dry", c("MDS1", "MDS2")]), ]  # hull values for non-dry plots


sdlfig <- ggplot(spp_df7, aes(MDS1, MDS2)) + 
  geom_polygon(data = grp7.np, aes(MDS1, MDS2), alpha = 0.5, fill = "aquamarine") +
  geom_polygon(data = grp7.dry, aes(MDS1, MDS2), alpha = 0.5, fill = NA, col = "black", lty = 2) +
  geom_polygon(data = grp7.other, aes(MDS1, MDS2), alpha = 0.5, fill = NA, col = "black", lty = 3) +
  #geom_text(aes(color = simple_lifeform, label = clean_code2)) +
  geom_text(data = subset(spp_df7, simple_lifeform == "N-fixer"), aes(label = clean_code2), col = "chocolate4", fontface = "bold", size = plottext) +
  geom_text(data = subset(spp_df7, simple_lifeform == "Forb"), aes(label = clean_code2), col = "grey30", size = plottext) +
  geom_text(data = subset(spp_df7, simple_lifeform == "Grass"), aes(label = clean_code2), col = "seagreen4", fontface = "bold", size = plottext) +
  geom_text(data = subset(spp_df7, simple_lifeform == "Shrub"), aes(label = clean_code2), col = "orchid", fontface = "bold", size = plottext) +
  geom_point(data = plot_df7, aes(MDS1, MDS2,fill = trt), pch = 21) +
  scale_color_discrete(name = "Lifeform") +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols) +
  #guides(guide_legend(override.aes = list(pch =21))) +
  #scale_shape_manual(values = c("Dry" = 21, "Mesic" = 24, "Unknown" = 22)) +
  ggtitle("Saddle plots 2016") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")



# last time point, nutnet
plot_df8 <- data.frame(nmds8$points) %>%
  mutate(rowid = row.names(.)) %>%
  left_join(sitematrix8)
spp_df8 <- data.frame(nmds8$species) %>%
  mutate(clean_code2 = row.names(.)) %>%
  left_join(distinct(spplist[,2:ncol(spplist)])) %>%
  #make simple lifeform grp
  mutate(simple_lifeform = ifelse(Family == "Fabaceae", "N-fixer",
                                  ifelse(grepl("Shrub", Growth_Habit), "Shrub",
                                         ifelse(Growth_Habit == "Graminoid", "Grass", "Forb"))))

plot_df8$trt <- factor(plot_df8$trt, levels = alltrts)

grp8.np <- plot_df8[plot_df8$trt == "N+P", ][chull(plot_df8[plot_df8$trt == "N+P", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp8.npk <- plot_df8[plot_df8$trt == "N+P+K", ][chull(plot_df8[plot_df8$trt == "N+P+K", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp8.nk <- plot_df8[plot_df8$trt == "N+K", ][chull(plot_df8[plot_df8$trt == "N+K", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp8.c <- plot_df8[plot_df8$trt == "C", ][chull(plot_df8[plot_df8$trt == "C", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp8.n <- plot_df8[plot_df8$trt == "N", ][chull(plot_df8[plot_df8$trt == "N", c("MDS1", "MDS2")]), ]  # hull values for grp n+p
grp8.k <- plot_df8[plot_df8$trt == "K", ][chull(plot_df8[plot_df8$trt == "K", c("MDS1", "MDS2")]), ]  # hull values for grp n+p


nnfig <- ggplot(spp_df8, aes(MDS1, MDS2)) + 
  geom_polygon(data = grp8.np, aes(MDS1, MDS2), alpha = 0.5, fill = "aquamarine") +
  geom_polygon(data = grp8.npk, aes(MDS1, MDS2), alpha = 0.5, fill = "lightgreen") +
  geom_polygon(data = grp8.c, aes(MDS1, MDS2), alpha = 0.5, fill = "purple") +
  geom_polygon(data = grp8.n, aes(MDS1, MDS2), alpha = 0.5, fill = "dodgerblue3") +
  geom_polygon(data = grp8.k, aes(MDS1, MDS2), alpha = 0.5, fill = "darkslateblue") +
  geom_text(data = subset(spp_df8, simple_lifeform == "N-fixer"), aes(label = clean_code2), col = "chocolate4", fontface = "bold", size = plottext) +
  geom_text(data = subset(spp_df8, simple_lifeform == "Forb"), aes(label = clean_code2), col = "grey30", size = plottext) +
  geom_text(data = subset(spp_df8, simple_lifeform == "Grass"), aes(label = clean_code2), col = "seagreen4", fontface = "bold", size = plottext) +
  geom_point(data = plot_df8, aes(MDS1, MDS2,fill = trt), pch = 21) +
  scale_fill_manual(name = "Plot\ntreatment", values = trtcols, drop = F) +
  ggtitle("NutNet plots 2017") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank())


lasttime_fig <- plot_grid(sdlfig, nnfig, nrow = 1,
          align = "h", rel_widths = c(1,1.3))

ggsave(plot = lasttime_fig, 
       filename = "alpine_addnuts/figures/lasttime_nmds.pdf", scale = 1.3)
