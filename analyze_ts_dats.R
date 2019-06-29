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



# -- REVIEW DATA -----
glimpse(plantcom)
glimpse(spplist)
glimpse(sdlplots)
glimpse(nnplots)
# all looks fine.. "control" in sdl site info not coded similarly as "C" in nutnet site info




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

sitematrix1 <- left_join()
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
  left_join(sdlplots, by = c("plotid" = "plot"))

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
orditorp (nmds2, display="species", col="black", air=0.02)

ordiplot(nmds4, type="n", main = "sdl sites, 2012 and 2016 only, all spp")
with(sitematrix4, ordihull(nmds4, yr, col="blue", label = TRUE))
with(sitematrix4, ordiellipse(nmds4, trt, kind = "se", conf = 0.95, col="red", lwd=2,
                              label=TRUE))
orditorp (nmds2, display="species", col="black", air=0.02)
