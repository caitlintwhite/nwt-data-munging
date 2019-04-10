# R script for "Extended Summer" figures:
# 1. Extended summer PCA (PC1 x PC2 with variables and years plotted)
# 2. PC1 (length of summer), 1982-2017
# > contact Caitlin White with any questions (caitlin.t.white@colorado.edu)


## ----- SETUP -----
# load needed libraries
library(tidyverse) #install.packages("tidyverse") if don't have
library(vegan)

# read in NWT climate dataset with PC scores
NWTclimate <- read_csv(file = file.choose("")) # "NWT_sumallPCclimate_19822017.csv"
sumallyrsVarout <- read_csv(file = file.choose("")) # NWT_sumallPCVarout_19822017.csv"
sumallyrsOutput <- NWTclimate[c("eco_year", "sumallPC1", "sumallPC2")]
  
# check read in as expected
glimpse(NWTclimate)
glimpse(sumallyrsVarout)
glimpse(sumallyrsOutput)



## ----- DATA PREP -----
# add in alternative plotting labels for PC plot
# order of original names
sumallyrsVarout$variable 

# add abbreviated variable names
sumallyrsVarout$varshort <- c("Temp", "Precip", "MoistureDeficit", "PET", "GDD", 
                              "Days5C", "Days12C", "GSL", "IceOff")

# add variable numbers as in NSF proposal
# GDD missed in proposal, so numbers shift by 1 except for precip
sumallyrsVarout$varnum <- NA # create field
# assign numbers in clockwise order (as they appear in figure) starting with precip
sumallyrsVarout$varnum[sumallyrsVarout$variable == "sum_meanT"] <- 3 
sumallyrsVarout$varnum[sumallyrsVarout$variable == "sum_precip"] <- 1 
sumallyrsVarout$varnum[sumallyrsVarout$variable == "sum_moisturedeficit"] <- 6 
sumallyrsVarout$varnum[sumallyrsVarout$variable == "sum_PET"] <- 4 
sumallyrsVarout$varnum[sumallyrsVarout$variable == "sum_GDD"] <- 2
sumallyrsVarout$varnum[sumallyrsVarout$variable == "fivedayrunning5C"] <- 8
sumallyrsVarout$varnum[sumallyrsVarout$variable == "fivedayrunning12C"] <- 9 
sumallyrsVarout$varnum[sumallyrsVarout$variable == "GSLthreedayneg3C"] <- 5 
sumallyrsVarout$varnum[sumallyrsVarout$variable == "iceoff_GL4"] <- 7

# rename PC1 and PC2 to sumallPC1 and sumallPC2
sumallyrsVarout <- sumallyrsVarout %>%
  dplyr::rename(sumallPC1 = PC1,
                sumallPC2 = PC2)

## ----- ANALYSES FOR PLOTTING/DESCRIPTIVE INTERPRATION -----
# [1] PCA of summer climate variables
# subset climate data for summer variables
climateSummer <- NWTclimate %>%
  dplyr::rename(year = eco_year) %>%
  select(year, sum_meanT, sum_precip, sum_moisturedeficit, sum_PET, sum_GDD, 
         fivedayrunning5C, fivedayrunning12C, 
         GSLthreedayneg3C, iceoff_GL4) %>%
  na.omit() # no NAs allowed (there aren't any in ctw's updated 1982-2017 anyway)
row.names(climateSummer)<-climateSummer$year
climateSummer <- as.data.frame(climateSummer) # preserves rownames in PCA output

# execute PCA and view results
sumallPCA <-rda(na.exclude(climateSummer[,2:ncol(climateSummer)]), scale=T)
summary(sumallPCA) #PC1 (extended summer) explains 49%, PC2 (moisture gradient) explains 17% 
biplot(sumallPCA)


# [2] Temporal trend in extended summer PC axis (sumallPC1)
# check for significance of trend in PC1 ~ year
# all years 1982-2017
summary(lm(sumallPC1 ~ eco_year, data = sumallyrsOutput))
# only NSF proposal years 1982-2014
summary(lm(sumallPC1 ~ eco_year, data = subset(sumallyrsOutput, eco_year < 2015)))


## NOTE: ## temporal trend not significant with 2015-2017 data added.. but should try again if chart data infilled following Jennings et al. method
# ctw infilled 2015-2017 following methods noted in metadata on NWT data portal
# used saddle logger data to infill for yrs 2015-2016, d1 chart data for 2017 (logger dat not available in 2017)
# 2017 is pretty different than 2015 and 2016.. perhaps difference in infill source influenced? 
# however, relatively few summer dates infilled .. 

# ctw infilled 7 observations of 1096 for summer precip 2015-2017
# ctw infilled 19 observations of 1096 for summer temp 2015-2017 (all in summer 2015)
# ctw infilled about 6% of Mar-Oct 2015-2017 dates
# summer PCA variables that could be influenced in Mar-Oct infilling: 
# ** GSL (relies on fall temp), Days12C (calculated from May 1), Days5C (calculated from March 1) 
# ** That said, those variable values in 2015 and 2016 are not that extreme


## ----- FIGURES -----
# specify plotting themes
plotcolor <- "dodgerblue3" # change this to change color in PC temporal trend plots
labelsize <- 5 # change this to change variable name size in PC plot
yrsize <- 3 # change this to change year size in PC plot
axessize <- 16 # change to this change size of axes text

# [1] PCA plot showing Extended Summer (all years summer PCA) with years and variables
summer_PCA <- ggplot(sumallyrsOutput, aes(x=sumallPC1, y=sumallPC2)) +
  #add x and y zero lines
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  # add arrows
  geom_segment(data = sumallyrsVarout,
               aes(x = 0, xend = .9 * sumallPC1,
                   y = 0, yend = .9 * sumallPC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
  # add yrs, auto-exlucde overlapping labels
  geom_text(aes(label=eco_year), size=yrsize, check_overlap=T) +
  #add yrs that didn't plot due to overlap
  geom_text(data=subset(sumallyrsOutput, eco_year %in% c(1998, 2014)), 
            aes(x=sumallPC1+.1, y=sumallPC2+.1, label=eco_year), size=yrsize, check_overlap=F) +
  # add variable names, customize position on plot through nudge_x or nudge_y
  # > can use number reference for variables by replacing "label=varshort" with "label=varnum"
  geom_text(data= subset(sumallyrsVarout, variable == "GSLthreedayneg3C"), 
            aes(sumallPC1, sumallPC2, label=varshort), 
            check_overlap = T,
            vjust = "outward",
            size = labelsize,
            col = plotcolor,
            fontface = "bold.italic") +
  geom_text(data= subset(sumallyrsVarout, variable == "sum_moisturedeficit"), 
            aes(sumallPC1, sumallPC2, label=varshort), 
            check_overlap = T,
            nudge_y = -0.04,
            size = labelsize,
            col = plotcolor,
            fontface = "bold.italic") +
  geom_text(data=subset(sumallyrsVarout, variable == "sum_PET"), 
            aes(sumallPC1, sumallPC2, label=varshort), 
            nudge_x = 0.05,
            nudge_y = -0.1,
            size= labelsize, color=plotcolor, 
            fontface="bold.italic") +
  geom_text(data=subset(sumallyrsVarout, variable == "sum_meanT"), 
            aes(sumallPC1, sumallPC2, label=varshort), 
            #hjust="outward",
            nudge_y = 0.1,
            size= labelsize, color=plotcolor, 
            fontface="bold.italic") +
  geom_text(data=subset(sumallyrsVarout, variable == "sum_GDD"), 
            aes(sumallPC1, sumallPC2, label=varshort), 
            #hjust="outward",
            nudge_y = -0.05,
            nudge_x = 0.1,
            size= labelsize, color=plotcolor, 
            fontface="bold.italic") +
  geom_text(data=subset(sumallyrsVarout, variable == "sum_precip"), 
            aes(sumallPC1, sumallPC2, label=varshort), 
            hjust="outward",
            nudge_y = -0.1,
            size= labelsize, color=plotcolor, 
            fontface="bold.italic") +
  geom_text(data=subset(sumallyrsVarout, variable == "fivedayrunning12C"), 
            aes(sumallPC1, sumallPC2, label=varshort), 
            nudge_y = 0.1,
            nudge_x = -0.1,
            size= labelsize, color=plotcolor, 
            fontface="bold.italic") +
  geom_text(data=subset(sumallyrsVarout, variable == "fivedayrunning5C"), 
            aes(sumallPC1, sumallPC2, label=varshort), 
            nudge_x = -0.22,
            size= labelsize, color=plotcolor, 
            fontface="bold.italic") +
  geom_text(data=subset(sumallyrsVarout, variable == "iceoff_GL4"), 
            aes(sumallPC1, sumallPC2, label=varshort), 
            nudge_x = 0.1,
            nudge_y = -0.1,
            size= labelsize, color=plotcolor, 
            fontface="bold.italic") +
  # #scale x and y by the minimum and maximum observed values 
  scale_y_continuous(limits=c(min(sumallyrsOutput$sumallPC2)-.2,max(sumallyrsOutput$sumallPC2)+.2)) + 
  scale_x_continuous(limits=c(min(sumallyrsOutput$sumallPC1)-.2,max(sumallyrsOutput$sumallPC1)+.2)) +
  #name axes with the proporiton of variance explained
  xlab(paste("PC1 (Extended Summer) [",sprintf("%.1f",sumallPCA$CA$eig["PC1"]/sumallPCA$tot.chi*100,3),"%]",sep="")) +
  ylab(paste("PC2 (precipitation quantity) [",sprintf("%.1f",sumallPCA$CA$eig["PC2"]/sumallPCA$tot.chi*100,3),"%]",sep="")) +
  #labs(x="PC1 (Extended Summer)", y="PC2 (precipitation quantity)") +
  theme_classic() +
  theme(axis.title = element_text(size = axessize),
        axis.text = element_text(size = axessize))


# [2] Extended summer temporal trend (plot PC1 score ~ year)
PC1byyear <- ggplot(sumallyrsOutput, aes(eco_year, sumallPC1)) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_line(col="grey30") +
  geom_point(pch=21, col = "black", fill = plotcolor, size = 4) +
  #geom_smooth(col = "black", method="lm", lty=2) +
  labs(x = "Year",
       y = "PC1 (Extended Summer)") +
  scale_x_continuous(breaks=seq(from=1980, to=2015, by=5), 
                     limits=c(1980, 2018),
                     expand=c(0.01,0.01)) +
  theme_classic() +
  theme(axis.title = element_text(size = axessize),
        axis.text = element_text(size = axessize))

PC1byyear_alt <- ggplot(sumallyrsOutput, aes(eco_year, sumallPC1)) +
  geom_line(col="grey30") +
  geom_point(pch=21, col = "black", fill = plotcolor, size = 4) +
  geom_smooth(data = subset(sumallyrsOutput, eco_year<2015), aes(eco_year, sumallPC1), method = "lm", se =F) +
  geom_smooth(col = "black", method="lm", se = F, lty=2) +
  labs(x = "Year",
       y = "PC1 (Extended Summer)") +
  scale_x_continuous(breaks=seq(from=1980, to=2015, by=5), 
                     limits=c(1980, 2018),
                     expand=c(0.01,0.01)) +
  theme_classic() +
  theme(axis.title = element_text(size = axessize),
        axis.text = element_text(size = axessize))


## ----- EXPORT FIGURES -----
# will write to your working directory unless you specify path
# > can write jpeg, tiff, pdf or png (just change extension type in filename)

# export PCA plot [figure 1]
tiff(filename = "NWTsummerPCA_1982-2017.tiff", 
     res = 300, # specify dpi resolution, see ?tiff for more info
     width=4, height=4, # specify dimensions of figure
     units = "in")
summer_PCA
dev.off() # execute this line then look in folder for figure

# export PC temporal trend [figure 2]
tiff(filename = "NWT_PC1trend_1982-2017.tiff", 
     res = 300, # specify dpi resolution, see ?tiff for more info
     width=6, height=4, # specify dimensions of figure
     units = "in")
PC1byyear
dev.off() # execute this line then look in folder for figure

# export PC temporal trend [alternative version with NSF proposal 1982-2014 trend]
tiff(filename = "NWT_PC1trend_1982-2017_alternate.tiff", 
     res = 150, # specify dpi resolution, see ?tiff for more info
     width=6, height=4, # specify dimensions of figure
     units = "in")
PC1byyear_alt
dev.off() # execute this line then look in folder for figure
