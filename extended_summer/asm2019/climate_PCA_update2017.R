# compile updated climate data for extended summer PCA
# then run PCA to export .csv of PC scores for figure making


# > borrowing code from Emily's climate script
# > borrowing code from Lauren's PCA script
# vars needed for summer PCA:
## >> year, sum_meanT, sum_precip, sum_moisturedeficit, sum_PET, sum_GDD, 
## >> fivedayrunning5C, fivedayrunning12C, GSLthreedayneg3C, iceoff_GL4


# > The compilation code comes, in part, from Emily Farrer's Climate_Data_Calculations.R

###### Getting all files together for dataset #####
#Saddle files to merge for full Saddle climate data set
#SaddleGDD #calendar year
#Saddlemoisture #calendar year

#all range from 1982,2017
summary(Saddletemp3short) #(jun/jul/aug=summer)
summary(Saddleprecip3short) #(jun/jul/aug=summer) 
summary(Saddlemoisturewateryear) #deficit based on water year jun/jul/aug
summary(SaddleGDDwateryear)
Saddlefivedayrunning5C<-Saddlefivedayrunning5C[Saddlefivedayrunning5C$year %in% (1982:2017),]
Saddlefivedayrunning12C<-Saddlefivedayrunning12C[Saddlefivedayrunning12C$year %in% (1982:2017),]

ClimateAll<-cbind(Saddletemp3short,Saddleprecip3short[,-1],moisturedeficit=Saddlemoisturewateryear[,-1],GDD=SaddleGDDwateryear[,-1],sum_GDD=SaddleGDDwateryearsum[,3],fivedayrunning5C=Saddlefivedayrunning5C[,-1],fivedayrunning12C=Saddlefivedayrunning12C[,-1])
head(ClimateAll)
rownames(ClimateAll)<-1:nrow(ClimateAll)

#plot(1982:2014,wholesite$spr_ppt,type="l",ylim=c(100,700))
plot(1982:2017,ClimateAll$spr_precip,type="l",col=2)
plot(1982:2017,ClimateAll$moisturedeficit,type="l")

ClimateAll<-cbind(ClimateAll,
                  GSLthreeday0C=Saddlemintempthreeday0[Saddlemintempthreeday0$year %in% 1982:2017,2],
                  GSLthreedayneg3C=Saddlemintempthreedayneg3[Saddlemintempthreedayneg3$year %in% 1982:2017,2])

# > add in moisture deficit and PET, annual and seasonal
ClimateAll <- left_join(ClimateAll, SaddlePETwateryear, by=c("water_year" = "year")) %>%
                          left_join(as.data.frame(defpetseason), by = c("water_year"="year"))

# read in and update Green Lake 4 ice on
lakeice <- read_csv("http://niwot.colorado.edu/data_csvs/glakeice.nc.data.csv",
                    trim_ws = TRUE,
                    na = c("NaN", NA, "NA ", " ", ""))
glimpse(lakeice)

# subset gl4 dates and add in ice off estimates for 2016 and 2017
gl4_iceoff <- lakeice %>%
  filter(lake == "Green4") 

gl4_iceoff[36,1:2] <- c(2016, "Green4")
gl4_iceoff$clear_date[36] <- as.Date("2016-07-10") # estimated from Jen Morse emails  & looking at GL4 WQ first sample
gl4_iceoff[37,1:2] <- c(2017, "Green4")
gl4_iceoff$clear_date[37] <- as.Date("2017-07-18") # estimated from Jen Morse emails  & looking at GL4 WQ first sample
gl4_iceoff$clear_jday[36:37] <- yday(gl4_iceoff$clear_date[36:37])
gl4_iceoff$year <- as.numeric(gl4_iceoff$year) # year got converted to character when adding in 2016 and 2017

ClimateAll <- left_join(ClimateAll, gl4_iceoff[c("year","clear_jday")], by = c("water_year" = "year"))

# clean up table for PCA, use Emily's names or order of columns
ClimateAll <- ClimateAll %>%
  dplyr::rename(eco_year = water_year, PET = pet, spr_PET = petspr, sum_PET = petsum, fal_PET = petfal2, 
                sum_moisturedeficit = defsum, fal_moisturedeficit = deffal2, iceoff_GL4 = clear_jday) %>%
  # order columns like Emily's
  dplyr::select(eco_year, MAT, wnt_meanT, spr_meanT, sum_meanT, fal_meanT, tot_precip, wnt_precip, spr_precip, sum_precip, fal_precip,
                moisturedeficit, sum_moisturedeficit, fal_moisturedeficit, PET, spr_PET, sum_PET, fal_PET, GDD, sum_GDD,
                fivedayrunning5C, fivedayrunning12C, GSLthreeday0C, GSLthreedayneg3C, iceoff_GL4)

#write_csv(NWT_summerPCAclimate, "/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/climate_update/NWT_summerClimateData_19822017.csv")  




# > This PCA code come from Lauren Hallett's climate_PCA.R
###Climate visualizations###
library(tidyverse)
library(vegan)
library(corrgram)
library(grid)
library(gridExtra)

## Read in the climate data 
#climate<-read.csv("~/Dropbox/NWT_data/NWT_ClimateData_2015-11-02.csv") # Emily's climate 

climate <- ClimateAll
colnames(climate)[1]<-"year"
climate <- tbl_df(climate)

##Quick visualization
##Look at just the annual mean variables across terrestrial and aquatic
climateMeans <-climate %>%
  select(year, MAT, tot_precip, moisturedeficit, PET, GDD, fivedayrunning5C, fivedayrunning12C) 
         #sdl_max_snwdpth, sdl_meltout, iceon_Mean, daysicefree_Mean) #ctw does not have these ready for ASM

corrgram(climateMeans[,2:ncol(climateMeans)], order=F, upper.panel=panel.shade,
         lower.panel=NULL)


###################
###PCA ANALYSES####
###################

##THE PCA FOR SUBSEQUENT ANALYSIS: SUMMER-RELATED VARIABLES WITH LONG-TIME SERIES
##Summer-only variables with all the possible years (so, fewer variables)
climateSummer <-climate %>%
  select(year, sum_meanT, sum_precip, sum_moisturedeficit, sum_PET, sum_GDD, 
         fivedayrunning5C, fivedayrunning12C, 
         GSLthreedayneg3C, iceoff_GL4) %>%
  na.omit()
row.names(climateSummer)<-climateSummer$year

# Visualize with a correlogram
corrgram(climateSummer[,2:ncol(climateSummer)], order=T, upper.panel=panel.shade,
         lower.panel=NULL)

# Make the summer all-years PCA output dataframe 
sumallPCA <-rda(na.exclude(climateSummer[,2:ncol(climateSummer)]), scale=T)
plot(sumallPCA, scaling=3)
summary(sumallPCA)

# capture all output (if desired)
sumallyrsSummary <- summary(sumallPCA)
sumallyrs_siteloadings <- as.data.frame(sumallyrsSummary[["sites"]])
sumallyrs_siteloadings$year <- climateSummer$year
sumallyrs_siteloadings <- sumallyrs_siteloadings %>% 
  dplyr::select(`year`, PC1:PC6)
sumallyrs_sploadings <- as.data.frame(sumallyrsSummary[["species"]])
sumallyrs_sploadings$variable <- rownames(sumallyrs_sploadings) 
sumallyrs_sploadings <- sumallyrs_sploadings %>%
  dplyr::select(variable, PC1:PC6)

#Make the summer all years output (year scores)
sumallyrsOutput<-as.data.frame(scores(sumallPCA, choices=c(1,2), display=c("sites"))) %>%
  mutate(site=row.names(climateSummer)) 
names(sumallyrsOutput)[1:2]=c("sumallPC1", "sumallPC2")

# Make the summer all years output (variable scores)
sumallyrsVarout<-as.data.frame(scores(sumallPCA, choices=c(1,2), display=c("species")))
sumallyrsVarout$variable<-rownames(sumallyrsVarout)
write_csv(sumallyrsVarout,"/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/climate_update/NWT_sumallPCVarout_19822017.csv")


### **** Compile final merged data frame

NWT_summerPCAclimate <- sumallyrsOutput %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::rename(eco_year = `year`) %>%
  dplyr::select(eco_year, sumallPC1, sumallPC2) %>%
  left_join(ClimateAll, by = "eco_year") %>%
  as.data.frame()

write_csv(NWT_summerPCAclimate,"/Users/serahsierra/Documents/Suding\ Lab/NWT_GRA/climate_update/NWT_sumallPCclimate_19822017.csv")

