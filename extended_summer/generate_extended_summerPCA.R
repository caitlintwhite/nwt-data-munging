# run extended summer PCA analysis

# script purpose:
# create function to calculate NWT extended summer metric

# NOTE: code originally created by Lauren Hallett for 2015 NWT LTER NSF renewal
# updated and written into function by Caitlin White (Suding graduate student), 2019

# > This PCA code come from Lauren Hallett's climate_PCA.R

extendedSummer <- function(pcadat, year, outpath){
  
  require(vegan)
  require(corrgram)
  
  ###################
  ###PCA ANALYSES####
  ###################
  
  ##THE PCA FOR SUBSEQUENT ANALYSIS: SUMMER-RELATED VARIABLES WITH LONG-TIME SERIES
  ##Summer-only variables with all the possible years (so, fewer variables)
  climateSummer <- pcadat
  colnames(climateSummer)[which(colnames(climateSummer)== year)] <- "year"
  climateSummer <- climateSummer[c("year", "sum_meanT", "sum_precip", "sum_moisturedeficit", "sum_PET", "sum_GDD",
                                   "fivedayrunning5C", "fivedayrunning12C", "GSLthreedayneg3C", "iceoff_GL4")]
  climateSummer <- na.omit(climateSummer)
  row.names(climateSummer)<-climateSummer$year
  
  # Visualize with a correlogram
  print("Correlogram of summer climate input to extended summer PCA:")
  print(corrgram(climateSummer[,2:ncol(climateSummer)], order=T, upper.panel=panel.shade,
           lower.panel=NULL))
  
  # Make the summer all-years PCA output dataframe 
  sumallPCA <-rda(na.exclude(climateSummer[,2:ncol(climateSummer)]), scale=T)
  print("Results of summer climate PCA:")
  print(plot(sumallPCA, scaling=3))
  print(summary(sumallPCA))
  
  # capture all output (if desired)
  sumallyrsSummary <- summary(sumallPCA)
  sumallyrs_siteloadings <- as.data.frame(sumallyrsSummary[["sites"]])
  sumallyrs_siteloadings$year <- climateSummer$year
  sumallyrs_siteloadings <- sumallyrs_siteloadings[,c(which(colnames(sumallyrs_siteloadings)=="year"), which(colnames(sumallyrs_siteloadings)=="PC1"):which(colnames(sumallyrs_siteloadings)=="PC6"))]
  sumallyrs_sploadings <- as.data.frame(sumallyrsSummary[["species"]])
  sumallyrs_sploadings$variable <- rownames(sumallyrs_sploadings) 
  sumallyrs_sploadings <- sumallyrs_sploadings[c(which(colnames(sumallyrs_sploadings)=="variable"), which(colnames(sumallyrs_sploadings)=="PC1"):which(colnames(sumallyrs_sploadings)=="PC6"))]
  
  #Make the summer all years output (year scores)
  sumallyrsOutput<-as.data.frame(scores(sumallPCA, choices=c(1,2), display=c("sites")))
  sumallyrsOutput$site <- row.names(climateSummer) 
  names(sumallyrsOutput)[1:2]=c("sumallPC1", "sumallPC2")
  
  # Make the summer all years output (variable scores)
  sumallyrsVarout<-as.data.frame(scores(sumallPCA, choices=c(1,2), display=c("species")))
  sumallyrsVarout$variable<-rownames(sumallyrsVarout)
  
  write.csv(sumallyrsVarout,paste0(outpath, "NWT_sumallPCVarout_",min(climateSummer$year), max(climateSummer$year), ".csv"), row.names = F, quote = F)
  write.csv(sumallyrsOutput,paste0(outpath, "NWT_sumallPCOutput_",min(climateSummer$year), max(climateSummer$year), ".csv"), row.names = F, quote = F)
  
  
  ### **** Compile final merged data frame
  
  NWT_summerPCAclimate <- as.data.frame(sumallyrsOutput)
  colnames(NWT_summerPCAclimate)[3] <- "eco_year"
  NWT_summerPCAclimate$eco_year <- as.numeric(NWT_summerPCAclimate$eco_year)
  NWT_summerPCAclimate <- merge(NWT_summerPCAclimate, climateSummer, by.x="eco_year", by.y = "year", all.x =T)
  
  write.csv(NWT_summerPCAclimate, paste0(outpath, "NWT_sumallPCclimate_",min(climateSummer$year), max(climateSummer$year),".csv"), row.names=F, quote = F)
  return(NWT_summerPCAclimate)
}
