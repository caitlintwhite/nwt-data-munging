# commonly used functions for reading in edi data portal data

# SCE + CTW code to determine most recent version of package ID and read in current dataset on EDI
#function to determine current version of data package on EDI
getCurrentVersion<-function(edi_id){
  versions=readLines(paste0('https://pasta.lternet.edu/package/eml/knb-lter-nwt/', edi_id), warn=FALSE)
  currentV <- max(as.numeric(versions))
  return(currentV)
}
# function to get entity ID for current version
getEntityId <- function(edi_id, version){
  entID <- readLines(paste0('https://pasta.lternet.edu/package/eml/knb-lter-nwt/', edi_id, "/", version, "/"), warn=FALSE)[1]
  entID <- gsub(paste0("http.*/",edi_id,"/",version,"/"), "", entID) # remove all chars except what comes after last /
  return(entID)
}
# reads in tabular dataset for data package that has only one csv data file (could make more generic with read table, but should know what you're reading in to use)
getTabular <- function(edi_id, na_vals = c("", "NA", NA, NaN, ".", "NaN", " ")){
  v <- getCurrentVersion(edi_id)
  id <- getEntityId(edi_id, v)
  dat <- read.csv(paste0("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.", edi_id, ".", v, 
                         "&entityid=", id),
                  strip.white =TRUE, na.strings = na_vals)
  print(paste0("Reading in knb-lter-nwt.", edi_id, ".", v))
  return(dat)
}