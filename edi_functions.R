# commonly used functions for reading in edi data portal data

# function to determine data package version number only (not return full data package ID)
getPackageVersion<-function(edi_id, site = "nwt"){
  versions=readLines(paste0('https://pasta.lternet.edu/package/eml/knb-lter-', site, '/', edi_id), warn=FALSE)
  currentV <- max(as.numeric(versions))
  return(currentV)
}

# function to get entity ID for current data package version
getEntityId <- function(edi_id, version, site = "nwt", datanum = 1){
  entID <- readLines(paste0('https://pasta.lternet.edu/package/eml/knb-lter-', site, '/', edi_id, "/", version, "/"), warn=FALSE)[datanum]
  entID <- gsub(paste0("http.*/",edi_id,"/",version,"/"), "", entID) # remove all chars except what comes after last /
  return(entID)
}

# function to read in tabular csv dataset for data package (could make more generic with read table, but should know what you're reading in to use)
getTabular <- function(edi_id, na_vals = c("", "NA", NA, NaN, ".", "NaN", " "), site = "nwt", datanum = 1, col_names = TRUE){
  v <- getPackageVersion(edi_id, site = site)
  id <- getEntityId(edi_id, v, site = site, datanum = datanum)
  dat <- readr::read_csv(paste0("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-", site, ".", edi_id, ".", v, 
                         "&entityid=", id),
                  trim_ws =TRUE, na = na_vals, col_names = TRUE)
  print(paste0("Reading in knb-lter-", site, ".", edi_id, ".", v))
  return(dat)
}

# function to read in tabular csv dataset for data package (could make more generic with read table, but should know what you're reading in to use)
# uses bases read.csv
getTabular2 <- function(edi_id, na_vals = c("", "NA", NA, NaN, ".", "NaN", " "), site = "nwt", datanum = 1, h = TRUE){
  v <- getPackageVersion(edi_id, site = site)
  id <- getEntityId(edi_id, v, site = site, datanum = datanum)
  dat <- read.csv(paste0("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-", site, ".", edi_id, ".", v, 
                                "&entityid=", id),
                           strip.white =TRUE, na.strings = na_vals, header = h)
  print(paste0("Reading in knb-lter-", site, ".", edi_id, ".", v))
  return(dat)
}