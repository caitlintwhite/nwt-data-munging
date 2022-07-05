# functions to read NWT and neighbor datasets dynamically



# == FOR NWT DATA =====

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
getTabular <- function(edi_id, na_vals = c("", "NA", NA, NaN, ".", "NaN", " "), col_class = NULL, site = "nwt", datanum = 1){
  v <- getPackageVersion(edi_id, site = site)
  id <- getEntityId(edi_id, v, site = site, datanum = datanum)
  dat <- readr::read_csv(paste0("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-", site, ".", edi_id, ".", v, 
                                "&entityid=", id),
                         trim_ws =TRUE, na = na_vals, col_types = col_class,
                         guess_max = 100000)
  dat <- as.data.frame(dat)
  print(paste0("Reading in knb-lter-", site, ".", edi_id, ".", v))
  return(dat)
}


# function to read a list of edi nwt package numbers and return data in list
getNWTdatlist <- function(datnums){
  # read a list of package numbers as list of data tables
  datlist <- lapply(datnums, function(x) getTabular(x))
  return(datlist)
}


# function to compile NWT raw chart ongoing climate data
getNWTcharts <- function(sites = c("C1", "SDL", "D1"), mets = c("temp", "ppt")){
  
  # specify data package numbers for each site
  datnums <- list(
    # daily air temperature in C
    C1temp = 411,
    SDLtemp = 413,
    D1temp = 412,
    # daily precipitation in mm
    C1ppt = 414,
    SDLppt = 416,
    D1ppt = 415
  )
  
  # pare datnums to sites and mets of interest
  datnums <- datnums[grepl(stringi::stri_flatten(sites, collapse = "|"), names(datnums))]
  datnums <- datnums[grepl(stringi::stri_flatten(mets, collapse = "|"), names(datnums))]
  
  # read data to list
  getNWTdatlist(datnums)
  
}

# function to compile NWT infilled climate data
getNWTchartsinfilled <- function(sites = c("C1", "D1"), mets = c("temp", "ppt")){
  
  # specify data package numbers for each site
  datnums <- list(C1temp_infilled = 185,
                  D1temp_infilled = 187,
                  C1ppt_infilled = 184,
                  D1ppt_infilled = 186
                    
  )
  # pare datnums to sites and mets of interest
  datnums <- datnums[grepl(stringi::stri_flatten(sites, collapse = "|"), names(datnums))]
  datnums <- datnums[grepl(stringi::stri_flatten(mets, collapse = "|"), names(datnums))]
  
  # read data to list and return
  getNWTdatlist(datnums)
}


# function to compile NWT raw data logger daily climate data
getNWTdailyloggers <- function(sites = c("C1", "SDL", "GL4", "D1"), startyr = 1986, endyr = NA){
  
  # store current year
  currentyr <- lubridate::year(Sys.Date())
  
  # assign current year if end date not specified
  if(is.na(endyr)){
    endyr = currentyr
  }
  
  # specify data package numbers for each site
  datnums <- list(C121x = 400,
                  SDL21x = 78,
                  D121x = 70,
                  C123x = 401,
                  SDL23x = 405,
                  D123x = 402,
                  GL4 = 148 #cr10 (1997-2013), cr1000 (2013-2017), cr850 (2017-2019)
                  
  )
  # pare datnums by years to search (all or only 2000 onwards)
  if(startyr>1999){
    # remove 21x from datnums
    datnums <- datnums[!grepl("21x", names(datnums))]
  }
  
  # select sites user specified
  datnums <- datnums[grepl(stringi::stri_flatten(sites, collapse = "|"),names(datnums))]
  
  # read data to list and return
  getNWTdatlist(datnums)
}




# function to read NWT hourly infilled logger data (only one dataset -- all sites in one)
getNWThrlyinfilled <- function(sites = c("C1", "SDL", "D1")){
  
  # only one hourly infilled dataset (has all sites in one)
  dat <- getTabular(168)
  return(dat)
  
}


#c1aspirated <- getTabular(252)



# == FOR SNOTEL DATA ======

# function to read in snotel sites near NWT based on urls (CTW can't figure out url pattern re: 05J##S, ## doesn't correspond to site ID)
# > meaning: urls may break in future, if so use snotelr package
getSnotelNeighbors <- function(sites = c("Niwot", "UniversityCamp","Sawtooth", "HighLonesome")){
  
  # store urls for nearby snotel sites in list
  snotelpath <- list(Niwot = "https://wcc.sc.egov.usda.gov/nwcc/tabget?state=CO&stationidname=05J42S-NIWOT",
                     UniversityCamp = "https://wcc.sc.egov.usda.gov/nwcc/tabget?state=CO&stationidname=05J08S-UNIVERSITY+CAMP",
                     Sawtooth = "https://wcc.sc.egov.usda.gov/nwcc/tabget?state=CO&stationidname=05J45S-SAWTOOTH",
                     HighLonesome = "https://wcc.sc.egov.usda.gov/nwcc/tabget?state=CO&stationidname=05J46S-HIGH+LONESOME"
  )
  
  # select sites specified by user
  snotelpath <- snotelpath[sites]
  
  # read data to list
  snoteldat <- lapply(snotelpath, function(x) read.csv(x, header = T, comment.char = "#", strip.white = T, blank.lines.skip = T))
  
  return(snoteldat)
}

