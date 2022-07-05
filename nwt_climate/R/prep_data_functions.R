# functions to prepare input climate datasets

# set pathway to static input data files
# e.g., if user needs to download a climate dataset and it cannot be read in dynamically
set_path <- function(){
  
}

na_vals <- c(" ", "", ".", NA, NaN, "NA", "NaN", -9999)

#fxn to convert F to C
F2C <- function(xF){
  xC <- (xF - 32) * (5/9)
  xC <- round(xC, 2)
  return(xC)
}

# get snotel datasets
read.csv()

prepSnotel <- function(){
  
}



prepNOAA <- function(dat){
    dat$DATE <- as.Date(dat$DATE)
    tempdat <- data.frame(station = unique(dat$NAME),
                          stationID = unique(dat$STATION),
                          date = seq.Date(min(dat$DATE), max(dat$DATE), 1))
    tempdat <- tempdat %>%
      mutate(yr = year(date),
             mon = month(date), 
             doy = yday(date)) %>%
      left_join(dat, by = c("date" = "DATE"))
    keepnames <- which(colnames(tempdat) %in% c("TMAX", "TMIN", "PRCP", "SNOW"))
    tempdat <- tempdat[,c(1:6, keepnames)]
    return(tempdat)
}



  



# prepare CSU climate center data
prepCSU <- function(dat){
  tempdat <- dat
  tempdat$station <- names(dat)[1]
  names(tempdat)[1] <- "date"
  tempdat <- mutate(tempdat, yr = year(date),mon = month(date), doy = yday(date), stationID = NA) %>%
    rename(TMAX = maxt, TMIN = mint, PRCP = pcpn) %>%
    select(station, stationID, date, yr, mon, doy, TMAX, TMIN, PRCP) %>%
    mutate(station = gsub("[.]", " ", station)) %>%
    # remove alpha chars from temp and ppt cols, convert from char to numeric
    mutate_at(vars("TMAX", "TMIN", "PRCP"), .funs = function(x) gsub("T", "0", x)) %>%
    mutate_at(vars("TMAX", "TMIN", "PRCP"), .funs = function(x) as.numeric(gsub("A|S|M", "", x))) %>%
    # convert american to metric
    # F to C
    mutate_at(vars("TMAX", "TMIN"),~F2C(.)) %>%
    # in to mm
    mutate(PRCP = round(PRCP*25.4, 2))
  
  # retrieve station ID from metadata
  ID <- metadat_df$unlist.metadat.[grepl(unique(tempdat$station), metadat_df$station) & grepl("Station ID", metadat_df$unlist.metadat.)]
  ID <- gsub("Station ID: ", "", ID)
  tempdat$stationID  <- ID
  return(tempdat)
}

# == FOR AMERIFLUX =====

# function to read in Ameriflux data for sites at/near NWT (static file read-in)
# > for dynamic read in, use amerifluxr package (CTW slightly prefers more control with static file read in to amerifluxr package)

getAmeriflux <- function(fluxpath){
  
  # id unzipped folders that have ameriflux data
  fluxfolders <- list.dirs(fluxpath, full.names = T)
  # grab AMF folders only
  fluxfolders <- fluxfolders[grep("AMF", fluxfolders)]
  # grab .csv files within each folder
  fluxfiles <- lapply(fluxfolders, function(x) list.files(x, pattern = "csv$", full.names = T))
  fluxfiles <- unlist(fluxfiles)
  # use station ID as name
  names(fluxfiles) <- stringr::str_extract(fluxfiles, pattern = "US-NR[0-9]")
  
  # read in data to list
  datlist <- lapply(fluxfiles, function(x) read.csv(x, skip =2, colClasses = "character", na.strings = na_vals))
  
  # append site info to data (metadata spreadsheet should be in same folder, if not ignore)
  fluxmeta <- lapply(fluxfolders, function(x) list.files(x, pattern = "[.]xl", full.names = T))
  names(fluxmeta) <- names(fluxfiles)
  # drop anything that doesn't have a value
  #fluxmeta <- fluxmeta[sapply(fluxmeta, nchar)>0]
  
  # read in metadata via readxl and trim to site info and location metadata
  metalist <- lapply(fluxmeta, function(x) readxl::read_excel(x, trim_ws = T))
  metalist <- lapply(metalist, function(x) subset(x, grepl("elev$|lat$|long$|site_name", VARIABLE, ignore.case = T), select = c(SITE_ID, VARIABLE, DATAVALUE)))  
  metalist <- lapply(metalist, function(x) tidyr::spread(x, VARIABLE, DATAVALUE))
  
  # column-bind  metadata to datlist data frames
  for(i in names(datlist)){
    datlist[[i]] <- cbind(data.frame(metalist[[i]]), data.frame(datlist[[i]]))
  }
  
  # return Ameriflux data with site metadata as leading columns
  return(datlist)
}



prepAmeriflux <- function(dat, mets = c("TA", "P")){
  
  # prep search string for columns to keep
  keepcols <- paste0("^", mets, "($|_)")
  keepcols <- stringr::str_flatten(keepcols, collapse = "|")
  sitetimecols <-  paste0("^", c("SITE", "LOCATION", "TIMESTAMP"), "_")
  sitetimecols <- stringr::str_flatten(sitetimecols, collapse = "|")
  allcols <- paste(sitetimecols, keepcols, sep = "|")
  
  # subset Ameriflux to timestamp columns, variables, and variable flags of interest
  tempdat <- subset(dat, select = grepl(allcols, names(dat)))
  
  # separate date from time
  tempdat$date_start <- substr(tempdat$TIMESTAMP_START, start = 1, stop = 8) 
  tempdat$date_end <- substr(tempdat$TIMESTAMP_END, start = 1, stop = 8)
  tempdat$time_start <- substr(tempdat$TIMESTAMP_START, start = 9, stop = 12)
  tempdat$time_end <- substr(tempdat$TIMESTAMP_END, start = 9, stop = 12)
  tempdat$TIMESTAMP_START <- as.POSIXct(tempdat$TIMSETAMP_START, format = "%Y%m%d%H%M%OS", tz = "UTC")
  tempdat$TIMESTAMP_END <- as.POSIXct(tempdat$TIMSETAMP_END, format = "%Y%m%d%H%M%OS", tz = "UTC")
  
  # clean up
  tempdat <- dplyr::select(tempdat, date_start:fluxname, TIMESTAMP_START:ncol(tempdat)) %>%
    mutate_at(.vars = c("date_start", "date_end"), .funs = function(x) as.Date(x, format = "%Y%m%d")) %>%
    mutate_at(.vars = names(.)[grepl("TIME", names(.))], as.POSIXct())
    mutate_at(.vars = names(.)[grepl(keepcols, names(.))], as.numeric)
  
  return(tempdat)
}


# == GENERIC FUNCTIONS =====
prep_temp <- function(){
  
}



prep_ppt <- function(){
  
}


