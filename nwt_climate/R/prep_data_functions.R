# functions to prepare input climate datasets

# set pathway to static input data files
# e.g., if user needs to download a climate dataset and it cannot be read in dynamically
set_path <- function(){
  
}

na_vals <- c(" ", "", ".", NA, NaN, "NA", "NaN", -9999)

#function to convert degrees F to C
F2C <- function(xF){
  xC <- (xF - 32) * (5/9)
  xC <- round(xC, 2)
  return(xC)
}

# function to convert inches to mm
in2mm <- function(xI){
  xM <- (xI * (1/25.4))
  xM <- round(xM, 2) 
  return(xM)
}


# get snotel datasets


prepSnotel <- function(dat){
  # mainly need to convert temps from F to C, ppt from inch to mm
  # and rename cols to something shorter
  # all dats have same data structure
  
  
}


# function to read, stack, and prep all ghcnd datasets
# > all .csvs downloaded should live in same folder
getGHCND <- function(datpath){
  
  datfiles <- list.files(datpath, full.names = T, pattern = "[.]csv$")
  
  # create master df for row-binding all datasets
  master <- data.frame()
  for(i in datfiles){
    dat <- read.csv(i, na.strings = c(" ", "", NA, "NA"), blank.lines.skip = T, stringsAsFactors = F)
    # convert date to date class -- in standard form so should be able to detect
    dat$DATE <- as.Date(dat$DATE)
    # bind_rows efficiently rbinds datasets with mismatched colnames, preserving all colnames 
    # if ever bind_rows bonks, can use setdiff() on names
    master <- dplyr::bind_rows(master, dat)
  }
 return(master) 
}

prepGHCND <- function(datpath){
  
  require(magrittr)
  require(dplyr)
  
  datfiles <- list.files(datpath, full.names = T, pattern = "[.]csv$")
  # create master df for row-binding all datasets
  master <- data.frame()
  for(i in datfiles){
    dat <- read.csv(i, na.strings = c(" ", "", NA, "NA"), blank.lines.skip = T, stringsAsFactors = F) 
    # bind_rows efficiently rbinds datasets with mismatched colnames, preserving all colnames 
    # if ever bind_rows bonks, can use setdiff() on names
    master <- dplyr::bind_rows(master, dat)
    }
      
  # pull in observation data and flags (tidy) so metric in one col, value in next, and flags (attributes) are in next
  # goal is to pull out time of observation where available
  mets_atts <- names(master)[grepl("ATTRIB", names(master))]
  mets <- gsub("_[A-Z]+$", "", mets_atts)
  master2 <- tidyr::gather(master, key = METRIC, value = val, c(all_of(mets), all_of(mets_atts))) %>%
    dplyr::mutate(type = ifelse(grepl("ATTR", METRIC), "ATTRIBUTE", "VALUE"),
                  METRIC = gsub("_[A-Z]+$", "", METRIC)) %>%
    tidyr::spread(type, val) %>%
    dplyr::mutate(VALUE = as.numeric(VALUE),
                  time_obs = stringr::str_extract(ATTRIBUTE, "[0-9]{4}")) %>%
    dplyr::group_by(NAME, METRIC) %>%
    dplyr::mutate(check_time = length(unique(time_obs[!is.na(time_obs)]))) %>%
    dplyr::ungroup()
     

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

getAmeriflux <- function(datpath){
  
  # id unzipped folders that have ameriflux data
  fluxfolders <- list.dirs(datpath, full.names = T)
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
  # clean up time formatting
  tempdat[grep("^TIME", names(tempdat))] <- lapply(tempdat[grep("^TIME", names(tempdat))], function(x) as.POSIXct(x, format = "%Y%m%d%H%M%OS", tz = "UTC"))
  tempdat[grep("^date", names(tempdat))] <- lapply(tempdat[grep("^date", names(tempdat))], function(x) as.Date(x, format = "%Y%m%d"))
  
  # rearrange cols
  tempdat <- subset(tempdat, select = c(grep("SITE", names(tempdat)), 
                                         grep("LOC", names(tempdat)),  
                                         grep("TIME", names(tempdat)),
                                         date_start:time_end,
                                         grep(keepcols, names(tempdat))))
  # keep site-time-loc cols where they are, order variables columns by variable-flagging
  tempdat <- tempdat[c(names(tempdat)[grepl("site|loc|date|time", names(tempdat), ignore.case = T)], 
                        # order variables and their flag cols alphabetically
                       sort(names(tempdat)[grepl(keepcols, names(tempdat))]))]
  
  # subset dataset to first date-time where at least one variable measured is not NA
  narows <- apply(tempdat[grep(keepcols, names(tempdat))],1, function(x) all(is.na(x))) # are all measured/flagged obs NA? (T/F)
  # first element that is false will be starting row that has firstnon-NA value, take that to end of data record
  tempdat <- tempdat[names(narows[!narows][1]):nrow(tempdat),] 
  
  # make met observation columns, e.g,. ppt, temp, numeric (corresponding flag columns stay as character)
  tempdat[grep(keepcols, names(tempdat))] <- sapply(tempdat[grep(keepcols, names(tempdat))], as.numeric)
  
  return(tempdat)
}



# == GENERIC FUNCTIONS =====
prep_temp <- function(){
  
}

# function to tidy (long-form) temp (this could be made generic for ppt too..)
tidytemp <- function(dat, datasource = NA, sep = "_", special = "flag", dropcol = NA){
  #if cols to drop, drop
  if(!is.na(dropcol)){
    dat <- dat[!colnames(dat) %in% dropcol] 
  }
  
  # gather temp and any special cols
  # id start of temp cols
  temp_pos <- min(grep("temp", colnames(dat)))
  dat_long <- dat %>%
    gather(met, temp, temp_pos:ncol(.)) %>%
    arrange(met, date) %>%
    # add month and year
    mutate(yr = year(date),
           mon = month(date),
           doy = yday(date)) %>%
    dplyr::select(c(1:date, yr:doy, met:ncol(.)))
  
  
  # if special cols exist, pull out special cols and rejoin wide-form
  if(!is.na(special)){
    tempspecial <- dat_long %>%
      filter(grepl(special, met)) %>%
      mutate(met = gsub(paste0(special,"_"), "", met))
    # rename temp col as special val
    colnames(tempspecial)[which(colnames(tempspecial) == "temp")] <- special
    
    # drop special vals from long-form dat and join wide to temp vals
    dat_long <- subset(dat_long, !grepl(special, met)) %>%
      left_join(tempspecial) %>%
      dplyr::select(LTER_site:date, yr:doy, met:ncol(.)) 
  }
  
  # make sure temp is numeric (can be coerced to character when gathered with flags)
  dat_long$temp <- as.numeric(dat_long$temp)
  
  # if desired, prefix temp and special col colname with datasource
  if(!is.na(datasource)){
    colnames(dat_long)[colnames(dat_long) %in% c("temp", special)] <- paste(datasource, colnames(dat_long)[colnames(dat_long) %in% c("temp", special)], sep = sep)
  }
  
  # return tidy dataset and clean up environment
  return(dat_long)
  rm(tempspecial, temp_pos)
}


# for wide-form
pare_temp <- function(dat, tempstring, flagstring = NA, keepcols, datecol = "date", DTR = T, maxstring = "max", minstring = "min", reps = NA){
  tempcols <- names(dat)[grepl(tempstring, names(dat), ignore.case = T)]
  tempdat <- dat[c(keepcols, tempcols)]
  
  if(!is.na(flagstring)){
    flagcols <- names(tempdat[grepl(flagstring, names(tempdat), ignore.case = T)])
    flagdat <- subset(dat, select = c(keepcols, flagcols))
    # if creating diurnal temp, collapse any flags from maxT and minT
    if(DTR){
      if(all(is.na(reps))){
        maxcol <- names(flagdat)[grepl(maxstring, names(flagdat), ignore.case = T)]
        mincol <- names(flagdat)[grepl(minstring, names(flagdat), ignore.case = T)]
        flagdat <- rowwise(flagdat) %>%
          mutate(flag_DTR = ifelse(is.na(get(maxcol)) & is.na(get(mincol)), NA, 
                                   ifelse(is.na(get(maxcol)), paste0(minstring, ": ", get(mincol)), 
                                          ifelse(is.na(get(mincol)), paste0(maxstring, ": ", get(maxcol)),
                                                 ifelse(get(maxcol) == get(mincol), as.character(get(maxcol)),
                                                        paste0(maxstring, ": ", get(maxcol), ", ", minstring, ": ", get(mincol))))))) %>%
          ungroup()
        #prefix whatever flagstring is being used
        names(flagdat)[names(flagdat) == "flag_DTR"] <- paste0(flagstring, "_DTR")
      }else{
        # for multiple sensors, calculate per sensor DTR
        for(r in reps){
          maxcol <- names(flagdat)[grepl(r, names(flagdat), ignore.case = T) & grepl(maxstring, names(flagdat), ignore.case = T)]
          mincol <- names(flagdat)[grepl(r, names(flagdat), ignore.case = T) & grepl(minstring, names(flagdat), ignore.case = T)]
          flagdat <- rowwise(flagdat) %>%
            mutate(flag_DTR = ifelse(is.na(get(maxcol)) & is.na(get(mincol)), NA, 
                                     ifelse(is.na(get(maxcol)), paste0(minstring, ": ", get(mincol)), 
                                            ifelse(is.na(get(mincol)), paste0(maxstring, ": ", get(maxcol)),
                                                   ifelse(get(maxcol) == get(mincol), as.character(get(maxcol)),
                                                          paste0(maxstring, ": ", get(maxcol), ", ", minstring, ": ", get(mincol))))))) %>%
            ungroup()
          #prefix whatever flagstring is being used and append sensor to newly created DTR flag with sensor
          names(flagdat)[names(flagdat) == "flag_DTR"] <- paste0(flagstring, "_DTR_", r)
        }
      }
      flagcols <- names(flagdat)[grep(flagstring, names(flagdat), ignore.case = T)]
    }
    # make long form
    flagdat <- gather(flagdat, metric, flag, c(flagcols))
    # drop flag cols from tempdat
    tempdat <- tempdat[!names(tempdat) %in% flagcols]
  }
  
  if(DTR){
    if(all(is.na(reps))){
      tempdat$DTR <- tempdat[[grep(maxstring, names(tempdat), ignore.case = T)]] - tempdat[[grep(minstring, names(tempdat), ignore.case = T)]]
    }else{
      # for multiple sensors, calculate per sensor DTR
      for(r in reps){
        maxcol <- names(tempdat)[grepl(r, names(tempdat), ignore.case = T) & grepl(maxstring, names(tempdat), ignore.case = T)]
        mincol <- names(tempdat)[grepl(r, names(tempdat), ignore.case = T) & grepl(minstring, names(tempdat), ignore.case = T)]
        tempdat[paste0("DTR_", r)] <- tempdat[maxcol] - tempdat[mincol]
      }
    }
  }
  
  # make temperature data long form (tidy)
  temppos <- min(which(names(tempdat) %in% tempcols))
  templong <- gather(tempdat, metric, temp, c(temppos:ncol(tempdat)))
  
  #re-join flagging
  if(!is.na(flagstring)){
    # clean up metric in flagdat
    #tempcols <- unique(templong$metric)
    # make regex
    metricvals <- str_flatten(unique(templong$metric), collapse = "|")
    flagdat$metric <- str_extract(flagdat$metric, pattern = metricvals)
    templong <- merge(templong, flagdat, all.x = T)
  }
  
  # subset data so only period of measurement included (instrument active lifetime)
  # > this matters for loggers vs. hmps
  templong_startdate <- min(with(templong, unique(get(datecol)[!is.na(temp)])))
  templong_enddate <- max(with(templong, unique(get(datecol)[!is.na(temp)])))
  templong <- subset(templong, get(datecol) >= templong_startdate)
  templong <- subset(templong, get(datecol) <= templong_enddate)
  
  # return dataset
  return(templong)
}


prep_ppt <- function(){
  
}


