# qaqc functions common across ppt and temp

# check all expected timestamps present

# plausibility checks (e.g. outside physically plausible value or range detection of instrument)
## allow data user to enter bounds (max, min) and screen for those

flag_limits <- function(dat, maxcol, mincol = NA, maxval = NA, minval = NA){
  # check to be sure max limit > min limit (if both present)
  stopifnot((maxval>minval | is.na(maxval) | is.na(minval)))
  
  # run through each metric and flag vals that exceed limits (on either end)
  # 1. max col (or single col entered)
  flagmax_hi <- which(dat[maxcol] > maxval)
  flagmax_lo <- which(dat[maxcol] < minval)
  # create flag col
  dat[paste0("qaflag_", maxcol)] <- c()
  # add flagging
  dat[flagmax_hi, paste0("qaflag_", maxcol)] <- "above limit"
  dat[flagmax_lo, paste0("qaflag_", maxcol)] <- "below limit"
  
  # 2. mincol
  if(!is.na(mincol)){
    flagmin_hi <- which(dat[mincol] > maxval)
    flagmin_lo <- which(dat[mincol] < minval)
    # create flag col
    dat[paste0("qaflag_", mincol)] <- NA
    # add flagging
    dat[flagmin_hi, paste0("qaflag_", mincol)] <- "above limit"
    dat[flagmin_lo, paste0("qaflag_", mincol)] <- "below limit"
  }
  
  # 3. if both max and min present, logic check to be sure max > min
  if(all(!is.na(list(maxcol, mincol))) & maxcol != mincol){
    flagmaxmin <- which(dat[maxcol] < dat[mincol])
    # add flagging
    dat[flagmaxmin, paste0("qaflag_", maxcol)] <- paste0("; min exceeds max", dat[flagmaxmin, paste0("qaflag_", maxcol)])
    # clean up flag
    dat[flagmaxmin, paste0("qaflag_", maxcol)] <- gsub("NA; ", "", dat[flagmaxmin, paste0("qaflag_", maxcol)])
  }
  
  # print results
  print(paste(maxcol,"QA flags (blank if none):"))
  print(table(dat[[paste0("qaflag_", maxcol)]]))
  if(!is.na(mincol)){
    print(paste(mincol,"QA flags (blank if none):"))
    print(table(factor(dat[[paste0("qaflag_", mincol)]])))
  }
  return(dat)
}

# flag max/min and plausibility limits for long-form data
flag_maxmin <- function(dat, metric = "temp", maxmet = "airtemp_max", minmet = "airtemp_min", groupvars = NA, maxlim = 40, minlim = -40){
  # check to be sure max limit > min limit (if both present)
  stopifnot((maxval>minval | is.na(maxval) | is.na(minval)))
  # create rowid for tracking
  dat$rowid <- rownames(dat)
  # subset to just max and min temp
  
  # global pass at max and min
  dat$qa_maxmin <- NA
  dat$qa_maxmin <- NA
  return(dat)
}
# check for all expected date-times in series
## POSIX and hours data could be added in to this function, but writing just for normal Date classes to start
check_datetime <- function(dat, datecol = "date", increment = 1, dateform = "%Y-%m-%d"){
  if(class(dat[[datecol]]) != "Date"){
    dat[datecol] <- as.Date(dat[[datecol]], format = dateform)
  }
  expected <- seq.Date(min(dat[[datecol]]), max(dat[[datecol]]), increment) # increment by 1 day by default
  if(all(dat[[datecol]] %in% expected) & all(expected %in% dat[[datecol]])){
    print("All expected date-times present!")
  }else{
    expected_missing <- expected[!expected %in% dat[[datecol]]]
    names(expected_missing) <- "expected dates missing"
    unexpected_dates <- dat[[datecol]][!dat[[datecol]] %in% expected]
    names(unexpected_dates) <- "unexpected dates present"
    return(list(expected_missing, unexpected_dates))
  }
}

