#functions to infill precipitation following tim kittel's methods


# precipitation infill functions
tk_ppt_movingfill <- function(dat, target_site, missing_dates, site_order, window_days=13){
  # initiate empty data frame for storing predictions and regression info
  infill_df_m1 <- data.frame()
  # initiate counter at position 1
  i <- 1
  # initiate infill event counter at 1
  e <- 1
  while(i <=length(missing_dates)){
    
    # print dates being infilled to keep track of progress
    print(paste("Predicting missing values for", missing_dates[i]))
    
    # specify time window
    # find first date not NA looking backwards and count days specified back from that
    #subset df to all dates before missing date  
    ## specify xcol
    xcol <- paste0(target_site, "_tmean")
    firstnotNA <- max(dat$date[dat$date < missing_dates[i] & !is.na(dat[[xcol]])])
    lastnotNA <- min(dat$date[dat$date > missing_dates[i] & !is.na(dat[[xcol]])])
    #specify dates this applies to
    time_window <- missing_dates[which(missing_dates > firstnotNA & missing_dates < lastnotNA)]
    
    start_date <- firstnotNA - window_days # subtract 13 bc including the first and last not NA dates, so 14 days total
    end_date <- lastnotNA + window_days
    
    # subset dat
    temp_df <- subset(dat, date >= start_date & date <= end_date)
    
    # iterate through infill hiearchy and pick best r2 with pval < 0.05
    r2_df <- data.frame()
    for(site in site_order){
      ycol <- paste0(site, "_tmean")
      # check there are observations > 0 for explanatory site
      if(all(is.na(temp_df[ycol]))){
        next
      }
      # check for multiple loggers if infill source a logger -- if multiple, skip infill with logger
      if(grepl("cr", site)){
        # id logger col
        logcol <- paste0(gsub("cr", "", site), "_logger")
        #store logger val
        logger <- unique(temp_df[[logcol]])
        logger <- logger[!is.na(logger)] %>% str_flatten(collapse = " ")
      } else{
        logger <- NA
      }
      mod <- lm(formula = paste0(target_site, "_tmean ~ ", ycol), data = temp_df)
      r2_df <- rbind(r2_df, 
                     data.frame(cbind(site = site,
                                      logger = logger, 
                                      r2 = summary(mod)$r.squared,
                                      pval = summary(mod)$coefficients[8])))
      
    }
    
    # make stored r2 and pval numeric
    r2_df$r2 <- as.numeric(r2_df$r2) 
    r2_df$pval <- as.numeric(r2_df$pval)
    # select best
    best <- subset(r2_df, pval <= 0.05 & r2 == max(r2, na.rm = T))
    # if nothing has signif pval, select best r2
    if(nrow(best) == 0){
      best <- subset(r2_df, r2 == max(r2, na.rm = T))
    }
    
    # infill missing values in time_window based on best model
    best_mod <- lm(formula = paste0(target_site, "_", m, " ~ ", best$site, "_", m), data = temp_df)
    tempinfill <- predict(best_mod, newdata = subset(dat, date %in% time_window))
    tempinfill_df <- cbind(date = as.character(time_window), infill = tempinfill) %>%
      as.data.frame %>%
      mutate(metric = m,
             source.station = best$site,
             logger = best$logger,
             pval = summary(best_mod)$coefficients[8],
             r2 = summary(best_mod)$r.squared,
             n.obs = nrow(best_mod$model),
             equation = paste0("y = ",  best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
             infillrun = length(time_window),
             infillevent = e,
             method = paste0((window_days+1),"-d moving window")) %>%
      # convert date to Date class
      mutate(date = as.Date(date, format = "%Y-%m-%d"))
    
    # append model infill to master infill df
    infill_df_m1 <- rbind(infill_df_m1, tempinfill_df)
    
    # indicate how many days infilled to update progress
    print(paste(length(time_window), "days predicted"))
    
    # once infilled, update i (date after last date infilled) and increment infill event
    last_infilled <- which(missing_dates == max(time_window))
    i <- last_infilled +1
    e <- e+1
    # clean up things that shouldn't be recycled
    rm(r2_df, tempinfill_df, best, time_window, start_date, end_date)
  }
  # return infilled df when finished
  return(infill_df_m1)
}



# build multi-year regression, -31 and +30 days before and after target infill date
tk_ppt_seasonalfill <- function(dat, target_site, missing_dates, site_order, nobs_limit = 14){
  
  # initiate data frame for storing results
  infill_df_season <- data.frame()
  
  # iterate through each date with missing temp data
  for(d in as.character(missing_dates)){
    # print out to console to keep track of progress
    print(paste("Predicting",d))
    
    d <- as.Date(d, format = "%Y-%m-%d")
    
    # iterate through infill hiearchy and pick best r2 with pval < 0.05
    r2_df <- data.frame()
    for(site in site_order){
      
      # subset dat
      temp_df <- subset(dat, date > (d-31) & date < (d+30))
      if(grepl("cr", site)){
        # id logger col
        logcol <- paste0(site, "_logger") 
        #id logger deployed at time of missing observation
        logger <- dat[[logcol]][dat$date == d]
        #subset to that logger
        temp_df <- filter(temp_df, get(logcol) == logger)
      } else{
        logger <- NA
      }
      ycol <- paste0(site, "_ln") # specify logged ycol
      ycol_clean <- paste0(site, "_clean") # specify clean un-logged ycol
      # check there are observations > 0 for explanatory site
      if(all(is.na(temp_df[ycol]))){
        next
      }
      # check that there is a value for x (clean col, 0s okay) on the missing date
      if(is.na(temp_df[[ycol_clean]][temp_df$date == d])){
        next
      }
      # check that there are days where both target site and source station have value so can run lm model
      if(all(is.na(temp_df[[paste0(target_site, "_ln")]][!is.na(temp_df[ycol])]))){
        next
      }
      # run lm model and store results
      mod <- lm(formula = paste0(target_site,"_ln ~ ", ycol), data = temp_df)
      r2_df <- rbind(r2_df, 
                     data.frame(cbind(site = site,
                                      logger = logger, 
                                      r2 = summary(mod)$r.squared,
                                      pval = summary(mod)$coefficients[8],
                                      nobs = nrow(mod$model))))
    }
    
    # make numeric cols numeric
    r2_df$nobs <- as.numeric(r2_df$nobs)
    r2_df$r2 <- as.numeric(r2_df$r2)
    r2_df$pval <- as.numeric(r2_df$pval)
    
    # remove any row that has an r2 of 1 (unrealistic) and filter out any model nobs less than limit specified (TK has a min of 14)
    if(nrow(r2_df)>0){
      r2_df <- subset(r2_df, r2 != 1 & nobs >= nobs_limit)
    }
    # if r2_df empty because no models could be run, state that in infill df and move on
    if(nrow(r2_df) == 0){
      tempinfill_df <- data.frame(date = d, infill = NA,
                                  source.station = NA,
                                  logger = NA, 
                                  pval = NA,
                                  r2 = NA,
                                  n.obs = NA,
                                  equation = NA,
                                  infillrun = NA,
                                  method = "seasonal")
      # append model infill to master infill df
      infill_df_season <- rbind(infill_df_season, tempinfill_df)
      # clean up things that shouldn't recycled and move on
      rm(r2_df, tempinfill_df)
      # move on to next date
      next
    }
    
    
    # select best model
    best <- subset(r2_df, pval <= 0.05 & r2 == max(r2, na.rm = T))
    # if nothing has signif pval, select best r2
    if(nrow(best) == 0){
      best <- subset(r2_df, r2 == max(r2, na.rm = T))
    }
    # if there is a tie in r2, choose lowest pval (unless tie is r2==0)
    if(nrow(best) > 1 & !all(is.na(best$pval))){
      best <- subset(r2_df, r2 == max(r2, na.rm = T) & pval == max(pval, na.rm = T)) 
    }
    if(nrow(best) > 1 & all(is.na(best$pval))){
      # choose closest station by order hierarchy
      temp_order <- sapply(best$site, function(x) grep(x, c1_order))
      best_site <- names(temp_order)[min(temp_order)]
      best <- subset(r2_df, site == best_site)
    }
    
    # infill missing values based on best model
    ## re-subset temp_df based on best model
    temp_df <- subset(dat, date > (d-31) & date < (d+30))
    # if infill source is a logger, subset data to that logger only
    if(grepl("cr", best$site)){
      # id logger col
      logcol <- paste0(best$site, "_logger") 
      #subset to that logger
      temp_df <- filter(temp_df, get(logcol) == best$logger)
    } else{
      logger <- NA
    }
    # iterate through mean T and DTR, run lm, predict missing values, and store results in data frame
    best_mod <- lm(formula = paste0(target_site, "_ln", " ~ ", best$site, "_ln"), data = temp_df)
    # if source station had 0 rainfall on missing date, record -Inf for target (exponentiates to 0), else predict value
    if(dat[[paste0(best$site,"_clean")]][dat$date == d] == 0){
      tempinfill <- -Inf
    }else{
      tempinfill <- predict(best_mod, newdata = subset(dat, date == d))
    }
    tempinfill_df <- data.frame(date = d, infill = tempinfill,
                                source.station = best$site,
                                logger = best$logger, 
                                pval = summary(best_mod)$coefficients[8],
                                r2 = summary(best_mod)$r.squared,
                                n.obs = nrow(best_mod$model),
                                equation = paste0("y = ", best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
                                infillrun = length(d),
                                method = "seasonal")
    
    # append model infill to master infill df
    infill_df_season <- rbind(infill_df_season, tempinfill_df)
    
    # clean up things that shouldn't be recycled
    rm(r2_df, tempinfill_df, best, logger)
    
  }
  # return infilled df
  return(infill_df_season)
}


# build multi-year regression, +- 3 days on each side of target infill date
tk_ppt_historicfill <- function(dat, target_site, missing_dates, site_order, nobs_limit = 14){
  
  # initiate data frame for storing results
  infill_df_m2 <- data.frame()
  
  # iterate through each date with missing temp data
  for(d in as.character(missing_dates)){
    # print out to console to keep track of progress
    print(paste("Predicting",d))
    
    d <- as.Date(d, format = "%Y-%m-%d")
    # specify time window
    tempdoy <- dat$doy[dat$date == d]
    doyrange <- (tempdoy-3):(tempdoy+3)
    #adjust if at beginning or end of year -- going to ignore extra day in leap years, otherwise have to choose based on month and day, this is easier and close enough
    if(any(doyrange<1)){
      doyrange[doyrange<1] <- doyrange[doyrange<1] +365
    }
    if(any(doyrange>365)){
      doyrange[doyrange>365] <- doyrange[doyrange>365] - 365
    }
    
    # iterate through infill hiearchy and pick best r2 with pval < 0.05
    r2_df <- data.frame()
    for(site in site_order){
      # subset dat
      temp_df <- subset(dat, doy %in% doyrange)
      if(grepl("cr", site)){
        # id logger col
        logcol <- paste0(site, "_logger") 
        #id logger deployed at time of missing observation
        logger <- dat[[logcol]][dat$date == d]
        #subset to that logger
        temp_df <- filter(temp_df, get(logcol) == logger)
      } else{
        logger <- NA
      }
      ycol <- paste0(site, "_ln") # specify logged ycol
      ycol_clean <- paste0(site, "_clean") # specify clean un-logged ycol
      # check there are observations > 0 for explanatory site
      if(all(is.na(temp_df[ycol]))){
        next
      }
      # check that there is a value for x (clean col, 0s okay) on the missing date
      if(is.na(temp_df[[ycol_clean]][temp_df$date == d])){
        next
      }
      # check that there are days where both target site and source station have value so can run lm model
      if(all(is.na(temp_df[[paste0(target_site, "_ln")]][!is.na(temp_df[ycol])]))){
        next
      }
      # run lm model and store results
      mod <- lm(formula = paste0(target_site,"_ln ~ ", ycol), data = temp_df)
      r2_df <- rbind(r2_df, 
                     data.frame(cbind(site = site,
                                      logger = logger, 
                                      r2 = summary(mod)$r.squared,
                                      pval = summary(mod)$coefficients[8],
                                      nobs = nrow(mod$model))))
      
    }
    # make numeric cols numeric
    r2_df$nobs <- as.numeric(r2_df$nobs)
    r2_df$r2 <- as.numeric(r2_df$r2)
    r2_df$pval <- as.numeric(r2_df$pval)
    
    # remove any row that has an r2 of 1 (unrealistic) and filter out any model nobs less than limit specified (TK has a min of 14)
    if(nrow(r2_df)>0){
      r2_df <- subset(r2_df, r2 != 1 & nobs >= nobs_limit)
    }
    # if r2_df empty because no models could be run, state that in infill df and move on
    if(nrow(r2_df) == 0){
      tempinfill_df <- data.frame(date = d, infill = NA,
                                  source.station = NA,
                                  logger = NA, 
                                  pval = NA,
                                  r2 = NA,
                                  n.obs = NA,
                                  equation = NA,
                                  infillrun = NA,
                                  method = "multi-yr")
      # append model infill to master infill df
      infill_df_m2 <- rbind(infill_df_m2, tempinfill_df)
      # clean up things that shouldn't recycled and move on
      rm(r2_df, tempinfill_df)
      # move on to next date
      next
    }
    
    # select best model
    best <- subset(r2_df, pval <= 0.05 & r2 == max(r2, na.rm = T))
    # if nothing has signif pval, select best r2
    if(nrow(best) == 0){
      best <- subset(r2_df, r2 == max(r2, na.rm = T))
    }
    
    # infill missing values based on best model
    ## re-subset temp_df based on best model
    temp_df <- subset(dat, doy %in% doyrange)
    # if infill source is a logger, subset data to that logger only
    if(grepl("cr", best$site)){
      # id logger col
      logcol <- paste0(best$site, "_logger") 
      #subset to that logger
      temp_df <- filter(temp_df, get(logcol) == best$logger)
    } else{
      logger <- NA
    }
    # iterate through mean T and DTR, run lm, predict missing values, and store results in data frame
    best_mod <- lm(formula = paste0(target_site, "_ln", " ~ ", best$site, "_ln"), data = temp_df)
    # if source station had 0 rainfall on missing date, record -Inf for target (exponentiates to 0), else predict value
    if(dat[[paste0(best$site,"_clean")]][dat$date == d] == 0){
      tempinfill <- -Inf
    }else{
      tempinfill <- predict(best_mod, newdata = subset(dat, date == d))
    }
    tempinfill_df <- data.frame(date = d, infill = tempinfill,
                                source.station = best$site,
                                logger = best$logger, 
                                pval = summary(best_mod)$coefficients[8],
                                r2 = summary(best_mod)$r.squared,
                                n.obs = nrow(best_mod$model),
                                equation = paste0("y = ", best_mod$coefficients[[2]], "x + ", best_mod$coefficients[[1]]),
                                infillrun = length(d),
                                method = "multi-yr")
    
    # append model infill to master infill df
    infill_df_m2 <- rbind(infill_df_m2, tempinfill_df)
    
    # clean up things that shouldn't be recycled
    rm(r2_df, tempinfill_df, best, logger)
    
  }
  # return infilled df
  return(infill_df_m2)
}


# function to backfill days with accumulated ppt
backfill_ppt <- function(dat){
  # ID qdays > 1 date
  qdays_dates <- subset(dat, qdays > 1) %>%
    dplyr::select(date, ppt_tot, qdays)
  
  # add empty col for storing backfilled c1 ppt vals, source of backfill, and empty date vector for dates where no complete companion station date available for backfilling
  dat$backfill <- NA
  dat$Flag.1 <- NA
  dat$Flag.2 <- NA
  nobackfill <- NULL
  
  # test for loop
  for(i in 1:nrow(qdays_dates)){
    print(paste("Backfilling", qdays_dates$date[i]))
    
    # specify date seq to backfill
    event_dates <- seq.Date(qdays_dates$date[i] - (qdays_dates$qdays[i]-1), qdays_dates$date[i], 1)
    temp_df <- subset(dat, date %in% event_dates)
    
    # if accumulated ppt is 0, backfill all missing dates with 0
    if(qdays_dates$ppt_tot[i] == 0){
      dat$backfill[dat$date %in% event_dates] <- 0
      dat$infilldays[dat$date %in% event_dates] <- qdays_dates$qdays[i]
      dat$Flag.1[dat$date %in% event_dates] <- "A"
      dat$Flag.2[dat$date %in% event_dates] <- "G"
      print("0 accumulated ppt, 0s backfilled")
      next
    }
    
    # else, if accumulated ppt non-zero, backfill using infill values
    # sum project infill total
    infill_tot <- sum(temp_df$exp.ppt)
    # if infill sum not 0 when total accumulated is non-zero, proceed
    if(!(infill_tot ==0 & qdays_dates$ppt_tot[i] >0)){
      # calculate daily relative contribution of snotel ppt
      temp_df$relppt <- temp_df$exp.ppt/infill_tot
      dat$backfill[dat$date %in% event_dates] <- qdays_dates$ppt_tot[i] * temp_df$relppt
      dat$infilldays[dat$date %in% event_dates] <- qdays_dates$qdays[i]
      dat$Flag.2[dat$date %in% event_dates] <- "B"
      print("chart ppt backfilled based on source station")
      next
    } else{
      # store date for infill
      nobackfill <- c(nobackfill, as.character(qdays_dates$date[i]))
      dat$Flag.2[dat$date %in% event_dates] <- "0 ppt accumulated at all source stations"
      print("0 accumulated at source station")
    }
  }
  # infill Flag 1 for values backfilled by a source station
  Flag2B_dates <- dat$date[dat$Flag.2 == "B" & !is.na(dat$Flag.2)]
  for(f in Flag2B_dates){
    if(dat$method[dat$date == f] == "seasonal"){
      dat$Flag.1[dat$date == f] <- ifelse(dat$pval[dat$date == f] <= 0.05, "B", "C") #B < 0.05, C > 0.05 for meth1
    }else{
      # will be method 2 (historic fill)
      dat$Flag.1[dat$date == f] <- ifelse(dat$pval[dat$date == f] <= 0.05, "D", "E") #D < 0.05, E > 0.05 for meth2
    }
  }
  # infill missing days
  print("Infilling missing days (no period total/accumulated ppt):")
  dat <- infill_singles(dat)
  print("0 accumulated at source station for:")
  print(nobackfill)
  return(dat)
}


# function to infill days that are truly missing (no accumulated ppt)
## needs to be run AFTER backfill days (or can tuck function inside backfill ppt at the end)
infill_singles <- function(dat){
  single_dates <- dat$date[is.na(dat$Flag.2) & !is.na(dat$exp.ppt)]
  dat$backfill[dat$date %in% single_dates] <- dat$exp.ppt[dat$date %in% single_dates]
  dat$Flag.2[dat$date %in% single_dates] <- "A" # not adjustd for any period total (accumlated ppt)
  # assign Flag 1 based on method and pval
  for(f in as.character(single_dates)){
    print(paste("Infilling", f))
    f <- as.Date(f)
    if(dat$method[dat$date == f] == "seasonal"){
      dat$Flag.1[dat$date == f] <- ifelse(dat$pval[dat$date == f] <= 0.05, "B", "C") #B < 0.05, C > 0.05 for meth1
    }else{
      # will be method 2 (historic fill)
      dat$Flag.1[dat$date == f] <- ifelse(dat$pval[dat$date == f] <= 0.05, "D", "E") #D < 0.05, E > 0.05 for meth2
    }
  }
  return(dat)
}
