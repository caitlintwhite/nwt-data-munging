# functions to infill temp and precip following Tim Kittel methods



# 14-day moving window regression
tk_temp_movingfill <- function(dataset, metric, ytemp, xtemp, nmin = 10, days = 14, r2 = 0.6, p = 0.05){
  
  require(dplyr)
  
  #initialize df for storing infilled temp values
  infill_df <- data.frame()
  
  for(m in metric){
    dat <- as.data.frame(subset(dataset, met == m))
    # id missing dates
    missing_dates <- sort(dat["date"][is.na(dat[ytemp])])
    missing_dates <- as.Date(missing_dates)
    
    # for loop to execute moving window regressions on max T
    for(i in 1:length(missing_dates)){
      current_date <- missing_dates[i]
      # subset data to 2 weeks before and 2 weeks after missing date
      begin_date <- current_date - days
      end_date <-  current_date + days
      temp_df <- subset(dat, date >= begin_date & date <=end_date)
      # count complete records of chart tmax and logger tmax
      complete_obs <- sum(complete.cases(temp_df[c(xtemp, ytemp)]))
      # fill in date and count of complete observations
      infill_tempdf <- data.frame(missing_date = current_date, 
                                  met = m,
                                  logger = temp_df$logger[temp_df$date == current_date],
                                  complete_nobs = complete_obs,
                                  mon = month(current_date), yr = year(current_date))
      
      ## logic check: at least 10 complete observations (both sources have tmax data on same day)
      if(complete_obs < nmin) {
        #NAs for all other cols
        infill_tempdf <- cbind(infill_tempdf, 
                               xtemp = temp_df[[xtemp]][temp_df$date == current_date],
                               fit = NA, upr = NA, lwr = NA, se = NA,
                               adjr2 = NA, pval = NA, RSE = NA, method = NA)
        #next # skip to next date
      } else {
        # if passes logic check, continue with linear regression .. 
        temp_model <- lm(as.formula(paste(ytemp, xtemp, sep = "~")), data = temp_df)
        temp_predict <- predict.lm(temp_model, newdata = temp_df[temp_df$date == current_date,], se.fit = T, interval = "prediction")
        infill_tempdf <- cbind(infill_tempdf,
                               xtemp = temp_df[[xtemp]][temp_df$date == current_date],
                               temp_predict$fit,
                               se = temp_predict$se.fit,
                               adjr2 = summary(temp_model)$adj.r.squared,
                               pval = summary(temp_model)$coefficients[8],
                               RSE = summary(temp_model)$sigma,
                               method = paste(xtemp, days, "day lm"))
      }
      infill_df <- rbind(infill_df, infill_tempdf)
    }
  }
  # clean up
  infill_df <- infill_df %>%
    filter(adjr2 > r2 & pval <=p & !is.na(fit)) # keep only infilled values with r2 > 0.6 and pval <= 0.05 (per metadata methods)
  # correct colnames
  names(infill_df)[grepl("xtemp", colnames(infill_df))] <- xtemp
  return(infill_df)
  
}


# function to infill by monthly regression
tk_temp_monthfill <- function(dat, metric, ytemp, xtemp, mod){
  require(dplyr)
  
  # subset data to no NAs in y or x
  tempdat <- subset(dat, !is.na(dat[ytemp]) & !is.na(dat[xtemp]) & met %in% metric)
  # specify value to predict
  preddat <- subset(dat, met %in% metric & is.na(dat[ytemp]))
  
  # run model
  templm <- lm(as.formula(mod), data = tempdat)
  # null model to calculate p-vals for predicted vals
  nullmod <- lm(as.formula(paste(ytemp, "~ 1")), data = tempdat)
  # predict missing values
  predvals <- predict.lm(templm, newdata = preddat, 
                         se.fit = T, type = "response", interval = "prediction")
  
  # compile predictions with regression stats
  monthly_regress <- preddat %>%
    mutate(complete_nobs = nrow(templm$model)) %>%
    dplyr::select(date, met, logger, complete_nobs, mon, yr, eval(xtemp)) %>%
    rename(missing_date = date) %>%
    cbind(data.frame(predvals$fit,
                     se = predvals$se.fit,
                     adjr2 = summary(templm)$adj.r.squared,
                     pval = anova(nullmod, templm)$'Pr(>F)'[2],
                     RSE = sigma(templm),
                     method = paste(xtemp, "month lm")))
  return(monthly_regress)
  
}


# selection functions
# append infill event to longfill data frame for comparative purposes
compare_results <- function(dat_longfill, dat_shortfill){
  # start comparative dataframe
  dat_compare <- left_join(dat_longfill, dat_shortfill[c("date", "metric", "infillevent")]) %>%
    # append method 1 results to method 2
    rbind(dat_shortfill) %>%
    # create col for indicating whether selected or dropped
    ## also flag for models where nobs < # days infilled (compare if those would get dropped through mean approach)
    mutate(flag_mod = ifelse(infillrun > n.obs, TRUE, FALSE),
           flag_pval = ifelse(pval > 0.05, TRUE, FALSE))
  return(dat_compare)
}

# iterate by infill event and compare/select best model according to TK criteria
select_model <- function(dat_compare){
  
  # take overall means of pval and r2 by infill event
  dat_means <- group_by(dat_compare, infillevent, method) %>%
    summarise(mean_pval = mean(pval),
              mean_r2 = mean(r2)) %>%
    mutate(mean_pval_flag = ifelse(mean_pval < 0.05, FALSE, TRUE),
           r2_check1 = ifelse(mean_pval_flag == TRUE, 0, NA)) %>%
    ungroup() %>%
    # append model flag to indicate mods where nobs < days infilled
    left_join(distinct(dat_compare[c("infillevent", "method", "flag_mod")]))
  # specify r2_check1 as 1 if pair has 0
  # which events failed pval check?
  fail_events <- dat_means$infillevent[dat_means$r2_check1 == 0] %>% na.exclude()
  for(f in fail_events){
    dat_means$r2_check1[dat_means$infillevent == f & is.na(dat_means$r2_check1)] <- 1
  }
  # add second r2 check based on max r2
  dat_means <- group_by(dat_means, infillevent) %>%
    mutate(r2_check2 = mean_r2 == max(mean_r2)) %>%
    ungroup() %>%
    # select based on highest r2, as long as mean pval and model not flagged
    mutate(selectmod = ifelse(mean_pval_flag == TRUE | flag_mod == TRUE, FALSE, ## those that are flagged aren't selected outright
                              ifelse(mean_pval_flag == FALSE & flag_mod == FALSE & r2_check2 == TRUE, TRUE, FALSE))) ## those that pass flag checks and have highest r2 are selected outright
  # iterate through infill events that don't have a model selected and choose
  missingselect <- group_by(dat_means, infillevent) %>%
    summarise(sumselect = sum(selectmod)) %>%
    filter(sumselect == 0)
  for(m in missingselect$infillevent){
    temp_dat <- subset(dat_means, infillevent == m)
    temp_method <- temp_dat$method[temp_dat$flag_mod == F & temp_dat$mean_pval_flag == F]
    dat_means$selectmod[dat_means$infillevent == m & dat_means$method == temp_method] <- TRUE
  }
  return(dat_means)
}


