#' ---
#' title: Gap-fill temperature data 
#' author: CTW  
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#' 
#' ### Temperature gap-filling and post-infill review
#' 
#' Test report to:
#' 1. Read in prepared, qc'd NWT and neighbor station temperature datasets
#' 2. Stack all for cycling through infilling
#' 3. Run infill methods, choose best per TK methods
#' 4. Quick review to be sure no infill values violate qc flags
#' 5. Write out for homogenization
#' 
#' All code will be displayed to show procedure and work out bugs



# -- SETUP ----
library(tidyverse)
options(stringsAsFactors = F)

source("nwt_climate/R/fetch_data_functions.R") # to read in prepped tidy datasets
source("nwt_climate/R/dataviz_functions.R")
source("nwt_climate/R/temp_infill_functions.R")

# set path to prepared data
datpath <- "~/Documents/nwt_lter/nwt_climate/data/"
# list all rds files in qc'd data folder (full path directory)
rdsfiles_qc <- list.files(paste0(datpath, "qc"), pattern = "rds", full.names = T)

# read in prepared precip data
charttemp <- get_tidydat("chartTEMP", rdsfiles_qc, "*")
logtemp <- get_tidydat("loggerTEMP", rdsfiles_qc, "*")
ameriflux <- get_tidydat("fluxT", rdsfiles_qc, "*")
snotel <- get_tidydat("telT", rdsfiles_qc, "*")
ghcnd <- get_tidydat("ghcndT", rdsfiles_qc, "*")
allsites <- readRDS(rdsfiles_qc[grep("infoTEM", rdsfiles_qc)])

# ctw needs to go back and fix code so sdl measurements not NA'd (probably happened due to tk and ctw adjusted flags)
# use what raw is unless was previously infilled
# previously infilled d1 and c1 are not NA'd, must have written out wrong dataset
charttemp2 <- mutate(charttemp, measurement = ifelse(is.na(tk_predicted), raw, measurement))
                    



# -- IMPROMPTU FUNCTIONS -----
idInfillDates <- function(dat, site, startyr){
  infilldates <- with(dat, unique(date[local_site == site & yr >= startyr & is.na(measurement)]))
  return(infilldates)
}



# -- PREP DATA FOR REGRESSIONS -----
# everything needs 1) airtemp_avg & 2) DTR + local_site, yr, date, doy, metric + measurement

mean_and_diurnalT <- function(dat){
  
  tempdat <- subset(dat, select = c(date, yr, mon, doy, local_site, metric, measurement)) %>%
    tidyr::spread(metric, measurement)
  # if avg temp not present, make
  if(!any(grepl("avg", names(tempdat)))){
    tempdat$airtemp_avg <- (tempdat$airtemp_max + tempdat$airtemp_min)/2
  }
  # if diurnal temp not present, make
  if(!any(grepl("DTR", names(tempdat), ignore.case = T))){
    tempdat$DTR <- tempdat$airtemp_max - tempdat$airtemp_min
  }
  
  # return all mets back in case helpful for crunching tmax and tmin after predictions
  tempdat <- gather(tempdat, metric, measurement, grep("temp|DTR", names(tempdat)))
  return(tempdat)
}

charttemp_lm <- mean_and_diurnalT(charttemp2)
nwtlog_lm <- mean_and_diurnalT(logtemp)
snotel_lm <- mean_and_diurnalT(snotel)
# for ameriflux, keep qc_measurement for ameriflux forest site, but raw for tvans sites
ameriflux$measurement <- with(ameriflux, ifelse(grepl("NR1", local_site), qc_measurement, raw_measurement))
ameriflux_lm <- mean_and_diurnalT(ameriflux) 
ghcnd_lm <- mean_and_diurnalT(subset(ghcnd, metric != "TOBS"))

# need date, yr, mon, doy, local_site, and measurement (qc'd)
alldats <- rbind(charttemp_lm, nwtlog_lm, snotel_lm, ameriflux_lm, ghcnd_lm) %>%
  subset(grepl("avg|DTR", metric)) %>%
  arrange(local_site, date, metric)

# make sure allsites ordered by pair rank
allsites <- arrange(allsites, local_site, final_rank, paired_site)



# -- INFILL NWT -----

# test seasonal infill again.. old code seems funny
d180 <- subset(alldats, local_site %in% c("d1_chart", "c1_chart", "sdl_chart") & yr %in% 1992) #%>%
  unite(metric, local_site, metric) %>%
  spread(metric, measurement)

predict(mod)
test <- subset(wide_df, metric == "airtemp_avg")
test2 <- cbind(mod$model, mod$fitted.values)
test <- cbind(test, fit = predict(mod, test))
d180_test1


d180.1204

d1chart_missing <- idInfillDates(alldats, "d1_chart", 1980)
d1_order <- with(allsites, paired_site[grepl("d1_chart", local_site) & final_rank!= 1])
d1_season <- tk_temp_movingfill(alldats, target_site = "d1_chart", missing_dates = d1chart_missing, site_order =  d1_order)

redo_dates <- unique(d1_season$date[is.na(d1_season$infill)])
d1_season_redo <- tk_temp_movingfill(alldats, target_site = "d1_chart", missing_dates = redo_dates, singlefill = T, site_order =  d1_order)
d1_season_1992 <- tk_temp_movingfill(alldats, target_site = "d1_chart", missing_dates = d1chart_missing[year(d1chart_missing)== 1992], singlefill = T, site_order =  d1_order)
d1_season_1992.cap <- tk_temp_movingfill(alldats, target_site = "d1_chart", missing_dates = d1chart_missing[year(d1chart_missing)== 1992], windowcap = 30, site_order =  d1_order)
d1_season_1992.cap2 <- tk_temp_movingfill(alldats, target_site = "d1_chart", missing_dates = d1chart_missing[year(d1chart_missing)== 1992], windowcap = 30, site_order =  d1_order)

d1_season_2005.cap <- tk_temp_movingfill(alldats, target_site = "d1_chart", missing_dates = d1chart_missing[year(d1chart_missing)== 2005], windowcap = 30, site_order =  d1_order)
d1_season_2005 <- tk_temp_movingfill(alldats, target_site = "d1_chart", missing_dates = d1chart_missing[year(d1chart_missing)== 2005], window_days = 14, singlefill = T, windowcap = 31, site_order =  d1_order)


ggplot(subset(temp_df, metric == "airtemp_avg"), aes(date, measurement, group = local_site, col = local_site)) +
  geom_line(alpha = 0.5) +
  geom_line(data = subset(temp_df, metric == "airtemp_avg" & local_site == "d1_chart"), lwd = 1, col = "black") +
  geom_line(data = subset(temp_df, metric == "airtemp_avg" & grepl("d1_cr1000_hmp", local_site)), lwd = 1, col = "purple") +
  geom_line(data = subset(temp_df, metric == "airtemp_avg" & grepl("USC00053500", local_site)), lwd = 1, col = "chocolate")
  
testeqs <- subset(tktemp, local_site == "D1" & year == 1992)
unique_eqs <- distinct(testeqs, t_mean_regression_equation, num_obs_in_t_mean_regression_equation) %>%
  subset(!is.na(t_mean_regression_equation)) %>%
  mutate(ct = 1:nrow(.))

testeqs <- left_join(testeqs, unique_eqs)
testeqs <- dplyr::select(testeqs, LTER_site:t_mean_regression_equation, ct)

testeqs <- group_by(testeqs, ct) %>%
  mutate(nobs_reg = length(date))
View(subset(testeqs, !is.na(ct) & flag_1 == "B"))


# try jan 02 2005 and jan 20 2005 may 25 and may 26 2005, all with d1 cr23x -- all have 28 obs, but in my code fewer than 28 obs
# try nov 23, 24, 30 
testdate <- as.Date("1992-11-23")

start_date <- firstnotNA - 13 # replicates nov 1992 infilling.. 
end_date <- lastnotNA + 13
start_date <- max(start_date, testdate - 30)
end_date <- min(end_date, testdate + 30)

start_date <- testdate - 30 # replicates 2005 infilling
end_date <- testdate + 30
testdats <- subset(alldats, local_site %in% c(target_site, "sdl_chart") & date >= start_date & date <= end_date) %>% #d1_cr23x
  unite(local_site, local_site, metric) %>%
  spread(local_site, measurement)
testdats$allthere <- apply(testdats[grep("airtemp|DTR", names(testdats))], 1, function(x) all(!is.na(x)))

start_dates_pres <- with(testdats, date[date <= firstnotNA & allthere])
end_dates_pres <- with(testdats, date[date >= lastnotNA & allthere])
tail(start_dates_pres, n = 14)
head(end_dates_pres, n = 14)
moddat <- subset(testdats, date %in% c(tail(start_dates_pres, n = 14),head(end_dates_pres, n = 14)))

summary(lm("d1_chart_airtemp_avg ~ sdl_chart_airtemp_avg", data = wide_df2)) #d1_cr23x
summary(lm("d1_chart_airtemp_avg ~ sdl_chart_airtemp_avg", data = wide_df2))$coefficients # still not quite a match, but closer
summary(lm("d1_chart_DTR ~ sdl_chart_DTR", data = moddat))$coefficients # but DTR is a match..

# got all 2005 dates to work, now try 1992 sdl chart infilling in for nov
# e.g., end of 1977 into 1978 at D1, only infill method 2 (3day historic) is method used (i think bc 30d window not possible.. was out for a few months)



# -- SDL loggers -----
unique(allsites$local_site[grepl("sdl", allsites$local_site)])

sdl21x_missing <- idInfillDates(alldats, "sdl_cr21x", 1980)
sdl21x_order <- with(allsites, paired_site[grepl("sdl_cr21x", local_site) & final_rank!= 1])
# moving window infill
sdl21x_season <- tk_temp_movingfill(alldats, target_site = "sdl_cr21x", missing_dates = sdl21x_missing, site_order =  sdl21x_order)
sdl21x_season_tempnobs <- tk_temp_movingfill(alldats, target_site = "sdl_cr21x", missing_dates = sdl21x_missing, site_order =  sdl21x_order)
# historic infill
sdl21x_historic <- tk_temp_historicfill(alldats, "sdl_cr21x", sdl21x_missing, sdl21x_order)


# selection for sdl21x
comparesdl21x <- rbind(sdl21x_historic, sdl21x_season_tempnobs)

select_sd121x <- select_model(comparesdl21x)
outstanding_dates <- sdl21x_missing[!sdl21x_missing %in% dat_means$date[dat_means$selectmod]]

sdlcr1000

#hmps

