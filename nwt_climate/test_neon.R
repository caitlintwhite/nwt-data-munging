library(neonUtilities)

niwo_ppt <- loadByProduct("DP1.00006.001", site = "NIWO")
niwo_test <- niwo_ppt$PRIPRE_30min
niwo_test$date_start <- date(niwo_test$startDateTime)

niwo_dailyppt <- sub2daily(niwo_test, intervalcol = "date_start", mets = "priPrecipBulk", idcols = c("siteID"))
niwo_dailyppt <- dplyr::select(niwo_dailyppt, siteID:nobs, tot) %>%
  rename(measurement = tot, date = date_start) %>%
  mutate(metric = "ppt_tot")

ggplot(niwo_dailyppt, aes(date, measurement)) +
  geom_line(data = subset(chartppt_out_qc, station_name == "sdl" & date >= min(niwo_dailyppt$date)), col = "blue", alpha = 0.25, lwd = 1) +
  geom_line(data = subset(chartppt_out_qc, station_name == "d1" & date >= min(niwo_dailyppt$date)), col = "red", alpha = 0.25) +
  geom_line()
  facet_wrap(~month(date), scales = "free_x")
