library(lubridate)

sdl2 <- sdl
sdl2$yday <- yday(sdl$date)

# d1 chart, kittel et al. infilled
d1_kittel <- getTabular(187) %>% data.frame()
# c1 chart, kittel et al. infiled
c1_kittel <- getTabular(185) %>% data.frame()

max185 <- sdl$airtemp_max[sdl2$yday == 185]
min185 <- sdl2$airtemp_min[sdl2$yday == 185]
plot(asin(max185[!is.na(max185)]), type = "l", col = "red")
installlines(asin(min185), col = "blue")
plot(max185/min185, type = "l")


library(INQC)
sdl3 <- sdl %>%
  mutate(yr = year(date),
         mon = month(date),
         d = day(date),
         TX = airtemp_max,
         TN = airtemp_min,
         TG = round((airtemp_max + airtemp_min)/2,2)) %>%
  subset(select = c(date, yr, mon, d, TX, TN, TG))
paretogadget(sdl3$TX, ret = 365)
sdl3[355,] # correctly ID's bad tmax val
paretogadget(sdl3$TN, ret = 90) 
sdl3[c(2922,6220, 6245, 9483, 11311),] # at 60d or less
sort(sdl3$TN, decreasing = T)[1:30]
sdl3$maxd <- sdl3$TX - lag(sdl3$TX, 1)
spikes <- sdl3[jumps2(sdl3$date, sdl3$TX),]
ggplot(data = sdl3, aes(date, TX)) +
  geom_line(alpha = 0.5) +
  geom_point(data = spikes, aes(date, TX), col = "red")

friki <- sdl3[newfriki(sdl3$date, sdl3$maxd),]
sdlnoNA <-  subset(sdl3, !is.na(TX) & !is.na(TN))
IQRoutliers(date = sdlnoNA$date, value = sdlnoNA$TG)

p75 <- stats::quantile(sdl3$TX, 0.75, na.rm = TRUE)
p25 <- stats::quantile(sdl3$TX, 0.25, na.rm = TRUE)
iqr <- stats::IQR(sdl3$TX, na.rm = TRUE)
boxplot(sdl3$TX)
-9-16
13+16

plot(fft(sdlnoNA$TX))
sdlts <- ts(sdlnoNA$TX, start = c(1981,1), freq = 365)
harm <- (harmonic(sdlts))
max185ts <- ts(max185, start = (1981), deltat=1)
plot(harm[,1])
plot(harmonic(max185ts)[,1])

library(ShellChron)
sinreg(1:length(max185[!is.na(max185)]),max185[!is.na(max185)], plot = T)
library(caTools)
sdlnoNA$test <- runquantile(sdlnoNA$TX, k = 365*3, probs = c(0.9999))
sdlnoNA$test <- runquantile(sdlnoNA$TX, k = 365*5, probs = c(0.001))
ggplot(sdlnoNA, aes(date, TX)) +
  geom_line(alpha = 0.5) +
  geom_point(data = subset(sdlnoNA, TX >= test), aes(date, TX), col = "red")
sd(sdlnoNA$TX) #9.305888
sapply(split(sdlnoNA$TX, sdlnoNA$yr), sd) > (2* sd(sdlnoNA$TX))


library(climatol)


# test how climatol handles sdl, d1, and c1 chart.. need to prep how it wants it
write.table(subset(sdl3, yr >1981, select = c(date, yr, mon, d, TX, TN, TG)), "nwt_climate/long-term-trends_clim_sdl/sdl.txt")
# write d1 and c1 to match sdl years
d1_kittel$mon <- month(d1_kittel$date)
d1_kittel$day <- day(d1_kittel$date)
d1_out <- dplyr::select(d1_kittel, date, year, mon, day, max_temp, min_temp, mean_temp) %>%
  subset(date %in% sdl3$date) %>%
  rename(TX=max_temp, TN=min_temp, TG = mean_temp)
c1_kittel$mon <- month(c1_kittel$date)
c1_kittel$day <- day(c1_kittel$date)
c1_out <- subset(c1_kittel, date %in% sdl3$date, select = c(date, year, mon, day, max_temp, min_temp, mean_temp)) %>%
  rename(TX=max_temp, TN=min_temp, TG = mean_temp)

write.table(d1_out, "nwt_climate/long-term-trends_clim_sdl/d1.txt")
write.table(c1_out, "nwt_climate/long-term-trends_clim_sdl/c1.txt")

daily2climatol("nwt_climate/long-term-trends_clim_sdl/stations.txt", stcol = c(1:5,0), datcol = c(2:4, 6),anyi = 1982, anyf = 2017, varcli= "TN")
homogen("TN", 1982, 2017, expl = T)

plot(sdl3$TN[sdl3$yr > 1981], type = "l")
plot(dah[,3], col = "blue", type = "l")
lines(sdl3$TN[sdl3$yr > 1981], col = "pink")

load("TX_1982-2017.rda")
plot(dah[,3], col = "blue", type = "l")
lines(sdl3$TX[sdl3$yr > 1981], col = "pink") # climatol doesn't catch the tmax error in 1982


#Deviation from Meek and Hatfield
#Addition of second harmonic term in the regression
#tmax ~ cos(2*pi*doy/366) + sin(2*pi*doy/366) instead of tmax ~ cos(2*pi*doy/366)
#Provides better fit to data

tmax_lm <- lm(temp ~ cos(2*pi*doy/366) + sin(2*pi*doy/366), 
              data = tmax_doy)
tmax_pred <- data.frame("temp" = fitted(tmax_lm),
                        "doy" = 1:366,
                        "name" = "tmax_fit")

sdl_max <- sdl2 %>%
  mutate(lag1 = c(NA, diff(airtemp_max))) %>%
  group_by(yday) %>%
  summarise(maxt = max(airtemp_max, na.rm = T),
            maxd = max(abs(airtemp_max), na.rm = T))
sdl2$maxd <- c(NA, diff(sdl2$airtemp_max))
d1_kittel$yday <- yday(d1_kittel$date)
d1_max <- group_by(d1_kittel, mon, day) %>%
  summarise(maxt = min(max_temp[is.na(t_mean_pvalue)]))
d1_max$yday <- 1:366
tmax_lm <- lm(maxd ~ cos(2*pi*yday/366) + sin(2*pi*yday/366), #  
              data = sdl_max)
tmax_pred <- data.frame("temp" = fitted(tmax_lm),
                        "yday" = 1:366,
                        "name" = "tmax_fit")
tmax_pred <- data.frame((predict.lm(tmax_lm, se.fit = T, interval = "confidence", level = 0.95, newdata = data.frame(yday = 1:366)))$fit)
tmax_pred$yday <- 1:366
plot(fitted(tmax_lm))
predict(tmax_lm,level = 0.95, se.fit = T, interval = "prediction")
plot(sdl2$airtemp_max[sdl2$yday == 185], col = "pink")

ggplot(tmax_pred, aes(yday, fit)) +
  geom_text(data = sdl2, aes(yday, abs(maxd), label = year(date)), col = "orchid", alpha = 0.5) +
  geom_line() +
  geom_line(aes(yday, upr), lwd = 2, col= "red") +
  geom_line(aes(yday, lwr), lwd = 2, col = "red")


sdlres <- resid(lm(airtemp_max ~ year(date), data = sdl2))
sdl2$trendres <- sdlres
