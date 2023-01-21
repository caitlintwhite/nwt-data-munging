library(tidyverse)
library(lubridate)
library(cowplot)
theme_set(theme_test())

d1temp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.187.2&entityid=7d83b12a87738a45084d2f04d9256051")
d1ppt <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.186.3&entityid=e59295299fd2e7fbf748712f9b6851f5")
c1ppt <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.184.5&entityid=be2f9ce2465dddeec1e42538cbf626d8")
sdlwind <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-nwt.83.1&entityid=925e1e6f476a068ba738e0dcd112c9f3", na = c("", " ", "NaN", NaN, NA))

# pair temp and ppt
d1clim <- subset(d1ppt, select = c(LTER_site:precip)) %>%
  # join temp bc fewer years in that dat
  left_join(subset(d1temp, select = c(LTER_site:DTR))) %>%
  # note dates with mean temp > 32F or 0C
  mutate(above_freeze = mean_temp > 0,
         mo = month(date),
         doy = yday(date))

# reality check above freezing dates are mostly in summer months
subset(d1clim, above_freeze & precip >0) %>%
  group_by(year) %>%
  summarise(nobs = length(precip)) %>%
  ungroup() %>%
  ggplot(aes(year, nobs)) +
  geom_point() + #aes(col = factor(mo))
  geom_smooth(method = "gam") # doesn't look like there is an increasing trend, at least based on days above freezing

subset(d1clim, above_freeze) %>%
  group_by(year) %>%
  summarise(nobs = length(precip)) %>%
  ungroup() %>%
  ggplot(aes(year, nobs)) +
  geom_point() + #aes(col = factor(mo))
  geom_smooth() 

subset(d1clim, above_freeze) %>%
  group_by(year) %>%
  summarise(gdd = sum(mean_temp)) %>%
  ungroup() %>%
  ggplot(aes(year, gdd)) +
  geom_point() + #aes(col = factor(mo))
  geom_smooth(method = "lm")

mutate(d1clim, winter = mo %in% c(10:12, 1:5)) %>%
  group_by(year, winter) %>%
  summarise(totppt = sum(precip)) %>%
  ungroup() %>%
  ggplot(aes(year, totppt, col = winter)) +
  geom_point() + #aes(col = factor(mo))
  geom_smooth(method = "gam")

d1clim %>%
  group_by(year) %>%
  summarise(totppt = sum(precip)) %>%
  ungroup() %>%
  ggplot(aes(year, totppt)) +
  geom_line(col = "grey50", lty = 3) +
  geom_point() + #aes(col = factor(mo))
  geom_smooth(method = "gam")

# check how many days are > 0C in "winter" months
subset(d1clim, above_freeze & precip >0) %>%
  group_by(mo, year) %>%
  summarise(ppt = sum(precip)) %>%
  ungroup() %>%
  subset(mo %in% c(1:5, 10:12)) %>%
  ggplot(aes(year, ppt)) +
  geom_point() + #aes(col = factor(mo))
  geom_smooth(method = "gam") +
  facet_wrap(~mo) # not trivial amount in may

# what about reverse for non-winter
subset(d1clim, !above_freeze & precip >0) %>%
  group_by(mo, year) %>%
  summarise(ppt = sum(precip)) %>%
  ungroup() %>%
  subset(mo %in% c(6:9)) %>%
  ggplot(aes(year, ppt)) +
  geom_point() + #aes(col = factor(mo))
  geom_smooth(method = "gam") +
  facet_wrap(~mo) # this could all be snowfall

subset(d1clim, !above_freeze & precip >0) %>%
  group_by(mo, year) %>%
  summarise(ppt = sum(precip)) %>%
  ungroup() %>%
  subset(mo %in% c(6:9)) %>%
  ggplot(aes(factor(mo), ppt)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(aes(col = year)) + #+ #aes(col = factor(mo))
  scale_color_viridis_c()

# check to see if J Hux used winter adjusted or no
sdlppt <- read_csv("nwt_climate/nwt8-renewal_homogenize_climdat/data/sdl_ppt_1981-2020_draft.csv") 

mutate(sdlppt, winter = !month(date) %in% 6:9,
       wyo = ifelse(month(date) %in% c(10:12), year+1, year)) %>%
  group_by(wyo, winter) %>%
  summarise(ppt = sum(winter_adjusted)) %>%
  ungroup() %>%
  subset(wyo > 1990) %>%
  ggplot(aes(wyo, ppt, col = winter)) +
  geom_point() + 
  geom_smooth() # step function around 2000

# compare w d1 again
mutate(d1ppt, winter = !month(date) %in% 6:9,
  wyo = ifelse(month(date) %in% c(10:12), year+1, year)) %>%
  group_by(wyo, winter) %>%
  summarise(ppt = sum(precip)) %>%
  ungroup() %>%
  subset(wyo > 1990) %>%
  ggplot(aes(wyo, ppt, col = winter)) +
  geom_point() + 
  geom_smooth()

sdlwinter <- mutate(sdlppt, winter = !month(date) %in% 6:9,
                    wyo = ifelse(month(date) %in% c(10:12), year+1, year)) %>%
  group_by(wyo, winter) %>%
  summarise(sdlppt = sum(winter_adjusted)) %>%
  ungroup()

d1winter <- mutate(d1ppt, winter = !month(date) %in% 6:9,
       wyo = ifelse(month(date) %in% c(10:12), year+1, year)) %>%
  group_by(wyo, winter) %>%
  summarise(d1ppt = sum(precip)) %>%
  ungroup()

left_join(sdlwinter, d1winter) %>%
  mutate(decade = ifelse(wyo < 1990, 1980, ifelse(wyo < 2000, 1990, ifelse(wyo < 2010, 2000, 2010)))) %>%
  ggplot(aes(sdlppt, d1ppt, col = decade, group = decade, fill = decade)) +
  geom_text(aes(label = wyo)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_smooth(method = "lm") +
  #scale_color_viridis_c() +
  facet_wrap(~winter, scales = "free")

dplyr::select(sdlppt, year, date, winter_adjusted) %>%
  mutate(mo = month(date),
         wyo = ifelse(month(date) %in% c(10:12), year+1, year)) %>%
  left_join(d1ppt[c("date", "precip")]) %>%
  mutate(decade = ifelse(wyo < 1990, 1980, ifelse(wyo < 2000, 1990, ifelse(wyo < 2010, 2000, 2010)))) %>%
  data.frame() %>%
  subset(wyo < 2001) %>%
  ggplot(aes(winter_adjusted, precip, col = wyo, group = wyo, fill = wyo)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = F, alpha = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1), lty = 2, col = "grey50") +
  scale_color_viridis_c(option = "B") +
  scale_fill_viridis_c(option = "B") +
  facet_wrap(~mo, scales = "free")

# unadjusted
dplyr::select(sdlppt, year, date, precip) %>%
  mutate(mo = month(date),
         wyo = ifelse(month(date) %in% c(10:12), year+1, year)) %>%
  rename(sdlppt = precip) %>%
  left_join(d1ppt[c("date", "precip")]) %>%
  mutate(decade = ifelse(wyo < 1990, 1980, ifelse(wyo < 2000, 1990, ifelse(wyo < 2010, 2000, 2010)))) %>%
  data.frame() %>%
  subset(wyo < 1994) %>%
  ggplot(aes(sdlppt, precip, col = factor(wyo), group = wyo, fill = factor(wyo))) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = F, alpha = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1), lty = 2, col = "grey50") +
  scale_color_viridis_d(option = "B") +
  scale_fill_viridis_d(option = "B") +
  scale_y_continuous(limit = c(0,85)) +
  scale_x_continuous(limit = c(0,85)) +
  facet_wrap(~mo)

# just look at 1 month (e.g., jan)
dplyr::select(sdlppt, year, date, precip) %>%
  mutate(mo = month(date),
         wyo = ifelse(month(date) %in% c(10:12), year+1, year)) %>%
  rename(sdlppt = precip) %>%
  left_join(d1ppt[c("date", "precip")]) %>%
  mutate(decade = ifelse(wyo < 1990, 1980, ifelse(wyo < 2000, 1990, ifelse(wyo < 2010, 2000, 2010)))) %>%
  data.frame() %>%
  subset(month(date) == 7) %>%
  ggplot(aes(sdlppt, precip)) +
  geom_point(aes(col = wyo, group = wyo, fill = wyo), alpha = 0.8) +
  geom_smooth(aes(col = wyo, group = wyo, fill = wyo), method = "lm", alpha = 0.5) +
  geom_smooth(method = "lm", alpha = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1), lty = 2, col = "grey50") +
  scale_color_viridis_c(option = "B") +
  scale_fill_viridis_c(option = "B") +
  # scale_y_continuous(limit = c(0,85)) +
  # scale_x_continuous(limit = c(0,85)) +
  facet_wrap(~wyo)

# make joined dat with monthly cumulative sums and totals, and daily diffs
pptjoin <- dplyr::select(sdlppt, year, date, precip) %>%
  mutate(mo = month(date),
         wyo = ifelse(month(date) %in% c(10:12), year+1, year)) %>%
  rename(sdlppt = precip) %>%
  left_join(d1ppt[c("date", "precip")]) %>%
  mutate(decade = ifelse(wyo < 1990, 1980, ifelse(wyo < 2000, 1990, ifelse(wyo < 2010, 2000, 2010)))) %>%
  data.frame() %>%
  # totals and cumulative
  group_by(wyo, mo) %>%
  mutate(sdlrunning = cumsum(sdlppt),
         sdltot = sum(sdlppt),
         sdl_pptdays = sum(sdlppt > 0),
         d1running = cumsum(precip),
         d1tot = sum(precip),
         d1_pptdays = sum(precip > 0)) %>%
  ungroup() %>%
  mutate(sdl_d1_diff = sdlppt - precip,
         wymo = ifelse(mo %in% 10:12, mo-9, mo+3),
         doy = yday(date),
         wymo_letter = month(date, label = T, abbr = T))

# monthly comparison, color month and wrap wyo to look for shifts
ggplot(data = distinct(pptjoin, wymo, wyo, sdlrunning, d1running), aes(sdlrunning, d1running, col = wyo, group= wyo)) +
  geom_line(lwd = 1, alpha = 0.5) +
  #geom_smooth(method = "lm") +
  geom_abline(aes(intercept = 0, slope = 1), lty = 2, col = "grey50") +
  scale_color_viridis_c() +
  facet_wrap(~wymo)

ggplot(pptjoin, aes(doy, sdl_d1_diff)) +
  geom_point(aes(col = wyo), alpha = 0.1) +
  geom_smooth(aes(col = wyo, group = wyo, fill = wyo), se = F, method = "lm", alpha = 0.5) +
  #geom_smooth(method = "lm", alpha = 0.5) +
  #geom_abline(aes(intercept = 0, slope = 1), lty = 2, col = "grey50") +
  scale_color_viridis_c(option = "B") +
  scale_fill_viridis_c(option = "B") +
  # scale_y_continuous(limit = c(0,85)) +
  # scale_x_continuous(limit = c(0,85)) +
  facet_wrap(~wymo_letter, scales = "free")

ggplot(subset(pptjoin, date >=as.Date("1981-08-01")), aes(wyo, sdl_d1_diff)) +
  geom_violin(aes(group = wyo, col = wyo, fill = wyo), alpha = 0.1) +
  geom_hline(aes(yintercept = 0)) +
  geom_smooth(method = "lm", col = "green", fill = "green", alpha = 0.5) +
  geom_smooth(method = "gam", fill = "blue", alpha = 0.5) +
  #geom_abline(aes(intercept = 0, slope = 1), lty = 2, col = "grey50") +
  #scale_color_viridis_c(option = "B") +
  #scale_fill_viridis_c(option = "B") +
  # scale_y_continuous(limit = c(0,85)) +
  scale_x_continuous(breaks = seq(1980, 2020, 4)) +
  facet_wrap(~wymo_letter, scales = "free") +
  theme_bw() +
  coord_flip()

# monthly totals
ggplot(data = distinct(pptjoin, decade, wymo, wyo, wymo_letter, sdltot, d1tot), aes(sdltot, d1tot)) +
  geom_point(aes(fill = wyo), pch = 21, alpha = 0.8) +
  geom_smooth(aes(col = factor(decade), group = decade), se = F, method = "lm") +
  geom_abline(aes(intercept = 0, slope = 1), lty = 2, col = "grey50") +
  scale_color_viridis_d() +
  scale_fill_viridis_c() +
  facet_wrap(~wymo_letter)

distinct(pptjoin, decade, wymo, wyo, wymo_letter, sdltot, d1tot) %>%
  subset(wyo %in% 1982:2020) %>%
  group_by(wyo) %>%
  mutate(mean_diff = mean(sdltot - d1tot)) %>%
  ungroup() %>%
  ggplot(aes(wymo, sdltot - d1tot, col = wyo, fill = wyo)) +
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.8, lty = 2) +
  geom_smooth(method = "gam") +
  geom_line(aes(wymo, mean_diff), col = "red", lwd = 1.5, alpha = 0.8) +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey50") +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(2,12,2)) +
  facet_grid(~wyo)

distinct(pptjoin, decade, wymo, wyo, wymo_letter, sdltot, d1tot) %>%
  subset(wyo %in% 1982:2020) %>%
  group_by(wyo) %>%
  mutate(mean_diff = mean(sdltot - d1tot)) %>%
  ungroup() %>%
  ggplot(aes(wyo, sdltot - d1tot, col = wyo, fill = wyo)) +
  geom_point(alpha = 0.8) +
  #geom_line(alpha = 0.8, lty = 2) +
  geom_smooth(method = "gam") +
  #geom_line(aes(wyo, mean_diff), col = "red", lwd = 1.5, alpha = 0.8) +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey50") +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(1980,2020,4)) +
  theme_bw() +
  facet_wrap(~wymo)

ggplot(data = distinct(pptjoin, wymo, wyo, sdl_pptdays, d1_pptdays, decade), aes(sdl_pptdays, d1_pptdays)) +
  geom_point(aes(fill = wyo), pch = 21, alpha = 0.85) +
  geom_smooth(aes(col = factor(decade)), method = "lm", se= F) +
  geom_abline(aes(intercept = 0, slope = 1), lty = 2, col = "grey50") +
  scale_color_viridis_d() +
  scale_fill_viridis_c() +
  facet_wrap(~wymo)

ggplot(data = distinct(pptjoin, wymo, wyo, sdl_pptdays, d1_pptdays, decade), aes(wyo, sdl_pptdays - d1_pptdays)) +
  geom_point(aes(col = wyo), alpha = 0.85) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = "gam") +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey50") +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(1980,2020,4)) +
  theme_bw() +
  facet_wrap(~wymo) +
  coord_flip()


# repeat but for c1
c1_pptjoin <- dplyr::select(sdlppt, year, date, precip) %>%
  mutate(mo = month(date),
         wyo = ifelse(month(date) %in% c(10:12), year+1, year)) %>%
  rename(sdlppt = precip) %>%
  left_join(c1ppt[c("date", "precip")]) %>%
  mutate(decade = ifelse(wyo < 1990, 1980, ifelse(wyo < 2000, 1990, ifelse(wyo < 2010, 2000, 2010)))) %>%
  data.frame() %>%
  # totals and cumulative
  group_by(wyo, mo) %>%
  mutate(sdlrunning = cumsum(sdlppt),
         sdltot = sum(sdlppt),
         sdl_pptdays = sum(sdlppt > 0),
         c1running = cumsum(precip),
         c1tot = sum(precip),
         c1_pptdays = sum(precip > 0)) %>%
  ungroup() %>%
  mutate(sdl_c1_diff = sdlppt - precip,
         wymo = ifelse(mo %in% 10:12, mo-9, mo+3),
         doy = yday(date),
         wymo_letter = month(date, label = T, abbr = T))

ggplot(data = distinct(c1_pptjoin, wymo, wyo, sdl_pptdays, c1_pptdays, decade), aes(wyo, sdl_pptdays - c1_pptdays)) +
  geom_point(aes(col = wyo), alpha = 0.85) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = "gam") +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey50") +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(1980,2020,4)) +
  theme_bw() +
  facet_wrap(~wymo) +
  coord_flip()

distinct(c1_pptjoin, decade, wymo, wyo, wymo_letter, sdltot, c1tot) %>%
  subset(wyo %in% 1982:2020) %>%
  group_by(wyo) %>%
  mutate(mean_diff = mean(sdltot - c1tot)) %>%
  ungroup() %>%
  ggplot(aes(wyo, sdltot - c1tot, col = wyo, fill = wyo)) +
  geom_point(alpha = 0.8) +
  #geom_line(alpha = 0.8, lty = 2) +
  geom_smooth(method = "gam") +
  #geom_line(aes(wyo, mean_diff), col = "red", lwd = 1.5, alpha = 0.8) +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey50") +
  geom_vline(aes(xintercept = 1994), lty = 3, col = "red") +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(1980,2020,4)) +
  theme_bw() +
  facet_wrap(~wymo)

d1c1_pptjoin <- dplyr::select(d1ppt, year, date, precip) %>%
  mutate(mo = month(date),
         wyo = ifelse(month(date) %in% c(10:12), year+1, year)) %>%
  rename(d1ppt = precip) %>%
  left_join(c1ppt[c("date", "precip")]) %>%
  rename(c1ppt = precip) %>%
  mutate(decade = ifelse(wyo < 1990, 1980, ifelse(wyo < 2000, 1990, ifelse(wyo < 2010, 2000, 2010)))) %>%
  data.frame() %>%
  # totals and cumulative
  group_by(wyo, mo) %>%
  mutate(d1running = cumsum(d1ppt),
         d1tot = sum(d1ppt),
         d1_pptdays = sum(d1ppt > 0),
         c1running = cumsum(c1ppt),
         c1tot = sum(c1ppt),
         c1_pptdays = sum(c1ppt > 0)) %>%
  ungroup() %>%
  mutate(d1_c1_diff = d1ppt - c1ppt,
         wymo = ifelse(mo %in% 10:12, mo-9, mo+3),
         doy = yday(date),
         wymo_letter = month(date, label = T, abbr = T),
         d1_scale = (d1ppt- mean(d1ppt))/sd(d1ppt),
         c1_scale = (c1ppt - mean(c1ppt))/sd(c1ppt),
         d1_scale_lagdiff = (d1_scale - lag(d1_scale)),
         c1_scale_lagdiff= (c1_scale - lag(c1_scale)))

distinct(d1c1_pptjoin, decade, wymo, wyo, wymo_letter, d1tot, c1tot) %>%
  subset(wyo %in% 1982:2020) %>%
  group_by(wyo) %>%
  mutate(mean_diff = mean(d1tot - c1tot)) %>%
  ungroup() %>%
  ggplot(aes(wyo, d1tot - c1tot, col = wyo, fill = wyo)) +
  geom_point(alpha = 0.8) +
  #geom_line(alpha = 0.8, lty = 2) +
  geom_smooth(method = "lm") +
  #geom_line(aes(wyo, mean_diff), col = "red", lwd = 1.5, alpha = 0.8) +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey50") +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(1980,2020,4)) +
  theme_bw() +
  facet_wrap(~wymo)

distinct(d1c1_pptjoin, decade, wymo, wyo, wymo_letter, d1tot, c1tot) %>%
  subset(wyo %in% 1982:2020) %>%
  mutate(d1c1_diff = d1tot - c1tot) %>%
  left_join(distinct(pptjoin, decade, wymo, wyo, wymo_letter, sdltot)) %>%
  mutate(d1sdl_diff = d1tot - sdltot,
         sdld1_diff = sdltot - d1tot,
         sdlc1_diff = sdltot - c1tot) %>%
  subset(wymo < 9) %>%
  ggplot() +
  #geom_line(alpha = 0.5) +
  geom_smooth(aes(wyo, d1c1_diff), method = "gam", lty = 2, alpha = 0.5) +
  geom_smooth(aes(wyo, sdld1_diff), method = "gam", col = "red", fill = "pink", lty = 1, alpha = 0.8) +
  geom_smooth(aes(wyo, sdlc1_diff), method = "gam", col = "purple", fill = "orchid", lty = 1, alpha = 0.8) +
  geom_point(aes(wyo, d1c1_diff, col = wyo), alpha = 0.8) +
  geom_point(aes(wyo, sdld1_diff, col = wyo), pch = 8, alpha = 0.8) +
  geom_point(aes(wyo, sdlc1_diff, col = wyo), pch = 4, alpha = 0.8) +
  #geom_line(aes(wyo, mean_diff), col = "red", lwd = 1.5, alpha = 0.8) +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey50") +
  #geom_vline(aes(xintercept = 1995), lty = 3, col = "red") +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(1980,2020,4)) +
  theme_bw() +
  facet_wrap(~wymo, scales = "free_y")

# look at rollmean
distinct(d1c1_pptjoin, decade, wymo, wyo, wymo_letter, d1tot, c1tot) %>%
  subset(wyo %in% 1982:2020) %>%
  mutate(d1c1_diff = d1tot - c1tot) %>%
  left_join(distinct(pptjoin, decade, wymo, wyo, wymo_letter, sdltot)) %>%
  mutate(d1sdl_diff = d1tot - sdltot,
         sdld1_diff = sdltot - d1tot,
         sdlc1_diff = sdltot - c1tot) %>%
  subset(wymo < 9) %>%
  ggplot() +
  #geom_line(alpha = 0.5) +
  geom_smooth(aes(wyo, d1c1_diff), method = "gam", lty = 2, alpha = 0.5) +
  geom_smooth(aes(wyo, sdld1_diff), method = "gam", col = "red", fill = "pink", lty = 1, alpha = 0.8) +
  geom_smooth(aes(wyo, sdlc1_diff), method = "gam", col = "purple", fill = "orchid", lty = 1, alpha = 0.8) +
  geom_point(aes(wyo, d1c1_diff, col = wyo), alpha = 0.8) +
  geom_point(aes(wyo, sdld1_diff, col = wyo), pch = 8, alpha = 0.8) +
  geom_point(aes(wyo, sdlc1_diff, col = wyo), pch = 4, alpha = 0.8) +
  #geom_line(aes(wyo, mean_diff), col = "red", lwd = 1.5, alpha = 0.8) +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey50") +
  #geom_vline(aes(xintercept = 1995), lty = 3, col = "red") +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(1980,2020,4)) +
  theme_bw() +
  facet_wrap(~wymo, scales = "free_y")

subset(d1c1_pptjoin, year == 1989, select = c(date, year, d1_scale, c1_scale)) %>%
  gather(met,val, d1_scale:ncol(.)) %>%
  ggplot() +
  geom_line(aes(date, val, col = met)) +
  #coord_flip() +
  theme_bw() #+
  facet_wrap(~met, nrow = 2)

alljoin <- distinct(d1c1_pptjoin, decade, wymo, wyo, wymo_letter, d1tot, c1tot) %>%
  subset(wyo %in% 1982:2021) %>%
  mutate(d1c1_diff = d1tot - c1tot) %>%
  left_join(distinct(pptjoin, decade, wymo, wyo, wymo_letter, sdltot)) %>%
  mutate(d1sdl_diff = d1tot - sdltot,
         sdld1_diff = sdltot - d1tot,
         sdlc1_diff = sdltot - c1tot,
         d1scale = (d1tot - mean(d1tot))/sd(d1tot),
         c1scale = (c1tot - mean(c1tot))/sd(c1tot),
         sdlscale = (sdltot - mean(sdltot))/sd(sdltot),
         d1scale_delta = d1scale - lag(d1scale),
         c1scale_delta = c1scale - lag(c1scale),
         sdlscale_delta = sdlscale - lag(sdlscale)) %>%
  data.frame()


subset(alljoin, select = c(wyo, wymo, wymo_letter, d1scale_delta, c1scale_delta, sdlscale_delta)) %>%
  gather(met, val, d1scale_delta:ncol(.)) %>%
  group_by(met) %>%
  mutate(x_order = 1:length(wyo)) %>%
  ungroup() %>%
  subset(wyo < 2010) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey50", lty = 2) +
  geom_line(aes(wymo, val, col = met), lwd = 1, alpha = 0.75) +
  scale_color_manual(values = c("forestgreen", "skyblue", "orchid")) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  facet_wrap(~wyo, nrow = 3, scales = "free")

subset(alljoin, select = c(wyo, wymo, wymo_letter, d1scale, c1scale, sdlscale)) %>%
  gather(met, val, d1scale:ncol(.)) %>%
  group_by(met) %>%
  mutate(x_order = 1:length(wyo)) %>%
  ungroup() %>%
  #subset(wyo < 2015) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), col = "grey50", lty = 2) +
  geom_line(aes(wymo, val, col = met), lwd = 1, alpha = 0.75) +
  scale_color_manual(values = c("forestgreen", "skyblue", "orchid")) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  facet_wrap(~wyo, nrow = 3, scales = "free")

subset(alljoin, select = c(wyo, wymo, wymo_letter, d1tot, c1tot, sdltot)) %>%
  gather(met, val, d1tot:ncol(.)) %>%
  group_by(met) %>%
  mutate(x_order = 1:length(wyo)) %>%
  ungroup() %>%
  #subset(wyo < 2015) %>%
  ggplot() +
  geom_hline(aes(yintercept = 300), col = "red", lty = 3) +
  geom_vline(aes(xintercept = 4), col = "grey50", lty = 2) +
  geom_vline(aes(xintercept = 8), col = "grey50", lty = 2) +
  geom_line(aes(wymo, val, col = met), lwd = 1, alpha = 0.75) +
  scale_color_manual(values = c("forestgreen", "skyblue", "orchid")) +
  scale_x_continuous(breaks = seq(2,12,2)) +
  theme_bw() +
  #theme(axis.text.x = element_blank()) +
  facet_wrap(~wyo, nrow = 3)

subset(alljoin, select = c(wyo, wymo, wymo_letter, d1c1_diff, sdlc1_diff, sdld1_diff)) %>%
  gather(met, val, d1c1_diff:ncol(.)) %>%
  group_by(met) %>%
  mutate(x_order = 1:length(wyo)) %>%
  ungroup() %>%
  #subset(wyo < 2015) %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), col = "red", lty = 3) +
  geom_vline(aes(xintercept = 4), col = "grey50", lty = 2) +
  geom_vline(aes(xintercept = 8), col = "grey50", lty = 2) +
  geom_line(aes(wymo, val, col = met), lwd = 1, alpha = 0.75) +
  scale_color_manual(values = c("skyblue", "forestgreen", "orchid")) +
  scale_x_continuous(breaks = seq(2,12,2)) +
  #theme_minimal() +
  #theme(axis.text.x = element_blank()) +
  facet_wrap(~wyo, nrow = 3)

# subset(alljoin, select = c(wyo, wymo, wymo_letter, d1scale, c1scale, sdlscale)) %>%
#   gather(met, val, d1scale:ncol(.)) %>%
subset(alljoin, select = c(wyo, wymo, wymo_letter, d1c1_diff, sdlc1_diff, sdld1_diff)) %>%
  gather(met, val, d1c1_diff:ncol(.)) %>%
  group_by(met) %>%
  mutate(x_order = 1:length(wyo),
         plotdate = as.Date(paste(wyo, wymo, 1, sep = "-"), format = "%Y-%m-%d"),
         periods = ifelse(wyo %in% 1981:1987, 1981, ifelse(wyo %in% 1988:1994, 1988, 1994))) %>%
  ungroup() %>%
  #subset(wyo < 2015) %>%
  ggplot(aes(plotdate, zoo::rollmean(val, 12, fill = NA), col = met)) +
  geom_hline(aes(yintercept = 0), col = "grey30", lty = 2) +
  # geom_vline(aes(xintercept = 4), col = "grey50", lty = 2) +
  # geom_vline(aes(xintercept = 8), col = "grey50", lty = 2) +
  geom_line(alpha = 0.75) +
  geom_smooth(aes(lty =factor(periods)), method = "lm") +
  #geom_smooth(aes(fill = met), method = "gam") +
  #geom_line(aes(x_order, val, col = met), lty = 2, alpha = 0.5) +
  scale_color_manual(values = c("blue", "forestgreen", "orchid")) +
  scale_fill_manual(values = c("blue", "forestgreen", "orchid")) +
  #scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
  theme_bw() +
  #theme(axis.text.x = element_blank()) +
  facet_wrap(~met, nrow = 3)

# was there undercatch 1981-1987 if no wind shield?
# and then wind shield or something happened around 1988
# and then system in place starting water year 1994? (or something between 1994-1996)

subset(alljoin, select = c(wyo, wymo, wymo_letter, d1scale, c1scale, sdlscale)) %>%
  gather(met, val, d1scale:ncol(.)) %>%
# subset(alljoin, select = c(wyo, wymo, wymo_letter, d1c1_diff, sdlc1_diff, sdld1_diff)) %>%
#   gather(met, val, d1c1_diff:ncol(.)) %>%
  group_by(met) %>%
  mutate(x_order = 1:length(wyo),
         plotdate = as.Date(paste(wyo, wymo, 1, sep = "-"), format = "%Y-%m-%d"),
         periods = ifelse(wyo %in% 1981:1987, 1981, ifelse(wyo %in% 1988:1994, 1988, 1994))) %>%
  ungroup() %>%
  #subset(wyo < 2015) %>%
  ggplot(aes(plotdate, zoo::rollapply(val, FUN= sd, width = 4, fill = NA), col = met)) +
  geom_hline(aes(yintercept = 0), col = "grey30", lty = 2) +
  # geom_vline(aes(xintercept = 4), col = "grey50", lty = 2) +
  # geom_vline(aes(xintercept = 8), col = "grey50", lty = 2) +
  geom_line(alpha = 0.75) +
  geom_smooth(aes(lty =factor(periods)), method = "lm") +
  #geom_smooth(aes(fill = met), method = "gam") +
  #geom_line(aes(x_order, val, col = met), lty = 2, alpha = 0.5) +
  scale_color_manual(values = c("blue", "forestgreen", "orchid")) +
  scale_fill_manual(values = c("blue", "forestgreen", "orchid")) +
  #scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_x_date(date_breaks = "4 year", date_labels = "%Y") +
  theme_bw() +
  #theme(axis.text.x = element_blank()) +
  facet_wrap(~met, nrow = 3)

sdlwind %>% 
  gather(met, val, airtemp_max:ncol(.)) %>%
  left_join(sdlppt[c("date", "precip")])%>%
  subset(grepl("airtemp_m|^ws", met)) %>%
  ggplot(aes(date, val)) +
  geom_line(col = "grey50", alpha = 0.5) +
  geom_point(aes(col = precip, size = precip), alpha = 0.5) +
  scale_colour_viridis_c(direction = -1) +
  facet_wrap(~met, scales = "free", ncol = 4) +
  geom_line(aes(date, zoo::rollmean(val, 30, fill = NA))) +
  theme_bw() +
  coord_flip()


# -- plot D1 supplemental figs for JHux ----
# winter precip and GDD

# read in prelim dats (pay attn to any high values for d1 temp since that's not flagged)
d1temp <- read.csv("../nwt8-renewal/c1_d1_sdl_clim/homogenize_climdat/data/d1_temp_1952-2020_draft.csv")

d1_winterppt <- subset(d1ppt, !month(date) %in% 6:9, select = c(year, date, precip)) %>% # sub out non-winter months
  # make water year first (Oct 1 - Sep 30)
  mutate(wy = ifelse(month(date) > 9, year+1, year)) %>%
  # plot only complete winters (all months present)
  group_by(wy) %>%
  summarise(winterppt = sum(precip),
         nobs = length(precip)) %>%
  ungroup() %>%
  # should either be 243 or 244 days (depending on leap year)
  subset(nobs >= 243, select = -nobs)

# calculate GDD
d1_GDD <- subset(d1temp, select = c(year, date, mean_temp)) %>% # subset to any day meanT > 0C
  # make eco_year
  mutate(ecoyr = ifelse(month(date) >=9, year +1, year),
         # if curious about days per mo
         mon = month(date)) %>%
  # discard any ecoyr that isn't complete
  group_by(ecoyr) %>%
  filter(length(unique(mon ))==12) %>%
  # now subset to mean_temp > 0
  filter(mean_temp > 0) %>%
  # calculate GDD stats
  mutate(GDD = sum(mean_temp),
         GDDnobs = length(mean_temp)) %>%
  group_by(ecoyr, GDD, GDDnobs, mon) %>%
  summarise(monnobs = length(mean_temp)) %>%
  ungroup()
  

# make stacked figure like jared's for supplement
left_join(d1_winterppt, distinct(d1_GDD, ecoyr, GDD), by = c("wy" = "ecoyr")) %>%
  gather(met, val, winterppt, GDD) %>%
  ggplot(aes(wy, val)) +
  geom_line() +
  geom_point(col = "orange") +
  facet_wrap(~met, nrow = 2)

# make individual plots to follow Jared's styling
d1winterppt_fig <- ggplot(d1_winterppt, aes(wy, winterppt)) +
  geom_line() +
  geom_point(size = 1) +
  geom_point(data = subset(d1_winterppt, wy %in% c(2008, 2010:2020)), size = 1.75, col = "orange") +
  labs(y = "Winter Precipitation (mm)", x = NULL) +
  scale_x_continuous(breaks = seq(1950, 2020, 10), expand = c(0.025, 0.025)) +
  scale_y_continuous(breaks = seq(500, 1300, 200), expand = c(0.025, 0.025)) +
  theme_bw() +
  theme(axis.title.y = element_text(face = "bold", size = 11))

d1_GDD_simple <- distinct(d1_GDD, ecoyr, GDD)
d1GDD_fig <- ggplot(d1_GDD_simple, aes(ecoyr, GDD)) +
  geom_line() +
  geom_point(size = 1) +
  geom_point(data = subset(d1_GDD_simple, ecoyr %in% c(2008, 2010:2020)), size = 1.75, col = "orange") +
  labs(y = "Growing Degree Days (°C)", x = NULL) +
  scale_x_continuous(breaks = seq(1950, 2020, 10), expand = c(0.025, 0.025)) +
  scale_y_continuous(breaks = seq(500,1300, 200), expand = c(0.025, 0.025)) +
  theme_bw() +
  theme(axis.title.y = element_text(face = "bold", size = 11))

d1trends_fig <- plot_grid(d1winterppt_fig, d1GDD_fig, align = c("vh"), nrow = 2, labels = c("A)", "B)"), 
          label_size = 12, label_x = 0.04)
ggsave("/Users/scarlet/Documents/nwt_lter/jared_snow_sdl_traits/D1_winterPPT_GDD_trends_1952_2020.pdf", plot = d1trends_fig, dpi = "print",
        height = 4, width = 6, units = "in", scale = 1.1)


  
#dblcheck sdl to be sure matching what Jared is plotting

sdl_winterppt <- subset(sdl_ppt, !month(date) %in% 6:9, select = c(year, date, precip_winteradj)) %>% # sub out non-winter months
  rename(year = year, precip = precip_winteradj) %>%
  # make water year first (Oct 1 - Sep 30)
  mutate(wy = ifelse(month(date) > 9, year+1, year)) %>%
  # plot only complete winters (all months present)
  group_by(wy) %>%
  summarise(winterppt = sum(precip),
            nobs = length(precip)) %>%
  ungroup() %>%
  # should either be 243 or 244 days (depending on leap year)
  subset(nobs >= 243, select = -nobs)


sdl_GDD <- subset(sdl_temp, select = c(yr, date, airtemp_avg_homogenized)) %>% # subset to any day meanT > 0C
  rename(mean_temp = airtemp_avg_homogenized, year = yr) %>%
  # make eco_year
  mutate(ecoyr = ifelse(month(date) >=9, year +1, year),
         # if curious about days per mo
         mon = month(date)) %>%
  # discard any ecoyr that isn't complete
  group_by(ecoyr) %>%
  filter(length(unique(mon ))==12) %>%
  # now subset to mean_temp > 0
  filter(mean_temp > 0) %>%
  # calculate GDD stats
  summarise(GDD = sum(mean_temp),
         GDDnobs = length(mean_temp)) %>%
  ungroup()


sdlwinterppt_fig <- ggplot(subset(sdl_winterppt, wy > 2007), aes(wy, winterppt)) +
  geom_line() +
  geom_point(size = 1) +
  geom_point(data = subset(sdl_winterppt, wy %in% c(2008, 2010:2020)), size = 1.75, col = "orange") +
  labs(y = "Winter Precipitation (mm)", x = NULL) +
  scale_x_continuous(breaks = seq(2008, 2020, 2), expand = c(0.025, 0.025)) +
  scale_y_continuous(breaks = seq(600, 1200, 200), expand = c(0.025, 0.025)) +
  theme_bw() +
  theme(axis.title.y = element_text(face = "bold", size = 11))

sdlGDD_fig <- ggplot(subset(sdl_GDD, ecoyr >2007), aes(ecoyr, GDD)) +
  geom_line() +
  geom_point(size = 1) +
  geom_point(data = subset(subset(sdl_GDD, ecoyr >2007), ecoyr %in% c(2008, 2010:2020)), size = 1.75, col = "orange") +
  labs(y = "Growing Degree Days (°C)", x = NULL) +
  scale_x_continuous(breaks = seq(2008, 2020, 2), expand = c(0.025, 0.025)) +
  scale_y_continuous(breaks = seq(800,1200, 100), expand = c(0.025, 0.025)) +
  theme_bw() +
  theme(axis.title.y = element_text(face = "bold", size = 11))


sdltrends_fig <- plot_grid(sdlwinterppt_fig, sdlGDD_fig, align = c("vh"), nrow = 2, labels = c("A)", "B)"), 
                          label_size = 12, label_x = 0.04)
ggsave("/Users/scarlet/Documents/nwt_lter/jared_snow_sdl_traits/SDL_winterPPT_GDD_trends_2008_2021.pdf", 
       plot = sdltrends_fig, dpi = "print",
       height = 4, width = 6, units = "in", scale = 1.1)

# from curiosity, look at D1 GDD vs. SDL logger GDD.. looks like from plotting for Jared D1 is greater than SDL sometimes..
ggplot(sdl_GDD, aes(ecoyr, GDD)) +
  geom_vline(aes(xintercept = 2010), lty = 2) +
  geom_line(col = "orchid2", alpha = 0.75) +
  geom_line(data = subset(d1_GDD_simple, ecoyr %in% sdl_GDD$ecoyr), col = "blue2", alpha = 0.6) +
  geom_point(col = "orchid4") +
  geom_point(data = subset(d1_GDD_simple, ecoyr %in% sdl_GDD$ecoyr), col = "blue4") +
  scale_x_continuous(breaks = seq(1986, 2020, 4), expand = c(0.025, 0.025)) +
  labs(x = "Ecological Year (Sep 1 - Aug 31)", 
       y = "Growing degree days (°C)",
       subtitle ="D1 (chart) = blue, SDL (logger) = orchid, TK  ") +
  theme_minimal()

ggsave("nwt_climate/SDLGDD_v_D1GDD_1987_2021.pdf",
       height = 3, width = 5, units = "in", scale = 1.1)

