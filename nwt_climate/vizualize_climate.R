# climate data viz

# -- SETUP -----
library(tidyverse)

# plot temp as heatmap.. may need to bin to have it make sense

# start it at 1987 for complete years only
ggplot(subset(adjusted_dat_out, yr > 1986), aes(doy, airtemp_avg_homogenized)) +
  geom_bin_2d() +
  labs(x = "Day of year", y = "Saddle Tmax (°C)") +
  scale_fill_viridis_c(option = "B") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal()

ggplot(subset(adjusted_dat_out, yr > 1986), aes(doy, c(NA,diff(airtemp_avg_homogenized)))) +
  geom_bin_2d() +
  labs(x = "Day of year", y = "Saddle Tmean day-to-day change (°C)") +
  scale_fill_viridis_c(option = "D") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal()

View(adjusted_dat_out[which(diff(adjusted_dat_out$airtemp_avg_homogenized) < -15),])
