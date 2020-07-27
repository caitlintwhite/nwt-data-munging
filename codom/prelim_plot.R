dat <- read.csv("codom/codom_FI_4-8yr.csv", stringsAsFactors = F)
str(dat)
library(tidyverse)
theme_set(theme_bw())
ggplot(subset(dat, !grepl("B", plotid)), aes(increment, FI, group = plotid)) +
  geom_line(aes(col = local_site)) +
  scale_color_viridis_d() +
  facet_grid(plot~winsize, scale = "free_x")
  
ggplot(subset(dat, !grepl("B", plotid)), aes(timestep, FI, group = plotid)) +
  geom_line(aes(col = local_site)) +
  scale_color_viridis_d() +
  facet_grid(plot~winsize, scale = "free_x")
