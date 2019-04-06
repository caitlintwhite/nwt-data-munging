#sdl spp migration/extinctions with traits explore


# -- SETUP -----
options(stringsAsFactors = FALSE)
library(tidyverse)
library(readxl)
library(rgdal)
library(cowplot)

NA_vals <- c("NA", "", ".", " ", "NaN")

# read in datasets
# sdl grid spp comp from EDI
veg_datapath <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/93/1/03590a13459ffb31dc411ef6634ffaf2"
veg <- read.csv(veg_datapath, na.strings = NA_vals)

# sdl community types (from Jane) 
comm_datapath <- ("../../Documents/nwt_lter/unpub_data/NWT_Saddle_ComType.xlsx")
comm_clusters <- read_excel(comm_datapath, sheet = "data")

# marko + soren trait dataset (from EDI)
trait_path <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/500/1/7329b45be670902cdc35432e05200d73"
trait <- read.csv(trait_path, na.strings = NA_vals)

# sdl spp gain/loss table over time
sdl_plot_gains <- read.csv("sdl_plot_spp_gains.csv")
sdl_plot_loss <- read.csv("sdl_plot_lost_spp.csv")
sdl_meta_gains <- read.csv("sdl_meta_spp_gains.csv")
sdl_meta_loss <- read.csv("sdl_meta_lost_spp.csv")

# sdl grid point shp (for grid-point coordinates)
sdlgrd_pts <- readOGR("/Users/serahsierra/Google\ Drive/nwt_geospatial_data/geospatial_data_prep/data/saddle_grdpts/clean/saddle_grdpts/saddle_grdpts.shp")
grdpts <- data.frame(LTER_POINT = sdlgrd_pts@data$LTER_POINT, sdlgrd_pts@coords) %>%
  mutate_all(as.numeric)

# how different are traits by plot type in the saddle grid?
# boxplot(trait$Percent_N[trait$Species == "ARTSCO" & trait$Exp == "SAD"] ~ trait$TRT[trait$Exp == "SAD" & trait$Species == "ARTSCO"])
# boxplot(trait$SLA[trait$Species == "ARTSCO" & trait$Exp == "SAD"] ~ trait$TRT[trait$Exp == "SAD" & trait$Species == "ARTSCO"])
# boxplot(trait$LDMC[trait$Species == "ARTSCO" & trait$Exp == "SAD"] ~ trait$TRT[trait$Exp == "SAD" & trait$Species == "ARTSCO"])
# boxplot(trait$CN_ratio[trait$Species == "ARTSCO" & trait$Exp == "SAD"] ~ trait$TRT[trait$Exp == "SAD" & trait$Species == "ARTSCO"])
# boxplot(trait$SLA[trait$Species == "ARTSCO" & trait$Exp == "SAD"] ~ trait$TRT[trait$Exp == "SAD" & trait$Species == "ARTSCO"])

# bring in extended summer scores
sumallPC <- read.csv("/Users/serahsierra/Documents/nwt_lter/GRA/nwt_gra_fall2018/climate_update/NWT_sumallPCclimate_19822017.csv")
  
# -- EXPLORE DATA VISUALLY -----
pmargins <- c(0,0.1,0,0.1)
comm_levels <- c("rock", "FF", "DM", "SF", "ST", "SB", "MM", "WM") 
grdpts2 <- left_join(grdpts, comm_clusters[c("plot", "class_3")], by = c("LTER_POINT" = "plot")) %>%
  mutate(class_factor = factor(class_3, levels = comm_levels))
levels(grdpts2$class_factor)


# where was erisim in first yr of sampling
ersi_start <- veg %>%
  subset(USDA_code == "ERSI3" & year == min(year)) %>%
  dplyr::select(USDA_code, year, plot) %>%
  distinct() %>% 
  left_join(grdpts, by = c("plot" = "LTER_POINT"))

# what is lost in space over time?
# pair plot coordinates with data frame
sdl_plot_loss <- left_join(sdl_plot_loss, grdpts, by = c("plot" = "LTER_POINT")) %>%
  left_join(sumallPC, by = c("year" = "eco_year"))

sdl_plot_gains <- left_join(sdl_plot_gains, grdpts, by = c("plot" = "LTER_POINT")) %>%
  left_join(sumallPC, by = c("year" = "eco_year"))

loss_PC1 <- ggplot(grdpts2) +
  geom_point(aes(coords.x1, coords.x2, col = class_factor), size = 6, alpha = 0.4, pch = 15) +
  geom_point(data = subset(grdpts2, class_factor == "ST"), aes(coords.x1, coords.x2), col = "black", size = 6, pch = 0) + #col = "#74add1"
  geom_point(data = subset(grdpts2, class_factor == "DM"), aes(coords.x1, coords.x2), col = "black", size = 6, pch = 2) + #col = "#a50026"
  # geom_point(data = subset(sdl_plot_loss, grepl("simplex", USDA_name)), 
  #            aes(coords.x1, coords.x2,fill = sumallPC1), size = 3, pch= 21) +
  geom_point(data = subset(sdl_plot_loss, lost_spp == "ERSI3"), 
             aes(coords.x1, coords.x2,fill = sumallPC1), size = 3, pch= 21) +
  scale_color_brewer(name = "Community\ntype", palette = "YlGn", guide = FALSE) +
  scale_fill_distiller(name = "Ext summer", palette = "YlOrRd", direction = 1) +
  #scale_fill_distiller(name = "Year", palette = "BuPu", direction = -1) +
  labs(x = NULL,
       y = NULL,
       #title = "NWT LTER SADDLE GRID:\nErigeron simplex permanent disappearance, by year") + #extended summer score
       title = "'Permanent' loss, by extended summer score") +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #legend.position = "none",
        title = element_text(size = 9),
        plot.margin = margin(pmargins)) +
  coord_equal()


loss_yrly <- ggplot(grdpts2) +
  geom_point(aes(coords.x1, coords.x2, col = class_factor), size = 6, alpha = 0.4, pch = 15) +
  geom_point(data = subset(grdpts2, class_factor == "ST"), aes(coords.x1, coords.x2), col = "black", size = 6, pch = 0) + #col = "#74add1"
  geom_point(data = subset(grdpts2, class_factor == "DM"), aes(coords.x1, coords.x2), col = "black", size = 6, pch = 2) + #col = "#a50026"
  # geom_point(data = subset(sdl_plot_loss, grepl("simplex", USDA_name)), 
  #            aes(coords.x1, coords.x2,fill = sumallPC1), size = 3, pch= 21) +
  geom_point(data = subset(sdl_plot_loss, lost_spp == "ERSI3"), 
             aes(coords.x1, coords.x2,fill = year), size = 3, pch= 21) +
  scale_color_brewer(name = "Community\ntype", palette = "YlGn", guide = FALSE) +
  # scale_fill_distiller(name = "Ext summer", palette = "YlOrRd", direction = -1) +
  scale_fill_distiller(name = "Year", palette = "BuPu", direction = 1) +
  labs(x = NULL,
       y = NULL,
       #title = "NWT LTER SADDLE GRID:\nErigeron simplex initial recruitment, by year") + #extended summer score
       title = "'Permanent' loss, by year") +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        #legend.position = "none",
        title = element_text(size = 9),
        plot.margin = margin(pmargins)) +
  coord_equal()

gains_PC1 <- ggplot(grdpts2) +
  geom_point(aes(coords.x1, coords.x2, col = class_factor), size = 6, alpha = 0.4, pch = 15) +
  geom_point(data = subset(grdpts2, class_factor == "ST"), aes(coords.x1, coords.x2), col = "black", size = 6, pch = 0) + #col = "#74add1"
  geom_point(data = subset(grdpts2, class_factor == "SF"), aes(coords.x1, coords.x2), col = "black", size = 6, pch = 1) + #col = "#a50026"
  geom_point(data = subset(grdpts2, class_factor == "DM"), aes(coords.x1, coords.x2), col = "black", size = 6, pch = 2) + #col = "#a50026"
  geom_point(data = ersi_start, aes(coords.x1, coords.x2), col = "black") +
  geom_point(data = subset(sdl_plot_gains, spp_gains == "ERSI3"), 
              aes(coords.x1, coords.x2,fill = sumallPC1), size = 3, pch= 21) +
  # geom_point(data = subset(sdl_plot_gains, grepl("simplex", USDA_name)), 
  #            aes(coords.x1, coords.x2,fill = year), size = 3, pch= 21) +
  scale_color_brewer(name = "Community\ntype", palette = "YlGn") +
  scale_fill_distiller(name = "Ext summer", palette = "YlOrRd", direction = 1, guide = FALSE) +
  #scale_fill_distiller(name = "Year", palette = "BuPu", direction = -1) +
  labs(x = NULL,
       y = NULL,
       #title = "NWT LTER SADDLE GRID:\nErigeron simplex initial recruitment, by extended summer score") +
       title = "Intial recruitment, by extended summer score") +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "none",
        title = element_text(size = 9),
        plot.margin = margin(pmargins)) +
  coord_equal()

gains_yrly <- ggplot(grdpts2) +
  geom_point(aes(coords.x1, coords.x2, col = class_factor), size = 6, alpha = 0.4, pch = 15) +
  geom_point(data = subset(grdpts2, class_factor == "ST"), aes(coords.x1, coords.x2), col = "black", size = 6, pch = 0) + #col = "#74add1"
  geom_point(data = subset(grdpts2, class_factor == "SF"), aes(coords.x1, coords.x2), col = "black", size = 6, pch = 1) + #col = "#a50026"
  geom_point(data = subset(grdpts2, class_factor == "DM"), aes(coords.x1, coords.x2), col = "black", size = 6, pch = 2) + #col = "#a50026"
  geom_point(data = ersi_start, aes(coords.x1, coords.x2), col = "black") +
  #geom_point(data = subset(sdl_plot_gains, grepl("simplex", USDA_name)), 
  #           aes(coords.x1, coords.x2,fill = sumallPC1), size = 3, pch= 21) +
  geom_point(data = subset(sdl_plot_gains, spp_gains == "ERSI3"), 
             aes(coords.x1, coords.x2,fill = year), size = 3, pch= 21) +
  scale_color_brewer(name = "Community\ntype", palette = "YlGn") +
  #scale_fill_distiller(name = "Ext summer", palette = "YlOrRd", direction = -1) +
  scale_fill_distiller(name = "Year", palette = "BuPu", direction = 1, guide = FALSE) +
  labs(x = NULL,
       y = NULL,
       #title = "NWT LTER SADDLE GRID:\nErigeron simplex initial recruitment, by year") +
       title = "Intial recruitment, by year") +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "none",
        title = element_text(size = 9),
        plot.margin = margin(pmargins)) +
  coord_equal()

ersi_start_fig <- ggplot(grdpts2) +
  geom_point(aes(coords.x1, coords.x2, fill = class_factor), size = 6, alpha = 0.75, pch = 22) +
  #geom_point(aes(coords.x1, coords.x2, col = class_factor), size = 6, alpha = 0.75, pch = 15) +
  geom_point(data = ersi_start, aes(coords.x1, coords.x2), col = "black", size = 3) +
  scale_fill_brewer(name = "Community\ntype", palette = "YlGn") +
  #scale_color_brewer(palette = "YlGn", guide = FALSE) +
  labs(x = NULL,
       y = NULL,
       title = "NWT LTER SADDLE GRID:",
       subtitle = expression(paste(italic("Erigeron simplex"),
                          " presence at Saddle Grid, 1989 (yr1)"))) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) +
        #plot.margin = margin(c(0.5,0.1,0.1,0.1))) +
  coord_equal()


# plot erisim traits by comm type
subset(trait, grepl("simplex", Latin.name) & Exp == "SAD") %>%
  gather(trait_name, value, VegHeight:OHeight, CCAvg:CN_ratio) %>%
  filter(!is.na(value)) %>%
  mutate(trt_factor = factor(TRT, levels = c("SNOWBED", "MOIST", "WET"), labels = c("SB", "MM", "WM"))) %>%
  ggplot() +
  geom_boxplot(aes(trt_factor, value)) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~trait_name, scales = "free_y")


# put all erisim panels in cowplot
ersi_panel <- plot_grid(gains_yrly, loss_yrly, gains_PC1, loss_PC1, nrow = 2,
          rel_widths = c(.8,1,.8,1))
          #rel_heights = c(.75, .75, 1.25, .75),
ggsave(ersi_panel, width = 10, height = 6, filename = "ersi_gainloss.png")  
ggsave(ersi_start_fig, width = 10, height = 6, filename = "ersi_start.png")
