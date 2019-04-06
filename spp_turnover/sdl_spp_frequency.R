# nwt lter: saddle grid spp frequency trends 
# borrowed from bosmp-spp-frequency analysis (belongs to ctw)
# caitlin.t.white@colorado.edu

# ** NOTE: does not include nodal plots

# -- SETUP -----
options(stringsAsFactors = FALSE)
library(tidyverse)
library(readxl)

NA_vals <- c("NA", "", ".", " ")

# read in datasets
# sdl grid spp comp from EDI
veg_datapath <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/93/1/03590a13459ffb31dc411ef6634ffaf2"
veg <- read.csv(veg_datapath, na.strings = NA_vals)

comm_datapath <- ("../../Documents/nwt_lter/unpub_data/NWT_Saddle_ComType.xlsx")
comm_clusters <- read_excel(comm_datapath, sheet = "data")


# -- DATA PREP -----
str(veg)
summary(veg)
# look at unique vals to be sure all looks okay
lapply(veg, unique)

# question: I thought there were community types for each grid point?
# nothing here so would group grid plots by any factor (update: Jane sent me comm types)
str(commtype)
# look at unique vals to be sure all looks okay
lapply(commtype, unique)

# create veg_only df
veg_only <- veg[!grepl("^2", veg$USDA_code) & # remove all unknowns and non-veg
                  !(grepl("unk", veg$USDA_name, ignore.case = T)) & #remove more unknowns
                  grepl(" ", veg$USDA_name),] # only keep things ID'd to species level (exlude genus-only)
                     
# confirm only species-ID'd names in spp list
sort(unique(veg_only$USDA_code)) #yes
sort(unique(veg_only$USDA_name)) #yes

# do plots in commtype == plots in veg_only? or are plots in the spp comp data in the commtype plots?
summary(sort(unique(veg_only$plot)) %in% sort(unique(comm_clusters$plot))) # all in, there are more plots in the commtype dataset (bc rock plots)

#select community type used here so can run all code generically below
commtype <- comm_clusters[c("plot", "class_3")] # <- set here
names(commtype)[2] <- "type"

# must add commtype to veg_only df
veg_only <- left_join(veg_only, commtype)

# -- SPP GAINS, SITE-PLOT LEVEL -----
# for-loop tracks new spp added on site-plot level data

# build running tally of new spp 
# initiate empty master data frame
spp_gains_plot_counts_allT <- data.frame()
spp_gains_plot_names_df <- data.frame()
for(p in unique(veg_only$plot)){
  print(paste("looking at", p, "species gains..."))
  
  # create data frame for individual transect
  plot_df <- veg_only[veg_only$plot == p,] %>%
    mutate(event = as.numeric(as.factor(year)))
  
  # create temporary df for storing new spp counts
  temp_df <- data.frame(plot = p, 
                        event = unique(plot_df$event), 
                        year = unique(plot_df$year), 
                        type = unique(plot_df$type))
  temp_df$new_spp <- NA
  spp_list_running <- unique(plot_df$USDA_code[plot_df$event ==1])
  
  for(i in 2:max(plot_df$event)){
    # grab new native spp gained
    event_spp <- unique(plot_df$USDA_code[plot_df$event == i])
    new_spp <- event_spp[!event_spp %in% spp_list_running]
    
    # store spp gained
    if(length(new_spp) > 0){
      temp_spp_gains_df <- data.frame(spp_gains = new_spp)
    } else{
      temp_spp_gains_df <- data.frame(spp_gains = NA)
    }
    
    temp_spp_gains_df$event <- i
    temp_spp_gains_df$year <- unique(plot_df$year[plot_df$event == i])
    temp_spp_gains_df$plot <- p
    temp_spp_gains_df$type <- unique(plot_df$type)
    print(paste("plot", p, "event", i, "spp gained:", stringr::str_flatten(new_spp, collapse = ", ")))
    
    
    # add new_spp to running spp_list
    spp_list_running <- c(spp_list_running, new_spp)
    
    # add info to temp_df
    temp_df$new_spp[temp_df$event == i] <- length(new_spp)
    # add info to lost_spp_meta_df
    spp_gains_plot_names_df <- rbind(spp_gains_plot_names_df, temp_spp_gains_df)
  }
  # add transect new spp to master new spp counts data frame
  spp_gains_plot_counts_allT <- rbind(spp_gains_plot_counts_allT, na.omit(temp_df)) #na.omit removes event 1 since spp gained in that year will always be NA
}


# check to be sure no codes duplicated at transect level
with(spp_gains_plot_names_df, lapply(split(spp_gains, plot), function(x) x[duplicated(x)])) #only NAs duplicated, which is fine (some yrs didn't lose any spp)
# only NA! good

# add in spp names
spp_gains_plot_names_df <- left_join(spp_gains_plot_names_df, distinct(veg_only[c("USDA_code", "USDA_name")]), by = c("spp_gains" = "USDA_code"))

spp_gains_fig <- ggplot(spp_gains_plot_counts_allT, aes(year, new_spp)) +
  geom_jitter(alpha = 0.6, width = 0.25, height = 0) +
  geom_smooth(se = F) +
  labs(title = "NWT LTER SADDLE GRID: number of new species added to plot species list with each sampling event",
       subtitle = "Points within year jittered slightly; blue = plot-level trend, purple = meta-community species gains and trend ",
       y = "Number of new species gained",
       x = "Monitoring year") +
  #scale_x_continuous(breaks = seq(2002, 2018, by = 2)) +
  #scale_y_continuous(breaks = seq(0,36,4)) + #expand = c(0.02,0.01), limits = c(0,NA), 
  theme_light() +
  facet_grid(type~.)
spp_gains_fig


# -- SPP GAINS IN META COMMUNITY -----
# initiate empty master data frame
spp_gains_meta_counts_allT <- data.frame()
spp_gains_meta_names_df <- data.frame()
for(t in unique(veg_only$type)){
  print(paste("looking at", t, "species gains in meta community..."))
  # create data frame for meta community
  meta_df <- veg_only[veg_only$type == t,] %>%
    mutate(event = as.numeric(as.factor(year)))
  
  # create temporary df for storing new spp counts
  temp_df <- data.frame(event = unique(meta_df$event), 
                        year = unique(meta_df$year),
                        type = t)
  temp_df$new_spp <- NA
  spp_list_running <- sort(unique(meta_df$USDA_code[meta_df$event ==1 & meta_df$type == t]))
  
  for(i in 2:max(meta_df$event)){
    # grab new native spp gained
    event_spp <- sort(unique(meta_df$USDA_code[meta_df$event == i & meta_df$type == t]))
    new_spp <- event_spp[!event_spp %in% spp_list_running]
    
    # store spp gained
    if(length(new_spp) > 0){
      temp_spp_gains_df <- data.frame(spp_gains = new_spp)
    } else{
      temp_spp_gains_df <- data.frame(spp_gains = NA)
    }
    temp_spp_gains_df$type <- t
    temp_spp_gains_df$year <- unique(meta_df$year[meta_df$event == i])
    temp_spp_gains_df$event <- i
    print(paste("Event", i, t, "spp gained in system:", stringr::str_flatten(new_spp, collapse = ", ")))
    
    
    # add new_spp to running spp_list
    spp_list_running <- c(spp_list_running, new_spp)
    
    # add info to temp_df
    temp_df$new_spp[temp_df$event == i & temp_df$type == t] <- length(new_spp)
    # add info to lost_spp_meta_df
    spp_gains_meta_names_df <- rbind(spp_gains_meta_names_df, temp_spp_gains_df)
  }
  # add meta new spp to master new spp counts data frame
  spp_gains_meta_counts_allT <- rbind(spp_gains_meta_counts_allT, na.omit(temp_df))
}

# check to be sure no codes duplicated
with(spp_gains_meta_names_df, lapply(split(spp_gains, type), function(x) x[duplicated(x)])) #only NAs duplicated, which is fine (some yrs didn't lose any spp)
# only NAs, good

# add in site and treatment descriptive cols
spp_gains_meta_names_df <- left_join(spp_gains_meta_names_df, distinct(veg_only[c("USDA_code", "USDA_name")]), by = c("spp_gains" = "USDA_code"))


# add meta trend to transect spp gains fig
spp_gains_fig +
  scale_y_continuous(breaks = seq(0,8,2)) +
  scale_x_continuous(breaks = seq(1988,2018,4)) +
  geom_point(data = spp_gains_meta_counts_allT, aes(year, new_spp), col = "purple") +
  geom_smooth(data = spp_gains_meta_counts_allT, aes(year, new_spp), col = "purple", se =F) # fill = "orchid"


# -- SPP FREQ WITH CLEAN TRANSECTS ----
# how many spp only occur once in the record on a transect?
spp_freq.clean <- veg_dat[veg_dat$Lifeform != "Ground cover", c("Year", "Area", "plot", "spp")] %>%
  group_by(plot) %>%
  mutate(yrs_sampled = length(unique(Year))) %>%
  ungroup() %>%
  filter(!grepl("unk| sp", spp, ignore.case = T)) %>% #remove unknown spp
  distinct() %>%
  group_by(Area, plot, yrs_sampled, spp) %>%
  summarise(nobs = length(spp))
# what percent of spp, at the tranect level, only occur once in the whole record
nrow(spp_freq.clean[spp_freq.clean$nobs == 1,])/nrow(spp_freq.clean) #27.7% (1% down from uncorrected transect IDs)

# plot single-obs spp over time, by # yrs transect sampled (to compare to Ann's figure for GMAP)
spp_freq_pct.clean <- spp_freq.clean %>%
  mutate(N_once = ifelse(nobs == 1, 1, 0)) %>%
  group_by(Area, plot, yrs_sampled, N_once) %>%
  summarise(N_spp = length(spp)) %>%
  ungroup() %>%
  group_by(plot) %>%
  mutate(total_spp = sum(N_spp)) %>%
  ungroup() %>%
  mutate(rel_pct = (N_spp/total_spp)*100) %>%
  subset(N_once == 1)

ggplot(spp_freq_pct.clean, aes(yrs_sampled, rel_pct)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", lty = 2, col = "dodgerblue3") +
  annotate("text", x = 22, y = 28, label = "n.s.", size = 4, col = "dodgerblue3") +
  labs(title = "TGSNA: percent of species present only 1 year in entire record, transect-level",
       subtitle = "Monitoring years: 1991-2018; trendline = overall trend (n.s.); corrected plot in 2004",
       y = "Pct species present in only 1 year",
       x = "Year monitored") +
  scale_x_continuous(breaks = seq(20, 28, by = 1)) +
  theme_light() +
  theme(title = element_text(size = 10))



# -- SPP LOST OVER TIME -----
# what is the inverse? (spp lost over time)

# build running tally of spp lost
# initiate empty master data frame
spp_lost_plot_count_allT <- data.frame()
lost_spp_plot_names_df <- data.frame()
for(p in unique(veg_only$plot)){
  print(paste("looking at", p, "species lost..."))
  # create data frame for individual plot
  plot_df <- veg_only[veg_only$plot == p,] %>%
    mutate(event = as.numeric(as.factor(year)))
  
  # create temporary df for storing new spp counts
  temp_df <- data.frame(plot = p, 
                        event = unique(plot_df$event), 
                        year = unique(plot_df$year),
                        type = unique(plot_df$type))
  temp_df$lost_spp <- NA
  spplist_lag1 <- unique(plot_df$USDA_code[plot_df$event ==1])
  
  i <- 2
  while(i %in% 2:max(plot_df$event)){
    # grab full spp list for next year to end of sampling record
    fwd_spplist <- unique(plot_df$USDA_code[plot_df$event %in% (i+1):max(plot_df$event)])
    
    # grab native spp list for current sampling event
    event_spp <- unique(plot_df$USDA_code[plot_df$event == i])
    # compare spp that disappeared from year prior
    event_spp_loss <- spplist_lag1[!spplist_lag1 %in% event_spp]
    # compare spp lost from year prior to future species list
    event_spp_loss <- event_spp_loss[!event_spp_loss %in% fwd_spplist]
    print(paste("event", i, p, "spp lost: ", stringr::str_flatten(event_spp_loss, collapse = ", ")))
    
    # store spp lost
    if(length(event_spp_loss) > 0){
      temp_spp_loss_df <- data.frame(lost_spp = event_spp_loss)
    } else {
      temp_spp_loss_df <- data.frame(lost_spp = NA)
    }
    
    temp_spp_loss_df$year <- unique(plot_df$year[plot_df$event == i])
    temp_spp_loss_df$event <- i
    temp_spp_loss_df$plot <- p
    temp_spp_loss_df$type <- unique(plot_df$type)
    
    # update spp list to current year
    spplist_lag1 <- event_spp
    
    # add info to temp_df
    temp_df$lost_spp[temp_df$event == i] <- length(event_spp_loss)
    
    # add info to veg_lost_spp_plot_df
    lost_spp_plot_names_df <- rbind(lost_spp_plot_names_df, temp_spp_loss_df)
    i <- i+1
  }
  # add plot new spp to master new spp counts data frame
  spp_lost_plot_count_allT <- rbind(spp_lost_plot_count_allT, na.omit(temp_df))
}

# check to be sure no codes duplicated at plot level
with(lost_spp_plot_names_df, lapply(split(lost_spp, plot), function(x) x[duplicated(x)])) #only NAs duplicated, which is fine (some yrs didn't lose any spp)
# only NA! good

# add in species names
lost_spp_plot_names_df <- left_join(lost_spp_plot_names_df, distinct(veg_only[c("USDA_code", "USDA_name")]), by = c("lost_spp" = "USDA_code"))

# plot
veg_loss_fig <- ggplot(subset(spp_lost_plot_count_allT), aes(year, lost_spp)) +
  geom_jitter(alpha = 0.6, width = 0.25, height = 0) +
  geom_smooth(se = F) + #fill = "deepskyblue1"
  labs(title = "NWT LTER SADDLE GRID: number of species permanently lost, by plot, with each sampling event",
       subtitle = "Points within year jittered slightly; blue = plot-level trend, purple = meta community species loss and trend",
       y = "Number of species lost",
       x = "Monitoring year") +
  scale_x_continuous(breaks = seq(1988, 2018, by = 4)) +
  #scale_y_continuous(breaks = seq(0,36,4)) + #expand = c(0.02,0.01), limits = c(0,NA), 
  theme_light() +
  facet_grid(type~.)
veg_loss_fig


# -- SPP LOST FROM ENTIRE SYSTEM WITH TIME -----
# initiate empty master data frame
spp_lost_meta_counts_allT <- data.frame()
lost_spp_meta_names_df <- data.frame()
for(t in unique(veg_only$type)){
  print(paste("looking at", t, "species lost from meta community..."))
  # create data frame for meta community
  meta_df <- veg_only[veg_only$type == t,] %>%
    mutate(event = as.numeric(as.factor(year)))
  
  # create temporary df for storing new spp counts
  temp_df <- data.frame(event = unique(meta_df$event), 
                        year = unique(meta_df$year),
                        type = t)
  temp_df$lost_spp <- NA
  spplist_lag1 <- unique(meta_df$USDA_code[meta_df$event ==1 & meta_df$type == t])
  
  i <- 2
  while(i %in% 2:max(meta_df$event)){
    # grab full spp list for next year to end of sampling record
    fwd_spplist <- unique(meta_df$USDA_code[meta_df$event %in% (i+1):max(meta_df$event)])
    
    # grab native spp list for current sampling event
    event_spp <- unique(meta_df$USDA_code[meta_df$event == i & meta_df$type == t])
    # compare spp that disappeared from year prior
    event_spp_loss <- spplist_lag1[!spplist_lag1 %in% event_spp]
    # compare spp lost from year prior to future species list
    event_spp_loss <- event_spp_loss[!event_spp_loss %in% fwd_spplist]
    print(paste("Event", i, p, "spp lost: ", stringr::str_flatten(event_spp_loss, collapse = ", ")))
    
    # store native spp lost
    if(length(event_spp_loss) > 0){
      temp_spp_loss_df <- data.frame(lost_spp = event_spp_loss)
    } else{
      temp_spp_loss_df <- data.frame(lost_spp = NA)
    }
    temp_spp_loss_df$type <- t
    temp_spp_loss_df$year <- unique(meta_df$year[meta_df$event == i])
    temp_spp_loss_df$event <- i
    
    # update spp list to current year
    spplist_lag1 <- event_spp
    
    # add info to temp_df
    temp_df$lost_spp[temp_df$event == i] <- length(event_spp_loss)
    
    # add info to veg_lost_spp_meta_df
    lost_spp_meta_names_df <- rbind(lost_spp_meta_names_df, temp_spp_loss_df)
    i <- i+1
  }
  # add meta new spp to master new spp counts data frame
  spp_lost_meta_counts_allT <- rbind(spp_lost_meta_counts_allT, na.omit(temp_df)) # removes event 1 (no spp lost in first yr of sampling)
}

# check to be sure no codes duplicated
with(lost_spp_meta_names_df, lapply(split(lost_spp,type), function(x) x[duplicated(x)])) #only NAs duplicated, which is fine (some yrs didn't lose any spp)

# add species names
lost_spp_meta_names_df <- left_join(lost_spp_meta_names_df, distinct(veg_only[c("USDA_code", "USDA_name")]), by = c("lost_spp" = "USDA_code"))

# add metacom trend to nativity loss fig
veg_loss_fig +
  scale_y_continuous(breaks=seq(0,8,2)) +
  geom_point(data = subset(spp_lost_meta_counts_allT), aes(year, lost_spp), col = "purple") +
  geom_smooth(data = subset(spp_lost_meta_counts_allT), aes(year, lost_spp), col = "purple", se = F) # fill = "orchid"


# write out tables for jane
write.csv(lost_spp_meta_names_df, "sdl_meta_lost_spp.csv", row.names = F)
write.csv(lost_spp_plot_names_df, "sdl_plot_lost_spp.csv", row.names = F)
write.csv(spp_gains_meta_names_df, "sdl_meta_spp_gains.csv", row.names = F)
write.csv(spp_gains_plot_names_df, "sdl_plot_spp_gains.csv", row.names = F)

