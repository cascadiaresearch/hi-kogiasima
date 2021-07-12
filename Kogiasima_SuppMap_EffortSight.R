## Kogia sima analyses 2020:
## Paneled sightings/effort map: Maui Nui, Oahu, Kauai/Niihau

## Author: Michaela A. Kratofil, Cascadia Research Collective
## Updated: 12 JUL 2021

###############################################################

## load packages
library(tidyverse)
library(lubridate)
library(sf)
library(rgdal)
library(ggplot2)
library(ggspatial)
library(ggpubr)

## load effort data
eff <- read.csv("Kogiasima analyses - 2020/Effort_thruJul2020_forMaps.csv", header = T) # all effort data 
str(eff) # review
unique(eff$Island)
eff$Date <- as.Date(eff$Date_, format = "%m/%d/%Y") # format date

# use corrected effort for 1/8/2016 (note: this is corrected in most recent 2021 version)
eff2016 <- read.csv("Effort and sightings/EffortGIS20160622.csv")
eff_sub <- filter(eff2016, Date_ == "1/8/2016")
eff_sub <- eff_sub[,c(1:17)]

## maui nui
mn_eff <- filter(eff, Island %in% c("Maui","Lanai"))
mn_eff <- filter(mn_eff, Year_ %in% c(2002, 2003, 2012, 2017, 2018, 2019))

## ohau
oh_eff <- filter(eff, Island == "Oahu") # already 2002 thru 2020 (last year 2017)
oh_eff_sub <- filter(oh_eff, Date_ != "1/8/2016")
oh_eff_f <- bind_rows(oh_eff_sub, eff_sub)

## kauai/niihau
kn_eff <- filter(eff, Island == "Kauai") # already 2003 thru 2020 (Jul 2020)

## load sightings data 
crc_sight <- read.csv("Kogiasima analyses - 2020/Kogia_SightingsDB_forMap.csv", header = T) # database sightings, for all CRC sightings
unique(crc_sight$Island)
ks_sight <- filter(crc_sight, Species == "Dwarf sperm whale")
ks_sight$Lon <- ks_sight$Lon*-1

## filter out individual islands sightings
mn_crc <- filter(ks_sight, Island == "Lanai") # filter 
oh_crc <- filter(ks_sight, Island == "Oahu")
kn_crc <- filter(ks_sight, Island != "Lanai") %>%
  filter(., Island != "Hawaii") %>%
  filter(., Island != "Oahu")

# get dwarf sperm whale sightings not pygmy
kn_crc <- kn_crc[c(1,3,4,8),]

## project sightings data
mn_sf <- st_as_sf(mn_crc, coords = c("Lon","Lat"), crs = 4326) %>% # CRC source data 
  st_transform(crs = 26904)

oh_sf <- st_as_sf(oh_crc, coords = c("Lon","Lat"), crs = 4326) %>% # CRC source data 
  st_transform(crs = 26904)

kn_sf <- st_as_sf(kn_crc, coords = c("Lon","Lat"), crs = 4326) %>% # CRC source data 
  st_transform(crs = 26904)

# get coastline shapefile of islands 
coast <- readOGR("Shapefiles", layer = "Coastline")
coast <- st_as_sf(coast)# make sf object
coastr <- st_transform(coast, crs = 26904)# transform to projection of data 

## create island label
label_df <- data.frame(
  label = c(
  paste0("Kaua\u02BBi"), paste0("Ni\u02BBihau"), paste0("O\u02BBahu"),
  paste0("Moloka\u02BBi"), "Maui", paste0("L\u0101na\u02BBi"), paste0("Kaho\u02BBolawe"),
  paste0("Hawai\u02BBi")),
  lat = c(22.1, 21.7, 21.5, 21.14, 20.8, 20.81, 20.48, 19.6),
  lon = c(-159.5, -160.01, -158.04, -156.99, -156.35, - 156.9, - 156.63, - 155.54))

# project
label_sf <- st_as_sf(label_df, coords = c("lon","lat"), crs = 4326) %>%
  st_transform(crs = 26904)

# get coords
label_coords <- as.data.frame(st_coordinates(label_sf))
label_coords$label <- label_df$label

## plot CRC sightings and effort 
theme_map <- function() {
  theme_bw() +
    theme(panel.background = element_rect(fill = 'white', colour = 'black', size = 1.25),
          axis.text = element_text(colour = 'black'),
          plot.title = element_text(colour = 'black', face = 'bold')) #+
  
}

## Maui Nui plot =========================================================== ##
# make segments of tracklines to color by sea state

# need to fix one set of na values
mn_eff$Sea_state_new <- as.character(NA)

# adjust n/a values for particular day based on sea state before/after
mn_eff <- mn_eff %>%
  mutate(
    Sea_state_new = ifelse(Date_ == "12/14/2012" & Sea_state_num == 100,
                           "Three", Sea_state)
  )

# make new bft variable
mn_eff$bft <- as.character(NA)
mn_eff <- mn_eff %>%
  mutate(
    bft = ifelse(Sea_state_new %in% c("zero","Zero","One",
                                  "Two", "n/a"), "low", bft),
    bft = ifelse(Sea_state_new %in% c("Three","Four","Five",
                                  "Six"), "high", bft)
  )

# create segments by sea state value, by day, vessel
df_mn_sf <- mn_eff %>%
  
  # detect each time sea state changes, & create a duplicate point with previous sea state
  mutate(change.bft = tidyr::replace_na(lag(bft) != bft, FALSE)) %>%
  mutate(bft = ifelse(change.bft,
                      paste(lag(bft), bft, sep = ";"),
                      bft) %>%
           strsplit(";")) %>%
  tidyr::unnest(cols = c(bft)) %>%
  
  # create new group column that increments with every colour change
  mutate(change.bft = tidyr::replace_na(lag(bft) != bft, FALSE)) %>%
  mutate(new.bft = cumsum(change.bft)) %>%
  
  # project
  st_as_sf(coords = c('Long_dd', 'Lat_dd')) %>%
  st_set_crs(., value = 4326) %>%
  st_transform(crs = 26904) %>%
  
  # group by both original sea state (for colour) & new sea state (for group)
  group_by(Date, Vessel, bft, new.bft) %>%
  summarize(do_union = F) %>%
  st_cast(.,'LINESTRING') %>%
  ungroup()

# set coords of effort data for plot x and y limits
mn_eff_sf <- st_as_sf(mn_eff, coords = c("Long_dd", "Lat_dd"), crs = 4326) %>% # effort data 
  st_transform(crs = 26904)
mn_x <- as.data.frame(st_coordinates(mn_eff_sf))
#mn_y <- as.data.frame(st_coordinates(mn_sf))

mn_plot <- ggplot() +
  # effort  tracklines colored by sea state
  geom_sf(data = df_mn_sf, lwd = .4, aes(color = bft, group = new.bft), alpha = 0.8) +
  
  # island polygon
  geom_sf(data = coastr, lwd = 1.25, fill = "lightgrey") +
  
  # ks sightings maui nui
  geom_sf(data = mn_sf, shape = 21, color = 'black', fill = 'black', size = 1.5) +
  
  # island labels
  geom_text(data = label_coords, aes(X,Y, label = label, fontface = "bold"), size = 4.5) +
  
  # coordinate info
  coord_sf(xlim = c(min(mn_x$X) + 70000, max(mn_x$X) + 30000), ## for effort trackline maps
           ylim = c(min(mn_x$Y) - 6000, max(mn_x$Y) + 3000),
           crs = 26904) +
  # aesthetics
  theme_map() +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("grey81","grey45")) +
  scale_x_continuous(breaks = c(-157.3,-157,-156.7, -156.4)) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = 'bl', text_cex = unit(1, "cm")) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 2),
    axis.text = element_text(size = 11),
    legend.position = "none"
  )
mn_plot
ggsave("Kogiasima analyses - 2020/final high res maps/Updated 12JUL2021/Ks2020_Effort2002-2020_KsCRCSight_MauiNui_v4.jpeg",
dpi = 600, width = 6,
height = 5,  units = "in")

## Oahu plot =========================================================== ##
# make segments to color by sea state
unique(oh_eff_f$Sea_state) # 1 n/a, first loc of day, subsequent SS's = zero (low)

# make new bft variable
oh_eff_f$bft <- as.character(NA)
oh_eff_f <- oh_eff_f %>%
  mutate(
    bft = ifelse(Sea_state %in% c("zero","Zero","One",
                                  "Two", "n/a"), "low", bft),
    bft = ifelse(Sea_state %in% c("Three","Four","Five"), "high", bft)
  )

# create segments by sea state value, by day, vessel
df_oh_sf <- oh_eff_f %>%
  
  # detect each time sea state changes, & create a duplicate point with previous sea state
  mutate(change.bft = tidyr::replace_na(lag(bft) != bft, FALSE)) %>%
  mutate(bft = ifelse(change.bft,
                      paste(lag(bft), bft, sep = ";"),
                      bft) %>%
           strsplit(";")) %>%
  tidyr::unnest(cols = c(bft)) %>%
  
  # create new group column that increments with every colour change
  mutate(change.bft = tidyr::replace_na(lag(bft) != bft, FALSE)) %>%
  mutate(new.bft = cumsum(change.bft)) %>%
  
  # project
  st_as_sf(coords = c('Long_dd', 'Lat_dd')) %>%
  st_set_crs(., value = 4326) %>%
  st_transform(crs = 26904) %>%
  
  # group by both original sea state (for colour) & new sea state (for group)
  group_by(Date, Vessel, bft, new.bft) %>%
  summarize(do_union = F) %>%
  st_cast(.,'LINESTRING') %>%
  ungroup()

# set coords of effort data for plot x and y limits
oh_eff_sf <- st_as_sf(oh_eff_f, coords = c("Long_dd", "Lat_dd"), crs = 4326) %>% # effort data 
  st_transform(crs = 26904)
oh_x <- as.data.frame(st_coordinates(oh_eff_sf))
oh_y <- as.data.frame(st_coordinates(oh_sf))

# fix molokai label
label_df <- data.frame(
  label = c(
    paste0("Kaua\u02BBi"), paste0("Ni\u02BBihau"), paste0("O\u02BBahu"),
    paste0("Moloka\u02BBi"), "Maui", paste0("L\u0101na\u02BBi"), paste0("Kaho\u02BBolawe"),
    paste0("Hawai\u02BBi")),
  lat = c(22.1, 21.7, 21.5, 21.3, 20.8, 20.81, 20.48, 19.6),
  lon = c(-159.5, -160.01, -158.04, -157.28, -156.35, - 156.9, - 156.63, - 155.54))

# project
label_sf <- st_as_sf(label_df, coords = c("lon","lat"), crs = 4326) %>%
  st_transform(crs = 26904)

# get coords
label_coords <- as.data.frame(st_coordinates(label_sf))
label_coords$label <- label_df$label

# map
oh_plot <- ggplot() +
  # effort tracklines colored by sea state
  geom_sf(data = df_oh_sf, lwd = .4, aes(color = bft, group = new.bft), alpha = 0.8) +
  
  # island polygon
  geom_sf(data = coastr, lwd = 1.25, fill = "lightgrey") +
  
  # ks sightings oahu
  geom_sf(data = oh_sf, shape = 21, color = 'black', fill = 'black', size = 1.5) +
  
  # island labels
  geom_text(data = label_coords, aes(X,Y, label = label, fontface = "bold"), size = 4.5) +
  
  # coordinate info
  coord_sf(xlim = c(min(oh_x$X) + 50000, max(oh_x$X) + 9000), ## for effort trackline maps
           ylim = c(min(oh_x$Y) - 7000, max(oh_x$Y) + 1000),
           crs = 26904) +
  # aesthetics
  theme_map() +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("grey81","grey45")) +
  #scale_x_continuous(breaks = c(-157.3,-157,-156.7, -156.4)) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = 'bl', text_cex = unit(1, "cm")) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 2),
    axis.text = element_text(size = 11),
    legend.position = "none"
  )
oh_plot
ggsave("Kogiasima analyses - 2020/final high res maps/Updated 12JUL2021/Ks2020_Effort2002-2020_KsCRCSight_Oahu_v5.jpg",
       dpi = 600, width = 6,
       height = 6,  units = "in")

## Kauai/Niihau plot =========================================================== ##
# make segments to color by sea state
unique(kn_eff$Sea_state) # no n/a's

# make new bft variable
kn_eff$bft <- as.character(NA)
kn_eff <- kn_eff %>%
  mutate(
    bft = ifelse(Sea_state %in% c("zero","Zero","One",
                                  "Two","Two "), "low", bft),
    bft = ifelse(Sea_state %in% c("Three","Four","Five","Six"), "high", bft)
  )

# create segments by sea state value, by day, vessel
df_kn_sf <- kn_eff %>%
  
  # detect each time sea state changes, & create a duplicate point with previous sea state
  mutate(change.bft = tidyr::replace_na(lag(bft) != bft, FALSE)) %>%
  mutate(bft = ifelse(change.bft,
                      paste(lag(bft), bft, sep = ";"),
                      bft) %>%
           strsplit(";")) %>%
  tidyr::unnest(cols = c(bft)) %>%
  
  # create new group column that increments with every colour change
  mutate(change.bft = tidyr::replace_na(lag(bft) != bft, FALSE)) %>%
  mutate(new.bft = cumsum(change.bft)) %>%
  
  # project
  st_as_sf(coords = c('Long_dd', 'Lat_dd')) %>%
  st_set_crs(., value = 4326) %>%
  st_transform(crs = 26904) %>%
  
  # group by both original sea state (for colour) & new sea state (for group)
  group_by(Date, Vessel, bft, new.bft) %>%
  summarize(do_union = F) %>%
  st_cast(.,'LINESTRING') %>%
  ungroup()

# set coords of effort data for plot x and y limits
kn_eff_sf <- st_as_sf(kn_eff, coords = c("Long_dd", "Lat_dd"), crs = 4326) %>% # effort data 
  st_transform(crs = 26904)
kn_x <- as.data.frame(st_coordinates(kn_eff_sf))
kn_y <- as.data.frame(st_coordinates(kn_sf))

# fix labels
label_df <- data.frame(
  label = c(
    paste0("Kaua\u02BBi"), paste0("Ni\u02BBihau"), paste0("O\u02BBahu"),
    paste0("Moloka\u02BBi"), "Maui", paste0("L\u0101na\u02BBi"), paste0("Kaho\u02BBolawe"),
    paste0("Hawai\u02BBi")),
  lat = c(22.05, 21.95, 21.5, 21.3, 20.8, 20.81, 20.48, 19.6),
  lon = c(-159.53, -160.3, -158.04, -157.25, -156.35, - 156.9, - 156.63, - 155.54))

# project
label_sf <- st_as_sf(label_df, coords = c("lon","lat"), crs = 4326) %>%
  st_transform(crs = 26904)

# get coords
label_coords <- as.data.frame(st_coordinates(label_sf))
label_coords$label <- label_df$label

kn_plot <- ggplot() +
  # effort tracklines colored by sea state
  geom_sf(data = df_kn_sf, lwd = .4, aes(color = bft, group = new.bft), alpha = 0.8) +
  
  # island polygons
  geom_sf(data = coastr, lwd = 1.25, fill = "lightgrey") +
  
  # ks sightings kauai/niihau
  geom_sf(data = kn_sf, shape = 21, color = 'black', fill = 'black', size = 1.5) +
  
  # island labels
  geom_text(data = label_coords, aes(X,Y, label = label, fontface = "bold"), size = 4.5) +
  
  # coordinate info
  coord_sf(xlim = c(min(kn_x$X) - 7000, max(kn_x$X) + 2000), ## for effort trackline maps
           ylim = c(min(kn_x$Y) - 7000, max(kn_x$Y) + 2000),
           crs = 26904) +
  # aesthetics
  theme_map() +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("grey81","grey45")) +
  scale_y_continuous(breaks = c(21.5, 21.75, 22, 22.25, 22.5)) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = 'bl', text_cex = unit(1, "cm")) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 2),
    axis.text = element_text(size = 11),
    legend.position = "none"
  )
kn_plot
ggsave("Kogiasima analyses - 2020/final high res maps/Updated 12JUL2021/Ks2020_Effort_thruJun2020_KsCRCSight_KauaiNiihau_v4.jpg",
       dpi = 600, width = 6,
       height = 6,  units = "in")

## Stack all plots together ============================================================ ##
stack <- ggarrange(kn_plot, oh_plot, mn_plot, ncol = 1, nrow = 3)
stack

# save jpeg
ggsave("Kogiasima analyses - 2020/final high res maps/Updated 12JUL2021/Ks2020_Effort_thruJun2020_KsCRCSight_IslandsStacked_v7.jpg",
       dpi = 600, width = 6,
       height = 12,  units = "in")

# save high res tiff
ggsave("Kogiasima analyses - 2020/final high res maps/Updated 12JUL2021/Ks2020_Effort_thruJun2020_KsCRCSight_IslandsStacked_v7.tiff",
       dpi = 600, width = 6,
       height = 12,  units = "in")
