## Kogia sima analyses 2020:
## Hawaii map: effort tracklines + sightings

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
library(marmap)
library(metR) # works with version 0.7.0 devtools::install_version("metR", version = "0.7.0", repos = "http://cran.us.r-project.org")
library(raster)
library(sp)

## load effort data
eff <- read.csv("Kogiasima analyses - 2020/Effort_thruJul2020_forMaps.csv", header = T) # all effort data 
str(eff) # review
summary(eff$Long_dd) # check longitudes
eff$Date <- as.Date(eff$Date_, format = "%m/%d/%Y", tz = "Pacific/Honolulu") # format date
hi_eff <- filter(eff, Island == "Hawaii" ) # filter Hawaii tracklines
hi_eff <- filter(hi_eff, Nearest.Island != "Maui") # remove weird tracklines to Maui 
hi_eff <- filter(hi_eff, Year_ %in% c(2002:2020)) # filter years of Kogia sightings 

## load sightings data 
cat_sight <- read.csv("Kogiasima analyses - 2020/Kogiasima_CatalogSightings_forMap.csv", header = T) # catalog sightings, for HIKs020
str(cat_sight) # review
crc_sight <- read.csv("Kogiasima analyses - 2020/Kogia_SightingsDB_forMap.csv", header = T) # database sightings, for all CRC sightings
str(crc_sight) # review
ks_sight <- filter(crc_sight, Species == "Dwarf sperm whale") # filter K. sima sightings
ks_sight$Lon <- ks_sight$Lon*-1 # fix longitudes
hi_crc <- filter(ks_sight, Island == "Hawaii") # filter HI sightings

## project data 
eff_sf <- st_as_sf(hi_eff, coords = c("Long_dd", "Lat_dd"), crs = 4326) %>% # effort data 
  st_transform(crs = 26904)

crc_sf <- st_as_sf(hi_crc, coords = c("Lon","Lat"), crs = 4326) %>% # CRC source data 
  st_transform(crs = 26904)


# get coastline shapefile of islands 
coast <- readOGR("Shapefiles", layer = "Coastline")
coast <- st_as_sf(coast)# make sf object
coastr <- st_transform(coast, crs = 26904)# transform to projection of data 

## create island label
label_df <- data.frame(
  label = c(paste0("Hawai\u02BBi"), "Maui", paste0("Kaho\u02BBolawe")),
  lat = c(19.62, 20.53, 20.57),
  lon = c(-155.65, -156.25, -156.9))

# proejct
label_sf <- st_as_sf(label_df, coords = c("lon","lat"), crs = 4326) %>%
  st_transform(crs = 26904)

# get coords
label_coords <- as.data.frame(st_coordinates(label_sf))
label_coords$label <- label_df$label

# port labels 
port <- data.frame(label = c(paste0("Honok\u014dhau"), "Kawaihae"),
                   lat = c(19.67, 20.035),
                   lon = c(-156.027, -155.827))
port_sf <- st_as_sf(port, coords = c("lon","lat"), crs = 4326) %>%
  st_transform(crs = 26904)

port_coords <- as.data.frame(st_coordinates(port_sf))
port_coords$label <- port$label

# for HIKs020 map
label2 <- paste0("Hawai\u02BBi")
lat2 <- 19.72
lon2 <- -155.95
label_df2 <- data.frame(label = label2, lat = lat2, lon = lon2)

port2 <- port

# make effort tracklines
# make segments to color by sea state
unique(hi_eff$Sea_state_new) # review effort file to check n/a's

# need to fix one set of na values
hi_eff$Sea_state_new <- as.character(NA)

# adjust n/a values for particular day based on sea state before/after
hi_eff <- hi_eff %>%
  mutate(
    Sea_state_new = ifelse(Date_ %in% c("5/12/2003","5/5/2008",
                                        "5/13/2008","10/21/2009",
                                        "12/20/2009") & Sea_state_num == 100,
                           "Three", Sea_state)
  )


# make new bft variable
hi_eff$bft <- as.character(NA)
hi_eff <- hi_eff %>%
  mutate(
    bft = ifelse(Sea_state_new %in% c("zero","Zero","One", "One ", "Two ",
                                  "Two", "n/a"), "low", bft),
    bft = ifelse(Sea_state_new %in% c("Three","Four","Five", "Six", ""), "high", bft)
  )

# create segments by sea state value, by day, vessel
df_hi_sf <- hi_eff %>%
  
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


## plot CRC sightings and effort 
theme_map <- function() {
  theme_bw() +
    theme(panel.background = element_rect(fill = 'white', colour = 'black', size = 1.25),
          axis.text = element_text(colour = 'black'),
          plot.title = element_text(colour = 'black', face = 'bold')) #+
  
}

# get coords to adjust boundaries
x <- as.data.frame(st_coordinates(eff_sf))
y <- as.data.frame(st_coordinates(crc_sf))

## Hawaii, Effort 2002-2020, all CRC sightings 
ggplot() +
  # effort tracklines colored by sea state
  geom_sf(data = df_hi_sf, lwd = .4, aes(color = bft, group = new.bft), alpha = 0.8) +
  
  # island polygons
  geom_sf(data = coastr, lwd = 1.25, fill = "lightgrey") +
  
  # crc ks sightings
  geom_sf(data = crc_sf, shape = 21, color = 'black', fill = 'black', size = 1.5) +
  
  # island and port labels
  geom_text(data = label_coords, aes(X,Y, label = label, fontface = "bold"), size = 4.5) +
  geom_text(data = port_coords, aes(X,Y, label = label, fontface = "bold", hjust = -.1, vjust = -.5), size = 2.5) +
  
  # coordinate info
  coord_sf(xlim = c(min(x$X) - 1000, max(x$X) + 30000), ## for effort trackline maps
           ylim = c(min(x$Y) - 6000, max(x$Y) + 3000),
           crs = 26904) +
  # aesthetics
  scale_color_manual(values = c("grey81","grey45")) +
  theme_map() +
  xlab("") +
  ylab("") +
  scale_x_continuous(breaks = c(-157,-156.5,-156)) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = 'bl', text_cex = unit(1, "cm")) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 2),
    axis.text = element_text(size = 11),
    legend.position = "none"
  )

# save it (jpeg)
ggsave("Kogiasima analyses - 2020/final high res maps/Updated 12JUL2021/Ks2020_Effort2002-2020_KsCRCSight_Hawaii_v7.jpeg",
       dpi = 600, width = 6,
       height = 6,  units = "in")

# save it (tiff)
ggsave("Kogiasima analyses - 2020/final high res maps/Updated 12JUL2021/Ks2020_Effort2002-2020_KsCRCSight_Hawaii_v7.tiff",
       dpi = 600, width = 6,
       height = 6,  units = "in")

# save eps file
ggsave("Kogiasima analyses - 2020/final high res maps/Updated 12JUL2021/Ks2020_Effort2002-2020_KsCRCSight_Hawaii_v7.eps",
       dpi = 600, width = 6,
       height = 6,  units = "in")

## ========================================================================== ##
## HIKs020 sightings, all sources
## will need to plot everything in crs = 4326 with bathymetry data 

# get HIKs020 sightings
ks20 <- filter(cat_sight, ID == "HIKs020") %>%
  filter(!is.na(Lat))

# get depth contour lines from NOAA database via marmap
lon1 = floor(min(ks20$Lon+2))
lon2 = ceiling(max(ks20$Lon-2))
lat1 = ceiling(max(ks20$Lat+2))
lat2 = floor(min(ks20$Lat-2))

# get bathymetry data
b = getNOAA.bathy(lon1, lon2, lat1, lat2, resolution = 1.5)
bf = fortify.bathy(b)
plot(b) # check

## get depth contours from shapefiles 
iso200 <- readOGR("Shapefiles", layer = "d200m")
iso200 <- st_as_sf(iso200)# make sf object
iso200 <- st_transform(iso200, crs = 4326)# transform to projection of data 

## contour map, using rasters
depthH <- raster("Rasters/multibeamUTM4N.nc") # read raster
dc <- rasterToContour(depthH, levels = c(-1000, -2000, -3000, -4000), maxpixels = 1000000) # higher resolution
plot(dc) # check
dc_sf <- st_as_sf(dc) # make sf object
dc_wg84 <- st_transform(dc_sf, crs = 4326) # transform projection

# transform islands/coastline projection
coast_wg84 <- st_transform(coastr, crs = 4326)
plot(coast_wg84) # check

# label projections
label_df2_wg84 <- st_as_sf(label_df2, coords = c("lon","lat"), crs = 4326) %>%
  st_transform(crs = st_crs(world))
label2_coords <- as.data.frame(st_coordinates(label_df2_wg84))
label2_coords$label <- label_df2_wg84$label2

# map it
ks20_bw <- ggplot() +
  # island polygon 
  geom_sf(data = coast_wg84, lwd = 1.25) +
  
  # depth contour lines
  geom_sf(data = dc_wg842, linetype = "dashed", color = "darkgrey") +
  geom_sf(data = iso200_wg84, fill = NA, linetype = "dashed", color = "darkgrey") +
  
  # island and port labels
  geom_text(data = label2_coords, aes(X,Y, label = label, fontface = "bold"), size = 4.5) +
  geom_text(data = port2, aes(lon,lat, label = label, fontface = "bold", hjust = -.1, vjust = -.5), size = 2.5) +
  
  # coordinate info
  coord_sf(xlim = c(-156.35, -155.85),
           ylim = c(19.45, 19.95),
           expand = F) +
  
  # text labels for contour lines
  geom_text_contour(data = bf, aes(x= x, y = y, z = z),breaks=c(-200),
                    show.legend = FALSE, size = 3.5, alpha = .6, nudge_y = 0.042, nudge_x = -0.044, stroke = .3) +
  geom_text_contour(data = bf, aes(x= x, y = y, z = z),breaks=c(-1000),
                    show.legend = FALSE, size = 3.5, alpha = .6, nudge_y = 0.009, stroke = .2, nudge_x = -0.018) +
  geom_text_contour(data = bf, aes(x= x, y = y, z = z),breaks=c(-2000),
                    show.legend = FALSE, size = 3.5, alpha = .6, nudge_y = -.009, stroke = .2, nudge_x = -0.012) +
  geom_text_contour(data = bf, aes(x= x, y = y, z = z),breaks=c(-3000),
                    show.legend = FALSE, size = 3.5, alpha = .6, nudge_y = -.04, stroke = .2, nudge_x = 0.018) +
  geom_text_contour(data = bf, aes(x= x, y = y, z = z),breaks=c(-4000),
                    show.legend = FALSE, size = 3.5, alpha = .6, nudge_y = -.002, stroke = .2, nudge_x = -0.01) +
  geom_point(data = ks20, aes(x = Lon, y = Lat), shape = 21, color = "black", fill = "black", alpha = 0.7, size = 3) +
  
  # aesthetics
  theme_map() +
  xlab("") +
  ylab("") +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering(),
                         pad_x = unit(1.5, "cm")) +
  annotation_scale(location = 'bl', text_cex = unit(1, "cm")) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 2),
    axis.text = element_text(size = 11)
  )

ks20_bw

# save it (jpeg)
ggsave("Kogiasima analyses - 2020/final high res maps/Final 15JAN2021/Ks2020_HIKs020Sightings_allSources_DepthContour_zoomin_v5.jpg",
       dpi = 600, width = 6, height = 6, units = "in")

# save it (tiff)
ggsave("Kogiasima analyses - 2020/final high res maps/Final 15JAN2021/Ks2020_HIKs020Sightings_allSources_DepthContour_zoomin_v5.jpg",
       dpi = 600, width = 6, height = 6, units = "in")