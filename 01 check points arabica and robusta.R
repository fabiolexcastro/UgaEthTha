
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, geodata, glue, rgbif, ggpubr, RColorBrewer, rnaturalearthdata, rnaturalearth)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Vector data
wrld <- ne_countries(returnclass = 'sf', scale = 50)
afrc <- filter(wrld, region_un == 'Africa')
zone <- filter(afrc, name %in% c('Ethiopia', 'Uganda', 'Tanzania'))

## Tabular data
pnts <- read_csv('D:/OneDrive - CGIAR/Projects/Points/tbl/bd/coffee_v3_2022.csv')

## Filter for the study zone 
pnts <- filter(pnts, Country %in% c('Tanzania', 'Uganda', 'Ethiopia'))
write.csv(pnts, './tble/points-zone_ara-rob.csv', row.names = FALSE)
sort(unique(pnts$Country))
pnts <- mutate(pnts, Species = ifelse(Species == 'Coffea_arabica', 'Arabica', 'Robusta'))

# To add the country for the points ---------------------------------------
admn <- terra::extract(vect(wrld), pnts[,c('Longitude', 'Latitude')])
admn <- dplyr::select(admn, name)
pnts <- as_tibble(cbind(pnts, admn))

dir.create('./tble')
write.csv(pnts, './tble/points-zone_ara-rob.csv', row.names = FALSE)

# To make the map  --------------------------------------------------------#

gmap <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey50') + 
  geom_sf(data = zone, fill = NA, col = 'grey10') + 
  geom_point(data = pnts, aes(x = Longitude, y = Latitude, col = Species), size = 0.5) +
  coord_sf(xlim = ext(zone)[1:2], ylim = ext(zone)[3:4]) + 
  labs(x = 'Lon', y = 'Lat', col = 'Coffee') +
  theme_bw() + 
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 6), 
    axis.text.x = element_text(size = 6)
  )

gmap
ggsave(plot = gmap, filename = './png/maps/points_ara-rob.jpg', units = 'in', width = 4, height = 5, dpi = 300)


# Download GBIF -----------------------------------------------------------

## Points 
pnts.ourd <- read_csv('./tble/points-zone_ara-rob.csv')

## Points
pnts.gbif <- occ_search(scientificName = 'Coffea arabica', hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
pnts.gbif <- pnts.gbif$data
pnts.gbif <- filter(pnts.gbif, country %in% c('Ethiopia'))
pnts.gbif <- pnts.gbif %>% dplyr::select(scientificName, decimalLongitude, decimalLatitude)

## Join GBIF and our DB
pnts.ourd <- pnts.ourd %>% dplyr::select(Species, Longitude, Latitude)
pnts.gbif <- pnts.gbif %>% dplyr::select(Species = scientificName, Longitude = decimalLongitude, Latitude = decimalLatitude)

pnts.ourd %>% pull(Species) %>% table()
pnts.gbif %>% pull(Species) %>% table()
pnts.gbif <- mutate(pnts.gbif, Species = 'Arabica')

pnts.gbif <- pnts.gbif %>% mutate(source = 'GBIF')
pnts.ourd <- pnts.ourd %>% mutate(source = 'Our DB')

pnts.allp <- rbind(pnts.gbif, pnts.ourd)
write.csv(pnts.allp, './tble/points-zone_ara-rob_gbif.csv', row.names = FALSE)


# Ethiopia ----------------------------------------------------------------

etho <- zone %>% filter(sov_a3 == 'ETH')
pnts <- rbind(pnts.gbif, pnts.ourd)
eth1 <- st_as_sf(gadm(country = 'ETH', level = 1, path = './tmpr'))
lbls <- eth1 %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>% mutate(name = eth1$NAME_1)

pnts <- as_tibble(cbind(pnts, name = terra::extract(vect(etho), pnts[,2:3])[,c('name')]))
pnts <- filter(pnts, name == 'Ethiopia')

gmap <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey50') + 
  geom_sf(data = zone, fill = NA, col = 'grey10') + 
  geom_sf(data = eth1, fill = 'grey80', col = 'grey50') +
  geom_point(data = pnts, aes(x = Longitude, y = Latitude, col = source), size = 1.2) +
  scale_color_manual(values = brewer.pal(n = 2, name = 'Set2')) +
  geom_text_repel(data = lbls, aes(x = X, y = Y, label = name), family = 'Segoe UI', size = 2) +
  coord_sf(xlim = ext(etho)[1:2], ylim = ext(etho)[3:4]) + 
  ggtitle(label = 'Coffee arabica points (GBIF + Our database)', 
          subtitle = ) +
  labs(x = 'Lon', y = 'Lat', col = 'Coffee') +
  theme_bw() + 
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 6), 
    axis.text.x = element_text(size = 6), 
    plot.title = element_text(face = 'bold', hjust = 0.5, size = 14, family = 'Segoe UI'),
    text = element_text(family = 'Segoe UI'), 
    legend.position = c(0.9, 0.12)
  )

ggsave(plot = gmap, filename = './png/maps/points_ara_eth.jpg', units = 'in',width = 7, height = 5, dpi = 300)

write.csv(pnts, './tble/points-eth.csv', row.names = FALSE)
