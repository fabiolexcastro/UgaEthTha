
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, glue, ggpubr, RColorBrewer, rnaturalearthdata, rnaturalearth)

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


# To make the map  --------------------------------------------------------

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


