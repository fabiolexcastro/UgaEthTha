

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
sort(unique(pnts$Country))

pnts.ara <- filter(pnts, Species %in% c('Coffee Arabica', 'Coffee arabica', 'Arabica_Otros', 'arabica', 'Arabica'))
pnts.rob <- filter(pnts, Species %in% c('Coffee Robusta', 'robusta', 'Robusta'))

pnts.ara <- pnts.ara %>% 
  dplyr::select(Longitude, Latitude) %>% 
  mutate(name = 'arabica')

pnts.rob <- pnts.rob %>% 
  dplyr::select(Longitude, Latitude) %>% 
  mutate(name = 'Robusta')

# Remove duplicated by cell -----------------------------------------------

##
rmve.dupc <- function(occ){
  
  cat('To start\n')
  msk <- geodata::worldclim_global(var = 'prec', res = 2.5, path = './tmpr')
  msk <- msk[[1]]
  cll <- terra::extract(msk, occ[,c('Longitude', 'Latitude')], cell = T)
  cll <- cll[,3]
  occ <- mutate(occ, cell = cll)
  occ <- mutate(occ, dupv = duplicated(cell))
  occ <- filter(occ, dupv == FALSE)
  occ <- dplyr::select(occ, name, Longitude, Latitude)
  return(occ)
  
}

##
pnts.ara <- rmve.dupc(occ = pnts.ara)
pnts.rob <- rmve.dupc(occ = pnts.rob)

##
pnts <- rbind(pnts.ara, pnts.rob)

# To add the country for the points ---------------------------------------

admn <- terra::extract(vect(wrld), pnts[,c('Longitude', 'Latitude')])
admn <- dplyr::select(admn, name, admin, sov_a3)
admn <- rename(admn, country = name)
pnts <- as_tibble(cbind(pnts, admn))

dir.create('./tble')
write.csv(pnts, './tble/points-world_ara-rob.csv', row.names = FALSE)

# Filtering for the study zone --------------------------------------------
sort(unique(pnts$country))

