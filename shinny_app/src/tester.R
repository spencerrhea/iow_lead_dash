library(sf)
library(tidyverse)

look <- read_sf('../processing/data/processed_data/building_adress_join/NC0103106.geojson')
mapview::mapview(look)

buildings <- read_sf('processing/data/processed_data/boundary_building/NC0416025.geojson')
mapview::mapview(buildings)

wd_boundary <- read_sf('processing/data/processed_data/wd_boundaries/NC0416025.geojson')
mapview::mapview(wd_boundary) 

cropped_parcels_ <- st_crop(parcle_layer, st_transform(wd_boundary, st_crs(parcle_layer)))
mapview::mapview(cropped_parcels_)

adresses <- st_read('CARTERET.gdb')

joined_building <- st_join(buildings, st_transform(adresses, st_crs(buildings)))
mapview::mapview(joined_building)


justice40 <- st_read('shinny_app/data/usa')

justice40_nc <- justice40 %>%
    filter(SF == 'North Carolina')



mapview::mapview(justice40)

# mapview::mapview(st_read('processing/data/input_data/geoconnex_boundaries.geojson'))
# all_boundaries <- st_read('processing/data/input_data/geoconnex_boundaries.geojson')
