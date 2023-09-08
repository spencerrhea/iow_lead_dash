library(sf)
library(tidyverse)


# Download Boundaries from geoconnex
# boundaries <- sf::read_sf('https://urldefense.com/v3/__https://reference.geoconnex.us/collections/pws/items?state_code=NC&limit=10000&f=json__;!!OToaGQ!vMRRMCW-1Nb6i0rC2DarRFkssxktKHSEa1RFjVrXkr3p1xPh_10Gl3IVLFk29TDV-JfqVmCTvIxvMl7mJIK0f-A$')
# write_sf(boundaries, 'processing/data/input_data/geoconnex_boundaries.geojson')
# boundaries <- read_sf('processing/data/input_data/geoconnex_boundaries.geojson')

# Read in building layer
# building_layer <- read_sf('processing/data/input_data/NC_Buildings_Footprints_(2010).geojson')
# Read in parcle data
parcle_layer <- read_sf('processing/data/input_data/NC_Parcels_all.gdb',
                        layer = 'nc_parcels_poly')


buildings_bounbdaries <- list.files('processing/data/processed_data/boundary_building/', 
                                    full.names = T)

names_to_keep <- c('OBJECTID', 'YEAR_BUILT', 'OCCUP_TYPE', 'BUILD_TYPE', 'parno', 'ownname', 'mailadd', 'munit',
                   'mcity', 'mzip', 'siteadd', 'sunit', 'scity', 'legdecfull', 'parusecode', 'parusedesc', 'parusecd2',
                   'ownfrst', 'ownlast', 'ownname2')

dir.create('processing/data/processed_data/building_parcle_join')

# 2006 boundaies 
for(i in 48:length(buildings_bounbdaries)){
    print(i)
    buildings <- st_read(buildings_bounbdaries[i])
    
    if(nrow(buildings) == 0) next
    
    boundary_file <- str_split_fixed(buildings_bounbdaries[i], '/', n = Inf)[6]
    
    boundary <- st_read(paste0('processing/data/processed_data/wd_boundaries/', boundary_file))
    
    # cropped_parcels <- st_intersects(st_transform(boundary, st_crs(parcle_layer)), parcle_layer)
    cropped_parcels <- st_crop(parcle_layer, st_transform(boundary, st_crs(parcle_layer)))
    
    # parcle_numbres <- parcle_layer[cropped_parcels[[1]],]$parno
    
    # wd_parcels <- parcle_layer %>%
    #     filter(parno %in% !!parcle_numbres)
    
    buildings <- st_transform(buildings, st_crs(parcle_layer))
    
    if(nrow(buildings) == 0) next
    all_buildings <- tibble()
    for(s in 1:nrow(buildings)){
        this_building <- buildings[s,]
        
        joined_building <- st_join(this_building, cropped_parcels)
        
        all_buildings <- rbind(all_buildings, joined_building)
    }
    
    write_sf(select(all_buildings, names_to_keep), paste0('processing/data/processed_data/building_parcle_join/', boundary_file),
             delete_layer = T)
}


# join_building_parcels <- function(buildings, parcels){
#     
#     buildings <- st_transform(buildings, st_crs(parcels))
#     
#     look <- st_join(buildings, parcels)
# }
# 
# buildings <- st_read(buildings_bounbdaries[9])
# 
# output <- map(buildings, parcle_layer, join_building_parcels)