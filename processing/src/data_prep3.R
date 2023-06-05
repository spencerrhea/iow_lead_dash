library(sf)
library(tidyverse)

# addresses <- st_read('CARTERET.gdb')
# 
# boundries <- st_read('processing/data/input_data/geoconnex_boundaries.geojson')
# 
# hark_boundary <- st_read('processing/data/processed_data/wd_boundaries/NC0416025.geojson')
# hark_address <- st_filter(addresses, st_transform(hark_boundary, st_crs(addresses)))
# 
# buildings <- st_read('processing/data/processed_data/boundary_building/NC0416025.geojson')
# 
# joined_building <- st_join(this_building, cropped_parcels)
# 
# all_addresses <- st_read('processing/data/input_data/AddressNC.gdb')
# 
# st_write(all_addresses, 
#          'processing/data/input_data/AddressNC_dupremoved.geojson')
# 
# all_addresses <- all_addresses %>%
#     distinct(Full_Address, .keep_all = TRUE)
# 

# Download Boundaries from geoconnex
# boundaries <- sf::read_sf('https://urldefense.com/v3/__https://reference.geoconnex.us/collections/pws/items?state_code=NC&limit=10000&f=json__;!!OToaGQ!vMRRMCW-1Nb6i0rC2DarRFkssxktKHSEa1RFjVrXkr3p1xPh_10Gl3IVLFk29TDV-JfqVmCTvIxvMl7mJIK0f-A$')
# write_sf(boundaries, 'processing/data/input_data/geoconnex_boundaries.geojson')
# boundaries <- read_sf('processing/data/input_data/geoconnex_boundaries.geojson')

# Read in building layer
# building_layer <- read_sf('processing/data/input_data/NC_Buildings_Footprints_(2010).geojson')
# Read in address data
all_addresses <- st_read('processing/data/input_data/AddressNC_dupremoved.geojson')


buildings_bounbdaries <- list.files('processing/data/processed_data/boundary_building/', 
                                    full.names = T)

occup_key <- read_csv('processing/data/processed_data/biulding_occup_key.csv') 
build_key <- read_csv('processing/data/processed_data/biulding_build_key.csv') 
flood_scheme_key <- read_csv('processing/data/processed_data/biulding_flood_scheme_key.csv')
flood_zone_key <- read_csv('processing/data/processed_data/biulding_flood_zone_key.csv') %>%
    rename(in_flood_zone = flood_zone_type)
stories_key <- read_csv('processing/data/processed_data/biulding_stories_key.csv')
units_key <- read_csv('processing/data/processed_data/biulding_units_key.csv') 



# names_to_keep <- c('OBJECTID', 'YEAR_BUILT', 'OCCUP_TYPE', 'BUILD_TYPE', 'parno', 'ownname', 'mailadd', 'munit',
#                    'mcity', 'mzip', 'siteadd', 'sunit', 'scity', 'legdecfull', 'parusecode', 'parusedesc', 'parusecd2',
#                    'ownfrst', 'ownlast', 'ownname2')

names_to_keep <- c('OBJECTID', 'YEAR_BUILT', 'occupancy_type', 'build_type', 'flood_scheme_type',
                   'in_flood_zone', 'num_stories', 'num_units', 'BLDG_VALUE', 'HTD_SQ_FT', 
                   'County', 'Full_Address')

dir.create('processing/data/processed_data/building_address_join_full')
dir.create('processing/data/processed_data/building_address_join_pretty')

# 2006 boundaies 

log_too_big <- tibble()
for(i in 137:length(buildings_bounbdaries)){
    print(i)
    buildings <- st_read(buildings_bounbdaries[i])
    
    boundary_file <- str_split_fixed(buildings_bounbdaries[i], '/', n = Inf)[,6]
    
    if(nrow(buildings) >= 12000) {
        boundary_name <- str_split_fixed(buildings_bounbdaries[i], '/', n = Inf)[,6]
        boundary_name <- str_split_fixed(boundary_name, '[.]', n = Inf)[,1]
        
        this_report <- tibble(boundary_name = !!boundary_name,
                              reason = 'file too large and did noit attempt')
        
        log_too_big <- rbind(log_too_big, this_report)
        
        print('file too large, did not atttemp')
        next()
    }
    
    if(nrow(buildings) == 0) next
    
    # Transform file
    buildings <- st_transform(buildings, st_crs(all_addresses))

    cropped_address <- st_crop(all_addresses, buildings)
    
    if(nrow(buildings) == 0) next
    
    all_buildings <- tibble()
    for(s in 1:nrow(buildings)){
        this_building <- buildings[s,]
        
        joined_building <- st_join(this_building, cropped_address)
        
        all_buildings <- rbind(all_buildings, joined_building)
    }
    
     all_buildings <- try(st_transform(all_buildings, crs = 4326) %>%
        distinct(OBJECTID, .keep_all = TRUE))
     
     # Sometimes errors with s2, switch off for those and turn back on
     if(inherits(all_buildings, 'try-error')){
         sf_use_s2(FALSE)
         
         all_buildings <- st_transform(all_buildings, crs = 4326) %>%
             distinct(OBJECTID, .keep_all = TRUE)
         
         sf_use_s2(TRUE)
     }
    
    write_sf(all_buildings, paste0('processing/data/processed_data/building_address_join_full/', boundary_file),
             delete_dsn = T)
    
    
    all_buildings_pretty <- left_join(all_buildings, occup_key, by = 'OCCUP_TYPE') %>%
        left_join(build_key, by = 'BUILD_TYPE') %>%
        left_join(flood_scheme_key, by = 'FL_SCHEME') %>%
        left_join(flood_zone_key, by = 'STATIC_BFE') %>%
        left_join(stories_key, by = 'NUM_STORY') %>%
        left_join(units_key, by = 'NUM_UNITS') %>%
        select(!!names_to_keep) %>%
        mutate(build_type = str_to_title(build_type),
               flood_scheme_type = str_to_title(flood_scheme_type),
               in_flood_zone = str_to_title(in_flood_zone),
               num_stories = str_to_title(num_stories),
               num_units = str_to_title(num_units)) %>%
        mutate(geometry = st_centroid(geometry))
    
    write_sf(all_buildings_pretty, paste0('processing/data/processed_data/building_address_join_pretty/', boundary_file),
             delete_dsn = T)
}

