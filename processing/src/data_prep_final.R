library(sf)
library(tidyverse)

#### Download Boundaries from geoconnex ####
boundaries <- sf::read_sf('https://urldefense.com/v3/__https://reference.geoconnex.us/collections/pws/items?state_code=NC&limit=10000&f=json__;!!OToaGQ!vMRRMCW-1Nb6i0rC2DarRFkssxktKHSEa1RFjVrXkr3p1xPh_10Gl3IVLFk29TDV-JfqVmCTvIxvMl7mJIK0f-A$')
write_sf(boundaries, 'processing/data/input_data/geoconnex_boundaries.geojson')
boundaries <- read_sf('processing/data/input_data/geoconnex_boundaries.geojson')



#### Subset Water district layers #### 
# Read in water district boundaries 
# boundaries <- read_sf('processing/data/input_data/temm.geojson')
boundaries <- read_sf('processing/data/input_data/geoconnex_boundaries.geojson') %>%
    rename(pwsid = id)

# Create names to boundary idea look up table 
boundary_names_to_code <- boundaries %>%
    as.data.frame() %>%
    select(pwsid, pws_name)

write_csv('processing/data/processed_data/boundary_names_to_code.csv')

# Filter for North Carolina 
nc_boundaries <- boundaries %>%
    filter(state_code == 'NC')
    # select(-service_area_type_code)

# Save NC boundaries 
sf::write_sf(obj = nc_boundaries, 
             dsn = 'processing/data/processed_data/nc_boundaries.geojson')

#### Subset building layer to water district boundaries #### 
# Read in building layer 
# Download data here: https://www.nconemap.gov/datasets/2d07f32b93184a758be29dc1f41344bd_0/explore
building_layer <- read_sf('processing/data/input_data/NC_Buildings_Footprints_(2010).geojson')

# Transform water boundries to building layer projection  
nc_boundaries <- nc_boundaries %>%
    st_transform(., st_crs(building_layer))

# Subset and save the boundaries as seperate files 
dir.create('processing/data/processed_data/boundary_building')
log_errors <- tibble()
boundary_codes <- nc_boundaries$pwsid
for(i in 1:length(boundary_codes)){
    
    this_boundary <- nc_boundaries[i,]
    
    # Filter building in boundary layer 
    boundary_buildings <- try(sf::st_filter(building_layer, this_boundary))
    
    if(inherits(boundary_buildings, 'try-error')){
        error_tib <- tibble(pwsid = this_boundary,
                            status = 'error')
        
        log_errors <- rbind(log_errors, error_tib)
        next
    }
    
    # Save individual file for buildings in each water disdrict
    file_name <- paste0('processing/data/processed_data/boundary_building/', boundary_codes[i], '.geojson')
    sf::st_write(boundary_buildings, file_name,
                 delete_dsn = T)
    
    # Log errors 
    sucesses_tib <- tibble(pwsid = this_boundary,
                           status = 'saved')
    
    log_errors <- rbind(log_errors, sucesses_tib)
    
}



#### Addresses ####
# Read in address data
# Download data here: https://www.nconemap.gov/datasets/32a40e6c5a734f6eac7b922e759592f3_1/explore
# The raw data set contained duplicates, unccoment and run these lines if data is
# re downloaded 

# # Read in Address dataset
# all_addresses <- st_read('processing/data/input_data/AddressNC.gdb')
# # remove duplicates
# all_addresses <- all_addresses %>%
#     distinct(Full_Address, .keep_all = TRUE)
# # Save data
# st_write(all_addresses,
#          'processing/data/input_data/AddressNC_dupremoved.geojson')

all_addresses <- st_read('processing/data/input_data/AddressNC_dupremoved.geojson')

# List buildings cropped to water boundaries 
buildings_bounbdaries <- list.files('processing/data/processed_data/boundary_building/', 
                                    full.names = T)

# Read in keys for codes to meaning for each metric in building dataset 
occup_key <- read_csv('processing/data/processed_data/biulding_occup_key.csv') 
build_key <- read_csv('processing/data/processed_data/biulding_build_key.csv') 
flood_scheme_key <- read_csv('processing/data/processed_data/biulding_flood_scheme_key.csv')
flood_zone_key <- read_csv('processing/data/processed_data/biulding_flood_zone_key.csv') %>%
    rename(in_flood_zone = flood_zone_type)
stories_key <- read_csv('processing/data/processed_data/biulding_stories_key.csv')
units_key <- read_csv('processing/data/processed_data/biulding_units_key.csv') 


names_to_keep <- c('OBJECTID', 'YEAR_BUILT', 'occupancy_type', 'build_type', 'flood_scheme_type',
                   'in_flood_zone', 'num_stories', 'num_units', 'BLDG_VALUE', 'HTD_SQ_FT', 
                   'County', 'Full_Address')

dir.create('processing/data/processed_data/building_address_join_full')
dir.create('processing/data/processed_data/building_address_join_pretty')

log_too_big <- tibble()
for(i in 1:length(buildings_bounbdaries)){
    # print(i)
    # Read in building associated with this boundary 
    buildings <- st_read(buildings_bounbdaries[i])
    
    boundary_file <- str_split_fixed(buildings_bounbdaries[i], '/', n = Inf)[,6]
    
    # Code seems to break when there are too many buildings in a layer, skip these
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
    
    # Transform file to addresses projection
    buildings <- st_transform(buildings, st_crs(all_addresses))

    # Crop addresses to boundry outline 
    cropped_address <- st_crop(all_addresses, buildings)
    
    if(nrow(buildings) == 0) next
    
    # Join buildings to parcel data 
    all_buildings <- tibble()
    for(s in 1:nrow(buildings)){
        this_building <- buildings[s,]
        
        joined_building <- st_join(this_building, cropped_address)
        
        all_buildings <- rbind(all_buildings, joined_building)
    }
    
    # Transfor inot 4326, what will be used for app 
     all_buildings <- try(st_transform(all_buildings, crs = 4326) %>%
        distinct(OBJECTID, .keep_all = TRUE))
     
     
     # Sometimes errors with s2, switch off for those and turn back on
     if(inherits(all_buildings, 'try-error')){
         sf_use_s2(FALSE)
         
         all_buildings <- st_transform(all_buildings, crs = 4326) %>%
             distinct(OBJECTID, .keep_all = TRUE)
         
         sf_use_s2(TRUE)
     }
    
     # Save buildings joined to adresses with all the data
    write_sf(all_buildings, paste0('processing/data/processed_data/building_address_join_full/', boundary_file),
             delete_dsn = T)
    
    # Creat what will be displayed on the app 
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
    
    # Save pretty file 
    write_sf(all_buildings_pretty, paste0('processing/data/processed_data/building_address_join_pretty/', boundary_file),
             delete_dsn = T)
}

#### Copy final dataset to shiny app directory ####
# Buildings + addresses 
pretty_addresses_dir <- 'processing/data/processed_data/building_address_join_pretty/'
pretty_addresses_files <- list.files(pretty_addresses_dir, full.names = T)

file.copy(pretty_addresses_files, 'shinny_app/data/building_address_join_pretty')

# Boundaries 
boundaries_dir <- 'processing/data/processed_data/wd_boundaries'
boundries_files <- list.files('processing/data/processed_data/wd_boundaries', full.names = T)

file.copy(boundries_files, 'shinny_app/data/wd_boundaries/')

# Boundary look up table 
file.copy('processing/data/processed_data/boundary_names_to_code.csv', 'shinny_app/data/boundary_names_to_code.csv')

#### Process justice 40 data ####
source('processing/src/prep_justice_40.R')
