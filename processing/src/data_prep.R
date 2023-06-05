# Get NC Boundaries 
library(sf)
library(tidyverse)

#### Subset building layer #### 
boundaries <- read_sf('processing/data/input_data/temm.geojson')
nc_boundaries <- boundaries %>%
    filter(state_code == 'NC') %>%
    select(-service_area_type_code)
sf::write_sf(obj = nc_boundaries, 
             dsn = 'processing/data/processed_data/nc_boundaries.geojson')

building_layer <- read_sf('processing/data/input_data/NC_Buildings_Footprints_(2010).geojson')

nc_boundaries <- nc_boundaries %>%
    st_transform(., st_crs(building_layer))

dir.create('processing/data/processed_data/boundary_building')
log_errors <- tibble()
boundary_codes <- nc_boundaries$pwsid
for(i in 1:length(boundary_codes)){
    
    this_boundary <- nc_boundaries[i,]
    
    boundary_buildings <- try(sf::st_filter(building_layer, this_boundary))
    
    if(inherits(boundary_buildings, 'try-error')){
        error_tib <- tibble(pwsid = this_boundary,
                            status = 'error')
        
        log_errors <- rbind(log_errors, error_tib)
        next
    }
    
    file_name <- paste0('processing/data/processed_data/boundary_building/', boundary_codes[i], '.geojson')
    sf::st_write(boundary_buildings, file_name,
                 delete_dsn = T)
    
    sucesses_tib <- tibble(pwsid = this_boundary,
                           status = 'saved')
    
    log_errors <- rbind(log_errors, sucesses_tib)
    
}

#### Make centriod layers ####
wd_buildings <- list.files('processing/data/processed_data/boundary_building/', full.names = T)



nc_boundaries <- nc_boundaries %>%
    st_transform(., st_crs(building_layer))

dir.create('processing/data/processed_data/wd_building_centroids')
log_errors <- tibble()
for(i in 1:length(wd_buildings)){
    
    disctrict_name <- str_split_fixed(wd_buildings[i], '/', n = Inf)[,6]
    disctrict_name <- str_remove(disctrict_name, '[.]geojson')
    disctrict_path <- wd_buildings[i]
    
    this_boundary <- read_sf(disctrict_path)
    
    this_boundary_centroid <- try(this_boundary %>%
        mutate(geometry = st_centroid(geometry)))
    
    if(inherits(this_boundary_centroid, 'try-error')){
        
        this_boundary_centroid <- try(this_boundary %>%
                                          mutate(geometry = st_make_valid(geometry)) %>%
                                          mutate(geometry = st_centroid(geometry)))
        
        if(inherits(this_boundary_centroid, 'try-error')){
            error_tib <- tibble(pwsid = disctrict_name,
                                status = 'error')
            
            log_errors <- rbind(log_errors, error_tib)
            next
        }
    }
    
    new_path <- str_replace(disctrict_path, 'boundary_building', 'wd_building_centroids')
    sf::st_write(this_boundary_centroid, new_path,
                 delete_dsn = T)
    
    sucesses_tib <- tibble(pwsid = disctrict_name,
                           status = 'saved')
    
    log_errors <- rbind(log_errors, sucesses_tib)
    
}

#### Parcel data ####

# read in parcel data
parcle_layer <- read_sf('processing/data/input_data/NC_Parcels_all.gdb')

# Read in NC boundaries 
nc_boundaries <- read_sf('processing/data/processed_data/nc_boundaries.geojson')
boundary_codes <- nc_boundaries$pwsid

nc_boundaries <- nc_boundaries %>%
    st_transform(., st_crs(parcle_layer))

dir.create('processing/data/processed_data/wd_parcel_centroids')
dir.create('processing/data/processed_data/boundary_parcels')
log_errors_subset_parcels <- tibble()
log_errors_centroids <- tibble()
for(i in 1:length(boundary_codes)){
    
    distric_name <- boundary_codes[i]
    
    this_boundary <- nc_boundaries[i,]
    
    boundary_parcels <- try(sf::st_filter(parcle_layer, this_boundary))
    
    if(inherits(boundary_parcels, 'try-error')){
        error_tib <- tibble(pwsid = disctrict_name,
                            status = 'error')
        
        log_errors_subset_parcels <- rbind(log_errors_subset_parcels, error_tib)
        next
    }
    
    file_name <- paste0('processing/data/processed_data/wd_parcel_centroids/', boundary_codes[i], '.geojson')
    
    boundary_parcels <- st_transform(boundary_parcels, 4326)

    sf::st_write(boundary_parcels, file_name,
                 delete_dsn = T)
    sucesses_tib <- tibble(pwsid = disctrict_name,
                           status = 'saved')
    
    log_errors_subset_parcels <- rbind(log_errors_subset_parcels, sucesses_tib)
}

# Save boundaries and individual files 
nc_boundaries <- read_sf('processing/data/processed_data/nc_boundaries.geojson')
dir.create('processing/data/processed_data/wd_boundaries')

for(i in 1:nrow(nc_boundaries)) {
    this_boundary <- nc_boundaries[i,]
    boundary_name <- this_boundary$pwsid
    
    write_sf(this_boundary, paste0('processing/data/processed_data/wd_boundaries/', boundary_name, '.geojson'))
}

