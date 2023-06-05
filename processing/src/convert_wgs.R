library(sf)
library(tidyverse)

boundary_files <- list.files('processing/data/processed_data/wd_boundaries/', full.names = T)

transform_to_wgs <- function(file_path){
    this_boundary <- st_read(file_path) %>%
        st_transform(., crs = 4326)
    
    st_write(this_boundary, file_path, delete_dsn = TRUE)
}

map(boundary_files, transform_to_wgs)


#### Move Files ####
# Buildings 
file.remove(list.files('shinny_app/data/building_address_join_pretty/', full.names = T))

fils_names <- list.files('processing/data/processed_data/building_address_join_pretty/')
fils_path <- list.files('processing/data/processed_data/building_address_join_pretty/',
                        full.names = TRUE)

new_path <- paste0('shinny_app/data/building_address_join_pretty/', fils_names)

dir.create('shinny_app/data/building_address_join_pretty')


file.copy(from = fils_path, to = new_path)

# boundaries 
file.remove(list.files('shinny_app/data/wd_boundaries/', full.names = TRUE))
fils_names_bound <- list.files('processing/data/processed_data/wd_boundaries/')
fils_path_bound <- list.files('processing/data/processed_data/wd_boundaries/',
                        full.names = TRUE)

fils_names_bound_move <- fils_names_bound[fils_names_bound %in% fils_names]

fils_path_bound <- fils_path_bound[fils_names_bound %in% fils_names]
new_path_bound <- paste0('shinny_app/data/wd_boundaries/', fils_names_bound_move)

dir.create('shinny_app/data/wd_boundaries')

file.copy(from = fils_path_bound, to = new_path_bound)


# look <- st_read('processing/data/processed_data/building_address_join_pretty/043740039.geojson')
# names(look)
