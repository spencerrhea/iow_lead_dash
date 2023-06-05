library(sf)
library(tidyverse)


files_to_cent <- list.files('processing/data/processed_data/building_adress_join/',
                            full.names = TRUE)

for(i in 14:length(files_to_cent)){
    
    boundary_name <- str_split_fixed(files_to_cent[i], pattern = '/', n = Inf)[,6]
    
    this_set <- st_read(files_to_cent[i]) %>%
        mutate(geometry = st_centroid(geometry))
    
    write_sf(this_set, paste0('shinny_app/data/building_adress_join/', boundary_name))
}
