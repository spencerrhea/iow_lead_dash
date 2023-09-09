# Scrap yard 
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
# all_addresses <- all_addresses %>%
#     distinct(Full_Address, .keep_all = TRUE)

# st_write(all_addresses, 
#          'processing/data/input_data/AddressNC_dupremoved.geojson')
# 
# 
# names_to_keep <- c('OBJECTID', 'YEAR_BUILT', 'OCCUP_TYPE', 'BUILD_TYPE', 'parno', 'ownname', 'mailadd', 'munit',
#                    'mcity', 'mzip', 'siteadd', 'sunit', 'scity', 'legdecfull', 'parusecode', 'parusedesc', 'parusecd2',
#                    'ownfrst', 'ownlast', 'ownname2')


#### Make inissial log ####
# boundires_buildings_held <- list.files('shinny_app/data/building_address_join_pretty/')
# boundires_buildings_held <- str_split_fixed(boundires_buildings_held, '[.]', n = Inf)[,1]
# 
# boundires_not_held <- boundaries$pwsid[! boundaries$pwsid %in% boundires_buildings_held]
# boundires_held <- boundaries$pwsid[boundaries$pwsid %in% boundires_buildings_held]
# 
# boundaries_dates <- boundaries %>%
#     as.data.frame() %>%
#     select(pwsid, source_date, contribution_date)
# 
# log_file <- tibble(pwsid = c(boundires_held, boundires_not_held),
#                    status = c(rep('held', length(boundires_held)),
#                               rep('processing_error', length(boundires_not_held)))) %>%
#     left_join(boundaries_dates)
# 
# write_csv(log_file, 'logs/boundary_logging_file.csv')