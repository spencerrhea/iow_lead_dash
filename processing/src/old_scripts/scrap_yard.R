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
