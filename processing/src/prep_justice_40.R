library(sf)
library(tidyverse)

# Read in justice 40 data
justice40 <- st_read('processing/data/input_data/usa')
col_names <- st_read('processing/data/input_data/justice_40_columns.csv')

# Columns to keep
dis_cols <- col_names[30:37,]
dis_cols$column_name <- str_split_fixed(dis_cols$column_name, ' [(]', n = Inf)[,1]

# Filter for NC
justice_nc <- justice40 %>%
    filter(SF == 'North Carolina') %>%
    select(CF, col_names$shapefile_column[30:37]) %>%
    # Add all justice 40 factors 
    mutate(tot_dis = N_WTR+N_WKFC+N_CLT+N_ENY+N_TRN+N_HSG+N_PLN+N_HLTH) 

no_geo <- justice_nc %>%
    as.data.frame() %>%
    select(-geometry)
    
for(i in 1:nrow(justice_nc)){
    
    dis_groups <- names(no_geo[i,])[no_geo[i,] == 1]
    
    if(length(dis_groups) == 0){
        dis_groups_desc <- 'None'
    } else{
        dis_groups_desc <- dis_cols %>%
            filter(shapefile_column %in% !!dis_groups) %>%
            pull(column_name)
    }
    
    justice_nc[i,'dis_factors'] <- paste(dis_groups_desc, collapse = ', ')
}

col_pal <- tibble(cols = c("#FFFFFF", tmaptools::get_brewer_pal("Blues", n = 8, plot = F)),
                  tot_dis = 0:8)

justice_nc <- left_join(justice_nc, col_pal)

st_write(justice_nc, 'processing/data/processed_data/justice_40_nc.geojson', delete_dsn = T)
st_write(justice_nc, 'shinny_app/data/justice_40_nc.geojson', delete_dsn = T)
