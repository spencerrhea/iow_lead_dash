all_buildings <- st_read('processing/data/input_data/NC_Buildings_Footprints_(2010).geojson')

with_name <- read_csv('processing/data/input_data/NC_Buildings_Footprints_(2010)_withnames.csv')

ids_in_durham <- with_name$OBJECTID

durham_buildings <- all_buildings %>%
    filter(OBJECTID %in% !!ids_in_durham)

names_to_codes <- full_join(durham_buildings, with_name, by = 'OBJECTID')

OCCUP_TYPE_key <- names_to_codes %>%
    as.data.frame() %>%
    select(OCCUP_TYPE.x, OCCUP_TYPE.y) %>%
    distinct()

durham_codes <- read_csv('processing/data/input_data/durham_with_codes.csv')

colnames(durham_codes) <- colnames(durham_buildings)[2:31]

durham_codes <- durham_codes %>%
    mutate(BLDG_ID = as.character(BLDG_ID))

OCCUP_TYPE_key <- left_join(durham_codes, durham_buildings, by = 'BLDG_ID')
OCCUP_TYPE_key <- OCCUP_TYPE_key %>%
    as.data.frame() %>%
    select(OCCUP_TYPE.x, OCCUP_TYPE.y) %>%
    distinct()

OCCUP_TYPE_key <- OCCUP_TYPE_key %>%
    rename(occup_name = OCCUP_TYPE.x,
           OCCUP_TYPE = OCCUP_TYPE.y)

all_buildings_names <- left_join(all_buildings, OCCUP_TYPE_key)

check <- all_buildings_names %>%
    as.data.frame() %>%
    filter(is.na(occup_name))

unique(check$OCCUP_TYPE)
