# Terrible coding
library(tidyverse)

# Process file with codes 
building_codes <- read.delim('processing/data/input_data/Export_Output.txt')

colnames(building_codes)

look <- str_split(building_codes[2,], '[{]')

## OCCUP_TYPE
which(str_detect(look[[1]], 'OCCUP_TYPE'))

look[[1]][str_detect(look[[1]], 'OCCUP_TYPE')]

which(str_detect(look[[1]], 'type'))

look[[1]][15:547][3:532]

test <- str_remove_all(look[[1]][15:547][3:532], '\"name\":\"')
test <- str_remove_all(test, ':')
test <- str_remove_all(test, '"')
test <- str_remove_all(test, '[}]')
test <- str_remove_all(test, 'code')
test <- str_remove_all(test, '[]]')

almost_there <- str_split_fixed(test, ',', n = Inf)
sep_uses <- str_split_fixed(almost_there[,1], ' - ', n = Inf)

OCCUP_key <- tibble(occupancy_type = sep_uses[,1],
                    occupancy_source = sep_uses[,2],
                    occupancy_confidence = sep_uses[,3],
                    OCCUP_TYPE = almost_there[,2])

write_csv(OCCUP_key, 'processing/data/processed_data/biulding_occup_key.csv')



## NUM_STORY
which(str_detect(look[[1]], 'NUM_STORY'))

look[[1]][str_detect(look[[1]], 'NUM_STORY')]

which(str_detect(look[[1]], 'type'))

test <- str_remove_all(look[[1]][780:848][3:68], '\"name\":\"')
test <- str_remove_all(test, ':')
test <- str_remove_all(test, '"')
test <- str_remove_all(test, '[}]')
test <- str_remove_all(test, 'code')
test <- str_remove_all(test, '[]]')

almost_there <- str_split_fixed(test, ',', n = Inf)
sep_uses <- str_split_fixed(almost_there[,1], ' - ', n = Inf)

floor_key <- tibble(num_stories = sep_uses[,1],
                    num_stories_source = sep_uses[,2],
                    num_stories_confidence = sep_uses[,3],
                    NUM_STORY = almost_there[,2])

write_csv(floor_key, 'processing/data/processed_data/biulding_stories_key.csv')

## NUM_UNITS
which(str_detect(look[[1]], 'NUM_UNITS'))

look[[1]][str_detect(look[[1]], 'NUM_UNITS')]

which(str_detect(look[[1]], 'type'))

test <- str_remove_all(look[[1]][848:868][3:20], '\"name\":\"')
test <- str_remove_all(test, ':')
test <- str_remove_all(test, '"')
test <- str_remove_all(test, '[}]')
test <- str_remove_all(test, 'code')
test <- str_remove_all(test, '[]]')

almost_there <- str_split_fixed(test, ',', n = Inf)
sep_uses <- str_split_fixed(almost_there[,1], ' - ', n = Inf)

units_key <- tibble(num_units = sep_uses[,1],
                    num_units_source = sep_uses[,2],
                    num_units_confidence = sep_uses[,3],
                    NUM_UNITS = almost_there[,2])

write_csv(units_key, 'processing/data/processed_data/biulding_units_key.csv')

## BUILD_TYPE
which(str_detect(look[[1]], 'BUILD_TYPE'))

look[[1]][str_detect(look[[1]], 'BUILD_TYPE')]

which(str_detect(look[[1]], 'type'))

test <- str_remove_all(look[[1]][547:591][3:44], '\"name\":\"')
test <- str_remove_all(test, ':')
test <- str_remove_all(test, '"')
test <- str_remove_all(test, '[}]')
test <- str_remove_all(test, 'code')
test <- str_remove_all(test, '[]]')

almost_there <- str_split_fixed(test, ',', n = Inf)
sep_uses <- str_split_fixed(almost_there[,1], ' - ', n = Inf)

build_key <- tibble(build_type = sep_uses[,1],
                    build_type_source = sep_uses[,2],
                    build_type_confidence = sep_uses[,3],
                    BUILD_TYPE = almost_there[,2])

write_csv(build_key, 'processing/data/processed_data/biulding_build_key.csv')

## FL_SCHEME
which(str_detect(look[[1]], 'FL_SCHEME'))

look[[1]][str_detect(look[[1]], 'FL_SCHEME')]

which(str_detect(look[[1]], 'type'))

test <- str_remove_all(look[[1]][592:597][2:5], '\"name\":\"')
test <- str_remove_all(test, ':')
test <- str_remove_all(test, '"')
test <- str_remove_all(test, '[}]')
test <- str_remove_all(test, 'code')
test <- str_remove_all(test, '[]]')

almost_there <- str_split_fixed(test, ',', n = Inf)
sep_uses <- str_split_fixed(almost_there[,1], ' - ', n = Inf)

flood_scheme_key <- tibble(flood_scheme_type = sep_uses[,1],
                           FL_SCHEME = c('1000', '1010', 'NA', 'NP'))

write_csv(flood_scheme_key, 'processing/data/processed_data/biulding_flood_scheme_key.csv')

## STATIC_BFE
which(str_detect(look[[1]], 'FLD_ZONE'))

look[[1]][str_detect(look[[1]], 'FL_SCHEME')]

which(str_detect(look[[1]], 'type'))

test <- str_remove_all(look[[1]][621:638], '\"name\":\"')
test <- str_remove_all(test, ':')
test <- str_remove_all(test, '"')
test <- str_remove_all(test, '[}]')
test <- str_remove_all(test, 'code')
test <- str_remove_all(test, '[]]')

almost_there <- str_split_fixed(test, ',', n = Inf)
sep_uses <- str_split_fixed(almost_there[,1], ' - ', n = Inf)

flood_zone_key <- tibble(flood_zone_type = sep_uses[,1],
                           flood_zone_source = sep_uses[,2],
                           flood_zone_confidence = sep_uses[,3],
                           STATIC_BFE = almost_there[,2])

write_csv(flood_zone_key, 'processing/data/processed_data/biulding_flood_zone_key.csv')




