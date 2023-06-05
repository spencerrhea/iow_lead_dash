# /home/sr446/git/iow_lead_pipe_rule/shinny_app (copy)/data/wd_boundaries/NC0106117.geojson

look <- st_read('data/wd_boundaries/NC0106117.geojson') %>%
    st_transform(., crs = 4326)

st_write(look, 'data/wd_boundaries/NC0106117.geojson', delete_dsn = T)
