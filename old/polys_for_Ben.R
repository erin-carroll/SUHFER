library(sf)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER')

polys = st_read('data/fsveg_gmug.gpkg') %>%
  st_drop_geometry()
  
fsveg = st_read('data/FSVegSpatial2Feb2021.gpkg') %>%
  # st_transform(crs=st_crs(polys))
  st_drop_geometry() %>%
  select(STAND_ID, ASPECT_CLA, ELEVATION_, SLOPE_PERC)

df = polys %>%
  left_join(fsveg, by='STAND_ID')

tmp = df %>%
  group_by(STAND_ID) %>%
  summarize(n=n())

df <- df[!duplicated(df$STAND_ID), ]

fp = 'data/fsveg_gmug_topo.csv'
write.csv(df, fp, row.names=F)

unique(df$ASPECT_CLA)

cover = data.frame(COVER_TYPE = unique(df$COVER_TYPE))
write.csv(cover, 'data/fsveg_cover_types.csv', row.names=F)
