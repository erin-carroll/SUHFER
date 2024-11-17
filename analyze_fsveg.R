library(sf)
library(terra)
library(ggplot2)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER/')

########################################
# FSVEG - join all predictions to polygons
########################################

fsveg = st_read('data/FSVegSpatial2Feb2021_AOI/FSVegSpatial2Feb2021_AOI.shp') %>%
  st_transform(crs=4326)

fps = list.files('data/predictions', full.names=T)

for (fp in fps){
  r = rast(fp)
  vals = terra::extract(r, fsveg, fun='mean', na.rm=T, touches=T)
  fsveg$val = vals[,2]
  colnames(fsveg)[length(colnames(fsveg))] = names(r)[1]
}

# 



########################################
# FSVEG - aspen cover validation
########################################

## fsveg polys!
cover_types = read.csv('data/FSVegSpatial2Feb2021_AOI/cover_types.csv') %>%
  rename(cover_type_description=description)
fsveg = st_read('data/FSVegSpatial2Feb2021_AOI/FSVegSpatial2Feb2021_AOI.shp') %>%
  st_transform(crs=4326) %>%
  left_join(cover_types, by=c('COVER_TYPE'='cover_type'))

colnames(fsveg)
unique(fsveg$COVER_TYPE)
unique(fsveg$cover_type_description)

# what I actually want to do is extract predicted aspen values to these polygons, and see how the predicted values are distributed across cover types
vals = extract(aspen2018, fsveg, fun='mean', na.rm=T, touches=T)
fsveg$pred_aspen2018_mean = vals$prediction_aspencover_032923_mean_2018

mean_values <- data.frame(fsveg) %>%
  group_by(cover_type_description) %>%
  summarize(mean_value = mean(pred_aspen2018_mean)) %>%
  arrange(desc(mean_value))  # Sort by the mean value
fsveg = fsveg %>%
  mutate(cover_type_description = factor(cover_type_description, levels=mean_values$cover_type_description))


ggplot(data=fsveg) +
  geom_boxplot(aes(x=cover_type_description, y=pred_aspen2018_mean)) +
  theme(axis.text.x=element_text(angle=45, hjust=1))


ggplot(data=fsveg %>% filter(COVER_TYPE=='TAA')) +
  geom_point(aes(x=pred_aspen2018_mean, y=TREE_COVER)) # DLF_COVER? MLF_SPP_COVER? none of it looks great
## to think more about this later

# other potentiall useful fields
summary(fsveg$CANOPY_CLO) # canopy closer + species could be useful for NDVI estimates?
summary(fsveg$STAND_COND)
