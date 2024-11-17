library(sf)
library(terra)
library(ggplot2)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER/')

########################################
# FSVEG - join all predictions to polygons
########################################

fsveg = st_read('data/FSVegSpatial2Feb2021_AOI/FSVegSpatial2Feb2021_AOI.shp') %>%
  st_transform(crs=4326)# %>%
fsveg = fsveg %>%
  mutate(unique_id = 1:nrow(fsveg))

fps = list.files('data/predictions', full.names=T)

for (fp in fps){
  r = rast(fp)
  vals = terra::extract(r, fsveg, fun='mean', na.rm=T, touches=T)
  fsveg$val = vals[,2]
  colnames(fsveg)[length(colnames(fsveg))] = names(r)[1]
}

fsveg_long = fsveg %>%
  pivot_longer(cols=c(ndmi_2017:prediction_lwc_013024_mean_2023)) %>%
  mutate(year=as.numeric(str_sub(name, -4, -1))) %>%
  mutate(var = if_else(grepl('ndmi', name), 'ndmi', '')) %>%
  mutate(var = if_else(grepl('ndvi', name), 'ndvi', var),
         var = if_else(grepl('aspencover', name), 'aspencover', var),
         var = if_else(grepl('lwc', name), 'lwc', var)) %>%
  filter(!is.na(value))

# test viz
ggplot(fsveg_long %>% filter(unique_id==unique(fsveg_long$unique_id)[1]), aes(x=year, y=value)) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  facet_wrap(~var, scales='free_y')

# why is there not a unique setting_id for each polygon in my fsveg table?
tmp = fsveg %>%
  group_by(SETTING_ID) %>%
  summarize(n=n())
# because 020406DR01020097 has 193 rows where every other ID only has one... why? 
# who knows. Just added my own unique_id

# assess trends
for (i in 1:nrow(fsveg)){
  id = fsveg$unique_id[i]
  print(id)
  # ndvi
  d = fsveg_long %>% filter(unique_id==id, var=='ndvi')
  if (nrow(d)>0){
    mod = lm(value~year, data=d)
    fsveg$ndvi_trend[i] = mod$coefficients[2]
    fsveg$ndvi_p[i] = summary(mod)$coefficients[2,4]
  }
  # ndmi
  d = fsveg_long %>% filter(unique_id==id, var=='ndmi')
  if (nrow(d)>0){
    mod = lm(value~year, data=d)
    fsveg$ndmi_trend[i] = mod$coefficients[2]
    fsveg$ndmi_p[i] = summary(mod)$coefficients[2,4]
  }
  # lwc
  d = fsveg_long %>% filter(unique_id==id, var=='lwc')
  if (nrow(d)>0){
    mod = lm(value~year, data=d)
    fsveg$lwc_trend[i] = mod$coefficients[2]
    fsveg$lwc_p[i] = summary(mod)$coefficients[2,4] 
  }
}

fsveg %>%
  st_write('data/FSVegSpatial2Feb2021_AOI/FSVegSpatial2Feb2021_predictions.gpkg', append=F) # export

# just checking that it looks right
tmp = st_read('data/FSVegSpatial2Feb2021_AOI/FSVegSpatial2Feb2021_predictions.gpkg')
# okay - for how many polygons do we have a significant, negative trend in all ndmi + ndvi + lwc?
sig = 0.1
check = tmp %>% # this actually gives us 0 polygons lol
  filter(ndvi_trend<0, ndmi_trend<0, lwc_trend<0,
         ndvi_p<sig, ndmi_p<sig, lwc_p<sig)
check = tmp %>% 
  filter(ndvi_trend<0, ndmi_trend<0, ndvi_p<sig, ndmi_p<sig) # this only gives us 4 rows... They must be doing quite poorly?

# what about just where they're all negative? p values are tricky with an n of 7?
check = tmp %>% 
  filter(ndvi_trend<0, ndmi_trend<0, lwc_trend<0) # this gives many rows! About 12% of all

########################################
# FSVEG - gmug activity validation
########################################
fire = st_read('data/GMUG_Activities/GMUG_FirePerimeters.shp')
hazFuels = st_read('data/GMUG_Activities/GMUG_HazFuelsTrts.shp')
silvReforestation = st_read('data/GMUG_Activities/GMUG_SilvReforestation.shp')
timberHarvest = st_read('data/GMUG_Activities/GMUG_TimberHarvest.shp')


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
