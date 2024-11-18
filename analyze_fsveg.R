library(sf)
sf_use_s2(FALSE)
library(terra)
library(ggplot2)
library(dplyr)
library(tidyverse)

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

# write.csv(fsveg_long, 'data/FSVegSpatial2Feb2021_AOI/fsveg_long.csv')

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
aoi = st_read('data/AOI/AOI.shp')
fsveg = st_read('data/FSVegSpatial2Feb2021_AOI/FSVegSpatial2Feb2021_predictions.gpkg')
fsveg_long = read.csv('data/FSVegSpatial2Feb2021_AOI/fsveg_long.csv')
fire = st_read('data/GMUG_Activities/GMUG_FirePerimeters.shp') %>%
  st_transform(crs=4326) #%>%
  # filter(st_intersects(geometry, aoi) %>% lengths() > 0)
hazFuels = st_read('data/GMUG_Activities/GMUG_HazFuelsTrts.shp') %>%
  st_transform(crs=4326) %>%
  filter(st_intersects(geometry, aoi) %>% lengths() > 0)
silvReforestation = st_read('data/GMUG_Activities/GMUG_SilvReforestation.shp') %>%
  st_transform(crs=4326) %>%
  filter(st_intersects(geometry, aoi) %>% lengths() > 0)
timberHarvest = st_read('data/GMUG_Activities/GMUG_TimberHarvest.shp') %>%
  st_transform(crs=4326) %>%
  # filter(st_intersects(geometry, aoi) %>% lengths() > 0)%>%
  mutate(FY_COMPLET = as.integer(FY_COMPLET))

# this analysis might actually make more sense at the pixel-scale? Given the limited sample size of events like fire within the relevant time scale
# largely because fire polygons in particular often do not cover the entire polygon! Only small chunks. So absolutely should do this at the pixel-level
# and in that case, the plots would be per-fire, not per-fsveg polygon?
# what about the other treatment types - do those happen at the fsveg polygon scale?
# there are plenty of timber cuts, and they appear to happen more at the fsveg polygon scale. But would still make sense to do it on a per-treatment, pixel basis.
# would make sense to me to just do timber, fire to start. We're looking for evidence that our values are tracking something real and useful on the ground...

# there are ZERO examples of clear cutting that or similar treatment types deployed in current AOI... Seems like a better
# plan would be to identify the fire, clear cuts that happen ANYWHERE in the GMUG from 2018-2023, to see if we are capturing them.
# generate new imagery and then do the analysis on a per-area basis.
# I'll gather all the new imagery at one time. So I should (1) identify the fire/timber regions I will want imagery for. 
# and then (2) move on to figuring out the other validation datasets that we have; and then (3) getting imagery for both.
# do i also (2.5) want to see if in our demography datasets that are any clear cases of increasing mortality over the period 2020-2023? In which I could do some similar analyses, since still in the GMUG I think?

# there are 241 timberHarvest polygons, 12 fire polygons that could be useful to look at
timberHarvest = timberHarvest %>% 
  filter(FY_COMPLET>2017, TREATMENT_ %in% c("Stand Clearcutting(E", "Stand clearcutting (", "Patch clearcutting (", "Coppice cut (EA/RN/F")) %>% # my guess at which of these would be most obvious? But are they really what I think they are lol
  select(geometry)
fire = fire %>%
  filter(FIREYEAR>2017) %>%
  select(geometry)
# merge them
new_aoi = timberHarvest %>%
  rbind(fire) %>%
  filter(!st_intersects(geometry, aoi) %>% lengths() > 0) %>%
  st_write('data/AOI/new_AOI_fire_timber.gpkg')
# okay, will gather imagery for above when I get there. To see what we're capturing.

# sql note, disregard
# TREATMENT_ IN ('Stand Clearcutting(E', 'Stand clearcutting (', 'Patch clearcutting (', 'Coppice cut (EA/RN/F') and (FY_COMPLET > '2017')

# for timber, there are different kinds of treatments (TREATMENT_)
timberHarvest %>% filter(FY_COMPLET>2017) %>% pull(TREATMENT_) %>% unique()
# can get years from FY_COMPLET
hist(timberHarvest$FY_COMPLET)

# question 1 - how many of the above occur within our time window?
hist(fire$FIREYEAR)

# FIRE
for (i in 1:nrow(fsveg)){
  id = fsveg$unique_id[i]
  d = fsveg_long %>%
    filter(unique_id==id, var!='aspencover')
  
  fire_year = fire %>%
    filter(st_intersects(geometry, fsveg[i,]) %>% lengths() > 0) %>%
    filter(FIREYEAR>2014) %>%
    pull(FIREYEAR) %>%
    unique()
  
  if (length(fire_year)>0){
    gg = ggplot(d, aes(x=year, y=value)) +
      geom_point() +
      geom_smooth(method='lm') +
      facet_wrap(~var, scales='free_y') +
      labs(title=paste0(fsveg$SETTING_ID[i], ' - ', id)) +
      geom_vline(xintercept=fire_year[1], linetype='dashed', color='red')
    print(gg)
  }
}

# TIMBER
for (i in 1:nrow(fsveg)){
  id = fsveg$unique_id[i]
  d = fsveg_long %>%
    filter(unique_id==id, var!='aspencover')
  timber_year = timberHarvest %>%
    filter(st_intersects(geometry, fsveg[i,]) %>% lengths() > 0) %>%
    filter(FY_COMPLET>2017) %>%
    pull(FY_COMPLET) %>%
    unique()
  if (length(timber_year)>0){
    gg = ggplot(d, aes(x=year, y=value)) +
      geom_point() +
      geom_smooth(method='lm') +
      facet_wrap(~var, scales='free_y') +
      labs(title=paste0(fsveg$SETTING_ID[i], ' - ', id)) +
      geom_vline(xintercept=timber_year[1], linetype='dashed', color='green')
    print(gg)
  }
}

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
