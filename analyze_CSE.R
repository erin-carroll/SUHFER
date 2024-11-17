library(sf)
library(terra)
library(ggplot2)
library(tidyverse)
library(stringr)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER/')

# load spatial cse 
pts = st_read('data/BullCSE2018/BullCSE2018_pts.gpkg')
polys = st_read('data/BullCSE2018/BullCSE2018_polys.gpkg')

st_crs(pts)
st_crs(polys)

########################################
# aspen cover
########################################

# load 2018 pct aspen coverage raster
aspen2018 = rast('data/predictions/prediction_aspencover_032923_mean_2018.tif')

## pts

# extract aspen2018 values to cse points
vals = extract(aspen2018, pts)
pts$pred_aspen2018 = vals$prediction_aspencover_032923_mean_2018
  
ggplot(data=pts, aes(x=pct_potr5, y=pred_aspen2018)) +
  geom_point() +
  theme_bw() +
  labs(x='CSE points: fraction aspen', y='predicted fraction aspen 2018', title='Points')
  geom_smooth(method='lm')

# absolutely no linear relationship
mod = lm(pred_aspen2018~pct_potr5, data=cse)
summary(mod)

pts %>% filter(!is.na(pred_aspen2018)) %>% nrow() # n

## polys

# summarize aspen2018 values to cse polygons
vals = extract(aspen2018, polys, fun='sum', na.rm=T, touches=T)
polys$pred_aspen2018_sum = vals$prediction_aspencover_032923_mean_2018
vals = extract(aspen2018, polys, fun='mean', na.rm=T, touches=T)
polys$pred_aspen2018_mean = vals$prediction_aspencover_032923_mean_2018

ggplot(data=polys, aes(x=pct_potr5, y=pred_aspen2018_mean)) +
  geom_point() +
  theme_bw() +
  labs(x='CSE points: fraction aspen', y='predicted fraction aspen 2018') # , title='Polygons'

########################################
# mortality
########################################

# try to correlate temporal trend within the CSE polygons that we have

fps = list.files('data/predictions', full.names=T)

for (fp in fps){
  r = rast(fp)
  vals = terra::extract(r, polys, fun='mean', na.rm=T, touches=T)
  polys$val = vals[,2]
  colnames(polys)[length(colnames(polys))] = names(r)[1]
}

polys = polys %>%
  filter(!is.na(ndvi_2017))

# for each of the predicted variables, I want to calculate a polygon-level slope and significance

polys_long = polys %>%
  pivot_longer(cols=c(ndmi_2017:prediction_lwc_013024_mean_2023)) %>%
  mutate(year=as.numeric(str_sub(name, -4, -1))) %>%
  mutate(var = if_else(grepl('ndmi', name), 'ndmi', '')) %>%
  mutate(var = if_else(grepl('ndvi', name), 'ndvi', var),
         var = if_else(grepl('aspencover', name), 'aspencover', var),
         var = if_else(grepl('lwc', name), 'lwc', var))

ggplot(polys_long %>% filter(ORIGINAL_S=='0204056502000003'), aes(x=year, y=value)) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  facet_wrap(~var, scales='free_y')

for (i in 1:nrow(polys)){
  id = polys$ORIGINAL_S[i]
  print(id)
  # ndvi
  mod = lm(value~year, data=polys_long %>% filter(ORIGINAL_S==id, var=='ndvi'))
  polys$ndvi_trend[i] = mod$coefficients[2]
  polys$ndvi_p[i] = summary(mod)$coefficients[2,4]
  # ndmi
  mod = lm(value~year, data=polys_long %>% filter(ORIGINAL_S==id, var=='ndmi'))
  polys$ndmi_trend[i] = mod$coefficients[2]
  polys$ndmi_p[i] = summary(mod)$coefficients[2,4]
  # lwc
  mod = lm(value~year, data=polys_long %>% filter(ORIGINAL_S==id, var=='lwc'))
  polys$lwc_trend[i] = mod$coefficients[2]
  polys$lwc_p[i] = summary(mod)$coefficients[2,4]
}

# export to a shapefile
polys %>%
  select(ORIGINAL_S, n:lwc_p) %>%
  st_write('data/BullCSE2018/BullCSE2018_polys_predictions.gpkg', append=F) # export

# compare predicted trends with pct_mortality?
ggplot(polys) +
  geom_point(aes(x=pct_dead, y=ndvi_trend, color=ndvi_p)) +
  labs(x='CSE frac dead 2018', y='predicted trend 2017-2023', title='NDVI', color='p value')
ggplot(polys) +
  geom_point(aes(x=pct_dead, y=ndmi_trend, color=ndmi_p)) +
  labs(x='CSE frac dead 2018', y='predicted trend 2017-2023', title='NDMI', color='p value')
ggplot(polys) +
  geom_point(aes(x=pct_dead, y=lwc_trend, color=lwc_p)) +
  labs(x='CSE frac dead 2018', y='predicted trend 2017-2023', title='LWC', color='p value')




