library(sf)
library(terra)
library(ggplot2)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER/')

# load cse gpkg
cse = st_read('data/BullCSE2018/BullCSE2018.gpkg')

# load 2018 pct aspen coverage raster
aspen2018 = rast('data/aspen_cover_predictions/prediction_aspencover_032923_mean_2018.tif')

# extract aspen2018 valeus to cse points
vals = extract(aspen2018, cse)
cse$pred_aspen2018 = vals$prediction_aspencover_032923_mean_2018
  
ggplot(data=cse, aes(x=pct_potr5, y=pred_aspen2018)) +
  geom_point() +
  geom_smooth(method='lm')

# absolutely no linear relationship
mod = lm(pred_aspen2018~pct_potr5, data=cse)
summary(mod)

# will need to look at it in a map...