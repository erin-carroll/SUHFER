library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(lubridate)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER/')

dat = read.csv('data/SBEADMR/Overstory.csv')

# how many plots, how many times visited?
tmp = dat %>%
  group_by(PLOT_ID) %>%
  summarize(n=n_distinct(Year))
tmp %>% filter(n>1) %>% nrow()
# only 53 of 163 plots visited more than once. Stick with just those? Nah keep all...

# with this stuff I'll be able to do the same pct dead/species metrics. Yay!

shp = st_as_sf(dat, coords=c('UTM_E', 'UTM_N'), crs=32613)
st_write(shp, 'data/AOI/new_AOI_sbeadmr.gpkg')

plot(st_geometry(shp))
