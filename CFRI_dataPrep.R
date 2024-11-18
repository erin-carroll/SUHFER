library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(lubridate)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER/')

list.files("data/UP-CFRLP/")

# regen = read.csv("data/UP-CFRLP/RegenerationMaster20240216.csv")
ra = read.csv("data/UP-CFRLP/RA_data_Master_20240305_overstory.csv")
ra_shp = st_read('data/UP-CFRLP/RAplots/RAplots.shp') %>%
  st_transform(crs=4326)

#################################################
# RA plots
#################################################

length(unique(ra_shp$PlotCode)) # unique identifier
unique(ra_shp$TreatmentT) # there is plot-level treatment data - thinned, burned, control
# most sites visited twice, a handful visited 3x, a few visited 4x

# filter to those we have spatial data for
ra = ra %>%
  filter(PLOT %in% unique(ra_shp$PlotCode)) %>%
  mutate(Status=toupper(Status),
         Spp=toupper(Spp),
         DATE = as.Date(parse_date_time(DATE, c('ymd', 'mdy')))) %>%
  filter(Status!='.') %>%
  mutate(dead_or_down=if_else(Status %in% c('D', 'X', 'Y'), 1, 0), 
         live=if_else(Status=='L', 1, 0),
         stump=if_else(Status=='S', 1, 0),
         conifer=if_else(Spp %in% c('PIPU', 'PIPO', 'ABLA', 'PIEN', 'PSME', 'PIXX', 'UNKFIR', 'UNKFL'), 1, 0),
         oak=if_else(Spp=='QUGA', 1, 0),
         aspen=if_else(Spp=='POTR', 1, 0))
## could also calculate basal area metrics from dbh if properly motivated (e.g. live vs. dead basal area, or change in live basal area over time)

plots = ra %>%
  group_by(PLOT, DATE) %>%
  summarize(n_trees=n(),
            n_dead_or_down=sum(dead_or_down),
            n_live=sum(live),
            n_stump=sum(stump),
            n_conifer=sum(conifer),
            n_oak=sum(oak),
            n_aspen=sum(aspen)) %>%
  mutate(pct_dead_or_down=n_dead_or_down/n_trees,
         pct_live=n_live/n_trees,
         pct_stump=n_stump/n_trees,
         pct_conifer=n_conifer/n_trees,
         pct_oak=n_oak/n_trees,
         pct_aspen=n_aspen/n_trees)

for (plot in unique(plots$PLOT)){
  d = plots %>% filter(PLOT==plot)
  if(nrow(d)>1){
    gg = ggplot(d) +
      geom_point(aes(x=DATE, y=pct_live)) +
      labs(title=plot)
    print(gg)
  }
}



# eventually we will want to know how the trend predicted by the model relates to the trend predicted by this plot-level data
# will largely want to visualize. Per plot - this plot-level data + s2 time series, and a vline marking the relevant treatments
# but! next step is to keep collecting the relevant S2 imagery

# filter ra_shp to where we have relevant plot-level data (it's all of them)
aoi = ra_shp %>%
  filter(PlotCode %in% unique(plots$PLOT)) %>%
  st_write('data/AOI/new_AOI_CFRI_raplots.gpkg')

plot(st_geometry(aoi))
## will eventually have to go back and bound all the new AOIs in polygons. But can do that when gathering imagery





unique(ra$Status)
unique(ra$Spp)

tmp = ra %>%
  group_by(PLOT) %>%
  summarize(n=n(), visits=n_distinct(DATE))

# where visited more than once, I can get a trend from status?

plots = ra %>%
  group_by(PLOT, DATE) %>%
  summarize()
# pct stump
# pct dead
# pct of live/standing adult trees of each species
# 


#################################################
# activity polygons
#################################################

# there is also more activity data that is likely to be useful! In polygon form
polys = st_read('data/UP-CFRLP/UncompahgrePlateauPolygons/UncompahgrePlateauPolygons.shp') %>%
  st_transform(crs=4326) %>%
  mutate(across(any_of(c('YearComple', 'YearComp_1','YearComp_2','YearComp_3')), as.numeric)) %>%
  filter((YearComple>2017) | (YearComp_1>2017) | (YearComp_2>2017) | (YearComp_3>2017)) # at least one treatment occurred within the window we have imagery for (takes us down to 12)
# 12 left, two of which are burning (broadcast burn). So still might expect improvement, not decrease in metrics? Because not canopy fires?

# export
polys %>%
  st_write('data/AOI/new_AOI_CFRIactivities.gpkg')






sort(colnames(polys))
str(polys)

length(unique(ra$PLOT))

unique(ra_shp$PlotCode)


colnames(ra)

length(unique(ra$PLOT))

ra_sites = ra %>%
  group_by(PLOT) %>%
  summarize(n = n(),
            n_dates = n_distinct(DATE),
            n_spp = n_distinct(Spp)) %>%
  filter(n_dates > 1)

sites = unique(ra_sites$PLOT)

# limit to just plots that were visited more than once 
ra = ra %>%
  filter(PLOT %in% sites) %>%
  mutate(DBH_num = as.numeric(DBH))

ra = ra %>%
  mutate(live_overstory_basal_area = ifelse(Status=='L', pi*(DBH_num/2)^2, NA),
         adult_dead = ifelse(Status %in% c('D', 'S', 's', 'X'), 1, 0))

ra_sites = ra %>%
  group_by(PLOT, DATE) %>%
  summarize(live_overstory_basal_area = sum(live_overstory_basal_area, na.rm=T),
            n_adult_dead = sum(adult_dead, na.rm=T),
            n_species = n_distinct(Spp)) %>%
  mutate(year = str_extract(DATE, "[^/]+$")) %>%
  mutate(year = ifelse(nchar(year)==2, as.integer(paste0('20', year)), as.integer(year)))

ggplot(ra_sites %>% filter(PLOT=='L11')) +
  geom_point(aes(x=year, y=live_overstory_basal_area))

ggplot(ra_sites %>% filter(PLOT=='L11')) +
  geom_point(aes(x=year, y=n_adult_dead))

# site-level metrics that I could calculate:
# live basal area density?
# n adult dead
# see how those change between years?

## what size are these plots???

# okay this was good progress! I got some data together.