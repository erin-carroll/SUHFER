library(dplyr)
library(sf)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER/')

cse = read.csv('data/BULLCSE2018/BullCSE2018.csv')
pts = st_read('data/BullCSE2018/GMUGCSEpts20002020')
polys = st_read('data/BullCSE2018/GMUGCSEpolys20002020')

##########################################
# pts
##########################################

# attach spatial data from pts to cse
pts = pts %>%
  select(DA_STANDPL, LATITUDE, LONGITUDE) # this is the setting_id (which joins to polygons) + the plot_id (which I am assuming joins to pts)
cse_pts = cse %>%
  filter(!is.na(TREE_TAG_ID)) %>%
  mutate(full_id = paste0('0', SETTING_ID, '_', sprintf('%0*d', 4, TREE_TAG_ID))) %>%
  left_join(pts, by=c('full_id'='DA_STANDPL'))

# okay making the (likely faulty) assumption for now that measurement_Date in cse is accurate (to clarify with Carlyn)
cse_pts = cse_pts %>%
  mutate(tree_dead = if_else(TREE_STATUS=='D', 1, 0),
         POTR5 = if_else(TREE_SPECIES=='POTR5', 1, 0), # aspen
         PIEN = if_else(TREE_SPECIES=='PIEN', 1, 0), # engelmann spruce (picea engelmanii)
         ABLA = if_else(TREE_SPECIES=='ABLA', 1, 0), # subalpine fir (abies lasiocarpa)
         PIPO = if_else(TREE_SPECIES=='PIPO', 1, 0), # ponderosa pine (pinus ponderosa)
         PSME = if_else(TREE_SPECIES=='PSME', 1, 0)) # doug fir (Pseudotsuga menziesii)
dat = cse_pts %>%
  group_by(full_id) %>%
  summarize(n=n(),
            n_dead = sum(tree_dead),
            n_potr5 = sum(POTR5), 
            n_pien = sum(PIEN),
            n_abla = sum(ABLA),
            n_pipo = sum(PIPO),
            n_psme = sum(PSME),
            sum_PLOT_BA = sum(PLOT_BA), # still not totally sure what these are... basal area, but are they fixed radius plots? # but why would there be multiple different BA, TPA values for each full_id??
            sum_PLOT_TPA = sum(PLOT_TPA), # tpa = trees per acre?
            lon = first(LONGITUDE),
            lat = first(LATITUDE)) %>% 
  mutate(pct_dead = n_dead/n,
         pct_potr5 = n_potr5/n,
         pct_pien = n_pien/n,
         pct_abla = n_abla/n,
         pct_pipo = n_pipo/n,
         pct_psme = n_psme/n,)

hist(dat$n) # n varies from 1-34?? Why so variable - still assuming single time point? Are these fixed radius plots?
hist(dat$n_dead)
hist(dat$pct_dead)
hist(dat$pct_potr5)
hist(dat$sum_PLOT_BA)

# assuming all from a single site visit in 2018
unique(cse$MEASUREMENT_DATE) # no way this is correct?

# export
dat_shp = dat %>%
  filter(!is.na(lat)) %>%
  st_as_sf(coords=c('lon', 'lat'), crs=4326)
st_write(dat_shp, 'data/BullCSE2018/BullCSE2018_pts.gpkg', append=F)

##########################################
# polygons
##########################################

# filter to polys with relevant setting_ids
setting_ids = paste0(0, unique(cse$SETTING_ID))
polys = polys %>%
  filter(ORIGINAL_S %in% setting_ids)
# We don't have a polygon for every setting_id included in CSE? But the question is, do we lose less data this way?
# ALSO Sample_D_1 does not match cse$MEASUREMENT_DATE!

# **Because I can also get a fraction dead, fraction species at the polygon level!

# summarize cse at the setting_id level
cse_polys = cse %>%
  mutate(SETTING_ID = paste0(0, SETTING_ID),
         tree_dead = if_else(TREE_STATUS=='D', 1, 0),
         POTR5 = if_else(TREE_SPECIES=='POTR5', 1, 0), # aspen
         PIEN = if_else(TREE_SPECIES=='PIEN', 1, 0), # engelmann spruce (picea engelmanii)
         ABLA = if_else(TREE_SPECIES=='ABLA', 1, 0), # subalpine fir (abies lasiocarpa)
         PIPO = if_else(TREE_SPECIES=='PIPO', 1, 0), # ponderosa pine (pinus ponderosa)
         PSME = if_else(TREE_SPECIES=='PSME', 1, 0)) %>% # doug fir (Pseudotsuga menziesii)
  group_by(SETTING_ID) %>%
  summarize(n=n(),
            n_dead = sum(tree_dead),
            n_potr5 = sum(POTR5), 
            n_pien = sum(PIEN),
            n_abla = sum(ABLA),
            n_pipo = sum(PIPO),
            n_psme = sum(PSME),
            sum_PLOT_BA = sum(PLOT_BA), # still not totally sure what these are... basal area, but are they fixed radius plots? # but why would there be multiple different BA, TPA values for each full_id??
            sum_PLOT_TPA = sum(PLOT_TPA)) %>%  # TPA trees per acre?
  mutate(pct_dead = n_dead/n,
         pct_potr5 = n_potr5/n,
         pct_pien = n_pien/n,
         pct_abla = n_abla/n,
         pct_pipo = n_pipo/n,
         pct_psme = n_psme/n,)

# join summarized data to polygons
polys_joined = polys %>%
  left_join(cse_polys, by = c('ORIGINAL_S'='SETTING_ID')) %>%
  st_write('data/BullCSE2018/BullCSE2018_polys.gpkg', append=F) # export

##########################################
# Joining to FIA data?
##########################################

# only 3 measurement dates...
unique(cse$MEASUREMENT_DATE)

# there are many tree_CNs per full_id - why? time series?
length(unique(cse$full_id))
length(unique(cse$TREE_CN))