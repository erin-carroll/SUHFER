library(dplyr)
library(sf)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER/')

cse = read.csv('data/BULLCSE2018/BullCSE2018.csv')
pts = st_read('data/BullCSE2018/GMUGCSEpts20002020')
polys = st_read('data/BullCSE2018/GMUGCSEpolys20002020')

# attach spatial data from pts to cse
pts = pts %>%
  mutate(full_id = paste0(SETTING_ID, '_', PLOT_ID)) %>%
  select(full_id, LATITUDE, LONGITUDE)
cse = cse %>%
  filter(!is.na(TREE_TAG_ID)) %>%
  mutate(full_id = paste0('0', SETTING_ID, '_', TREE_TAG_ID)) %>%
  left_join(pts, by='full_id')

# only 3 measurement dates...
unique(cse$MEASUREMENT_DATE)

# there are many tree_CNs per full_id
length(unique(cse$full_id))
length(unique(cse$TREE_CN))

# filter to polys with relevant setting_ids
setting_ids = paste0(0, unique(cse$SETTING_ID))
polys = polys %>%
  filter(ORIGINAL_S %in% setting_ids)
# We don't have a polygon for every setting_id included in CSE? 
# ALSO Sample_D_1 does not match cse$MEASUREMENT_DATE

# okay making the (likely faulty) assumption for now that measurement_Date in cse is accurate (to clarify with Carlyn)
cse = cse %>%
  mutate(tree_dead = if_else(TREE_STATUS=='D', 1, 0),
         POTR5 = if_else(TREE_SPECIES=='POTR5', 1, 0), # aspen
         PIEN = if_else(TREE_SPECIES=='PIEN', 1, 0), # engelmann spruce (picea engelmanii)
         ABLA = if_else(TREE_SPECIES=='ABLA', 1, 0), # subalpine fir (abies lasiocarpa)
         PIPO = if_else(TREE_SPECIES=='PIPO', 1, 0), # ponderosa pine (pinus ponderosa)
         PSME = if_else(TREE_SPECIES=='PSME', 1, 0),) # doug fir (Pseudotsuga menziesii)
dat = cse %>%
  group_by(full_id) %>%
  summarize(n=n(),
            n_dead = sum(tree_dead),
            n_potr5 = sum(POTR5), 
            n_pien = sum(PIEN),
            n_abla = sum(ABLA),
            n_pipo = sum(PIPO),
            n_psme = sum(PSME),
            sum_PLOT_BA = sum(PLOT_BA), # still not totally sure what these are... assuming this is basal area. Are they fixed radius plots? # but why would there be multiple different BA, TPA values for each full_id??
            sum_PLOT_TPA = sum(PLOT_TPA)) %>% # tpa = trees per acre?
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

# export to shapefile! for visualizations














# figuring out FIA COND data
cond = read.csv("C:/Users/erinc/Desktop/Research/Projects/SUHFER/data/FIA/CO_COND.csv")
tmp = cond %>%
  group_by(PLT_CN) %>%
  summarize(n_condCN = n_distinct(CN),
            n_years = n_distinct(INVYR))
# each plt_CN has more than one cond_CN, but only one invyr??

tmp = cond %>%
  group_by(CN) %>%
  summarize(n_condCN = n_distinct(PLT_CN),
            n_years = n_distinct(INVYR))

tmp2 = cond %>%
  filter(PLT_CN == tmp$PLT_CN[1])


plot = read.csv("C:/Users/erinc/Desktop/Research/Projects/SUHFER/data/FIA/CO_PLOT.csv")

tmp = plot %>% 
  group_by(CN) %>%
  summarize(years = n_distinct(INVYR))

# doesn't seem like any plots were visited more than once? As best I can tell?
# in all of Colorado??

tree = read.csv("C:/Users/erinc/Desktop/Research/Projects/SUHFER/data/FIA/CO_TREE.csv")
colnames(tree)

tmp = tree %>%
  group_by(CN) %>%
  summarize(years = n_distinct(INVYR))

unique(tree$INVYR)





# sbeadmr
sbeadmr = read.csv("C:/Users/erinc/Desktop/Research/Projects/SUHFER/data/SBEADMR/Overstory.csv")

length(unique(sbeadmr$PLOT_ID))

sort(unique(sbeadmr$Year))

# were plots visited more than once?

tmp = sbeadmr %>%
  group_by(PLOT_ID) %>%
  summarize(years = n_distinct(Year)) %>%
  filter(years > 1)