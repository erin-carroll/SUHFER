library(ggplot2)
library(lubridate)
library(stringr)

# TaylorPark_DataInc2022_ASCC is a cleaned version of TaylorPark_DataInc5_ASCC_CSE?
dat22 = read.csv('data/COOP/TaylorPark_DataInc2022_ASCC_plot.csv')
# unique ID is STANDPLOT_ID?
# so no repeat visits logged?
# how many unique lat lons are there?
# 112, so basically unique. This shows no repeat visits.

tmp = dat22 %>%
  mutate(latlon=paste(LATITUDE, LONGITUDE))
length(unique(tmp$latlon))

# do the 2023, 2024 datasets show evidence of any repeat visits?

# USE THE TREE, NOT PLOT DATA FOR 23, 24
dat23 = read.csv('data/COOP/TaylorPark_TreeData2023_ASCC_plot.csv') %>%
  filter(ID=='')  %>% # not sure what's going on in the first 3 rows, remove
  mutate(STANDPLOT_ID = paste0(STAND_ID, '_P', str_pad(PLOT_ID, width=3, pad='0')))
# are any of these in dat22
tmp = dat23 %>%
  filter(STANDPLOT_ID %in% dat22$STANDPLOT_ID)
# still only 3, same as when started with plot xlsx... this is dumb?

# check 2024, then go back to spatial proximity approach
dat24 = read.csv('data/COOP/TaylorPark_TreeData2024_ASCC_tree.csv') %>%
  filter(ID=='')  %>% # not sure what's going on in the first 3 rows, remove
  mutate(STANDPLOT_ID = paste0(STAND_ID, '_P', str_pad(PLOT_ID, width=3, pad='0')))
# are any of these in dat22
tmp = dat24 %>%
  filter(STANDPLOT_ID %in% dat22$STANDPLOT_ID) #again, 3 plots visited more than once? Makes no sense

# reformat dat23, dat24 to open in arc
str(dat23)
dat23 = dat23 %>%
  mutate(utm_zone = str_sub(LATITUDE, 1, 4),
         LATITUDE = as.numeric(str_sub(LATITUDE, 6, -1)),
         LONGITUDE = as.numeric(LONGITUDE))
write.csv(dat23, 'data/COOP/TaylorPark_TreeData2023_ASCC_plot.csv', row.names=F)

str(dat23)
