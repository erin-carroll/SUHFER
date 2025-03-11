library(dplyr)
library(sf)
library(lubridate)

hazfuels = st_read('data/GMUG_Activities/raw/GMUG_HazFuelsTrts.shp') %>%
  data.frame() %>%
  mutate(date_completed = parse_date_time(DATE_COMPL, orders=c('ymd'))) %>%
  filter(year(date_completed) > 2017) # filter to relevant dates completeds (2017-2023)

# filter to relevant activity types
sort(unique(hazfuels$ACTIVITY_C)) # 27 uinque activity types
unique(hazfuels$ACTIVITY)

keep = c(4113, 4117, 4143, 4220, 4270)
hazfuels = hazfuels %>%
  filter(ACTIVITY_C %in% keep)

tmp = hazfuels %>%
  group_by(ACTIVITY) %>%
  summarize(n=n())
# tons of useful data still. Great. Now just check timberharvest and then extract data

############ 
# timber harvest polygosn

timb = st_read('data/raw/GMUG_Activities/raw/GMUG_TimberHarvest.shp') %>%
  data.frame() %>%
  mutate(date_completed = parse_date_time(DATE_COMPL, orders=c('ymd'))) %>%
  filter(year(date_completed) > 2017) # filter to relevant dates completeds (2017-2023)

sort(unique(timb$ACTIVITY_2))
unique(timb$ACTIVITY_N)
keep = c(4101, 4113, 4117, 4143, 4220, 4270)
timb = timb %>%
  filter(ACTIVITY_2 %in% keep)
  
tmp = timb %>%
  group_by(ACTIVITY_N) %>%
  summarize(n=n())  

########################################
# final check - how many of timb, hazfuels are repeats?
#############################################
  
sort(unique(timb$SUID))
sort(unique(hazfuels$SUID))

for (i in timb$SUID){
  print(i)
  print(i %in% hazfuels$SUID)
  print('')
}

tmp = timb %>%
  filter(!(SUID %in% hazfuels$SUID))

# okay yes all but 3 are repeats! So use timb, not hazfuels.

# final export - 
timb = st_read('data/GMUG_Activities/GMUG_TimberHarvest.shp') %>%
  mutate(date_completed = parse_date_time(DATE_COMPL, orders=c('ymd'))) %>%
  filter(year(date_completed) > 2017, # filter to relevant dates completeds (2017-2023)
         ACTIVITY_2 %in% c(4101, 4113, 4117, 4143, 4220, 4270)) 

st_write(timb, 'data/GMUG_Activities/GMUG_TimberHarvest_SUHFER.shp')
