library(ggplot2)
library(dplyr)
library(lubridate)

plots = read.csv('data/UP-CFRLP/RA_data_Master_20240305_plotinfo.csv') %>%
  filter(coordinate_E!='.') %>%
  mutate(coordinate_E = as.numeric(coordinate_E),
         coordinate_N = as.numeric(coordinate_N))

overstory = read.csv('data/UP-CFRLP/RA_data_Master_20240305_overstory.csv') %>%
  mutate(DATE = parse_date_time(DATE, orders=c('mdy')))

colnames(plots)
colnames(overstory)

unique(plots$Plot_name)
unique(overstory$PLOT)

# filter overstory to where we have spatial coordinates in plots
overstory = overstory %>%
  filter(PLOT %in% plots$Plot_name)

# then filter again to where visited more than once
tmp = overstory %>%
  group_by(PLOT) %>%
  summarize(n = n(),
            n_dates = n_distinct(DATE),
            n_spp = n_distinct(Spp)) %>%
  filter(n_dates > 1)

overstory = overstory %>%
  filter(PLOT %in% tmp$PLOT)

# visualize dates per plot 
ggplot(data=overstory) +
  geom_point(aes(x=DATE, y=1)) +
  facet_wrap(~PLOT)

# filter spatial data to repeat visits
plots = plots %>%
  filter(Plot_name %in% overstory$PLOT)

# fix one weird coordinate, plus lacking negative sign
plots$coordinate_N[plots$coordinate_N==33.44706] = 38.44706
plots$coordinate_E = plots$coordinate_E*-1

write.csv(plots, 'data/UP-CFRLP/RAplots_plotinfo_repeatVisit.csv', row.names=F)

