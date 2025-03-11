library(ggplot2)
library(dplyr)
library(lubridate)

# plot-level data
plots = read.csv('data/UP-CFRLP/raw/RA_data_Master_20240305_plotinfo.csv') %>%
  filter(coordinate_E!='.') %>%
  mutate(coordinate_E = as.numeric(coordinate_E),
         coordinate_N = as.numeric(coordinate_N))

# tree-level data
overstory = read.csv('data/UP-CFRLP/raw/RA_data_Master_20240305_overstory.csv') %>%
  mutate(DATE = parse_date_time(DATE, orders=c('mdy')),
         Spp=if_else(Spp %in% c('UNID', 'UNKFIR', 'UNKFl'), 'UNK', Spp),
         Spp=toupper(Spp),
         Status=toupper(Status)) %>%
  filter(PLOT %in% plots$Plot_name, # filter to where we have spatial coordinates in plots
         PLOT_TYPE!='PARTIAL') %>% # remove partially surveyed plots
  mutate(Year=year(DATE),
         standing_live=if_else(Status=='L', 1, 0),
         down_or_dead = if_else(Status %in% c('S', 'D', 'X', 'Y'), 1, 0))

# clean overstory
colnames(overstory)
str(overstory)
unique(overstory$PRE.POST)
unique(overstory$PLOT_TYPE) # what are partial plots?
unique(overstory$Spp)
unique(overstory$Status) # don't remove non-standing trees, the relevant values will be percentages but specifically changes in those percentage distributions over time.
# L = live
# D = dead
# S = stump
# X = down dead
# Y = down live

# then filter again to where visited more than once
# use overstory, not plots, to be sure we actually have the tree-level data
keep_plots = overstory %>%
  group_by(PLOT) %>%
  summarize(n_years = n_distinct(Year)) %>%
  filter(n_years > 1) #%>%
  pull(PLOT)

overstory = overstory %>%
  filter(PLOT %in% keep_plots$PLOT)
plots = plots %>%
  filter(Plot_name %in% overstory$PLOT)

length(unique(plots$Plot_name)) # 49 plots??
length(unique(overstory$PLOT)) # 49 plots??


# visualize dates per plot 
ggplot(data=overstory) +
  geom_point(aes(x=DATE, y=1)) +
  facet_wrap(~PLOT)

# fix one weird coordinate, plus lacking negative sign
plots$coordinate_N[plots$coordinate_N==33.44706] = 38.44706
plots$coordinate_E = plots$coordinate_E*-1

# this is what I will use to extract spatial data to plot coordinates, and to map for presentation
# will need to update to use most recently collected plot coordinates (E.g. 2023)
write.csv(plots, 'data/UP-CFRLP/CFRI_plots.csv', row.names=F)

# next - summarize stress metrics by plot/year
sort(unique(overstory$Year))

# are any plots visited more than once in a year? checked, fine
tmp = overstory %>%
  group_by(PLOT, Year) %>%
  summarize(n_visites=n_distinct(DATE))
# Yes - figure out what to do with that first
tmp_ = overstory %>% # combine them - looks like survey just done over two days. No action necessary (group by year, not date)
  filter(PLOT=='L23', Year==2022)
tmp_ = overstory %>% # same
  filter(PLOT=='RA1210', Year==2015)
tmp_ = overstory %>% # same
  filter(PLOT=='RA219', Year==2014)
tmp_ = overstory %>% # same
  filter(PLOT=='RA271')

overstory_summary = overstory %>%
  group_by(PLOT, Year) %>%
  summarize(n_trees_surveyed=n(), 
            n_species=n_distinct(Spp), # add field that gives counts n of each species?
            n_standing_live=sum(standing_live),
            n_down_or_dead=sum(down_or_dead)) %>%
  mutate(pct_standing_live=n_standing_live/n_trees_surveyed,
         pct_down_or_dead=n_down_or_dead/n_trees_surveyed)

# export
fp_out = 'data/UP-CFRLP/CFRI_trees_plot-level.csv'
write.csv(overstory_summary, fp_out, row.names=F)

cfri_plots = plots %>%
  group_by(Plot_name) %>%
  filter(Year==max(Year))
fp_out =  'data/UP-CFRLP/CFRI_plots.csv'
write.csv(cfri_plots, fp_out, row.names=F)
