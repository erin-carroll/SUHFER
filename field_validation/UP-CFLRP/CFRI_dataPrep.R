library(ggplot2)
library(dplyr)
library(lubridate)

# plot-level data
plots = read.csv('data/UP-CFRLP/raw/RA_data_Master_20240305_plotinfo.csv') %>%
  filter(coordinate_E!='.',
         Pre_Post!='?') %>%
  mutate(coordinate_E = as.numeric(coordinate_E),
         coordinate_N = as.numeric(coordinate_N),
         plot_type=if_else(Pre_Post %in% c("Post","POSTRX","PostRX","POST","PostRx"), 'POST', 'PRE'))

# how many pre, post surveys per plot?
tmp = plots %>%
  group_by(Plot_name, plot_type) %>%
  summarize(n_years=n_distinct(Year)) %>%
  filter(plot_type=='POST', n_years>1)

# tree-level data
overstory = read.csv('data/UP-CFRLP/raw/RA_data_Master_20240305_overstory.csv') %>%
  mutate(DATE = parse_date_time(DATE, orders=c('mdy')),
         Spp=if_else(Spp %in% c('UNID', 'UNKFIR', 'UNKFl'), 'UNK', Spp),
         Spp=toupper(Spp),
         Status=toupper(Status),
         DBH=as.numeric(DBH)) %>%
  mutate(basal_area = pi*((DBH/2)^2)) %>%
  filter(PLOT %in% plots$Plot_name, # filter to where we have spatial coordinates in plots
         PLOT_TYPE!='PARTIAL') %>% # remove partially surveyed plots
  mutate(Year=year(DATE),
         ba_standing_live=if_else(Status=='L', basal_area, 0),
         ba_dead = if_else(Status %in% c('D'), basal_area, 0),
         ba_dead_or_down = if_else(Status %in% c('D', 'S', 'X', 'Y'), basal_area, 0),
         PRE.POST=if_else(PRE.POST %in% c("POSTRX", "POST ", "POST"), 'POST', PRE.POST)) %>%
  mutate(PRE.POST=if_else(PRE.POST=='?', NA, PRE.POST))

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

length(unique(plots$Plot_name)) # 49 plots
length(unique(overstory$PLOT)) # 49 plots

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

plot_area_cm2 = 164*164 * 929.03 # plot s164x164 ft

overstory_summary = overstory %>%
  group_by(PLOT, Year, PRE.POST) %>%
  summarize(n_trees_surveyed=n(), 
            n_species=n_distinct(Spp), # add field that gives counts n of each species?
            total_ba_cm2=sum(basal_area, na.rm=T),
            total_ba_standing_live_cm2=sum(ba_standing_live, na.rm=T),
            total_ba_dead_cm2=sum(ba_dead, na.rm=T),
            total_ba_dead_or_down_cm2=sum(ba_dead_or_down, na.rm=T)) %>%
  ungroup() %>%
  mutate(plot_area_cm2=plot_area_cm2) %>%
  mutate(pct_ba_standing_live=total_ba_standing_live_cm2/total_ba_cm2,
         pct_ba_dead=total_ba_dead_cm2/total_ba_cm2,
         pct_ba_dead_or_down=total_ba_dead_or_down_cm2/total_ba_cm2,
         standing_live_ba_density=total_ba_standing_live_cm2/plot_area_cm2,
         dead_ba_density=total_ba_dead_cm2/plot_area_cm2,
         dead_or_down_ba_density=total_ba_dead_or_down_cm2/plot_area_cm2)

# export
fp_out = 'data/UP-CFRLP/CFRI_trees_plot-level.csv'
write.csv(overstory_summary, fp_out, row.names=F)

cfri_plots = plots %>%
  group_by(Plot_name) %>%
  filter(Year==max(Year))
fp_out =  'data/UP-CFRLP/CFRI_plots.csv'
write.csv(cfri_plots, fp_out, row.names=F)
