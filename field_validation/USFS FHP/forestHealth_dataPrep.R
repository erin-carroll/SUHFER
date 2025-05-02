library(dplyr)
library(ggplot2)
library(stringr)
library(sf)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER')


# prepare plot data (ids, years, coordinates)
plot08 = read.csv('data/forest health/raw/aspenSADplots08_clean_PLOT.csv') %>%
  filter(!is.na(Year)) %>%
  select(Year:NAD83.GPS.Zone) %>%
  rename(plot_number=PLOTn,
         plot_type=Health,
         date=Date.Completed,
         GPS.Zone=NAD83.GPS.Zone) %>%
  select(-NatFor, -Preselected.Point..Attempts.)

plot13 = read.csv('data/forest health/raw/aspenSADplots13_clean_PLOT.csv') %>%
  rename(plot_number = Plot..,
         plot_type=PLOT.TYPE,
         date=Date,
         GPS.N=N,
         GPS.E=E,
         GPS.Zone=Zone..crew.written.GPS.) %>%
  select(-Notes, -Sat.accuracy..ft., -Location.Type) %>%
  mutate(GPS.Zone=str_replace_all(GPS.Zone, 'S', 'N'))

# get everything in one CRS
tmp = plot13 %>%
  filter(GPS.Zone=='12N') %>%
  st_as_sf(coords=c('GPS.E', 'GPS.N'), crs=26912) %>%
  st_transform(crs=26913) %>%
  mutate(GPS.E_13N=st_coordinates(.)[,1],
         GPS.N_13N=st_coordinates(.)[,2])
plot13 = plot13 %>%
  left_join(tmp) %>%
  mutate(GPS.N = if_else(GPS.Zone=='12N', GPS.N_13N, GPS.N),
         GPS.E = if_else(GPS.Zone=='12N', GPS.E_13N, GPS.E),
         GPS.Zone='13N') %>%
  select(Year:GPS.E)

plot2x = read.csv('data/forest health/raw/aspenSADplots202x_clean_PLOT.csv') %>%
  select(Year:Date, -NF) %>%
  rename(plot_number=Plot..,
         plot_type=PLOT.TYPE,
         date=Date)

plots = bind_rows(plot08, plot13, plot2x) %>%
  mutate(Plot=str_replace_all(Plot, 'D', 'S')) %>%
  mutate(Plot=str_replace_all(Plot, 'Ha', 'H'),
         plot_type=str_replace_all(plot_type, 'a', ''))

tmp = plots %>%
  group_by(Plot) %>%
  summarize(n_years = n_distinct(Year),
            n_unique_coodrinates_N = n_distinct(GPS.N, na.rm=T),
            n_unique_coodrinates_E = n_distinct(GPS.E, na.rm=T),
            max_N = max(GPS.N, na.rm=T),
            min_N = min(GPS.N, na.rm=T),
            max_E = max(GPS.E, na.rm=T),
            min_E = min(GPS.E, na.rm=T)) %>%
  mutate(dif_N = max_N - min_N,
         dif_E = max_E - min_E) %>%
  filter(dif_N >= 50 | dif_E >= 50)

plots = plots %>%
  mutate(FLAG_plot_location_discrepancy = if_else(Plot %in% tmp$Plot, 1, 0))

# shrink down for plot_location csv
plot_locations = plots %>%
  filter(Year==2013)

# prepare tree data
tree08 = read.csv('data/forest health/raw/aspenSADplots08_clean_TREE.csv') %>%
  filter(!is.na(Year)) %>%
  select(Year:Status, -Plot.., -PLOT.TYPE) %>%
  rename(tree_number=Tree.., dbh_cm=DBH..cm.,
         crown_loss_pct=X..Crown.loss) %>%
  mutate(tree_number=as.character(tree_number)) %>%
  filter(Plot!='079H') %>%
  mutate(basal_area=pi*((dbh_cm/2)^2))
  
tree13 = read.csv('data/forest health/raw/aspenSADplots13_clean_TREE.csv') %>%
  filter(!is.na(Year)) %>%
  select(Year:RCL...., -BAF, -Date, -Plot.., -PLOT.TYPE) %>%
  rename(tree_number=Tree..,
         dbh_cm=DBH..cm.,
         crown_loss_pct=RCL....,
         Status=Tree.Type) %>%
  mutate(tree_number=as.character(tree_number))  %>%
  mutate(basal_area=pi*((dbh_cm/2)^2))

tree2x = read.csv('data/forest health/raw/aspenSADplots202x_clean_TREE.csv') %>%
  filter(!is.na(Year)) %>%
  select(Year:RCL...., -Plot.., -PLOT.TYPE, -Forest, -BAF, -Date) %>%
  rename(tree_number=Tree..,
         dbh_cm=DBH..cm.,
         crown_loss_pct=RCL....,
         Status=Tree.Type) %>% 
  mutate(tree_number=as.character(tree_number)) %>%
  mutate(Status=if_else(((Plot=='92H') & (Species=='PIPO')), NA, Status))  %>%
  mutate(basal_area=pi*((dbh_cm/2)^2))

trees = bind_rows(tree08, tree13, tree2x) %>%
  mutate(Plot=str_replace_all(Plot, 'D', 'S')) %>%
  mutate(Plot=str_replace_all(Plot, 'Ha', 'H')) %>%
  mutate(Plot=str_replace_all(Plot, 'Sa', 'S')) %>%
  mutate(Plot=str_pad(Plot, width=4, side='left', pad='0')) %>%
  mutate(Plot=str_replace_all(Plot, 's', 'S')) %>%
  filter(Status!='Nonplot',
         Species!='') %>%
  mutate(tree_number=as.integer(tree_number),
         crown_loss_pct=if_else(crown_loss_pct==999, NA, crown_loss_pct)) %>%
  mutate(Status=str_to_lower(Status)) %>%
  mutate(Status=str_replace_all(Status, 'recdd', 'recent dead')) %>%
  mutate(Status=if_else((Status=='' | Status=='-' | Status=='notaspen' | Status=='not recorded' | Status=='conifer'), NA, Status)) %>%
  mutate(Status=str_replace_all(Status, 'dying', 'declining'))

# summarize trees at the plot-level
colnames(trees)
unique(trees$Status)

trees_by_plot = trees %>%
  filter(!is.na(Status)) %>%
  mutate(ba_healthy=if_else(Status=='healthy', basal_area, 0),
         ba_dead=if_else(Status=='recent dead', basal_area, 0),
         ba_declining=if_else(Status=='declining', basal_area, 0)) %>%
  group_by(Plot, Year) %>%
  summarize(n_trees = n(),
            n_aspen = sum(Species=='POTR5', na.rm=T),
            n_healthy = sum(Status=='healthy', na.rm=T),
            n_recent_dead = sum(Status=='recent dead', na.rm=T),
            n_declining = sum(Status=='declining', na.rm=T),
            n_snag = sum(Status=='snag', na.rm=T),
            total_ba = sum(basal_area),
            total_ba_healthy = sum(ba_healthy),
            total_ba_dead = sum(ba_dead),
            total_ba_declining = sum(ba_declining)) %>%
  mutate(FLAG_lessthan75pctaspen=if_else(((n_aspen/n_trees)>=0.75), 0, 1),
         pct_ba_healthy=total_ba_healthy/total_ba,
         pct_ba_dead=total_ba_dead/total_ba,
         pct_ba_declining=total_ba_declining/total_ba) # flag where < 75% aspen, meaning missing a lot of data

# final time series?
ggplot(data=trees_by_plot) +
  geom_point(aes(x=Year, y=n_healthy/n_aspen)) +
  facet_wrap(~Plot) +
  ylab('pct healthy')

fp_out='data/forest health/FHP_plots.csv'
write.csv(plots, fp_out, row.names=F)

fp_out='data/forest health/FHP_plot-level.csv'
write.csv(trees_by_plot, fp_out, row.names=F)


