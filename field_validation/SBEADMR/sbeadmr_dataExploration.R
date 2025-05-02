library(dplyr)
library(ggplot2)
library(stringr)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER')

dat = read.csv('data/SBEADMR/raw/Overstory.csv')


#### exploration
colnames(dat)

sort(unique(dat$Year))

# are there only unique combinations of plot, year, species, livedead, sawclass? >> yes, already summarized
tmp = dat %>%
  group_by(PLOT_ID, Year, SPECIES, live.dead, SizeClass) %>%
  summarize(n=n())

tmp = dat %>%
  group_by(SPECIES) %>%
  summarize(n=n())

# Sapling: 0-4.9 inch DBH
# Pole: 5 – 7.9 inch DBH
# Sawtimber: 8+ inch DBH
tmp = dat %>%
  group_by(SizeClass) %>%
  summarize(n=n())

unique(dat$live.dead)


#### transform data

# filter to plots visited more than once
keep_plots = dat %>%
  group_by(PLOT_ID) %>%
  summarize(n = n_distinct(Year)) %>%
  filter(n > 1) %>%
  pull(PLOT_ID)
dat = dat %>%
  filter(PLOT_ID %in% keep_plots)

# summarize plot location data
plots = dat %>%
  group_by(PLOT_ID) %>%
  filter(Year==max(Year)) %>%
  summarize(UTM_E=mean(UTM_E),
            UTM_N=mean(UTM_N)) %>%
  mutate(treatment=str_split_i(PLOT_ID, '-', 3)) %>%
  mutate(treatment=if_else(treatment=='CTLP', 'control', 'treatment'))
write.csv(plots, 'data/SBEADMR/SBEADMR_plots.csv', row.names=F)

# summarize tree data per plot
plot_area_m2 = 500
dat_plots = dat %>%
  mutate(ba.metric.live=if_else(live.dead=='L', BA.metric, 0),
         ba.metric.dead=if_else(live.dead=='D', BA.metric, 0)) %>%
  group_by(PLOT_ID, Year) %>%
  summarize(n_species=n_distinct(SPECIES),
            total_ba=sum(BA.metric),
            total_ba_live=sum(ba.metric.live),
            total_ba_dead=sum(ba.metric.dead),
            UTM_E=mean(UTM_E),
            UTM_N=mean(UTM_N)) %>%
  mutate(pct_ba_live=total_ba_live/total_ba,
         pct_ba_dead=total_ba_dead/total_ba,
         live_ba_density=total_ba_live/plot_area_m2,
         dead_ba_density=total_ba_dead/plot_area_m2,
         treatment=str_split_i(PLOT_ID, '-', 3)) %>%
  mutate(treatment=if_else(treatment=='CTLP', 'control', 'treatment'))
write.csv(dat_plots, 'data/SBEADMR/SBEADMR_plot-level.csv', row.names=F)


