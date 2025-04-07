library(dplyr)
library(ggplot2)

dat = read.csv('data/SBEADMR/raw/Overstory.csv')
colnames(dat)

sort(unique(dat$Year))

tmp = dat %>%
  group_by(SPECIES) %>%
  summarize(n=n())


# filter to plots visited more than once
keep_plots = dat %>%
  group_by(PLOT_ID) %>%
  summarize(n = n_distinct(Year)) %>%
  filter(n > 1) %>%
  pull(PLOT_ID)
dat = dat %>%
  filter(PLOT_ID %in% keep_plots)

unique(dat$SizeClass) # keep them all I guess? Sapling is pretty small... but about a third of the available data? So keep?
# Sapling: 0-4.9 inch DBH
# Pole: 5 – 7.9 inch DBH
# Sawtimber: 8+ inch DBH
tmp = dat %>%
  group_by(SizeClass) %>%
  summarize(n=n())

unique(dat$live.dead)

dat_plots = dat %>%
  mutate(live=if_else(live.dead=='L', 1, 0),
         dead=if_else(live.dead=='D', 1, 0)) %>%
  group_by(PLOT_ID, Year) %>%
  summarize(n_trees_surveyed=n(),
            n_species=n_distinct(SPECIES),
            n_live=sum(live),
            n_dead=sum(dead),
            UTM_E=mean(UTM_E),
            UTM_N=mean(UTM_N)) %>%
  mutate(pct_live=n_live/n_trees_surveyed,
         pct_dead=n_dead/n_trees_surveyed)
write.csv(dat_plots, 'data/SBEADMR/SBEADMR_plot-level.csv', row.names=F)

plots = dat %>%
  group_by(PLOT_ID) %>%
  filter(Year==max(Year)) %>%
  summarize(UTM_E=mean(UTM_E),
            UTM_N=mean(UTM_N))
write.csv(plots, 'data/SBEADMR/SBEADMR_plots.csv', row.names=F)  




