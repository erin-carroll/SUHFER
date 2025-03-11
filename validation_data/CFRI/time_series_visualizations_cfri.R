library(dplyr)
library(ggplot2)
library(gridExtra)

getwd()

cfri_trees = read.csv('data/UP-CFRLP/CFRI_trees_plot-level.csv')
cfri_spec = read.csv('data/UP-CFRLP/CFRI_plots_spectra_polygons.csv') %>%
  mutate(date=paste(month, year)) %>%
  mutate(date=lubridate::my(date)) #%>%
  # pivot_wider(names_from=metric, values_from=c(min, mean, max, std))

sbeadmr_trees = read.csv('data/SBEADMR/SBEADMR_plot-level.csv')
sbeadmr_spec = read.csv('data/SBEADMR/SBEADMR_plots_spectra.csv')

fhp_trees = read.csv('data/forest health/forest_health_trees_plot-level_20250301.csv')
fhp_spec = read.csv('data/forest health/plots_spectra.csv')

# CFRI - using min within polygon

# RA2170
plot_ = 'RA2170'
years_ = c(2016, 2022)
insitu = ggplot(cfri_trees %>% filter(PLOT==plot_),
       aes(x=Year, y=pct_standing_live)) +
  geom_smooth(method='lm', color='darkgray', alpha=0.5) +
  geom_point() +
  xlim(years_) +
  labs(title=plot_)
spec = ggplot(cfri_spec %>% filter(plot==plot_)) +
  geom_smooth(method='lm', se=F, aes(x=date, y=min), color='darkgray', alpha=0.5) +
  geom_point(aes(x=date, y=min, color=month)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  facet_wrap(~metric, scales='free_y')
p = grid.arrange(insitu, spec, ncol=2, widths=c(1,3))

plot_ = 'RA2190'
years_ = c(2016, 2022)
insitu = ggplot(cfri_trees %>% filter(PLOT==plot_),
                aes(x=Year, y=pct_standing_live)) +
  geom_smooth(method='lm', color='darkgray', alpha=0.5) +
  geom_point() +
  xlim(years_) +
  labs(title=plot_)
spec = ggplot(cfri_spec %>% filter(plot==plot_)) +
  geom_smooth(method='lm', se=F, aes(x=date, y=min), color='darkgray', alpha=0.5) +
  geom_point(aes(x=date, y=min, color=month)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  facet_wrap(~metric, scales='free_y')
p = grid.arrange(insitu, spec, ncol=2, widths=c(1,3))


plot_ = 'RA1110'
years_ = c(2015, 2024)
insitu = ggplot(cfri_trees %>% filter(PLOT==plot_),
                aes(x=Year, y=pct_standing_live)) +
  geom_smooth(method='lm', color='darkgray', alpha=0.5, se=F) +
  geom_point() +
  xlim(years_) +
  labs(title=plot_)
spec = ggplot(cfri_spec %>% filter(plot==plot_)) +
  geom_smooth(method='lm', se=F, aes(x=date, y=min), color='darkgray', alpha=0.5) +
  geom_point(aes(x=date, y=min, color=month)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  facet_wrap(~metric, scales='free_y')
p = grid.arrange(insitu, spec, ncol=2, widths=c(1,3))

plot_ = 'RA2040'
years_ = c(2016, 2024)
insitu = ggplot(cfri_trees %>% filter(PLOT==plot_),
                aes(x=Year, y=pct_standing_live)) +
  geom_smooth(method='lm', color='darkgray', alpha=0.5, se=F) +
  geom_point() +
  xlim(years_) +
  labs(title=plot_)
spec = ggplot(cfri_spec %>% filter(plot==plot_)) +
  geom_smooth(method='lm', se=F, aes(x=date, y=min), color='darkgray', alpha=0.5) +
  geom_point(aes(x=date, y=min, color=month)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  facet_wrap(~metric, scales='free_y')
p = grid.arrange(insitu, spec, ncol=2, widths=c(1,3))

plot_ = 'RA610'
years_ = c(2015, 2024)
insitu = ggplot(cfri_trees %>% filter(PLOT==plot_),
                aes(x=Year, y=pct_standing_live)) +
  geom_smooth(method='lm', color='darkgray', alpha=0.5, se=F) +
  geom_point() +
  xlim(years_) +
  labs(title=plot_)
spec = ggplot(cfri_spec %>% filter(plot==plot_)) +
  geom_smooth(method='lm', se=F, aes(x=date, y=min), color='darkgray', alpha=0.5) +
  geom_point(aes(x=date, y=min, color=month)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  facet_wrap(~metric, scales='free_y')
p = grid.arrange(insitu, spec, ncol=2, widths=c(1,3))

plot_ = 'L13'
years_ = c(2015, 2024)
insitu = ggplot(cfri_trees %>% filter(PLOT==plot_),
                aes(x=Year, y=pct_standing_live)) +
  geom_smooth(method='lm', color='darkgray', alpha=0.5, se=F) +
  geom_point() +
  xlim(years_) +
  labs(title=plot_)
spec = ggplot(cfri_spec %>% filter(plot==plot_)) +
  geom_smooth(method='lm', se=F, aes(x=date, y=min), color='darkgray', alpha=0.5) +
  geom_point(aes(x=date, y=min, color=month)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  facet_wrap(~metric, scales='free_y')
p = grid.arrange(insitu, spec, ncol=2, widths=c(1,3))

