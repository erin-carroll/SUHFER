library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

getwd()

trees = read.csv('data/SBEADMR/SBEADMR_plot-level.csv') %>%
  rename(plot=PLOT_ID)
plotspec = read.csv('data/SBEADMR/SBEADMR_plots_spectra.csv') %>%
  mutate(date=paste(month, year)) %>%
  mutate(date=lubridate::my(date)) %>%
  rename(plot=PLOT_ID) %>%
  pivot_longer(cols=c(NDMI, NDVI, lwc), names_to='metric', values_to='values')

# QU-2-PLT
plot_ = 'QU-2-PLT'
years_ = c(2017, 2023)
insitu = ggplot(trees %>% filter(plot==plot_),
       aes(x=Year, y=pct_live)) +
  geom_smooth(method='lm', color='darkgray', alpha=0.5) +
  geom_point() +
  xlim(years_) +
  labs(title=plot_)
spec = ggplot(plotspec %>% filter(plot==plot_)) +
  geom_smooth(method='lm', se=F, aes(x=date, y=values), color='darkgray', alpha=0.5) +
  geom_point(aes(x=date, y=values, color=month)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  facet_wrap(~metric, scales='free_y')
p = grid.arrange(insitu, spec, ncol=2, widths=c(1,3))

# QU-0-PLT
plot_ = 'QU-0-PLT'
years_ = c(2017, 2024)
insitu = ggplot(trees %>% filter(plot==plot_),
                aes(x=Year, y=pct_live)) +
  geom_smooth(method='lm', color='darkgray', alpha=0.5) +
  geom_point() +
  xlim(years_) +
  labs(title=plot_)
spec = ggplot(plotspec %>% filter(plot==plot_)) +
  geom_smooth(method='lm', se=F, aes(x=date, y=values), color='darkgray', alpha=0.5) +
  geom_point(aes(x=date, y=values, color=month)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  facet_wrap(~metric, scales='free_y')
p = grid.arrange(insitu, spec, ncol=2, widths=c(1,3))

# WP-6-PLT
plot_ = 'WP-6-PLT'
years_ = c(2014, 2023)
insitu = ggplot(trees %>% filter(plot==plot_),
                aes(x=Year, y=pct_live)) +
  geom_smooth(method='lm', color='darkgray', alpha=0.5) +
  geom_point() +
  xlim(years_) +
  labs(title=plot_)
spec = ggplot(plotspec %>% filter(plot==plot_)) +
  geom_smooth(method='lm', se=F, aes(x=date, y=values), color='darkgray', alpha=0.5) +
  geom_point(aes(x=date, y=values, color=month)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  facet_wrap(~metric, scales='free_y')
p = grid.arrange(insitu, spec, ncol=2, widths=c(1,3))

# RB-3-PLT
plot_ = 'RB-3-PLT'
years_ = c(2014, 2024)
insitu = ggplot(trees %>% filter(plot==plot_),
                aes(x=Year, y=pct_live)) +
  geom_smooth(method='lm', color='darkgray', alpha=0.5, se=F) +
  geom_point() +
  xlim(years_) +
  labs(title=plot_)
spec = ggplot(plotspec %>% filter(plot==plot_)) +
  geom_smooth(method='lm', se=F, aes(x=date, y=values), color='darkgray', alpha=0.5) +
  geom_point(aes(x=date, y=values, color=month)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  facet_wrap(~metric, scales='free_y')
p = grid.arrange(insitu, spec, ncol=2, widths=c(1,3))


