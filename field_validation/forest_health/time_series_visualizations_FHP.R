library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

getwd()

trees = read.csv('data/forest health/forest_health_trees_plot-level_20250301.csv') %>%
  mutate(pct_healthy = n_healthy/n_aspen,
         pct_declining = n_declining/n_aspen,
         pct_dead = (n_recent_dead + n_snag)/n_aspen,
         pct_dead_or_declining = (n_recent_dead + n_snag + n_declining)/n_aspen,
         pct_live = (n_healthy+n_declining)/n_aspen,
         plot_type = str_sub(Plot, -1, -1),
         year_ = Year) %>%
  rename(plot=Plot)
trees$year_[trees$year_ %in% c(2007, 2008)] = '2007-2008'
trees$year_[trees$year_ %in% c(2020, 2021, 2022)] = '2020-2022'

plotspec = read.csv('data/forest health/plots_spectra.csv') %>%
  mutate(date=paste(month, year)) %>%
  mutate(date=lubridate::my(date),
         plot_type = str_sub(Plot, -1, -1)) %>%
  pivot_longer(cols=c(NDMI, NDVI, lwc), names_to='metric', values_to='values') %>%
  rename(plot=Plot)

# 058H
plot_ = '058H'
years_ = c(2007, 2023)
insitu = ggplot(trees %>% filter(plot==plot_),
       aes(x=Year, y=pct_healthy)) +
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


# 056H
plot_ = '056H'
years_ = c(2013, 2024)
insitu = ggplot(trees %>% filter(plot==plot_),
                aes(x=Year, y=pct_healthy)) +
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

# 316S
plot_ = '316S'
years_ = c(2007, 2024)
insitu = ggplot(trees %>% filter(plot==plot_),
                aes(x=Year, y=pct_healthy)) +
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



# 282S
plot_ = '282S'
years_ = c(2007, 2024)
insitu = ggplot(trees %>% filter(plot==plot_),
                aes(x=Year, y=pct_healthy)) +
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
