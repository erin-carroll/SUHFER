library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(stringr)
library(lubridate)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER')

# prepare all individualplots 

#########################
# FHP
trees = read.csv('data/forest health/forest_health_trees_plot-level_20250301.csv') %>%
  mutate(pct_healthy = n_healthy/n_aspen,
         pct_declining = n_declining/n_aspen,
         pct_dead = (n_recent_dead + n_snag)/n_aspen,
         pct_dead_or_declining = (n_recent_dead + n_snag + n_declining)/n_aspen,
         pct_live = (n_healthy+n_declining)/n_aspen,
         plot_type = str_sub(Plot, -1, -1),
         year_ = Year,
         year_date=make_date(year=Year, month=1, day=1)) %>%
  rename(plot=Plot)
trees$year_[trees$year_ %in% c(2007, 2008)] = '2007-2008'
trees$year_[trees$year_ %in% c(2020, 2021, 2022)] = '2020-2022'

plotspec = read.csv('data/forest health/plots_spectra.csv') %>%
  mutate(date=paste(month, year)) %>%
  mutate(date=lubridate::my(date),
         plot_type = str_sub(Plot, -1, -1)) %>%
  pivot_longer(cols=c(NDMI, NDVI, lwc), names_to='metric', values_to='values') %>%
  rename(plot=Plot) %>%
  filter(metric!='lwc')

# 058H
plot_ = '058H'
years_ = c(2007, 2023)
p_fhp_down = ggplot() +
  geom_point(data=trees %>% filter(plot==plot_),
             aes(x=year_date, y=pct_healthy, color='pct healthy')) +
  geom_point(data=plotspec %>% filter(plot==plot_),
             aes(x=date, y=values, color=metric)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  geom_smooth(data=trees %>% filter(plot==plot_),
              aes(x=year_date, y=pct_healthy, color='pct healthy'),
              method='lm', se=F,
              alpha=0.5) +
  geom_smooth(data=plotspec %>% filter(plot==plot_),
              aes(x=date, y=values, color=metric),
              method='lm', se=F) + 
  labs(title=plot_, y='value', x='date') +
  scale_color_manual(values=c('#6677CD', '#38A800', 'black')) +
  theme(legend.title=element_blank()) #+
  theme(legend.position = "none")
p_fhp_down  

# 282S
plot_ = '282S'
years_ = c(2007, 2024)
p_fhp_up = ggplot() +
  geom_point(data=trees %>% filter(plot==plot_),
             aes(x=year_date, y=pct_healthy, color='pct healthy')) +
  geom_point(data=plotspec %>% filter(plot==plot_),
             aes(x=date, y=values, color=metric)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  geom_smooth(data=trees %>% filter(plot==plot_),
              aes(x=year_date, y=pct_healthy, color='pct healthy'),
              method='lm', se=F,
              alpha=0.5) +
  geom_smooth(data=plotspec %>% filter(plot==plot_),
              aes(x=date, y=values, color=metric),
              method='lm', se=F) + 
  labs(title=plot_, y='value', x='date') +
  scale_color_manual(values=c('#6677CD', '#38A800', 'black')) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none")
p_fhp_up

#########################
# UP-CFLRP

cfri_trees = read.csv('data/UP-CFRLP/CFRI_trees_plot-level.csv') %>%
  mutate(year_date=make_date(year=Year, month=1, day=1))
cfri_spec = read.csv('data/UP-CFRLP/CFRI_plots_spectra_polygons.csv') %>%
  mutate(date=paste(month, year)) %>%
  mutate(date=lubridate::my(date)) %>%
  filter(metric!='lwc', !(date=="2017-09-01"))

# RA2170
plot_ = 'RA2170'
years_ = c(2016, 2022)
p_cfri_down = ggplot() +
  geom_point(data=cfri_trees %>% filter(PLOT==plot_),
             aes(x=year_date, y=pct_standing_live, color='pct live')) +
  geom_point(data=cfri_spec %>% filter(plot==plot_),
             aes(x=date, y=mean, color=metric)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  geom_smooth(data=cfri_trees %>% filter(PLOT==plot_),
              aes(x=year_date, y=pct_standing_live, color='pct live'),
              method='lm', se=F,
              alpha=0.5) +
  geom_smooth(data=cfri_spec %>% filter(plot==plot_),
              aes(x=date, y=mean, color=metric),
              method='lm', se=F) + 
  labs(title=plot_, y='value', x='date') +
  scale_color_manual(values=c('#6677CD', '#38A800', 'black')) +
  theme(legend.title=element_blank()) #+
  theme(legend.position = "none")
p_cfri_down

plot_ = 'RA1110'
years_ = c(2015, 2024)
p_cfri_up = ggplot() +
  geom_point(data=cfri_trees %>% filter(PLOT==plot_),
             aes(x=year_date, y=pct_standing_live, color='pct live')) +
  geom_point(data=cfri_spec %>% filter(plot==plot_),
             aes(x=date, y=mean, color=metric)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  geom_smooth(data=cfri_trees %>% filter(PLOT==plot_),
              aes(x=year_date, y=pct_standing_live, color='pct live'),
              method='lm', se=F,
              alpha=0.5) +
  geom_smooth(data=cfri_spec %>% filter(plot==plot_),
              aes(x=date, y=mean, color=metric),
              method='lm', se=F) + 
  labs(title=plot_, y='value', x='date') +
  scale_color_manual(values=c('#6677CD', '#38A800', 'black')) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none")
p_cfri_up


#########################
# SBEADMR

trees = read.csv('data/SBEADMR/SBEADMR_plot-level.csv') %>%
  rename(plot=PLOT_ID) %>%
  mutate(year_date=make_date(year=Year, month=1, day=1))

plotspec = read.csv('data/SBEADMR/SBEADMR_plots_spectra.csv') %>%
  mutate(date=paste(month, year)) %>%
  mutate(date=lubridate::my(date)) %>%
  rename(plot=PLOT_ID) %>%
  pivot_longer(cols=c(NDMI, NDVI, lwc), names_to='metric', values_to='values') %>%
  filter(metric!='lwc')

# QU-0-PLT
plot_ = 'QU-0-PLT'
years_ = c(2017, 2024)
p_sbeadmr_down = ggplot() +
  geom_point(data=trees %>% filter(plot==plot_),
             aes(x=year_date, y=pct_live, color='pct live')) +
  geom_point(data=plotspec %>% filter(plot==plot_),
             aes(x=date, y=values, color=metric)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  geom_smooth(data=trees %>% filter(plot==plot_),
              aes(x=year_date, y=pct_live, color='pct live'),
              method='lm', se=F,
              alpha=0.5) +
  geom_smooth(data=plotspec %>% filter(plot==plot_),
              aes(x=date, y=values, color=metric),
              method='lm', se=F) + 
  labs(title=plot_, y='value', x='date') +
  scale_color_manual(values=c('#6677CD', '#38A800', 'black')) +
  theme(legend.title=element_blank()) #+
  theme(legend.position = "none")
p_sbeadmr_down

# WP-6-PLT
plot_ = 'WP-6-PLT'
years_ = c(2014, 2023)
p_sbeadmr_up = ggplot() +
  geom_point(data=trees %>% filter(plot==plot_),
             aes(x=year_date, y=pct_live, color='pct live')) +
  geom_point(data=plotspec %>% filter(plot==plot_),
             aes(x=date, y=values, color=metric)) +
  xlim(ymd(sprintf('%s-01-01', years_[1])),ymd(sprintf('%s-01-01', years_[2]))) +
  geom_smooth(data=trees %>% filter(plot==plot_),
              aes(x=year_date, y=pct_live, color='pct live'),
              method='lm', se=F,
              alpha=0.5) +
  geom_smooth(data=plotspec %>% filter(plot==plot_),
              aes(x=date, y=values, color=metric),
              method='lm', se=F) + 
  labs(title=plot_, y='value', x='date') +
  scale_color_manual(values=c('#6677CD', '#38A800', 'black')) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none")
p_sbeadmr_up

#########################
# combined

# lis = p_fhp_up, p_fhp_down, p_cfri_up, p_cfri_down, p_sbeadmr_up, p_sbeadmr_down
p = grid.arrange(p_fhp_up, p_fhp_down, p_cfri_up, p_cfri_down, p_sbeadmr_up, p_sbeadmr_down, ncol=2)
ggsave("report/fig4.png", plot=p, width = 6.5, height = 9, units = "in", dpi = 300)


