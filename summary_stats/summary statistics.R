library(dplyr)
library(ggplot2)


data_types = read.csv('fsveg_local_types.csv')

data = read.csv('fsveg_gmug_topo.csv') %>%
  left_join(data_types, by='LOCAL_TYPE') %>%
  filter(! (Description %in% c(NA))) %>%
  mutate(aspect_ordered = ifelse(ASPECT_CLA %in% c('SO','SW','SE'),-1,
                                                   ifelse(ASPECT_CLA %in% c('WE','EA'),0,
                                                          ifelse(ASPECT_CLA %in% c('NW','NO','NE'),1, NA))) %>% factor)
    
g_slope_ndmi = ggplot(data, aes(x=Description,y=slope_NDMI, size=ACRES/max(ACRES))) +
  geom_boxplot(outliers = FALSE, color='purple') +
  geom_point(alpha=0.05,shape=1) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept = 0, color='red') +
  theme(legend.position='none')
ggsave(g_slope_ndmi, file='g_slope_ndmi.png',width=6,height=5)


g_slope_ndvi = ggplot(data, aes(x=Description,y=slope_NDVI, size=ACRES/max(ACRES))) +
  geom_boxplot(outliers = FALSE, color='purple') +
  geom_point(alpha=0.05,shape=1) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept = 0, color='red') +
  theme(legend.position='none')
ggsave(g_slope_ndvi, file='g_slope_ndvi.png',width=6,height=5)



g_trend_sign_ndvi = data %>% 
  mutate(significant=p_NDVI < 0.05, slope_positive = slope_NDVI > 0) %>% 
  group_by(Description, significant, slope_positive) %>% 
  summarize(AREA_TOTAL=sum(ACRES)) %>%
  mutate(slope_positive = ifelse(significant, ifelse(slope_positive,'positive','negative'), 'not significant')) %>%
  select(-significant) %>%
  ggplot(aes(x=Description,y=AREA_TOTAL,fill=slope_positive)) +
  geom_bar(stat='identity') +
  theme_bw() +
  coord_flip() +
  ylab('Area (acres)') + xlab('LOCAL_TYPE') +
  #theme(legend.position='none') +
  ggtitle('NDVI') +
  coord_flip() +
  scale_fill_manual(values=c('red','gray','blue'),name='Temporal trend slope')
ggsave(g_trend_sign_ndvi, file='g_trend_sign_ndvi.png', width=7, height=5)

g_trend_sign_ndmi = data %>% 
  mutate(significant=p_NDMI < 0.05, slope_positive = slope_NDMI > 0) %>% 
  group_by(Description, significant, slope_positive) %>% 
  summarize(AREA_TOTAL=sum(ACRES)) %>%
  mutate(slope_positive = ifelse(significant, ifelse(slope_positive,'positive','negative'), 'not significant')) %>%
  select(-significant) %>%
  ggplot(aes(x=Description,y=AREA_TOTAL,fill=slope_positive)) +
  geom_bar(stat='identity') +
  theme_bw() +
  coord_flip() +
  ylab('Area (acres)') + xlab('LOCAL_TYPE') +
  #theme(legend.position='none') +
  ggtitle('NDMI') +
  coord_flip() +
  scale_fill_manual(values=c('red','gray','blue'),name='Temporal trend slope')
ggsave(g_trend_sign_ndmi, file='g_trend_sign_ndmi.png', width=7, height=5)




g_ndvi_ndmi = ggplot(data, aes(x=slope_NDMI,y=slope_NDVI, color=p_NDMI<0.05 & p_NDVI<0.05, size=ACRES/max(ACRES))) +
  theme_bw() +
  facet_wrap(~Description) +
  scale_color_manual(values=c('gray','purple'),name='Temporal trend p<0.05') +
  geom_abline(slope=1,intercept=0) +
  geom_point(alpha=0.2,shape=1) +
  theme(legend.position = 'bottom')
ggsave(g_ndvi_ndmi, file='g_ndvi_ndmi.png',width=8,height=8)

g_ndmi_elevation_aspect = ggplot(data %>% filter(!is.na(aspect_ordered)), aes(x=ELEVATION_,y=aspect_ordered,color=slope_NDMI), size=ACRES/max(ACRES)) + 
  theme_bw() +
  scale_color_viridis_c(name='Temporal trend slope') +
  facet_wrap(~Description) +
  geom_jitter(width=0, height=0.2, alpha=0.2,shape=1) +
  ggtitle('NDMI') +
  theme(legend.position = 'bottom') +
  xlab("Elevation (ft)") + ylab("Cosine aspect")
ggsave(g_ndmi_elevation_aspect, file='g_ndmi_elevation_aspect.png',width=12,height=5)

g_ndvi_elevation_aspect = ggplot(data %>% filter(!is.na(aspect_ordered)), aes(x=ELEVATION_,y=aspect_ordered,color=slope_NDVI, size=ACRES/max(ACRES))) + 
  geom_point(alpha=0.2,shape=1) +
  theme_bw() +
  scale_color_viridis_c(name='Temporal trend slope') +
  facet_wrap(~Description) +
  geom_jitter(width=0, height=0.2, alpha=0.2,shape=1) +
  ggtitle('NDVI') +
  theme(legend.position = 'bottom') +
  xlab("Elevation (ft)") + ylab("Cosine aspect")
ggsave(g_ndvi_elevation_aspect, file='g_ndvi_elevation_aspect.png',width=12,height=5)

                    