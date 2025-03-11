library(ggplot2)
library(dplyr)
library(lubridate)

dat = read.csv('data/suhfer_fsveg_timeseries_ndvi-ndmi-lwc.csv')

colnames(dat)

dat = dat %>%
  mutate(date=paste(month, year)) %>%
  mutate(date=lubridate::my(date))

ggplot(data=dat, aes(x=date, y=max, color=COVER_TYPE)) +
  geom_point() +
  facet_wrap(~metric, scales='free_y') +
  geom_smooth(method='lm')


# try summarizing the data first?

tmp = dat %>%
  group_by(COVER_TYPE, year, month, metric, date) %>%
  summarize(min=mean(min),
            max=mean(max),
            mean=mean(mean),
            std=sd(std))

ggplot(data=tmp) +
  geom_line(aes(x=date, y=max, color=COVER_TYPE)) +
  facet_wrap(~metric, scales='free_y')
