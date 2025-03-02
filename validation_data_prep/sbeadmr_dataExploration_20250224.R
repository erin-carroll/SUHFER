library(dplyr)
library(ggplot2)

dat = read.csv('data/SBEADMR/Overstory.csv')

# is any plot visited in more than one year in this dataset?
tmp = dat %>%
  group_by(PLOT_ID) %>%
  summarize(n = n_distinct(Year)) %>%
  filter(n > 1)

dat = dat %>%
  filter(PLOT_ID %in% tmp$PLOT_ID)

write.csv(dat, 'data/SBEADMR/Overstory_repeatVisit.csv', row.names=F)
