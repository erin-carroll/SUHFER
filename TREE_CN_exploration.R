library(dplyr)
library(data.table)
library(ggplot2)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER/')
tree = read.csv('data/FIA/CO_TREE.csv')
cond = read.csv('data/FIA/CO_COND.csv')
plot = read.csv('data/FIA/CO_PLOT.csv')
cse = read.csv('data/BULLCSE2018/BullCSE2018.csv')

## what about plot_cn?
cse_plotCN = unique(cse %>% filter(!is.na(PLOT_CN)) %>% pull())

cse_treeCN[1] %in% plot$CN

any(plot==cse_plotCN[1], na.rm=T)

ggplot(data=plot) +
  geom_histogram(aes(x=CN)) #+
  geom_vline(aes(xintercept=6502000003040081920)) #+
  xlim(10555783010602, 50555783010602)

colnames(tree)
head(as.character(tree$CN))

cse_treeCN = unique(cse %>% filter(!is.na(TREE_CN)) %>% pull(TREE_CN))
str(cse_treeCN)
str(tree$CN)

ggplot(data=tree) +
  geom_histogram(aes(x=CN)) +
  geom_vline(aes(xintercept=28555783010602)) +
  xlim(10555783010602, 50555783010602)

as.character(cse_treeCN[1])

tmp = tree %>% filter(CN<30555783010602, CN>20555783010602)

hist(tree$CN)


any(tree==cse_treeCN[3], na.rm=T)

for (i in cse_treeCN){
  value_exists = any(tree==i, na.rm=T)
  if (value_exists==T){
    print(paste(value_exists, i))
  }
}


tree==cse_treeCN[1]

cse_treeCN %in% tree

cols = colnames(tree)[1:25] # could these be my tree_CN values from cse?
for (i in colnames(tree)){
  lis = tree %>% pull(i)
  val = unique(cse_treeCN %in% lis)
  if ('TRUE' %in% val){
    print(i)
  }
}
# [1] "PREV_TRE_CN"
# [1] "PREVCOND"
# [1] "DIA"
# [1] "HT"
# [1] "HTCD"
# [1] "ACTUALHT"
# [1] "TREECLCD"
# [1] "CR"
# [1] "CCLCD"
# [1] "TREEGRCD"
# [1] "AGENTCD"

tree %>% nrow()
tree %>% filter(!is.na(CN)) %>% nrow()
tmp = tree %>% filter(!is.na(PREV_TRE_CN))
tmp$CN==tmp$PREV_TRE_CN # so maybe I just have the old TREE_CN values in the CSE data that was shared?

tmp = tree %>% filter(PREV_TRE_CN %in% cse_treeCN)

















str(cond)
str(tree)
# are my TREE_CN values in CO_TREE?
cse_treeCN = unique(cse$TREE_CN)
unique(cse_treeCN %in% tree$CN) # false
unique(cse_treeCN %in% tree$CN) # 


unique(tree$STATECD)
unique(tree$INVYR)
unique(tree$UNITCD)
unique(tree$COUNTYCD)
unique(tree$PLOT)
unique(tree$SUBP)
unique(tree$TREE)

str(tree)

hist(tree$CN)


