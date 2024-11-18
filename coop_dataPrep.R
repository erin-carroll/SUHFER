library(readxl)

setwd("C:/Users/erinc/Desktop/Research/Projects/SUHFER")

# plot23 = read_excel('data/COOP/TaylorPark_PlotData2023_ASCC.xlsx', sheet='Plot_Data') %>%
#   rename(Elev_m='Elev. (m)')
# plot24 = read_excel('data/COOP/TaylorPark_PlotData2024_ASCC.xlsx', sheet='Plot_Data')
# 
# colnames(plot23)==colnames(plot24)
# plot24$`Elev. (ft)` = plot24$`Elev. (ft)`*0.3048
# plot24 = plot24 %>%
#   rename(Elev_m='Elev. (ft)')
# 
# plots = plot23 %>%
#   rbind(plot24)
# 
# # only one site appears to have been visited more than once?
# tmp = plots %>%
#   group_by(`Location #`, `Plot #`) %>%
#   summarize(n_visits=n_distinct(Date))

# check above in trees dataset... 

list.files('data/COOP')

### may not need plot-level data, plot info is included in treedata?

trees22 = read_excel('data/COOP/TaylorPark_TreeData2023_ASCC.xlsx', sheet='FVS_TreeInit')



plot23 = read_excel('data/COOP/TaylorPark_DataInc2022_ASCC.xlsx', sheet='FVS_PlotInit') %>%
  filter(is.na(ID)) %>%
  select(-c(ID, Block, Plot_No, PLOT_ID_old, STANDPLOT_ID, VARIANT, ADDFILES:STANDPLOT_ID_Orig)) %>%
  mutate(STANDPLOT_ID=paste0(STAND_ID, '_P', sprintf('%03d', PLOT_ID)))
tree23 = read_excel('data/COOP/TaylorPark_TreeData2023_ASCC.xlsx', sheet='FVS_TreeInit') %>%
  filter(is.na(ID)) %>%
  select(-c(ID, BLK, TRT, PLOT_ID_old, STAND_ID_Orig, TAG_ID, TREE_COUNT:HISTORY))
  


