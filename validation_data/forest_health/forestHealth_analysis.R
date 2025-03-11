library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(lubridate)
library(corrplot)
library(reshape2)
library(randomForest)
library(caret)
library(keras)

setwd('C:/Users/erinc/Desktop/Research/Projects/SUHFER')

dat = read.csv('data/forest health/plots_spectra.csv') %>%
  mutate(date=paste(month, year)) %>%
  mutate(date=lubridate::my(date),
         plot_type = str_sub(Plot, -1, -1))
plots = read.csv('data/forest health/forest_health_plots.csv')
trees = read.csv('data/forest health/forest_health_trees_plot-level_20250301.csv') %>%
  mutate(pct_healthy = n_healthy/n_aspen,
         pct_declining = n_declining/n_aspen,
         pct_dead = (n_recent_dead + n_snag)/n_aspen,
         pct_dead_or_declining = (n_recent_dead + n_snag + n_declining)/n_aspen,
         pct_live = (n_healthy+n_declining)/n_aspen,
         plot_type = str_sub(Plot, -1, -1),
         year_ = Year)
trees$year_[trees$year_ %in% c(2007, 2008)] = '2007-2008'
trees$year_[trees$year_ %in% c(2020, 2021, 2022)] = '2020-2022'



length(unique(plots$Plot))
length(unique(plots$plot_number))

# filter to plots at least 75% aspen
keep_plots = trees %>%
  filter(FLAG_lessthan75pctaspen==0) %>%
  group_by(Plot) %>%
  summarize(years=n()) %>%
  filter(years>2) %>%
  pull(Plot) %>%
  unique()

# time series of forest health data for each plot
for (i in 1:length(keep_plots)){
  ggplot(data=trees %>%
           filter(Plot %in% keep_plots[i]) %>%
           pivot_longer(cols=pct_healthy:pct_live, names_to='metric', values_to='value') %>%
           filter(metric %in% c('pct_healthy', 'pct_dead_or_declining'))) +
    geom_point(aes(x=Year, y=value)) +
    ylim(0,1) +
    facet_wrap(~metric) +
    labs(title=sprintf('%s', keep_plots[i]))
}

# just 316s
ggplot(data=trees %>%
         filter(Plot=='316S') %>%
         pivot_longer(cols=pct_healthy:pct_live, names_to='metric', values_to='value') %>%
         filter(metric %in% c('pct_healthy', 'pct_dead_or_declining'))) +
  geom_point(aes(x=Year, y=value)) +
  ylim(0,1) +
  facet_wrap(~metric) +
  labs(title='316S') + 
  geom_smooth(aes(x=Year, y=value), method='lm', se=TRUE)



# histogram of pct_d_or_d by plot type for each year
ggplot(data=trees %>%
         filter(Plot %in% keep_plots)) +
  geom_histogram(aes(x=pct_dead_or_declining, fill=plot_type),position='identity', alpha=0.5) +
  scale_fill_manual(values=c('darkgreen','red')) +
  facet_wrap(~year_) +
  theme(legend.position = 'bottom')

# step 1 - just remake the same plots using spectra, not in situ data 

# first, how correlated are ndmi, lwc? 
cor(dat$NDMI, dat$lwc, use='complete.obs') # 0.6858162

# time series of spectral predictors for each plot
for (i in 1:length(keep_plots)){
  p = ggplot(data=dat %>%
           filter(Plot %in% keep_plots[i]) %>%
           pivot_longer(cols=NDVI:lwc, names_to='metric', values_to='value')) +
    geom_point(aes(x=date, y=value)) +
    facet_wrap(~metric, scale='free_y') +
    labs(title=sprintf('%s', keep_plots[i]))
  print(p)
}

# just 316S
ggplot(data=dat %>%
         filter(Plot=='316S') %>%
         pivot_longer(cols=NDVI:lwc, names_to='metric', values_to='value')) +
  geom_point(aes(x=date, y=value, color=factor(month, levels=c('jul','aug','sep')))) +
  facet_wrap(~metric, scale='free_y') +
  labs(title='316S') +
  theme(legend.title=element_blank(),
        legend.position='bottom') +
  geom_smooth(method='lm', aes(x=date, y=value, color=factor(month, levels=c('jul','aug','sep'))), se=F)

# histogram of spectral predictor by plot type for each year
ggplot(data=dat %>%
         filter(Plot %in% keep_plots) %>%
         group_by(Plot, year, plot_type) %>%
         summarize(NDVI=max(NDVI))) +
  geom_histogram(aes(x=NDVI, fill=plot_type),position='identity', alpha=0.5) +
  scale_fill_manual(values=c('darkgreen','red')) +
  facet_wrap(~year) +
  theme(legend.position = 'bottom')

###### STILL TO DO!! also make map #############
# to do - for several plots, visualize NDVI vs. year, % healthy vs. year, etc. (basically what i already did for 316S, but all together and for multiple plots)

# correlations of change over time
plot_trends = trees %>%
  filter(Plot %in% keep_plots, Year>2008) %>%
  group_by(Plot) %>%
  summarize(slope_pct_healthy=coef(lm(pct_healthy~Year))[2],
            slope_pct_declining=coef(lm(pct_declining~Year))[2],
            slope_pct_dead=coef(lm(pct_dead~Year))[2],
            slope_pct_dead_or_declining=coef(lm(pct_dead_or_declining~Year))[2],
            slope_pct_live=coef(lm(pct_live~Year))[2])

plot_trends_spectra = dat %>%
  filter(Plot %in% keep_plots, year<2023) %>%
  group_by(Plot) %>%
  summarize(slope_NDVI = coef(lm(NDVI~year))[2],
            slope_NDMI = coef(lm(NDMI~year))[2],
            slope_lwc = coef(lm(lwc~year))[2])

plot_trends = plot_trends %>%
  left_join(plot_trends_spectra, by='Plot')

df = plot_trends %>% select(-plot)
cor_matrix <- cor(df, use = "complete.obs")
cor.mtest <- function(mat) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  colnames(p.mat) <- colnames(mat)
  rownames(p.mat) <- colnames(mat)
  
  diag(p.mat) <- 0  # Self-correlation p-values set to 0
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      test <- cor.test(mat[, i], mat[, j])
      p.mat[i, j] <- test$p.value
      p.mat[j, i] <- test$p.value  # Ensure symmetry
    }
  }
  return(p.mat)
}
p_matrix <- cor.mtest(df)
label_matrix <- matrix(NA, nrow = ncol(df), ncol = ncol(df))
for (i in 1:ncol(df)) {
  for (j in 1:ncol(df)) {
    label_matrix[i, j] <- sprintf("%.2f (%.2f)", cor_matrix[i, j], p_matrix[i, j])
  }
}
rownames(label_matrix) <- colnames(df)
colnames(label_matrix) <- colnames(df)
subset_rows <- 1:5
subset_cols <- tail(colnames(df), 5)
cor_matrix_subset <- cor_matrix[subset_rows, subset_cols]
p_matrix_subset <- p_matrix[subset_rows, subset_cols]
label_matrix_subset <- label_matrix[subset_rows, subset_cols]
corrplot(cor_matrix_subset, method = "color", 
         tl.col = "black",        # Make axis labels black
    )
corr_coords_subset <- expand.grid(1:length(subset_cols), 1:length(subset_rows))  # Properly map row & col order
corr_coords_subset$Var2 <- rev(corr_coords_subset$Var2)  # Reverse Y-axis to match `corrplot()`
p_values <- as.numeric(gsub(".*\\((.*)\\)", "\\1", as.vector(t(label_matrix_subset))))  # Extract p-values
for (i in seq_along(p_values)) {
  font_weight <- ifelse(p_values[i] < 0.1, 2, 1)  # Bold if p < 0.05, otherwise normal
  
  text(corr_coords_subset$Var1[i], corr_coords_subset$Var2[i], 
       labels = as.vector(t(label_matrix_subset))[i], 
       col = "black", cex = 0.7, font = font_weight)  # Correct font application
}

#######################################
## predict declining / improving / stable
###########################################

# first assign values to plots based on FHP data
hist(plot_trends$slope_pct_dead)
stable_threshold = 0.01

# seriously declining category?
plot_trends$status_true = 'stable'
plot_trends$status_true[plot_trends$slope_pct_dead < -stable_threshold] = 'improving'
plot_trends$status_true[plot_trends$slope_pct_dead > stable_threshold] = 'declining'
plot_trends$status_true = factor(plot_trends$status_true, levels=c('declining', 'stable', 'improving'))

# then try to predict those categories as a function of NDVI, NDMI?

# over the period 2013 - 202x
ggplot(data=plot_trends) +
  geom_bar(aes(x=status_true, fill=status_true))

## first just with NDMI thresholds
ggplot(plot_trends)+
  geom_histogram(aes(x=slope_NDMI, fill=status_true), position='identity', alpha=0.5)
stable_threshold = 0
plot_trends$status_predicted1 = 'stable'
plot_trends$status_predicted1[plot_trends$slope_NDMI < -stable_threshold] = 'declining'
plot_trends$status_predicted1[plot_trends$slope_NDMI > stable_threshold] = 'improving'
plot_trends$status_predicted1 = factor(plot_trends$status_predicted1, levels=c('declining', 'stable', 'improving'))
confusionMatrix(plot_trends$status_predicted1, plot_trends$status_true)

# if NDMI and NDVI are declining, it's declining
# then re-define decline more conservatively
plot_trends = plot_trends %>%
  mutate(status_true2=if_else(slope_pct_dead>0, 'declining', 'not declining'),
         status_predicted2=if_else((slope_NDMI<0)&(slope_NDVI<0), 'declining', 'not declining')) %>%
  mutate(status_true2=factor(status_true2, levels=c('declining', 'not declining')),
         status_predicted2=factor(status_predicted2, levels=c('declining', 'not declining')))
confusionMatrix(plot_trends$status_predicted2, plot_trends$status_true2)



# super simple - just use a random forest to predict status a function of the trend lines?
set.seed(1234)
# train = createFolds(plot_trends$status_true, k=10, list=F)
train = createDataPartition(plot_trends$status_true, p=0.7, list=F)
train_data <- plot_trends[train, ] %>% select(slope_NDVI, slope_NDMI, status_true)
test_data  <- plot_trends[-train, ] %>% select(slope_NDVI, slope_NDMI, status_true)

rf_model <- randomForest(status_true ~ ., 
                         data = train_data, 
                         ntree = 500,  # Number of trees
                         importance = TRUE)  # Compute variable importance

print(rf_model)
predictions <- predict(rf_model, test_data)
conf_matrix <- confusionMatrix(predictions, test_data$status_true)
print(conf_matrix)
