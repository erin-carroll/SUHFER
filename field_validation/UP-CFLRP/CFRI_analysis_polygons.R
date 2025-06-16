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

# load data
dat = read.csv('data/UP-CFRLP/CFRI_plots_spectra_polygons.csv') %>%
  mutate(date=paste(month, year)) %>%
  mutate(date=lubridate::my(date)) %>%
  pivot_wider(names_from=metric, values_from=c(min, mean, max, std))
plots = read.csv('data/UP-CFRLP/CFRI_plots.csv') %>%
  rename(plot=Plot_name)
# subset field sampling to time points relevant to time window of spectral data (2018 - )
trees = read.csv('data/UP-CFRLP/CFRI_trees_plot-level_CJPNotes.csv') %>%
  rename(plot=PLOT)

# sanity check, everyone has the same name
sort(unique(dat$plot)) == sort(unique(plots$plot)) 
sort(unique(plots$plot)) == sort(unique(trees$plot))

# again, how correlated are ndmi, lwc?
cor(dat$max_NDMI, dat$max_lwc, use='complete.obs') # 0.5960665

##########################
## visualizations
##########################

# will come back to this after correlations

##########################
## Corrplot
##########################

sort(unique(trees$Year))
plot_trends = trees %>%
  # filter(PRE.POST=='POST') %>%
  # filter(Year>2016) %>% # time series 2017-2023 # nevermind - this limits data too majorly! Lose half of the already small dataset because first year (making any possible time series) is pre-2017
  group_by(plot) %>%
  summarize(slope_pct_ba_standing_live=coef(lm(pct_ba_standing_live~Year))[2],
            slope_pct_ba_dead=coef(lm(pct_ba_dead~Year))[2],
            slope_pct_ba_dead_or_down=coef(lm(pct_ba_dead_or_down~Year))[2],
            slope_standing_live_ba_density=coef(lm(standing_live_ba_density~Year))[2],
            slope_dead_ba_density=coef(lm(dead_ba_density~Year))[2],
            slope_dead_or_down_ba_density=coef(lm(dead_or_down_ba_density~Year))[2])

sort(unique(dat$year))
plot_trends_spectra = dat %>%
  filter(year<2024) %>%
  group_by(plot) %>%
  summarize(slope_max_NDVI = coef(lm(max_NDVI~year))[2],
            slope_max_NDMI = coef(lm(max_NDMI~year))[2],
            slope_min_NDVI = coef(lm(min_NDVI~year))[2],
            slope_min_NDMI = coef(lm(min_NDMI~year))[2],
            slope_mean_NDVI = coef(lm(mean_NDVI~year))[2],
            slope_mean_NDMI = coef(lm(mean_NDMI~year))[2],
            slope_std_NDVI = coef(lm(std_NDVI~year))[2],
            slope_std_NDMI = coef(lm(std_NDMI~year))[2])

plot_trends = plot_trends %>%
  left_join(plot_trends_spectra, by='plot')

df = plot_trends %>% select(-plot)
colnames(df) = gsub('slope_', '', colnames(df))
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
subset_rows <- c("standing_live_ba_density", "dead_ba_density", "dead_or_down_ba_density")
# subset_cols <- tail(colnames(df), 12)
subset_cols = c("max_NDVI", "max_NDMI", "min_NDVI", "min_NDMI", "mean_NDVI", "mean_NDMI" )
# subset_rows = c("max_NDVI", "max_NDMI", "min_NDVI", "min_NDMI", "mean_NDVI", "mean_NDMI", "std_NDVI", "std_NDMI" )
# subset_cols <- 1:2

cor_matrix_subset <- cor_matrix[subset_rows, subset_cols]
p_matrix_subset <- p_matrix[subset_rows, subset_cols]
label_matrix_subset <- label_matrix[subset_rows, subset_cols]

png('report/fig5, cfri.png', width=2000, height=561, res=300)

corrplot(cor_matrix_subset, method = "color", 
         tl.col = "black", tl.cex=1        # Make axis labels black
)
corr_coords_subset <- expand.grid(1:length(subset_cols), 1:length(subset_rows))  # Properly map row & col order
corr_coords_subset$Var2 <- rev(corr_coords_subset$Var2)  # Reverse Y-axis to match `corrplot()`
p_values <- as.numeric(gsub(".*\\((.*)\\)", "\\1", as.vector(t(label_matrix_subset))))  # Extract p-values
for (i in seq_along(p_values)) {
  font_weight <- ifelse(p_values[i] < 0.1, 2, 1)  # Bold if p < 0.05, otherwise normal
  
  text(corr_coords_subset$Var1[i], corr_coords_subset$Var2[i], 
       labels = as.vector(t(label_matrix_subset))[i], 
       col = "black", cex = 1, font = font_weight)  # Correct font application
}
dev.off()



# other viz
linesize=0.5
pointsize=0.25
textsize=8

ndmi = ggplot(data=plot_trends,
              aes(x=slope_dead_or_down_ba_density, y=slope_mean_NDMI)) +
  geom_point(size=pointsize) +
  geom_smooth(method='lm', se=F, size=linesize) +
  theme_bw(base_size=textsize) +
  theme(legend.position='none')
ndmi

ndvi = ggplot(data=plot_trends,
              aes(x=slope_dead_or_down_ba_density, y=slope_mean_NDVI)) +
  geom_point(size=pointsize) +
  geom_smooth(method='lm', se=F, size=linesize) +
  theme_bw(base_size=textsize) +
theme(legend.position='none')
ndvi

p = ndmi + ndvi + plot_layout(ncol = 2)
p
ggsave(plot=p,
       filename='report/cfri ggplot.png',
       units='in',
       width=6.25,
       height=3,
       dpi=300)

ggplot(data=plot_trends,
       aes(x=slope_pct_dead, y=slope_NDVI, color=plot_type)) +
  geom_point() +
  geom_smooth(method='lm', se=F)




