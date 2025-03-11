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
dat = read.csv('data/UP-CFRLP/CFRI_plots_spectra.csv') %>%
  mutate(date=paste(month, year)) %>%
  mutate(date=lubridate::my(date)) %>%
  rename(plot=Plot_name)
plots = read.csv('data/UP-CFRLP/CFRI_plots.csv') %>%
  rename(plot=Plot_name)
trees = read.csv('data/UP-CFRLP/CFRI_trees_plot-level.csv') %>%
  rename(plot=PLOT)

# sanity check, everyone has the same name
sort(unique(dat$plot)) == sort(unique(plots$plot)) 
sort(unique(plots$plot)) == sort(unique(trees$plot))

# any necessary plot filtering?
# I don't think any is necessary in this case

# again, how correlated are ndmi, lwc?
cor(dat$NDMI, dat$lwc, use='complete.obs') # 0.5898976

##########################
## visualizations
##########################

# will come back to this after correlations

##########################
## Corrplot
##########################

sort(unique(trees$Year))
plot_trends = trees %>%
  # filter(Year>2016) %>% # time series 2017-2023 # nevermind - this limits data too majorly! Lose half of the already small dataset because first year (making any possible time series) is pre-2017
  group_by(plot) %>%
  summarize(slope_pct_standing_live=coef(lm(pct_standing_live~Year))[2],
            slope_pct_down_or_dead=coef(lm(pct_down_or_dead~Year))[2])

sort(unique(dat$year))
plot_trends_spectra = dat %>%
  filter(year<2024) %>%
  group_by(plot) %>%
  summarize(slope_NDVI = coef(lm(NDVI~year))[2],
            slope_NDMI = coef(lm(NDMI~year))[2],
            slope_lwc = coef(lm(lwc~year))[2])

plot_trends = plot_trends %>%
  left_join(plot_trends_spectra, by='plot')

# shoot - forgot to extract data over a larger area.... because 50m plots. Will go back and fix soon.

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
subset_rows <- 1:4
subset_cols <- tail(colnames(df), 3)
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







