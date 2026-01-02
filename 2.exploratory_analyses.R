# Hannah-Marie Lamle
# LSAT habitat suitability modelling
# Creation date of this document: 11/6/2025

# Exploratory Analysis Script for LSAT, Rugosity, Slope, 
# and Related Habitat Variables

library(tidyverse)
library(janitor)
library(ggpubr)
library(GGally)
library(sf)
library(ggplot2)
library(dplyr)
library(gridExtra)  # for grid.arrange (optional)
library(patchwork) 
library(purrr)
library(mgcv)
library(broom)
library(dplyr)

# --------------------- 1. Load Data -----------------------------------

master <- read_csv("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/toUse/LSAT/master_LSAT_dataset.csv") %>%
  clean_names()

# Quick structure 
glimpse(master)
summary(master)

# ----------------- 2. Check distributions --------------------------------------------------------

## Predictor Variables: ----------------

num_vars <- master %>% dplyr::select(sediment_depth, turf_length, rugo_mean, slope_mean, sapr, std_curve, plan_curve, tpi)

# Histograms
num_vars %>%
  gather(variable, value) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

# Density plots
num_vars %>%
  gather(variable, value) %>%
  ggplot(aes(value)) +
  geom_density() +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

## Response variables: ------------------

hist(master$turf_length, breaks = 40)
hist(master$sediment_depth, breaks = 40)

# ---------------- 3. Correlation inspection ----------------------------

# Correlation matrix (numeric only)
ggcor <- ggcorr(num_vars, label = TRUE, label_round = 2)
print(ggcor)

# Pairs plot 
GGally::ggpairs(num_vars)

ggplot(d100, aes(x = rugo_mean, y = sapr)) +
  geom_line()

# ----------------- 4. Site differences -----------------------------

# Boxplots comparing sites for major variables
vars_of_interest <- c("rugo_mean", "rugo_var", "sapr", "std_curve", "plan_curve",
                      "slope_mean", "slope_var", "tpi")

for (v in vars_of_interest) {
  p <- clean_master %>%
    ggplot(aes(x = site_code, y = .data[[v]], fill = site)) +
    geom_boxplot() +
    theme_bw() +
    ggtitle(paste("Site comparison:", v))
  print(p)
}

rugo <- no_nndr %>%
  ggplot(aes(x = site_code, y = rugo_mean, fill = site)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle(paste("Rugosity"))
print(rugo)

sapr <- no_nndr %>%
  ggplot(aes(x = site_code, y = sapr, fill = site)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle(paste("SAPAR"))
print(sapr)

slope <- no_nndr %>%
  ggplot(aes(x = site_code, y = slope_mean, fill = site)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle(paste("Slope"))
print(rugo)

std_curve <- no_nndr %>%
  ggplot(aes(x = site_code, y = std_curve, fill = site)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle(paste("Standard Curvature"))
print(std_curve)

plan_curve <- no_nndr %>%
  ggplot(aes(x = site_code, y = plan_curve, fill = site)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle(paste("Planform Curvature"))
print(plan_curve)

TPI <- no_nndr %>%
  ggplot(aes(x = site_code, y = tpi, fill = site)) +
  geom_boxplot() +
  theme_bw() +
  ggtitle(paste("TPI"))
print(TPI)

(rugo | sapr) / (std_curve | plan_curve) / (TPI | slope)

combined_plot <- (rugo + sapr + std_curve + plan_curve + TPI + slope) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")   # optional (you can choose "right", etc.)

combined_plot
ggsave("figs/boxplots.png", units="in", width=7, height=6, dpi=600)

## Predictors and LSAT scatterplots -------

### Turf Length vs Rugosity 
p <- master %>%
  filter(scale_cm %in% scales) %>%
  ggplot(aes(x = rugo_mean, y = turf_length, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  facet_wrap(~ scale_cm, nrow = 1, ncol = 3, scales = "fixed") +  # facets in one row, 3 columns
  labs(title = "Turf Length vs Rugosity", x = "Rugosity Mean", y = "Turf Length") +
  theme(plot.title = element_text(hjust = 0.5))

print(p)

### Sediment Depth vs Rugosity 
p <- master %>%
  filter(scale_cm %in% scales) %>%
  ggplot(aes(x = rugo_mean, y = sediment_depth, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  facet_wrap(~ scale_cm, nrow = 1, ncol = 3, scales = "fixed") +  # facets in one row, 3 columns
  labs(title = "Sediment Depth vs Rugosity", x = "Rugosity Mean", y = "Sediment Depth") +
  theme(plot.title = element_text(hjust = 0.5))

print(p)

### Turf Length vs Slope 
p <- master %>%
  filter(scale_cm %in% scales) %>%
  ggplot(aes(x = slope_mean, y = turf_length, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  facet_wrap(~ scale_cm, nrow = 1, ncol = 3, scales = "fixed") +  # facets in one row, 3 columns
  labs(title = "Turf Length vs Slope", x = "Slope Mean", y = "Turf Length") +
  theme(plot.title = element_text(hjust = 0.5))

print(p)

### Sediment Depth vs Slope 
p <- master %>%
  filter(scale_cm %in% scales) %>%
  ggplot(aes(x = slope_mean, y = sediment_depth, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  facet_wrap(~ scale_cm, nrow = 1, ncol = 3, scales = "fixed") +  # facets in one row, 3 columns
  labs(title = "Sediment Depth vs Slope", x = "Slope Mean", y = "Sediment Depth") +
  theme(plot.title = element_text(hjust = 0.5))

print(p)


# Turf Length vs SAPR
p_sapr_turf <- master %>%
  filter(scale_cm %in% scales) %>%
  ggplot(aes(x = sapr, y = turf_length, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  facet_wrap(~ scale_cm, nrow = 1, ncol = 3, scales = "fixed") +
  labs(title = "Turf Length vs SAPR", x = "Surface Area to Planar Area Ratio (SAPR)", y = "Turf Length") +
  theme(plot.title = element_text(hjust = 0.5))

print(p_sapr_turf)


# Sediment Depth vs SAPR
p_sapr_sed <- master %>%
  filter(scale_cm %in% scales) %>%
  ggplot(aes(x = sapr, y = sediment_depth, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  facet_wrap(~ scale_cm, nrow = 1, ncol = 3, scales = "fixed") +
  labs(title = "Sediment Depth vs SAPR", x = "Surface Area to Planar Area Ratio (SAPR)", y = "Sediment Depth") +
  theme(plot.title = element_text(hjust = 0.5))

print(p_sapr_sed)


# Turf Length vs Standard Curvature
p_std_curve_turf <- master %>%
  filter(scale_cm %in% scales) %>%
  ggplot(aes(x = std_curve, y = turf_length, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  facet_wrap(~ scale_cm, nrow = 1, ncol = 3, scales = "fixed") +
  labs(title = "Turf Length vs Standard Curvature", x = "Standard Curvature", y = "Turf Length") +
  theme(plot.title = element_text(hjust = 0.5))

print(p_std_curve_turf)


# Sediment Depth vs Standard Curvature
p_std_curve_sed <- master %>%
  filter(scale_cm %in% scales) %>%
  ggplot(aes(x = std_curve, y = sediment_depth, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  facet_wrap(~ scale_cm, nrow = 1, ncol = 3, scales = "fixed") +
  labs(title = "Sediment Depth vs Standard Curvature", x = "Standard Curvature", y = "Sediment Depth") +
  theme(plot.title = element_text(hjust = 0.5))

print(p_std_curve_sed)


# Turf Length vs Plan Curvature
p_plan_curve_turf <- master %>%
  filter(scale_cm %in% scales) %>%
  ggplot(aes(x = plan_curve, y = turf_length, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  facet_wrap(~ scale_cm, nrow = 1, ncol = 3, scales = "fixed") +
  labs(title = "Turf Length vs Plan Curvature", x = "Plan Curvature", y = "Turf Length") +
  theme(plot.title = element_text(hjust = 0.5))

print(p_plan_curve_turf)


# Sediment Depth vs Plan Curvature
p_plan_curve_sed <- master %>%
  filter(scale_cm %in% scales) %>%
  ggplot(aes(x = plan_curve, y = sediment_depth, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  facet_wrap(~ scale_cm, nrow = 1, ncol = 3, scales = "fixed") +
  labs(title = "Sediment Depth vs Plan Curvature", x = "Plan Curvature", y = "Sediment Depth") +
  theme(plot.title = element_text(hjust = 0.5))

print(p_plan_curve_sed)


# Turf Length vs TPI
p_tpi_turf <- master %>%
  filter(scale_cm %in% scales) %>%
  ggplot(aes(x = tpi, y = turf_length, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  facet_wrap(~ scale_cm, nrow = 1, ncol = 3, scales = "fixed") +
  labs(title = "Turf Length vs TPI", x = "Topographic Position Index (TPI)", y = "Turf Length") +
  theme(plot.title = element_text(hjust = 0.5))

print(p_tpi_turf)


# Sediment Depth vs TPI
p_tpi_sed <- master %>%
  filter(scale_cm %in% scales) %>%
  ggplot(aes(x = tpi, y = sediment_depth, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw() +
  facet_wrap(~ scale_cm, nrow = 1, ncol = 3, scales = "fixed") +
  labs(title = "Sediment Depth vs TPI", x = "Topographic Position Index (TPI)", y = "Sediment Depth") +
  theme(plot.title = element_text(hjust = 0.5))

print(p_tpi_sed)




# ----------------- 5. Check for Outliers -------------------------

## Boxplots ---------------------------
for (v in vars_of_interest) {
  p <- master %>%
    ggplot(aes(y = .data[[v]])) +
    geom_boxplot() +
    theme_bw() +
    ggtitle(paste(v, " summary"))
  print(p)
}

# There appears to be some outliers in rugosity, sapr, std curve, plan curve, 
# tpi, and maaayyybe slope?

is_outlier <- function(x, lower = 0.01, upper = 0.99) {
  lo <- quantile(x, lower, na.rm = TRUE)
  hi <- quantile(x, upper, na.rm = TRUE)
  x < lo | x > hi
}

# Identify which values are outliers (TRUE/FALSE per cell)
outlier_mask <- sapply(master[vars_of_interest], is_outlier)

# Combine mask with the original dataset (optional)
outlier_flags <- cbind(master, as.data.frame(outlier_mask))

# Filter rows that contain any TRUE
rows_with_outliers <- outlier_flags[rowSums(outlier_mask) > 0, ] 

rows_with_outliers

# There is a few outliers that are mostly coming from the NNDR site. This kinda 
# makes sense tho, since that site is SO structurally complex compared to the 
# three other sites that are super flat...

# but let's remove and run the models with and without the outliers removed 
# to see if those being left out does make a difference in model significance

# clean_master <- master %>%
#  filter(
#    between(tpi, quantile(tpi, 0.01, na.rm = TRUE), quantile(tpi, 0.99, na.rm = TRUE)),
#    between(std_curve, quantile(std_curve, 0.01, na.rm = TRUE), quantile(std_curve, 0.99, na.rm = TRUE)),
#    between(plan_curve, quantile(plan_curve, 0.01, na.rm = TRUE), quantile(plan_curve, 0.99, na.rm = TRUE)),
#    between(rugo_mean, quantile(rugo_mean, 0.01, na.rm = TRUE), quantile(rugo_mean, 0.99, na.rm = TRUE)),
#    between(slope_mean, quantile(tpi, 0.01, na.rm = TRUE), quantile(slope_mean, 0.99, na.rm = TRUE)),
#    between(sapr, quantile(sapr, 0.01, na.rm = TRUE), quantile(sapr, 0.99, na.rm = TRUE))
#  )

## OUTLIERS UPDATE 11/24/25 ------------------
# Rolo and Ryan said only to remove the TPI outlier at site 5 because it's 
# really the only outlier that makes sense (bad stitching of DEM) and is the
# farthest away. But, I am going to go ahead with what makes the most sense to
# me and remove that whole row of data for all 3 scales and variables

# removal of NNDR points 5 and 6: 

clean_master <- master[!(master$nail =="5" | master$nail =="6" & master$site =="NNDR" ),] 


# --------------------------6. Standardize Data -----------------------

scaled_master <- clean_master %>%
  mutate(across(c(rugo_mean, slope_mean, sapr, std_curve, plan_curve, tpi), scale))

# scaled_not_clean <- master %>%
#  mutate(across(c(rugo_mean, slope_mean, sapr, std_curve, plan_curve, tpi), scale))


# ------------------------- 7. Basic models -----------------------

predictors <- c("rugo_mean", "slope_mean", "sapr", "std_curve", "plan_curve", "tpi")
responses <- c("turf_length", "sediment_depth")

# Create all combinations of response, predictor, scale
model_grid <- expand.grid(response = responses, predictor = predictors, scale = scales)

model_results <- model_grid %>%
  mutate(
    formula = paste(response, "~", predictor),
    data_subset = map(scale, ~ filter(master, scale_cm == .x)),
    model = map2(formula, data_subset, ~ lm(as.formula(.x), data = .y)),
    summary = map(model, summary),
    r_squared = map_dbl(summary, ~ .x$r.squared),
    p_value = map_dbl(summary, ~ coef(.x)[2,4])
  )

print(model_results %>% dplyr::select(response, predictor, scale, r_squared, p_value))


