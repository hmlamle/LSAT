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

# 1. --------------------- Load Data -----------------------------------

master <- read_csv("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/toUse/LSAT/master_LSAT_dataset.csv") %>%
  clean_names()

# Quick structure ---------------------------------------------------------------
glimpse(master)
summary(master)

# 2. ----------------- Check distributions --------------------------------------------------------

num_vars <- master %>% select(where(is.numeric))

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

# 3. ---------------- Correlation inspection ----------------------------

# Correlation matrix (numeric only)
ggcor <- ggcorr(num_vars, label = TRUE, label_round = 2)
print(ggcor)

# Pairs plot -------------------------------------------------------------------
GGally::ggpairs(num_vars)


# ----------------- 4. Site differences -----------------------------

# Boxplots comparing sites for major variables
vars_of_interest <- c("lsat_cover", "rugosity_25", "rugosity_50", "rugosity_100", 
                      "slope_mean", "slope_var")

for (v in vars_of_interest) {
  p <- master %>%
    ggplot(aes(x = site, y = .data[[v]], fill = site)) +
    geom_boxplot() +
    theme_bw() +
    ggtitle(paste("Site comparison:", v))
  print(p)
}


# 5. ----------------------- LSAT vs predictors scatterplots -------------


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



# ------------------------- 6. Basic models -----------------------

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

print(model_results %>% select(response, predictor, scale, r_squared, p_value))


# Interaction check: rugosity × slope -----------------------------------------

master <- master %>% mutate(scale_cm = as.numeric(scale_cm))
scales <- c(25, 50, 100) # this is already in the environment if you've run script 1 but re-confirming

interaction_turf <- list()

for (s in scales) {
  
  data_sub <- master %>% filter(scale_cm == s)
  
  cat("\nScale:", s, "cm  n =", nrow(data_sub), "\n")
  
  mod <- lm(turf_length ~ rugo_mean * slope_mean, data = data_sub)
  
  print(summary(mod))
}



# store outputs
lm_results <- list()
gam_results <- list()
plots <- list()

# Loop through responses, scales, and predictors --------------------------------
for (resp in responses) {
  for (s in scales) {
    
    data_sub <- master %>% filter(scale_cm == s)
    
    for (pred in predictors) {
      
      # Skip if predictor missing for that scale
      if (!pred %in% names(data_sub)) next
      
      # Linear model ------------------------------------------------------------
      lm_fit <- lm(as.formula(paste(resp, "~", pred)), data = data_sub)
      lm_df <- tidy(lm_fit)
      lm_r2 <- glance(lm_fit)$r.squared
      
      lm_results[[paste(resp, s, pred, sep = "_")]] <- 
        list(scale = s, response = resp, predictor = pred,
             summary = lm_df, r2 = lm_r2)
      
      # GAM model ---------------------------------------------------------------
      gam_fit <- gam(as.formula(paste(resp, "~ s(", pred, ")")),
                     data = data_sub, method = "REML")
      
      gam_df <- tidy(gam_fit)
      gam_r2 <- summary(gam_fit)$r.sq
      
      gam_results[[paste("GAM", resp, s, pred, sep = "_")]] <- 
        list(scale = s, response = resp, predictor = pred,
             summary = gam_df, r2 = gam_r2)
      
      # Plot smoother -----------------------------------------------------------
      p <- ggplot(data_sub, aes_string(x = pred, y = resp, color = "site")) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "loess", se = TRUE) +
        theme_bw() +
        labs(
          title = paste(resp, "vs", pred, "| Scale:", s, "cm"),
          x = pred,
          y = resp
        ) +
        theme(plot.title = element_text(hjust = 0.5))
      
      plots[[paste(resp, s, pred, sep = "_")]] <- p
      
    }
  }
}

# To view results:
lm_results
gam_results

# To view all plots:
for (nm in names(plots)) print(plots[[nm]])

# Example: View a single plot
plots[["turf_length_25_std_curve"]]

# ----------------------- Pooling site, comparing scale --------------------

master$scale_cm <- factor(master$scale_cm, levels = c(25, 50, 100))


# fit model
all_sites_scale <- lm(
  turf_length ~ rugo_mean * scale_cm + slope_mean * scale_cm,
  data = master
)

summary(all_sites_scale)

# prediction grid: vary rugosity, hold slope constant, include all 3 scales
newdata_rugo <- expand.grid(
  rugo_mean = seq(min(master$rugo_mean, na.rm = TRUE),
                  max(master$rugo_mean, na.rm = TRUE),
                  length.out = 100),
  slope_mean = mean(master$slope_mean, na.rm = TRUE),
  scale_cm = factor(c(25, 50, 100), levels = c(25, 50, 100))
)

# correct model name here
newdata_rugo$pred <- predict(all_sites_scale, newdata = newdata_rugo)

# plot
ggplot(master, aes(x = rugo_mean, y = turf_length)) +
  geom_point(alpha = 0.3, color = "gray50") +
  geom_line(data = newdata_rugo,
            aes(y = pred, color = scale_cm),
            size = 1.2) +
  theme_bw() +
  labs(title = "Effect of Rugosity on Turf Across Scales",
       x = "Rugosity",
       y = "Turf Length",
       color = "Scale (cm)")

# prediction grid: vary slope, hold rugosity constant, include all 3 scales
newdata_slope <- expand.grid(
  slope_mean = seq(min(master$slope_mean, na.rm = TRUE),
                   max(master$slope_mean, na.rm = TRUE),
                   length.out = 100),
  rugo_mean = mean(master$rugo_mean, na.rm = TRUE),
  scale_cm = factor(c(25, 50, 100), levels = c(25, 50, 100))
)

# predicted values
newdata_slope$pred <- predict(all_sites_scale, newdata = newdata_slope)

# plot
ggplot(master, aes(x = slope_mean, y = turf_length)) +
  geom_point(alpha = 0.3, color = "gray50") +
  geom_line(data = newdata_slope,
            aes(y = pred, color = scale_cm),
            size = 1.2) +
  theme_bw() +
  labs(title = "Effect of Slope on Turf Across Scales",
       x = "Slope",
       y = "Turf Length",
       color = "Scale (cm)")

