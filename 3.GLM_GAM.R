# Hannah-Marie Lamle
# LSAT habitat suitability modelling
# Combine all data to create GAM and GLMs 
# Creation date of this document: 11/6/2025

library(tidyverse)
library(MASS)
library(broom)
library(AICcmodavg)
library(MuMIn)
library(glmmTMB)
library(performance)
library(purrr)
library(dplyr)
library(bbmle)
library(ggplot2)
library(ggeffects)

# --------------------- Prepare Clean Data -----------------------

# Add small constant to zero responses so Gamma(log) works
scaled_master <- scaled_master %>%
  mutate(
    turf_length     = ifelse(turf_length == 0, 0.001, turf_length),
    sediment_depth  = ifelse(sediment_depth == 0, 0.001, sediment_depth)
  )

responses <- c("turf_length", "sediment_depth")
scales    <- c("25","50","100")

# --------------------- Model Formulas ----------------------------

model_configs <- tribble(
  ~model_type,     ~formula,
  "all_vars",      "rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi",
  #"no_tpi",        "rugo_mean + slope_mean + sapr + std_curve + plan_curve",
  #"no_std_curve",  "rugo_mean + slope_mean + sapr + plan_curve + tpi"
)

# --------------------- Create Model Grid -------------------------

model_grid <- expand_grid(
  scale = as.numeric(scales),
  response = responses,
  model_type = model_configs$model_type
)

# ------------------ Function to Fit Gamma GLM --------------------

fit_gamma_model <- function(scale, response, model_type) {
  
  df <- scaled_master %>% filter(scale_cm == as.numeric(scale))
  
  # get RHS
  rhs <- model_configs$formula[model_configs$model_type == model_type]
  fml <- as.formula(paste(response, "~", rhs))
  
  mod <- tryCatch(
    glmmTMB(fml, data = df, family = Gamma(link = "log")),
    error = function(e) NULL
  )
  if (is.null(mod)) return(NULL)
  
  tibble(
    scale = scale,
    response = response,
    model_type = model_type,
    n = nrow(df),
    AIC = AIC(mod),
    AICc = AICc(mod),
    logLik = as.numeric(logLik(mod)),
    deviance = mod$deviance,
    model = list(mod)
  )
}


fit_tweedie_model <- function(scale, response, model_type) {
  
  df <- scaled_master %>% filter(scale_cm == as.numeric(scale))
  
  # get RHS
  rhs <- model_configs$formula[model_configs$model_type == model_type]
  fml <- as.formula(paste(response, "~", rhs))
  
  mod <- tryCatch(
    glmmTMB(fml, data = df, family = tweedie(link ="log")),
    error = function(e) NULL
  )
  if (is.null(mod)) return(NULL)
  
  tibble(
    scale = scale,
    response = response,
    model_type = model_type,
    n = nrow(df),
    AIC = AIC(mod),
    AICc = AICc(mod),
    logLik = as.numeric(logLik(mod)),
    deviance = mod$deviance,
    model = list(mod)
  )
}

# ------------------ Fit All Models -------------------------------

results_gamma <- pmap_dfr(model_grid, fit_gamma_model)
print(results_gamma %>% arrange(response, scale, model_type))

results_tweedie <- pmap_dfr(model_grid, fit_tweedie_model)
print(results_tweedie %>% arrange(response, scale, model_type))

## Where is the 25cm and 50cm turf length models? 
# check to see if they run or fail by themselves: 
### --------------- RUN GLMS USING glmmTMB PACKAGE!!! -------------

# Gina's next steps: 
  # run check_model(my model) to get the performance plots for each one 
    # we are thinking there will be overdispersion.... there's a way but cross that bridge
  # dredge every global model 

# ------------------ Plot Model Performance -----------------------


# Gamma family:
gamma_turf25   <- results_gamma$model[[1]] # 25cm turf length
gamma_sed25    <- results_gamma$model[[2]] # 25cm sediment depth
gamma_turf50   <- results_gamma$model[[3]] # 50cm turf length
gamma_sed50    <- results_gamma$model[[4]] # 50cm sediment depth
gamma_turf100  <- results_gamma$model[[5]] # 100cm turf length
gamma_sed100   <- results_gamma$model[[6]] # 100cm sediment depth

# Tweedie family:
tweedie_turf25   <- results_tweedie$model[[1]] # 25cm turf length
tweedie_sed25    <- results_tweedie$model[[2]] # 25cm sediment depth
tweedie_turf50   <- results_tweedie$model[[3]] # 50cm turf length
tweedie_sed50    <- results_tweedie$model[[4]] # 50cm sediment depth
tweedie_turf100  <- results_tweedie$model[[5]] # 100cm turf length
tweedie_sed100   <- results_tweedie$model[[6]] # 100cm sediment depth


df_scaled <- scaled_master %>%
  group_by(scale_cm) %>%
  nest() %>%
  mutate(m_sed = map(data, \(data) glmmTMB(sediment_depth ~ 
                                             rugo_mean + slope_mean + 
                                             sapr + std_curve + plan_curve + 
                                             tpi, data = data, 
                                           family = tweedie(link = "log"))), 
         m_turf =  map(data, \(data) glmmTMB(turf_length ~ 
                                               rugo_mean + slope_mean + 
                                               sapr + std_curve + plan_curve + 
                                               tpi, data = data, 
                                             family = tweedie(link = "log"))))
                              
df_scaled$m_sed[[1]] # double check and make sure it works, it does 
df_scaled$m_turf[[1]]

check_model(df_scaled$m_sed[[1]])
check_model(df_scaled$m_sed[[2]])
check_model(df_scaled$m_sed[[3]]) # now the standard curve and tpi are in the moderate range for colinearity 
check_model(df_scaled$m_turf[[1]])
check_model(df_scaled$m_turf[[2]])
check_model(df_scaled$m_turf[[3]])

AIC(gamma_sed25, tweedie_sed25)
AIC(gamma_sed50, tweedie_sed50)
AIC(gamma_sed100, tweedie_sed100)
AIC(gamma_turf25, tweedie_turf25)
AIC(gamma_turf50, tweedie_turf50)
AIC(gamma_turf100, tweedie_turf100)

## Tweedie wins!  -------------------


# -------------- Dredge all tweedie global models  ------------------------

df25 <- scaled_master %>%
  filter(scale_cm =="25")
df50 <- scaled_master %>%
  filter(scale_cm =="50")
df100 <- scaled_master %>%
  filter(scale_cm =="100")

options(na.action = "na.fail")

### 25cm and sediment depth: -------------------------
sed_25 <- glmmTMB(sediment_depth ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                  data = df25, family = tweedie(link ="log"))
dredge_25_sed <- dredge(sed_25, rank = "AICc")
subset(dredge_25_sed, delta <= 4)

# select the best model:
best_sed_25 <- get.models(dredge_25_sed, subset = 9)[[1]]

# NO NNDR: 

no_nndr <- master[!(master$nail =="5" | master$nail =="6" & master$site =="NNDR" ),] %>%
  filter(!site_code == "NNDR")
no_nndr_scaled <- scaled_master %>%
  filter(!site_code == "NNDR")

df25_nndr <- no_nndr_scaled %>%
  filter(scale_cm =="25")
df50_nndr <- no_nndr_scaled %>%
  filter(scale_cm =="50")
df100_nndr <- no_nndr_scaled %>%
  filter(scale_cm =="100")

sed_25_nndr <- glmmTMB(sediment_depth ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                  data = df25_nndr, family = tweedie(link ="log"))
dredge_25_sed_nndr <- dredge(sed_25_nndr, rank = "AICc")
subset(dredge_25_sed_nndr, delta <= 4)

best_sed_25 <- get.models(dredge_25_sed_nndr, subset = 10)[[1]]

### 50cm and sediment depth: -------------------------
sed_50 <- glmmTMB(sediment_depth ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                  data = df50, family = tweedie(link ="log"))
dredge_50_sed <- dredge(sed_50, rank = "AICc")
subset(dredge_50_sed, delta <= 4)

# select the best model:
best_sed_50 <- get.models(dredge_50_sed, subset = 3)[[1]]


# NO NNDR: 

sed_50_nndr <- glmmTMB(sediment_depth ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                       data = df50_nndr, family = tweedie(link ="log"))
dredge_50_sed_nndr <- dredge(sed_50_nndr, rank = "AICc")
subset(dredge_50_sed_nndr, delta <= 4)

best_sed_50 <- get.models(dredge_50_sed_nndr, subset = 5)[[1]]

### 100cm and sediment depth: ------------------------
sed_100 <- glmmTMB(sediment_depth ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                  data = df100, family = tweedie(link ="log"))
dredge_100_sed <- dredge(sed_100, rank = "AICc")
subset(dredge_100_sed, delta <= 4)

# just use rugosity for the best model for sed 100. 

# select the best model:
best_sed_100 <- get.models(dredge_100_sed, subset = 1)[[1]]

# NO NNDR: 

sed_100_nndr <- glmmTMB(sediment_depth ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                       data = df100_nndr, family = tweedie(link ="log"))
dredge_100_sed_nndr <- dredge(sed_100_nndr, rank = "AICc")
subset(dredge_100_sed_nndr, delta <= 4)

best_sed_100 <- get.models(dredge_100_sed_nndr, subset = 1)[[1]]

### 25cm and Turf Length: ----------------------------
turf_25 <- glmmTMB(turf_length ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                  data = df25, family = tweedie(link ="log"))
dredge_25_turf <- dredge(turf_25, rank = "AICc")
subset(dredge_25_turf, delta <= 4)

check_model(turf_25)

# select the best model:
best_turf_25 <- get.models(dredge_25_turf, subset = 2)[[1]]

# NO NNDR: 

turf_25_nndr <- glmmTMB(turf_length ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                        data = df25_nndr, family = tweedie(link ="log"))
dredge_turf_25_nndr <- dredge(turf_25_nndr, rank = "AICc")
subset(dredge_turf_25_nndr, delta <= 4)

# select the best model:
best_turf_25 <- get.models(dredge_turf_25_nndr, subset = 1)[[1]]

### 50cm and Turf Length: ---------------------------
turf_50 <- glmmTMB(turf_length ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                  data = df50, family = tweedie(link ="log"))
dredge_50_turf <- dredge(turf_50, rank = "AICc")
subset(dredge_50_turf, delta <= 4)

# select the best model:
best_turf_50 <- get.models(dredge_50_turf, subset = 5)[[1]]

# NO NNDR:

turf_50_nndr <- glmmTMB(turf_length ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                        data = df50_nndr, family = tweedie(link ="log"))
dredge_50_turf_nndr <- dredge(turf_50_nndr, rank = "AICc")
subset(dredge_50_turf_nndr, delta <= 4)

# select the best model:
best_turf_50 <- get.models(dredge_50_turf_nndr, subset = 1)[[1]]

### 100cm and Turf Length: --------------------------
turf_100 <- glmmTMB(turf_length ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                   data = df100, family = tweedie(link ="log"))
dredge_100_turf <- dredge(turf_100, rank = "AICc")
subset(dredge_100_turf, delta <= 4)

# select the best model:
best_turf_100 <- get.models(dredge_100_turf, subset = 2)[[1]]

# NO NNDR:

turf_100_nndr <- glmmTMB(turf_length ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                        data = df100_nndr, family = tweedie(link ="log"))
dredge_100_turf_nndr <- dredge(turf_100_nndr, rank = "AICc")
subset(dredge_100_turf_nndr, delta <= 4)

best_turf_100 <- get.models(dredge_100_turf_nndr, subset = 6)[[1]]

# ------------------- Comparison of scale for most parsimonious -----------
# comparing best dredged model (i have to pick)

## ------ Turf Length ----------

library(bbmle)
AICtab(best_turf_25, best_turf_50, best_turf_100)

## -------- Sediment Depth -----------

AICtab(best_sed_25, best_sed_50, best_sed_100)



## ---------- Trying to get r^2 ----------------

# sediment depth: 25 
observed <- df25$sediment_depth
predicted <- predict(best_sed_25, type = "response")

pseudo_r2 <- cor(observed, predicted)^2
print(pseudo_r2)

# sediment depth: 50 
observed <- df50$sediment_depth
predicted <- predict(best_sed_50, type = "response")

pseudo_r2 <- cor(observed, predicted)^2
print(pseudo_r2)

# sediment depth: 100
observed <- df100$sediment_depth
predicted <- predict(best_sed_100, type = "response")

pseudo_r2 <- cor(observed, predicted)^2
print(pseudo_r2)


# Turf Length: 25
observed <- df25$turf_length
predicted <- predict(best_turf_25, type = "response")

pseudo_r2 <- cor(observed, predicted)^2
print(pseudo_r2)

# Turf Length: 50
observed <- df50$turf_length
predicted <- predict(best_turf_50, type = "response")

pseudo_r2 <- cor(observed, predicted)^2
print(pseudo_r2)

# Turf Length: 100
observed <- df100$turf_length
predicted <- predict(best_turf_100, type = "response")

pseudo_r2 <- cor(observed, predicted)^2
print(pseudo_r2)

## Trying other ways to get an r^2 value?? 

compare_performance(best_turf_100, best_turf_50, best_turf_25)
deviance(best_sed_100)


# ------------------- Marginal Effects plots ----------------------
# only do the best one for sediment depth and turf length 

## Turf Length ---------------
# best model was 50cm : turf length ~ planform curvature + mean slope 

# planform curvature: 
orig_breaks <- c(-75000, -50000, -25000, 0, 25000)
z_breaks <- (orig_breaks - -6270.369) / 39516.62

pred_turf_planform <- ggpredict(best_turf_50, terms = "plan_curve") #[-0.72:4.8549,by= 0.1]
turf_planform <- plot(pred_turf_planform) +
  ggtitle(" ") +
  coord_cartesian(ylim = c(0, 15)) +
  theme_classic()+
  labs(y = "Predicted Turf Length",
       x = "Planform Curvature")+
  scale_x_continuous(
    breaks = z_breaks,   
    labels = function(z) round(z * 39516.62 + -6270.369, 2))+
  theme(axis.title = element_text(size = 14), 
        axis.text.y = element_text(size = 14, colour = "black"), 
        axis.text.x = element_text(size = 14, colour = "black"),
        plot.title = element_text(size = 14, hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 12))
print(turf_planform)

# slope:
orig_breaks <- c(20, 25, 30, 35, 40, 45, 50)   # original values you want
z_breaks <- (orig_breaks - 39.33403) / 6.613804

pred_turf_slope_mean <- ggpredict(best_turf_50, terms = "slope_mean")
turf_slope <- plot(pred_turf_slope_mean) +
  ggtitle(" ") +
  coord_cartesian(ylim = c(0, 15)) +
  theme_classic()+
  labs(y = " ",
       x = "Mean Slope")+
  scale_x_continuous(
    breaks = z_breaks,   
    labels = function(z) round(z * 6.613804 + 39.33403, 1))+
  theme(axis.title = element_text(size = 14), 
        axis.text.y = element_text(size = 14, colour = "black"), 
        axis.text.x = element_text(size = 14, colour = "black"),
        plot.title = element_text(size = 14, hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 12))
print(turf_slope)


(turf_planform | turf_slope)
ggsave("figs/turf_predict.png", units="in", width=8, height=6, dpi=600)
  

## Sediment Depth ---------------

# Slope: 
orig_breaks <- c(20, 25, 30, 35, 40, 45, 50)   # original values you want

z_breaks <- (orig_breaks - 39.33403) / 6.613804

pred_sed_slope_mean <- ggpredict(best_sed_50, terms = "slope_mean") #[-0.72:4.8549,by= 0.1]
sed_slope <- plot(pred_sed_slope_mean) +
  ggtitle(" ") +
  coord_cartesian(xlim = c(-2, 1.4074)) +
  theme_classic()+
  labs(y = "Predicted Sediment Depth",
       x = "Mean Slope")+
  scale_x_continuous(
    breaks = z_breaks,   
    labels = function(z) round(z * 6.613804 + 39.33403, 2))+
  theme(axis.title = element_text(size = 14), 
        axis.text.y = element_text(size = 14, colour = "black"), 
        axis.text.x = element_text(size = 14, colour = "black"),
        plot.title = element_text(size = 14, hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 12))
print(sed_slope)

ggsave("figs/sed_predict.png", units="in", width=8, height=6, dpi=600) # better way to save figs 


# ------------------------ Top Predictors across Scales ----------------------

# sediment depth models
p_sed_25  <- ggpredict(sed_25,  terms = "rugo_mean") %>% mutate(scale = "25 cm")
p_sed_50  <- ggpredict(sed_50,  terms = "rugo_mean") %>% mutate(scale = "50 cm")
p_sed_100 <- ggpredict(sed_100, terms = "rugo_mean") %>% mutate(scale = "100 cm")

sed_all <- bind_rows(p_sed_25, p_sed_50, p_sed_100)

# turf length models
p_turf_25  <- ggpredict(turf_25,  terms = "rugo_mean") %>% mutate(scale = "25 cm")
p_turf_50  <- ggpredict(turf_50,  terms = "rugo_mean") %>% mutate(scale = "50 cm")
p_turf_100 <- ggpredict(turf_100, terms = "rugo_mean") %>% mutate(scale = "100 cm")

turf_all <- bind_rows(p_turf_25, p_turf_50, p_turf_100)

plot_sed <- ggplot(sed_all, aes(x = x, y = predicted, color = scale)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = scale),
              alpha = 0.12, color = NA) +
  labs(
    x = "Rugosity (rugo_mean)",
    y = "Predicted Sediment Depth",
    color = "Buffer Scale",
    fill = "Buffer Scale",
    title = "Sediment Depth ~ Rugosity Across Buffer Scales"
  ) +
  theme_bw(base_size = 14)

plot_turf <- ggplot(turf_all, aes(x = x, y = predicted, color = scale)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = scale),
              alpha = 0.12, color = NA) +
  labs(
    x = "Rugosity (rugo_mean)",
    y = "Predicted Turf Length",
    color = "Buffer Scale",
    fill = "Buffer Scale",
    title = "Turf Length ~ Rugosity Across Buffer Scales"
  ) +
  theme_bw(base_size = 14)

plot_sed / plot_turf

