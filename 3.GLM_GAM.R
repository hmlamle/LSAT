# Hannah-Marie Lamle
# LSAT habitat suitability modelling
# Combine all data to create GAM and GLMs 
# Creation date of this document: 11/6/2025

library(dplyr)
library(MASS)
library(ggplot2)
library(purrr)
library(broom)
library(MuMIn)

# ------------- Global Models based on scale ---------------------------

# create new dataframes at each scale: 
d25  <- scaled_master %>% filter(scale_cm == "25")
d50  <- scaled_master %>% filter(scale_cm == "50")
d100 <- scaled_master %>% filter(scale_cm == "100")
df25_outliers <- scaled_not_clean %>%
  filter(scale_cm == "25")
df50_outliers <- scaled_not_clean %>%
  filter(scale_cm == "50")
df100_outliers <- scaled_not_clean %>%
  filter(scale_cm == "100")

# Making lists for each of the components:
datasets <- list(clean = scaled_master, dirty = scaled_not_clean)
responses <- c("turf_length", "sediment_depth")
scales <- c("25", "50", "100")


# Create all combinations in a tibble
model_grid <- purrr::cross_df(list(
  dataset = names(datasets),
  response = responses,
  scale = scales
))

# Function to fit model for each combination
fit_nb_glm <- function(dataset_name, response_var, scale_val) {
  data <- datasets[[dataset_name]] %>% filter(scale_cm == scale_val)
  
  # Build formula dynamically
  formula <- as.formula(paste0(response_var, " ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi"))
  
  # Fit Negative Binomial GLM (MASS package)
  mod <- tryCatch(
    MASS::glm.nb(formula, data = data),
     error = function(e) {
      message(glue::glue("Model failed for {dataset_name}, {response_var}, scale {scale_val}: {e$message}"))
      return(NULL)
    }
  )
  
  if (is.null(mod)) return(NULL)
  
  # Tidy model output for quick summary
  tidy_mod <- broom::tidy(mod)
  glance_mod <- broom::glance(mod)
  
  # Return list with model and summaries
  list(
    model = mod,
    tidy = tidy_mod,
    glance = glance_mod,
    dataset = dataset_name,
    response = response_var,
    scale = scale_val,
    n = nrow(data)
  )
}

# Fit models for all combinations
model_results <- pmap(model_grid, fit_nb_glm)

# Remove failed fits (NULLs)
model_results <- compact(model_results)

# Optional: Convert results to a dataframe with summaries only for easy comparison
summary_df <- map_dfr(model_results, function(x) {
  tibble(
    dataset = x$dataset,
    response = x$response,
    scale = x$scale,
    n = x$n,
    AIC = x$glance$AIC,
    logLik = as.numeric(x$glance$logLik),  # coerce to numeric
    deviance = x$glance$deviance
  )
})


print(summary_df)

# The clean dataset modelling sediment depth at the 25cm scale 
# performed the best... that is pretty neat! 

## ----------------- Removing colinear variables -----------------

library(AICcmodavg)


# --- Prepare base datasets ----
datasets <- list(clean = scaled_master, dirty = scaled_not_clean)
scales <- c("25", "50", "100")
responses <- c("turf_length", "sediment_depth")

# --- Model configurations ----
model_configs <- tribble(
  ~model_type,       ~formula,
  "tpi_no_std",      "rugo_mean + slope_mean + sapr + plan_curve + tpi",
  "std_no_tpi",      "rugo_mean + slope_mean + sapr + plan_curve + std_curve"
)

# --- Create all combinations ----
model_grid <- expand_grid(
  dataset_name = names(datasets),
  scale = scales,
  response = responses,
  model_type = model_configs$model_type
)

# --- Function to fit one model ----
fit_nb_model <- function(dataset_name, scale, response, model_type) {
  df <- datasets[[dataset_name]] %>%
    filter(scale_cm == scale)
  
  # Get formula string from config table
  formula_rhs <- model_configs$formula[model_configs$model_type == model_type]
  formula <- as.formula(paste(response, "~", formula_rhs))
  
  # Fit model safely
  model <- tryCatch(glm.nb(formula, data = df),
                    error = function(e) NULL)
  if (is.null(model)) return(NULL)
  
  tibble(
    dataset = dataset_name,
    scale = scale,
    response = response,
    model_type = model_type,
    n = nrow(df),
    AIC = AIC(model),
    AICc = AICc(model),
    logLik = as.numeric(logLik(model)),
    deviance = model$deviance
  )
}

# --- Apply to all combinations ----
comparison_table <- pmap_dfr(model_grid, fit_nb_model)


# --- View tidy table of all models ----
comparison_table %>%
  arrange(response, dataset, scale, model_type) %>%
  print(n = 36)


print(min(summary_df$AIC)) #241 from clean, sed. depth, 25cm
print(min(comparison_table$AIC)) #239 from clean, sed. depth, 25cm AND no standard curve values

# --------------------- Dredge the best model ------------------------

clean25 <- scaled_master %>% filter(scale_cm == "25")

# Fit the best base model again (just to be explicit)
best_model <- glm.nb(
  sediment_depth ~ rugo_mean + slope_mean + sapr + plan_curve + tpi,
  data = clean25
)

# Run dredge on that model
options(na.action = "na.fail")  # required for dredge to work
dredge_results <- dredge(best_model, rank = "AICc")

# View the model ranking
print(dredge_results)

# Extract the top model
best_dredged_model <- get.models(dredge_results, subset = 1)[[1]]

# Summarize it
summary(best_dredged_model)
