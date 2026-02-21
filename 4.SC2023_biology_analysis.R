# Hannah-Marie Lamle
# LSAT habitat suitability modelling
# Use the available biological abundance and diversity data to continue modelling LSAT metrics 
# Creation date of this document: 2/20/2026


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
library(readxl)


# ---------------- DATA PREP -----------------------------------------

SC_og <- master_lsat %>%
  filter(site_code == "SC")

SC_bio <- read.csv("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/raw/LSAT/4. Biological metrics/1_Benthic_Com_FL_SED_2023-24.csv")
SC_bio <- SC_bio %>%
  filter(Site == "South Canyon" & Season == "Fall") 


SC_master <- cbind(SC_og, SC_bio) %>%
  dplyr::select(-Season, -Site, -Month, -Day, -Year)

# -------------- GLMs ----------------------------------------------

## -------------------- Abundance ~ Morphometrics ------------------

### 25 cm scale --------------
SC_25 <- SC_master %>%
  filter(scale_cm == 25)

abundance_25 <- glmmTMB(LSAT_Abundance ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                  data = SC_25, 
                  family = tweedie(link ="log"),
                  na.action = "na.fail")
abundance_25 <- dredge(abundance_25, rank = "AICc")
subset(abundance_25, delta <= 4) # null model is best model for LSAT abundance in a 25cm plot and 25cm scale


# select the best model:
best_ab_25 <- get.models(abundance_25, subset = 8)[[1]]


### 50 cm scale --------------
SC_50 <- SC_master %>%
  filter(scale_cm == 50)

abundance_50 <- glmmTMB(LSAT_Abundance ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                        data = SC_50, 
                        family = tweedie(link ="log"),
                        na.action = "na.fail")
abundance_50 <- dredge(abundance_50, rank = "AICc")
subset(abundance_50, delta <= 4) # null model is best model for LSAT abundance in a 50cm plot and 25cm scale


# select the best model:
best_ab_50 <- get.models(abundance_50, subset = 1)[[1]]


### 100 cm scale --------------
SC_100 <- SC_master %>%
  filter(scale_cm == 100)

abundance_100 <- glmmTMB(LSAT_Abundance ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                        data = SC_100, 
                        family = tweedie(link ="log"),
                        na.action = "na.fail")
abundance_100 <- dredge(abundance_100, rank = "AICc")
subset(abundance_100, delta <= 4) # null model is best model for LSAT abundance in a 100cm plot and 25cm scale


# select the best model:
best_ab_100 <- get.models(abundance_100, subset = 7)[[1]]


model = glmmTMB(LSAT_Abundance ~ sapr,
                data = SC_100,
                family = tweedie(link = "log"))
summary(model)
