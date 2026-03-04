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


SC_og <- read.csv("master_LSAT_scaled.csv")
SC_og <- SC_og %>%
  filter(site_code == "SC")

SC_bio <- read.csv("1_Benthic_Com_FL_SED_2023-24.csv")
SC_bio <- SC_bio %>%
  filter(Site == "South Canyon" & Season == "Fall") 
SC_bio$LSAT_Abundance <- SC_bio$LSAT_Abundance / 100


SC_master <- cbind(SC_og, SC_bio) %>%
  dplyr::select(-Season, -Site, -Month, -Day, -Year)

# -------------- GLMs ----------------------------------------------

## -------------------- Abundance ~ Morphometrics ------------------

### 25 cm scale --------------
SC_25 <- SC_master %>%
  filter(scale_cm == 25)

abundance_25 <- glmmTMB(LSAT_Abundance ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                  ziformula = ~1, 
                  data = SC_25, 
                  family = beta_family(link ="logit"),
                  na.action = "na.fail")
abundance_25 <- dredge(abundance_25, rank = "AICc")
subset(abundance_25, delta <= 4) # null model is most parsimonious for LSAT abundance in a 25cm plot and 25cm scale


# select the best model:
best_ab_25 <- get.models(abundance_25, subset = 1)[[1]]


### 50 cm scale --------------
SC_50 <- SC_master %>%
  filter(scale_cm == 50)

abundance_50 <- glmmTMB(LSAT_Abundance ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                        ziformula = ~1,
                        data = SC_50, 
                        family = beta_family(link ="logit"),
                        na.action = "na.fail")
abundance_50 <- dredge(abundance_50, rank = "AICc")
subset(abundance_50, delta <= 4) # null model is best model for LSAT abundance in a 50cm plot and 25cm scale


# select the best model:
best_ab_50 <- get.models(abundance_50, subset = 2)[[1]]


### 100 cm scale --------------
SC_100 <- SC_master %>%
  filter(scale_cm == 100)

abundance_100 <- glmmTMB(LSAT_Abundance ~ rugo_mean + slope_mean + sapr + std_curve + plan_curve + tpi,
                        ziformula = ~1, 
                        data = SC_100, 
                        family = beta_family(link ="logit"),
                        na.action = "na.fail")
abundance_100 <- dredge(abundance_100, rank = "AICc")
subset(abundance_100, delta <= 4) # null model is best model for LSAT abundance in a 100cm plot and 25cm scale


# select the best model:
best_ab_100 <- get.models(abundance_100, subset = 2)[[1]]

## ------------------- Comparison of scale for most parsimonious -----------
# comparing best dredged model (i have to pick)

library(bbmle)

AICtab(best_ab_25, best_ab_50, best_ab_100) # only for South Canyon
# 25cm scale is the winner! 

# --------------------------- Plot Figs ----------------------------

orig_breaks <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5)
z_breaks <- (orig_breaks - 1.665161) / 0.8671408

  

pred_abundance <- ggpredict(best_ab_25, terms = "rugo_mean") 
abundance_rugo <- plot(pred_abundance) +
  ggtitle("") +
  coord_cartesian(xlim = c(-0.6, -0.1904662)) +
  theme_classic()+
  labs(y = "Predicted LSAT % Cover",
       x = "Mean Rugosity") +
  scale_x_continuous(
    breaks = z_breaks,
    labels = function(z) round(z * 0.8671408 + 1.665161, 2))+
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
print(abundance_rugo)

ggsave("figs/pcover_predict.png", units="in", width=8, height=6, dpi=600) # better way to save figs 
