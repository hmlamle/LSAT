# Hannah-Marie Lamle
# LSAT Distribution Modelling
# Using predicted relationships created from script 3 to project LSAT onto LAIs
# Creation date of this document: 3/1/2025
# Modified From James et al 2025 E-scapes paper: 
# https://github.com/CoastalFishScience/James_etal_sgE-scape_GCB/tree/main

# -------------------- load libraries & Data ------------------------------
library(tidyverse)
library(sf)
library(stars)
library(gstat)
library(automap)
library(terra)
library(glmmTMB)
library(ggplot2)

# Fort Lauderdale Rasters: 
slope_FTL     <- terra::rast("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/GIS/LSAT/Slope_FTL.tif")
plancurve_FTL <- terra::rast("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/GIS/LSAT/PlanCurve_FTL.tif")

res(slope_FTL)

# South Canyon Rasters: 
slope_SC     <- terra::rast("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/GIS/LSAT/Slope_SC.tif")
plancurve_SC <- terra::rast("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/GIS/LSAT/PlanCurve_SC.tif")

# Emerald Reef Rasters: 
slope_ER     <- terra::rast("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/GIS/LSAT/Slope_ER.tif")
plancurve_ER <- terra::rast("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/GIS/LSAT/PlanCurve_ER.tif")


# ------------------------ Make 50cm Grid -----------------------------

# Fort Lauderdale: 

# grid_50cm_ftl = st_make_grid(
#   st_as_sf(as.polygons(slope_FTL)), cellsize = 0.5) %>% # have to change my raster layer into a simple feature first (polygons like a vector layer) for the make grid to work.
#   st_sf(geometry = .) %>%
#   filter(lengths(st_intersects(., slope_FTL)) > 0) 

# Convert raster footprint to sf polygon (only if not already done)
ftl_poly <- st_as_sf(as.polygons(slope_FTL))

# Create 50 cm grid
grid_50cm_ftl <- st_make_grid(ftl_poly, cellsize = 0.5) %>% 
  st_sf() %>% 
  filter(lengths(st_intersects(., ftl_poly)) > 0)


# ---------------------- Make Simple Features to store data -----------

# Use the 50cm grid as your buffer/grid
ftl_sf <- tibble(grid = 1:nrow(grid_50cm_ftl),  # unique ID per grid cell
                    site = 'FTL') |> 
  mutate(
    slope_mean = exact_extract(slope_FTL, grid_50cm_ftl, 'mean', progress = FALSE)[[1]],
    plan_curve_mean = exact_extract(plancurve_FTL, grid_50cm_ftl, 'mean', progress = FALSE)[[1]]
  ) |> 
  mutate(across(c(slope_mean, plan_curve_mean), ~if_else(.x < 0, 0, .x)))  # replace any tiny negatives with 0

# -------------------- Predict models across ortho: -----------------


ftl_sf$pred_sediment <- predict(
  best_sed_50_nndr,       # your fitted model
  newdata = ftl_sf,       # the grid with predictor values
  type = "response"       # gives prediction on original scale
)

ftl_sf$pred_turf <- predict(
  best_turf_50_nndr,
  newdata = ftl_sf,
  type = "response"
)

# ---------------- Plot predicted rasters using ggplot: -----------------

ggplot(ftl_sf) +
  geom_sf(aes(fill = pred_sediment), color = NA) +  # no polygon borders
  scale_fill_viridis_c(option = "plasma") +        # nice color scale
  labs(fill = "Predicted Sediment Depth (mm)") +
  theme_minimal()


ggplot(ftl_sf) +
  geom_sf(aes(fill = pred_turf), color = NA) +
  scale_fill_viridis_c(option = "magma") +
  labs(fill = "Predicted Turf Length (cm)") +
  theme_minimal()