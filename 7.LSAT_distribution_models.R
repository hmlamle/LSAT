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
library(exactextractr)

# Fort Lauderdale Rasters: 
slope_FTL     <- terra::rast("C:/Users/hlamle/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/GIS/LSAT/Slope_FTL_down2.tif")
plancurve_FTL <- terra::rast("C:/Users/hlamle/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/GIS/LSAT/PlanCurve_FTL_down2.tif")#, NAflag = -99999)

res(slope_FTL)

# South Canyon Rasters: 
slope_SC     <- terra::rast("C:/Users/hlamle/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/GIS/LSAT/Slope_SC.tif")
plancurve_SC <- terra::rast("C:/Users/hlamle/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/GIS/LSAT/PlanCurve_SC.tif")
# Emerald Reef Rasters: 
slope_ER     <- terra::rast("C:/Users/hlamle/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/GIS/LSAT/Slope_ER.tif")
plancurve_ER <- terra::rast("C:/Users/hlamle/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/GIS/LSAT/PlanCurve_ER.tif")


# ------------------------ Make 50cm Grid -----------------------------


# Trial 1: 

# grid_50cm_ftl = st_make_grid(
#   st_as_sf(as.polygons(slope_FTL)), cellsize = 0.5) %>% # have to change my raster layer into a simple feature first (polygons like a vector layer) for the make grid to work.
#   st_sf(geometry = .) %>%
#   filter(lengths(st_intersects(., slope_FTL)) > 0) 

# Convert raster footprint to sf polygon (only if not already done)
# SC_poly <- st_as_sf(as.polygons(slope_SC))
# 
# # Create 50 cm grid
# grid_50cm_SC <- st_make_grid(SC_poly, cellsize = 0.5) %>% 
#   st_sf() %>% 
#   filter(lengths(st_intersects(., SC_poly)) > 0)


# Get raster extent
# ftl_extent <- ext(slope_FTL)

# Convert extent to bounding box polygon
# ftl_bbox <- st_as_sfc(st_bbox(c(
#   xmin = ftl_extent[1],
#   ymin = ftl_extent[3],
#   xmax = ftl_extent[2],
#   ymax = ftl_extent[4])))

# Trial 2: 

ftl_extent <- ext(slope_FTL)
# 
# ftl_extent
# as.vector(ftl_extent)
# 
# grid_50cm_ftl <- st_make_grid(
#   st_as_sfc(st_bbox(c(
#     xmin = ftl_extent[1],
#     xmax = ftl_extent[2],
#     ymin = ftl_extent[3],
#     ymax = ftl_extent[4]
#   ))),
#   cellsize = 0.5
# ) |> 
#   st_sf()

# Trial 3: 

# --- your extent (xmin, xmax, ymin, ymax) ---
# ftl_extent = ext(-13.1299621486295, 10.0197761374426, -9.69310731918855, 11.2136089237742)
# 
# # 1) Crop the raster to that extent (optional but usually good)
# r_crop = crop(slope_FTL, ftl_extent)
# 
# # 2) Create a 50 x 50 grid raster over that extent
# #    (this defines the grid geometry)
# grid_r = rast(ext = ftl_extent, ncols = 50, nrows = 50, crs = crs(r_crop))  # crs can be NA; just keep consistent
# 
# # 3) Convert grid cells to polygons + add IDs
# grid_v = as.polygons(grid_r)
# values(grid_v) = data.frame(cell_id = 1:ncell(grid_r))
# grid_sf = st_as_sf(grid_v)
# 
# # 4) Extract mean raster value per grid cell (exact area-weighted mean)
# #    Note: exactextractr wants a Raster* object; it works with terra SpatRaster too in recent versions,
# #    but if you hit issues, convert to raster::raster.
# mean_df = exact_extract(r_crop, grid_sf, "mean") %>%
#   mutate(cell_id = grid_sf$cell_id)
# 
# grid_out = grid_sf %>%
#   left_join(mean_df, by = "cell_id")


# Make 50 cm grid
grid_50cm_FTL <- st_make_grid(ftl_extent, cellsize = 0.5, crs = crs(slope_FTL)) %>% 
  st_as_sf() %>% 
  rename(geom = x)

crs(slope_FTL)
crs(grid_50cm_FTL)

st_crs(grid_50cm_FTL) == st_crs(slope_FTL)

# chatgpt extent check: 
ggplot() +
  geom_sf(data = st_as_sf(as.polygons(ext(slope_FTL)), crs = st_crs(slope_FTL)), fill = NA, color = "blue") +
  geom_sf(data = st_as_sfc(st_bbox(grid_50cm_FTL)), fill = NA, color = "red") +
  coord_sf()

## changing CRS: -----------------


# Assign the correct CRS (meters) — example EPSG:32631 (UTM zone 31N)
crs(slope_FTL) <- "EPSG:32631"

# Make the grid in the same CRS
grid_50cm_FTL <- st_set_crs(st_sf(st_make_grid(ftl_extent, cellsize = 0.5)), st_crs(slope_FTL))
crs(grid_50cm_FTL)

r_ext <- st_as_sf(as.polygons(ext(slope_FTL)))
r_ext <- st_set_crs(r_ext, st_crs(slope_FTL))

ggplot() +
  geom_sf(data = r_ext, fill = NA, color = "blue") +
  geom_sf(data = grid_50cm_FTL, fill = NA, color = "red", linetype = "dashed") +
  coord_sf() +
  theme_minimal()

crs(slope_FTL) <- st_crs(grid_50cm_FTL)$wkt
crs(plancurve_FTL) <- st_crs(grid_50cm_FTL)$wkt


# ---------------------- Make Simple Features to store data -----------

# Use the 50cm grid as your buffer/grid
FTL_sf <- tibble(grid = 1:nrow(grid_50cm_FTL),  # unique ID per grid cell
                    site = 'FTL') |> 
  mutate(
    slope_mean_true = exact_extract(slope_FTL, grid_50cm_FTL, 'mean', progress = FALSE),
    plan_curve_true = exact_extract(plancurve_FTL, grid_50cm_FTL, 'mean', progress = FALSE)
  )   
  # mutate(across(c(slope_mean, plan_curve_mean), ~if_else(.x < 0, 0, .x)))  # replace any tiny negatives with 0

df = grid_50cm_FTL %>% 
  mutate(site = 'FTL',
         grid = row_number(),
         slope_mean_true = exact_extract(slope_FTL, grid_50cm_FTL, 'mean', progress = FALSE),
         plan_curve_true = exact_extract(plancurve_FTL, grid_50cm_FTL, 'mean', progress = FALSE)) 


ggplot()+
  geom_sf(data = df %>% filter(!is.nan(plan_curve_true)), aes(fill = plan_curve_true))

# BREAK: 3/2/26 12PM 
# Can now plot the bounding box and grid and it almost matches up 
# creating simple feature with exact extract still returning zeroes at all grids (see below)
# > fivenum(FTL_sf$slope_mean_true)
# [1] 0 0 0 0 0
# > fivenum(FTL_sf$plan_curve_true)
# [1] 0 0 0 0 0

# -------------------- Predict models across ortho: -----------------

# Stopped here on 3/1/26 at 11:41pm. 
# Everything is working according to plan, but the values used in the model 
# are z-scored and needs to be back transformed before continuing.
# plus for predicted turf model it can't find the object plan_curve check the 
# name I used. 

FTL_sf <- df %>%
  mutate(slope_mean = (slope_mean_true - 39.33403) / 6.613804) %>%
  mutate(plan_curve = (plan_curve_true - -6270.369) / 39516.62)

FTL_sf$pred_sediment <- predict(
  best_sed_50_nndr,       # your fitted model
  newdata = FTL_sf,       # the grid with predictor values
  type = "response"       # gives prediction on original scale
)

FTL_sf$pred_turf <- predict(
  best_turf_50_nndr,
  newdata = FTL_sf,
  type = "response"
)

# ---------------- Plot predicted rasters using ggplot: -----------------

# ggplot(ftl_sf) +
#   geom_sf(aes(fill = pred_sediment), color = NA) +  # no polygon borders
#   scale_fill_viridis_c(option = "plasma") +        # nice color scale
#   labs(fill = "Predicted Sediment Depth (mm)") +
#   theme_minimal()
# 
# 
# ggplot(ftl_sf) +
#   geom_sf(aes(fill = pred_turf), color = NA) +
#   scale_fill_viridis_c(option = "magma") +
#   labs(fill = "Predicted Turf Length (cm)") +
#   theme_minimal()

ggplot()+
  geom_sf(data = FTL_sf %>% filter(!is.nan(pred_sediment)), aes(fill = pred_sediment), color = NA)+
  theme_minimal() +
  scale_fill_gradient(
    low = "#EDEBEB",
    high = "#990000",
    name = "Predicted\nSediment"
  ) +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())

ggsave("figs/sed_expansion.png", units="in", width=8, height=6, dpi=600)

ggplot()+
  geom_sf(data = FTL_sf %>% filter(!is.nan(pred_turf)), aes(fill = pred_turf), color = NA)+
  theme_minimal() +
  scale_fill_gradient(
    low = "#EDEBEB",
    high = "#990000",
    name = "Predicted\nTurf"
  ) +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())

ggsave("figs/turf_expansion.png", units="in", width=8, height=6, dpi=600)
