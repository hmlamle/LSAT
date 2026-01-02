# Hannah-Marie Lamle
# LSAT habitat suitability modelling
# Creation date of this document: 11/6/2025

# READ ME: 
# This document prepares and combines all of the variables for modelling into a 
# master document from:   
## - Viscore rugosity outputs (25/50/100 cm)
## - ArcGIS slope metrics (25/50/100 cm)
## - Biological LSAT data (turf length, sediment depth)



library(tidyverse)
library(readxl)
library(dplyr)

# ======================= BASE DIRECTORY ================================

base_dir <- "C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/raw/LSAT"


# ======================= SITE LOOKUP TABLE =============================
# NOTE: As more data is collected, this table should be amended to include
# unique site/date combination. 

sites <- tibble::tribble(
  ~site_name,         ~site_code, ~survey_date, ~n_points,
  "Fortlauderdale",   "FTL",      "20250721",   24,
  "Southcanyon",      "SC",       "20231020",   25,
  "Emerald",          "ER",       "20250622",   12,
  "NNDR",             "NNDR",     "20251010",   25
)

scales <- c(25, 50, 100)   # measurement boxes


# ======================== PATH GENERATORS ==============================

## ----- Rugosity (Viscore) -----------

get_rugosity_folder <- function(site_name, site_code, date, scale_cm) {
  file.path(
    base_dir,
    "2. Viscore rugosity metrics",
    site_name,
    paste0(site_code, "_", date),
    paste0(scale_cm, "cm box")
  )
}

## --- Slope rasters (ArcGIS) -------------
get_slope_file <- function(site_name, site_code, date, scale_cm) {
  file.path(
    base_dir,
    "3. GIS rugosity metrics",
    site_name,
    paste0(site_code, "_", date),
    paste0("slope_", scale_cm, "cm.csv")
  )
}

## --- SAPR rasters (ArcGIS) -------------
get_sapr_file <- function(site_name, site_code, date, scale_cm) {
  file.path(
    base_dir,
    "3. GIS rugosity metrics",
    site_name,
    paste0(site_code, "_", date),
    paste0(scale_cm, "cm_SAPR.csv")
  )
}

## --- Plan Curvature rasters (ArcGIS) -------------
get_plan_curve_file <- function(site_name, site_code, date, scale_cm) {
  file.path(
    base_dir,
    "3. GIS rugosity metrics",
    site_name,
    paste0(site_code, "_", date),
    paste0("plan_curve_", scale_cm, "cm.csv")
  )
}

## --- Standard Curvature rasters (ArcGIS) -------------
get_std_curve_file <- function(site_name, site_code, date, scale_cm) {
  file.path(
    base_dir,
    "3. GIS rugosity metrics",
    site_name,
    paste0(site_code, "_", date),
    paste0("std_curve_", scale_cm, "cm.csv")
  )
}

## --- TPI rasters (ArcGIS) -------------
get_tpi_file <- function(site_name, site_code, date, scale_cm) {
  file.path(
    base_dir,
    "3. GIS rugosity metrics",
    site_name,
    paste0(site_code, "_", date),
    paste0("TPI_", scale_cm, "cm.csv")
  )
}

## --- Biological LSAT data (turf + sediment) --------
get_bio_file <- function(site_name, site_code, date) {
  file.path(
    base_dir,
    "4. Biological metrics",
    site_name,
    paste0(site_code, "_", date),
    paste0(site_code, "_", date, "_lsat.xlsx")
  )
}


## =============== READ & PROCESS RUGOSITY DATA (25/50/100 cm) =================


read_rugosity <- function() {
  
  purrr::pmap_dfr(sites, function(site_name, site_code, survey_date, n_points) {
    
    purrr::map_dfr(scales, function(s) {
      
      folder <- get_rugosity_folder(site_name, site_code, survey_date, s)
      files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
      
      if (length(files) == 0) {
        warning("No rugosity files found for ", site_name, " scale ", s)
        return(NULL)
      }
      
      data_list <- purrr::map(files, ~read_csv(.x, 
                                               col_names = c("spacing", "transect", "sample", "x", "y", "z"),
                                               show_col_types = FALSE))
      
      purrr::map2_dfr(data_list, seq_along(data_list), ~{
        
        df <- .x
        nail_id <- as.character(.y)
        
        # Drop z = 0 rows — treat as missing, NOT corruption
        df2 <- df %>% filter(z != 0)
        
        # If all rows were 0 → no usable data
        if (nrow(df2) == 0) {
          warning(glue::glue(
            "⚠ Nail {nail_id} at {site_name}, scale {s} has all z=0; no rugosity computed."
          ))
          
          return(tibble(
            rugo_mean = NA_real_,
            rugo_var  = NA_real_,
            avg_point_range = NA_real_,
            nail = nail_id,
            scale_cm = s,
            site = site_name,
            site_code = site_code,
            date = survey_date
          ))
        }
        
        # If <2 points left → can’t compute diff()
        if (nrow(df2) < 2) {
          warning(glue::glue(
            "⚠ Nail {nail_id} at {site_name}, scale {s} has <2 valid rows (after removing z=0)."
          ))
          
          return(tibble(
            rugo_mean = NA_real_,
            rugo_var  = NA_real_,
            avg_point_range = NA_real_,
            nail = nail_id,
            scale_cm = s,
            site = site_name,
            site_code = site_code,
            date = survey_date
          ))
        }
        
        # Compute rugosity safely
        dx <- diff(df2$x)
        dy <- diff(df2$y)
        dz <- diff(df2$z)
        
        standard_length <- sum(sqrt(dx^2 + dy^2))
        true_length     <- sum(sqrt(dx^2 + dy^2 + dz^2))
        
        rugosity <- ifelse(standard_length == 0, NA_real_,
                           true_length / standard_length)
        
        point_range <- max(df2$z) - min(df2$z)
        
        tibble(
          rugo_mean = rugosity,
          rugo_var  = var(c(rugosity)),  # placeholder, leave NA if you prefer
          avg_point_range = point_range,
          nail = nail_id,
          scale_cm = s,
          site = site_name,
          site_code = site_code,
          date = survey_date
        )
      })
    })
  })
}




## ================== READ & PROCESS SLOPE DATA (25/50/100 cm) =================

read_slope <- function() {
  
  purrr::pmap_dfr(sites, function(site_name, site_code, survey_date, n_points) {
    
    purrr::map_dfr(scales, function(s) {
      
      file <- get_slope_file(site_name, site_code, survey_date, s)
      
      if (!file.exists(file)) {
        warning("Missing slope file: ", file)
        return(NULL)
      }
      
      read_csv(file, col_types = cols(), show_col_types = FALSE) %>%
        rename(
          nail = OBJECTID_1,
          slope_mean = MEAN,
          slope_var = STD
        ) %>%
        mutate(
          nail = as.character(nail),
          scale_cm = s,
          site = site_name,
          site_code = site_code,
          date = survey_date
        ) %>%
        select(nail, slope_mean, slope_var, site, site_code, date, scale_cm)
      
    })
  })
}

## ================== READ & PROCESS SAPR DATA (25/50/100 cm) =================

read_sapr <- function() {
  
  purrr::pmap_dfr(sites, function(site_name, site_code, survey_date, n_points) {
    
    purrr::map_dfr(scales, function(s) {
      
      file <- get_sapr_file(site_name, site_code, survey_date, s)
      
      if (!file.exists(file)) {
        warning("Missing SAPR file: ", file)
        return(NULL)
      }
      
      read_csv(file, col_types = cols(), show_col_types = FALSE) %>%
        rename(
          nail = Box_ID,
          surface_area_2d = Surface_Area_2D,
          surface_area_3d = Surface_Area_3D
        ) %>%
        # Calculate SAPR:
        mutate(
          sapr = surface_area_3d / surface_area_2d,
          nail = as.character(nail),
          scale_cm = s,
          site = site_name,
          site_code = site_code,
          date = survey_date
        ) %>%
        select(nail, sapr, site, site_code, date, scale_cm)
      
    })
  })
}

## ========== READ & PROCESS STANDARD CURVATURE DATA (25/50/100 cm) ==========

read_std_curve <- function() {
  
  purrr::pmap_dfr(sites, function(site_name, site_code, survey_date, n_points) {
    
    purrr::map_dfr(scales, function(s) {
      
      file <- get_std_curve_file(site_name, site_code, survey_date, s)
      
      if (!file.exists(file)) {
        warning("Missing standard curve file: ", file)
        return(NULL)
      }
      
      read_csv(file, col_types = cols(), show_col_types = FALSE) %>%
        rename(
          nail = OBJECTID,
          std_curve = MEAN,
        ) %>%
        mutate(
          nail = as.character(nail),
          scale_cm = s,
          site = site_name,
          site_code = site_code,
          date = survey_date
        ) %>%
        select(nail, std_curve, site, site_code, date, scale_cm)
      
    })
  })
}

## ========== READ & PROCESS PLAN CURVATURE DATA (25/50/100 cm) ==========

read_plan_curve <- function() {
  
  purrr::pmap_dfr(sites, function(site_name, site_code, survey_date, n_points) {
    
    purrr::map_dfr(scales, function(s) {
      
      file <- get_plan_curve_file(site_name, site_code, survey_date, s)
      
      if (!file.exists(file)) {
        warning("Missing plan curve file: ", file)
        return(NULL)
      }
      
      read_csv(file, col_types = cols(), show_col_types = FALSE) %>%
        rename(
          nail = OBJECTID,
          plan_curve = MEAN,
        ) %>%
        mutate(
          nail = as.character(nail),
          scale_cm = s,
          site = site_name,
          site_code = site_code,
          date = survey_date
        ) %>%
        select(nail, plan_curve, site, site_code, date, scale_cm)
      
    })
  })
}

## ========== READ & PROCESS TPI DATA (25/50/100 cm) ==========

read_TPI <- function() {
  
  purrr::pmap_dfr(sites, function(site_name, site_code, survey_date, n_points) {
    
    purrr::map_dfr(scales, function(s) {
      
      file <- get_tpi_file(site_name, site_code, survey_date, s)
      
      if (!file.exists(file)) {
        warning("Missing TPI file: ", file)
        return(NULL)
      }
      
      read_csv(file, col_types = cols(), show_col_types = FALSE) %>%
        rename(
          nail = OBJECTID,
          tpi = MEAN,
        ) %>%
        mutate(
          nail = as.character(nail),
          scale_cm = s,
          site = site_name,
          site_code = site_code,
          date = survey_date
        ) %>%
        select(nail, tpi, site, site_code, date, scale_cm)
      
    })
  })
}

## ==================== READ BIOLOGICAL LSAT DATA =====================

read_bio <- function() {
  
  purrr::pmap_dfr(sites, function(site_name, site_code, survey_date, n_points) {
    
    file <- get_bio_file(site_name, site_code, survey_date)
    
    if (!file.exists(file)) {
      warning("Missing biological LSAT file: ", file)
      return(NULL)
    }
    
    readxl::read_excel(file) %>%
      mutate(
        site = site_name,
        site_code = site_code,
        date = survey_date
      )
    
  })
}


## ==================== EXECUTE READS ==================================

rugosity_raw   <- read_rugosity()
slope_raw      <- read_slope()
sapr_raw       <- read_sapr()
std_curve_raw  <- read_std_curve()
plan_curve_raw <- read_plan_curve()
tpi_raw        <- read_TPI()
bio_raw        <- read_bio()


## ===================== CLEANING + FINAL MERGE ========================

# You can adjust column renaming here depending on your real biological data column names:

bio_clean <- bio_raw %>%
  rename(
    turf_length = turf_length_mm,
    sediment_depth = sed_depth_mm,
    nail = Plot
  )
bio_clean$nail <- as.character(bio_clean$nail)


# Join morphometrics by site, date, scale, nail
rs_joined <- rugosity_raw %>%
  full_join(slope_raw, by = c("site", "site_code", "date", "scale_cm", "nail")) %>%
  full_join(sapr_raw, by = c("site", "site_code", "date", "scale_cm", "nail")) %>%
  full_join(std_curve_raw, by = c("site", "site_code", "date", "scale_cm", "nail")) %>%
  full_join(plan_curve_raw, by = c("site", "site_code", "date", "scale_cm", "nail")) %>%
  full_join(tpi_raw, by = c("site", "site_code", "date", "scale_cm", "nail")) %>%
  select(-avg_point_range)  # remove this column if it exists

# Join biological data by site, date, nail (no scale for biology)
master_lsat <- left_join(rs_joined, bio_clean, 
                         by = c("site", "site_code", "date", "nail"))
master_lsat <- master_lsat %>%
  select(site, site_code, date, scale_cm, nail, sediment_depth, turf_length, rugo_mean, slope_mean, slope_var, sapr, std_curve, plan_curve, tpi)


## ======================= WRITE FINAL DATASET============================

write_csv(master_lsat, "C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/toUse/LSAT/master_LSAT_dataset.csv")

message("✅ Master LSAT dataset created successfully!")

