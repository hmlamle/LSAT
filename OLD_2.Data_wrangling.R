# LSAT data visualization 
# Hannah-Marie Lamle
# 5/21/2024
## Update 3/12/2025
## Update 9/4/2025
## Update 11/6/2025 (remove linear regression from this file, make one dataframe for all sites)
# Script 2

library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(patchwork)
library(ggpubr)
library(grid)

# ------------------------- Load GIS & biological data ------------------------


# bring in slope data from GIS: 
slope25 <- read.csv("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/raw/LSAT/3. GIS rugosity metrics/Fortlauderdale/FTL_20250721/slope_25cm.csv") 
slope25 <- slope25 %>%
  rename(
    nail = OBJECTID_1,
    slope = MEAN) %>%
  select(-OBJECTID, -COUNT, -AREA)

slope50 <- read.csv("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/raw/LSAT/3. GIS rugosity metrics/Fortlauderdale/FTL_20250721/slope_50cm.csv")
slope50 <- slope50 %>%
  rename(
    nail = OBJECTID_1,
    slope = MEAN) %>%
  select(-OBJECTID, -COUNT, -AREA)

slope100 <- read.csv("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/raw/LSAT/3. GIS rugosity metrics/Fortlauderdale/FTL_20250721/slope_100cm.csv")
slope100 <- slope100 %>%
  rename(
    nail = OBJECTID_1,
    slope = MEAN) %>%
  select(-OBJECTID, -COUNT, -AREA)

# bring in biotic data: 
LSAT_bio <- read_xlsx("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/raw/LSAT/4. Biological metrics/Fortlauderdale/FTL_20250721/FTL_20250721_lsat.xlsx") %>%
  rename(nail = Plot) 


#------------------------- setting up the Databases  -------------------------

#
# 25cm: 
#


# and make sure the column type is the same before joining: 
LSAT_bio$nail <- as.character(LSAT_bio$nail)
rugosity_25cm$nail <- as.character(rugosity_25cm$nail)
slope25$nail <- as.character(slope25$nail)

rawdata25 <- list(LSAT_bio, rugosity_25cm, slope25) %>% 
  reduce(full_join, by='nail', copy = TRUE)
write.csv(rawdata25, file = "25cm Box data.csv")

#
# 50cm: 
#


# and make sure the column type is the same before joining: 
rugosity_50cm$nail <- as.character(FINAL_50_rugo$nail)
# FINAL_Alain50$nail <- as.character(FINAL_Alain50$nail)
slope50$nail <- as.character(slope50$nail)

rawdata50 <- list(LSAT_bio, rugosity_50cm, slope50) %>% 
  reduce(full_join, by='nail', copy = TRUE)


#
# 100cm: 
#


# and make sure the column type is the same before joining: 
rugosity_100cm$nail <- as.character(FINAL_100_rugo$nail)
# FINAL_Alain100$nail <- as.character(FINAL_Alain100$nail)
slope100$nail <- as.character(slope100$nail)

rawdata100 <- list(LSAT_bio, rugosity_100cm, slope100) %>% 
  reduce(full_join, by='nail', copy = TRUE)


