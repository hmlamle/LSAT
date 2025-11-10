# Hannah-Marie Lamle 
# Nico's code for calculating rugosity metrics from viscore
# Modified for LSAT rugosity measurements
# 5/21/24
# Script 1

# ------------------------------- Setup ----------------------------------------

library(tidyverse)

# creating a for loop to make the data entry easier, since we have 1 csv for each nail.... 

# Define the number of files for each category (12 if did half, 25 if did full transect)
n_files <- 25

# Initialize empty lists to store each csv in a list:
data_25cm <- list()
data_50cm <- list()
data_100cm <- list()

# fetching the csvs for 25cm microquadrat: 
for (i in 1:n_files) {
  file_name <- paste0("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/raw/LSAT/2. Viscore rugosity metrics/Southcanyon/SC_20231020/25cm box/", "p", i, ".csv")
  data_25cm[[i]] <- read.csv(file_name)
}


# rename the columns
for (i in 1:n_files) {
  colnames(data_25cm[[i]]) <- c(paste0("nail", i), "transect", "sample", "x", "y", "z")
}


# 50cm microquadrat: 
for (i in 1:n_files) {
  file_name <- paste0("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/raw/LSAT/2. Viscore rugosity metrics/Southcanyon/SC_20231020/50cm box/", "p", i, ".csv")
  data_50cm[[i]] <- read.csv(file_name)
}

# rename the columns
for (i in 1:n_files) {
  colnames(data_50cm[[i]]) <- c(paste0("nail", i), "transect", "sample", "x", "y", "z")
}


# 100cm microquadrat: 
for (i in 1:n_files) {
  file_name <- paste0("C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/raw/LSAT/2. Viscore rugosity metrics/Southcanyon/SC_20231020/100cm box/", "p", i, ".csv")
  data_100cm[[i]] <- read.csv(file_name)
}

# rename the columns
for (i in 1:n_files) {
  colnames(data_100cm[[i]]) <- c(paste0("nail", i), "transect", "sample", "x", "y", "z")
}


# -------------------- Rugosity Calculations -----------------------


# Initialize an empty list to store the results

rugo25 <- list()
rugo50 <- list()
rugo100 <- list()

#
# 25cm (original microquadrats):
#


# a For Loop to apply rugo calculation for each nail for 25cm: 
for (i in 1:length(data_25cm)) {
  # Perform the calculations on each nail dataset
  rugo25[[i]] <- data_25cm[[i]] %>%
    filter(z != 0) %>%
    group_by(transect) %>%
    summarize(
      standard_length = sum(sqrt(diff(x)^2 + diff(y)^2)),  # Euclidean distance in 2D space
      true_length = sum(sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)),  # 3D distance
      rugosity = (true_length / standard_length), 
      point_range = max(z) - min(z)  # Range between lowest and highest point
    ) %>%
    ungroup() %>%
    summarize(
      avg_rugo25 = mean(rugosity),
      rugo25_variance = var(rugosity),
      avg_point_range25 = mean(point_range)
    )
  
}


rugosity_25cm <- bind_rows(rugo25, .id = "nail")
write.csv(rugosity_25cm, file="SC_20231020_25cm_rugo.csv")


#
# 50cm: 
#


# a For Loop to apply rugo calculation for each nail: 
for (i in 1:length(data_50cm)) {
  # Perform the calculations on each nail dataset
  rugo50[[i]] <- data_50cm[[i]] %>%
    filter(z != 0) %>%
    group_by(transect) %>%
    summarize(
      standard_length = sum(sqrt(diff(x)^2 + diff(y)^2)),  # Euclidean distance in 2D space
      true_length = sum(sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)),  # 3D distance
      rugosity = (true_length / standard_length), 
      point_range = max(z) - min(z)  # Range between lowest and highest point
    ) %>%
    ungroup() %>%
    summarize(
      avg_rugo50 = mean(rugosity),
      rugo50_variance = var(rugosity),
      avg_point_range50 = mean(point_range)
    )
  
}


rugosity_50cm <- bind_rows(rugo50, .id = "nail")
write.csv(rugosity_50cm, file="SC_20231020_50cm_rugo.csv")  

#
# 100cm: 
#

# a For Loop to apply rugo calculation for each nail: 
for (i in 1:length(data_100cm)) {
  # Perform the calculations on each nail dataset
  rugo100[[i]] <- data_100cm[[i]] %>%
    filter(z != 0) %>%
    group_by(transect) %>%
    summarize(
      standard_length = sum(sqrt(diff(x)^2 + diff(y)^2)),  # Euclidean distance in 2D space
      true_length = sum(sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)),  # 3D distance
      rugosity = (true_length / standard_length), 
      point_range = max(z) - min(z)  # Range between lowest and highest point
    ) %>%
    ungroup() %>%
    summarize(
      avg_rugo100 = mean(rugosity),
      rugo100_variance = var(rugosity),
      avg_point_range100 = mean(point_range)
    )
  
}


rugosity_100cm <- bind_rows(rugo100, .id = "nail")
write.csv(rugosity_100cm, file="SC_20231020_100cm_rugo.csv")  




# -------------------- 0-1 scale Rugosity (Extra) -----------------------



# Initialize an empty list to store the results
Alain_rugo25 <- list()
Alain_rugo50 <- list()
Alain_rugo100 <- list()


#
# 25cm (original microquadrats):
#


# Loop through each nail dataset and apply the rugosity calculation 
for (i in 1:length(data_25cm)) {
  # Perform the calculations on each nail dataset
  Alain_rugo25[[i]] <- data_25cm[[i]] %>%
    filter(z != 0) %>%
    group_by(transect) %>%
    summarize(
      standard_length = sum(sqrt(diff(x)^2 + diff(y)^2)),  # Euclidean distance in 2D space
      true_length = sum(sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)),  # 3D distance
      rugosity = (standard_length / true_length), 
      point_range = max(z) - min(z)  # Range between lowest and highest point
    ) %>%
    ungroup() %>%
    summarize(
      rugo25_A = mean(rugosity),
      rugo25_var_A = var(rugosity),
      avg_point_range_A25 = mean(point_range)
    )
  
}



FINAL_Alain25 <- bind_rows(Alain_rugo25, .id = "nail")
write.csv(FINAL_Alain25, file="25_rugo_Alain.csv")


#
# 50cm:
#


# Loop through each nail dataset and apply the rugosity calculation 
for (i in 1:length(data_50cm)) {
  # Perform the calculations on each nail dataset
  Alain_rugo50[[i]] <- data_50cm[[i]] %>%
    filter(z != 0) %>%
    group_by(transect) %>%
    summarize(
      standard_length = sum(sqrt(diff(x)^2 + diff(y)^2)),  # Euclidean distance in 2D space
      true_length = sum(sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)),  # 3D distance
      rugosity = (standard_length / true_length), 
      point_range = max(z) - min(z)  # Range between lowest and highest point
    ) %>%
    ungroup() %>%
    summarize(
      rugo50_A = mean(rugosity),
      rugo50_var_A = var(rugosity),
      avg_point_range_A50 = mean(point_range)
    )
  
}


FINAL_Alain50 <- bind_rows(Alain_rugo50, .id = "nail")
write.csv(FINAL_Alain50, file="50_rugo_Alain.csv")

#
# 100cm:
#


# Loop through each nail dataset and apply the rugosity calculation 
for (i in 1:length(data_100cm)) {
  # Perform the calculations on each nail dataset
  Alain_rugo100[[i]] <- data_100cm[[i]] %>%
    filter(z != 0) %>%
    group_by(transect) %>%
    summarize(
      standard_length = sum(sqrt(diff(x)^2 + diff(y)^2)),  # Euclidean distance in 2D space
      true_length = sum(sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)),  # 3D distance
      rugosity = (standard_length / true_length), 
      point_range = max(z) - min(z)  # Range between lowest and highest point
    ) %>%
    ungroup() %>%
    summarize(
      rugo100_A = mean(rugosity),
      rugo100_var_A = var(rugosity),
      avg_point_range_A100 = mean(point_range)
    )
  
}


FINAL_Alain100 <- bind_rows(Alain_rugo100, .id = "nail")
write.csv(FINAL_Alain100, file="100_rugo_Alain.csv")







# ---------------------------Extras from Nico's code --------------------------

# I don't think I need any of this stuff... 

# pre<-read.csv("./overallpre.csv")
# pre4<-read.csv("./422pre.csv")
# post<-read.csv("overall.csv")
# post4<-read.csv("4m2.csv")
# quick<-read.csv(file.choose())
# quick<-read.csv("C:/Users/nicor/Downloads/M12-53-2021-R.csv")
# quick<-read.csv("C:/Users/nicor/Downloads/M12-53-2021-R - Copy.csv")
# quick<-read.csv("C:/Users/nicor/Downloads/M12-53-2021-R - Copy (2).csv")

# rugosity <- pre %>%
#  group_by(SitePlot,Transects) %>%
#  summarize(
#    standard_length = sum(sqrt(diff(x)^2 + diff(y)^2)),  # Euclidean distance in 2D space
#    true_length = sum(sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)),  # 3D distance
#    rugosity = 1-(standard_length / true_length),
#    point_range = max(z) - min(z)  # Range between lowest and highest point
#  )%>%
#  ungroup() %>%
#  group_by(SitePlot) |> 
#  summarize(
#    avg_rugosity = mean(rugosity),
#    rugosity_variance = var(rugosity),
#    avg_point_range = mean(point_range)
#  )

# -------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)

get_centroids_from_folder <- function(folder_path, scale_cm, site_name) {
  
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  map_dfr(files, function(file) {
    nail_id <- tools::file_path_sans_ext(basename(file))  # gets 'p22.1' etc
    
    df <- read_csv(file, col_names = c("spacing", "transect", "sample", "x", "y", "z"),
                   show_col_types = FALSE)
    
    df %>%
      summarise(
        mean_x = mean(x, na.rm=TRUE),
        mean_y = mean(y, na.rm=TRUE)
      ) %>%
      mutate(
        nail = nail_id,
        scale_cm = scale_cm,
        site = site_name
      )
  })
}

folder_50cm <- "C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/raw/LSAT/2. Viscore rugosity metrics/Fortlauderdale/FTL_20250721/50cm box"

centroids_50 <- get_centroids_from_folder(folder_50cm, 50, "FortLauderdale")

folder_25cm <- "C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/raw/LSAT/2. Viscore rugosity metrics/Fortlauderdale/FTL_20250721/25cm box"
folder_100cm <- "C:/Users/hanna/Florida International University/Coral Reef Fisheries - 2. Hannah-Marie Lamle/data/raw/LSAT/2. Viscore rugosity metrics/Fortlauderdale/FTL_20250721/100cm box"

centroids_25 <- get_centroids_from_folder(folder_25cm, 25, "FortLauderdale")
centroids_100 <- get_centroids_from_folder(folder_100cm, 100, "FortLauderdale")

centroids_all <- bind_rows(centroids_25, centroids_50, centroids_100)

ggplot(centroids_all, aes(x = mean_x, y = mean_y, color = as.factor(scale_cm), shape = as.factor(scale_cm))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(aes(label = nail), vjust = -1, size = 3) +
  labs(title = "Centroid positions of LSAT boxes by scale (Fort Lauderdale)",
       x = "X coordinate",
       y = "Y coordinate",
       color = "Scale (cm)",
       shape = "Scale (cm)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 0.4)

