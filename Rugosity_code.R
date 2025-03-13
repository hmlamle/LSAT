# Nico's code for calculating rugosity metrics from viscore
# Modified for LSAT rugosity measurements
# Hannah-Marie Lamle 
# 5/21/24


# ------------------------------- Setup ----------------------------------------

library(tidyverse)


# pre<-read.csv("./overallpre.csv")
# pre4<-read.csv("./422pre.csv")
# post<-read.csv("overall.csv")
# post4<-read.csv("4m2.csv")
# quick<-read.csv(file.choose())
# quick<-read.csv("C:/Users/nicor/Downloads/M12-53-2021-R.csv")
# quick<-read.csv("C:/Users/nicor/Downloads/M12-53-2021-R - Copy.csv")
# quick<-read.csv("C:/Users/nicor/Downloads/M12-53-2021-R - Copy (2).csv")


# creating a for loop to make the data entry easier, 
# since we have 1 cv for each nail.... 

# Define the number of files for each category
n_files <- 26

# Initialize empty lists to store the data
data_25cm <- list()
data_50cm <- list()
data_100cm <- list()

# 25cm microquadrat: 
for (i in 1:n_files) {
  file_name <- paste0("C:/Users/hanna/OneDrive - Florida International University/photogrammetry/LSAT data/10-20-2023/North Miami Beach/Viscore redo 031225/", "25cm_", i, ".csv")
  data_25cm[[i]] <- read.csv(file_name)
}

# rename the columns
for (i in 1:n_files) {
  colnames(data_25cm[[i]]) <- c(paste0("nail", i), "transect", "sample", "x", "y", "z")
}


# 50cm microquadrat: 
for (i in 1:n_files) {
  file_name <- paste0("C:/Users/hanna/OneDrive - Florida International University/photogrammetry/LSAT data/10-20-2023/North Miami Beach/Viscore redo 031225/", "50cm_", i, ".csv")
  data_50cm[[i]] <- read.csv(file_name)
}

# rename the columns
for (i in 1:n_files) {
  colnames(data_50cm[[i]]) <- c(paste0("nail", i), "transect", "sample", "x", "y", "z")
}


# and last, 100cm microquadrat: 
for (i in 1:n_files) {
  file_name <- paste0("C:/Users/hanna/OneDrive - Florida International University/photogrammetry/LSAT data/10-20-2023/North Miami Beach/Viscore redo 031225/", "100cm-", i, ".csv")
  data_100cm[[i]] <- read.csv(file_name)
}

# rename the columns
for (i in 1:n_files) {
  colnames(data_100cm[[i]]) <- c(paste0("nail", i), "transect", "sample", "x", "y", "z")
}


# -------------------- Rugosity how Ryan & Rolo suggest -----------------------


# Initialize an empty list to store the results
# rugosity_list <- list() don't need this anymore, was reminant of code before automated data import

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


FINAL_25_rugo <- bind_rows(rugo25, .id = "nail")
write.csv(FINAL_25_rugo, file="25_rugo.csv")


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


FINAL_50_rugo <- bind_rows(rugo50, .id = "nail")
write.csv(FINAL_50_rugo, file="50_rugo.csv")

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


FINAL_100_rugo <- bind_rows(rugo100, .id = "nail")
write.csv(FINAL_100_rugo, file="100_rugo.csv")



# -------------------- Rugosity how Alain discussed -----------------------



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


