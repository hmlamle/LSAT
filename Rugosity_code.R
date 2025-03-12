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

# not sure what all of these were about, I will only be using one "read.csv" command
# and replacing each time for each of my nail csv's 

nail1 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_1_rugo.csv")
nail2 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_2_rugo_unsure.csv")
nail3 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_3_rugo.csv")
nail4 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_4_rugo_unsure.csv")
nail5 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_5_rugo.csv")
nail6 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_6_rugo.csv")
nail7 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_7_rugo.csv")
nail8 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_8_rugo.csv")
nail9 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_9_rugo.csv")
nail10 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_10_rugo_unsure.csv")
nail11 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_11_rugo.csv")
nail12 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_12_rugo.csv")
nail13 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_13_rugo.csv")
nail14 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_14_rugo.csv")
nail15 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_15_rugo.csv")
nail16 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_16_rugo.csv")
nail17 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_17_rugo.csv")
nail18 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_18_rugo.csv")
nail19 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_19_rugo.csv")
nail20 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_20_rugo.csv")
nail21 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_21_rugo.csv")
nail22 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_22_rugo.csv")
nail23 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_23_rugo.csv")
nail24 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_24_rugo.csv")
nail25 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_25_rugo.csv")
nail26 <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_26_rugo.csv")

# listing all viscore values to make using a for loop easier

all_nails <- list(nail1, nail2, nail3, nail4, nail5, nail6, nail7, nail8, nail9, nail10, nail11, nail12, nail13, nail14, nail15, nail16, nail17, nail18, nail19, nail20, nail21, nail22, nail23, nail24, nail25, nail26) 


# -------------------- Rugosity how Ryan & Rolo suggest -----------------------


# Initialize an empty list to store the results
rugosity_list <- list()

# Loop through each nail dataset and apply the rugosity calculation
for (i in 1:length(all_nails)) {
  # Perform the calculations on each nail dataset
  rugosity_list[[i]] <- all_nails[[i]] %>%
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
      avg_rugosity = mean(rugosity),
      rugosity_variance = var(rugosity),
      avg_point_range = mean(point_range)
    )
  
}


combined_rugosity <- bind_rows(rugosity_list, .id = "nail")


write.csv(combined_rugosity,"rugo_25cm.csv")




# -------------------- Rugosity how Alain discussed -----------------------


# Initialize an empty list to store the results
Alain_rugosity <- list()

# Loop through each nail dataset and apply the rugosity calculation
for (i in 1:length(all_nails)) {
  # Perform the calculations on each nail dataset
  Alain_rugosity[[i]] <- all_nails[[i]] %>%
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
      avg_rugosity = mean(rugosity),
      rugosity_variance = var(rugosity),
      avg_point_range = mean(point_range)
    )
  
}


Alain_combined_rugosity <- bind_rows(Alain_rugosity, .id = "nail")


write.csv(Alain_combined_rugosity,"Alain_rugo_25cm.csv")








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


