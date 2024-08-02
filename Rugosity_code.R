# Nico's code for calculating rugosity metrics from viscore
# Modified for LSAT rugosity measurements
# Hannah-Marie Lamle 
# 5/21/24


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

nail <- read.csv("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/nail_26_rugo.csv")

rugosity <- nail %>%  
  filter(z != 0) %>%
  group_by(transect) %>%
  summarize(
    standard_length = sum(sqrt(diff(x)^2 + diff(y)^2)),  # Euclidean distance in 2D space
    true_length = sum(sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)),  # 3D distance
    rugosity = (true_length / standard_length),
    point_range = max(z) - min(z)  # Range between lowest and highest point
  )%>%
  ungroup() |> 
  summarize(
    avg_rugosity = mean(rugosity),
    rugosity_variance = var(rugosity),
    avg_point_range = mean(point_range)
  )

rugosity <- pre %>%
  group_by(SitePlot,Transects) %>%
  summarize(
    standard_length = sum(sqrt(diff(x)^2 + diff(y)^2)),  # Euclidean distance in 2D space
    true_length = sum(sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)),  # 3D distance
    rugosity = 1-(standard_length / true_length),
    point_range = max(z) - min(z)  # Range between lowest and highest point
  )%>%
  ungroup() %>%
  group_by(SitePlot) |> 
  summarize(
    avg_rugosity = mean(rugosity),
    rugosity_variance = var(rugosity),
    avg_point_range = mean(point_range)
  )

write.csv(rugosity,"rugosity.csv")
