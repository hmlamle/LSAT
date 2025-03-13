# LSAT data visualization 
# Hannah-Marie Lamle
# 5/21/24
## Update 3/12/25

library(tidyverse)
library(ggplot2)
library(readxl)

# major revamp incoming. I did not consolidate everything using excel. 

#Comparison_metrics_South_Canyon <- read_excel("Comparison_metrics_South_Canyon.xlsx") 


#------- setting up the Databases with all values for each area: 25, 50, 100 ---

LSAT_bio <- read_xlsx("South Canyon Fall LSAT Data.xlsx") %>%
rename(LSAT_bio, nail = Plot)
slope25 <- read_xlsx("avg_slope_25cm.xlsx")


rawdata25 <- list(LSAT_bio, rugo25, Alain_rugo25, slope25)
rawdata25 %>% reduce(full_join, by='nail', copy = TRUE)

print(alain_rugo25[1])






ggplot(Comparison_metrics_South_Canyon, aes(Turf_length, Sediment_depth))+      # modeling sediment depth (y) as 
  geom_point()+                                                                 # function of turf length (x)
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(Sediment_depth ~ Turf_length, data = Comparison_metrics_South_Canyon)
summary(model)

ggplot(Comparison_metrics_South_Canyon, aes(Turf_length, avg_slope))+           # modelling slope (y) as a function
  geom_point()+                                                                 # of turf legnth (x)
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(avg_slope ~ Turf_length, data = Comparison_metrics_South_Canyon)
summary(model)

ggplot(Comparison_metrics_South_Canyon, aes(Turf_length, avg_rugosity))+        # modelling rugosity (y) as function
  geom_point()+                                                                 # of turf length (x)
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(avg_rugosity ~ Turf_length, data = Comparison_metrics_South_Canyon)
summary(model)

ggplot(Comparison_metrics_South_Canyon, aes(avg_slope, avg_rugosity))+          # modelling rugosity (y) as function 
  geom_point()+                                                                 # of turf length (x)
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(avg_rugosity ~ avg_slope, data = Comparison_metrics_South_Canyon)
summary(model)

# From Rolo: we are trying to see if microstructure taken from Viscore/GIS can 
# reliably predict LSAT metrics (sediment depth, turf length). 

# Therefore, need to flip the x and y axes and model LSAT as a function of complexity

# Feedback from Rolo incorporated, here is how microstructure ranges are altering 
# turf depth and sediment depth: 

library(ggpubr)

ggplot(Comparison_metrics_South_Canyon, aes(avg_rugosity, Turf_length))+         # Modelling turf length (y) as function of 
  geom_point()+                                                                  # rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(Turf_length ~ avg_rugosity, data = Comparison_metrics_South_Canyon)
summary(model)



ggplot(Comparison_metrics_South_Canyon, aes(avg_rugosity, Sediment_depth))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(Sediment_depth ~ avg_rugosity, data = Comparison_metrics_South_Canyon)
summary(model)



ggplot(Comparison_metrics_South_Canyon, aes(avg_slope, Turf_length))+           # modelling turf length (y) as function 
  geom_point()+                                                                 # of slope
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(Turf_length ~ avg_slope, data = Comparison_metrics_South_Canyon)
summary(model)



ggplot(Comparison_metrics_South_Canyon, aes(avg_slope, Sediment_depth))+        # modelling sediment depth (y) as function
  geom_point()+                                                                 # of slope (x)
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(Sediment_depth ~ avg_slope, data = Comparison_metrics_South_Canyon)
summary(model)


# Okay, good... but relationships aren't super strong. What about if we play around 
# with the scale matchups? Increase the box in Viscore/GIS and how does that effect 
# relationship with LSAT? By incorporating larger spatial scale, are we able to 
# better predict LSAT metrics? 


rugo_100cm <- read_excel("100cm_quadrat_metrics.xlsx")

ggplot(rugo_100cm, aes(Avg_Slope, Turf_length))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(Turf_length ~ Avg_Slope, data = rugo_100cm)
summary(model)
