# LSAT data visualization 
# Hannah-Marie Lamle
# 5/21/24

library(tidyverse)
library(ggplot2)
library(readxl)

Comparison_metrics_South_Canyon <- read_excel("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/Comparison_metrics_South_Canyon.xlsx")

ggplot(Comparison_metrics_South_Canyon, aes(Turf_length, Sediment_depth))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(Sediment_depth ~ Turf_length, data = Comparison_metrics_South_Canyon)
summary(model)

ggplot(Comparison_metrics_South_Canyon, aes(Turf_length, avg_slope))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(avg_slope ~ Turf_length, data = Comparison_metrics_South_Canyon)
summary(model)

ggplot(Comparison_metrics_South_Canyon, aes(Turf_length, avg_rugosity))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(avg_rugosity ~ Turf_length, data = Comparison_metrics_South_Canyon)
summary(model)

ggplot(Comparison_metrics_South_Canyon, aes(avg_slope, avg_rugosity))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(avg_rugosity ~ avg_slope, data = Comparison_metrics_South_Canyon)
summary(model)

## Feedback from Rolo incorporated, here is how microstructure ranges are altering 
# turf depth and sediment depth 

ggplot(Comparison_metrics_South_Canyon, aes(avg_rugosity, Turf_length))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(Turf_length ~ avg_rugosity, data = Comparison_metrics_South_Canyon)
summary(model)

ggplot(Comparison_metrics_South_Canyon, aes(avg_rugosity, Sediment_depth))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(Sediment_depth ~ avg_rugosity, data = Comparison_metrics_South_Canyon)
summary(model)

ggplot(Comparison_metrics_South_Canyon, aes(avg_slope, Turf_length))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(Turf_length ~ avg_slope, data = Comparison_metrics_South_Canyon)
summary(model)

ggplot(Comparison_metrics_South_Canyon, aes(avg_slope, Sediment_depth))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(Sediment_depth ~ avg_slope, data = Comparison_metrics_South_Canyon)
summary(model)

rugosity_metrics_100cm <- read_excel("C:/Users/hanna/OneDrive - Florida International University/R/LSAT rugosity/100cm_quadrat_metrics.xlsx")

ggplot(rugosity_metrics_100cm, aes(Avg_Slope, Turf_length))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)
model <- lm(Turf_length ~ Avg_Slope, data = rugosity_metrics_100cm)
summary(model)
