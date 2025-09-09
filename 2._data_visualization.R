# LSAT data visualization 
# Hannah-Marie Lamle
# 5/21/2024
## Update 3/12/2025
## Update 9/4/2025
# Script 2

library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(patchwork)
library(ggpubr)
library(grid)

# ------------------------- Adding other data ------------------------

#Comparison_metrics_South_Canyon <- read_excel("Comparison_metrics_South_Canyon.xlsx") 

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
#FINAL_Alain25$nail <- as.character(FINAL_Alain25$nail)
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


# -------------------- Old code: not using ---------------------------------


#ggplot(rawdata25, aes(Turf_length, Sediment_depth))+      # modeling sediment depth (y) as 
#  geom_point()+                                           # function of turf length (x)
#  geom_smooth(method = "lm", formula = y ~ x)
#model <- lm(Sediment_depth ~ Turf_length, data = Comparison_metrics_South_Canyon)
#summary(model)

#ggplot(Comparison_metrics_South_Canyon, aes(Turf_length, avg_slope))+           # modelling slope (y) as a function
#  geom_point()+                                                                 # of turf legnth (x)
#  geom_smooth(method = "lm", formula = y ~ x)
#model <- lm(avg_slope ~ Turf_length, data = Comparison_metrics_South_Canyon)
#summary(model)

#ggplot(Comparison_metrics_South_Canyon, aes(Turf_length, avg_rugosity))+        # modelling rugosity (y) as function
#  geom_point()+                                                                 # of turf length (x)
#  geom_smooth(method = "lm", formula = y ~ x)
#model <- lm(avg_rugosity ~ Turf_length, data = Comparison_metrics_South_Canyon)
#summary(model)

#ggplot(Comparison_metrics_South_Canyon, aes(avg_slope, avg_rugosity))+          # modelling rugosity (y) as function 
#  geom_point()+                                                                 # of turf length (x)
#  geom_smooth(method = "lm", formula = y ~ x)
#model <- lm(avg_rugosity ~ avg_slope, data = Comparison_metrics_South_Canyon)
#summary(model)

# From Rolo: we are trying to see if microstructure taken from Viscore/GIS can 
# reliably predict LSAT metrics (sediment depth, turf length). 

# Therefore, need to flip the x and y axes and model LSAT as a function of complexity

# Feedback from Rolo incorporated, here is how microstructure ranges are altering 
# turf depth and sediment depth: 


# --------------------------- Linear Regressions: 25 ---------------------------


# regular rugosity vs turf length
rugo25_turf <- ggplot(rawdata25, aes(x = avg_rugo25, y = `turf_ length_mm`))+   # Modelling turf length (y) as function of rugosity (x)
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")+
  ggtitle("25cm Box")
model <- lm(`turf_ length_mm` ~ avg_rugo25, data = rawdata25)
summary(model)
rugo25_turf


# regular rugosity vs sediment depth
rugo25_sediment <- ggplot(rawdata25, aes(x = avg_rugo25,  y = sed_depth_mm))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")
model <- lm(sed_depth_mm ~ avg_rugo25, data = rawdata25)
summary(model)
rugo25_sediment

# 0-1 rugosity vs turf length
#ggplot(rawdata25, aes(rugo25_A, y = `Turf length (mm)`))+     # modelling sediment depth (y) as function
#  geom_point()+                                                                 # of rugosity (x)
#  geom_smooth(method = "lm", formula = y ~ x)+
#  xlab("Rugosity on 0-1 scale")
#model <- lm(`Turf length (mm)` ~ rugo25_A, data = rawdata25)
#summary(model)

# 0-1 rugosity vs sediment depth
#ggplot(rawdata25, aes(rugo25_A, y = `Sediment depth (mm)`))+     # modelling sediment depth (y) as function
#  geom_point()+                                                                 # of rugosity (x)
#  geom_smooth(method = "lm", formula = y ~ x)+
#  xlab("Rugosity on 0-1 scale")
#model <- lm(`Sediment depth (mm)` ~ rugo25_A, data = rawdata25)
#summary(model)


# slope vs turf length
slope25_turf <- ggplot(rawdata25, aes(slope, y = `turf_ length_mm`))+           # modelling turf length (y) as function 
  geom_point()+                                                                 # of slope
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")+
  ggtitle("25cm Box")
model <- lm(`turf_ length_mm` ~ MEAN, data = rawdata25)
summary(model)
slope25_turf

# slope vs sediment depth 
slope25_sediment <- ggplot(rawdata25, aes(slope, y = sed_depth_mm))+        # modelling sediment depth (y) as function
  geom_point()+                                                                 # of slope (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")
model <- lm(sed_depth_mm ~ MEAN, data = rawdata25)
summary(model)
slope25_sediment


# --------------------------- Linear Regressions: 50 ---------------------------


# regular rugosity vs turf length
rugo50_turf <- ggplot(rawdata50, aes(x = avg_rugo50, y = `turf_ length_mm`))+   # Modelling turf length (y) as function of rugosity (x)
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")+
  ggtitle("50cm Box")
model <- lm(`turf_ length_mm` ~ avg_rugo50, data = rawdata50)
summary(model)


# regular rugosity vs sediment depth
rugo50_sediment <- ggplot(rawdata50, aes(x = avg_rugo50,  y = sed_depth_mm))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")
model <- lm(sed_depth_mm ~ avg_rugo50, data = rawdata50)
summary(model)


# 0-1 rugosity vs turf length
#ggplot(rawdata50, aes(rugo50_A, y = `Turf length (mm)`))+     # modelling sediment depth (y) as function
#  geom_point()+                                                                 # of rugosity (x)
#  geom_smooth(method = "lm", formula = y ~ x)+
#  xlab("Rugosity on 0-1 scale")
#model <- lm(`Turf length (mm)` ~ rugo50_A, data = rawdata50)
#summary(model)

# 0-1 rugosity vs sediment depth
#ggplot(rawdata50, aes(rugo50_A, y = `Sediment depth (mm)`))+     # modelling sediment depth (y) as function
#  geom_point()+                                                                 # of rugosity (x)
#  geom_smooth(method = "lm", formula = y ~ x)+
#  xlab("Rugosity on 0-1 scale")
#model <- lm(`Sediment depth (mm)` ~ rugo50_A, data = rawdata50)
#summary(model)


# slope vs turf length
slope50_turf <- ggplot(rawdata50, aes(slope, y = `turf_ length_mm`))+           # modelling turf length (y) as function 
  geom_point()+                                                                 # of slope
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")+
  ggtitle("50cm Box")
model <- lm(`turf_ length_mm` ~ MEAN, data = rawdata50)
summary(model)


# slope vs sediment depth 
slope50_sediment <- ggplot(rawdata50, aes(slope, y = sed_depth_mm))+        # modelling sediment depth (y) as function
  geom_point()+                                                                 # of slope (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")
model <- lm(sed_depth_mm ~ MEAN, data = rawdata50)
summary(model)


# --------------------------- Linear Regressions: 100 ---------------------------


# regular rugosity vs turf length
rugo100_turf <- ggplot(rawdata100, aes(x = avg_rugo100, y = `turf_ length_mm`))+   # Modelling turf length (y) as function of rugosity (x)
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")+
  ggtitle("100cm Box")
model <- lm(`turf_ length_mm` ~ avg_rugo100, data = rawdata100)
summary(model)


# regular rugosity vs sediment depth
rugo100_sediment <- ggplot(rawdata100, aes(x = avg_rugo100,  y = sed_depth_mm))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")
model <- lm(sed_depth_mm ~ avg_rugo100, data = rawdata100)
summary(model)


# 0-1 rugosity vs turf length
#ggplot(rawdata100, aes(rugo100_A, y = `Turf length (mm)`))+     # modelling sediment depth (y) as function
#  geom_point()+                                                                 # of rugosity (x)
#  geom_smooth(method = "lm", formula = y ~ x)+
#  xlab("Rugosity on 0-1 scale")
#model <- lm(`Turf length (mm)` ~ rugo100_A, data = rawdata100)
#summary(model)

# 0-1 rugosity vs sediment depth
#ggplot(rawdata100, aes(rugo100_A, y = `Sediment depth (mm)`))+     # modelling sediment depth (y) as function
#  geom_point()+                                                                 # of rugosity (x)
#  geom_smooth(method = "lm", formula = y ~ x)+
#  xlab("Rugosity on 0-1 scale")
#model <- lm(`Sediment depth (mm)` ~ rugo100_A, data = rawdata100)
#summary(model)


# slope vs turf length
slope100_turf <- ggplot(rawdata100, aes(slope, y = `turf_ length_mm`))+           # modelling turf length (y) as function 
  geom_point()+                                                                 # of slope
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")+
  ggtitle("100cm Box")
model <- lm(`turf_ length_mm` ~ MEAN, data = rawdata100)
summary(model)


# slope vs sediment depth 
slope100_sediment <- ggplot(rawdata100, aes(slope, y = sed_depth_mm))+        # modelling sediment depth (y) as function
  geom_point()+                                                                 # of slope (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")
model <- lm(sed_depth_mm ~ MEAN, data = rawdata100)
summary(model)


# ---------------------------- Plotting figs ------------------------

## ------------ Rugosity metrics -----------------

p1 <- ggplot(rawdata25, aes(x = avg_rugo25, y = `Turf length (mm)`))+   
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")+
  ggtitle("25cm microquadrat")+
  theme_classic() <- ggplot(rawdata25, aes(x = avg_rugo25, y = `Turf length (mm)`))+   
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")+
  ggtitle("25cm microquadrat")+
  theme_classic()

p2 <- ggplot(rawdata25, aes(x = avg_rugo25,  y = `Sediment depth (mm)`))+     
  geom_point()+                                                               
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")+
  ggtitle("25cm microquadrat")+
  theme_classic()

p3 <- ggplot(rawdata50, aes(x = avg_rugo50, y = `Turf length (mm)`))+   # Modelling turf length (y) as function of rugosity (x)
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")+
  ggtitle("50cm microquadrat")+
  theme_classic()

p4 <- ggplot(rawdata50, aes(x = avg_rugo50,  y = `Sediment depth (mm)`))+     
  geom_point()+                                                                
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")+
  ggtitle("50cm microquadrat")+
  theme_classic()

p5 <- ggplot(rawdata100, aes(x = avg_rugo100, y = `Turf length (mm)`))+   # Modelling turf length (y) as function of rugosity (x)
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")+
  ggtitle("100cm quadrat")+
  theme_classic()

p6 <- ggplot(rawdata100, aes(x = avg_rugo100,  y = `Sediment depth (mm)`))+   
  geom_point()+                                                                 
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")+
  ggtitle("100cm Quadrat")+
  theme_classic()

(p1 | p2) / (p3 | p4) / (p5 | p6) 

# use this instead, it just uses the graphs created above rather than rewriting them: 

(rugo25_turf | rugo25_sediment) / (rugo50_turf | rugo50_sediment) / (rugo100_turf | rugo100_sediment) +
  plot_annotation(
    title = "Rugosity Results",
  )



## ------------ Slope Metrics -----------------

p1 <- ggplot(rawdata25, aes(MEAN, y = `Turf length (mm)`))+          
  geom_point()+                                                                 
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")+
  ggtitle("25cm microquadrat")+
  theme_classic()

p2 <- ggplot(rawdata25, aes(MEAN, y = `Sediment depth (mm)`))+        
  geom_point()+                                                                
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")+
  ggtitle("25cm Quadrat")+
  theme_classic()

p3 <- ggplot(rawdata50, aes(MEAN, y = `Turf length (mm)`))+      
  geom_point()+                                                                
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")+
  ggtitle("50cm microquadrat")+
  theme_classic()

p4 <- ggplot(rawdata50, aes(MEAN, y = `Sediment depth (mm)`))+     
  geom_point()+                                                                 
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")+
  ggtitle("50cm Quadrat")+
  theme_classic()

p5 <- ggplot(rawdata100, aes(MEAN, y = `Turf length (mm)`))+       
  geom_point()+                                                                 
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")+
  ggtitle("100cm Quadrat")+
  theme_classic()

p6 <- ggplot(rawdata100, aes(MEAN, y = `Sediment depth (mm)`))+       
  geom_point()+                                                                 
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")+
  theme_classic()

(p1 | p2) / (p3 | p4) / (p5 | p6) 

# use this instead, it just uses the graphs created above rather than rewriting them: 

(slope25_turf | slope25_sediment) / (slope50_turf | slope50_sediment) / (slope100_turf | slope100_sediment) +
  plot_annotation(
    title = "Slope Results",
  )
