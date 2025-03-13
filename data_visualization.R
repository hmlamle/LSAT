# LSAT data visualization 
# Hannah-Marie Lamle
# 5/21/24
## Update 3/12/25

library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)

# major revamp incoming. I did not consolidate everything using excel. 

#Comparison_metrics_South_Canyon <- read_excel("Comparison_metrics_South_Canyon.xlsx") 


# -------------------------- The extra nail problem... -----------------------
# I am deleting the last nail #26. Even through I can very clearly see this one 
# in the orthomosiac, it could just be signaling the end of the transect? so
# deleting. 

# bring in slope data from GIS: 
slope25 <- read_xlsx("avg_slope_25cm.xlsx")
slope50 <- read_xlsx("avg_slope_50cm.xlsx")
slope100 <- read_xlsx("avg_slope_100cm.xlsx")

FINAL_25_rugo <- FINAL_25_rugo %>% filter(nail != 26)
FINAL_50_rugo <- FINAL_50_rugo %>% filter(nail != 26)
FINAL_100_rugo <- FINAL_100_rugo %>% filter(nail != 26)
FINAL_Alain25 <- FINAL_Alain25 %>% filter(nail != 26)
FINAL_Alain50 <- FINAL_Alain50 %>% filter(nail != 26)
FINAL_Alain100 <- FINAL_Alain100 %>% filter(nail != 26)
slope25 <- slope25 %>% filter(nail != 26)
slope50 <- slope50 %>% filter(nail != 26)
slope100 <- slope100 %>% filter(nail != 26)

#------------------------- setting up the Databases  -------------------------

#
# 25cm: 
#


# bring in biotic data: 
LSAT_bio <- read_xlsx("South Canyon Fall LSAT Data.xlsx") %>%
  rename(nail = Plot) 

# and make sure the column type is the same before joining: 
LSAT_bio$nail <- as.character(LSAT_bio$nail)
FINAL_25_rugo$nail <- as.character(FINAL_25_rugo$nail)
FINAL_Alain25$nail <- as.character(FINAL_Alain25$nail)
slope25$nail <- as.character(slope25$nail)

rawdata25 <- list(LSAT_bio, FINAL_25_rugo, FINAL_Alain25, slope25) %>% 
  reduce(full_join, by='nail', copy = TRUE)


#
# 50cm: 
#


# and make sure the column type is the same before joining: 
FINAL_50_rugo$nail <- as.character(FINAL_50_rugo$nail)
FINAL_Alain50$nail <- as.character(FINAL_Alain50$nail)
slope50$nail <- as.character(slope50$nail)

rawdata50 <- list(LSAT_bio, FINAL_50_rugo, FINAL_Alain50, slope50) %>% 
  reduce(full_join, by='nail', copy = TRUE)


#
# 100cm: 
#


# and make sure the column type is the same before joining: 
FINAL_100_rugo$nail <- as.character(FINAL_100_rugo$nail)
FINAL_Alain100$nail <- as.character(FINAL_Alain100$nail)
slope100$nail <- as.character(slope100$nail)

rawdata100 <- list(LSAT_bio, FINAL_100_rugo, FINAL_Alain100, slope100) %>% 
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


library(ggpubr)

# regular rugosity vs turf length
ggplot(rawdata25, aes(x = avg_rugo25, y = `Turf length (mm)`))+   # Modelling turf length (y) as function of rugosity (x)
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")
model <- lm(`Turf length (mm)` ~ avg_rugo25, data = rawdata25)
summary(model)


# regular rugosity vs sediment depth
ggplot(rawdata25, aes(x = avg_rugo25,  y = `Sediment depth (mm)`))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")
model <- lm(`Sediment depth (mm)` ~ avg_rugo25, data = rawdata25)
summary(model)


# 0-1 rugosity vs turf length
ggplot(rawdata25, aes(rugo25_A, y = `Turf length (mm)`))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity on 0-1 scale")
model <- lm(`Turf length (mm)` ~ rugo25_A, data = rawdata25)
summary(model)

# 0-1 rugosity vs sediment depth
ggplot(rawdata25, aes(rugo25_A, y = `Sediment depth (mm)`))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity on 0-1 scale")
model <- lm(`Sediment depth (mm)` ~ rugo25_A, data = rawdata25)
summary(model)


# slope vs turf length
ggplot(rawdata25, aes(MEAN, y = `Turf length (mm)`))+           # modelling turf length (y) as function 
  geom_point()+                                                                 # of slope
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")
model <- lm(`Turf length (mm)` ~ MEAN, data = rawdata25)
summary(model)


# slope vs sediment depth 
ggplot(rawdata25, aes(MEAN, y = `Sediment depth (mm)`))+        # modelling sediment depth (y) as function
  geom_point()+                                                                 # of slope (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")
model <- lm(`Sediment depth (mm)` ~ MEAN, data = rawdata25)
summary(model)


# Okay, good... but relationships aren't super strong. What about if we play around 
# with the scale matchups? Increase the box in Viscore/GIS and how does that effect 
# relationship with LSAT? By incorporating larger spatial scale, are we able to 
# better predict LSAT metrics? 


# --------------------------- Linear Regressions: 50 ---------------------------


# regular rugosity vs turf length
ggplot(rawdata50, aes(x = avg_rugo50, y = `Turf length (mm)`))+   # Modelling turf length (y) as function of rugosity (x)
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")
model <- lm(`Turf length (mm)` ~ avg_rugo50, data = rawdata50)
summary(model)


# regular rugosity vs sediment depth
ggplot(rawdata50, aes(x = avg_rugo50,  y = `Sediment depth (mm)`))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")
model <- lm(`Sediment depth (mm)` ~ avg_rugo50, data = rawdata50)
summary(model)


# 0-1 rugosity vs turf length
ggplot(rawdata50, aes(rugo50_A, y = `Turf length (mm)`))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity on 0-1 scale")
model <- lm(`Turf length (mm)` ~ rugo50_A, data = rawdata50)
summary(model)

# 0-1 rugosity vs sediment depth
ggplot(rawdata50, aes(rugo50_A, y = `Sediment depth (mm)`))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity on 0-1 scale")
model <- lm(`Sediment depth (mm)` ~ rugo50_A, data = rawdata50)
summary(model)


# slope vs turf length
ggplot(rawdata50, aes(MEAN, y = `Turf length (mm)`))+           # modelling turf length (y) as function 
  geom_point()+                                                                 # of slope
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")
model <- lm(`Turf length (mm)` ~ MEAN, data = rawdata50)
summary(model)


# slope vs sediment depth 
ggplot(rawdata50, aes(MEAN, y = `Sediment depth (mm)`))+        # modelling sediment depth (y) as function
  geom_point()+                                                                 # of slope (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")
model <- lm(`Sediment depth (mm)` ~ MEAN, data = rawdata50)
summary(model)


# --------------------------- Linear Regressions: 100 ---------------------------


# regular rugosity vs turf length
ggplot(rawdata100, aes(x = avg_rugo100, y = `Turf length (mm)`))+   # Modelling turf length (y) as function of rugosity (x)
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")
model <- lm(`Turf length (mm)` ~ avg_rugo100, data = rawdata100)
summary(model)


# regular rugosity vs sediment depth
ggplot(rawdata100, aes(x = avg_rugo100,  y = `Sediment depth (mm)`))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity")
model <- lm(`Sediment depth (mm)` ~ avg_rugo100, data = rawdata100)
summary(model)


# 0-1 rugosity vs turf length
ggplot(rawdata100, aes(rugo100_A, y = `Turf length (mm)`))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity on 0-1 scale")
model <- lm(`Turf length (mm)` ~ rugo100_A, data = rawdata100)
summary(model)

# 0-1 rugosity vs sediment depth
ggplot(rawdata100, aes(rugo100_A, y = `Sediment depth (mm)`))+     # modelling sediment depth (y) as function
  geom_point()+                                                                 # of rugosity (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Rugosity on 0-1 scale")
model <- lm(`Sediment depth (mm)` ~ rugo100_A, data = rawdata100)
summary(model)


# slope vs turf length
ggplot(rawdata100, aes(MEAN, y = `Turf length (mm)`))+           # modelling turf length (y) as function 
  geom_point()+                                                                 # of slope
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")
model <- lm(`Turf length (mm)` ~ MEAN, data = rawdata100)
summary(model)


# slope vs sediment depth 
ggplot(rawdata100, aes(MEAN, y = `Sediment depth (mm)`))+        # modelling sediment depth (y) as function
  geom_point()+                                                                 # of slope (x)
  geom_smooth(method = "lm", formula = y ~ x)+
  xlab("Average Slope (degrees)")
model <- lm(`Sediment depth (mm)` ~ MEAN, data = rawdata100)
summary(model)

