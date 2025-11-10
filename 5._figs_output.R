# figs created in the results PDF but in raw format

library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)

rugo_25cm <- read.csv("25_rugo.csv")
rugo_50cm <- read.csv("50_rugo.csv")
rugo_100cm <- read.csv("100_rugo.csv")

slope_25cm <- read_excel("avg_slope_25cm.xlsx")
slope_50cm <- read_excel("avg_slope_50cm.xlsx")
slope_100cm <- read_excel("avg_slope_100cm.xlsx")


