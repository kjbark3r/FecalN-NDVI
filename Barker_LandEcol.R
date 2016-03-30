#######################################################
#### LANDSCAPE ECOLOGY CLASS PROJECT - Spring 2016 ####
################## Kristin Barker #####################
#######################################################

#Goal: Quantify relationship between remotely sensed vegetation
       #index (NDVI or EVI) and elk fecal nitrogen

#load libraries
library(dplyr)
library(tidyr)

#read in data
setwd("C:/Users/kristin.barker/Documents/GitHub/FecalN-NDVI")
remote.raw <- read.csv("remotedata.csv")
fecaln.raw <- read.csv("fecalndata.csv")

#if you wanna check anything out
summary(remote.raw)
str(remote.raw)
summary(fecaln.raw)
str(fecaln.raw)

#pull pellet-related remote data only
remote.data <- filter(remote.raw, Type == "Pellet 2014")
remote.data$PlotID <- extract_numeric(remote.data$PlotID)

