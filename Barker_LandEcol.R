#######################################################
#### LANDSCAPE ECOLOGY CLASS PROJECT - Spring 2016 ####
################## Kristin Barker #####################
#######################################################

#Goal: Quantify relationship between remotely sensed vegetation
       #index (NDVI or EVI) and elk fecal nitrogen

#read in data
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\FecalN-NDVI"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\FecalN-NDVI"

if (file.exists(wd_workcomp)){
  inpath="C:\\Users\\kristin.barker\\Documents\\GitHub\\FecalN-NDVI\\"
} else {
  inpath="C:\\Users\\kjbark3r\\Documents\\GitHub\\FecalN-NDVI\\"
}

setwd(inpath)
getwd() #sanity check

remote.raw <- read.csv("remotedata.csv")
fecaln.raw <- read.csv("fecalndata2014.csv")

#load libraries
  ##data wrangling
library(dplyr)
library(tidyr)
  ##spatial stuff

#if you wanna check anything out
summary(remote.raw)
str(remote.raw)
summary(fecaln.raw)
str(fecaln.raw)

#pull pellet-related remote data only
remote.data <- filter(remote.raw, Type == "Pellet 2014")
remote.data$PlotID <- extract_numeric(remote.data$PlotID)

#check out the data
##hey data, you come here often?
plot(fecaln.raw$PctFN ~ fecaln.raw$SamplePd)
