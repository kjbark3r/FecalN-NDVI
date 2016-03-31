#######################################################
#### LANDSCAPE ECOLOGY CLASS PROJECT - Spring 2016 ####
################## Kristin Barker #####################
#######################################################

#Goal: Quantify relationship between remotely sensed vegetation
       #index (NDVI or EVI) and elk fecal nitrogen

###set wd; read in data
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

###load libraries
  #data wrangling
library(dplyr)
library(tidyr)
  #spatial stuff

###if you wanna check anything out
summary(remote.raw)
str(remote.raw)
summary(fecaln.raw)
str(fecaln.raw)

###prep remote data
  #pull pellet-related remote data only; fix plot names
remote.data <- filter(remote.raw, Type == "Pellet 2014")
remote.data$PlotID <- extract_numeric(remote.data$PlotID)

  #clean up and tidy
remote.data <- rename(remote.data, SampleID = PlotID) 
remote.data <- subset(remote.data, select = -c(system.index, Longitude,
                                       Type, description, name))

###prep EVI data
evi.data <- gather(remote.data, key = Date, value = "EVI", -SampleID)
evi.data <- na.omit(evi.data)
evi.data$EVI.date <- substr(evi.data$EVI.date, 5, 12)
  #make EVI.date column into an actual date
evi.data$EVI.date <- as.Date(as.character(evi.data$EVI.date), format='%Y%m%d')


###prep NDVI data
ndvi.data <- gather(remote.data, key = Date, value = "NDVI", -SampleID)
ndvi.data <- na.omit(ndvi.data)
ndvi.data$Date <- substr(ndvi.data$Date, 5, 12)
#make EVI.date column into an actual date
ndvi.data$Date <- as.Date(as.character(ndvi.data$Date), format='%Y%m%d')


###prep fecaln data
  #add lat-long to each sample
colxndata <- read.csv("colxnsites2014.csv")
fecaln.raw$SampleID <- fecaln.raw$Sample.ID
fn.data <- inner_join(fecaln.raw, colxndata, by = "SampleID") 
  #remove extraneous columns
fn.data <- subset(fn.data, select = -c(Sample.ID, ResidentMigratory,
                                       SampleType, SampleDate, Collector))
  #format date properly
fn.data$Date <- as.Date(as.character(fn.data$Date), format = "%m/%d/20%y")
  #create column relating collxn date to NDVI/EVI date
fn.data$SDate <- ifelse(as.Date(2014-06-10, origin = "1970-01-01") < fn.data$Date,
                        fn.data$SDate <- "2014-06-10",
                        fn.data$SDate <- "NA")

#####BELOW ARE STARTED (NOT WORKING) ATTEMPTS AT DATE STUFF
fn.data$SDate <- ifelse(between(fn.data$Date, as.Date(2014-06-10, origin = "1970-01-01"),
                                as.Date(2014-06-26, origin = "1970-01-01")), 
                        fn.data$SDate <- "2014-06-10",
                        fn.data$SDate <- "NA")

remove(fn.data)

#fn.data <- mutate(Date <- ifelse(between()))

#if (as.Date(2014-06-10, origin = "1970-01-01") < fn.data$Date 
  #  && fn.data$Date < 2014-06-25)
#  {
#  fn.data$Sdate <- as.Date(2014-06-10, origin = "1970-01-01")
#} else {
#  fn.data$Sdate <- "NA"
#}

#fn.data$T1 <- 2014-06-10
#fn.data$T2
#fn.data$T3
#fn.data$T4
#fn.data$T5
#fn.data$T6
#fn.data$T7
#fn.data$T8


#####if it's between these 2 dates, call it this. If between these, this...
#####these are the dates you want to call it
unique(unlist(ndvi.data$Date))

###combine data
   

###check out the data
#hey data, you come here often?
plot(fn.data$PctFN ~ fn.data$SamplePd, 
     col=c("black", "red")[fn.data$MigStatus])
plot(evi.data$Date, evi.data$EVI, xlab = "Date")
plot(ndvi.data$Date, ndvi.data$NDVI, xlab = "Date")
  #haven't figured this part out yet axis.Date(1, ndvi.data$Date, )