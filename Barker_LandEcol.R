#######################################################
#### LANDSCAPE ECOLOGY CLASS PROJECT - Spring 2016 ####
################## Kristin Barker #####################
#######################################################

#GOAL: Quantify relationship between remotely sensed 
       #vegetation index and elk fecal nitrogen

##########SETUP##############
###set wd; read in data
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\FecalN-NDVI"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\FecalN-NDVI"
  if (file.exists(wd_workcomp)){
    inpath="C:\\Users\\kristin.barker\\Documents\\GitHub\\FecalN-NDVI\\"
  } else {
    inpath="C:\\Users\\kjbark3r\\Documents\\GitHub\\FecalN-NDVI\\"
  }
setwd(inpath)
remote.raw <- read.csv("remotedata.csv")
fecaln.raw <- read.csv("fecalndata2014.csv")

###load libraries
  #data wrangling
library(dplyr)
library(tidyr)
  #plotting
#library(ggplot2)
  #analysis
library(AICcmodavg) #AICc

##########DATA PREP##############
###prep remote data
  #pull pellet-related NDVI data only; fix plot names; tidy
remote.data <- filter(remote.raw, Type == "Pellet 2014")
remote.data <- select(remote.data, starts_with("ndvi."), PlotID)
remote.data$PlotID <- extract_numeric(remote.data$PlotID)
remote.data <- rename(remote.data, SampleID = PlotID) 

###prep NDVI data
ndvi.data <- gather(remote.data, key = SDate, value = "NDVI", -SampleID)
ndvi.data$SDate <- substr(ndvi.data$SDate, 6, 13)
ndvi.data$SDate <- as.Date(as.character(ndvi.data$SDate), format='%Y%m%d')

###prep fecaln data
  #add lat-long to each sample
colxndata <- read.csv("colxnsites2014_elevs.csv")
fecaln.raw <- rename(fecaln.raw, SampleID = Sample.ID)
fn.data <- inner_join(fecaln.raw, colxndata, by = "SampleID") 
  #remove extraneous columns; format date
fn.data <- subset(fn.data, select = -c(ResidentMigratory,
                                       SampleType, SampleDate, Collector))
fn.data$Date <- as.Date(as.character(fn.data$Date), format = "%m/%d/20%y")
  #pathetically create column relating collxn date to NDVI/EVI date
fn.data$SDate <- c("2014-06-10", "2014-06-10","2014-06-10","2014-06-10","2014-06-10","2014-06-10",
                   "2014-06-26","2014-06-26","2014-06-26","2014-06-26","2014-06-26",
                   "2014-07-12","2014-07-12","2014-07-12","2014-07-12",
                   "2014-07-28","2014-07-28","2014-07-28","2014-07-28",
                   "2014-08-13","2014-08-13","2014-08-13","2014-08-13","2014-08-13","2014-08-13","2014-08-13",
                   "2014-08-29","2014-08-29","2014-08-29","2014-08-29","2014-08-29",
                   "2014-09-14","2014-09-14","2014-09-14","2014-09-14",
                   "2014-09-30","2014-09-30","2014-09-30")
fn.data$SDate <- as.Date(as.character(fn.data$SDate), format='%Y-%m-%d')

###combine data
data <- inner_join(fn.data, ndvi.data, by=c("SampleID", "SDate"))   
  #shit, now what?

##########PLOTS##############
###check out the data
#hey data, come here often?

#Normalizing FN
par(mfrow=c(3,1))
hist(data$PctFN)
hist(1/(data$PctFN))
hist(log(data$PctFN))

#Normalizing NDVI
par(mfrow=c(3,1))
hist(data$NDVI) 
hist(log(data$NDVI))
hist(data$NDVI^2)
##yeesh, let's look at all NDVI data
par(mfrow=c(2,1))
hist(ndvi.data$NDVI) 
#hist(log(ndvi.data$NDVI)) #terrible
hist(ndvi.data$NDVI^2)

#Look at most normal of each
par(mfrow=c(3,1))
hist(1/(data$PctFN), main="FN")
hist(data$NDVI^2, main = "NDVI")
scatter.smooth(1/data$PctFN ~ data$NDVI^2)

#Each by date - transformed
par(mfrow=c(1,1))
plot(1/data$PctFN ~ data$SDate, col=c("black", "red")[data$MigStatus], ylab="PctFN^-1")
lines(loess.smooth(data$Date, 1/data$PctFN), col="brown")
par(new=TRUE)
plot(data$NDVI^2 ~ data$SDate, col=c("orange", "purple")[data$MigStatus], ylab="")
lines(loess.smooth(data$Date, data$NDVI^2), col="green")
axis(4)
mtext(side=4, 'NDVI^2')

par(mfrow=c(1,1))
scatter.smooth(1/data$PctFN ~ data$NDVI^2, col=c("black", "red")[data$MigStatus])
scatter.smooth(1/data$PctFN ~ data$Elevm, col=c("black", "red")[data$MigStatus])
scatter.smooth(1/data$PctFN ~ data$Date, col=c("black", "red")[data$MigStatus])

###DATA BELOW THIS MAY NOT BE TRANSFORMED###
#Each by date - raw
par(mfrow=c(1,1))
plot(data$PctFN ~ data$SDate, col=c("black", "red")[data$MigStatus], ylab="PctFN")
lines(loess.smooth(data$Date, data$PctFN), col="brown")
par(new=TRUE)
plot(data$NDVI ~ data$SDate, col=c("orange", "purple")[data$MigStatus], ylab="")
lines(loess.smooth(data$Date, data$NDVI), col="green")
axis(4)
mtext(side=4, 'NDVI')

scatter.smooth(ndvi.data$NDVI ~ ndvi.data$SDate, xlim=c(16230,16350))
par(new=TRUE)
plot(data$NDVI ~ data$SDate, col="green", xlim=c(16230,16350))
lines(loess.smooth(data$Date, data$NDVI), col="green")

##########REGRESSIONS##############
###QUESTION: What factors best explain var'n in fecal N?




##########PLAYING WITH DATA##############
#1. Split out by resident/migrant?? (Meh)
#2. Plot on map to just glance at habitat stuff
#3. determine best measures of spread for fn and ndvi
  #then compare those
cor.test(data$NDVI^2, 1/data$PctFN, alternative="two.sided",
         method="pearson", conf.level = 0.95)


##########CSV'S##############
write.csv(tab, file = "regressions.csv")
write.csv(data, file = "fn-ndvi.csv")

ndvi.coord.data <- inner_join(ndvi.data, colxndata, by = "SampleID")
write.csv(ndvi.coord.data, file = "ndvicoords.csv")
