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
library(zoo) #integration - rollmean
library(MESS) #integration - spline

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
colxndata <- read.csv("colxnsites2014.csv")
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
par(mfrow=c(3,1))
hist(data$NDVI)
hist(1/(data$PctFN))
hist(log(data$PctFN))

par(mfrow=c(1,1))
plot(log(data$PctFN) ~ data$SDate, col=c("black", "red")[data$MigStatus], ylab="log(PctFN)")
lines(loess.smooth(data$Date, log(data$PctFN)), col="blue")
par(new=TRUE)
plot(data$NDVI ~ data$SDate, col=c("orange", "purple")[data$MigStatus])
lines(loess.smooth(data$Date, data$NDVI), col="green")
axis(4)
mtext(side=4, 'NDVI')

par(mfrow=c(1,1))
scatter.smooth(log(data$PctFN) ~ data$NDVI, col=c("black", "red")[data$MigStatus])

scatter.smooth(ndvi.data$NDVI ~ ndvi.data$SDate, xlim=c(16230,16350))
par(new=TRUE)
plot(data$NDVI ~ data$SDate, col="green", xlim=c(16230,16350))
lines(loess.smooth(data$Date, data$NDVI), col="green")

##########REGRESSIONS##############
###QUESTION: What factors best explain var'n in fecal N?
###make a table of the outputs of various linear regressions
reg1 <- lm(PctFN ~ NDVI, data=data) ; summary(reg1)
reg2 <- lm(PctFN ~ NDVI+SDate, data=data) ; summary(reg2)
reg3 <- lm(PctFN ~ NDVI+SDate+MigStatus, data=data) ; summary(reg3)
reg4 <- lm(PctFN ~ NDVI+SDate*MigStatus, data=data) ; summary(reg4)
reg5 <- lm(PctFN ~ SDate+MigStatus, data=data) ; summary(reg5)
reg6 <- lm(PctFN ~ SDate*MigStatus, data=data) ; summary(reg6)
reg7 <- lm(PctFN ~ SDate, data=data) ; summary(reg7)
  
lm1 <- c(reg1$coefficients[1], reg1$coefficients[2], summary(reg1)$r.squared, summary(reg1)$sigma)
lm2 <- c(reg2$coefficients[1], reg2$coefficients[2], summary(reg2)$r.squared, summary(reg2)$sigma)
lm3 <- c(reg3$coefficients[1], reg3$coefficients[2], summary(reg3)$r.squared, summary(reg3)$sigma)
lm4 <- c(reg4$coefficients[1], reg4$coefficients[2], summary(reg4)$r.squared, summary(reg4)$sigma)
lm5 <- c(reg5$coefficients[1], reg5$coefficients[2], summary(reg5)$r.squared, summary(reg5)$sigma)
lm6 <- c(reg6$coefficients[1], reg6$coefficients[2], summary(reg6)$r.squared, summary(reg6)$sigma)
lm7 <- c(reg7$coefficients[1], reg7$coefficients[2], summary(reg7)$r.squared, summary(reg7)$sigma)

tprep <- rbind(lm1, lm2, lm3, lm4, lm5, lm6, lm7)
tab <- as.data.frame(tprep, row.names = 
                       c("NDVI", "NDVI + Date", "NDVI + Date + MigStatus",
                         "NDVI + Date * MigStatus", "Date + MigStatus", 
                         "Date * MigStatus", "Date"))
tab <- rename(tab, NDVIcoeff = NDVI)
tab <- rename(tab, Rsquared = V3)
tab <- rename(tab, StdError = V4)
View(tab)


##########PLAYING WITH DATA##############
#1. Split out by resident/migrant?? (Meh)
#2. Plot on map to just glance at habitat stuff
#3. determine best measures of spread for fn and ndvi
  #then compare those


cor.test(data$NDVI, 1/(data$PctFN), alternative="two.sided",
         method="pearson", conf.level = 0.95)

##########CSV'S##############
write.csv(tab, file = "regressions.csv")
write.csv(data, file = "fn-ndvi.csv")

ndvi.coord.data <- inner_join(ndvi.data, colxndata, by = "SampleID")
write.csv(ndvi.coord.data, file = "ndvicoords.csv")
