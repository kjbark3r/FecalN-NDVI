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
colxndata <- read.csv("colxnsites2014_elevs_habs.csv")

###load libraries
  #data wrangling
library(dplyr)
library(tidyr)
  #data analysis
library(AICcmodavg)

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
  #set hab types
fn.data <- rename(fn.data, HabClass = RASTERVALU) 
fn.data <- mutate(fn.data, Landcov = ifelse(HabClass == 0, "Mask", 
                                     ifelse(HabClass == 1, "Badlands",
                                     ifelse(HabClass == 2, "Riparian",
                                     ifelse(HabClass == 3, "Forest",
                                     ifelse(HabClass == 4, "Shrub",
                                     ifelse(HabClass == 5, "Sagebrush",
                                     ifelse(HabClass == 6, "Grassland",
                                     ifelse(HabClass == 7, "Agricultural",
                                            NA)))))))))
fn.data <- mutate(fn.data, Treecov = ifelse(HabClass == 0, "N", 
                                     ifelse(HabClass == 1, "N",
                                     ifelse(HabClass == 2, "Y",
                                     ifelse(HabClass == 3, "Y",
                                     ifelse(HabClass == 4, "N",
                                     ifelse(HabClass == 5, "N",
                                     ifelse(HabClass == 6, "N",
                                     ifelse(HabClass == 7, "N",
                                            NA)))))))))
fn.data$Landcov <- as.factor(fn.data$Landcov)
fn.data$Treecov <- as.factor(fn.data$Treecov)

###combine data
data <- inner_join(fn.data, ndvi.data, by=c("SampleID", "SDate"))   
#add day of year
data$DOY <- strftime(data$Date, format = "%j")
data$DOY <- as.numeric(data$DOY)
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
##vs plot-specific NDVI
par(mfrow=c(2,1))
hist(ndvi.data$NDVI, xlim=c(3000,9000), 
     main="All NDVI Data", xlab="NDVI")
hist(data$NDVI, xlim=c(3000,9000), 
     main="NDVI corresponding to sampling plots", , xlab="NDVI")

#Look at most normal of each
par(mfrow=c(3,1))
hist(1/(data$PctFN), main="FN")
hist(data$NDVI^2, main = "NDVI")
scatter.smooth(1/data$PctFN ~ data$NDVI^2)

##FN~NDVI - raw vs transformed data
par(mfrow=c(1,1))
scatter.smooth(data$PctFN ~ data$NDVI, xlab="NDVI", 
               ylab="Percent Fecal N", main = "Raw")

#Each by date - transformed
par(mfrow=c(1,1))
plot(1/data$PctFN ~ data$SDate, col=c("black", "red")[data$MigStatus], ylab="PctFN^-1")
lines(loess.smooth(data$Date, 1/data$PctFN), col="brown")
par(new=TRUE)
plot(data$NDVI^2 ~ data$SDate, col=c("orange", "purple")[data$MigStatus], ylab="")
lines(loess.smooth(data$Date, data$NDVI^2), col="green")
axis(4)
mtext(side=4, 'NDVI^2')

#FN-NDVI
scatter.smooth(1/data$PctFN ~ data$NDVI^2, col=c("black", "red")[data$MigStatus],
               xlab = "NDVI^2", ylab = "Percent Fecal N^-1")
#FN-Elev
scatter.smooth(1/data$PctFN ~ data$Elevm,
               xlab="Elevation (m)", ylab="Percent Fecal N^-1")
#FN-Elev by MigStatus
par(mfrow=c(2,1))
scatter.smooth(1/res$PctFN ~ res$Elevm,
               xlab="Elevation (m)", ylab="Percent Fecal N^-1", main = "Resident")
scatter.smooth(1/mig$PctFN ~ mig$Elevm,
               xlab="Elevation (m)", ylab="Percent Fecal N^-1", main = "Migrant")

#FN-Date
par(mfrow=c(1,1))
scatter.smooth(1/data$PctFN ~ data$Date,
               xlab="Date", ylab="1/Percent Fecal N")

#FN-DOY
par(mfrow=c(1,1))
scatter.smooth(data$PctFN ~ data$DOY,
               xlab="Day of Year", ylab="Percent Fecal N")

#FN-Date+Elev
scatter.smooth(1/data$PctFN ~ data$Elevm+data$DOY,
               xlab="Date*Elev", ylab="Percent Fecal N^-1")

#FN-Date by MigStatus
####NEED TO FORMAT DATES BETTER
par(mfrow=c(2,1))
scatter.smooth(1/res$PctFN ~ res$Date, col=c("black", "red")[res$MigStatus],
               xlab="Date", ylab="Percent Fecal N^-1", main = "Resident")
scatter.smooth(1/mig$PctFN ~ mig$Date, col=c("black", "red")[mig$MigStatus],
               xlab="Date", ylab="Percent Fecal N^-1", main = "Migrant")

###DATA BELOW THIS MAY NOT BE TRANSFORMED###
#Each by date - raw
par(mfrow=c(2,1))
plot(data$PctFN ~ data$SDate, col="brown", ylab="Fecal N (%)", xlab="Date")
lines(loess.smooth(data$Date, data$PctFN), col="brown")
plot(data$NDVI ~ data$SDate, col="green", ylab="NDVI", xlab="Date")
lines(loess.smooth(data$Date, data$NDVI), col="green")

#Each by date - raw - res/mig split
par(mfrow=c(3,1))
plot(res$PctFN ~ res$SDate, col="brown", ylab="Fecal N (%)", 
     ylim=c(2,4), xlab="Date", main="Resident")
lines(loess.smooth(res$Date, res$PctFN), col="brown")
plot(mig$PctFN ~ mig$SDate, col="brown", ylab="Fecal N (%)", 
     ylim=c(2,4), xlab="Date", main="Migrant")
lines(loess.smooth(mig$Date, mig$PctFN), col="brown")
plot(data$NDVI ~ data$SDate, col="green", ylab="NDVI", xlab="Date")
  lines(loess.smooth(data$Date, data$NDVI), col="green")

scatter.smooth(ndvi.data$NDVI ~ ndvi.data$SDate, xlim=c(16230,16350))
par(new=TRUE)
plot(data$NDVI ~ data$SDate, col="green", xlim=c(16230,16350))
lines(loess.smooth(data$Date, data$NDVI), col="green")


##########CORRELATIONS##############
#FN-NDVI - raw
cor.test(data$NDVI, data$PctFN, alternative="two.sided",
         method="pearson", conf.level = 0.95)

#FN-NDVI - transformed [this probably makes no sense]
cor.test(data$NDVI^2, 1/data$PctFN, alternative="two.sided",
         method="pearson", conf.level = 0.95)

#FN-Date
cor.test(data$DOY, data$PctFN, alternative="two.sided",
         method="pearson", conf.level = 0.95)

#FN-Elev
cor.test(data$Elevm, data$PctFN, alternative="two.sided",
         method="pearson", conf.level = 0.95)

##########REGRESSIONS##############
##Models## Plus residual plots, commented out
d <- lm(PctFN ~ Date, data=data); summary(d)
de <- lm(PctFN ~ Date+Elevm, data=data); summary(de)
dn <- lm(PctFN ~ Date+NDVI, data=data); summary(dn)
den <- lm(PctFN ~ Date+Elevm+NDVI, data=data); summary(den)
  #par(mfrow=c(2,2))
  #plot(d)
  #plot(de)
  #plot(dn)
  #plot(den)

lm1 <- c(de$coefficients[1], summary(de)$adj.r.squared, summary(de)$sigma)
lm2 <- c(den$coefficients[1], summary(den)$adj.r.squared, summary(den)$sigma)
lm3 <- c(d$coefficients[1], summary(d)$adj.r.squared, summary(d)$sigma)
lm4 <- c(dn$coefficients[1], summary(dn)$adj.r.squared, summary(dn)$sigma)

tprep <- rbind(lm1, lm2, lm3, lm4)
tab <- as.data.frame(tprep, row.names = 
                       c("Date + Elevation", "Date + Elevation + NDVI", "Date", "Date + NDVI"))
#tab <- rename(tab, NDVIcoeff = NDVI)
tab <- rename(tab, AdjRsquared = V2)
tab <- rename(tab, StdError = V3)
View(tab)
write.csv(tab, file = "regressions.csv")

##AIC##
Cand.set <- list( )
Cand.set[[1]] <- glm(PctFN ~ Date, data=data)
Cand.set[[2]] <- glm(PctFN ~ Date+Elevm, data=data)
Cand.set[[3]] <- glm(PctFN ~ Date+NDVI, data=data)
Cand.set[[4]] <- glm(PctFN ~ Date+Elevm+NDVI, data=data)
names(Cand.set) <- c("Date", 
                     "Date + Elevation", 
                     "Date + NDVI", 
                     "Date + Elevation + NDVI")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)


##########Misc CSV'S##############
write.csv(data, file = "fn-ndvi.csv")

ndvi.coord.data <- inner_join(ndvi.data, colxndata, by = "SampleID")
write.csv(ndvi.coord.data, file = "ndvicoords.csv")
