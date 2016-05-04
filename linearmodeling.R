######################################
######Figuring out linear models######
######################################
#Run this after top portion of LandEcol code

#######
#PLOTS#
#######

########
#NDVI

###1ai. Scatterplots - raw, transformed
par(mfrow=c(1,1))
scatter.smooth(data$PctFN ~ data$NDVI) #raw

###1aii. Linear regressions/Residual plots
ndvi <- lm(PctFN ~ NDVI, data=data); summary(ndvi)
par(mfrow=c(2,2)) ; plot(ndvi)

########
#ELEVATION

###1bi. Scatterplots
par(mfrow=c(2,1))
scatter.smooth(data$PctFN ~ data$Elevm) #raw
scatter.smooth(data$PctFN ~ data$Elevm^2) #elev^2 (xup)
hist(data$Elevm)

###1bii. Linear regressions/Residual plots
elev.set <- list( )
elev.set[[1]] <- lm(PctFN ~ Elevm, data=data)
elev.set[[2]] <- lm(PctFN ~ I(Elevm^2), data=data)
elev.set[[3]] <- lm(PctFN ~ Elevm+I(Elevm^2), data=data)
names(elev.set) <- c("ElevRaw", "Elev^2", "Elev+Elev^2")
elevtable <- aictab(elev.set, second.ord=TRUE)
elevresults <- print(elevtable, digits = 2, LL = FALSE)

elev.lm <- lm(PctFN ~ I(Elevm^2), data=data)
par(mfrow=c(2,2)) ; plot(elev.lm)

elev.lm1 <- lm(PctFN ~ Elevm, data=data)
par(mfrow=c(2,2)) ; plot(elev.lm1)

########
#DAY

###1ci. Scatterplots
par(mfrow=c(2,1))
scatter.smooth(data$PctFN ~ data$DOY) #raw
scatter.smooth(data$PctFN ~ data$DOY^0.5) #DOY^.5 (xdn)
scatter.smooth(data$PctFN ~ log(data$DOY)) #log(DOY) (xdn2)

###1cii. Linear regressions/Residual plots
doy <- lm(PctFN ~ DOY, data=data); summary(doy)
par(mfrow=c(2,2)) ; plot(doy)

doy2 <- lm(data$PctFN ~ data$DOY*data$DOY); summary(doy2)
par(mfrow=c(2,2)) ; plot(doy2)

########
#LANDCOV
plot(data$PctFN ~ data$Landcov) #raw

########
#MODELS#
########

##CHECKING FOR CORRELATIONS
#NDVI-Elev
cor.test(data$NDVI, data$Elev, alternative="two.sided",
         method="pearson", conf.level = 0.95)
#NDVI-DOY
cor.test(data$NDVI, data$DOY, alternative="two.sided",
         method="pearson", conf.level = 0.95)
#NDVI-Treecov ###doesn't work
cor.test(data$NDVI, data$Treecov, alternative="two.sided",
         method="pearson", conf.level = 0.95)
#Elev-DOY
cor.test(data$Elev, data$DOY, alternative="two.sided",
         method="pearson", conf.level = 0.95)
#Elev-Treecov
cor.test(data$Elev, data$Treecov, alternative="two.sided",
         method="pearson", conf.level = 0.95)
#DOY-Treecov
cor.test(data$DOY, data$Treecov, alternative="two.sided",
         method="pearson", conf.level = 0.95)

######################
#just trying stuff

a <- lm(PctFN ~ Date * HabClass, data=data); summary(a)
b <- lm(PctFN ~ Landcov, data=data); summary(b)
c <- lm(NDVI ~ HabClass, data=data); summary(c)
d <- lm(PctFN ~ Date * Treecov, data=data); summary(d)

######actual aic

#Decide whether Elevm should be squared
elev.set <- list( )
elev.set[[1]] <- lm(PctFN ~ Elevm, data=data)
elev.set[[2]] <- lm(PctFN ~ I(Elevm*Elevm), data=data)
elev.set[[3]] <- lm(PctFN ~ Elevm+I(Elevm*Elevm), data=data)
names(elev.set) <- c("Elevation", 
                     "Elevation^2",
                     "Elev+Elev^2")
elevtable <- aictab(elev.set, second.ord=TRUE)
elevresults <- print(elevtable, digits = 2, LL = FALSE)
  #fuck it, i'm going with no

#decide whether elev is better with land/treecover
elev.set <- list( )
elev.set[[1]] <- lm(PctFN ~ Elevm, data=data)
elev.set[[2]] <- lm(PctFN ~ Elevm+Treecov, data=data)
elev.set[[3]] <- lm(PctFN ~ Elevm+Landcov, data=data)
names(elev.set) <- c("Elevation", 
                     "Elevation + Tree cover",
                     "Elevation + Landcover")
elevtable <- aictab(elev.set, second.ord=TRUE)
elevresults <- print(elevtable, digits = 2, LL = FALSE)
  #elev alone FTW

#decide whether NDVI is better with land/tree cover
ndvi.set <- list( )
ndvi.set[[1]] <- lm(PctFN ~ NDVI, data=data)
ndvi.set[[2]] <- lm(PctFN ~ NDVI*Treecov, data=data)
ndvi.set[[3]] <- lm(PctFN ~ NDVI+Treecov, data=data)
names(ndvi.set) <- c("NDVI", 
                     "NDVI * Treecover",
                     "NDVI + Treecover")
ndvitable <- aictab(ndvi.set, second.ord=TRUE)
ndviresults <- print(ndvitable, digits = 2, LL = FALSE)
  #NDVI alone FTW... weird

#decide whether Date is better with land/tree cover
date.set <- list( )
date.set[[1]] <- lm(PctFN ~ Date, data=data)
date.set[[2]] <- lm(PctFN ~ Date+Landcov, data=data)
date.set[[3]] <- lm(PctFN ~ Date+Treecov, data=data)
names(date.set) <- c("Date", 
                     "Date + Landcov",
                     "Date + Treecover")
datetable <- aictab(date.set, second.ord=TRUE)
dateresults <- print(datetable, digits = 2, LL = FALSE)
#Date or Date+Treecover

#Date+Elev or Date*Elev 
dateelev.set <- list( )
dateelev.set[[1]] <- lm(PctFN ~ Date+Elevm, data=data)
dateelev.set[[2]] <- lm(PctFN ~ Date*Elevm, data=data)
names(dateelev.set) <- c("Date + Elev",
                     "Date * Elev")
dateelevtable <- aictab(dateelev.set, second.ord=TRUE)
dateelevresults <- print(dateelevtable, digits = 2, LL = FALSE)

#landcover or tree cover for FN
cov.set <- list( )
cov.set[[1]] <- lm(PctFN ~ Landcov, data=data)
cov.set[[2]] <- lm(PctFN ~ Treecov, data=data)
names(cov.set) <- c("Landcov",
                     "Treecover")
covtable <- aictab(cov.set, second.ord=TRUE)
covresults <- print(covtable, digits = 2, LL = FALSE)

#final models
Cand.set <- list( )
Cand.set[[1]] <- glm(PctFN ~ Date, data=data)
Cand.set[[2]] <- glm(PctFN ~ Date+Elevm, data=data)
Cand.set[[3]] <- glm(PctFN ~ Date+NDVI, data=data)
Cand.set[[4]] <- glm(PctFN ~ Date+Elevm+NDVI, data=data)
Cand.set[[5]] <- glm(PctFN ~ NDVI*Treecov, data=data)
names(Cand.set) <- c("Date", 
                     "Date + Elevation", 
                     "Date + NDVI", 
                     "Date + Elevation + NDVI",
                     "NDVI * Tree cover")

aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)

####################
#shitload of models
tons.set <- list( )
tons.set[[1]] <- lm(PctFN ~ Date, data=data)
tons.set[[2]] <- lm(PctFN ~ Date+Treecov, data=data)
tons.set[[3]] <- lm(PctFN ~ Date+Elevm, data=data)
tons.set[[4]] <- lm(PctFN ~ Date+NDVI, data=data)
tons.set[[5]] <- lm(PctFN ~ Date+Elevm+Treecov, data=data)
tons.set[[6]] <- lm(PctFN ~ Date+Elevm+NDVI, data=data)
tons.set[[7]] <- lm(PctFN ~ Date+Elevm+Treecov+NDVI, data=data)
tons.set[[8]] <- lm(PctFN ~ Date+Elevm+Treecov*NDVI, data=data)
tons.set[[9]] <- lm(PctFN ~ Date+Elevm+Landcov*NDVI, data=data)
names(tons.set) <- c("Date", 
                     "Date+Treecov", 
                     "Date+Elevm", 
                     "Date+NDVI",
                     "Date+Elevm+Treecov",
                     "Date+Elevm+NDVI",
                     "Date+Elevm+Treecov+NDVI",
                     "Date+Elevm+Treecov*NDVI")
tonstable <- aictab(tons.set, second.ord=TRUE)
tonsresults <- print(tonstable, digits = 2, LL = FALSE)

#See whether this is different for mig and res
###RES
res.set <- list( )
res.set[[1]] <- lm(PctFN ~ Date, data=res)
res.set[[2]] <- lm(PctFN ~ Date+Treecov, data=res)
res.set[[3]] <- lm(PctFN ~ Date+Elevm, data=res)
res.set[[4]] <- lm(PctFN ~ Date+NDVI, data=res)
res.set[[5]] <- lm(PctFN ~ Date+Elevm+Treecov, data=res)
res.set[[6]] <- lm(PctFN ~ Date+Elevm+NDVI, data=res)
res.set[[7]] <- lm(PctFN ~ Date+Elevm+Treecov+NDVI, data=res)
res.set[[8]] <- lm(PctFN ~ Date+Elevm+Treecov*NDVI, data=res)
res.set[[9]] <- lm(PctFN ~ Date+Elevm+Landcov, data=res)
names(res.set) <- c("Date", 
                    "Date+Treecov", 
                    "Date+Elevm", 
                    "Date+NDVI",
                    "Date+Elevm+Treecov",
                    "Date+Elevm+NDVI",
                    "Date+Elevm+Treecov+NDVI",
                    "Date+Elevm+Treecov*NDVI",
                    "Date+Elevm+Landcov")
restable <- aictab(res.set, second.ord=TRUE)
resresults <- print(restable, digits = 2, LL = FALSE)

###MIG
mig.set <- list( )
mig.set[[1]] <- lm(PctFN ~ Date, data=mig)
mig.set[[2]] <- lm(PctFN ~ Date+Treecov, data=mig)
mig.set[[3]] <- lm(PctFN ~ Date+Elevm, data=mig)
mig.set[[4]] <- lm(PctFN ~ Date+NDVI, data=mig)
mig.set[[5]] <- lm(PctFN ~ Date+Elevm+Treecov, data=mig)
mig.set[[6]] <- lm(PctFN ~ Date+Elevm+NDVI, data=mig)
mig.set[[7]] <- lm(PctFN ~ Date+Elevm+Treecov+NDVI, data=mig)
mig.set[[8]] <- lm(PctFN ~ Date+Elevm+Treecov*NDVI, data=mig)
mig.set[[9]] <- lm(PctFN ~ Elevm, data=mig)
names(mig.set) <- c("Date", 
                    "Date+Treecov", 
                    "Date+Elevm", 
                    "Date+NDVI",
                    "Date+Elevm+Treecov",
                    "Date+Elevm+NDVI",
                    "Date+Elevm+Treecov+NDVI",
                    "Date+Elevm+Treecov*NDVI",
                    "Elevm")
migtable <- aictab(mig.set, second.ord=TRUE)
migresults <- print(migtable, digits = 2, LL = FALSE)


#shitload of models v2 - with mig status - which is dumb
tons2.set <- list( )
tons2.set[[1]] <- lm(PctFN ~ Date+Elevm, data=data)
tons2.set[[2]] <- lm(PctFN ~ Date+Elevm+MigStatus, data=data)
tons2.set[[3]] <- lm(PctFN ~ Date+Elevm+NDVI, data=data)
tons2.set[[4]] <- lm(PctFN ~ MigStatus, data=data)
tons2.set[[5]] <- lm(PctFN ~ Date+Elevm+Treecov, data=data)
tons2.set[[6]] <- lm(PctFN ~ Date+Elevm+Treecov+MigStatus, data=data)
names(tons2.set) <- c("Date+Elevm", 
                     "Date+Elevm+MigStatus", 
                     "Date+Elevm+NDVI", 
                     "MigStatus",
                     "Date+Elevm+Treecov",
                     "Date+Elevm+Treecov+MigStatus")
tons2table <- aictab(tons2.set, second.ord=TRUE)
tons2results <- print(tons2table, digits = 2, LL = FALSE)

#######################
#lms of top 3
de <- lm(PctFN ~ Date+Elevm, data=data); summary(de)
den <- lm(PctFN ~ Date+Elevm+NDVI, data=data); summary(den)
det <- lm(PctFN ~ Date+Elevm+Treecov, data=data); summary(det)
d <- lm(PctFN ~ Date, data=data); summary(d)
#par(mfrow=c(2,2))
#plot(d)
#plot(den)
#plot(det)

lm1 <- c(de$coefficients[1], summary(de)$adj.r.squared, summary(de)$sigma)
lm2 <- c(den$coefficients[1], summary(den)$adj.r.squared, summary(den)$sigma)
lm3 <- c(det$coefficients[1], summary(det)$adj.r.squared, summary(det)$sigma)
lm4 <- c(d$coefficients[1], summary(d)$adj.r.squared, summary(d)$sigma)

tprep <- rbind(lm1, lm2, lm3, lm4)
tab <- as.data.frame(tprep, row.names = 
                       c("Date + Elevation", 
                         "Date + Elevation + NDVI", 
                         "Date + Elevation + Treecov",
                         "Date"))
#tab <- rename(tab, NDVIcoeff = NDVI)
tab <- rename(tab, AdjRsquared = V2)
tab <- rename(tab, StdError = V3)
View(tab)
write.csv(tab, file = "regressions.csv")
