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

#decide whether elev interacts with landcover
elev.set <- list( )
elev.set[[1]] <- lm(PctFN ~ Elevm, data=data)
elev.set[[2]] <- lm(PctFN ~ Elevm*Landcov, data=data)
names(elev.set) <- c("Elevation", 
                     "Elevation * Landcover")
elevtable <- aictab(elev.set, second.ord=TRUE)
elevresults <- print(elevtable, digits = 2, LL = FALSE)
  #elev alone FTW

#decide whether NDVI interacts with land/tree cover
ndvi.set <- list( )
ndvi.set[[1]] <- lm(PctFN ~ NDVI, data=data)
ndvi.set[[2]] <- lm(PctFN ~ NDVI*Landcov, data=data)
ndvi.set[[3]] <- lm(PctFN ~ NDVI*Treecov, data=data)
names(ndvi.set) <- c("NDVI", 
                     "NDVI * Landcover",
                     "NDVI * Treecover")
ndvitable <- aictab(ndvi.set, second.ord=TRUE)
ndviresults <- print(ndvitable, digits = 2, LL = FALSE)
  #NDVI alone FTW... weird

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

#shitload of models
tons.set <- list( )
tons.set[[1]] <- lm(PctFN ~ Date, data=data)
tons.set[[2]] <- lm(PctFN ~ Date+Elevm, data=data)
tons.set[[3]] <- lm(PctFN ~ Date+NDVI+Landcov, data=data)
tons.set[[4]] <- lm(PctFN ~ Date+Elevm+NDVI, data=data)
tons.set[[5]] <- lm(PctFN ~ Date+Elevm+NDVI+Landcov, data=data)
tons.set[[6]] <- lm(PctFN ~ Treecov, data=data)
tons.set[[7]] <- lm(PctFN ~ Date+Elevm+Landcov, data=data)
tons.set[[8]] <- lm(PctFN ~ NDVI+Landcov, data=data)
names(tons.set) <- c("Date", 
                     "Date + Elevation", 
                     "Date + NDVI*Landcov", 
                     "Date + Elevation + NDVI",
                     "Date+Elevm+NDVI*Landcov",
                     "Treecov",
                     "Date+Elevm+Landcov",
                     "NDVI+Landcov")
tonstable <- aictab(tons.set, second.ord=TRUE)
tonsresults <- print(tonstable, digits = 2, LL = FALSE)
