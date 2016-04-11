######################################
######Figuring out linear models######
######################################
#Run this after top portion of LandEcol code

#1. Determine linearity of relationships
#make them

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
