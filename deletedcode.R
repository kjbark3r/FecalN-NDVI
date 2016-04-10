##Deleted code from LandEco lproject
##in case it's actually handy for some inexplicable reason

##df of sampling dates, in temporal order
dates <- data.frame(c(unique(unlist(ndvi.data$Date))))
dates <- rename(dates, SDate = c.unique.unlist.ndvi.data.Date...)
dates <- arrange(dates, SDate)

#named this via Key so didn't have to rename
ndvi.data <- rename(ndvi.data, SDate = Date)


###prep EVI data
evi.data <- gather(remote.data, key = Date, value = "EVI", -SampleID)
evi.data <- na.omit(evi.data)
evi.data$EVI.date <- substr(evi.data$EVI.date, 5, 12)
  #make EVI.date column into an actual date
evi.data$EVI.date <- as.Date(as.character(evi.data$EVI.date), format='%Y%m%d')

###if you wanna check anything out
summary(remote.raw)
str(remote.raw)
summary(fecaln.raw)
str(fecaln.raw)

##integrals, apparently
> auc(data$SDate, data$NDVI, type='spline')
[1] 681871
> auc(data$SDate, data$PctFN, type='spline')
[1] 283.9458

library(zoo) #integration - rollmean
library(MESS) #integration - spline

###make a table of the outputs of various linear regressions
#deleted these bc it makes no sense to regress NDVI on FN
###especially linearly since their (non)relationship is nonlinear
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

library(AICcmodavg) #AICc

#Each by date - raw
par(mfrow=c(1,1))
plot(data$PctFN ~ data$SDate, col=c("black", "red")[data$MigStatus], ylab="PctFN")
lines(loess.smooth(data$Date, data$PctFN), col="brown")
par(new=TRUE)
plot(data$NDVI ~ data$SDate, col=c("orange", "purple")[data$MigStatus], ylab="")
lines(loess.smooth(data$Date, data$NDVI), col="green")
axis(4)
mtext(side=4, 'NDVI')

#below is the same as scatter.smooth
plot(data$PctFN ~ data$NDVI, xlab="NDVI", 
     ylab="Percent Fecal N", main = "Raw")
lines(loess.smooth(data$NDVI, data$PctFN))
