##########MUCKERY##############

##########
#1. remove late july and early august dates; rerun plot and correlation
rmvd.data <- filter(data, SDate == "2014-06-10" | SDate == "2014-06-26" | 
                      SDate == "2014-07-12"| SDate == "2014-08-29" | 
                      SDate == "2014-09-14"| SDate == "2014-09-30")
#untransformed
par(mfrow=c(1,1))
plot(rmvd.data$PctFN ~ rmvd.data$SDate, col=c("black", "red")[rmvd.data$MigStatus], ylab="PctFN", main="Removed Data, Untransformed")
lines(loess.smooth(rmvd.data$Date, rmvd.data$PctFN), col="brown")
par(new=TRUE)
plot(rmvd.data$NDVI ~ rmvd.data$SDate, col=c("orange", "purple")[rmvd.data$MigStatus], ylab="")
lines(loess.smooth(rmvd.data$Date, rmvd.data$NDVI), col="green")
axis(4)
mtext(side=4, 'NDVI')
      
cor.test(rmvd.data$NDVI, rmvd.data$PctFN, alternative="two.sided",
         method="pearson", conf.level = 0.95)

#transformed
par(mfrow=c(1,1))
plot(rmvd.data$PctFN ~ rmvd.data$SDate, col=c("black", "red")[rmvd.data$MigStatus], ylab="PctFN", main="Removed Data, Untransformed")
lines(loess.smooth(rmvd.data$Date, rmvd.data$PctFN), col="brown")
par(new=TRUE)
plot(rmvd.data$NDVI ~ rmvd.data$SDate, col=c("orange", "purple")[rmvd.data$MigStatus], ylab="")
lines(loess.smooth(rmvd.data$Date, rmvd.data$NDVI), col="green")
axis(4)
mtext(side=4, 'NDVI')

cor.test(rmvd.data$NDVI, rmvd.data$PctFN, alternative="two.sided",
         method="pearson", conf.level = 0.95)

##########
#2. split out my migratory status; rerun plot and correlation
##2a. do this with and without those removed dates above?
res <- filter(data, MigStatus=="Res")
mig <- filter(data, MigStatus=="Mig")

#correlation test - still neither one works
cor.test(res$NDVI^2, 1/res$PctFN, alternative="two.sided",
         method="pearson", conf.level = 0.95)
cor.test(mig$NDVI^2, 1/mig$PctFN, alternative="two.sided",
         method="pearson", conf.level = 0.95)

##########
#3. check out QA for those weird dates
qa <- filter(remote.raw, Type == "Pellet 2014")
qa <- select(qa, starts_with("ndvi."), starts_with("summary"), PlotID)
qa$PlotID <- extract_numeric(qa$PlotID)
qa <- rename(qa, SampleID = PlotID) 
qa <- gather(qa, key = SDate, value = "NDVI", -SampleID)

#make 2 dfs- one gathered NDVI, one gathered QA
#then rejoin based on plot ID or date
qa.ndvi <- filter(qa, NDVI > 2)
qa.ndvi$SDate <- substr(qa.ndvi$SDate, 6, 13)
qa.ndvi$SDate <- as.Date(as.character(qa.ndvi$SDate), format='%Y%m%d')

qa.qa <- filter(qa, NDVI < 3 & NDVI > -1)
qa.qa <- rename(qa.qa, QA = NDVI)
qa.qa$SDate <- substr(qa.qa$SDate, 11, 18)
qa.qa$SDate <- as.Date(as.character(qa.qa$SDate), format='%Y%m%d')

qa.data <- inner_join(qa.ndvi, qa.qa, by=c("SampleID", "SDate")) 
qa.data <- inner_join(qa.data, data, by=c("SampleID", "SDate"))

#just looking at misc graphs

##res/mig FN and NDVI by date
par(mfrow=c(2,2))
plot(res$PctFN ~ res$SDate, col="brown", ylab="Fecal N (%)", 
     ylim=c(2,4), xlab="Date", main="Resident")
lines(loess.smooth(res$Date, res$PctFN), col="brown")
plot(res$NDVI ~ res$SDate, col="green", ylab="NDVI", xlab="Date")
lines(loess.smooth(res$Date, res$NDVI), col="green")

plot(mig$PctFN ~ mig$SDate, col="brown", ylab="Fecal N (%)", 
     ylim=c(2,4), xlab="Date", main="Migrant")
lines(loess.smooth(mig$Date, mig$PctFN), col="brown")
plot(mig$NDVI ~ mig$SDate, col="green", ylab="NDVI", xlab="Date")
lines(loess.smooth(mig$Date, mig$NDVI), col="green")

##res/mig FN-NDVI relationship - transformed data
par(mfrow=c(2,1))
scatter.smooth(1/res$PctFN ~ res$NDVI^2, col=c("black", "red")[res$MigStatus],
               xlab="NDVI^2", ylab="Percent Fecal N^-1", main = "Resident")
scatter.smooth(1/mig$PctFN ~ mig$NDVI^2, col=c("black", "red")[mig$MigStatus],
               xlab="NDVI^2", ylab="Percent Fecal N^-1", main = "Migrant")

##res/mig FN-NDVI relationship - raw data
par(mfrow=c(2,1))
scatter.smooth(res$PctFN ~ res$NDVI, xlab="NDVI", 
               ylab="Percent Fecal N", main = "Resident")
scatter.smooth(mig$PctFN ~ mig$NDVI, xlab="NDVI", 
               ylab="Percent Fecal N", main = "Migrant")

##FN-NDVI relationship - raw vs transformed data
par(mfrow=c(2,1))
scatter.smooth(data$PctFN ~ data$NDVI, xlab="NDVI", 
               ylab="Percent Fecal N", main = "Raw")
scatter.smooth(1/data$PctFN ~ data$NDVI^2, xlab="NDVI^2", 
               ylab="1/Percent Fecal N", main = "Transformed")

#making linear relationship between FN and date
par(mfrow=c(1,1))
scatter.smooth(data$PctFN ~ data$Date) #raw
scatter.smooth(1/data$PctFN ~ data$Date) #exp fn
scatter.smooth(log(data$PctFN) ~ data$Date) #log fn

########AIC
#aic stuff
Cand.set <- list( )
Cand.set[[1]] <- glm(1/PctFN ~ Date, data=data)
Cand.set[[2]] <- glm(1/PctFN ~ Date+Elevm, data=data)
Cand.set[[3]] <- glm(1/PctFN ~ Date*Elevm, data=data)
names(Cand.set) <- c("date", "date+elev", "date*elev")

aictab(Cand.set, modnames=NULL, second.ord=TRUE, nobs=NULL)
