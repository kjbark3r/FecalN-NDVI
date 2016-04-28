#######GRAPHS FOR LANDECOL PROJECT PRESENTATION
#######REQ. RUNNING 1ST PART OF BARKER_LANDECOL.R
#######COMPARING NDVI AND FN

#differences in res and mig FN
res <- filter(data, MigStatus=="Res")
mig <- filter(data, MigStatus=="Mig")
par(mfrow=c(1,1))
boxplot(PctFN ~ MigStatus, data, ylab  = "Fecal N (% dry matter)", 
        main = "Adult Female Elk Nutrition")

t.test(PctFN ~ MigStatus, data=data, paired=FALSE, var.equal=FALSE,
       conf.level=0.95)

#FN by date - res and mig - just lines
plot(PctFN ~ Date, data=data)

par(mfrow=c(1,1))
scatter.smooth(data$PctFN ~ data$NDVI, xlab="NDVI", 
               ylab="Percent Fecal N", main = "All Elk")

par(mfrow=c(2,1))
scatter.smooth(res$PctFN ~ res$NDVI, xlab="NDVI", 
               ylab="Percent Fecal N", main = "Residents",
               xlim=c(4000,8000), ylim=c(1.9,3.8))
scatter.smooth(mig$PctFN ~ mig$NDVI, xlab="NDVI", 
               ylab="Percent Fecal N", main = "Migrants",
               xlim=c(4000,8000), ylim=c(1.9,3.8))

###without migrant outlier
mig.nooutlier <- mig[-3,]

#FN ~ NDVI (all animals, not res/mig)


#FN and NDVI separately by date

#FN by date

#FN by elevation

