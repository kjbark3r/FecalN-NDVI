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