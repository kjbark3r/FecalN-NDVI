#####################################################~#
### Relating Remotely-sensed NDVI to Fecal Nitrogen ###
###        For NSERP FWP Report - Mar 2017          ###
################## Kristin Barker ###################~#
#####################################################~#

#GOAL: Quantify relationship between remotely sensed 
       #vegetation index and elk fecal nitrogen

########## SETUP ##############

## SET WD ##
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\FecalN-NDVI"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\FecalN-NDVI"
ifelse(file.exists(wd_workcomp), setwd(wd_workcomp), setwd(wd_laptop))
rm(wd_workcomp, wd_laptop)


## LOAD LIBRARIES ##
library(AICcmodavg) # model selection
library(raster) # for ndvi amp & ti-ndvi
library(tidyr) # data wrangling
library(dplyr) # data wrangling


## READ IN RAW DATA ##
remote.raw <- read.csv("remotedata.csv")
fecaln.raw <- read.csv("fecalndata2014.csv")
colxndata <- read.csv("colxnsites2014_elevs_habs.csv")


########## DATA PREP ##############

## REMOTE DATA - FROM GOOGLE EARTH ENGINE/BRADY ALLRED ##
# pull pellet-related NDVI data only; fix plot names; tidy
remote.data <- filter(remote.raw, Type == "Pellet 2014")
remote.data <- select(remote.data, starts_with("ndvi."), PlotID)
remote.data$PlotID <- extract_numeric(remote.data$PlotID)
remote.data <- rename(remote.data, SampleID = PlotID) 

## NDVI DATES ##
ndvi.data <- gather(remote.data, key = SDate, value = "NDVI", -SampleID)
ndvi.data$SDate <- substr(ndvi.data$SDate, 6, 13)
ndvi.data$SDate <- as.Date(as.character(ndvi.data$SDate), format='%Y%m%d')

## FECAL NITROGEN DATA ##
# add lat-long to each sample
fecaln.raw <- rename(fecaln.raw, SampleID = Sample.ID)
fn.data <- inner_join(fecaln.raw, colxndata, by = "SampleID") 
# remove extraneous columns; format date
fn.data <- subset(fn.data, select = -c(ResidentMigratory,
                                       SampleType, SampleDate, Collector))
fn.data$Date <- as.Date(as.character(fn.data$Date), format = "%m/%d/20%y")
# pathetically create column relating collxn date to NDVI/EVI date
fn.data$SDate <- c("2014-06-10", "2014-06-10","2014-06-10","2014-06-10","2014-06-10","2014-06-10",
                   "2014-06-26","2014-06-26","2014-06-26","2014-06-26","2014-06-26",
                   "2014-07-12","2014-07-12","2014-07-12","2014-07-12",
                   "2014-07-28","2014-07-28","2014-07-28","2014-07-28",
                   "2014-08-13","2014-08-13","2014-08-13","2014-08-13","2014-08-13","2014-08-13","2014-08-13",
                   "2014-08-29","2014-08-29","2014-08-29","2014-08-29","2014-08-29",
                   "2014-09-14","2014-09-14","2014-09-14","2014-09-14",
                   "2014-09-30","2014-09-30","2014-09-30")
fn.data$SDate <- as.Date(as.character(fn.data$SDate), format='%Y-%m-%d')
# set habitat types
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
fn.data <- mutate(fn.data, Treecov = ifelse(HabClass == 0, "0", 
                                     ifelse(HabClass == 1, "0",
                                     ifelse(HabClass == 2, "1",
                                     ifelse(HabClass == 3, "1",
                                     ifelse(HabClass == 4, "0",
                                     ifelse(HabClass == 5, "0",
                                     ifelse(HabClass == 6, "0",
                                     ifelse(HabClass == 7, "0",
                                            NA)))))))))
fn.data$Landcov <- as.factor(fn.data$Landcov)
fn.data$Treecov <- as.numeric(fn.data$Treecov)

# combine point data
data.tmp <- inner_join(fn.data, ndvi.data, by=c("SampleID", "SDate"))   
#add day of year
data.tmp$DOY <- strftime(data.tmp$Date, format = "%j")
data.tmp$DOY <- as.numeric(data.tmp$DOY)


## CREATE SEPARATE RES/MIG DFs ##
res <- filter(data, MigStatus=="Res")
mig <- filter(data, MigStatus=="Mig")


#### KJB NEXT STEPS ####

#1. correlation plots from bass's prof's code
#2. rerun covariate and model selection
#3. make your methods not suck... actually say what you did



#########################################
#### CUT CODE/MISC UNIMPORTANT STUFF ####

#### ADD RASTER DATA - TIME-INTEGRATED NDVI, NDVI AMPLITUDE ####

# set projections
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")

# read in rasters
ndvi.ti.sp <- raster("../Vegetation/writtenrasters/cropped/ndvi_ti_14.tif")
ndvi.amp.sp <- raster("../Vegetation/writtenrasters/cropped/ndvi_amp_14.tif")

# reproject in wgs84 to match plot lat/longs
ndvi.ti <- projectRaster(ndvi.ti.sp, crs = latlong)
ndvi.amp <- projectRaster(ndvi.amp.sp, crs = latlong)
ndvi.s <- stack(ndvi.ti, ndvi.amp)

# create spatial dataframe of fecal n locations
data.xy <- data.frame("x" = data.tmp$Longitude, "y" = data.tmp$Latitude)
data.sp <- SpatialPointsDataFrame(data.xy, data.tmp, proj4string = latlong)
ext <- raster::extract(ndvi.s, data.sp)
data <- cbind(ext, data.tmp)


