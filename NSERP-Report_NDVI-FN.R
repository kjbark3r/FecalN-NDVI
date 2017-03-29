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
fecaln.raw <- read.csv("fecalndata20142015.csv")
colxndata <- read.csv("pelletcolxnsites.csv")


########## DATA PREP ##############

## REMOTE DATA - FROM GOOGLE EARTH ENGINE/BRADY ALLRED ##
# pull pellet-related NDVI data only; fix plot names; tidy
remote.data <- dplyr::filter(remote.raw, Type == "Pellet 2014" |
                        Type == "Pellet 2015")
remote.data <- select(remote.data, starts_with("ndvi."), PlotID)
remote.data$PlotID <- readr::parse_number(remote.data$PlotID)
remote.data <- rename(remote.data, SampleID = PlotID) 

## NDVI DATES ##
ndvi.data <- gather(remote.data, key = SDate, value = "NDVI", -SampleID)
ndvi.data$SDate <- substr(ndvi.data$SDate, 6, 13)
ndvi.data$SDate <- as.Date(as.character(ndvi.data$SDate), format='%Y%m%d')

## FECAL NITROGEN DATA ##
# add lat-long to each sample
fn.data <- fecaln.raw %>%
  rename(SampleID = Sample.ID,
         Date = SampleDate) #~#%>%
#~#fn.data <- inner_join(fecaln.raw, colxndata, by = "SampleID") 
# remove extraneous columns; format date
#~#fn.data <- select(fecaln.raw, -c(SampleType, SampleDate, Collector))
fn.data$Date <- as.Date(as.character(fn.data$Date), format = "%m/%d/20%y")
# pathetically create column relating collxn date to NDVI/EVI date
fn.data$SDate <- c("2014-06-10","2014-06-10","2014-06-10","2014-06-10","2014-06-10","2014-06-10",
                   "2014-06-26","2014-06-26","2014-06-26","2014-06-26","2014-06-26",
                   "2014-07-12","2014-07-12","2014-07-12","2014-07-12",
                   "2014-07-28","2014-07-28","2014-07-28","2014-07-28",
                   "2014-08-13","2014-08-13","2014-08-13","2014-08-13",
                   "2014-08-13","2014-08-13","2014-08-13","2014-08-29",
                   "2014-09-14","2014-09-14","2014-09-14","2014-09-14",
                   "2014-09-30","2014-09-30","2014-09-30","2014-09-30","2014-09-30","2014-09-30","2014-09-30",
                   "2015-06-10", "2015-06-10","2015-06-10",
                   "2015-06-26","2015-06-26","2015-06-26","2015-06-26","2015-06-26",
                   "2015-07-12","2015-07-12","2015-07-12","2015-07-12",
                   "2015-07-28","2015-07-28","2015-07-28","2015-07-28","2015-07-28",
                   "2015-08-13","2015-08-13","2015-08-13","2015-08-13",
                   "2015-08-29","2015-08-29","2015-08-29","2015-08-29",
                   "2015-09-14","2015-09-14","2015-09-14","2015-09-14",
                   "2015-09-30","2015-09-30","2015-09-30","2015-09-30","2015-09-30","2015-09-30","2015-09-30","2015-09-30")
fn.data$SDate <- as.Date(as.character(fn.data$SDate), format='%Y-%m-%d')


# combine point data
data.tmp <- left_join(fn.data, ndvi.data, by=c("SampleID", "SDate"))   
# add day of year
data.tmp$DOY <- strftime(data.tmp$Date, format = "%j")
data.tmp$DOY <- as.numeric(data.tmp$DOY)
# add lat/longs
colxndata <- rename(colxndata, SampleID = Sample.ID)
data.tmp <- left_join(data.tmp, colxndata, by = "SampleID")


#### ADD ELEV AND LANDCOVER ####

# set projections
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")

# read in and set projections
elev <- raster("../Vegetation/writtenrasters/covs2014/elev.tif")
elev <- projectRaster(elev, crs = latlong)
lc14 <- raster("../Vegetation/writtenrasters/covs2014/landcov_14.tif")
lc14 <- projectRaster(lc14, crs = latlong)
lc15 <- raster("../Vegetation/writtenrasters/covs2015/landcov_15.tif")
lc15 <- projectRaster(lc15, crs = latlong)
s <- stack(elev, lc14, lc15)

# create spatial dataframe of fecal n locations
data.xy <- data.frame("x" = data.tmp$Longitude, "y" = data.tmp$Latitude)
data.sp <- SpatialPointsDataFrame(data.xy, data.tmp, proj4string = latlong)
ext <- raster::extract(s, data.sp)
data <- cbind(ext, data.tmp)



#~#~THIS WENT after fn.data$Date line
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




## CREATE SEPARATE RES/MIG DFs ##
res <- filter(data, MigStatus=="Res")
mig <- filter(data, MigStatus=="Mig")





## check for correlations
dat.cor <- data.tmp %>%
  dplyr::select(MigStatus, PctFN, Elevm, Landcov, Treecov, NDVI, DOY)
source("../zMisc/pairs-panels.R")
pairs.panels(dat.cor)

#### KJB NEXT STEPS ####


#2. rerun covariate and model selection
#3. make your methods not suck... actually say what you did
#4. if time, before results/disc: rerun with 2015 incl
  #reqs: 
    #new colxnsites_elevs_habs where pull
      #elevs and habs from arcmap, ew (and keep lat/longs)
    #new pathetic SDate creation code, no biggie


#### RES/MIG FN DIFFS INCL 2015 DATA ####

fn.all <- read.csv("fecalndata20142015.csv")
hist(fn.all$PctFN)
hist(fn.all[fn.all$MigStatus == "Res",]$PctFN)
hist(fn.all[fn.all$MigStatus == "Mig",]$PctFN)
t.test(fn.all[fn.all$MigStatus == "Res",]$PctFN,
       fn.all[fn.all$MigStatus == "Mig",]$PctFN,
       alternative = "two.sided", 
       paired = FALSE, 
       var.equal = FALSE)

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

# set habitat types
#~#fn.data <- rename(fn.data, HabClass = RASTERVALU) 
#~#fn.data <- mutate(fn.data, Landcov = ifelse(HabClass == 0, "Mask", 
#~#                                     ifelse(HabClass == 1, "Badlands",
#~#                                     ifelse(HabClass == 2, "Riparian",
#~#                                     ifelse(HabClass == 3, "Forest",
#~#                                     ifelse(HabClass == 4, "Shrub",
#~#                                     ifelse(HabClass == 5, "Sagebrush",
#~#                                     ifelse(HabClass == 6, "Grassland",
#~#                                     ifelse(HabClass == 7, "Agricultural",
#~#                                            NA)))))))))
