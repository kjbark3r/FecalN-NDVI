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



########## DATA PREP (run once) ##############

## READ IN RAW DATA ##
remote.raw <- read.csv("remotedata.csv")
fecaln.raw <- read.csv("fecalndata20142015.csv")
colxndata <- read.csv("pelletcolxnsites.csv")

## REMOTE DATA - FROM GOOGLE EARTH ENGINE/BRADY ALLRED ##
# pull pellet-related NDVI data only; fix plot names; tidy
remote.data <- dplyr::filter(remote.raw, Type == "Pellet 2014" |
                        Type == "Pellet 2015")
remote.data <- select(remote.data, starts_with("ndvi."), PlotID)
remote.data$PlotID <- readr::parse_number(remote.data$PlotID)
remote.data <- rename(remote.data, SampleID = PlotID) 
ndvi.data <- gather(remote.data, key = SDate, value = "NDVI", -SampleID)
ndvi.data$SDate <- substr(ndvi.data$SDate, 6, 13)
ndvi.data$SDate <- as.Date(as.character(ndvi.data$SDate), format='%Y%m%d')


# pull pellet-related EVI data only; fix plot names; tidy
remote.data2 <- dplyr::filter(remote.raw, Type == "Pellet 2014" |
                        Type == "Pellet 2015")
remote.data2 <- select(remote.data2, starts_with("evi."), PlotID)
remote.data2$PlotID <- readr::parse_number(remote.data2$PlotID)
remote.data2 <- rename(remote.data2, SampleID = PlotID) 
evi.data <- gather(remote.data2, key = SDate, value = "EVI", -SampleID)
evi.data$SDate <- substr(evi.data$SDate, 5, 12)
evi.data$SDate <- as.Date(as.character(evi.data$SDate), format='%Y%m%d')

## and combine ndvi and evi
# (calling it ndvi data so i don't have to record a bunch of stuff)
ndvi.data <- full_join(ndvi.data, evi.data, by = c("SampleID", "SDate"))

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


### ADD ELEV AND LANDCOVER ###

# set projections
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")

# read in rasters
elev <- raster("../Vegetation/writtenrasters/covs2014/elev.tif")
lc14 <- raster("../Vegetation/writtenrasters/covs2014/landcov_14.tif")
s <- stack(elev, lc14)

# create spatial dataframe of fecal n locations
# in same projection as rasters 
# (can't reproject rasters bc it changes their values)
data.xy <- data.frame("x" = data.tmp$Longitude, "y" = data.tmp$Latitude)
data.ll <- SpatialPointsDataFrame(data.xy, data.tmp, proj4string = latlong)
data.sp <- spTransform(data.ll, stateplane)
ext <- raster::extract(s, data.sp)
dat <- cbind(ext, data.tmp)

# add landcover class names and treecover (open/closed canopy)
clsref <- data.frame(Landcov = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     ClassName = c("Mesic Forest (Burn >15)", #1
                                    "Dry Forest (Burn >15)", #2 
                                    "Grass/Shrub/Open Woodland", #3
                                    "Dry Ag", #4
                                    "Valley Bottom Riparian", #5
                                    "Montane Riparian", #6
                                    "Irrigated Ag", #7
                                    "Dry Forest Burn 0-5", #8
                                    "Dry Forest Burn 6-15", #9
                                    "Mesic Forest Burn 0-5",#10
                                    "Mesic Forest Burn 6-15", #11
                                    "Rx Dry Forest Burn 0-5")) #12
data <- dat %>%
  rename(Landcov = landcov_14,
         Elevm = elev) %>%
  left_join(clsref, by = "Landcov") %>%
  mutate(Treecov = ifelse(Landcov == 1, "1", 
                   ifelse(Landcov == 2, "1",
                   ifelse(Landcov == 3, "0",
                   ifelse(Landcov == 4, "0",
                   ifelse(Landcov == 5, "1",
                   ifelse(Landcov == 6, "1",
                   ifelse(Landcov == 7, "0",
                   ifelse(Landcov == 8, "0",
                   ifelse(Landcov == 9, "0",
                   ifelse(Landcov == 10, "0",
                   ifelse(Landcov == 11, "0",
                   ifelse(Landcov == 12, "0",
                          NA))))))))))))) %>%
    dplyr::select(SampleID, MigStatus, Date, PctFN, 
                  NDVI, EVI, Elevm, Treecov, Landcov, ClassName, 
                  DOY, SDate, Latitude, Longitude)
write.csv(data, file = "ndvi-fn-data.csv", row.names=F)



########## COVARIATE SELECTION ##############

# if code not run in full from above
data <- read.csv("ndvi-fn-data.csv") 

# set factors and factor levels
data$Treecov <- as.factor(data$Treecov)
data$Landcov <- as.factor(data$Landcov)

ref.lev <- data %>%
  dplyr::select(c(Landcov, class_name, DE)) %>%
  group_by(class_name, landcov) %>%
  summarise(MedianDE = median(DE)) %>%
  arrange(MedianDE) %>% ## Order from least to most forb GDM 
  ungroup() 
ref.lev # so reference level is landcover type with lowest DE
dat$landcov <- factor(dat$landcov, levels = as.vector(ref.lev$landcov))

## check for correlations
dat.cor <- data %>%
  dplyr::select(MigStatus, PctFN, Elevm, Landcov, Treecov, NDVI, DOY)
source("../zMisc/pairs-panels.R")
pairs.panels(dat.cor)




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


#~#######################################~#
##### CUT CODE/MISC UNIMPORTANT STUFF #####

# just realized i never made a csv of landcover classes;
# could be handy
write.csv(clsref, file = "landcov-classes.csv", row.names=F)

# checking for diffs in landcover bt years
tmp <- data %>%
  mutate(lcdiff = landcov_14 - landcov_15)
unique(tmp$lcdiff) 
# no diffs; using same landcover values both years

## CREATE SEPARATE RES/MIG DFs ##
res <- filter(data, MigStatus=="Res")
mig <- filter(data, MigStatus=="Mig")

# old habitat types (before Kelly's sweet landcover layer)
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
