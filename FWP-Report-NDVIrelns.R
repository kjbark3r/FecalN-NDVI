#####################################################~#
### Relating Remotely-sensed NDVI to Elk Forage Data ##
###        For NSERP FWP Report - Mar 2017          ###
################## Kristin Barker ###################~#
#####################################################~#

#GOAL: Quantify relationship between remotely sensed 
       #vegetation index and forage quantity/quality

########## SETUP ##############

## SET WD ##
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\FecalN-NDVI"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\FecalN-NDVI"
ifelse(file.exists(wd_workcomp), setwd(wd_workcomp), setwd(wd_laptop))
rm(wd_workcomp, wd_laptop)


## LOAD LIBRARIES ##
library(AICcmodavg) # model selection
library(raster) # ndvi amp & ti-ndvi
library(ggplot2) # pretty plots
library(gridExtra) # display several plots
library(tidyr) # data wrangling
library(dplyr) # data wrangling


########## DATA ##############

# read in ndvi, biomass, forage quality, and fecal n data
ndvi <- read.csv("../Vegetation/remote-plot.csv")
bm <- read.csv("../Vegetation/biomass-plot-summeronly.csv")
fq <- read.csv("../Vegetation/de-plot-summeronly.csv")
fn <- read.csv("ndvi-fn-data.csv")
ndvi.ti.amp <- read.csv("../Vegetation/DE-model-data.csv") %>%
  select(PlotVisit, ndvi_amp, ndvi_ti)

# fix classes of fecal n data
fn$Date <- as.Date(fn$Date)
fn$Treecov <- as.factor(fn$Treecov)

# combine data
vegtmp <- fq %>%
  select(PlotVisit, DE) %>%
  full_join(bm, by = "PlotVisit") %>%
  select(-c(Latitude, Longitude)) %>%
  left_join(ndvi, by = "PlotVisit") %>%
  left_join(ndvi.ti.amp, by = "PlotVisit") %>%
  dplyr::filter(ForageBiomass < 10000) %>%
  dplyr::filter(DE > 0)
vegtmp <- vegtmp[!is.na(vegtmp$Latitude),]

# add tree cover
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")
lc14 <- raster("../vegetation/writtenrasters/covs2014/landcov_14.tif")
data.xy <- data.frame("x" = vegtmp$Longitude, "y" = vegtmp$Latitude)
data.ll <- SpatialPointsDataFrame(data.xy, vegtmp, proj4string = latlong)
data.sp <- spTransform(data.ll, stateplane)
ext <- raster::extract(lc14, data.sp) # same as lv15, verified
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
vegtmp.a <- cbind(vegtmp, ext)
veg <- vegtmp.a %>%
  rename(Landcov = ext) %>%
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
                          NA))))))))))))) 
veg$Treecov <- as.factor(veg$Treecov)
        

########## ANALYSES ##############


## visualizing univariate relationships ##


## forage biomass - loess smoother ##
p.bm.n <- ggplot(veg, aes(x = NDVI, y = ForageBiomass)) +
  geom_smooth(method = "loess", color = "black")
p.bm.e <- ggplot(veg, aes(x = EVI, y = ForageBiomass)) +
  geom_smooth(method = "loess", color = "black")
p.bm.na <- ggplot(veg, aes(x = ndvi_amp, y = ForageBiomass)) +
  geom_smooth(method = "loess", color = "black")
p.bm.nt <- ggplot(veg, aes(x = ndvi_ti, y = ForageBiomass)) +
  geom_smooth(method = "loess", color = "black")
grid.arrange(p.bm.n, p.bm.e, p.bm.na, p.bm.nt)


## digestible energy ##
p.de.n <- ggplot(veg, aes(x = NDVI, y = DE)) +
  geom_smooth(method = "loess", color = "black")
p.de.e <- ggplot(veg, aes(x = EVI, y = DE)) +
  geom_smooth(method = "loess", color = "black")
p.de.na <- ggplot(veg, aes(x = ndvi_amp, y = DE)) +
  geom_smooth(method = "loess", color = "black")
p.de.nt <- ggplot(veg, aes(x = ndvi_ti, y = DE)) +
  geom_smooth(method = "loess", color = "black")
grid.arrange(p.de.n, p.de.e, p.de.na, p.de.nt)


## fecal nitrogen ##
p.fn.n <- ggplot(fn, aes(x = NDVI, y = PctFN)) +
  geom_smooth(method = "loess", color = "black")
p.fn.e <- ggplot(fn, aes(x = EVI, y = PctFN)) +
  geom_smooth(method = "loess", color = "black")
grid.arrange(p.fn.n, p.fn.e)



## comparing relationships w remotely sensed veg indices ##

## forage biomass ##
Cand.set <- list( )
Cand.set[[1]] <- glm(ForageBiomass ~ NDVI, data = veg)
Cand.set[[2]] <- glm(ForageBiomass ~ EVI, data = veg)
Cand.set[[3]] <- glm(ForageBiomass ~ ndvi_amp, 
                     data = veg)
Cand.set[[4]] <- glm(ForageBiomass ~ ndvi_ti, 
                     data = veg)
names(Cand.set) <- c("Biomass-NDVI", "Biomass-EVI",
                     "Biomass-NDVIamp", "Biomass-NDVIti")
aictable1 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable1, digits = 2, LL = FALSE)

## digestible energy ##
Cand.set <- list( )
Cand.set[[1]] <- glm(DE ~ NDVI, data = veg)
Cand.set[[2]] <- glm(DE ~ EVI, data = veg)
Cand.set[[3]] <- glm(DE ~ ndvi_amp, data = veg)
Cand.set[[4]] <- glm(DE ~ ndvi_ti, data = veg)
names(Cand.set) <- c("DE-NDVI", "DE-EVI", "DE-NDVIamp",
                     "DE-NDVIti")
aictable2 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable2, digits = 2, LL = FALSE)


## fecal nitrogen ##
Cand.set <- list( )
Cand.set[[1]] <- glm(PctFN ~ NDVI, data = fn)
Cand.set[[2]] <- glm(PctFN ~ EVI, data = fn)
names(Cand.set) <- c("FN-NDVI", "FN-EVI")
aictable3 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable3, digits = 2, LL = FALSE)

## store and export results ##
aictab.all <- rbind(aictable1, aictable2, aictable3)
write.csv(aictab.all, file = "aic-results.csv", row.names = F)



## comparing top remotely sensed indices alone w addl covariates ##


## forage biomass ##
Cand.set <- list( )
Cand.set[[1]] <- lm(ForageBiomass ~ NDVI, data = veg)
Cand.set[[2]] <- lm(ForageBiomass ~ NDVI+Treecov, data = veg)
Cand.set[[3]] <- lm(ForageBiomass ~ NDVI*Treecov, data = veg)
names(Cand.set) <- c("B-NDVI", "B-NDVI+Treecov",
                     "B-NDVI*Treecov")
aictable4 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable4, digits = 2, LL = FALSE)


## digestible energy ##
Cand.set <- list( )
Cand.set[[1]] <- lm(DE ~ ndvi_amp, data = veg)
Cand.set[[2]] <- lm(DE ~ ndvi_amp+Treecov, data = veg)
Cand.set[[3]] <- lm(DE ~ ndvi_amp*Treecov, data = veg)
names(Cand.set) <- c("DE-NDVI", "DE-NDVI+Treecov",
                     "DE-NDVI*Treecov")
aictable5 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable5, digits = 2, LL = FALSE)


## fecal nitrogen ##
Cand.set <- list( )
Cand.set[[1]] <- lm(PctFN ~ NDVI, data = fn)
Cand.set[[2]] <- lm(PctFN ~ NDVI+Treecov, data = fn)
Cand.set[[3]] <- lm(PctFN ~ NDVI*Treecov, data = fn)
names(Cand.set) <- c("FN-NDVI", "FN-NDVI+Treecov",
                     "FN-NDVI*Treecov")
aictable6 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable6, digits = 2, LL = FALSE)


## more fecal nitrogen ##
Cand.set <- list( )
Cand.set[[1]] <- glm(PctFN ~ NDVI, data = fn)
Cand.set[[2]] <- glm(PctFN ~ MigStatus, data = fn)
Cand.set[[3]] <- glm(PctFN ~ Elevm+DOY, data = fn)
names(Cand.set) <- c("FN-NDVI", "FN-Elevm+Date",
                     "FN-Elevm+DOY")
aictable7 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable7, digits = 2, LL = FALSE)

## just 2014
fn2014 <- filter(fn2014, 


#~##############~##~##############~##~##############~#
#### CUT CODE ####

## univariate relationships w ndvi ##

mbm <- lm(ForageBiomass ~ NDVI, data = veg)
summary(mbm)
par(mfrow=c(2,2))
plot(mbm)

mfq <- lm(DE ~ NDVI, data = veg)
summary(mfq)
plot(mfq)

mfn <- lm(PctFN ~ NDVI, data = fn)
summary(mfn)
plot(mfn)



### glm-smoothed plots ###

## forage biomass - glm smoother ##
p.bm.n <- ggplot(veg, aes(x = NDVI, y = ForageBiomass)) +
  geom_smooth(method = "glm", color = "black")
p.bm.e <- ggplot(veg, aes(x = EVI, y = ForageBiomass)) +
  geom_smooth(method = "glm", color = "black")
p.bm.na <- ggplot(veg, aes(x = ndvi_amp, y = ForageBiomass)) +
  geom_smooth(method = "glm", color = "black")
p.bm.nt <- ggplot(veg, aes(x = ndvi_ti, y = ForageBiomass)) +
  geom_smooth(method = "glm", color = "black")
grid.arrange(p.bm.n, p.bm.e, p.bm.na, p.bm.nt)

## digestible energy ##
p.de.n <- ggplot(veg, aes(x = NDVI, y = DE)) +
  geom_smooth(method = "glm", color = "black")
p.de.e <- ggplot(veg, aes(x = EVI, y = DE)) +
  geom_smooth(method = "glm", color = "black")
p.de.na <- ggplot(veg, aes(x = ndvi_amp, y = DE)) +
  geom_smooth(method = "glm", color = "black")
p.de.nt <- ggplot(veg, aes(x = ndvi_ti, y = DE)) +
  geom_smooth(method = "glm", color = "black")
grid.arrange(p.de.n, p.de.e, p.de.na, p.de.nt)

## fecal nitrogen ##
p.fn.n <- ggplot(fn, aes(x = NDVI, y = PctFN)) +
  geom_smooth(method = "glm", color = "black")
p.fn.e <- ggplot(fn, aes(x = EVI, y = PctFN)) +
  geom_smooth(method = "glm", color = "black")
grid.arrange(p.fn.n, p.fn.e)