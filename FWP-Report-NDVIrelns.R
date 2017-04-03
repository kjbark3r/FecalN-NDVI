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


#### model selection - which remotely-sensed index? ####

## forage biomass ##
Cand.set <- list( )
Cand.set[[1]] <- glm(ForageBiomass ~ NDVI, data = veg)
Cand.set[[2]] <- glm(ForageBiomass ~ NDVI + I(NDVI^2), data = veg)
Cand.set[[3]] <- glm(ForageBiomass ~ NDVI + I(NDVI^2) + I(NDVI^3), data = veg)
Cand.set[[4]] <- glm(ForageBiomass ~ EVI, data = veg)
Cand.set[[5]] <- glm(ForageBiomass ~ EVI + I(EVI^2), data = veg)
Cand.set[[6]] <- glm(ForageBiomass ~ ndvi_amp + I(ndvi_amp^2), data = veg)
Cand.set[[7]] <- glm(ForageBiomass ~ ndvi_ti + I(ndvi_ti^2), data = veg)
names(Cand.set) <- c("Biomass-NDVI", "Biomass-NDVI2", 
                     "Biomass-NDVI3", "Biomass-EVI",
                     "Biomass-EVI2", "Biomass-NDVIamp2", 
                     "Biomass-NDVIti2")
aictable1 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable1, digits = 2, LL = TRUE)

## digestible energy ##
Cand.set <- list( )
Cand.set[[1]] <- glm(DE ~ NDVI, data = veg)
Cand.set[[2]] <- glm(DE ~ NDVI + I(NDVI^2), data = veg)
Cand.set[[3]] <- glm(DE ~ EVI, data = veg) #unexpected
Cand.set[[4]] <- glm(DE ~ EVI +I(EVI^2), data = veg)
Cand.set[[5]] <- glm(DE ~ ndvi_amp, data = veg)
Cand.set[[6]] <- glm(DE ~ ndvi_amp + I(ndvi_amp^2) + I(ndvi_amp^3), data = veg)
Cand.set[[7]] <- glm(DE ~ ndvi_ti, data = veg)
Cand.set[[8]] <- glm(DE ~ ndvi_ti + I(ndvi_ti^2), data = veg)
Cand.set[[9]] <- glm(DE ~ ndvi_ti + I(ndvi_ti^2) + I(ndvi_ti^3), data = veg)
names(Cand.set) <- c("DE-NDVI", "DE-NDVI2", "DE-EVIno",
                     "DE-EVI2", "DE-NDVIamp", "DE-NDVI-amp3",
                     "DE-NDVIti", "DE-NDVIti2", "DE-NDVIti3")
aictable2 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable2, digits = 2, LL = TRUE)


## fecal nitrogen ##
Cand.set <- list( )
Cand.set[[1]] <- glm(PctFN ~ NDVI, data = fn)
Cand.set[[2]] <- glm(PctFN ~ NDVI + I(NDVI^2) + I(NDVI^3), data = fn)
Cand.set[[3]] <- glm(PctFN ~ EVI, data = fn)
Cand.set[[4]] <- glm(PctFN ~ EVI + I(EVI^2) + I(EVI^3), data = fn)
names(Cand.set) <- c("FN-NDVI", "FN-NDVI3",
                     "FN-EVI", "FN-EVI3")
aictable3 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable3, digits = 2, LL = TRUE)

## store and export results ##
aictab.all <- rbind(aictable1, aictable2, aictable3)
write.csv(aictab.all, file = "aic-results.csv", row.names = F)


#### model selection - tree cover? ####


## forage biomass ##
Cand.set <- list( )
Cand.set[[1]] <- glm(ForageBiomass ~ NDVI + I(NDVI^2) + I(NDVI^3), data = veg)
Cand.set[[2]] <- glm(ForageBiomass ~ NDVI*Treecov + I(NDVI^2)*Treecov + I(NDVI^3)*Treecov, data = veg)
names(Cand.set) <- c("B-NDVI3", "B-NDVI3*Treecov")
aictable4 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable4, digits = 2, LL = TRUE)


## digestible energy ##
Cand.set <- list( )
Cand.set[[1]] <- glm(DE ~ ndvi_amp + I(ndvi_amp^2) + I(ndvi_amp^3), data = veg)
Cand.set[[2]] <- glm(DE ~ ndvi_amp, data = veg)
Cand.set[[3]] <- glm(DE ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov + I(ndvi_amp^3)*Treecov, data = veg)
Cand.set[[4]] <- glm(DE ~ ndvi_amp*Treecov, data = veg)
names(Cand.set) <- c("DE-NDVIamp3", "DE-NDVIamp",
                     "DE-NDVIamp3*Treecov", "DE-NDVIamp*Treecov")
aictable5 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable5, digits = 2, LL = TRUE)


## fecal nitrogen ##
Cand.set <- list( )
Cand.set[[1]] <- lm(PctFN ~ NDVI, data = fn)
Cand.set[[2]] <- lm(PctFN ~ NDVI*Treecov, data = fn)
names(Cand.set) <- c("FN-NDVI", "FN-NDVI*Treecov")
aictable6 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable6, digits = 2, LL = TRUE)

## store and export results ##
aictab.all2 <- rbind(aictable4, aictable5, aictable6)
write.csv(aictab.all2, file = "aic-results-treecov.csv", row.names = F)


#### top models ####

par(mfrow=c(2,2))

top.bm <- lm(ForageBiomass ~ NDVI, data = veg)
summary(top.bm)
plot(top.bm)

top.de2 <- lm(DE ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov + I(ndvi_amp^3)*Treecov, data = veg)
summary(top.de2)
plot(top.de2)

top.de1 <- lm(DE ~ ndvi_amp*Treecov, data = veg)
summary(top.de1)
plot(top.de1)

top.fn1 <- lm(PctFN ~ NDVI, data = fn)
summary(top.fn1)
plot(top.fn1)

top.fn2 <- lm(PctFN ~ EVI, data = fn)
summary(top.fn2)
plot(top.fn2)

lm1 <- c(top.bm$coefficients[1], 
         summary(top.bm)$adj.r.squared, 
         summary(top.bm)$sigma)
lm2 <- c(top.de1$coefficients[1], 
         summary(top.de1)$adj.r.squared, 
         summary(top.de1)$sigma)
lm3 <- c(top.de2$coefficients[1], 
         summary(top.de2)$adj.r.squared, 
         summary(top.de2)$sigma)
lm4 <- c(top.fn$coefficients[1], 
         summary(top.fn)$adj.r.squared, 
         summary(top.fn)$sigma)

tprep <- rbind(lm1, lm2, lm3, lm4)
tab <- as.data.frame(tprep, row.names = 
                       c("Biomass-NDVI", 
                         "DE-NDVIamp*Treecov", 
                         "DE-NDVIamp3*Treecov",
                         "FN-NDVI"))
tab <- rename(tab, AdjRsquared = V2)
tab <- rename(tab, StdError = V3)
View(tab)
write.csv(tab, file = "regressions.csv")


#### biomass without outliers ####
veg2 <- filter(veg, ForageBiomass < 1000)
test <- lm(ForageBiomass ~ NDVI, data = veg2)
plot(test)

#### VISUALS ####

#### visualizing univariate relationships ####


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


#### interaction bt tree cover and NDVI for DE ####
ggplot(veg, aes(x=ndvi_amp, y=DE, fill=Treecov)) + 
  stat_smooth(method="glm", 
              method.args = list(family="gaussian"), 
              level=0.95) 








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


## comparing relationships w remotely sensed veg indices ##
### bic ###

# forage biomass #
mods <- list()
mods[[1]] <- glm(ForageBiomass ~ NDVI, data = veg)
mods[[2]] <- glm(ForageBiomass ~ NDVI + I(NDVI^2), data = veg)
mods[[3]] <- glm(ForageBiomass ~ NDVI + I(NDVI^2) + I(NDVI^3), data = veg)
mods[[4]] <- glm(ForageBiomass ~ EVI, data = veg)
mods[[5]] <- glm(ForageBiomass ~ EVI + I(EVI^2), data = veg)
mods[[6]] <- glm(ForageBiomass ~ ndvi_amp + I(ndvi_amp^2), data = veg)
mods[[7]] <- glm(ForageBiomass ~ ndvi_ti + I(ndvi_ti^2), data = veg)
bictab1 <- data.frame(mod = c("Biomass-NDVI", "Biomass-NDVI2", "Biomass-NDVI3", 
            "Biomass-EVI","Biomass-EVI2", 
            "Biomass-NDVIamp2", "Biomass-NDVIti2"),
            res = c(BIC(mods[[1]]), BIC(mods[[2]]), BIC(mods[[3]]), 
                    BIC(mods[[4]]), BIC(mods[[5]]), BIC(mods[[6]]),
                    BIC(mods[[7]])))
arrange(bictab1, res)


# digestible energy #
mods <- list()
mods[[1]] <- glm(DE ~ NDVI, data = veg)
mods[[2]] <- glm(DE ~ NDVI + I(NDVI^2), data = veg)
mods[[3]] <- glm(DE ~ EVI, data = veg) #unexpected
mods[[4]] <- glm(DE ~ EVI +I(EVI^2), data = veg)
mods[[5]] <- glm(DE ~ ndvi_amp, data = veg)
mods[[6]] <- glm(DE ~ ndvi_amp + I(ndvi_amp^2) + I(ndvi_amp^3), data = veg)
mods[[7]] <- glm(DE ~ ndvi_ti, data = veg)
mods[[8]] <- glm(DE ~ ndvi_ti + I(ndvi_ti^2), data = veg)
mods[[9]] <- glm(DE ~ ndvi_ti + I(ndvi_ti^2) + I(ndvi_ti^3), data = veg)
bictab2 <- data.frame(mod = c("DE-NDVI", "DE-NDVI2", "DE-EVIno",
                     "DE-EVI2", "DE-NDVIamp", "DE-NDVI-amp3",
                     "DE-NDVIti", "DE-NDVIti2", "DE-NDVIti3"),
            res = c(BIC(mods[[1]]), BIC(mods[[2]]), BIC(mods[[3]]), 
                    BIC(mods[[4]]), BIC(mods[[5]]), BIC(mods[[6]]),
                    BIC(mods[[7]]), BIC(mods[[8]]), BIC(mods[[9]])))
arrange(bictab2, res)


# fecal nitrogen #
mods <- list()
mods[[1]] <- glm(PctFN ~ NDVI, data = fn)
mods[[2]] <- glm(PctFN ~ NDVI + I(NDVI^2) + I(NDVI^3), data = fn)
mods[[3]] <- glm(PctFN ~ EVI, data = fn)
mods[[4]] <- glm(PctFN ~ EVI + I(EVI^2) + I(EVI^3), data = fn)
bictab3 <- data.frame(mod = c("FN-NDVI", "FN-NDVI3",
                     "FN-EVI", "FN-EVI3"),
            res = c(BIC(mods[[1]]), BIC(mods[[2]]), BIC(mods[[3]]), 
                    BIC(mods[[4]])))
arrange(bictab3, res)

## store and export results ##
bictab.all <- rbind(bictab1, bictab2, bictab3)
write.csv(bictab.all, file = "bic-results.csv", row.names = F)


## random effect of landcover type ##
library(lme4)
test <- glmer(ForageBiomass ~ NDVI + (1|ClassName), data = veg)
summary(test)


## kinda-supported models ##
mid.bm <- lm(ForageBiomass ~ NDVI + I(NDVI^2) + I(NDVI^3), data = veg)
summary(mid.bm)
plot(mid.bm)