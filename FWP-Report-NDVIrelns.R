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
  select(PlotVisit, landcov, ndvi_amp, ndvi_ti) %>%
  rename(Landcov = landcov)

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
veg <- vegtmp %>%
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
        
#### biomass without outliers####
# (anomalous NDVI and biomass numbers) #
veg2 <- filter(veg, ForageBiomass < 1000,
               NDVI > 2500)


########## ANALYSES ##############


#### model selection ####

## forage biomass ##
Cand.set <- list( )
Cand.set[[1]] <- lm(ForageBiomass ~ NDVI, data = veg2)
Cand.set[[2]] <- lm(ForageBiomass ~ NDVI + I(NDVI^2), data = veg2)
Cand.set[[3]] <- lm(ForageBiomass ~ NDVI + I(NDVI^2) + I(NDVI^3), data = veg2)
Cand.set[[4]] <- lm(ForageBiomass ~ EVI, data = veg2)
Cand.set[[5]] <- lm(ForageBiomass ~ EVI + I(EVI^2), data = veg2)
Cand.set[[6]] <- lm(ForageBiomass ~ EVI + I(EVI^2) + I(EVI^3), data = veg2)
Cand.set[[7]] <- lm(ForageBiomass ~ ndvi_amp, data = veg2)
Cand.set[[8]] <- lm(ForageBiomass ~ ndvi_amp + I(ndvi_amp^2), data = veg2)
Cand.set[[9]] <- lm(ForageBiomass ~ ndvi_amp + I(ndvi_amp^2) + I(ndvi_amp^3), data = veg2)
Cand.set[[10]] <- lm(ForageBiomass ~ ndvi_ti, data = veg2)
Cand.set[[11]] <- lm(ForageBiomass ~ ndvi_ti + I(ndvi_ti^2), data = veg2)
Cand.set[[12]] <- lm(ForageBiomass ~ ndvi_ti + I(ndvi_ti^2) + I(ndvi_ti^3), data = veg2)
Cand.set[[13]] <- lm(ForageBiomass ~ NDVI*Treecov, data = veg2)
Cand.set[[14]] <- lm(ForageBiomass ~ NDVI*Treecov + I(NDVI^2)*Treecov, data = veg2)
Cand.set[[15]] <- lm(ForageBiomass ~ NDVI*Treecov + I(NDVI^2)*Treecov + I(NDVI^3)*Treecov, data = veg2)
Cand.set[[16]] <- lm(ForageBiomass ~ EVI*Treecov, data = veg2)
Cand.set[[17]] <- lm(ForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov, data = veg2)
Cand.set[[18]] <- lm(ForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov + I(EVI^3)*Treecov, data = veg2)
Cand.set[[19]] <- lm(ForageBiomass ~ ndvi_amp*Treecov, data = veg2)
Cand.set[[20]] <- lm(ForageBiomass ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov, data = veg2)
Cand.set[[21]] <- lm(ForageBiomass ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov + I(ndvi_amp^3)*Treecov, data = veg2)
Cand.set[[22]] <- lm(ForageBiomass ~ ndvi_ti*Treecov, data = veg2)
Cand.set[[23]] <- lm(ForageBiomass ~ ndvi_ti*Treecov + I(ndvi_ti^2)*Treecov, data = veg2)
Cand.set[[24]] <- lm(ForageBiomass ~ ndvi_ti*Treecov + I(ndvi_ti^2)*Treecov + I(ndvi_ti^3)*Treecov, data = veg2)
names(Cand.set) <- c("Biomass-NDVI", "Biomass-NDVI2", "Biomass-NDVI3", 
                     "Biomass-EVI","Biomass-EVI2", "Biomass-EVI3",
                     "Biomass-NDVIamp", "Biomass-NDVIamp2", "Biomass-NDVIamp3",
                     "Biomass-NDVIti", "Biomass-NDVI2ti", "Biomass-NDVI3ti",
                     "Biomass-NDVI*TC", "Biomass-NDVI2*TC", "Biomass-NDVI3*TC", 
                     "Biomass-EVI*TC","Biomass-EVI2*TC", "Biomass-EVI3*TC",
                     "Biomass-NDVIamp*TC", "Biomass-NDVIamp2*TC", "Biomass-NDVIamp3*TC",
                     "Biomass-NDVIti*TC", "Biomass-NDVI2ti*TC", "Biomass-NDVI3ti*TC")
aictable1 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable1, digits = 2, LL = TRUE)

## herbaceous forage biomass ##
Cand.set <- list( )
Cand.set[[1]] <- lm(HerbaceousForageBiomass ~ NDVI, data = veg2)
Cand.set[[2]] <- lm(HerbaceousForageBiomass ~ NDVI + I(NDVI^2), data = veg2)
Cand.set[[3]] <- lm(HerbaceousForageBiomass ~ NDVI + I(NDVI^2) + I(NDVI^3), data = veg2)
Cand.set[[4]] <- lm(HerbaceousForageBiomass ~ EVI, data = veg2)
Cand.set[[5]] <- lm(HerbaceousForageBiomass ~ EVI + I(EVI^2), data = veg2)
Cand.set[[6]] <- lm(HerbaceousForageBiomass ~ EVI + I(EVI^2) + I(EVI^3), data = veg2)
Cand.set[[7]] <- lm(HerbaceousForageBiomass ~ ndvi_amp, data = veg2)
Cand.set[[8]] <- lm(HerbaceousForageBiomass ~ ndvi_amp + I(ndvi_amp^2), data = veg2)
Cand.set[[9]] <- lm(HerbaceousForageBiomass ~ ndvi_amp + I(ndvi_amp^2) + I(ndvi_amp^3), data = veg2)
Cand.set[[10]] <- lm(HerbaceousForageBiomass ~ ndvi_ti, data = veg2)
Cand.set[[11]] <- lm(HerbaceousForageBiomass ~ ndvi_ti + I(ndvi_ti^2), data = veg2)
Cand.set[[12]] <- lm(HerbaceousForageBiomass ~ ndvi_ti + I(ndvi_ti^2) + I(ndvi_ti^3), data = veg2)
Cand.set[[13]] <- lm(HerbaceousForageBiomass ~ NDVI*Treecov, data = veg2)
Cand.set[[14]] <- lm(HerbaceousForageBiomass ~ NDVI*Treecov + I(NDVI^2)*Treecov, data = veg2)
Cand.set[[15]] <- lm(HerbaceousForageBiomass ~ NDVI*Treecov + I(NDVI^2)*Treecov + I(NDVI^3)*Treecov, data = veg2)
Cand.set[[16]] <- lm(HerbaceousForageBiomass ~ EVI*Treecov, data = veg2)
Cand.set[[17]] <- lm(HerbaceousForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov, data = veg2)
Cand.set[[18]] <- lm(HerbaceousForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov + I(EVI^3)*Treecov, data = veg2)
Cand.set[[19]] <- lm(HerbaceousForageBiomass ~ ndvi_amp*Treecov, data = veg2)
Cand.set[[20]] <- lm(HerbaceousForageBiomass ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov, data = veg2)
Cand.set[[21]] <- lm(HerbaceousForageBiomass ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov + I(ndvi_amp^3)*Treecov, data = veg2)
Cand.set[[22]] <- lm(HerbaceousForageBiomass ~ ndvi_ti*Treecov, data = veg2)
Cand.set[[23]] <- lm(HerbaceousForageBiomass ~ ndvi_ti*Treecov + I(ndvi_ti^2)*Treecov, data = veg2)
Cand.set[[24]] <- lm(HerbaceousForageBiomass ~ ndvi_ti*Treecov + I(ndvi_ti^2)*Treecov + I(ndvi_ti^3)*Treecov, data = veg2)
names(Cand.set) <- c("HerbBiomass-NDVI", "HerbBiomass-NDVI2", "HerbBiomass-NDVI3", 
                     "HerbBiomass-EVI","HerbBiomass-EVI2", "HerbBiomass-EVI3",
                     "HerbBiomass-NDVIamp", "HerbBiomass-NDVIamp2", "HerbBiomass-NDVIamp3",
                     "HerbBiomass-NDVIti", "HerbBiomass-NDVI2ti", "HerbBiomass-NDVI3ti",
                     "HerbBiomass-NDVI*TC", "HerbBiomass-NDVI2*TC", "HerbBiomass-NDVI3*TC", 
                     "HerbBiomass-EVI*TC","HerbBiomass-EVI2*TC", "HerbBiomass-EVI3*TC",
                     "HerbBiomass-NDVIamp*TC", "HerbBiomass-NDVIamp2*TC", "HerbBiomass-NDVIamp3*TC",
                     "HerbBiomass-NDVIti*TC", "HerbBiomass-NDVI2ti*TC", "HerbBiomass-NDVI3ti*TC")
aictable2 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable2, digits = 2, LL = TRUE)


## digestible energy ##
Cand.set <- list( )
Cand.set[[1]] <- lm(DE ~ NDVI, data = veg2)
Cand.set[[2]] <- lm(DE ~ NDVI + I(NDVI^2), data = veg2)
Cand.set[[3]] <- lm(DE ~ NDVI + I(NDVI^2) + I(NDVI^3), data = veg2)
Cand.set[[4]] <- lm(DE ~ EVI, data = veg2)
Cand.set[[5]] <- lm(DE ~ EVI + I(EVI^2), data = veg2)
Cand.set[[6]] <- lm(DE ~ EVI + I(EVI^2) + I(EVI^3), data = veg2)
Cand.set[[7]] <- lm(DE ~ ndvi_amp, data = veg2)
Cand.set[[8]] <- lm(DE ~ ndvi_amp + I(ndvi_amp^2), data = veg2)
Cand.set[[9]] <- lm(DE ~ ndvi_amp + I(ndvi_amp^2) + I(ndvi_amp^3), data = veg2)
Cand.set[[10]] <- lm(DE ~ ndvi_ti, data = veg2)
Cand.set[[11]] <- lm(DE ~ ndvi_ti + I(ndvi_ti^2), data = veg2)
Cand.set[[12]] <- lm(DE ~ ndvi_ti + I(ndvi_ti^2) + I(ndvi_ti^3), data = veg2)
Cand.set[[13]] <- lm(DE ~ NDVI*Treecov, data = veg2)
Cand.set[[14]] <- lm(DE ~ NDVI*Treecov + I(NDVI^2)*Treecov, data = veg2)
Cand.set[[15]] <- lm(DE ~ NDVI*Treecov + I(NDVI^2)*Treecov + I(NDVI^3)*Treecov, data = veg2)
Cand.set[[16]] <- lm(DE ~ EVI*Treecov, data = veg2)
Cand.set[[17]] <- lm(DE ~ EVI*Treecov + I(EVI^2)*Treecov, data = veg2)
Cand.set[[18]] <- lm(DE ~ EVI*Treecov + I(EVI^2)*Treecov + I(EVI^3)*Treecov, data = veg2)
Cand.set[[19]] <- lm(DE ~ ndvi_amp*Treecov, data = veg2)
Cand.set[[20]] <- lm(DE ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov, data = veg2)
Cand.set[[21]] <- lm(DE ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov + I(ndvi_amp^3)*Treecov, data = veg2)
Cand.set[[22]] <- lm(DE ~ ndvi_ti*Treecov, data = veg2)
Cand.set[[23]] <- lm(DE ~ ndvi_ti*Treecov + I(ndvi_ti^2)*Treecov, data = veg2)
Cand.set[[24]] <- lm(DE ~ ndvi_ti*Treecov + I(ndvi_ti^2)*Treecov + I(ndvi_ti^3)*Treecov, data = veg2)
names(Cand.set) <- c("DE-NDVI", "DE-NDVI2", "DE-NDVI3", 
                     "DE-EVI","DE-EVI2", "DE-EVI3",
                     "DE-NDVIamp", "DE-NDVIamp2", "DE-NDVIamp3",
                     "DE-NDVIti", "DE-NDVI2ti", "DE-NDVI3ti",
                     "DE-NDVI*TC", "DE-NDVI2*TC", "DE-NDVI3*TC", 
                     "DE-EVI*TC","DE-EVI2*TC", "DE-EVI3*TC",
                     "DE-NDVIamp*TC", "DE-NDVIamp2*TC", "DE-NDVIamp3*TC",
                     "DE-NDVIti*TC", "DE-NDVI2ti*TC", "DE-NDVI3ti*TC")
aictable3 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable3, digits = 2, LL = TRUE)


## fecal nitrogen ##
Cand.set <- list( )
Cand.set[[1]] <- lm(PctFN ~ NDVI, data = fn)
Cand.set[[2]] <- lm(PctFN ~ NDVI + I(NDVI^2), data = fn)
Cand.set[[3]] <- lm(PctFN ~ NDVI + I(NDVI^2) + I(NDVI^3), data = fn)
Cand.set[[4]] <- lm(PctFN ~ EVI, data = fn)
Cand.set[[5]] <- lm(PctFN ~ EVI + I(EVI^2), data = fn)
Cand.set[[6]] <- lm(PctFN ~ EVI + I(EVI^2) + I(EVI^3), data = fn)
Cand.set[[7]] <- lm(PctFN ~ NDVI*Treecov, data = fn)
Cand.set[[8]] <- lm(PctFN ~ NDVI*Treecov + I(NDVI^2)*Treecov, data = fn)
Cand.set[[9]] <- lm(PctFN ~ NDVI*Treecov + I(NDVI^2)*Treecov + I(NDVI^3)*Treecov, data = fn)
Cand.set[[10]] <- lm(PctFN ~ EVI*Treecov, data = fn)
Cand.set[[11]] <- lm(PctFN ~ EVI*Treecov + I(EVI^2)*Treecov, data = fn)
Cand.set[[12]] <- lm(PctFN ~ EVI*Treecov + I(EVI^2)*Treecov + I(EVI^3)*Treecov, data = fn)
names(Cand.set) <- c("FN-NDVI", "FN-NDVI2", "FN-NDVI3", 
                     "FN-EVI","FN-EVI2", "FN-EVI3",
                     "FN-NDVI*TC", "FN-NDVI2*TC", "FN-NDVI3*TC", 
                     "FN-EVI*TC","FN-EVI2*TC", "FN-EVI3*TC")
aictable4 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable4, digits = 2, LL = TRUE)


## store and export results ##
aictab.all <- rbind(aictable1, aictable2, aictable3, aictable4)
write.csv(aictab.all, file = "aic-results.csv", row.names = F)


#### top models ####

par(mfrow=c(2,2))

top.bm <- lm(ForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov, data = veg2)
summary(top.bm)
plot(top.bm)

top.bmh <- lm(HerbaceousForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov + I(EVI^3)*Treecov, data = veg2)
summary(top.bmh)
plot(top.bmh)

top.de <- lm(DE ~ ndvi_ti*Treecov + I(ndvi_ti^2)*Treecov + I(ndvi_ti^3)*Treecov, data = veg2)
summary(top.de)
plot(top.de)

top.fn <- lm(PctFN ~ EVI*Treecov, data = fn)
summary(top.fn)
plot(top.fn)

## all supported models ##
m1 <- lm(ForageBiomass ~ NDVI, data = veg2)
m2 <- lm(ForageBiomass ~ EVI*Treecov, data = veg2)
m3 <- lm(ForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov, data = veg2)
m4 <- lm(ForageBiomass ~ NDVI + I(NDVI^2), data = veg2)
m5 <- lm(HerbaceousForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov + I(EVI^3)*Treecov, data = veg2)
m6 <- lm(HerbaceousForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov, data = veg2)
m7 <- lm(DE ~ ndvi_ti*Treecov + I(ndvi_ti^2)*Treecov + I(ndvi_ti^3)*Treecov, data = veg2)
m8 <- lm(DE ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov, data = veg2)
m9 <- lm(PctFN ~ EVI*Treecov, data = fn)
m10 <- lm(PctFN ~ NDVI, data = fn)
m11 <- lm(PctFN ~ EVI, data = fn)

lm1 <- c(m1$coefficients[1], 
         summary(m1)$adj.r.squared, 
         summary(m1)$sigma)
lm2 <- c(m2$coefficients[1], 
         summary(m2)$adj.r.squared, 
         summary(m2)$sigma)
lm3 <- c(m3$coefficients[1], 
         summary(m3)$adj.r.squared, 
         summary(m3)$sigma)
lm4 <- c(m4$coefficients[1], 
         summary(m4)$adj.r.squared, 
         summary(m4)$sigma)
lm5 <- c(m5$coefficients[1], 
         summary(m5)$adj.r.squared, 
         summary(m5)$sigma)
lm6 <- c(m6$coefficients[1], 
         summary(m6)$adj.r.squared, 
         summary(m6)$sigma)
lm7 <- c(m7$coefficients[1], 
         summary(m7)$adj.r.squared, 
         summary(m7)$sigma)
lm8 <- c(m8$coefficients[1], 
         summary(m8)$adj.r.squared, 
         summary(m8)$sigma)
lm9 <- c(m9$coefficients[1], 
         summary(m9)$adj.r.squared, 
         summary(m9)$sigma)
lm10 <- c(m10$coefficients[1], 
         summary(m10)$adj.r.squared, 
         summary(m10)$sigma)
lm11 <- c(m11$coefficients[1], 
         summary(m11)$adj.r.squared, 
         summary(m11)$sigma)


tprep <- rbind(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10, lm11)
tab <- as.data.frame(tprep, row.names = 
                       c("B-NDVI", "B-EVI*T", "B-EVI2*T",
                         "B-NDVI2", "HB-EVI3*T", "HB-EVI2*T",
                         "DE-NDVIti3*T", "DE-NDVIa2*T",
                         "FN-EVI*T", "FN-NDVI", "FN-EVI"))
tab <- rename(tab, AdjRsquared = V2)
tab <- rename(tab, StdError = V3)
View(tab)
write.csv(tab, file = "regressions.csv")


#### top model summaries ####

summary(lm(ForageBiomass ~ NDVI, data = veg2))
summary(lm(ForageBiomass ~ EVI*Treecov, data = veg2))
summary(lm(ForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov, data = veg2))
summary(lm(HerbaceousForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov + I(EVI^3)*Treecov, data = veg2))
summary(lm(HerbaceousForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov, data = veg2))
summary(lm(DE ~ ndvi_ti*Treecov + I(ndvi_ti^2)*Treecov + I(ndvi_ti^3)*Treecov, data = veg2))
summary(lm(DE ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov, data = veg2))
summary(lm(PctFN ~ EVI*Treecov, data = fn))
summary(lm(PctFN ~ NDVI, data = fn))
summary(m(PctFN ~ EVI, data = fn))


#### quick data summaries ####
length(unique(veg2$PlotVisit))


#### VISUALS ####

#### visualizing univariate relationships ####


## forage biomass - loess smoother ##
p.bm.n <- ggplot(veg2, aes(x = NDVI, y = ForageBiomass)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "NDVI", y = expression(paste(
                        "Forage Biomass (g/", 
                         m^2, ")", sep="")))
p.bm.e <- ggplot(veg2, aes(x = EVI, y = ForageBiomass)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "EVI", y = "")
p.bm.na <- ggplot(veg2, aes(x = ndvi_amp, y = ForageBiomass)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "NDVI amplitude", y = "")
p.bm.nt <- ggplot(veg2, aes(x = ndvi_ti, y = ForageBiomass)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "Time-integrated NDVI", y = "")
#grid.arrange(p.bm.n, p.bm.e, p.bm.na, p.bm.nt)



## herbaceous forage biomass - loess smoother ##
p.bmh.n <- ggplot(veg2, aes(x = NDVI, y = HerbaceousForageBiomass)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "NDVI", y = expression(paste(
                        "Herbaceous \nForage Biomass (g/", 
                         m^2, ")", sep="")))
p.bmh.e <- ggplot(veg2, aes(x = EVI, y = HerbaceousForageBiomass)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "EVI", y = "")
p.bmh.na <- ggplot(veg2, aes(x = ndvi_amp, y = HerbaceousForageBiomass)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "NDVI amplitude", y = "")
p.bmh.nt <- ggplot(veg2, aes(x = ndvi_ti, y = HerbaceousForageBiomass)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "Time-integrated NDVI", y = "")
#grid.arrange(p.bmh.n, p.bmh.e, p.bmh.na, p.bmh.nt)



## digestible energy ##
p.de.n <- ggplot(veg2, aes(x = NDVI, y = DE)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "NDVI", y = "Digestible Energy (kcal/g)")
p.de.e <- ggplot(veg2, aes(x = EVI, y = DE)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "EVI", y = "")
p.de.na <- ggplot(veg2, aes(x = ndvi_amp, y = DE)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "NDVI amplitude", y = "")
p.de.nt <- ggplot(veg2, aes(x = ndvi_ti, y = DE)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "Time-integrated NDVI", y = "")
#grid.arrange(p.de.n, p.de.e, p.de.na, p.de.nt)


## fecal nitrogen ##
p.fn.n <- ggplot(fn, aes(x = NDVI, y = PctFN)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "NDVI", y = "Fecal Nitrogen (%)")
p.fn.e <- ggplot(fn, aes(x = EVI, y = PctFN)) +
  geom_smooth(method = "loess", color = "black") +
  geom_point(size = 0.5) +
  labs(x = "EVI", y = "")
#grid.arrange(p.fn.n, p.fn.e)

## all together, hopefully ##
grid.arrange(p.bm.n, p.bm.e, p.bm.na, p.bm.nt,
             p.bmh.n, p.bmh.e, p.bmh.na, p.bmh.nt,
             p.de.n, p.de.e, p.de.na, p.de.nt,
             p.fn.n, p.fn.e)

#### how tree cover affects responses ####

p.bm.tc <- ggplot(veg2, aes(x=EVI, y=ForageBiomass, fill=Treecov)) +
    stat_smooth(method="glm", 
              method.args = list(family="gaussian"), 
              level=0.95)
p.bm.tc

p.de.tc <- ggplot(veg2, aes(x=ndvi_amp, y=DE, fill=Treecov)) +
    stat_smooth(method="glm", 
              method.args = list(family="gaussian"), 
              level=0.95) 

p.fn.tc <- ggplot(fn, aes(x=EVI, y=PctFN, fill=Treecov)) +
    stat_smooth(method="glm", 
              method.args = list(family="gaussian"), 
              level=0.95) 



#### prediction plots, incl polynomials ####


## forage biomass ## 
    #supported models#
    #m1 <- lm(ForageBiomass ~ NDVI, data = veg2)
    #m2 <- lm(ForageBiomass ~ EVI*Treecov, data = veg2)
    #m3 <- lm(ForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov, data = veg2)
p.bm.m1 <- ggplot(veg2, aes(x=NDVI, y=ForageBiomass)) +
  stat_smooth(method="lm", 
              se = TRUE,
              colour = "black",
              lty = "1F")  +
  theme(legend.position="none") +
  labs(y = expression(paste(
                        "Forage Biomass (g/", 
                         m^2, ")", sep="")))
p.bm.m2 <- ggplot(veg2, aes(x=EVI, y=ForageBiomass, linetype=Treecov)) + 
  stat_smooth(method="lm", 
              se = TRUE, 
              colour = "black")+
  theme(legend.position="none")+
  labs(y = "")
p.bm.m3 <- ggplot(veg2, aes(x=EVI, y=ForageBiomass, linetype=Treecov)) + 
  stat_smooth(method="lm", 
              se = TRUE, 
              formula = y ~ poly(x, 2, raw = TRUE), 
              colour = "black")+
  theme(legend.position="none")+
  labs(y="")


## herbaceous forage biomass ##
    #supported models#
    #m5 <- lm(HerbaceousForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov + I(EVI^3)*Treecov, data = veg2)
    #m6 <- lm(HerbaceousForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov, data = veg2)
p.fb <- ggplot(veg2, aes(x = EVI, y = HerbaceousForageBiomass,
                         linetype = Treecov))
p.fb.m1 <- p.fb +
  stat_smooth(method="lm", 
              se = TRUE, 
              formula = y ~ poly(x, 3, raw = TRUE), 
              colour = "black")  +
  theme(legend.position="none") +
  labs(y = expression(paste(
                        "Herbaceous \nForage Biomass (g/", 
                         m^2, ")", sep="")))
p.fb.m2 <- p.fb +
  stat_smooth(method="lm", 
              se = TRUE, 
              formula = y ~ poly(x, 2, raw = TRUE), 
              colour = "black") +
  theme(legend.position="none") +
  labs(y="")


## digestible energy ##
    #supported models#
    #m7 <- lm(DE ~ ndvi_ti*Treecov + I(ndvi_ti^2)*Treecov + I(ndvi_ti^3)*Treecov, data = veg2)
    #m8 <- lm(DE ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov, data = veg2)
p.de.m1 <- ggplot(veg2, aes(x = ndvi_ti, y = DE, linetype = Treecov)) +
  stat_smooth(method = lm,
              se = TRUE,
              formula = y ~ poly(x, 3, raw = TRUE),
              colour = "black")+
  theme(legend.position="none") +
  labs(x = "Time-integrated NDVI",
       y = "Digestible Energy (kcal/g)")
p.de.m2 <- ggplot(veg2, aes(x = ndvi_amp, y = DE, linetype = Treecov)) +
  stat_smooth(method = lm,
              se = TRUE,
              formula = y ~ poly(x, 2, raw = TRUE),
              colour = "black")+
  theme(legend.position="none")+
  labs(x = "NDVI amplitude",
       y="")

## fecal nitrogen ##
    #supported models#
    #m9 <- lm(PctFN ~ EVI*Treecov, data = fn)
    #m10 <- lm(PctFN ~ NDVI, data = fn)
    #m11 <- lm(PctFN ~ EVI, data = fn)
p.fn.m1 <- ggplot(fn, aes(x=EVI, y=PctFN, linetype=Treecov)) +
  stat_smooth(method="lm", 
              se = TRUE, 
              colour = "black")+
  theme(legend.position="none") +
  labs(y = "Fecal Nitrogen (%)")
p.fn.m2 <- ggplot(fn, aes(x=NDVI, y=PctFN)) +
  stat_smooth(method="lm", 
              se = TRUE, 
              colour = "black",
              lty = "1F")+
  theme(legend.position="none")+
  labs(y="")
p.fn.m3 <- ggplot(fn, aes(x=EVI, y=PctFN)) +
  stat_smooth(method="lm", 
              se = TRUE, 
              colour = "black",
              lty = "1F")+
  theme(legend.position="none")+
  labs(y="")


## plot together ##

#extract legend (from sthda.com)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
#getting hacky to savetime
fnno <- fn %>%
  mutate(tc = ifelse(Treecov == 0, "Open Canopy", "Forest Canopy")) 
fnno$tc <- factor(fnno$tc,
                     levels = c("Open Canopy", "Forest Canopy"))
p.fn.m <- ggplot(fnno, aes(x=EVI, y=PctFN, linetype=tc)) +
  stat_smooth(method="lm", 
              se = TRUE, 
              colour = "black")+
  theme(legend.title=element_blank())
legend <- get_legend(p.fn.m)


grid.arrange(p.bm.m1, p.bm.m2, p.bm.m3,
             p.fb.m1, p.fb.m2, legend,
             p.de.m1, p.de.m2, 
             p.fn.m1, p.fn.m2, p.fn.m3,
             layout_matrix = rbind(c(1,2,3),
                                   c(4,5,6),
                                   c(7,8, NA),
                                   c(9,10,11)))
