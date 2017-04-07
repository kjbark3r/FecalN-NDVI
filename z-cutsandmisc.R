### cut code ###
### from remote sensing section of fwp report ###
## some day i'll do something right on the first try ###

## plotting polynomial response
  ## if start just poly then try interaction
test <- lm(ForageBiomass ~ EVI + I(EVI^2) + I(EVI^3), data = veg2)
test2 <- veg2$ForageBiomass ~ veg2$EVI + I(veg2$EVI^2) + I(veg2$EVI^3)
curve(test2, from = min(veg2$ForageBiomass), to = max(veg2$ForageBiomass),
      n = 100)
# newp #

p <- ggplot(veg2, aes(x=EVI, y=ForageBiomass, linetype=Treecov))
p2 <- p + 
  stat_smooth(method="lm", 
              se = TRUE, 
              #fill = NA,
              formula = y ~ poly(x, 3, raw = TRUE), 
              colour = "black")
#omfggg              
              

## ndvi, evi thru yr
test <- ndvi
test$SDate <- as.Date(as.character(test$RemoteDate), format='%Y-%m-%d')
test$DOY <- strftime(test$SDate, format = "%j")
plot(NDVI ~ DOY, data = test)

## hierarchical selection, bad plan


## forage biomass ##
Cand.set <- list( )
Cand.set[[1]] <- glm(ForageBiomass ~ NDVI, data = veg2)
Cand.set[[2]] <- glm(ForageBiomass ~ NDVI + I(NDVI^2), data = veg2)
Cand.set[[3]] <- glm(ForageBiomass ~ NDVI + I(NDVI^2) + I(NDVI^3), data = veg2)
Cand.set[[4]] <- glm(ForageBiomass ~ EVI, data = veg2)
Cand.set[[5]] <- glm(ForageBiomass ~ EVI + I(EVI^2), data = veg2)
Cand.set[[6]] <- glm(ForageBiomass ~ EVI + I(EVI^2) + I(EVI^3), data = veg2)
Cand.set[[7]] <- glm(ForageBiomass ~ ndvi_amp, data = veg2)
Cand.set[[8]] <- glm(ForageBiomass ~ ndvi_amp + I(ndvi_amp^2), data = veg2)
Cand.set[[9]] <- glm(ForageBiomass ~ ndvi_amp + I(ndvi_amp^2) + I(ndvi_amp^3), data = veg2)
Cand.set[[10]] <- glm(ForageBiomass ~ ndvi_ti, data = veg2)
Cand.set[[11]] <- glm(ForageBiomass ~ ndvi_ti + I(ndvi_ti^2), data = veg2)
Cand.set[[12]] <- glm(ForageBiomass ~ ndvi_ti + I(ndvi_ti^2) + I(ndvi_ti^3), data = veg2)
names(Cand.set) <- c("Biomass-NDVI", "Biomass-NDVI2", "Biomass-NDVI3", 
                     "Biomass-EVI","Biomass-EVI2", "Biomass-EVI3",
                     "Biomass-NDVIamp", "Biomass-NDVIamp2", "Biomass-NDVIamp3",
                     "Biomass-NDVIti", "Biomass-NDVI2ti", "Biomass-NDVI3ti")
aictable1 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable1, digits = 2, LL = TRUE)


## herbaceous forage biomass ##
## forage biomass ##
Cand.set <- list( )
Cand.set[[1]] <- glm(HerbaceousForageBiomass ~ NDVI, data = veg2)
Cand.set[[2]] <- glm(HerbaceousForageBiomass ~ NDVI + I(NDVI^2), data = veg2)
Cand.set[[3]] <- glm(HerbaceousForageBiomass ~ NDVI + I(NDVI^2) + I(NDVI^3), data = veg2)
Cand.set[[4]] <- glm(HerbaceousForageBiomass ~ EVI, data = veg2)
Cand.set[[5]] <- glm(HerbaceousForageBiomass ~ EVI + I(EVI^2), data = veg2)
Cand.set[[6]] <- glm(HerbaceousForageBiomass ~ EVI + I(EVI^2) + I(EVI^3), data = veg2)
Cand.set[[7]] <- glm(HerbaceousForageBiomass ~ ndvi_amp, data = veg2)
Cand.set[[8]] <- glm(HerbaceousForageBiomass ~ ndvi_amp + I(ndvi_amp^2), data = veg2)
Cand.set[[9]] <- glm(HerbaceousForageBiomass ~ ndvi_amp + I(ndvi_amp^2) + I(ndvi_amp^3), data = veg2)
Cand.set[[10]] <- glm(HerbaceousForageBiomass ~ ndvi_ti, data = veg2)
Cand.set[[11]] <- glm(HerbaceousForageBiomass ~ ndvi_ti + I(ndvi_ti^2), data = veg2)
Cand.set[[12]] <- glm(HerbaceousForageBiomass ~ ndvi_ti + I(ndvi_ti^2) + I(ndvi_ti^3), data = veg2)
names(Cand.set) <- c("HerbBiomass-NDVI", "HerbBiomass-NDVI2", "HerbBiomass-NDVI3", 
                     "HerbBiomass-EVI","HerbBiomass-EVI2", "HerbBiomass-EVI3",
                     "HerbBiomass-NDVIamp", "HerbBiomass-NDVIamp2", "HerbBiomass-NDVIamp3",
                     "HerbBiomass-NDVIti", "HerbBiomass-NDVI2ti", "HerbBiomass-NDVI3ti")
aictable10 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable10, digits = 2, LL = TRUE)

## digestible energy ##
Cand.set <- list( )
Cand.set[[1]] <- glm(DE ~ NDVI, data = veg2)
Cand.set[[2]] <- glm(DE ~ NDVI + I(NDVI^2), data = veg2)
Cand.set[[3]] <- glm(DE ~ NDVI + I(NDVI^2) + I(NDVI^3), data = veg2)
Cand.set[[4]] <- glm(DE ~ EVI, data = veg2)
Cand.set[[5]] <- glm(DE ~ EVI + I(EVI^2), data = veg2)
Cand.set[[6]] <- glm(DE ~ EVI + I(EVI^2) + I(EVI^3), data = veg2)
Cand.set[[7]] <- glm(DE ~ ndvi_amp, data = veg2)
Cand.set[[8]] <- glm(DE ~ ndvi_amp + I(ndvi_amp^2), data = veg2)
Cand.set[[9]] <- glm(DE ~ ndvi_amp + I(ndvi_amp^2) + I(ndvi_amp^3), data = veg2)
Cand.set[[10]] <- glm(DE ~ ndvi_ti, data = veg2)
Cand.set[[11]] <- glm(DE ~ ndvi_ti + I(ndvi_ti^2), data = veg2)
Cand.set[[12]] <- glm(DE ~ ndvi_ti + I(ndvi_ti^2) + I(ndvi_ti^3), data = veg2)
names(Cand.set) <- c("DE-NDVI", "DE-NDVI2", "DE-NDVI3", 
                     "DE-EVI","DE-EVI2", "DE-EVI3",
                     "DE-NDVIamp", "DE-NDVIamp2", "DE-NDVIamp3",
                     "DE-NDVIti", "DE-NDVI2ti", "DE-NDVI3ti")
aictable2 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable2, digits = 2, LL = TRUE)


## fecal nitrogen ##
Cand.set <- list( )
Cand.set[[1]] <- glm(PctFN ~ NDVI, data = fn)
Cand.set[[2]] <- glm(PctFN ~ NDVI + I(NDVI^2), data = fn)
Cand.set[[3]] <- glm(PctFN ~ NDVI + I(NDVI^2) + I(NDVI^3), data = fn)
Cand.set[[4]] <- glm(PctFN ~ EVI, data = fn)
Cand.set[[5]] <- glm(PctFN ~ EVI + I(EVI^2), data = fn)
Cand.set[[6]] <- glm(PctFN ~ EVI + I(EVI^2) + I(EVI^3), data = fn)
names(Cand.set) <- c("FN-NDVI", "FN-NDVI2", "FN-NDVI3",
                     "FN-EVI", "FN-EVI2", "FN-EVI3")
aictable3 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable3, digits = 2, LL = TRUE)

## store and export results ##
aictab.all <- rbind(aictable1, aictable10, aictable2, aictable3)
write.csv(aictab.all, file = "aic-results.csv", row.names = F)



#### model selection - tree cover? ####


## forage biomass ##
Cand.set <- list( )
Cand.set[[1]] <- glm(ForageBiomass ~ NDVI, data = veg2)
Cand.set[[2]] <- glm(ForageBiomass ~ NDVI*Treecov, data = veg2)
Cand.set[[3]] <- glm(ForageBiomass ~ EVI, data = veg2)
Cand.set[[4]] <- glm(ForageBiomass ~ EVI*Treecov, data = veg2)
names(Cand.set) <- c("B-NDVI", "B-NDVI*Treecov",
                     "B-EVI", "B-EVI*Treecov")
aictable4 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable4, digits = 2, LL = TRUE)

## herbaceous forage biomass ##
Cand.set <- list( )
Cand.set[[1]] <- glm(HerbaceousForageBiomass ~ EVI + I(EVI^2), data = veg2)
Cand.set[[2]] <- glm(HerbaceousForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov, data = veg2)
Cand.set[[3]] <- glm(HerbaceousForageBiomass ~ EVI + I(EVI^2) + I(EVI^3), data = veg2)
Cand.set[[4]] <- glm(HerbaceousForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov + I(EVI^3)*Treecov, data = veg2)
names(Cand.set) <- c("BH-EVI2", "BH-EVI2*Treecov",
                     "BH-EVI3", "BH-EVI3*Treecov")
aictable11 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable11, digits = 2, LL = TRUE)


## digestible energy ##
Cand.set <- list( )
Cand.set[[1]] <- glm(DE ~ ndvi_amp + I(ndvi_amp^2) + I(ndvi_amp^3), data = veg2)
Cand.set[[2]] <- glm(DE ~ ndvi_amp, data = veg2)
Cand.set[[3]] <- glm(DE ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov + I(ndvi_amp^3)*Treecov, data = veg2)
Cand.set[[4]] <- glm(DE ~ ndvi_amp*Treecov, data = veg2)
names(Cand.set) <- c("DE-NDVIamp3", "DE-NDVIamp",
                     "DE-NDVIamp3*Treecov", "DE-NDVIamp*Treecov")
aictable5 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable5, digits = 2, LL = TRUE)


## fecal nitrogen ##
Cand.set <- list( )
Cand.set[[1]] <- lm(PctFN ~ NDVI, data = fn)
Cand.set[[2]] <- lm(PctFN ~ NDVI*Treecov, data = fn)
Cand.set[[3]] <- lm(PctFN ~ EVI, data = fn)
Cand.set[[4]] <- lm(PctFN ~ EVI*Treecov, data = fn)
names(Cand.set) <- c("FN-NDVI", "FN-NDVI*Treecov",
                     "FN-EVI", "FN-EVI*Treecov")
aictable6 <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable6, digits = 2, LL = TRUE)

## store and export results ##
aictab.all2 <- rbind(aictable4, aictable11, aictable5, aictable6)
write.csv(aictab.all2, file = "aic-results-treecov.csv", row.names = F)

par(mfrow=c(2,2))

top.bm <- lm(ForageBiomass ~ NDVI, data = veg2)
summary(top.bm)
plot(top.bm)

top.bmh <- lm(HerbaceousForageBiomass ~ EVI*Treecov + I(EVI^2)*Treecov, data = veg2)
summary(top.bmh)
plot(top.de2)

top.de1 <- lm(DE ~ ndvi_amp*Treecov, data = veg2)
summary(top.de1)
plot(top.de1)

top.de2 <- lm(DE ~ ndvi_amp*Treecov + I(ndvi_amp^2)*Treecov + I(ndvi_amp^3)*Treecov, data = veg2)
summary(top.de2)
plot(top.de2)

top.fn1 <- lm(PctFN ~ EVI*Treecov, data = fn)
summary(top.fn1)
plot(top.fn1)
# reporting this only bc r2 = .09 , 
# next model FN~NDVI only 0.04 and p > 0.05
# not that that really matters, but...

lm1 <- c(top.bm$coefficients[1], 
         summary(top.bm)$adj.r.squared, 
         summary(top.bm)$sigma)
lm2 <- c(top.bmh$coefficients[1], 
         summary(top.bmh)$adj.r.squared, 
         summary(top.bmh)$sigma)
lm3 <- c(top.de1$coefficients[1], 
         summary(top.de1)$adj.r.squared, 
         summary(top.de1)$sigma)
lm4 <- c(top.de2$coefficients[1], 
         summary(top.de2)$adj.r.squared, 
         summary(top.de2)$sigma)
lm5 <- c(top.fn1$coefficients[1], 
         summary(top.fn1)$adj.r.squared, 
         summary(top.fn1)$sigma)

tprep <- rbind(lm1, lm2, lm3, lm4, lm5)
tab <- as.data.frame(tprep, row.names = 
                       c("Biomass-NDVI*Treecov", 
                         "HerbBiomass-EVI3*Treecov", 
                         "DE-NDVIamp*Treecov",
                         "DE-NDVIamp3*Treecov",
                         "FN-EVI*Treecov"))
tab <- rename(tab, AdjRsquared = V2)
tab <- rename(tab, StdError = V3)
View(tab)
write.csv(tab, file = "regressions.csv")


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


#### figuring out the na landcov/treecov issue ####
tmp <- read.csv("../Vegetation/DE-model-data.csv")
colnames(tmp)
any(is.na(tmp$landcov))
# use this instead of extract

## extracting landcover (gives NAs)
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")
lc14 <- raster("../vegetation/writtenrasters/covs2014/landcov_14.tif")
data.xy <- data.frame("x" = vegtmp$Longitude, "y" = vegtmp$Latitude)
data.ll <- SpatialPointsDataFrame(data.xy, vegtmp, proj4string = latlong)
data.sp <- spTransform(data.ll, stateplane)
ext <- raster::extract(lc14, data.sp) # same as lv15, verified
