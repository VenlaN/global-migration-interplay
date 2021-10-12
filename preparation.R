# Check and load libraries

rm(list = ls())

setwd("mydir")

## Set path out (for exporting data and figures)
path_out <- "mypath"

## Set path for packages
.libPaths("/Documents/R/")

## Install packages
packages <- c("raster")
not_installed <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(not_installed)){install.packages(not_installed)}

## Open packages
library(raster)

##### Aggregate net-migration data #####

# Load migration data (res: 30 arcsec)
NM_30arcsec <- raster("/DATA/30arcsec-net-migration-1990-2000.tif")

# Load a reference raster (res: 5 arcmin)
ref_raster <- raster("DAT//governance.tif")

# Check no-data value
NM_30arcsec[1,1] #NA

# Set extent of land-area-30arcsec
NM_30arcsec_extent_set <- raster::extend(NM_30arcsec, extent(ref_raster)) 

#Check res and extent
NM_30arcsec_extent_set #OK

# Plot
plot(NM_30arcsec_extent_set)

# Aggregate to 5 arcmin using raster::aggregate. Grid cell in 30arcsec: 1x1km, in 5arcmin: 10x10km -> factor=10
NM_5arcmin <- raster::aggregate(NM_30arcsec_extent_set, fact = 10, fun = sum, na.rm = T)
NM_5arcmin
# Plot
plot(NM_5arcmin)

writeRaster(NM_5arcmin, "NM_5arcmin_revised150420.tif", options = c("TFW = YES"), overwrite = T)

#### Calculate net-migration per population ####
# Load nm-data
NM_raster <- raster("DATA/NM_5arcmin_revised150420.tif")

# Load population data
pop_raster <- raster("DATA/popc_1990AD.asc")

# First set 0 pop to NA
pop_raster[pop_raster == 0] <- NA
plot(pop_raster)

# Calculate net-migrants per population
NM_raster_ppop <- NM_raster / pop_raster
plot(NM_raster_ppop)

#### Divide net-migration data into net-negative and net-positive rasters ####
# net-positive migration
NM_in <- NM_raster
NM_in[NM_raster < 0] <- NA # set negative values to NA
NM_in

NM_in_ppop <- NM_raster_ppop
NM_in_ppop[NM_raster_ppop < 0] <- NA # set negative values to NA
NM_in_ppop

# net-negative migration
NM_out <- NM_raster
NM_out[NM_raster >= 0] <- NA # set positive values (0 or larger) to NA
NM_out <- NM_out*-1 # change sign
NM_out

NM_out_ppop <- NM_raster_ppop
NM_out_ppop[NM_raster_ppop >= 0] <- NA # set negative values to NA
NM_out_ppop <- NM_out_ppop*-1

# Collect in and out-migration into a stack
NM_stack <- raster::stack(NM_in, NM_in_ppop, NM_out, NM_out_ppop)
names(NM_stack) <- c("NM_in", "NM_in_ppop", "NM_out", "NM_out_ppop")

writeRaster(NM_stack, "/DATA/NM_stack.tif", overwrite = T)  


#### Scale AC variables ####

# Load AC variables
# Governance in 1990-2015
governance <- raster::stack("/DATA/governance.nc")
# Econymy GNI, 1990-2019
economy <- raster::stack("/DATA/downscaledGNI_1990_2019_nightLights_unsmoothed_HDIscaled.tif")
# Health # 1990-2019
health <- raster::stack("/DATA/hdiComponents_5arcmin/hdiComponent_healthindex.tif")
# Education
education <- raster::stack("/DATA/hdiComponents_5arcmin/hdiComponent_edindex.tif")

# Subset 1990-2000
economy <- subset(economy, 1:11)
education <- subset(education, 1:11)
health <- subset(health, 1: 11)
governance <- raster::subset(governance, 1: 11)

# Subset 2010-2015
economy <- subset(economy, 21:26)
education <- subset(education, 21:26)
health <- subset(health, 21:26)
governance <- raster::subset(governance, c("X2010" ,"X2011" ,"X2012" ,"X2013", "X2014", "X2015"))


# Calculate decadal mean
economy <- calc(economy, fun = mean, na.rm = T)
education <- calc(education, fun = mean, na.rm = T)
health <- calc(health,fun = mean, na.rm = T)
governance <- calc(governance, fun = mean, na.rm = T)

# Stack rasters
AC_vars <- raster::stack(economy, education, health, governance)
names(AC_vars) <- c("economy", "education", "health", "governance")

# Normalize: lowest & highest get 0 and 1 -> scale between
quants <- raster::quantile(AC_vars, na.rm = T, probs = c(0.05, 0.95))

education[education >= quants[2,2]] <- quants[2,2]
education[education <= quants[2,1]] <- quants[2,1]

health[health >= quants[3,2]] <- quants[3,2]
health[health <= quants[3,1]] <- quants[3,1]

education <- (education - cellStats(education, stat = min, na.rm =T)) / (cellStats(education, stat = max, na.rm =T) - cellStats(education, stat = min, na.rm =T))
health <- (health - cellStats(health, stat = min, na.rm =T)) / (cellStats(health, stat = max, na.rm =T) - cellStats(health, stat = min, na.rm =T))

plot(economy)
plot(education)
plot(health)

# Save pressures
writeRaster(economy, "/DATA/economy.tif", overwrite = T)
writeRaster(education, "/DATA/education.tif", overwrite = T)
writeRaster(health, "/DATA/health.tif", overwrite = T)
writeRaster(governance, "/DATA/governance.tif", overwrite = T)

#### Scale ES variables ####
#### Food production scarcity ####
# Load food production data for 2000
foodProd <- raster("/DATA/foodProd2000.tif")
# multiply by 10^6
foodProd <- foodProd *10^6

# Load population count for 2000
pop <- raster("/DATA/popc_2000AD.asc")
# set 0 pop to NA 
pop[pop == 0] <- NA

plot(pop)

# Check data
cellStats(foodProd, stat = "sum")/365/(cellStats(pop, stat = sum, na.rm = T))
# ok

# Calculate per capita production per day for each cell in year 2000
foodProdPc <- foodProd /pop
foodProdPc <- foodProdPc / 365

plot(foodProdPc)

writeRaster(foodProdPc, "/DATA/foodProd2000percapPerday.tif", overwrite = T)

# Scale data into FPS indicator
temp <- foodProdPc
temp[temp >= 5000] <- 5000 
temp[temp <= 500]  <- 500

# (max - X) / (max - min)
temp <- (cellStats(temp, stat = max, na.rm =T) - temp) / (cellStats(temp, stat = max, na.rm =T) - cellStats(temp, stat = min, na.rm =T))

plot(temp)

writeRaster(temp, "foodProdScarcityScaled.tif", overwrite = T)


#### Water risk ####

# Baseline water stress 
waterStress <- raster("/DATA/water_risk_reprojected/bws.tif ") 

# Unimproved drinking water & unimproved sanitation
dwStress <- raster("/DATA/water_risk_reprojected/udw.tif ") 
sanStress <- raster("/DATA/water_risk_reprojected/usa.tif ") 
# Combine to qualitative stress
qualStress <- sqrt(dwStress^2 + sanStress^2)
qualStress
plot(qualStress)

# Combine to total stress
totStress <- sqrt(waterStress^2 + qualStress^2)
totStress
plot(totStress) 

# Min-max normalize
quants <- as.vector(raster::quantile(totStress, na.rm = T, probs = c(0.05, 0.95)))
quants

temp <- totStress
temp[temp >= quants[2]] <- quants[2]
temp[temp <= quants[1]] <- quants[1]
temp

temp <- (temp - cellStats(temp, stat = min, na.rm =T)) / (cellStats(temp, stat = max, na.rm =T) - cellStats(temp, stat = min, na.rm =T))
temp
plot(temp)

writeRaster(temp, "waterRiskScaled.tif", overwrite = T)


#### Drought risk ####
# Load data
spei <- brick("data/spei12.nc")

dates <- spei@z$Date
ind <- dates >= as.Date("1990-01-01") & dates < as.Date("2000-01-01")
y9000 <- subset(spei, which(ind))
ind <- dates >= as.Date("2000-01-01") & dates <= as.Date("2009-12-31")
y0010 <- subset(spei, which(ind))
sum_squared <- function(x, na.rm = TRUE) {
  x <- na.omit(x)
  x[x > 0] <- 0
  out <- sum(x^2)
  return(out)
}

agg9000 <- calc(y9000, fun=sum_squared, na.rm=TRUE)
agg0010 <- calc(y0010, fun=sum_squared, na.rm=TRUE)

writeRaster(agg9000, filename = "data/squared_spei_drought_1990-1999.tif")
writeRaster(agg0010, filename = "data/squared_spei_drought_2000-2009.tif")

agg9000[agg9000 > 500] <- 500
agg0010[agg0010 > 500] <- 500

rasterVis::levelplot(agg9000)
rasterVis::levelplot(agg0010)

# upload data
spei <- raster("/DATA/squared_spei_drought_1990-1999.tif")
spei

# Check sea values
spei[1,1]

# Set sea values to NA
spei[spei[1,1]] <- NA

plot(spei)

# Resample 
# Load a reference raster
ref_raster <- raster("/DATA/governance.tif")

temp <- resample(spei, ref_raster)
plot(temp)

# Min-max normalize
quants <- as.vector(raster::quantile(temp, na.rm = T, probs = c(0.05, 0.95)))

temp[temp >= quants[2]] <- quants[2]
temp[temp <= quants[1]] <- quants[1]
temp

temp <- (temp - cellStats(temp, stat = min, na.rm =T)) / (cellStats(temp, stat = max, na.rm =T) - cellStats(temp, stat = min, na.rm =T))
temp
plot(temp)

writeRaster(temp, "droughtRiskScaled.tif", overwrite = T)




