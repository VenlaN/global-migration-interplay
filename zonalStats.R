rm(list = ls())

setwd("/Users/nivav1/Documents/")

path_out <- "/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/"

## Set path for packages (local disk)
.libPaths("/Users/nivav1/Documents/R/")

## Timestamp for filenames
today <- Sys.Date()

## Open packages
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(raster)
library(rgdal)
library(sf)

#### Load data ####
#### AC-components ####
# Governance
governance <- raster("DATA/governance.tif")
# Econymy (GDPpc) 
economy <- raster("/DATA/economy.tif")
# Health 
health <- raster("DATA/health.tif")
# Education
education <- raster("DATA/education.tif")

#### ES-components ####
# Natural hazards: subset data and calculate decadal mean
natHaz <- raster::stack("DATA/Resilience_nc/natural_hazards.nc") %>% 
  subset(., 1: 11) %>% calc(., fun = mean, na.rm = T)
# Water risk
waterRisk <- raster::raster("DATA/waterRiskScaled.tif")
# Food prod scarcity
foodProdSca <- raster::raster("DATA/foodProdScarcityScaled.tif")
# Drought risk
droughtRisk <- raster::raster("DATA/droughtRiskScaled.tif")  
#### Stack components ####

AC_vars <- raster::stack(economy, education, governance, health)
names(AC_vars) <- c("economy", "education", "governance",  "health")

ES_vars <- raster::stack(natHaz,waterRisk,foodProdSca, droughtRisk)
names(ES_vars) <- c("natHaz","waterRisk","foodProdSca", "droughtRisk")

responseVars <- stack(economy, health, education, governance, natHaz, waterRisk, droughtRisk, foodProdSca)

writeRaster(responseVars, "/DATA/MigrationRespVars.tif", overwrite = T)  

#### Combine into composite indices ####

AC <- calc(AC_vars, fun = mean, na.rm = T)
ES <- calc(ES_vars, fun = mean, na.rm = T)

# Set sea-values to NA in EV
ES[ES == ES[1,1]] <- NA

#### Create classification matrix ####
# Initiate a classification raster
class_raster <- raster(AC)
# Assign a class for each value based on the following conditions
class_raster[AC >= 0 & AC < 0.25 & EV >= 0.75 & EV <= 1.00] <- 1
class_raster[AC >= 0.25 & AC< 0.50 & EV >= 0.75 & EV <= 1.00] <- 2
class_raster[AC >= 0.5 & AC<  0.75 & EV >= 0.75 & EV <= 1.00] <-  3
class_raster[AC >= 0.75 & AC <= 1.00 & EV >= 0.75 & EV <= 1.00] <- 4
class_raster[AC >= 0 & AC< 0.25 & EV >= 0.5 & EV < 0.75]  <- 5
class_raster[AC >= 0.25 & AC <  0.50 & EV >= 0.5  & EV < 0.75] <- 6
class_raster[AC >= 0.5 & AC < 0.75 & EV >= 0.5 & EV < 0.75]  <- 7
class_raster[AC >= 0.75 & AC <= 1.00 & EV >= 0.5 & EV < 0.75] <- 8
class_raster[AC >= 0 & AC < 0.25 & EV >= 0.25 & EV < 0.50] <- 9
class_raster[AC >= 0.25 & AC< 0.50 & EV >= 0.25 & EV < 0.50] <- 10
class_raster[AC >= 0.5 & AC < 0.75 & EV >= 0.25 & EV < 0.50] <- 11
class_raster[AC >= 0.75 & AC <= 1.00 & EV >= 0.25 & EV < 0.50] <- 12
class_raster[AC >= 0 & AC < 0.25 & EV >= 0 & EV < 0.25] <- 13
class_raster[AC >= 0.25 & AC < 0.50 & EV >= 0 & EV < 0.25]  <- 14
class_raster[AC >= 0.5 & AC < 0.75 & EV >= 0 & EV < 0.25] <- 15
class_raster[AC >= 0.75 & AC <= 1.00 & EV >= 0 & EV < 0.25] <- 16

writeRaster(class_raster, "DATA/class_raster.tif", overwrite = T)  

#### Zonal statistics based on classification matrix ####
# Load data
class_raster <- raster("DATA/class_raster.tif")
NM_stack <- raster::stack("DATA/NM_stack.tif")
names(NM_stack) <- c("NM_in", "NM_in_ppop", "NM_out", "NM_out_ppop")
pop_raster <- raster("DATA/1990AD_pop-1/popc_1990AD.asc")
landRaster <- raster("DATA/land_area_km2_5arcmin.tif")


temp_pop_in <- pop_raster
temp_pop_in[is.na(NM_stack$NM_in)] <- NA

temp_pop_out <- pop_raster
temp_pop_out[is.na(NM_stack$NM_out)] <- NA

temp_land_in <- landRaster
temp_land_in[is.na(NM_stack$NM_in)] <- NA

temp_land_out <- landRaster
temp_land_out[is.na(NM_stack$NM_out)] <- NA

# Absolute number of migration in net positive (in) and net negative (out) areas in each class
classStats <- as_tibble(raster::zonal(NM_stack$NM_in, class_raster, fun = "sum", na.rm = T)) %>% dplyr::rename(., "in_NM" = "sum") %>% 
  left_join(as_tibble(raster::zonal(NM_stack$NM_out, class_raster, fun = "sum", na.rm = T)), by = c("zone" = "zone")) %>% dplyr::rename(., "out_NM" = "sum") %>% 
  left_join(as_tibble(raster::zonal(temp_pop_in, class_raster, fun = "sum", na.rm = T)), by = c("zone" = "zone")) %>% dplyr::rename(., "pop_inCells" = "sum") %>% 
  left_join(as_tibble(raster::zonal(temp_pop_out, class_raster, fun = "sum", na.rm = T)), by = c("zone" = "zone")) %>% dplyr::rename(., "pop_outCells" = "sum") %>% 
  left_join(as_tibble(raster::zonal(pop_raster, class_raster, fun = "sum", na.rm = T)), by = c("zone" = "zone")) %>% dplyr::rename(., "pop_all" = "sum") %>% 
  left_join(as_tibble(raster::zonal(temp_land_in, class_raster, fun = "sum", na.rm = T)), by = c("zone" = "zone")) %>% dplyr::rename(., "land_inCells" = "sum") %>% 
  left_join(as_tibble(raster::zonal(temp_land_out, class_raster, fun = "sum", na.rm = T)), by = c("zone" = "zone")) %>% dplyr::rename(., "land_outCells" = "sum") %>%
  left_join(as_tibble(raster::zonal(landRaster, class_raster, fun = "sum", na.rm = T)), by = c("zone" = "zone")) %>% dplyr::rename(., "landA_all" = "sum")


classStats <- mutate(classStats, shr_in = in_NM / sum(in_NM),
                     shr_out = out_NM / sum(out_NM),
                     shr_pop_in = pop_inCells / sum(pop_all), #share of population in net-positive cells out of global pop
                     shr_pop_out = pop_outCells / sum(pop_all), #share of population in net-negative cells out of global pop
                     shr_land_in = land_inCells / sum(landA_all),
                     shr_land_out = land_outCells / sum(landA_all))


classStats <- classStats %>% group_by(., zone) %>% 
  mutate(shrOutNMperPopCluster = out_NM/pop_all, # share of net-neg. migr. of total pop in cluster
         shrInNMperPopCluster = in_NM/pop_all, # share of net-pos. migr. of total pop in cluster
         shrOutNMper1000PopCluster = (out_NM/pop_all)*1000, # share of net-neg. migr. of total pop in cluster
         shrInNMper1000PopCluster = (in_NM/pop_all)*1000,
         shroutNMperPopGlobal = out_NM/sum(classStats$pop_all), # share of net-neg. migr. of global pop
         shrinNMperPopGlobal = in_NM/sum(classStats$pop_all), # share of net-pos. migr. of global pop
         shrNetNegPopGlobal = pop_outCells/sum(classStats$pop_all), # share of population living in net-negative migration areas of global pop
         shrNetPosPopGlobal = pop_inCells/sum(classStats$pop_all), # share of population living in net-positive migration areas of global pop
         shrPop = pop_all/sum(classStats$pop_all), # share of population in each cluster
         shrNetNegLandGGlobal = land_outCells/sum(classStats$landA_all), # share of land in net-negative migration areas of global land A
         shrNetPosLandGlobal = land_inCells/sum(classStats$landA_all), # share of land  in net-positive migration areas of global land A
         shrland = landA_all/sum(classStats$landA_all))# share of land in each cluster per global land

write_csv(classStats, "resultsByClass.csv")

