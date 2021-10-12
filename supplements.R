rm(list = ls())


setwd("/Users/nivav1/Documents/")
path_out <- "/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/illustrations"
.libPaths("/Users/nivav1/Documents/R/")

library(dplyr)
library(readr)
library(tidyr)
library(raster)
library(GGally)
library(stringr)
library(raster)
library(rgdal)
library(sf)
library(tmap)
library(classInt)


#### Correlation plots ####
# Extracted observations per country
netMigrZones <- read_csv("DATA/migrationCntry.csv")
# Extracted observations per country
predictors <- read_csv("DATA/migrationPredictorsCntryUpdate.csv")

data <- cbind(netMigrZones, predictors)
head(data)
# Remove duplicate columns
data <-  data[,!duplicated(names(data))] %>% as_tibble(.)
data <- dplyr::select(data, zone, NM_in, NM_in_ppop,  NM_out, NM_out_ppop, income, health, education, governance, natHaz, waterRisk, droughtRisk, foodProdSca)
data <- arrange(data, zone)

# Take sample for correlation plots
set.seed(123456)
dataSample <- sample_n(data, round(nrow(data)*0.25, digits=0)) 


corrOut <- GGally::ggcorr(dataSample %>% 
         dplyr::select( -NM_out, -zone, -NM_in_ppop, -NM_in) %>%
         select_if(is.numeric) %>% 
         drop_na(),
       method = c("everything", "spearman"),
       label = T) 

corrIn <- GGally::ggcorr(data %>% 
                 dplyr::select( -NM_out_ppop, -NM_out, -zone, -NM_in) %>%
                 select_if(is.numeric) %>% 
                 drop_na(),
               method = c("everything", "spearman"),
               label =T) 

library(cowplot)
cowplot::plot_grid(corrOut, corrIn)
ggsave2(paste0(path_out, "illustrations/corrplots.pdf"))

#### Variable distributions #####

#### Scaled variables ####

variables <- stack("/DATA/MigrationRespVars.tif")
names(variables) <- c("economy", "health", "education", "governance", "natHaz", "waterRisk", "droughtRisk", "foodProdSca")

variables[1,1]
variables$droughtRisk[variables$droughtRisk == variables$droughtRisk[1,1]] <- NA

# Fix projection

variables <- projectRaster(variables, crs = "+proj=robin", over= T, method = "ngb")

# Define colourpalette 

mybreaks <- c(0,0.1,0.2,0.4,0.6,0.8,0.9,1)
mycolor <- paletteer::paletteer_c("scico::batlow", n=length(mybreaks), direction = -1)


variablePlots <- tm_shape(variables, projection = "+proj=robin") +
  tm_raster("variables", 
            breaks=mybreaks, 
            palette = mycolor,
            legend.show = T)

variablePlots
tmap_save(variablePlots, filename = paste0(path_out, "/variablePlotsSupplements.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)

#### Original data ####

#### Food production per capita per day ####
foodprodPcPd <- raster("/DATA/food_prod/year2000/foodProd2000percapPerday.tif")
foodprodPcPd <- projectRaster(foodprodPcPd, crs = "+proj=robin", over= T, method = "ngb")


mybreaks <- c(0,500,1000,2000,3000,4000, 5000)
mycolor <- paletteer::paletteer_c("scico::bamako", n=length(mybreaks), direction = -1)

foodProdPlot <- tm_shape(foodprodPcPd, projection = "+proj=robin") +
  tm_raster("foodProd2000percapPerday", 
            breaks=mybreaks, 
            palette = mycolor,
            legend.show = T)
tmap_save(foodProdPlot, filename = paste0(path_out, "/foodProdPlotSupplements.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)


#### Annual crop yield ####
# Load food production data for 2000
foodProd <- raster("/DATA/food_prod/year2000/foodProd2000.tif")
foodProd <- projectRaster(foodProd, crs = "+proj=robin", over= T, method = "ngb")
# multiply by 10^6
foodProd <- foodProd *10^6

mybreaks <- c(0, 0.5, 1, 5, 10,50,100)*10^9
mycolor <- paletteer::paletteer_c("scico::bamako", n=length(mybreaks), direction = -1)

cropYields <- tm_shape(foodProd, projection = "+proj=robin") +
  tm_raster("layer", 
            breaks=mybreaks, 
            palette = mycolor,
            legend.show = T)
tmap_save(cropYields, filename = paste0(path_out, "cropYieldsPlotSupplements.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)


#### Droughts (SPEI index) ####
droughts <- raster("/DATA/SPEI/squared_spei_drought_1990-1999.tif")
droughts[1,1]
droughts[droughts == droughts[1,1]] <- NA
droughts[1,1]

droughts <- projectRaster(droughts, crs = "+proj=robin", over= T, method = "ngb")

mybreaks <- c(0,40,70,100,150,1000, 2000) #signif(classInt::classIntervals(values(droughts)[!is.na(values(droughts))],n=6, style = "fisher")$brks,1)
mycolor <- paletteer::paletteer_c("scico::lajolla", n=length(mybreaks), direction = 1)

droughtsPlot <- tm_shape(droughts, projection = "+proj=robin") +
  tm_raster("squared_spei_drought_1990.1999", 
            breaks=mybreaks, 
            palette = mycolor,
            legend.show = T)
tmap_save(droughtsPlot, filename = paste0(path_out, "/DroughtsSupplements.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)


#### Water risk ####
waterStress <- raster("/DATA/water_risk_reprojected/bws.tif ") 
waterStress <- projectRaster(waterStress, crs = "+proj=robin", over= T, method = "ngb")

# Physical stress quality (unimproved drinking water & unimproved sanitation)
dwStress <- raster("/DATA/water_risk_reprojected/udw.tif ") 
dwStress <- projectRaster(dwStress, crs = "+proj=robin", over= T, method = "ngb")

sanStress <- raster("/DATA/water_risk_reprojected/usa.tif ") 
sanStress <- projectRaster(sanStress, crs = "+proj=robin", over= T, method = "ngb")


# Individual plots
mybreaks <- seq(0,5,0.5)
mycolor <- paletteer::paletteer_c("scico::davos", n=length(mybreaks), direction = -1)
mycolor[1] <- "ivory"
mycolor

ws <- tm_shape(waterStress, projection = "+proj=robin") +
  tm_raster("layer", 
            breaks=mybreaks, 
            palette = mycolor,
            legend.show = T)

dw <- tm_shape(dwStress, projection = "+proj=robin") +
  tm_raster("layer", 
            breaks=mybreaks, 
            palette = mycolor,
            legend.show = T)

san <- tm_shape(sanStress, projection = "+proj=robin") +
  tm_raster("usa", 
            breaks=mybreaks, 
            palette = mycolor,
            legend.show = T)

waterRisk <- tmap_arrange(ws, dw, san, ncol = 2, nrow = 2)
tmap_save(waterRisk, filename = paste0(path_out, "/waterRiskSupplements.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)


# Combine to qualitative stress
qualStress <- sqrt(dwStress^2 + sanStress^2)
totStress <- sqrt(waterStress^2 + qualStress^2)
totStress

mybreaks <- round(classInt::classIntervals(values(totStress)[!is.na(values(totStress))],n=6, style = "fisher")$brks, 1)
mycolor <- paletteer::paletteer_c("scico::davos", n=length(mybreaks), direction = -1)

WRPlot <- tm_shape(totStress, projection = "+proj=robin") +
  tm_raster("layer", 
            breaks=mybreaks, 
            palette = mycolor,
            legend.show = T)
tmap_save(WRPlot, filename = paste0(path_out, "/TotWaterRiskSupplements.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)


#### Natural hazards ####

natHaz <- raster::stack("/DATA/Resilience_nc/natural_hazards.nc") %>% 
  subset(., 1: 11) %>% calc(., fun = mean, na.rm = T)
natHaz <- projectRaster(natHaz, crs = "+proj=robin", over= T, method = "ngb")


mybreaks <- c(0,0.1,0.2,0.4,0.6,0.8,0.9,1)
mycolor <- paletteer::paletteer_c("scico::hawaii", n=length(mybreaks), direction = -1)

natHazPlot <- tm_shape(natHaz, projection = "+proj=robin") +
  tm_raster("layer", 
            breaks=mybreaks, 
            palette = mycolor,
            legend.show = T)

tmap_save(natHazPlot, filename = paste0(path_out, "/natHazPlotSupplements.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)


#### AC variables ####
governance <- raster::stack("/DATA/Resilience_nc/governance.nc")
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

economy <- calc(economy, fun = mean, na.rm = T)
education <- calc(education, fun = mean, na.rm = T)
health <- calc(health,fun = mean, na.rm = T)
governance <- calc(governance, fun = mean, na.rm = T)

economy <- projectRaster(economy , crs = "+proj=robin", over= T, method = "ngb")
education <- projectRaster(education , crs = "+proj=robin", over= T, method = "ngb")
health <- projectRaster(health , crs = "+proj=robin", over= T, method = "ngb")
governance <- projectRaster(governance , crs = "+proj=robin", over= T, method = "ngb")

mybreaks <- c(0,0.1,0.2,0.4,0.6,0.8,0.9,1)

econPlot <- tm_shape(economy, projection = "+proj=robin") +
  tm_raster("layer", 
            breaks=mybreaks, 
            palette = paletteer::paletteer_c("scico::acton", n=length(mybreaks), direction = -1),
            legend.show = T)

eduPlot <- tm_shape(education, projection = "+proj=robin") +
  tm_raster("layer", 
            breaks=mybreaks, 
            palette = paletteer::paletteer_c("scico::buda", n=length(mybreaks), direction = -1),
            legend.show = T)

healthPlot <- tm_shape(health, projection = "+proj=robin") +
  tm_raster("layer", 
            breaks=mybreaks, 
            palette = paletteer::paletteer_c("scico::nuuk", n=length(mybreaks), direction = -1),
            legend.show = T)

govPlot <- tm_shape(governance, projection = "+proj=robin") +
  tm_raster("layer", 
            breaks=mybreaks, 
            palette = paletteer::paletteer_c("scico::tokyo", n=length(mybreaks), direction = -1),
            legend.show = T)

ACplots <- tmap_arrange(econPlot, eduPlot, healthPlot, govPlot, ncol = 2, nrow = 2)
tmap_save(ACplots, filename = paste0(path_out, "/ACplotsSupplements.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)

# Plot in a grid
allPlots <- tmap_arrange(foodprodPlot, droughtsPlot, WRPlot, natHazPlot, govPlot, eduPlot, healthPlot, econPlot,ncol = 2, nrow = 4)
tmap_save(allPlots, filename = paste0(path_out, "OrigVariablePlotsSupplements.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)


#### Supplements Table 2 ####

RF_out <- readr::read_csv( "/RF_permutation_importance_out_NM_cntries.csv", col_names = T) %>% 
  dplyr::rename(c("importance" = "."))

RF_in <- readr::read_csv( "/RF_importance_permutation_in_NM_cntries.csv", col_names = T) %>% 
  dplyr::rename(c("importance" = "."))

diagnosis_out <- readr::read_csv("/RF_permutation_diagnosis_out_NM_cntries.csv", col_names = T)
diagnosis_in <- readr::read_csv("/RF_diagnosis_permutation_in_NM_cntries.csv", col_names = T)


cntriesRegionsZones <- readr::read_csv("DATA/countriesRegionsZones.csv", col_names = T)
cntriesRegionsZones


#### Sum of pop in small countries ####

pop_raster <- raster("/DATA/1990AD_pop-1/popc_1990AD.asc")

cntry_raster <- raster("/DATA/cntry_raster_masked.tif")

cntriesRegionsZones <- readr::read_csv("DATA/countriesRegionsZones.csv", col_names = T) %>% dplyr::select(-zone)

noCellsPerCntry <- read_csv("DATA/migrationPredictorsCntryUpdate.csv") %>%
  group_by(zone) %>% 
  summarise(noCells = n()) %>% 
  left_join(., cntriesRegionsZones, by =c("zone" = "cntry_id")) %>% 
  dplyr::select(zone, country_name, noCells) %>% 
  distinct()

cntryPop <- raster::zonal(pop_raster, cntry_raster, fun = "sum", na.rm = T) %>% tibble::as.tibble()
sum(cntryPop$sum, na.rm=T)
totalPop <- cellStats(pop_raster, stat=sum, na.rm=T)

noCellsPerCntry <-  left_join(noCellsPerCntry, cntryPop, by =c("zone" = "zone"))

smallPop <- noCellsPerCntry %>% filter(noCells <= 20)

sum(smallPop$sum, na.rm = T)/totalPop

# Sample size table
diagnosis_out <- readr::read_csv("/RF_permutation_diagnosis_out_NM_cntriesUpdate.csv", col_names = T)
diagnosis_in <- readr::read_csv("/RF_diagnosis_permutation_in_NM_cntriesUpdate.csv", col_names = T)

cntriesRegionsZones <- readr::read_csv("/revised/countriesRegionsZones.csv", col_names = T)


myTable <- cntriesRegionsZones %>% dplyr::left_join(., diagnosis_out, by = c("cntry_id" = "cntries")) %>% 
  dplyr::select(-zone, -r2,-MSE) %>% unique() %>% 
  dplyr::rename( "sampleSizeOut" ="sampleSize") %>% 
  dplyr::left_join(., cntriesRegionsZones %>% 
                     dplyr::left_join(., diagnosis_in, by = c("cntry_id" = "cntries")) %>%
                     dplyr::select(-zone, -r2,-MSE) %>% unique() %>% 
                     dplyr::rename( "sampleSizeIn" ="sampleSize") %>% 
                     dplyr::select(cntry_id, sampleSizeIn),
                   by = c("cntry_id" = "cntry_id")) 

readr::write_csv(myTable, paste0(path_out, "/cntryGroupsTableSupplements.csv"), col_names = T)


##### Coefficient of variation #####

M_30arcsec <- raster("/DATA/30arcsec-net-migration-1990-2000-geotiff/30arcsec-net-migration-1990-2000.tif")


NM_30arcsec_abs <- abs(NM_30arcsec)

NM_5min_mean <- raster::aggregate(NM_30arcsec_abs, fact = 10, fun = mean, na.rm = T)
NM_5min_sd <- raster::aggregate(NM_30arcsec_abs, fact = 10, fun = sd, na.rm = T)

NM_5min_CV <- NM_5min_sd / NM_5min_mean

plot(NM_5min_CV)

writeRaster(NM_5min_CV, "/NM_CV_Update.tif")

# Project raster to robinson 
NM_5min_CV <- raster::projectRaster(NM_5min_CV, crs = "+proj=robin +lon_0=0 +x_0=-180 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

mybreaks <- seq(0,2,0.2)

cntries_sf <- read_sf("/DATA/cntryID.gpkg")


CVPlot <- tm_shape(NM_5min_CV) +
  tm_raster(palette = "-viridis", legend.show = T, style = "fixed", breaks = mybreaks) +
  tm_style("white",legend.show =  T,
           frame = F,
           bg.color = "NA",
           earth.boundary = T,
           n = 7,
           space.color = "white",
           panel.show = T,
           legend.is.portrait = F) +
  tm_shape(cntries_sf,projection = "robin") +
  tm_borders(col="black",alpha = 1, lwd = 0.2)+
  tm_layout(legend.bg.color = TRUE,
            legend.outside.position = "bottom",
            legend.outside = TRUE,
            frame = FALSE,
            earth.boundary = T,
            earth.boundary.lwd = NA,
            earth.boundary.color = "white",
            panel.show = F,)


tmap_save(CVPlot,  filename = paste0(path_out, "/CV_map.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)



