rm(list = ls())

setwd("/Users/nivav1/Documents/")

## Set path out for exporting data and figures (aalto disk)
path_out <- "/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/illustrations"

## Set path for packages (local disk)
.libPaths("/Users/nivav1/Documents/R/")

## Open packages
library(dplyr)
library(readr)
library(forcats)
library(tmap)
library(paletteer)
library(cowplot)
library(sf)
library(ggplot2)


#### Heatmaps: zonal stats ####
#### First create sf-polygons for later plotting ####
bipolygons <- function(limitx, limity, catx, caty) {
  
  lx <- length(limitx)
  ly <- length(limity)
  limitx <- c(limitx, 1)
  limity <- c(limity, 1)
  pols <- st_point(c(0,0)) %>%
    st_sfc() %>%
    st_sf(cat = "none")
  for(x in 0:lx) {
    for(y in 0:ly) {
      if(x == 0 && y == 0) {
        cat <- paste(catx[x+1], caty[y+1], sep='-')
        polygon <- data.frame(x = c(limitx[1],
                                    0,
                                    0,
                                    limitx[1],
                                    limitx[1]),
                              y = c(limity[1],
                                    limity[1],
                                    0,
                                    0,
                                    limity[1])) %>%
          as.matrix() %>%
          list() %>%
          st_polygon() %>%
          st_sfc() %>%
          st_sf(cat = cat)
        pols <- rbind(pols, polygon)
      } else if(x == 0) {
        cat <- paste(catx[x+1], caty[y+1], sep='-')
        polygon <- data.frame(x = c(limitx[1],
                                    0,
                                    0,
                                    limitx[1],
                                    limitx[1]),
                              y = c(limity[y+1],
                                    limity[y+1],
                                    limity[y],
                                    limity[y],
                                    limity[y+1])) %>%
          as.matrix() %>%
          list() %>%
          st_polygon() %>%
          st_sfc() %>%
          st_sf(cat = cat)
        pols <- rbind(pols, polygon)
      } else if(y == 0) {
        cat <- paste(catx[x+1], caty[y+1], sep='-')
        polygon <- data.frame(x = c(limitx[x+1],
                                    limitx[x],
                                    limitx[x],
                                    limitx[x+1],
                                    limitx[x+1]),
                              y = c(limity[1],
                                    limity[1],
                                    0,
                                    0,
                                    limity[1])) %>%
          as.matrix() %>%
          list() %>%
          st_polygon() %>%
          st_sfc() %>%
          st_sf(cat = cat)
        pols <- rbind(pols, polygon)
      } else {
        cat <- paste(catx[x+1], caty[y+1], sep='-')
        polygon <- data.frame(x = c(limitx[x+1],
                                    limitx[x],
                                    limitx[x],
                                    limitx[x+1],
                                    limitx[x+1]),
                              y = c(limity[y+1],
                                    limity[y+1],
                                    limity[y],
                                    limity[y],
                                    limity[y+1])) %>%
          as.matrix() %>%
          list() %>%
          st_polygon() %>%
          st_sfc() %>%
          st_sf(cat = cat)
        pols <- rbind(pols, polygon)
      }
      
      
    }
  }
  pols <- pols[-1,]
  return(pols)
}

# Assign breaks for classes
AC_breaks <- c(0,0.25,0.5,0.75,1)
ES_breaks <- c(0,0.25,0.5,0.75,1)

# Set limits and categories
limitx <- AC_breaks[2:c(length(AC_breaks)-1)]
limity <- ES_breaks[2:c(length(ES_breaks)-1)]
catx <- c("AC_low", "AC_med_low", "AC_med_high", "AC_high")
caty <- c("EV_low", "EV_med_low", "EV_med_high", "EV_high")

# Make grid
class_grid <- bipolygons(limitx, limity, catx, caty)

# Assign category IDs
class_grid$AC_id <- c(rep(c(AC_breaks[2:length(AC_breaks)]),each=length(catx)))
class_grid$ES_id <- c(rep(c(ES_breaks[2:length(ES_breaks)]), length(caty)))

# Order grid
class_grid <- arrange(class_grid, desc(ES_id))

# Assign ID for each grid
class_grid$grid_ID <- c(1:nrow(class_grid))

class_grid

#### Load data ####
classStats <- write_csv(classStats, "/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/resultsByClass.csv")

#### Plot share of net +/- migration in each class ####
mybreaks <-  unique(sort(round(c(classStats$shr_in,classStats$shr_out),2)))
mycolours <- paletteer_c("scico::turku", n=length(mybreaks), direction = -1)

# net-pos
mydata <- left_join(class_grid, classStats, by = c("grid_ID" = "zone")) %>% 
  dplyr::select(., AC_id, EV_id, shr_in) %>% 
  mutate(mybreaks = cut(shr_in, breaks = mybreaks, include.lowest = T))

heatmap_inNM <- tm_shape(mydata) +
  tm_polygons(col = "mybreaks",
              title = "Share of net-pos migr per class",
              breaks = mybreaks,
              palette = mycolours,
              colorNA = "grey90",
              lwd = NA) +
  #tm_shape(coastline) +
  #tm_borders() +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)

#net-neg
mydata <- left_join(class_grid, classStats, by = c("grid_ID" = "zone")) %>% 
  dplyr::select(., AC_id, EV_id, shr_out) %>% 
  mutate(mybreaks = cut(shr_out, breaks = mybreaks, include.lowest = T))

heatmap_outNM <- tm_shape(mydata) +
  tm_polygons(col = "mybreaks",
              title = "Share of net-neg migr per class",
              breaks = mybreaks,
              palette = mycolours,
              colorNA = "grey90",
              lwd = NA) +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)

heatmaps <- tmap_arrange(heatmap_outNM, heatmap_inNM, nrow = 1, ncol = 2)

tmap_save(heatmaps, filename = paste0(path_out, "/heatmapsNMratiosUpdate.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)


#### Share of net +/- migration per 1000 pop in each cluster ####
mybreaks <-  c(0, round(classInt::classIntervals(c(classStats$shrInNMper1000PopCluster,classStats$shrOutNMper1000PopCluster), style = "jenks")$brks,0))
mycolours <- paletteer_c("scico::bilbao", n=length(mybreaks), direction = 1)

# net-pos
mydata <- left_join(class_grid, classStats, by = c("grid_ID" = "zone")) %>% 
  dplyr::select(., AC_id, EV_id, shrInNMper1000PopCluster) %>% 
  mutate(mybreaks = cut(shrInNMper1000PopCluster, breaks = mybreaks, include.lowest = T))

heatmap_shrInNMper1000PopCluster <- tm_shape(mydata) +
  tm_polygons(col = "shrInNMper1000PopCluster",
              title = "Share of net-pos migr per 1000 pop per class",
              breaks = mybreaks,
              palette = mycolours,
              colorNA = "grey90",
              lwd = NA) +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)

#net-neg
mydata <- left_join(class_grid, classStats, by = c("grid_ID" = "zone")) %>% 
  dplyr::select(., AC_id, EV_id, shrOutNMper1000PopCluster) %>% 
  mutate(mybreaks = cut(shrOutNMper1000PopCluster, breaks = mybreaks, include.lowest = T))

heatmap_shrOutNMper1000PopCluster <- tm_shape(mydata) +
  tm_polygons(col = "shrOutNMper1000PopCluster",
              title = "Share of net-neg migr per 1000 pop per class",
              breaks = mybreaks,
              #labels = lbl_percap,
              palette = mycolours,
              colorNA = "grey90",
              lwd = NA) +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)

heatmaps <- tmap_arrange(heatmap_shrOutNMper1000PopCluster, heatmap_shrInNMper1000PopCluster, nrow = 1, ncol = 2)

tmap_save(heatmaps, filename = paste0(path_out, "/heatmapsNMper1000popUpdate.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)





#### Random forest / feature importance ####
#### Load data ####
cntries_sf <- read_sf("/Users/nivav1/Documents/DATA/cntryID.gpkg")

RF_out <- readr::read_csv( "/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/RF_permutation_importance_out_NM_cntriesUpdate.csv", col_names = T) %>% dplyr::rename(c("importance" = "."))
diagnosis_out <- readr::read_csv("/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/RF_permutation_diagnosis_out_NM_cntriesUpdate.csv", col_names = T)
myRanks_out <- readr::read_csv("/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/RFRanks_out.csv")

RF_in <- readr::read_csv( "/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/RF_importance_permutation_in_NM_cntriesUpdate.csv", col_names = T) %>% dplyr::rename(c("importance" = "."))
diagnosis_in <- readr::read_csv("/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/RF_diagnosis_permutation_in_NM_cntriesUpdate.csv", col_names = T)
myRanks_in <- readr::read_csv("/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/RFRanks_in.csv")

cntriesRegionsZones <- readr::read_csv("/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/countriesRegionsZones.csv", col_names = T)


#### Maps: Feature importance rankings ####
#### Net-negative areas ####
mydata <- myRanks_out %>% 
  dplyr::left_join(., cntries_sf, by = c( "cntryID" = "cntry_ID")) %>% st_as_sf()

# Add missing areas (NAs)
id <- sort(unique(cntries_sf$cntry_ID))
idShort <-  sort(unique(mydata$cntryID))
missingIds <- id[!(id %in% idShort)]

# Add variables to each missing id
missingCntries <- cntries_sf %>% filter(., cntry_ID %in% missingIds)

missingCntries <- missingCntries %>% mutate(predictor = "income") %>% bind_rows(missingCntries %>% mutate(predictor = "health")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "education")) %>% bind_rows(missingCntries %>% mutate(predictor = "governance")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "natHaz")) %>% bind_rows(missingCntries %>% mutate(predictor = "waterRisk")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "droughtRisk")) %>% bind_rows(missingCntries %>% mutate(predictor = "foodProdSca"))

missingCntries <- missingCntries %>% 
  mutate(my_ranks = NA) %>% 
  select(cntry_ID, my_ranks, predictor, geom) %>% 
  rename(., cntryID = cntry_ID)

mydata <- rbind(mydata %>% dplyr::select(cntryID, my_ranks, predictor, geom), 
                missingCntries)

# define colour palette
require(scico)
mycolours <- scico(n = length(unique(mydata$my_ranks)), begin = 0, end = 1, direction = 1, palette = "batlow")

RF_out_rank_maps <- tm_shape(mydata,
                             projection = "+proj=robin") +
  tm_polygons(col = "my_ranks",
              #breaks = c(0,1,2,3,4,5,6,7,8,9),
              title = "rank of feature importance",
              palette = mycolours,
              colorNA = "grey90",
              textNA = "no data",
              lwd = 0.1) +
  tm_facets(by = "predictor") +
  tm_layout(main.title = "Ranks of feature importances of explanatory variables on net-negative migration",
            legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)

RF_out_rank_maps

tmap_save(RF_out_rank_maps, filename = paste0(path_out, "/RF_rank_out_cntriesUpdate.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)


#### Net-positive areas ####
mydata <- myRanks_in %>% 
  dplyr::left_join(., cntries_sf, by = c( "cntryID" = "cntry_ID")) %>% st_as_sf()

# Add missing areas (NAs)
id <- sort(unique(cntries_sf$cntry_ID))
idShort <-  sort(unique(mydata$cntryID))
missingIds <- id[!(id %in% idShort)]

# Add variables to each missing id
missingCntries <- cntries_sf %>% filter(., cntry_ID %in% missingIds)

missingCntries <- missingCntries %>% mutate(predictor = "income") %>% 
  bind_rows(missingCntries %>% mutate(predictor = "health")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "education")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "governance")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "natHaz")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "waterRisk")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "droughtRisk")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "foodProdSca"))

missingCntries <- missingCntries %>% 
  mutate(my_ranks = NA) %>% 
  select(cntry_ID, my_ranks, predictor, geom) %>% 
  rename(., cntryID = cntry_ID)

mydata <- rbind(mydata %>% dplyr::select(cntryID, my_ranks, predictor, geom), 
                missingCntries)
mydata

# define colour palette
require(scico)
mycolours <- scico(n = length(unique(mydata$my_ranks))+1, begin = 0, end = 1, direction = 1, palette = "batlow")


RF_in_rank_maps <- tm_shape(mydata,
                            projection = "+proj=robin") +
  tm_polygons(col = "my_ranks",
              title = "rank of feature importance",
              palette = mycolours,
              colorNA = "grey90",
              textNA = "no data",
              lwd = 0.1) +
  tm_facets(by = "predictor") +
  tm_layout(main.title = "Ranks of feature importances of explanatory variables on net-positive migration",
            legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)

RF_in_rank_maps

tmap_save(RF_in_rank_maps, filename = paste0(path_out, "/RF_rank_in_cntriesUpdate.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)







#### Maps: Relative importance (permutation) of each variable in each model (=country) ####
# Relative importance = how good are the features in relation to the best feature
# For illustration purposes:
# First replace any negative importances with 0 in order to calculate relative importance
  # Negative importance is" returned when a random permutation of a feature's values results in a better performance metric"
  # No positive effect on the target variable

#### NET-NEGATIVE AREAS ####
# Calculate relative importance of each variable in each country
mydata <- RF_out %>% 
  left_join(., diagnosis_out, by = c("cntryID" = "cntries")) %>% 
  group_by(., cntryID) %>% 
  mutate(., importance  = ifelse(r2 < 0, NA, importance)) %>% # mask any values where r2 of the model < 0
  mutate(., importance = ifelse(importance < 0, 0, importance)) %>% # mask any values where importance is < 0
  mutate(., importanceRel = ifelse(is.na(importance), NA, importance / max(importance, na.rm = T))) %>% 
  mutate(., importanceRel  = ifelse(r2 < 0, -1, importanceRel)) # assign -1 to all values where r2 < 0

mybreaks <- c(-1, 0, 0.005, 0.01, 0.2,0.4,0.8,1)

## Set breaks for relative importances
mydata <- mydata %>% dplyr::ungroup() %>%  dplyr::mutate(., mybreaks = cut(x = mydata$importanceRel, breaks = mybreaks, include.lowest = T))
mydata <- mydata %>%  dplyr::select(-model) %>% select(cntryID, predictor, importance, r2, MSE, sampleSize, importanceRel, mybreaks)
mydata

# Join importances data to cntry sf by cntryID
mydata <- dplyr::left_join(mydata, cntries_sf, by = c( "cntryID" = "cntry_ID")) %>% st_as_sf()

# Add missing areas (NAs)
id <- sort(unique(cntries_sf$cntry_ID))
idShort <-  sort(unique(mydata$cntryID))
missingIds <- id[!(id %in% idShort)]

# Add variables to each missing id
missingCntries <- cntries_sf %>% filter(., cntry_ID %in% missingIds)
missingCntries <- missingCntries %>% mutate(predictor = "income") %>% 
  bind_rows(missingCntries %>% mutate(predictor = "health")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "education")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "governance")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "natHaz")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "waterRisk")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "droughtRisk")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "foodProdSca")) 

missingCntries <- missingCntries %>% mutate(mybreaks = NA, importanceRel = NA) %>% select(cntry_ID, predictor,importanceRel, mybreaks, geom) %>% rename(., cntryID = cntry_ID)

mydata <- rbind(mydata %>% dplyr::select(cntryID, predictor, importanceRel, mybreaks, geom), 
                missingCntries)

# MAP
# Create a color palette
mycolours <- paletteer::paletteer_c("scico::batlow", n=length(mybreaks), direction = -1)

RF_out_maps <- tm_shape(mydata,
                        projection = "+proj=robin") +
  tm_polygons(col = "importanceRel",
              title = "rel importance of  vars",
              breaks = mybreaks,
              #labels = lbl_percap,
              palette = mycolours,
              colorNA = "grey90",
              textNA = "no population",
              lwd = NA) +
  tm_facets(by = "predictor") +
  #tm_shape(coastline) +
  #tm_borders() +
  tm_layout(main.title = "Importances of explanatory variables on net-negative migration",
            legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)
RF_out_maps

tmap_save(RF_out_maps, filename = paste0(path_out, "/RF_rel_out_cntriesUpdate.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)


#### NET-POSITIVE AREAS ####
# Calculate breaks for importances

mydata <- RF_in %>% 
  left_join(., diagnosis_in, by = c("cntryID" = "cntries")) %>% 
  group_by(., cntryID) %>% 
  mutate(., importance  = ifelse(r2 < 0, NA, importance)) %>% # mask any values where r2 of the model < 0
  mutate(., importance = ifelse(importance < 0, 0, importance)) %>% # mask any values where importance is less than 0
  mutate(., importanceRel = ifelse(is.na(importance), NA, importance / max(importance, na.rm = T))) %>% 
  mutate(., importanceRel  = ifelse(r2 < 0, -1, importanceRel)) # assign -1 to all values where r2 < 0

## Set breaks for relative importances

mydata <- mydata %>% dplyr::ungroup() %>%  dplyr::mutate(., mybreaks = cut(x = mydata$importanceRel, breaks = mybreaks, include.lowest = T))
mydata <- mydata %>%  dplyr::select(-model) %>% select(cntryID, predictor, importance, r2, MSE, sampleSize, importanceRel, mybreaks)
mydata

# Join importances data to cntry sf by cntryID
mydata <- dplyr::left_join(mydata, cntries_sf, by = c( "cntryID" = "cntry_ID")) %>% st_as_sf()

# Add missing areas (NAs)
id <- sort(unique(cntries_sf$cntry_ID))
idShort <-  sort(unique(mydata$cntryID))
missingIds <- id[!(id %in% idShort)]

# Add variables to each missing id
missingCntries <- cntries_sf %>% filter(., cntry_ID %in% missingIds)
missingCntries <- missingCntries %>% mutate(predictor = "income") %>% 
  bind_rows(missingCntries %>% mutate(predictor = "health")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "education")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "governance")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "natHaz")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "waterRisk")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "droughtRisk")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "foodProdSca")) 

missingCntries <- missingCntries %>% mutate(mybreaks = NA, importanceRel = NA) %>% select(cntry_ID, predictor,importanceRel, mybreaks, geom) %>% rename(., cntryID = cntry_ID)

mydata <- rbind(mydata %>% dplyr::select(cntryID, predictor, importanceRel, mybreaks, geom), 
                missingCntries)
mydata

RF_in_maps <- tm_shape(mydata,
                       projection = "+proj=robin") +
  tm_polygons(col = "importanceRel",
              title = "rel importance on net-pos migr",
              breaks = mybreaks,
              #labels = lbl_percap,
              palette = mycolours,
              colorNA = "grey90",
              textNA = "no population",
              lwd = NA) +
  tm_facets(by = "predictor") +
  #tm_shape(coastline) +
  #tm_borders() +
  tm_layout(main.title = "Importances of explanatory variables on net-positive migration",
            legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)
RF_in_maps

tmap_save(RF_in_maps, filename = paste0(path_out, "/RF_rel_in_cntriesUpdate.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)





#### Maps: Absolute importance (permutation) of each variable in each model (=country) ####
# Check range
myvec <- rbind(RF_in %>% 
                 left_join(., diagnosis_in, by = c("cntryID" = "cntries")) %>%
                 filter(., r2 >= 0),
               RF_out %>% 
                 left_join(., diagnosis_in, by = c("cntryID" = "cntries")) %>%
                 filter(., r2 >= 0)) %>% 
  dplyr::select(importance) %>% 
  drop_na() %>% 
  mutate(importance = ifelse(importance < 0, 0, importance))

range(myvec, na.rm = T)
hist(as.matrix(myvec))
mybreaks <- c(-1, 0, 10^-6,10^-5, 10^-4,10^-3,10^-2,10^-1,1,10,100,1000)

#### NET-NEGATIVE AREAS ####
mydata <- RF_out %>% 
  left_join(., diagnosis_out, by = c("cntryID" = "cntries")) %>% 
  #left_join(., cntriesRegionsZones, by = c("cntryID" = "cntry_id")) %>% 
  group_by(., cntryID) %>% 
  mutate(., importance  = ifelse(r2 < 0, NA, importance)) %>% # mask any values where r2 of the model < 0
  mutate(., importance = ifelse(importance < 0, 0, importance)) %>%  # mask any values where importance < 0
  mutate(., importance  = ifelse(r2 < 0, -1, importance)) # assign -1 to all values where r2 < 0

# Define breaks
mydata <- mydata %>% dplyr::ungroup() %>%  dplyr::mutate(., mybreaks = cut(x = mydata$importance, breaks = mybreaks, include.lowest = T))

# Join importances data to cntry df by cntryID
mydata <- dplyr::left_join(mydata, cntries_sf, by = c( "cntryID" = "cntry_ID")) %>% st_as_sf()

# Add missing areas (NAs)
id <- sort(unique(cntries_sf$cntry_ID))
idShort <-  sort(unique(mydata$cntryID))
missingIds <- id[!(id %in% idShort)]

# Add variables to each missing id
missingCntries <- cntries_sf %>% filter(., cntry_ID %in% missingIds)
missingCntries <- missingCntries %>% mutate(predictor = "income") %>% 
  bind_rows(missingCntries %>% mutate(predictor = "health")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "education")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "governance")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "natHaz")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "waterRisk")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "droughtRisk")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "foodProdSca"))


missingCntries <- missingCntries %>% mutate(mybreaks = NA, importance = NA) %>% select(cntry_ID, importance, predictor, mybreaks, geom) %>% rename(., cntryID = cntry_ID)

mydata <- rbind(mydata %>% dplyr::select(cntryID, importance, predictor, mybreaks, geom), 
                missingCntries)
mydata

# define colour palette
mycolours <- paletteer::paletteer_c("scico::batlow", length(mybreaks), direction = -1)

RF_out_maps <- tm_shape(mydata,
                        projection = "+proj=robin") +
  tm_polygons(col = "importance",
              title = "feature importance",
              breaks = mybreaks,
              palette = mycolours,
              colorNA = "grey90",
              textNA = "no data/R2<0",
              lwd = NA) +
  tm_facets(by = "predictor") +
  tm_layout(main.title = "Absolute importances of explanatory variables on net-negative migration",
            legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)
RF_out_maps

tmap_save(RF_out_maps, filename = paste0(path_out, "/RF_abs_out_cntriesUpdate.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)

#### NET-POSITIVE AREAS ####
# Calculate breaks for importances

mydata <- RF_in %>% 
  left_join(., diagnosis_in, by = c("cntryID" = "cntries")) %>% 
  group_by(., cntryID) %>% 
  mutate(., importance  = ifelse(r2 < 0, NA, importance)) %>% # mask any values where r2 of the model < 0
  mutate(., importance = ifelse(importance < 0, 0, importance)) %>% # mask any values where importance < 0
  mutate(., importance  = ifelse(r2 < 0, -1, importance))

# Define breaks
mydata <- mydata %>% dplyr::ungroup() %>%  dplyr::mutate(., mybreaks = cut(x = mydata$importance, breaks = mybreaks, include.lowest = T))

# Join importances data to cntry df by cntryID
mydata <- dplyr::left_join(mydata, cntries_sf, by = c( "cntryID" = "cntry_ID")) %>% st_as_sf()

# Add missing areas (NAs)
id <- sort(unique(cntries_sf$cntry_ID))
idShort <-  sort(unique(mydata$cntryID))
missingIds <- id[!(id %in% idShort)]

# Add variables to each missing id
missingCntries <- cntries_sf %>% filter(., cntry_ID %in% missingIds)
missingCntries <- missingCntries %>% mutate(predictor = "income") %>% 
  bind_rows(missingCntries %>% mutate(predictor = "health")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "education")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "governance")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "natHaz")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "waterRisk")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "droughtRisk")) %>% 
  bind_rows(missingCntries %>% mutate(predictor = "foodProdSca"))


missingCntries <- missingCntries %>% mutate(mybreaks = NA, importance = NA) %>% select(cntry_ID, importance, predictor, mybreaks, geom) %>% rename(., cntryID = cntry_ID)
mydata <- rbind(mydata %>% dplyr::select(cntryID, importance, predictor, mybreaks, geom), missingCntries)


# define colour palette
mycolours <- paletteer::paletteer_c("scico::batlow", n=length(mybreaks), direction = -1)

RF_in_maps <- tm_shape(mydata,
                       projection = "+proj=robin") +
  tm_polygons(col = "importance",
              title = "feature importance",
              breaks = mybreaks,
              palette = mycolours,
              colorNA = "grey90",
              textNA = "no data/R2<0",
              lwd = NA) +
  tm_facets(by = "predictor") +
  tm_layout(main.title = "Absolute importances of explanatory variables on net-positive migration",
            legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)
RF_in_maps

tmap_save(RF_in_maps, filename = paste0(path_out, "/RF_abs_in_cntriesUpdate.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)








#### Maps: R-SQUARED ####
# Calculate common break for net-neg and net-pos areas
mydata <- rbind(diagnosis_out, diagnosis_in)
range(mydata$r2, na.rm = T)
mybreaks <- c(-1,-0.5,seq(0,1,0.1))

# Create a color palette
mycolours <- paletteer::paletteer_c("scico::hawaii", n=length(mybreaks), direction = -1)

# Set category for facet wrap
diagnosis_out$cat <- "out"
diagnosis_in$cat <- "in"
mydata <- rbind(diagnosis_out, diagnosis_in)

# Set breaks for r2
mydata <- dplyr::mutate(mydata, mybreaks = cut(mydata$r2, breaks = mybreaks, include.lowest = T))
mydata

id <- sort(unique(cntries_sf$cntry_ID))
idShort <-  sort(unique(mydata$cntries))
missingIds <- id[!(id %in% idShort)]

# Add variables to each missing id
missingCntries <- cntries_sf %>% filter(., cntry_ID %in% missingIds)
missingCntries <- missingCntries %>% mutate(cat = "in") %>% 
  bind_rows(missingCntries %>% mutate(cat = "out"))

missingCntries <- missingCntries %>% mutate(mybreaks = NA) %>% select(cntry_ID, mybreaks, cat, geom) %>% rename(c(cntries = cntry_ID))

# Join data to cntry df by cntryID for mapping
mydata <- dplyr::left_join(mydata, cntries_sf, by = c( "cntries" = "cntry_ID")) %>% st_as_sf()
mydata <- rbind(dplyr::select(mydata, cntries, mybreaks, cat, geom), missingCntries)
mydata

Rsquared <- tm_shape(mydata,
                     projection = "+proj=robin") +
  tm_polygons(col = "mybreaks",
              title = "R2",
              breaks = mybreaks,
              #labels = lbl_percap,
              palette = mycolours,
              colorNA = "grey90",
              textNA = "no population",
              lwd = NA) +
  tm_facets(by = "cat") +
  tm_layout(main.title = "Proportion of the variance in net-negative and net-positive migration explained by explanatory variables within each country",
            legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)
Rsquared

tmap_save(Rsquared, filename = paste0(path_out, "/RF_R2_Update.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)

#### Maps: MSE ####
mydata <- rbind(diagnosis_out, diagnosis_in)
max(mydata$MSE, na.rm =T)
min(mydata$MSE, na.rm =T)
lseq <- function(from=0.000001, to=80000, length.out=8) {
  exp(seq(log(from), log(to), length.out = length.out))
}

mybreaks <- lseq() 

mybreaks / 10^-3

# Set category for facet wrap
diagnosis_out$cat <- "out"
diagnosis_in$cat <- "in"
mydata <- rbind(diagnosis_out, diagnosis_in)

# Set breaks for r2
mydata <- dplyr::mutate(mydata, mybreaks = cut(mydata$MSE, breaks = mybreaks, include.lowest = T))
mydata

id <- sort(unique(cntries_sf$cntry_ID))
idShort <-  sort(unique(mydata$cntries))
missingIds <- id[!(id %in% idShort)]

# Add variables to each missing id
missingCntries <- cntries_sf %>% filter(., cntry_ID %in% missingIds)
missingCntries <- missingCntries %>% mutate(cat = "in") %>% 
  bind_rows(missingCntries %>% mutate(cat = "out"))

missingCntries <- missingCntries %>% mutate(mybreaks = NA) %>% select(cntry_ID, mybreaks, cat, geom) %>% rename(c(cntries = cntry_ID))

# Join data to cntry df by cntryID for mapping
mydata <- dplyr::left_join(mydata, cntries_sf, by = c( "cntries" = "cntry_ID")) %>% st_as_sf()
mydata <- rbind(dplyr::select(mydata, cntries, mybreaks, cat, geom), missingCntries)
mydata

# Create a color palette
mycolours <- paletteer::paletteer_c("scico::hawaii", n=length(mybreaks), direction = -1)


MSE <- tm_shape(mydata,
                projection = "+proj=robin") +
  tm_polygons(col = "mybreaks",
              title = "MSE",
              breaks = mybreaks,
              #labels = lbl_percap,
              palette = mycolours,
              colorNA = "grey90",
              textNA = "no population",
              lwd = NA) +
  tm_facets(by = "cat") +
  tm_layout(main.title = "Overall out of bag prediction error of each model",
            legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE)
MSE
tmap_save(MSE, filename = paste0(path_out, "/RF_MSE_Update.pdf"), height = 297, width = 210, units = c("mm"), dpi = 300)


#### Boxplot: The distributions of relative feature importances by country groups ####
# Define colours

mycolours <- c("#540b0e","#9e2a2b","#e09f3e", "#fff3b0", "#f1faee", "#a8dadc", "#457b9d","#1d3557")

# Join regions to RF results
# Calculate relative importance of each variable in each country

## Net-negative areas
box_out <- RF_out %>% 
  left_join(., diagnosis_out, by = c("cntryID" = "cntries")) %>% 
  left_join(., cntriesRegionsZones, by = c("cntryID" = "cntry_id")) %>% 
  group_by(., cntryID) %>% 
  mutate(., importance  = ifelse(r2 < 0, NA, importance)) %>% # mask any values where r2 of the model < 0
  mutate(., importance = ifelse(importance < 0, 0, importance)) %>% # mask any values where importance is less than 0
  mutate(., importanceRel = ifelse(is.na(importance), NA, importance / max(importance, na.rm = T))) %>%
  mutate(order = factor(predictor, levels = unique(mydata$predictor))) %>%
  ggplot2::ggplot(ggplot2::aes(x = order, y = importanceRel, fill = order)) + 
  ggplot2::stat_boxplot(geom = "errorbar", size = 0.3) +
  ggplot2::geom_boxplot(outlier.size = 0.75, lwd=0.3) +
  ggplot2::stat_summary(fun = median,
                        geom = "point",
                        size = 0.75,
                        color = "grey") +
  ggplot2::ylim(c(0,1))+
  ggplot2::scale_fill_manual(values = mycolours)+
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9, angle = 0, hjust = 1))+ 
  ggplot2::coord_flip()+
  ggplot2::labs(title="Distribution of relative variable importances (out-migration)") + 
  ggplot2::facet_wrap(~regionName)

## Net-positive areas
box_in <- RF_in %>% 
  left_join(., diagnosis_in, by = c("cntryID" = "cntries")) %>% 
  left_join(., cntriesRegionsZones, by = c("cntryID" = "cntry_id")) %>% 
  group_by(., cntryID) %>% 
  mutate(., importance  = ifelse(r2 < 0, NA, importance)) %>% # mask any values where r2 of the model < 0
  mutate(., importance = ifelse(importance < 0, 0, importance)) %>% # mask any values where importance is less than 0
  mutate(., importanceRel = ifelse(is.na(importance), NA, importance / max(importance, na.rm = T))) %>% 
  mutate(order = factor(predictor, levels = unique(mydata$predictor))) %>%
  ggplot2::ggplot(ggplot2::aes(x = order, y = importanceRel, fill = order)) + 
  ggplot2::stat_boxplot(geom = "errorbar", size = 0.3) +
  ggplot2::geom_boxplot(outlier.size = 0.75, lwd=0.3) +
  ggplot2::stat_summary(fun = median,
                        geom = "point",
                        size = 0.75,
                        color = "grey") +
  ggplot2::ylim(c(0,1))+
  ggplot2::scale_fill_manual(values = mycolours)+
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9, angle = 0, hjust = 1))+ 
  ggplot2::coord_flip()+
  ggplot2::labs(title="Distribution of relative variable importances (in-migration)") + 
  ggplot2::facet_wrap(~regionName)

box_in

cowplot::plot_grid(box_out, box_in, labels = "AUTO", label_size = 8, ncol=1, nrow = 2)

cowplot::ggsave2(path = path_out, "RF_regions_boxplotsUpdate.pdf", width =40,
                 height = 60,
                 units = c( "cm"),
                 dpi = 330)
