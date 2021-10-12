
rm(list = ls())

setwd("/Users/nivav1/Documents/")

path_out <- "/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/"

## Set path for packages (local disk)
.libPaths("/Users/nivav1/Documents/R/")

## Open packages
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(raster)
library(ranger)

#### Load data ####
cntry_raster <- raster("/Users/nivav1/Documents/DATA/cntry_raster_masked.tif")
predictorStack <- raster::stack("/Users/nivav1/Documents/DATA/MigrationRespVars.tif")
NM_stack <- raster::stack("/Users/nivav1/Documents/DATA/NM_stack.tif")

# Sort and remove NAs from zones vector
cntry <- sort(unique(values(cntry_raster)))
#zones <- sort(unique(values(zone_raster))[-1])

#### Extract observations in each county ####
#### Explanatory variables ####
# Initiate a list for values in each country
cntry_predictors <- list()
for(z in cntry) {
  
  r_index <- values(cntry_raster) == z
  r_index[is.na(r_index)] <- FALSE
  
  if(all(!r_index)) {
    next 
  } else {
    
    predictors <- dplyr::as_tibble(values(predictorStack)[r_index,]) %>% 
      tibble::add_column(zone = z, .before = 1)
    
    cntry_predictors[[as.character(z)]] <- predictors
  }
}

save(cntry_predictors, file = "cntry_data.RData")

cntry_predictors_df <- do.call("rbind", cntry_predictors)

# find dataframes with less than 9 columns
drop_cntries <- lapply(cntry_predictors, function(x) ncol(x)) %>% 
  as_tibble() %>% 
  pivot_longer("1":"940", names_to = "cntry", values_to = "ncols" ) %>% 
  dplyr::filter(., ncols < 9) %>% 
  dplyr::select(., cntry) %>% 
  as.vector()

# assign null to countries to be dropped
temp <- cntry_predictors
temp[drop_cntries$cntry] <- NULL

lapply(temp, function(x) ncol(x)) %>% 
  as_tibble() %>% 
  pivot_longer("1":"940", names_to = "cntry", values_to = "ncols" ) %>% 
  dplyr::filter(., ncols < 9) %>% dplyr::select(., cntry) %>% as.vector()


cntry_predictors_df <- do.call("rbind", temp)
# save
library(readr)
write_csv(cntry_predictors_df, paste0(path_out,"migrationPredictorsCntry.csv"))


#### Response variables ####

# Initiate a list for values in each country
cntry_netmigr <- list()
for(z in cntry) {
  
  r_index <- values(cntry_raster) == z
  r_index[is.na(r_index)] <- FALSE
  
  if(all(!r_index)) {
    next 
  } else {
    
    netmigr <- dplyr::as_tibble(values(NM_stack)[r_index,]) %>% 
      tibble::add_column(zone = z, .before = 1)
    
    cntry_netmigr[[as.character(z)]] <- netmigr
  }
}

save(cntry_netmigr, file = "cntry_netmigr.RData")

cntry_netmigr_df <- plyr::ldply(cntry_netmigr, data.frame)

# find dataframes with less than 9 columns
drop_cntries <- lapply(cntry_netmigr, function(x) ncol(x)) %>% 
  as_tibble() %>% 
  pivot_longer("1":"940", names_to = "cntry", values_to = "ncols" ) %>% 
  dplyr::filter(., ncols < 5) %>% 
  dplyr::select(., cntry) %>% 
  as.vector()

# assign null to countries to be dropped
temp <- cntry_netmigr
temp[drop_cntries$cntry] <- NULL

lapply(temp, function(x) ncol(x)) %>% 
  as_tibble() %>% 
  pivot_longer("1":"940", names_to = "cntry", values_to = "ncols" ) %>% 
  dplyr::filter(., ncols < 5) %>% dplyr::select(., cntry) %>% as.vector()


cntry_netmigr_df <- do.call("rbind", temp)

# save
library(readr)
write_csv(cntry_netmigr_df, paste0(path_out,"migrationCntryUpdate.csv"))



#### Random forest ####
#### Load data ####
netMigr <- read_csv("/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/migrationCntryUpdate.csv") %>% select(-value, -.id)
predictors <- read_csv("/Users/nivav1/Documents/Analyses/analysis_msII/results/revised/migrationPredictorsCntryUpdate.csv")
data <- cbind(netMigr, predictors)
# Remove duplicate columns
data <-  data[,!duplicated(names(data))] %>% as_tibble(.)
data <- dplyr::select(data, zone, NM_in, NM_in_ppop,  NM_out, NM_out_ppop, income, health, education, governance, natHaz, waterRisk, droughtRisk, foodProdSca)
data <- arrange(data, zone)

#### Net-negative migration ####

# First create dummy data from sample
dummy_data <- data %>% dplyr::select(-NM_out, -NM_in_ppop, -NM_in) %>% 
  arrange(., zone) %>% 
  group_by(zone) %>% 
  drop_na()

# split data into lists based on a group.
dummy_list_out <- dummy_data %>% group_by(zone) %>% group_split()

### Ranger ###
set.seed(12345678)
fit_models_out_ranger <- lapply(dummy_list_out, function(x){
  ranger(NM_out_ppop ~. , data = x[2:10], importance = "permutation") 
})

# Extrac MSEs and R squared
cntries <- unique(dummy_data$zone)
r2 <- sapply(fit_models_out_ranger, function(x) { x$r.squared }) #Computed on out of bag data.
MSE <- sapply(fit_models_out_ranger, function(x) { x$prediction.error})
sampleSize <- sapply(fit_models_out_ranger, function(x) { x$num.samples})
diagnosis_out <- data.frame(cntries, r2, MSE,sampleSize)

## Extract importances
model_importances_out_ranger <- lapply(fit_models_out_ranger, ranger::importance)
# Convert to table
table_importance_out_ranger <- lapply(model_importances_out_ranger, function(x){
  out <- x %>% as.data.frame() %>% 
    tibble::rownames_to_column("predictor") %>% as_tibble() 
  return(out)}) %>% 
  purrr::map_df(., ~as_tibble(.x), .id="model") 

# Convert model/country ID to numeric
table_importance_out_ranger <-  mutate(table_importance_out_ranger, model = as.numeric(model))

# Combine model ids and cntry ids
modelID <- unique(table_importance_out_ranger$model)
ids <- data.frame(modelID, cntries) %>% dplyr::as_tibble()
names(ids) <- c("modelID", "cntryID")
ids

table_importance_out_ranger <- left_join(table_importance_out_ranger, ids, by = c("model" = "modelID"))

write_csv(table_importance_out_ranger, paste0(path_out, "RF_permutation_importance_out_NM_cntriesUpdate.csv"))
write_csv(diagnosis_out, paste0(path_out, "RF_permutation_diagnosis_out_NM_cntriesUpdate.csv"))


#### Net-positive migration ####

set.seed(123)

# First create dummy data from sample
dummy_data <- data %>% dplyr::select(-NM_in, -NM_out, -NM_out_ppop) %>% 
  arrange(., zone) %>% 
  drop_na() 

# Split data into lists based on a group
dummy_list_in <- dummy_data %>% 
  group_by(zone) %>% 
  group_split()

### Ranger ###
set.seed(12345678)
fit_models_in_ranger <- lapply(dummy_list_in, function(x){
  ranger(NM_in_ppop ~. , data = x[2:10], importance = "permutation") 
})

# Extrac MSEs and R squared
cntries <- unique(dummy_data$zone) 
r2 <- sapply(fit_models_in_ranger, function(x) { x$r.squared })
MSE <- sapply(fit_models_in_ranger, function(x) { x$prediction.error})
sampleSize <- sapply(fit_models_in_ranger, function(x) { x$num.samples})
diagnosis_in <- data.frame(cntries, r2, MSE,sampleSize)

# Extract importances
model_importances_in_ranger <- lapply(fit_models_in_ranger, ranger::importance)
# Convert to table
table_importance_in_ranger <- lapply(model_importances_in_ranger, function(x){
  
  out <- x %>% as.data.frame() %>% 
    tibble::rownames_to_column("predictor") %>% as_tibble() 
  return(out)}) %>% 
  purrr::map_df(., ~as_tibble(.x), .id="model") 

table_importance_in_ranger <- mutate(table_importance_in_ranger, model = as.numeric(model))

# Combine model ids and cntry ids
modelID <- unique(table_importance_in_ranger$model)
ids <- data.frame(modelID, cntries) %>% dplyr::as_tibble()
names(ids) <- c("modelID", "cntryID")
ids

table_importance_in_ranger <- left_join(table_importance_in_ranger, ids, by = c("model" = "modelID"))

write_csv(table_importance_in_ranger, paste0(path_out, "RF_importance_permutation_in_NM_cntriesUpdate.csv"))
write_csv(diagnosis_in, paste0(path_out, "RF_diagnosis_permutation_in_NM_cntriesUpdate.csv"))



#### Rank features ####
#### Net-negative areas ####
myRanks_out <- RF_out %>%  
  left_join(., diagnosis_out, by = c("cntryID" = "cntries")) %>% 
  group_by(cntryID) %>%
  mutate(my_ranks = order(order(importance, decreasing=TRUE))) %>% 
  mutate(firstPlace = ifelse(my_ranks == 1, 1, 0)) %>% 
  mutate(my_ranks  = ifelse(r2<0,9, my_ranks)) %>% 
  left_join(., cntriesRegionsZones, by = c("cntryID" = "cntry_id")) %>% 
  select(-zone) %>% 
  distinct() %>% 
  mutate(my_ranks = as.factor(my_ranks))

write_csv(myRanks_out, paste0(path_out, "RFRanks_out.csv"))


globalRank_out <- myRanks_out %>% 
  group_by(predictor) %>% 
  summarise(., rank = sum(firstPlace)/n(),
            sumRanks = sum(my_ranks, na.rm = T)) %>% 
  arrange(desc(rank)) %>% 
  mutate(rank = round(rank, 2)) %>% 
  mutate(regionName = "Global") %>% 
  select(regionName, predictor, rank, sumRanks)

#### Net-positive areas ####
myRanks_in <- RF_in %>%  
  left_join(., diagnosis_in, by = c("cntryID" = "cntries")) %>% 
  group_by(cntryID) %>%
  mutate(my_ranks = order(order(importance, decreasing=TRUE))) %>% 
  mutate(firstPlace = ifelse(my_ranks == 1, 1, 0)) %>% 
  mutate(my_ranks  = ifelse(r2<0,9, my_ranks)) %>% 
  left_join(., cntriesRegionsZones, by = c("cntryID" = "cntry_id")) %>% 
  select(-zone) %>% 
  distinct() %>% 
  mutate(my_ranks = as.factor(my_ranks))

write_csv(myRanks_in, paste0(path_out, "RFRanks_in.csv"))

globalRank_in <- myRanks_in %>% 
  group_by(predictor) %>% 
  summarise(., rank = sum(firstPlace)/n(),
            sumRanks = sum(my_ranks, na.rm = T)) %>% 
  arrange(desc(rank)) %>% 
  mutate(rank = round(rank, 2)) %>% 
  mutate(regionName = "Global") %>% 
  select(regionName, predictor, rank, sumRanks)

#### Global medians of feature importance ####

mediansOut <- myRanks_out %>% 
  group_by(cntryID) %>% 
  mutate(importance  = ifelse(r2<0,NA, importance)) %>% 
  mutate(., importanceRel = ifelse(is.na(importance), NA, importance / max(importance, na.rm = T))) %>% 
  group_by(predictor) %>% 
  summarise(medianVIMPNetNeg = median(importance, na.rm =T),
            medianRelVIMPNetNeg = median(importanceRel, na.rm =T)) %>% 
  arrange(desc(medianRelVIMPNetNeg))


mediansIn <- myRanks_in %>% 
  group_by(cntryID) %>% 
  mutate(importance  = ifelse(r2<0,NA, importance)) %>% 
  mutate(., importanceRel = ifelse(is.na(importance), NA, importance / max(importance, na.rm = T))) %>% 
  group_by(predictor) %>% 
  summarise(medianVIMPNetPos = median(importance, na.rm =T),
            medianRelVIMPNetPos = median(importanceRel, na.rm =T)) %>% 
  arrange(desc(medianRelVIMPNetPos))

