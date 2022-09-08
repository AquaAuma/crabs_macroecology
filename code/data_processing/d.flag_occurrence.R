################################################################################
#### Flag OBIS and GBIF separately and add similar column names
#### Uses homogenized occurrence taxonomy from <3.get_occurrence_data.R>
#### Coding: Aurore Maureaud, June 2021
################################################################################

rm(list = ls())

# set date
date <- '9JUN2021'

# Load libraries
library(readr)
library(tidyverse)
library(rfishbase)
library(data.table)

# Load relevant functions from git repo
source("functions/clean_dates.R")
source("functions/clean_countries.R")
source("functions/clean_geolocation.R")
source("functions/clean_realm.R")


##############################################################################################################
##############################################################################################################
#### 1. BRACHYURANS
##############################################################################################################
##############################################################################################################

### A. Load datasets
load(paste0("outputs_MOL/real/occ/BRACHYURANS_OCC_GBIF_TAXOCLEAN_7JUN2021.RData"))
load(paste0("outputs_MOL/real/occ/BRACHYURANS_OCC_OBIS_TAXOCLEAN_7JUN2021.RData"))

# GBIF
# columns to watch out for: 
# lat, long, coordinate uncertainty, year, month, day, eventdate, depth, depth accuracy,
# basisofrecord, issue, has coordinate, has geospatialissues, countrycode, locality
# state province, occurrenceStatus


### B. Subset taxa of interest = already done

### C. Correct date
gbif_raw7 <- clean_dates(gbif_raw6, input_source = "GBIF")

### D. Correct country
gbif_raw7 <- clean_countries(gbif_raw7, input_source = "GBIF")

### E. Correct coordinates
gbif_raw7 <- clean_geolocation(gbif_raw7, input_source = "GBIF")

### F. Correct realm
gbif_raw7 <- clean_realm(gbif_raw7, input_source = "GBIF")

### G. Correct format
gbif_raw8 <- gbif_raw7 %>% 
  rename(latitude = decimalLatitude,
         longitude = decimalLongitude,
         coordinate_error = coordinateUncertaintyInMeters,
         depth_error = depthAccuracy,
         collection_code = collectionCode,
         catalog_number = catalogNumber,
         institution_code = institutionCode,         
         dataset_name = datasetName,
         dataset_id = datasetKey,
         aphia_id = valid_AphiaID,
         taxonomic_status = taxonomicStatus,
         taxon_rank = taxonRank
  ) %>% 
  mutate(source = "GBIF") %>% 
  select(year, month, day, latitude, longitude, coordinate_error, depth, depth_error,
         institution_code, collection_code, catalog_number, dataset_name, dataset_id,
         country_code, 
         accepted_name, aphia_id, class, order, infraorder, family, genus, subgenus, 
         species, subspecies, taxonomic_status, taxon_rank, type, authorship, 
         source, id, 
         flag_year, flag_month, flag_day, flag_coordinate, flag_outlier, 
         flag_coord_uncertainty, flag_depth, flag_realm)

save(gbif_raw8, file = paste0("outputs_MOL/real/occ/BRACHYURANS_OCC_GBIF_TAXOCLEAN_FLAGGED",date,".RData"))

# OBIS
# columns to watch out for: 
# lat, long, flags, absence, record_type, basisofrecord, occurrenceremarks,
# occurrence status, countrycode

### B. Subset taxa of interest = already done

### C. Correct date
obis_raw7 <- clean_dates(obis_raw6, input_source = "OBIS")

### D. Correct country
obis_raw7 <- clean_countries(obis_raw7, input_source = "OBIS")

### E. Correct coordinates
obis_raw7 <- clean_geolocation(obis_raw7, input_source = "OBIS")

### F. Correct realm
obis_raw7 <- clean_realm(obis_raw7, input_source = "OBIS")

### G. Correct format
obis_raw8 <- obis_raw7 %>% 
  rename(latitude = decimallatitude,
         longitude = decimallongitude,
         coordinate_error = coordinateuncertaintyinmeters,
         depth = verbatimdepth,
         collection_code = collectioncode,
         catalog_number = catalognumber,
         institution_code = institutioncode,
         dataset_name = datasetname,
         aphia_id = valid_AphiaID,
         taxonomic_status = taxonomicStatus,
         taxon_rank = taxonRank
  ) %>% 
  mutate(source = "OBIS",
         depth_error = maximumdepthinmeters - minimumdepthinmeters) %>% 
  select(year, month, day, latitude, longitude, coordinate_error, depth, depth_error,
         institution_code, collection_code, catalog_number, dataset_name, dataset_id,
         country_code, 
         accepted_name, aphia_id, class, order, infraorder, family, genus, subgenus, 
         species, subspecies, taxonomic_status, taxon_rank, type, authorship, 
         source, id, 
         flag_year, flag_month, flag_day, flag_coordinate, flag_outlier, 
         flag_coord_uncertainty, flag_depth, flag_realm)

save(obis_raw8, file = paste0("outputs_MOL/real/occ/BRACHYURANS_OCC_OBIS_TAXOCLEAN_FLAGGED",date,".RData"))


##############################################################################################################
##############################################################################################################
#### 2. ANOMURANS
##############################################################################################################
##############################################################################################################

rm(list = ls())

# set date
date <- '9JUN2021'

# Load relevant functions from git repo
source("functions/clean_dates.R")
source("functions/clean_countries.R")
source("functions/clean_geolocation.R")
source("functions/clean_realm.R")

### A. Load datasets
load(paste0("outputs_MOL/wannabe/occ/ANOMURANS_OCC_GBIF_TAXOCLEAN_7JUN2021.RData"))
load(paste0("outputs_MOL/wannabe/occ/ANOMURANS_OCC_OBIS_TAXOCLEAN_7JUN2021.RData"))

# GBIF
# columns to watch out for: 
# lat, long, coordinate uncertainty, year, month, day, eventdate, depth, depth accuracy,
# basisofrecord, issue, has coordinate, has geospatialissues, countrycode, locality
# state province, occurrenceStatus


### B. Subset taxa of interest = already done

### C. Correct date
gbif_raw7 <- clean_dates(gbif_raw6, input_source = "GBIF")

### D. Correct country
gbif_raw7 <- clean_countries(gbif_raw7, input_source = "GBIF")

### E. Correct coordinates
gbif_raw7 <- clean_geolocation(gbif_raw7, input_source = "GBIF")

### F. Correct realm
gbif_raw7 <- clean_realm(gbif_raw7, input_source = "GBIF")

### G. Correct format
gbif_raw8 <- gbif_raw7 %>% 
  rename(latitude = decimalLatitude,
         longitude = decimalLongitude,
         coordinate_error = coordinateUncertaintyInMeters,
         depth_error = depthAccuracy,
         collection_code = collectionCode,
         catalog_number = catalogNumber,
         institution_code = institutionCode,         
         dataset_name = datasetName,
         dataset_id = datasetKey,
         aphia_id = valid_AphiaID,
         taxonomic_status = taxonomicStatus,
         taxon_rank = taxonRank
  ) %>% 
  mutate(source = "GBIF") %>% 
  select(year, month, day, latitude, longitude, coordinate_error, depth, depth_error,
         institution_code, collection_code, catalog_number, dataset_name, dataset_id,
         country_code, 
         accepted_name, aphia_id, class, order, infraorder, family, genus, subgenus, 
         species, subspecies, taxonomic_status, taxon_rank, type, authorship, 
         source, id, 
         flag_year, flag_month, flag_day, flag_coordinate, flag_outlier, 
         flag_coord_uncertainty, flag_depth, flag_realm)

save(gbif_raw8, file = paste0("outputs_MOL/wannabe/occ/ANOMURANS_OCC_GBIF_TAXOCLEAN_FLAGGED",date,".RData"))

# OBIS
# columns to watch out for: 
# lat, long, flags, absence, record_type, basisofrecord, occurrenceremarks,
# occurrence status, countrycode

### B. Subset taxa of interest = already done

### C. Correct date
obis_raw7 <- clean_dates(obis_raw6, input_source = "OBIS")

### D. Correct country
obis_raw7 <- clean_countries(obis_raw7, input_source = "OBIS")

### E. Correct coordinates
obis_raw7 <- clean_geolocation(obis_raw7, input_source = "OBIS")

### F. Correct realm
obis_raw7 <- clean_realm(obis_raw7, input_source = "OBIS")

### G. Correct format
obis_raw8 <- obis_raw7 %>% 
  rename(latitude = decimallatitude,
         longitude = decimallongitude,
         coordinate_error = coordinateuncertaintyinmeters,
         depth = verbatimdepth,
         collection_code = collectioncode,
         catalog_number = catalognumber,
         institution_code = institutioncode,
         dataset_name = datasetname,
         aphia_id = valid_AphiaID,
         taxonomic_status = taxonomicStatus,
         taxon_rank = taxonRank
  ) %>% 
  mutate(source = "OBIS",
         depth_error = maximumdepthinmeters - minimumdepthinmeters) %>% 
  select(year, month, day, latitude, longitude, coordinate_error, depth, depth_error,
         institution_code, collection_code, catalog_number, dataset_name, dataset_id,
         country_code, 
         accepted_name, aphia_id, class, order, infraorder, family, genus, subgenus, 
         species, subspecies, taxonomic_status, taxon_rank, type, authorship, 
         source, id, 
         flag_year, flag_month, flag_day, flag_coordinate, flag_outlier, 
         flag_coord_uncertainty, flag_depth, flag_realm)

save(obis_raw8, file = paste0("outputs_MOL/wannabe/occ/ANOMURANS_OCC_OBIS_TAXOCLEAN_FLAGGED",date,".RData"))
