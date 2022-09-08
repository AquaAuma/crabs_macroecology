################################################################################
#### Code to de-duplicate OBIS-GBIF
#### Uses homogenized occurrence taxonomy from <4.flag_occ_data.R>
#### Coding: Aurore Maureaud, June 2021
################################################################################

rm(list = ls())

# set date
date <- '9JUN2021'

# load libraires
library(tidyverse)


##############################################################################################################
##############################################################################################################
#### 1. BRACHYURANS
##############################################################################################################
##############################################################################################################

# GBIF
load("outputs_MOL/real/occ/BRACHYURANS_OCC_GBIF_TAXOCLEAN_FLAGGED9JUN2021.RData")
# OBIS
load("outputs_MOL/real/occ/BRACHYURANS_OCC_OBIS_TAXOCLEAN_FLAGGED9JUN2021.RData")

# create one data frame
identical(names(obis_raw8),names(gbif_raw8))
dat_occ <- rbind(obis_raw8, gbif_raw8)

# deduplication of occurrences with year-month-day
dat_occ_ded_1 <- dat_occ %>% 
  mutate(lon_dep = round(longitude, 4), # round long lat for the deduplication, OBIS/GBIF do not report the same precision
         lat_dep = round(latitude, 4),
         # make the deduplication on the occ_id
         occ_id = paste(source, lon_dep, lat_dep, year, month, day, accepted_name, sep = " "),
         # add source of both databses if identified as duplicated
         source = if_else(duplicated(occ_id), "OBIS-GBIF", source)
  ) %>% 
  distinct(year, month, day, lat_dep, lon_dep, accepted_name, source, .keep_all = TRUE) %>% 
  select(-lat_dep, -lon_dep)

print(paste0("Data contained ",round((1-nrow(dat_occ_ded_1)/nrow(dat_occ))*100),"% of duplicates, ",
             (nrow(dat_occ)-nrow(dat_occ_ded_1))," occurrences"))

# deduplication with year-month
dat_occ_ded_2 <- dat_occ %>% 
  mutate(lon_dep = round(longitude, 4), # round long lat for the deduplication, OBIS/GBIF do not report the same precision
         lat_dep = round(latitude, 4),
         # make the deduplication on the occ_id
         occ_id = paste(source, lon_dep, lat_dep, year, month, accepted_name, sep = " "),
         # add source of both databses if identified as duplicated
         source = if_else(duplicated(occ_id), "OBIS-GBIF", source)
  ) %>% 
  distinct(year, month, lat_dep, lon_dep, accepted_name, source, .keep_all = TRUE) %>% 
  select(-lat_dep, -lon_dep)

print(paste0("Data contained ",round((1-nrow(dat_occ_ded_2)/nrow(dat_occ))*100),"% of duplicates, ",
             (nrow(dat_occ)-nrow(dat_occ_ded_2))," occurrences"))

# deduplication with year
dat_occ_ded_3 <- dat_occ %>% 
  mutate(lon_dep = round(longitude, 4), # round long lat for the deduplication, OBIS/GBIF do not report the same precision
         lat_dep = round(latitude, 4),
         # make the deduplication on the occ_id
         occ_id = paste(source, lon_dep, lat_dep, year, accepted_name, sep = " "),
         # add source of both databses if identified as duplicated
         source = if_else(duplicated(occ_id), "OBIS-GBIF", source)
  ) %>% 
  distinct(year, lat_dep, lon_dep, accepted_name, source, .keep_all = TRUE) %>% 
  select(-lat_dep, -lon_dep)

print(paste0("Data contained ",round((1-nrow(dat_occ_ded_3)/nrow(dat_occ))*100),"% of duplicates, ",
             (nrow(dat_occ)-nrow(dat_occ_ded_3))," occurrences"))

# save year-month-day
dat_occ <- dat_occ_ded_3
save(dat_occ, file = paste0("outputs_MOL/real/occ/BRACHYURANS_OCC_TAXOCLEAN_FLAGGED_DED_",date,".RData"))


##############################################################################################################
##############################################################################################################
#### 2. ANOMURANS
##############################################################################################################
##############################################################################################################

rm(list = ls())

# set date
date <- '9JUN2021'

# GBIF
load("outputs_MOL/wannabe/occ/ANOMURANS_OCC_GBIF_TAXOCLEAN_FLAGGED9JUN2021.RData")
# OBIS
load("outputs_MOL/wannabe/occ/ANOMURANS_OCC_OBIS_TAXOCLEAN_FLAGGED9JUN2021.RData")

# create one data frame
identical(names(obis_raw8),names(gbif_raw8))
dat_occ <- rbind(obis_raw8, gbif_raw8)

# deduplication of occurrences with year-month-day
dat_occ_ded_1 <- dat_occ %>% 
  mutate(lon_dep = round(longitude, 4), # round long lat for the deduplication, OBIS/GBIF do not report the same precision
         lat_dep = round(latitude, 4),
         # make the deduplication on the occ_id
         occ_id = paste(source, lon_dep, lat_dep, year, month, day, accepted_name, sep = " "),
         # add source of both databses if identified as duplicated
         source = if_else(duplicated(occ_id), "OBIS-GBIF", source)
  ) %>% 
  distinct(year, month, day, lat_dep, lon_dep, accepted_name, source, .keep_all = TRUE) %>% 
  select(-lat_dep, -lon_dep)

print(paste0("Data contained ",round((1-nrow(dat_occ_ded_1)/nrow(dat_occ))*100),"% of duplicates, ",
             (nrow(dat_occ)-nrow(dat_occ_ded_1))," occurrences"))

# deduplication with year-month
dat_occ_ded_2 <- dat_occ %>% 
  mutate(lon_dep = round(longitude, 4), # round long lat for the deduplication, OBIS/GBIF do not report the same precision
         lat_dep = round(latitude, 4),
         # make the deduplication on the occ_id
         occ_id = paste(source, lon_dep, lat_dep, year, month, accepted_name, sep = " "),
         # add source of both databses if identified as duplicated
         source = if_else(duplicated(occ_id), "OBIS-GBIF", source)
  ) %>% 
  distinct(year, month, lat_dep, lon_dep, accepted_name, source, .keep_all = TRUE) %>% 
  select(-lat_dep, -lon_dep)

print(paste0("Data contained ",round((1-nrow(dat_occ_ded_2)/nrow(dat_occ))*100),"% of duplicates, ",
             (nrow(dat_occ)-nrow(dat_occ_ded_2))," occurrences"))

# deduplication with year
dat_occ_ded_3 <- dat_occ %>% 
  mutate(lon_dep = round(longitude, 4), # round long lat for the deduplication, OBIS/GBIF do not report the same precision
         lat_dep = round(latitude, 4),
         # make the deduplication on the occ_id
         occ_id = paste(source, lon_dep, lat_dep, year, accepted_name, sep = " "),
         # add source of both databses if identified as duplicated
         source = if_else(duplicated(occ_id), "OBIS-GBIF", source)
  ) %>% 
  distinct(year, lat_dep, lon_dep, accepted_name, source, .keep_all = TRUE) %>% 
  select(-lat_dep, -lon_dep)

print(paste0("Data contained ",round((1-nrow(dat_occ_ded_3)/nrow(dat_occ))*100),"% of duplicates, ",
             (nrow(dat_occ)-nrow(dat_occ_ded_3))," occurrences"))


# save year-month-day
dat_occ <- dat_occ_ded_3
save(dat_occ, file = paste0("outputs_MOL/wannabe/occ/ANOMURANS_OCC_TAXOCLEAN_FLAGGED_DED_",date,".RData"))
