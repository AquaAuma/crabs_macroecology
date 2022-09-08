################################################################################
#### Code to filter certain occurrences
#### Uses homogenized occurrence taxonomy from <5.filter_occ_data.R> or <4.flag_occ_data.R>
#### Coding: Aurore Maureaud, June 2021
################################################################################

rm(list = ls())

# set date
date <- '10JUN2021'

# library
library(tidyverse)

# list of filter depending on flags

##############################################################################################################
##############################################################################################################
#### 1. BRACHYURANS
##############################################################################################################
##############################################################################################################
load("outputs_MOL/real/occ/BRACHYURANS_OCC_TAXOCLEAN_FLAGGED_DED_9JUN2021.RData")
dat_occ_real <- dat_occ

dat_occ_real_f <- dat_occ_real %>% 
  filter(is.na(flag_year),
         is.na(flag_coordinate), # problem with equal_coordinate = it's wrong!!
         flag_coord_uncertainty == "missing_uncertainty" || is.na(flag_coord_uncertainty),
         is.na(flag_realm))

print(paste0(round((1-nrow(dat_occ_real_f)/nrow(dat_occ_real))*100),"% of filtered occurrences, ",
             (nrow(dat_occ_real)-nrow(dat_occ_real_f))," occurrences removed"))



##############################################################################################################
##############################################################################################################
#### 2. ANOMURANS
##############################################################################################################
##############################################################################################################
load("outputs_MOL/wannabe/occ/ANOMURANS_OCC_TAXOCLEAN_FLAGGED_DED_9JUN2021.RData")
dat_occ_wannabe <- dat_occ

dat_occ_wannabe_f <- dat_occ_wannabe %>% 
  filter(is.na(flag_year),
         is.na(flag_coordinate),
         is.na(flag_coord_uncertainty) || flag_coord_uncertainty=="missing_uncertainty",
         is.na(flag_realm))

print(paste0(round((1-nrow(dat_occ_wannabe_f)/nrow(dat_occ_wannabe))*100),"% of filtered occurrences, ",
             (nrow(dat_occ_wannabe)-nrow(dat_occ_wannabe_f))," occurrences removed"))



##############################################################################################################
##############################################################################################################
#### 3. ALL CRABS
##############################################################################################################
##############################################################################################################
identical(names(dat_occ_real), names(dat_occ_wannabe))
dat_occ_crabs <- rbind(dat_occ_real, dat_occ_wannabe)

identical(names(dat_occ_real_f), names(dat_occ_wannabe_f))
dat_occ_crabs_f <- rbind(dat_occ_real_f, dat_occ_wannabe_f)

save(dat_occ_crabs, file = paste0("outputs_MOL/crabs/DAT_OCC_CRABS_",date,".RData"))
save(dat_occ_crabs_f, file = paste0("outputs_MOL/crabs/DAT_OCC_CRABS_FILTERED_",date,".RData"))
