################################################################################
#### Try out a data flagging processing workflow on a few crab species
#### Coding: Aurore Maureaud, September 2022
################################################################################

rm(list = ls())

# packages
library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
world <- ne_countries(scale = "medium", returnclass = "sf")
library(egg)
library(raster)
sf::sf_use_s2(FALSE)
library(RColorBrewer)
library(units)


##############################################################################################################
#### 0. LOAD DATA
##############################################################################################################

# load occ data for crabs
dat_occ_nf <- get(load("~/Yale University/MOL-inverts/TaxToSDM/outputs_MOL/crabs/DAT_OCC_CRABS_9JUN2021.RData"))
dat_occ <- get(load("~/Yale University/MOL-inverts/TaxToSDM/outputs_MOL/crabs/DAT_OCC_CRABS_FILTERED_9JUN2021.RData"))

# load bpow layer
bpow <- st_read("~/Yale University/Marine Biogeography/bpow/outputs/bpow/bpow_p8s5_abyssal_corr.shp")

# integrate flags and occ data
load("~/Yale University/MOL-inverts/TaxToSDM/outputs_MOL/crabs/DAT_OCC_CRABS_FLAG_EXPRT_26JUL2021.RData")
load("~/Yale University/MOL-inverts/TaxToSDM/outputs_MOL/crabs/regions/DAT_OCC_ECO_CRABS_26JUL2021.RData")
head(dat_occ_hab)

# identify duplicated data via sources
# even with this procedure, 21 duplicates remain because of different catalog numbers
# don't know if they then should be considered separate or not
dups <- dat_occ_crabs %>% 
  filter(duplicated(occ_id)) %>% 
  dplyr::select(occ_id) %>% pull()

dat_occ_crabs <- dat_occ_crabs %>% 
  filter(!(occ_id %in% dups & source == "OBIS"),
         !(occ_id %in% dups & source == "GBIF")) %>% 
  distinct()

# fao fishing areas
fao <- st_read("~/Yale University/Marine Biogeography/regions_base/fao/World_Fao_Zones.shp")

# load traits
traits <- read.csv("~/Yale University/MOL-inverts/TaxToSDM/outputs_MOL/crabs/traits/master_traits_22JUL2021.csv")
high_traits <- read.csv("~/Yale University/MOL-inverts/TaxToSDM/outputs_MOL/crabs/traits/high_traits_22JUL2021.csv")

# set up results data frame
results <- data.frame(matrix(ncol = 3, nrow = 6))
colnames(results) <- c("number_species","number_points","avg_points")  
rownames(results) <- c("no_filter","occurrence_filter","habitat_filter","habitat_inference_filter","province_filter","fao_filter")


##############################################################################################################
#### 1. NO FILTER
##############################################################################################################
results$number_species[1] <- length(unique(dat_occ_crabs$accepted_name))
results$number_points[1] <- length(unique(dat_occ_crabs$occ_id))
results$avg_points[1] <- length(unique(dat_occ_crabs$occ_id))/length(unique(dat_occ_crabs$accepted_name))


##############################################################################################################
#### 2. OCCURRENCE FILTER
##############################################################################################################
dat_occ_crabs_2 <- dat_occ_crabs %>% 
  filter(is.na(flag_year),
         is.na(flag_coordinate),
         flag_coord_uncertainty %in% c("NA","missing_uncertainty"),
         #is.na(flag_outlier),
         is.na(flag_realm))

results$number_species[2] <- length(unique(dat_occ_crabs_2$accepted_name))
results$number_points[2] <- length(unique(dat_occ_crabs_2$occ_id))
results$avg_points[2] <- length(unique(dat_occ_crabs_2$occ_id))/length(unique(dat_occ_crabs_2$accepted_name))


##############################################################################################################
#### 3. HABITAT FILTER
##############################################################################################################
traits <- traits %>% 
  filter(accid == 0) %>% 
  dplyr::select(canonical, AphiaID, depth_range_shallow, depth_range_deep,
                vertical_habitat_2, distance_broad, specific_association)

dat_occ_crabs_3 <- left_join(dat_occ_crabs_2, traits, by = c("accepted_name"="canonical")) %>% 
  filter(is.na(vertical_habitat_2)| vertical_habitat_2 %in% c("benthic", "demersal", "benthopelagic"))

results$number_species[3] <- length(unique(dat_occ_crabs_3$accepted_name))
results$number_points[3] <- length(unique(dat_occ_crabs_3$occ_id))
results$avg_points[3] <- length(unique(dat_occ_crabs_3$occ_id))/length(unique(dat_occ_crabs_3$accepted_name))


##############################################################################################################
#### 4. HABITAT FILTER WITH TRAIT INFERENCE
##############################################################################################################



##############################################################################################################
#### 5. BIOGEOGRAPHIC FILTER
##############################################################################################################



##############################################################################################################
#### 6. FAO ZONES FILTER
##############################################################################################################



##############################################################################################################
#### ** SAVE RESULTS **
##############################################################################################################
write.csv(results, file = "outputs/data_processing/filter_results.csv")

