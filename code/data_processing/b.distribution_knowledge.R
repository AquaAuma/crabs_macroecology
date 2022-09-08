rm(list = ls())

# set date
date <- '8SEPT2022'

# load libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(rfishbase)
library(rnaturalearth)
library(rnaturalearthdata)
library(worrms)


################################################################################
#### 1. BRACHYURANS
################################################################################

### A. Upload masterlist
########################
masterlist <- read.csv('outputs/taxonomy/real/BRACHYURANS_MASTER_TAXONOMY_JULY_2022.csv') %>% 
  filter(accid==0) # remove synonyms from the list
#synonyms <- read.csv('outputs/CRABS_Synonyms_sciName.csv', header=TRUE, sep=";", row.names=1)
# check names are similar
#identical(masterlist$ScientificName, synonyms$CommonName)


### B. Download info from SeaLifeBase
#####################################
server <- "sealifebase"
ECOL <- BIOL <- data.frame()
spp <- masterlist$canonical

for(i in 1:length(spp)){
  print(paste(i, spp[i]))
  
  # get the valid name from SeaLifeBase
  valid.spp <- validate_names(species_list = spp[i], server = server)
  if(length(valid.spp) %in% c(0,2)){valid.spp <- spp[i]}
  
  # 1. for information on species ecology, download the ecology table from SLB
  ECOL <- rbind(ECOL, ecology(species_list = valid.spp, server = server) %>% # keep all columns for now
                  # select(Species, Neritic, SupraLittoralZone, saltmarshes, LittoralZone, TidePools, Intertidal, SubLittoral, Oceanic, Epipelagic,
                  #        mesopelagic, bathypelagic, abyssopelagic, hadopelagic, Estuaries, Mangroves, MarshesSwamps, Benthic, 
                  #        Sessile, Mobile, Demersal, Endofauna, Pelagic, Megabenthos, Macrobenthos, Meiobenthos, SoftBottom, Sand, Mud, HardBottom, 
                  #        Rocky, SeaGrassBeds, CoralReefs, ReefFlats, Lagoons, Seamounts, DeepWaterCorals) %>% 
                  select(-DietRemark, -FoodRemark, -FoodRef, -IsoRemark, -IsoRef, -EcoTroph, -EcoseTroph, -EcoRemark, 
                         -EcoRef, -AddRems, -Entered, -Dateentered, -Modified, -Datemodified, -Expert, -Datechecked, -AssociationsRemarks, 
                         -InterstitialSpaces, -E_Append, -E_DateAppend, -FeedingType, -IsoTroph, -Herbivory2, -IsoseTroph,
                         -Marine, -BrackishWater, -FreshWater) %>% # remove columns absent in fishbase for homogenization
                  rename(Saltmarshes = saltmarshes, # rename columns which are inconsistent with fishbase
                         Bathypelagic = bathypelagic,
                         Mesopelagic = mesopelagic,
                         Abyssopelagic = abyssopelagic,
                         Hadopelagic = hadopelagic,
                         Caves = caves) %>% 
                  mutate(ReefExclusive = NA_integer_,
                         CaveAnchialine = NA_integer_,
                         ScientificName = spp[i]))
  
  # 2. get depth range from SLB, from the species table
  BIOL <- rbind(BIOL, species(species_list = valid.spp, server=server) %>% 
                  select(-FamCode, -GenCode, -Remark, -PicPreferredName, -PicPreferredNameM, -PicPreferredNameF, -PicPreferredNameJ, -Source, 
                         -AuthorRef, -SubGenCode, -Pic, -PictureFemale, -LarvaPic, -EggPic, -ImportanceRef, -Remarks7, -PriceReliability,
                         -LandingStatistics, -Landings, -II, -UsedforAquaculture, -LifeCycle, -AquacultureRef, -UsedasBait, -BaitRef, 
                         -Aquarium, -AquariumFishII, -AquariumRef, -GameFish, -GameRef, -DangerousRef, -Electrogenic, -ElectroRef, 
                         -Complete, -ASFA, -GoogleImage, -Emblematic, -Entered, -DateEntered, -Modified, -DateModified, -Expert, 
                         -DateChecked, -Synopsis, -DateSynopsis, -Flag, -Comments, -VancouverAquarium, -Sp2000_NameCode, -Sp2000_HierarchyCode,
                         -Sp2000_AuthorRefNumber, -E_Append, -E_DateAppend, -TS, -BodyShapeI, -Fresh, -Land) %>% 
                  mutate(isExtinct = NA_integer_,
                         PD50 = NA_integer_,
                         Amphibious = NA_character_,
                         AmphibiousRef = NA_character_,
                         ScientificName = spp[i]))
  
  rm(valid.spp)
}


### C. Check for problems
#########################
# some species are duplicated via different stock codes, with sometimes various habitats associated
ECOL <- unique(ECOL)
BIOL <- unique(BIOL)
nrow(ECOL) == length(spp) # false, problem because more rows in ECOL than length(spp)
identical(ECOL$ScientificName, spp)

pb <- c()
for(i in 1:length(spp)){
  x <- which(ECOL$ScientificName==spp[i])
  if(length(x)>1){pb[length(pb)+1] <- spp[i]}
}
ECOL.pb <- ECOL %>%
  filter(ScientificName %in% pb)
View(ECOL.pb)

ECOL <- ECOL %>% 
  dplyr::select(-StockCode, -autoctr, -EcologyRefNo, -HabitatsRef, -HerbivoryRef, 
                -FeedingTypeRef, -DietRef, -AssociationRef, -SubstrateRef) %>% 
  distinct()

# fix issues
ECOL <- ECOL %>% 
  filter()

identical(BIOL$ScientificName, spp)

pb <- c()
for(i in 1:length(spp)){
  x <- which(BIOL$ScientificName==spp[i])
  if(length(x)>1){pb[length(pb)+1] <- spp[i]}
}
BIOL.pb <- BIOL %>%
  filter(ScientificName %in% pb)
View(BIOL.pb)


### D. Save outputs
###################
write.csv(ECOL, file=paste0("outputs/distribution_trait/ECOLOGY_BRACHYURANS_", date, ".csv"))
write.csv(BIOL, file=paste0("outputs/distribution_trait/BIOLOGY_BRACHYURANS_", date, ".csv"))



################################################################################
#### 2. ANOMURANS
################################################################################

### A. Upload masterlist
########################
masterlist <- read.csv('outputs/taxonomy/wannabe/ANOMURANS_MASTER_TAXONOMY_JULY_2022.csv') %>% 
  filter(accid==0) # remove synonyms from the list


### B. Download info from SeaLifeBase
#####################################
server <- "sealifebase"
ECOL <- BIOL <- data.frame()
spp <- masterlist$canonical

for(i in 1:length(spp)){
  print(paste(i, spp[i]))
  
  # get the valid name from SeaLifeBase
  valid.spp <- validate_names(species_list = spp[i], server = server)
  if(length(valid.spp)==0){valid.spp <- spp[i]}
  
  # 1. for information on species ecology, download the ecology table from SLB
  ECOL <- rbind(ECOL, ecology(species_list = valid.spp, server = server) %>% # keep all columns for now
                  # select(Species, Neritic, SupraLittoralZone, saltmarshes, LittoralZone, TidePools, Intertidal, SubLittoral, Oceanic, Epipelagic,
                  #        mesopelagic, bathypelagic, abyssopelagic, hadopelagic, Estuaries, Mangroves, MarshesSwamps, Benthic, 
                  #        Sessile, Mobile, Demersal, Endofauna, Pelagic, Megabenthos, Macrobenthos, Meiobenthos, SoftBottom, Sand, Mud, HardBottom, 
                  #        Rocky, SeaGrassBeds, CoralReefs, ReefFlats, Lagoons, Seamounts, DeepWaterCorals) %>% 
                  select(-DietRemark, -FoodRemark, -FoodRef, -IsoRemark, -IsoRef, -EcoTroph, -EcoseTroph, -EcoRemark, 
                         -EcoRef, -AddRems, -Entered, -Dateentered, -Modified, -Datemodified, -Expert, -Datechecked, -AssociationsRemarks, 
                         -InterstitialSpaces, -E_Append, -E_DateAppend, -FeedingType, -IsoTroph, -Herbivory2, -IsoseTroph,
                         -Marine, -BrackishWater, -FreshWater) %>% # remove columns absent in fishbase for homogenization
                  rename(Saltmarshes = saltmarshes, # rename columns which are inconsistent with fishbase
                         Bathypelagic = bathypelagic,
                         Mesopelagic = mesopelagic,
                         Abyssopelagic = abyssopelagic,
                         Hadopelagic = hadopelagic,
                         Caves = caves) %>% 
                  mutate(ReefExclusive = NA_integer_,
                         CaveAnchialine = NA_integer_,
                         ScientificName = spp[i]))
  
  # 2. get depth range from SLB, from the species table
  BIOL <- rbind(BIOL, species(species_list = valid.spp, server=server) %>% 
                  select(-FamCode, -GenCode, -Remark, -PicPreferredName, -PicPreferredNameM, -PicPreferredNameF, -PicPreferredNameJ, -Source, 
                         -AuthorRef, -SubGenCode, -Pic, -PictureFemale, -LarvaPic, -EggPic, -ImportanceRef, -Remarks7, -PriceReliability,
                         -LandingStatistics, -Landings, -II, -UsedforAquaculture, -LifeCycle, -AquacultureRef, -UsedasBait, -BaitRef, 
                         -Aquarium, -AquariumFishII, -AquariumRef, -GameFish, -GameRef, -DangerousRef, -Electrogenic, -ElectroRef, 
                         -Complete, -ASFA, -GoogleImage, -Emblematic, -Entered, -DateEntered, -Modified, -DateModified, -Expert, 
                         -DateChecked, -Synopsis, -DateSynopsis, -Flag, -Comments, -VancouverAquarium, -Sp2000_NameCode, -Sp2000_HierarchyCode,
                         -Sp2000_AuthorRefNumber, -E_Append, -E_DateAppend, -TS, -BodyShapeI, -Fresh, -Land) %>% 
                  mutate(isExtinct = NA_integer_,
                         PD50 = NA_integer_,
                         Amphibious = NA_character_,
                         AmphibiousRef = NA_character_,
                         ScientificName = spp[i]))
  
  rm(valid.spp)
}

ECOL <- unique(ECOL)
BIOL <- unique(BIOL)
nrow(ECOL) == length(spp) # false, problem because more rows in ECOL than length(spp)
identical(ECOL$ScientificName, spp)

# pb <- c()
# for(i in 1:length(spp)){
#   x <- which(ECOL$ScientificName==spp[i])
#   if(length(x)>1){pb[length(pb)+1] <- spp[i]}
# }
# ECOL.pb <- ECOL %>% 
#   filter(ScientificName %in% pb)

identical(BIOL$ScientificName, spp)

### C. Save outputs
###################
write.csv(ECOL, file=paste0("outputs/distribution_trait/ECOLOGY_ANOMURANS_", date, ".csv"))
write.csv(BIOL, file=paste0("outputs/distribution_trait/BIOLOGY_ANOMURANS_", date, ".csv"))
