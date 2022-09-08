rm(list = ls())

# set date
date <- '1JUN2021'

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
masterlist <- read.csv('outputs_MOL/real/BRACHYURANS_MASTER_23MAR2021.csv') %>% 
  filter(accid==0) # remove synonyms from the list


### B. Download country table from SeaLifeBase
##############################################
server <- "sealifebase"
CNT <- data.frame()
spp <- masterlist$canonical

for(i in 1:length(spp)){
  print(paste(i, spp[i]))
  
  # get the valid name from SeaLifeBase
  valid.spp <- validate_names(species_list = spp[i], server = server)
  if(length(valid.spp) %in% c(0,2)){valid.spp <- spp[i]}
  
  # download country info
  cnt <- country(species_list = valid.spp, server = server) %>% 
    select(SpecCode, Species, C_Code, CountryRefNo, Status, CurrentPresence,
           Freshwater, Brackish, Saltwater, Land, CountList, Map, Occur, Region,
           country) %>% 
    mutate(query = spp[i])
  
  CNT <- rbind(CNT, cnt)
  
  rm(valid.spp, cnt)
}


### C. Finalize data
#########################
CNT <- CNT %>% 
  mutate(Status = ifelse(Status %in% c("endemic","Native"), "native", Status),
         Status = ifelse(Status=="Introduced","introduced",Status)) %>% 
  filter(!is.na(Species),
         !is.na(C_Code),
         Status %in% c("introduced","native"),
         Brackish==1 | Saltwater==1) 


### D. Save outputs
###################
write.csv(CNT, file=paste0("outputs_MOL/real/cnt/COUNTRY_BRACHYURANS_", date, ".csv"))


################################################################################
#### 1. ANOMURANS
################################################################################

### A. Upload masterlist
########################
masterlist <- read.csv('outputs_MOL/wannabe/ANOMURANS_MASTER_23MAR2021.csv') %>% 
  filter(accid==0) # remove synonyms from the list


### B. Download country table from SeaLifeBase
##############################################
server <- "sealifebase"
CNT <- data.frame()
spp <- masterlist$canonical

for(i in 1:length(spp)){
  print(paste(i, spp[i]))
  
  # get the valid name from SeaLifeBase
  valid.spp <- validate_names(species_list = spp[i], server = server)
  if(length(valid.spp) %in% c(0,2)){valid.spp <- spp[i]}
  
  # download country info
  cnt <- country(species_list = valid.spp, server = server) %>% 
    select(SpecCode, Species, C_Code, CountryRefNo, Status, CurrentPresence,
           Freshwater, Brackish, Saltwater, Land, CountList, Map, Occur, Region,
           country) %>% 
    mutate(query = spp[i])
  
  CNT <- rbind(CNT, cnt)
  
  rm(valid.spp, cnt)
}

### C. Finalize data
#########################
CNT <- CNT %>% 
  mutate(Status = ifelse(Status %in% c("endemic","Native"), "native", Status),
         Status = ifelse(Status=="Introduced","introduced",Status)) %>% 
  filter(!is.na(Species),
         !is.na(C_Code),
         Status %in% c("introduced","native"),
         Brackish==1 | Saltwater==1)

### D. Save outputs
###################
write.csv(CNT, file=paste0("outputs_MOL/wannabe/cnt/COUNTRY_ANOMURANS_", date, ".csv"))

