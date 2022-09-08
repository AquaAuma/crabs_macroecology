################################################################################
#### Code to merge OBIS & GBIF occurrence data
#### Uses the cleaned taxonomy built with R script a.
#### Coding: Aurore Maureaud, September 2022
################################################################################

rm(list = ls())

# set date
date <- '8SEPT2022'

# Load libraries
library(readr)
library(tidyverse)
library(rfishbase)
library(data.table)
#library(worms)


##############################################################################################################
##############################################################################################################
#### 1. BRACHYURANS
##############################################################################################################
##############################################################################################################

###########################################
#### A. Load cleaned taxonomy
###########################################
master <- read.csv("outputs/taxonomy/real/BRACHYURANS_MASTER_GBIF_15APR2021.csv") %>% 
  mutate(canonical = gsub('"',"",canonical)) # 11,464 names
master_accepted <- master %>% filter(accid==0) # 5837 accepted names
master_syn <- master %>% filter(accid>0) # 5627 names classified as synonyms


###########################################
#### B. Load and pre-clean GBIF data
###########################################
# Download of gbif occurrence records based on the following criteria:
gbif_raw <- fread(
  file.path("E:/Yale data/GBIF/BRACHYURA.25MAR2021/0231636-200613084148143/occurrence.txt") ,
  na.strings = c("", NA),
  quote = "",
  select = c(
    "gbifID",
    "decimalLatitude",
    "decimalLongitude",
    "coordinateUncertaintyInMeters",
    "year",
    "month",
    "day",
    "eventDate",
    "depth",
    "depthAccuracy",
    "basisOfRecord",
    "issue",
    "hasCoordinate",
    "hasGeospatialIssues",
    "countryCode",
    "locality",
    "stateProvince",
    "order",
    "family",
    "genus",
    "subgenus",
    "species",
    "specificEpithet",
    "infraspecificEpithet",
    "taxonRank",
    "taxonomicStatus",
    "occurrenceStatus",
    "collectionCode",
    "institutionCode",
    "catalogNumber",
    "datasetKey",
    "datasetName"
  )
) %>% 
  filter(basisOfRecord != "FOSSIL_SPECIMEN", # remove fossils
         !is.na(decimalLatitude), # remove records with lat/long
         !is.na(decimalLongitude),
         taxonRank %in% c("SPECIES","SUBSPECIES","VARIETY","UNRANKED"), # remove specimens that are family/genus
         occurrenceStatus == "PRESENT") %>% # remove absence records
  distinct()

# 948121 records from raw download
# 613434 records after first filter = lots of occurrence points without coordinates

# check availability of important fields for de-duplication

# institution code
length(unique(gbif_raw$institutionCode))
nrow(subset(gbif_raw, is.na(institutionCode)))
# collection code
length(unique(gbif_raw$collectionCode))
nrow(subset(gbif_raw, is.na(collectionCode)))
# catalog number
length(unique(gbif_raw$catalogNumber))
nrow(subset(gbif_raw, is.na(catalogNumber)))
# country code
length(unique(gbif_raw$countryCode))
nrow(subset(gbif_raw, is.na(countryCode)))
# dataset key
length(unique(gbif_raw$datasetKey))
nrow(subset(gbif_raw, is.na(datasetKey)))
# dataset name
length(unique(gbif_raw$datasetName))
nrow(subset(gbif_raw, is.na(datasetName)))


# gbif_dedup_1 <- gbif_raw %>% 
#   mutate(deduplication_high_level = paste(collectionCode,institutionCode,catalogNumber,sep=" ")) %>% 
#   dplyr::group_by(deduplication_high_level) %>% 
#   summarize(nbr_occ = length(deduplication_high_level))


###########################################
#### C. Load and pre-clean OBIS data
###########################################
# OBIS from Yani/MOL and subset for all marine decapods
obis_raw <- fread(
  file.path("E:/Yale data/OBIS/obis_decapoda/obis_decapoda.csv"),
  select = c("id",
             "decimallongitude",
             "decimallatitude",
             "scientificname",
             "date_year",
             "flags",
             "absence",
             "marine",
             "brackish",
             "freshwater",
             "terrestrial",
             "taxonrank",
             "aphiaid",
             "class",
             "order",
             "infraorder",
             "family",
             "genus",
             "subgenus",
             "species",
             "subspecies",
             "variety",
             "subvariety",
             "forma",
             "subforma",
             "type",
             "basisofrecord",
             "occurrenceremarks",
             "occurrencestatus",
             "year",
             "month",
             "day",
             "namepublishedin",
             "namepublishedinyear",
             "specificepithet",
             "infraspecificepithet",
             "scientificnameauthorship",
             "taxonomicstatus",
             "catalognumber",
             "collectioncode",
             "institutioncode",
             "dataset_id",
             "datasetname",
             "countrycode",
             "maximumdepthinmeters",
             "minimumdepthinmeters",
             "verbatimdepth",
             "coordinateuncertaintyinmeters")
) # 2,386,162 raws

unique(obis_raw$occurrencestatus) # all are present or unknown
unique(obis_raw$absence) # all are false, so only presence records
unique(obis_raw$taxonrank) # need to remove all non-species, subspecies and alike
unique(obis_raw$taxonomicstatus) # need to clean
unique(obis_raw$basisofrecord) # apparently no fossils there
summary(obis_raw$decimallatitude) # no NA
summary(obis_raw$decimallongitude) # no NA

obis_raw2 <- obis_raw %>% 
  filter(infraorder == "Brachyura", # 558,237 raws
         taxonrank %in% c("","Species","Subspecies")) %>%
  # mutate(collectioncode = ifelse(is.null(collectioncode),NA,collectioncode),
  #        catalognumber = ifelse(is.null(catalognumber),NA,catalognumber),
  #        institutioncode = ifelse(is.null(institutioncode),NA,institutioncode)) %>% 
  distinct() # 465,534

# institution code
length(unique(obis_raw2$institutioncode))
nrow(subset(obis_raw2, institutioncode==""))
# collection code
length(unique(obis_raw2$collectioncode))
nrow(subset(obis_raw2, collectioncode==""))
# catalog number
length(unique(obis_raw2$catalognumber))
nrow(subset(obis_raw2, catalognumber==""))
# country code
length(unique(obis_raw2$countrycode))
nrow(subset(obis_raw2, countrycode==""))
# dataset key
length(unique(obis_raw2$dataset_id))
nrow(subset(obis_raw2, dataset_id==""))
# dataset name
length(unique(obis_raw2$datasetname))
nrow(subset(obis_raw2, datasetname==""))

# obis_dedup_1 <- obis_raw2 %>% 
#   mutate(deduplication_high_level = paste(collectioncode,institutioncode,catalognumber,sep=" ")) %>% 
#   group_by(deduplication_high_level) %>% 
#   summarize(nbr_occ = length(deduplication_high_level))
# 

###########################################
#### D. High level deduplication OBIS-GBIF
###########################################

# remove duplicates from GBIF
nrow(gbif_raw)
gbif_raw2 <- gbif_raw %>% 
  select(-gbifID) %>% 
  distinct()
nrow(gbif_raw2) # removed 48,374 duplicates from GBIF!!

# remove obvious duplicated from OBIS
nrow(obis_raw2)
obis_raw3 <- obis_raw2 %>% 
  select(-id) %>% 
  distinct()
nrow(obis_raw3) # removed 14,134 duplicates from OBIS!!


### try for combination of collection code, institution code and catalog number
x_obis <- obis_raw3 %>% 
  filter(collectioncode!="",
         catalognumber!="",
         institutioncode!="") %>% 
  group_by(collectioncode,institutioncode,catalognumber) %>% 
  summarize(nbr_occ_obis = length(collectioncode))
x_gbif <- gbif_raw2 %>% 
  filter(!is.na(collectionCode),
         !is.na(catalogNumber),
         !is.na(institutionCode)) %>% 
  dplyr::rename(collectioncode = collectionCode,
                institutioncode = institutionCode,
                catalognumber = catalogNumber) %>% 
  group_by(collectioncode, institutioncode, catalognumber) %>% 
  summarize(nbr_occ_gbif = length(collectioncode))

x <- inner_join(x_gbif, x_obis, by=c("collectioncode","institutioncode","catalognumber"))
x_diff <- x %>% 
  mutate(equal = ifelse(nbr_occ_gbif==nbr_occ_obis, "yes","no"),
         higher_obis = ifelse(nbr_occ_gbif<nbr_occ_obis,TRUE,FALSE))

# x_diff_keep <- x_diff %>% filter(equal == "no" & higher_obis==FALSE)

x_diff_remove <- x_diff %>% 
  filter(!(equal=="no" & higher_obis==FALSE)) %>% 
  mutate(deduplication = paste(collectioncode, institutioncode, catalognumber, sep=" "))

# remove obvious duplicates from GBIF in two cases:
# (i) the codes correspond, and the number of occurrences are the same
# (ii) the codes correspond, and the number of occurrneces in OBIS are higher
gbif_raw3 <- gbif_raw2 %>% 
  mutate(deduplication = paste(collectionCode, institutionCode, catalogNumber, sep=" ")) %>% 
  filter(!deduplication %in% x_diff_remove$deduplication)
print(paste0("After high level deduplication, ", nrow(gbif_raw2)-nrow(gbif_raw3)," (",
             round((nrow(gbif_raw2)-nrow(gbif_raw3))/nrow(gbif_raw2)*100,1),"%)",
             " records removed from GBIF"))


####################################################
#### E. Homogenize species names based on masterlist
####################################################

### 1. GBIF
gbif_raw4 <- gbif_raw3 %>% 
  mutate(taxonRank_corr = taxonRank,
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && !is.na(infraspecificEpithet),"SUBSPECIES",taxonRank),
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && (!is.na(specificEpithet) & is.na(infraspecificEpithet)), "SPECIES", taxonRank),
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && (is.na(specificEpithet) & !is.na(genus)), "GENUS", taxonRank),
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && (is.na(genus) & !is.na(family)), "FAMILY", taxonRank),
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && length(str_split(species, " "))==1, "FAMILY/GENUS", taxonRank_corr),
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && length(str_split(species, " "))==2, "SPECIES", taxonRank_corr),
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && length(str_split(species, " "))==3, "SUBSPECIES", taxonRank_corr)) %>% 
  filter(taxonRank_corr %in% c("SPECIES","SUBSPECIES")) %>% 
  # dim(subset(gbif_raw4, taxonRank_corr=="UNRANKED")) #0, ok!
  rename(order_gbif = order,
         family_gbif = family,
         genus_gbif = genus,
         subgenus_gbif = subgenus,
         species_gbif = species,
         taxonRank_gbif = taxonRank_corr,
         taxonomicStatus_gbif = taxonomicStatus) %>% 
  select(-specificEpithet, -infraspecificEpithet, -taxonRank)

# there are a lot of duplicates in the masterlist, but not of canonical accepted names
# a lot of synonyms are duplicates because of subgenera and different authorship
# so it has to be included otherwise the data will have added rows with duplicated occurrences but different names

gbif_names_match <- gbif_raw4 %>% 
  mutate(accepted_name=NA_character_,
         id = NA_integer_) %>% 
  select(species_gbif,id,accepted_name) %>% 
  distinct()

for(i in 1:nrow(gbif_names_match)){
  j <- which(gbif_names_match$species_gbif[i]==master$canonical)
  
  sub_master <- master %>% 
    filter(id %in% j)
  
  if(nrow(sub_master)>0){
    if("accepted" %in% sub_master$type){
      gbif_names_match$id[i] <- sub_master[sub_master$type=="accepted",]$id
      gbif_names_match$accepted_name[i] <- sub_master[sub_master$type=="accepted",]$canonical
    } else{
      gbif_names_match$id[i] <- sub_master$accid[1]
      gbif_names_match$accepted_name[i] <- master[master$id==sub_master$accid[1],]$canonical
    }
  }
  
  rm(sub_master)
}

# still some NA's, because non-marine taxa
gbif_names_match <- gbif_names_match %>% 
  filter(!is.na(accepted_name)) %>% 
  select(-id)

gbif_raw5 <- left_join(gbif_raw4, gbif_names_match, by=c("species_gbif"="species_gbif"))
nrow(gbif_raw5)==nrow(gbif_raw4) #if TRUE, match names if correct!

# include classification from masterlist
gbif_raw6 <- left_join(gbif_raw5, master_accepted, by=c("accepted_name"="canonical")) %>% 
  select(-order_gbif, -family_gbif, -genus_gbif, -subgenus_gbif,
         -taxonRank_gbif, -taxonomicStatus_gbif)
nrow(gbif_raw5)==nrow(gbif_raw6) #if TRUE, match names if correct!


### 2. OBIS
# correct rank and filter species and subspecies levels
obis_raw4 <- obis_raw3 %>% 
  mutate(taxonrank_corr = taxonrank,
         taxonrank_corr = ifelse(taxonrank =="" & !is.na(infraspecificepithet),"Subspecies",taxonrank),
         taxonrank_corr = ifelse(taxonrank_corr == "" & (!is.na(specificepithet) & is.na(infraspecificepithet)), "Species", taxonrank_corr),
         taxonrank_corr = ifelse(taxonrank_corr == "" & (is.na(specificepithet) & !is.na(genus)), "GENUS", taxonrank_corr),
         taxonrank_corr = ifelse(taxonrank_corr == "" & (is.na(genus) & !is.na(family)), "FAMILY", taxonrank_corr),
         taxonrank_corr = ifelse(taxonrank_corr == "" & length(str_split(species, " "))==1, "FAMILY/GENUS", taxonrank_corr),
         taxonrank_corr = ifelse(taxonrank_corr == "" & length(str_split(species, " "))==2, "Species", taxonrank_corr),
         taxonrank_corr = ifelse(taxonrank_corr == "" & length(str_split(species, " "))==3, "Subspecies", taxonrank_corr)) %>% 
  filter(taxonrank_corr %in% c("Subspecies","Species")) %>% 
  rename(order_obis = order,
         family_obis = family,
         genus_obis = genus,
         subgenus_obis = subgenus,
         species_obis = species,
         subspecies_obis = subspecies,
         taxonRank_obis = taxonrank_corr,
         taxonomicStatus_obis = taxonomicstatus,
         record_type = type) %>% 
  select(-specificepithet, -infraspecificepithet, -taxonrank, -class, -infraorder,
         -variety, -subvariety, -forma, -subforma, -marine, -brackish, -freshwater, -terrestrial)

# output for name matching
obis_names_match <- obis_raw4 %>% 
  mutate(accepted_name=NA_character_,
         id = NA_integer_) %>% 
  select(species_obis,id,accepted_name) %>% 
  distinct()

for(i in 1:nrow(obis_names_match)){
  j <- which(obis_names_match$species_obis[i]==master$canonical)
  
  sub_master <- master %>% 
    filter(id %in% j)
  
  if(nrow(sub_master)>0){
    if("accepted" %in% sub_master$type){
      obis_names_match$id[i] <- sub_master[sub_master$type=="accepted",]$id
      obis_names_match$accepted_name[i] <- sub_master[sub_master$type=="accepted",]$canonical
    } else{
      obis_names_match$id[i] <- sub_master$accid[1]
      obis_names_match$accepted_name[i] <- master[master$id==sub_master$accid[1],]$canonical
    }
  }
  rm(sub_master)
}

obis_names_match <- obis_names_match %>% 
  filter(!is.na(accepted_name)) %>% 
  select(-id)

obis_raw5 <- left_join(obis_raw4, obis_names_match, by=c("species_obis"="species_obis"))
nrow(obis_raw5)==nrow(obis_raw4) #if TRUE, match names if correct!

# include classification from masterlist
obis_raw6 <- left_join(obis_raw5, master_accepted, by=c("accepted_name"="canonical")) %>% 
  select(-order_obis, -family_obis, -genus_obis, -subgenus_obis,
         -taxonRank_obis, -taxonomicStatus_obis, -subspecies_obis, -aphiaid)
nrow(obis_raw5)==nrow(obis_raw6) #if TRUE, match names if correct!


### 3. Save outputs
save(gbif_raw6, file = paste0("outputs/occurrence/BRACHYURANS_OCC_GBIF_TAXOCLEAN_",date,".RData"))
save(obis_raw6, file = paste0("outputs/occurrence/BRACHYURANS_OCC_OBIS_TAXOCLEAN_",date,".RData"))


##############################################################################################################
##############################################################################################################
#### 2. ANOMURANS
##############################################################################################################
##############################################################################################################


###########################################
#### A. Load cleaned taxonomy
###########################################
master <- read.csv("outputs/taxonomy/wannabe/ANOMURANS_MASTER_GBIF_15APR2021.csv") %>% 
  mutate(canonical = gsub('"',"",canonical)) # 11,464 names
master_accepted <- master %>% filter(accid==0) # 5837 accepted names
master_syn <- master %>% filter(accid>0) # 5627 names classified as synonyms


###########################################
#### B. Load and pre-clean GBIF data
###########################################
# Download of gbif occurrence records based on the following criteria:
gbif_raw <- fread(
  file.path("E:/Yale data/GBIF/ANOMURA.25MAR2021/0231554-200613084148143/occurrence.txt") ,
  na.strings = c("", NA),
  quote = "",
  select = c(
    "gbifID",
    "decimalLatitude",
    "decimalLongitude",
    "coordinateUncertaintyInMeters",
    "year",
    "month",
    "day",
    "eventDate",
    "depth",
    "depthAccuracy",
    "basisOfRecord",
    "issue",
    "hasCoordinate",
    "hasGeospatialIssues",
    "countryCode",
    "locality",
    "stateProvince",
    "order",
    "family",
    "genus",
    "subgenus",
    "species",
    "specificEpithet",
    "infraspecificEpithet",
    "taxonRank",
    "taxonomicStatus",
    "occurrenceStatus",
    "collectionCode",
    "institutionCode",
    "catalogNumber",
    "datasetKey",
    "datasetName"
  )
) %>% 
  filter(basisOfRecord != "FOSSIL_SPECIMEN", # remove fossils
         !is.na(decimalLatitude), # remove records with lat/long
         !is.na(decimalLongitude),
         taxonRank %in% c("SPECIES","SUBSPECIES","VARIETY","UNRANKED"), # remove specimens that are family/genus
         occurrenceStatus == "PRESENT") %>% # remove absence records
  distinct()

print(paste0(nrow(gbif_raw), " GBIF records"))

# check availability of important fields for de-duplication

# institution code
length(unique(gbif_raw$institutionCode))
nrow(subset(gbif_raw, is.na(institutionCode)))
# collection code
length(unique(gbif_raw$collectionCode))
nrow(subset(gbif_raw, is.na(collectionCode)))
# catalog number
length(unique(gbif_raw$catalogNumber))
nrow(subset(gbif_raw, is.na(catalogNumber)))
# country code
length(unique(gbif_raw$countryCode))
nrow(subset(gbif_raw, is.na(countryCode)))
# dataset key
length(unique(gbif_raw$datasetKey))
nrow(subset(gbif_raw, is.na(datasetKey)))
# dataset name
length(unique(gbif_raw$datasetName))
nrow(subset(gbif_raw, is.na(datasetName)))


###########################################
#### C. Load and pre-clean OBIS data
###########################################
# OBIS from Yani/MOL and subset for all marine decapods
obis_raw <- fread(
  file.path("E:/Yale data/OBIS/obis_decapoda/obis_decapoda.csv"),
  select = c("id",
             "decimallongitude",
             "decimallatitude",
             "scientificname",
             "date_year",
             "flags",
             "absence",
             "marine",
             "brackish",
             "freshwater",
             "terrestrial",
             "taxonrank",
             "aphiaid",
             "class",
             "order",
             "infraorder",
             "family",
             "genus",
             "subgenus",
             "species",
             "subspecies",
             "variety",
             "subvariety",
             "forma",
             "subforma",
             "type",
             "basisofrecord",
             "occurrenceremarks",
             "occurrencestatus",
             "year",
             "month",
             "day",
             "namepublishedin",
             "namepublishedinyear",
             "specificepithet",
             "infraspecificepithet",
             "scientificnameauthorship",
             "taxonomicstatus",
             "catalognumber",
             "collectioncode",
             "institutioncode",
             "dataset_id",
             "datasetname",
             "countrycode",
             "maximumdepthinmeters",
             "minimumdepthinmeters",
             "verbatimdepth",
             "coordinateuncertaintyinmeters")
) # 2,386,162 raws

unique(obis_raw$occurrencestatus) # all are present or unknown
unique(obis_raw$absence) # all are false, so only presence records
unique(obis_raw$taxonrank) # need to remove all non-species, subspecies and alike
unique(obis_raw$taxonomicstatus) # need to clean
unique(obis_raw$basisofrecord) # apparently no fossils there
summary(obis_raw$decimallatitude) # no NA
summary(obis_raw$decimallongitude) # no NA

obis_raw2 <- obis_raw %>% 
  filter(infraorder == "Anomura", # 558,237 raws
         taxonrank %in% c("","Species","Subspecies")) %>%
  # mutate(collectioncode = ifelse(is.null(collectioncode),NA,collectioncode),
  #        catalognumber = ifelse(is.null(catalognumber),NA,catalognumber),
  #        institutioncode = ifelse(is.null(institutioncode),NA,institutioncode)) %>% 
  distinct() # 159,565

# institution code
length(unique(obis_raw2$institutioncode))
nrow(subset(obis_raw2, institutioncode==""))
# collection code
length(unique(obis_raw2$collectioncode))
nrow(subset(obis_raw2, collectioncode==""))
# catalog number
length(unique(obis_raw2$catalognumber))
nrow(subset(obis_raw2, catalognumber==""))
# country code
length(unique(obis_raw2$countrycode))
nrow(subset(obis_raw2, countrycode==""))
# dataset key
length(unique(obis_raw2$dataset_id))
nrow(subset(obis_raw2, dataset_id==""))
# dataset name
length(unique(obis_raw2$datasetname))
nrow(subset(obis_raw2, datasetname==""))


###########################################
#### D. High level deduplication OBIS-GBIF
###########################################

# remove duplicates from GBIF
nrow(gbif_raw)
gbif_raw2 <- gbif_raw %>% 
  select(-gbifID) %>% 
  distinct()
nrow(gbif_raw2) # removed 48,374 duplicates from GBIF!!

# remove obvious duplicated from OBIS
nrow(obis_raw2)
obis_raw3 <- obis_raw2 %>% 
  select(-id) %>% 
  distinct()
nrow(obis_raw3) # removed 14,134 duplicates from OBIS!!


### try for combination of collection code, institution code and catalog number
x_obis <- obis_raw3 %>% 
  filter(collectioncode!="",
         catalognumber!="",
         institutioncode!="") %>% 
  group_by(collectioncode,institutioncode,catalognumber) %>% 
  summarize(nbr_occ_obis = length(collectioncode))
x_gbif <- gbif_raw2 %>% 
  filter(!is.na(collectionCode),
         !is.na(catalogNumber),
         !is.na(institutionCode)) %>% 
  dplyr::rename(collectioncode = collectionCode,
                institutioncode = institutionCode,
                catalognumber = catalogNumber) %>% 
  group_by(collectioncode, institutioncode, catalognumber) %>% 
  summarize(nbr_occ_gbif = length(collectioncode))

x <- inner_join(x_gbif, x_obis, by=c("collectioncode","institutioncode","catalognumber"))
x_diff <- x %>% 
  mutate(equal = ifelse(nbr_occ_gbif==nbr_occ_obis, "yes","no"),
         higher_obis = ifelse(nbr_occ_gbif<nbr_occ_obis,TRUE,FALSE))

x_diff_remove <- x_diff %>% 
  filter(!(equal=="no" & higher_obis==FALSE)) %>% 
  mutate(deduplication = paste(collectioncode, institutioncode, catalognumber, sep=" "))

# remove obvious duplicates from GBIF in two cases:
# (i) the codes correspond, and the number of occurrences are the same
# (ii) the codes correspond, and the number of occurrneces in OBIS are higher
gbif_raw3 <- gbif_raw2 %>% 
  mutate(deduplication = paste(collectionCode, institutionCode, catalogNumber, sep=" ")) %>% 
  filter(!deduplication %in% x_diff_remove$deduplication)
print(paste0("After high level deduplication, ", nrow(gbif_raw2)-nrow(gbif_raw3)," (",
             round((nrow(gbif_raw2)-nrow(gbif_raw3))/nrow(gbif_raw2)*100,1),"%)",
             " records removed from GBIF"))


####################################################
#### E. Homogenize species names based on masterlist
####################################################

### 1. GBIF
gbif_raw4 <- gbif_raw3 %>% 
  mutate(taxonRank_corr = taxonRank,
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && !is.na(infraspecificEpithet),"SUBSPECIES",taxonRank),
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && (!is.na(specificEpithet) & is.na(infraspecificEpithet)), "SPECIES", taxonRank),
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && (is.na(specificEpithet) & !is.na(genus)), "GENUS", taxonRank),
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && (is.na(genus) & !is.na(family)), "FAMILY", taxonRank),
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && length(str_split(species, " "))==1, "FAMILY/GENUS", taxonRank_corr),
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && length(str_split(species, " "))==2, "SPECIES", taxonRank_corr),
         taxonRank_corr = ifelse(taxonRank == "UNRANKED" && length(str_split(species, " "))==3, "SUBSPECIES", taxonRank_corr)) %>% 
  filter(taxonRank_corr %in% c("SPECIES","SUBSPECIES")) %>% 
  # dim(subset(gbif_raw4, taxonRank_corr=="UNRANKED")) #0, ok!
  rename(order_gbif = order,
         family_gbif = family,
         genus_gbif = genus,
         subgenus_gbif = subgenus,
         species_gbif = species,
         taxonRank_gbif = taxonRank_corr,
         taxonomicStatus_gbif = taxonomicStatus) %>% 
  select(-specificEpithet, -infraspecificEpithet, -taxonRank)

# there are a lot of duplicates in the masterlist, but not of canonical accepted names
# a lot of synonyms are duplicates because of subgenera and different authorship
# so it has to be included otherwise the data will have added rows with duplicated occurrences but different names

gbif_names_match <- gbif_raw4 %>% 
  mutate(accepted_name=NA_character_,
         id = NA_integer_) %>% 
  select(species_gbif,id,accepted_name) %>% 
  distinct()

for(i in 1:nrow(gbif_names_match)){
  j <- which(gbif_names_match$species_gbif[i]==master$canonical)
  
  sub_master <- master %>% 
    filter(id %in% j)
  
  if(nrow(sub_master)>0){
    if("accepted" %in% sub_master$type){
      gbif_names_match$id[i] <- sub_master[sub_master$type=="accepted",]$id
      gbif_names_match$accepted_name[i] <- sub_master[sub_master$type=="accepted",]$canonical
    } else {
      gbif_names_match$id[i] <- sub_master$accid[1]
      gbif_names_match$accepted_name[i] <- master[master$id==sub_master$accid[1],]$canonical
    }
  }
  rm(sub_master)
}

# still some NA's, because non-marine taxa
gbif_names_match <- gbif_names_match %>% 
  filter(!is.na(accepted_name)) %>% 
  select(-id)

gbif_raw5 <- left_join(gbif_raw4, gbif_names_match, by=c("species_gbif"="species_gbif"))
nrow(gbif_raw5)==nrow(gbif_raw4) #if TRUE, match names if correct!

# include classification from masterlist
gbif_raw6 <- left_join(gbif_raw5, master_accepted, by=c("accepted_name"="canonical")) %>% 
  select(-order_gbif, -family_gbif, -genus_gbif, -subgenus_gbif,
         -taxonRank_gbif, -taxonomicStatus_gbif)
nrow(gbif_raw5)==nrow(gbif_raw6) #if TRUE, match names if correct!


### 2. OBIS
# correct rank and filter species and subspecies levels
obis_raw4 <- obis_raw3 %>% 
  mutate(taxonrank_corr = taxonrank,
         taxonrank_corr = ifelse(taxonrank =="" & !is.na(infraspecificepithet),"Subspecies",taxonrank),
         taxonrank_corr = ifelse(taxonrank_corr == "" & (!is.na(specificepithet) & is.na(infraspecificepithet)), "Species", taxonrank_corr),
         taxonrank_corr = ifelse(taxonrank_corr == "" & (is.na(specificepithet) & !is.na(genus)), "GENUS", taxonrank_corr),
         taxonrank_corr = ifelse(taxonrank_corr == "" & (is.na(genus) & !is.na(family)), "FAMILY", taxonrank_corr),
         taxonrank_corr = ifelse(taxonrank_corr == "" & length(str_split(species, " "))==1, "FAMILY/GENUS", taxonrank_corr),
         taxonrank_corr = ifelse(taxonrank_corr == "" & length(str_split(species, " "))==2, "Species", taxonrank_corr),
         taxonrank_corr = ifelse(taxonrank_corr == "" & length(str_split(species, " "))==3, "Subspecies", taxonrank_corr)) %>% 
  filter(taxonrank_corr %in% c("Subspecies","Species")) %>% 
  rename(order_obis = order,
         family_obis = family,
         genus_obis = genus,
         subgenus_obis = subgenus,
         species_obis = species,
         subspecies_obis = subspecies,
         taxonRank_obis = taxonrank_corr,
         taxonomicStatus_obis = taxonomicstatus,
         record_type = type) %>% 
  select(-specificepithet, -infraspecificepithet, -taxonrank, -class, -infraorder,
         -variety, -subvariety, -forma, -subforma, -marine, -brackish, -freshwater, -terrestrial)

# output for name matching
obis_names_match <- obis_raw4 %>% 
  mutate(accepted_name=NA_character_,
         id = NA_integer_) %>% 
  select(species_obis,id,accepted_name) %>% 
  distinct()

for(i in 1:nrow(obis_names_match)){
  j <- which(obis_names_match$species_obis[i]==master$canonical)
  
  sub_master <- master %>% 
    filter(id %in% j)
  
  if(nrow(sub_master)>0){
    if("accepted" %in% sub_master$type){
      obis_names_match$id[i] <- sub_master[sub_master$type=="accepted",]$id
      obis_names_match$accepted_name[i] <- sub_master[sub_master$type=="accepted",]$canonical
    } else {
      obis_names_match$id[i] <- sub_master$accid[1]
      obis_names_match$accepted_name[i] <- master[master$id==sub_master$accid[1],]$canonical
    }
  }
  rm(sub_master)
}

obis_names_match <- obis_names_match %>% 
  filter(!is.na(accepted_name)) %>% 
  select(-id)

obis_raw5 <- left_join(obis_raw4, obis_names_match, by=c("species_obis"="species_obis"))
nrow(obis_raw5)==nrow(obis_raw4) #if TRUE, match names if correct!

# include classification from masterlist
obis_raw6 <- left_join(obis_raw5, master_accepted, by=c("accepted_name"="canonical")) %>% 
  select(-order_obis, -family_obis, -genus_obis, -subgenus_obis,
         -taxonRank_obis, -taxonomicStatus_obis, -subspecies_obis, -aphiaid)
nrow(obis_raw5)==nrow(obis_raw6) #if TRUE, match names if correct!


### 3. Save outputs
save(gbif_raw6, file = paste0("outputs/occurrence/ANOMURANS_OCC_GBIF_TAXOCLEAN_",date,".RData"))
save(obis_raw6, file = paste0("outputs/occurrence/ANOMURANS_OCC_OBIS_TAXOCLEAN_",date,".RData"))
