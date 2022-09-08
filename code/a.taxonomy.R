### Code contributors: Aurore Maureaud and Maisha Lucas
rm(list = ls())

# set date
date <- '03MAY2022'

# Load libraries
library(dplyr)
library(taxize)
library(readr)
library(tidyverse)
library(rfishbase)
library(data.table)
library(readxl)


##############################################################################################################
##############################################################################################################
#### 1. BRACHYURANS
##############################################################################################################
##############################################################################################################

# Load worms
worms <- read_delim("Yale University/MOL-inverts/WORMS/WoRMS_download_2022-03-01/taxon.txt", 
                    "\t", escape_double = FALSE, trim_ws = TRUE,
                    col_types = cols(specificEpithet = col_character(),
                                     infraspecificEpithet = col_character(),
                                     license = col_character(),
                                     datasetName = col_character()))

# Load biome info from worms
biome <- read_delim('Yale University/MOL-inverts/WORMS/WoRMS_download_2022-03-01/speciesprofile.txt',
                    "\t", escape_double = FALSE, trim_ws = TRUE)

worms <- left_join(worms, biome, by = 'taxonID')

# Sub-select crabs
# Corresponds to Domain:	Eukaryota
# Kingdom:	Animalia
# Phylum:	Arthropoda
# Subphylum:	Crustacea
# Class:	Malacostraca
# Order:	Decapoda
# Suborder:	Pleocyemata
# Infraorder:	Brachyura 7649 spp on Worms website
# keep Pseudocarcinus as a genus, there is no family attached http://www.marinespecies.org/aphia.php?p=taxdetails&id=440496

crabfamilies <- c('Aethridae','Belliidae','Bythograeidae','Calappidae','Matutidae','Atelecyclidae',
                  'Cancridae','Carpiliidae','Cheiragonidae','Corystidae','Dacryopilumnidae','Dairidae',
                  'Dorippidae','Ethusidae','Dairoididae','Eriphiidae','Hypothalassiidae',
                  'Menippidae','Oziidae','Platyxanthidae','Gecarcinucidae',
                  'Acidopsidae','Goneplacidae','Chasmocarcinidae','Conleyidae',
                  'Euryplacidae','Litocheiridae','Mathildellidae','Progeryonidae',
                  'Scalopidiidae','Sotoplacidae','Vultocinidae','Hexapodidae','Hymenosomatidae',
                  'Iphiculidae','Leucosiidae','Epialtidae','Inachidae','Inachoididae','Majidae',
                  'Mithracidae','Oregoniidae','Orithyiidae','Crossotonotidae','Palicidae',
                  'Parthenopidae','Galenidae','Pilumnidae','Tanaochelidae','Brusiniidae',
                  'Carcinidae','Geryonidae','Ovalipidae','Polybiidae','Portunidae',
                  'Thiidae','Potamidae','Pseudocarcinidae',
                  'Christmaplacidae','Pilumnoididae','Planopilumnidae',
                  'Pseudoziidae','Retroplumidae','Domeciidae','Tetraliidae','Trapeziidae',
                  'Trichopeltariidae','Linnaeoxanthidae','Panopeidae','Pseudorhombilidae','Xanthidae',
                  'Cryptochiridae','Gecarcinidae','Glyptograpsidae','Grapsidae','Leptograpsodidae','Percnidae',
                  'Plagusiidae','Sesarmidae','Varunidae','Xenograpsidae','Camptandriidae','Dotillidae',
                  'Ocypodidae','Heloeciidae','Macrophthalmidae','Mictyridae','Xenophthalmidae',
                  'Aphanodactylidae','Pinnotheridae','Cyclodorippidae','Cymonomidae','Phyllotymolinidae',
                  'Dromiidae','Dynomenidae','Homolodromiidae','Latreilliidae','Poupiniidae',
                  'Homolidae','Lyreididae','Raninidae')


#check <- worms %>% 
#filter(family %in% crabfamilies) %>% 
#select(family) %>% 
#distinct()

#check <- data.frame(check)
#identical(sort(crabfamilies), sort(check$family)) # TRUE is ok
#crabfamilies <- data.frame(crabfamilies)
#names(crabfamilies) <- 'family'

#anti_join(crabfamilies, check, by='family') #0, ok
#anti_join(check, crabfamilies, by='family') #NA, also ok, 

# for all Brachyura, taxon ranks are: family, genus, species, subfamily, subgenus, subspecies
# there is no further subspecies levels accepted = variety, forms etc...
# only levels species and subspecies are then kept

worms <- worms %>% 
  dplyr::filter(order == 'Decapoda',
                isMarine == 1,
                family %in% crabfamilies,
                taxonomicStatus =="accepted",
                taxonRank %in% c('Species','Subspecies'))


################################################
### A. Formatting Master list
################################################
worms2 <- worms %>%  
  mutate(AphiaID = gsub("[a-zA-Z:. ]", "", taxonID),
         # re-write species
         species = specificEpithet,
         subspecies = infraspecificEpithet,
         # re-write spp names
         ScientificName = if_else(!is.na(subgenus),
                                  paste0(genus, " (", subgenus, ") ", specificEpithet),
                                  paste(genus, specificEpithet, sep=" ")),
         ScientificName = if_else(!is.na(infraspecificEpithet),
                                  paste(ScientificName, infraspecificEpithet, sep=" "),
                                  ScientificName),
         AuthorshipYear = gsub("\\D","", scientificNameAuthorship),
         AuthorshipYear = as.numeric(case_when(nchar(AuthorshipYear)>4 ~ substr(AuthorshipYear, start=1, stop=4),
                                               nchar(AuthorshipYear)==4 ~ AuthorshipYear)),
         Authorship = gsub(", [0-9]+", "", scientificNameAuthorship),
         Authorship = gsub("[()]", "", Authorship),
         Authorship = gsub("[A-Z]. ", "", Authorship),
         authorship = paste(Authorship, AuthorshipYear, sep=" "),
         authorship = scientificNameAuthorship,
         accid = 0,
         source = "WoRMS",
         type = "accepted") %>% 
  select(AphiaID, accid, class, order, family, genus, subgenus, species, subspecies, ScientificName, taxonomicStatus, 
         taxonRank, type, authorship, source) %>%
  distinct()


# removing species with different AphiaID, same ScientificName but with/without subgenera

xx <- subset(worms2, duplicated(ScientificName))

worms2 <- worms2 %>%
  filter(!AphiaID %in% xx$AphiaID)

#no more duplicates with author, aphiaID and taxonomic status

# order columns
worms2 <- worms2[order(worms2$class,worms2$order,worms2$family,worms2$genus,worms2$species, worms2$subspecies),]

# 5827 species/subspecies!
write.csv(worms2, file=paste('outputs/taxonomy/real/BRACHYURANS_MASTERLIST_SUBSPE_',date,'.csv', sep=''), row.names=FALSE)


################################################
### B. Synonym list, using worms2
################################################
# Select taxon id
splist <- worms2$AphiaID

# Check species list for synonyms using taxon id
temp <- taxize::synonyms(splist,
                         db='worms')

# Convert raw species list to data.frame()
mylist <- synonyms_df(temp)

# Write file with synonyms since it takes a long time to compile them!!
save(mylist, file="outputs/taxonomy/real/synonyms.RData")

# Finalize the synonym file
load(file="outputs/taxonomy/real/synonyms.RData")

unique(mylist$match_type) # good

unique(mylist$status) # only unaccepted names

synonyms <- mylist %>% 
  #filter(rank %in% c('Species','Subspecies')) %>% # 3% of the synonyms are forms and varieties, 
  # we can keep them in case
  # remove sub genera in parenthesis
  mutate(subgenus = ifelse(str_detect(scientificname, "\\)")==TRUE,
                           paste0(str_split(scientificname, "\\)", simplify=T)[,1],")"),
                           NA_character_),
         ScientificName = gsub("\\s*\\([^\\)]+\\)", "", scientificname),
         valid_name = gsub("\\s*\\([^\\)]+\\)", "", valid_name),
         valid_AphiaID = as.character(valid_AphiaID),
         type = "synonym",
         accid = NA,
         source = "WORMS",
         species = str_split(ScientificName, " ", simplify=TRUE)[,2],
         subspecies = case_when(rank == "Subspecies" ~ str_split(ScientificName, " ", simplify=T)[,3],
                                rank %in% c("Forma","Variety") ~  paste(str_split(ScientificName, " ", simplify=T)[,3],
                                                                        str_split(ScientificName, " ", simplify=T)[,4], 
                                                                        sep = " "),
                                rank == "Species" ~ NA_character_),
         subgenus = ifelse(!is.na(subgenus),str_split(subgenus, " ", simplify = T)[,2],subgenus),
         subgenus = gsub("\\s*\\([^\\)]+\\)", "", subgenus)
  ) %>% 
  rename(authorship = authority,
         taxonomicStatus = status,
         taxonRank = rank) %>% 
  select(AphiaID, accid, class, order, family, genus, subgenus, 
         species, subspecies, ScientificName, taxonomicStatus, taxonRank, type,
         authorship, source, valid_AphiaID)


# save synonym file
write.csv2(synonyms, file=paste("outputs/taxonomy/real/BRACHYURANS_SYNONYMS_SUBSPE_",date,".csv", sep=""), quote=FALSE, row.names=TRUE)


################################################
### C. Finalize masterlist from WORMS
################################################

master <- worms2 %>%
  mutate(valid_AphiaID = NA)

identical(names(master), names(synonyms))

master <- rbind(master, synonyms)  

master <- master[order(master$class,master$order,master$family,master$genus,
                       master$species, master$subspecies),]

master$id <- c(1:nrow(master))

# associate right accid and transform subspecies in synonyms
for(i in 1:nrow(master)){
  
  if(master$taxonRank[i]=="subspecies" & master$type[i]=="accepted"){
    # transform subspecies as synonym
    master$type[i] <- "accepted_to_synonym"
    # associate right aphia ID
    v <- which(paste(master$genus[i], master$species[i], sep=" ") == master$ScientificName)
    master$valid_AphiaID[i] <- master$AphiaID[v]
    rm(v)
  }
  
  if(master$type[i] %in% c("synonym", "accepted_to_synonym")){
    v <- which(master$valid_AphiaID[i]==master$AphiaID)
    master$accid[i] <- master$id[v]
    rm(v)
  }
}


master <- master %>% rename(canonical = ScientificName)
# save master file
write.csv(master, file=paste("outputs/taxonomy/real/BRACHYURANS_MASTER_",date,".csv", sep=""), na="NA", row.names=FALSE)


### check subgenera effect
xx <- master %>% filter(accid==0) %>% dplyr::select(canonical) %>% 
  filter(duplicated(canonical))

dim(xx) # ok! no duplicated names with different AphiaID codes


################################################
### D. Synonyms from Sealifebase
################################################

master <- read.csv("outputs/taxonomy/real/BRACHYURANS_MASTER_28MAR2022.csv")

master_accepted <- master %>% filter(accid==0) # 5827

master_syn <- master %>% filter(accid>0) # 5757

slb_syn <- rfishbase::synonyms(species_list = master_accepted$canonical,
                               server = "sealifebase")



################################################
### E. Synonyms from GBIF
################################################
master <- read.csv("outputs/taxonomy/real/BRACHYURANS_MASTER_28MAR2022.csv")
master_accepted <- master %>% filter(accid==0) # 5827
master_syn <- master %>% filter(accid>0) # 5757


# Download of gbif occurrence records based on the following criteria:
# presents records only, all but those of fossils, only those with valid coordinates, all but those with zero coordinate issue

GBIF_raw <- fread(
  file.path("~/Documents/R/Yale_data/GBIF/BRACHYURA.28MAR2022_3/0196387-210914110416597/occurrence.txt"),
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
    "occurrenceStatus"
  )
) %>% 
  filter(basisOfRecord != "FOSSIL_SPECIMEN", # remove fossils
         !is.na(decimalLatitude), # remove records with lat/long
         !is.na(decimalLongitude),
         taxonRank %in% c("SPECIES","SUBSPECIES","VARIETY","UNRANKED"), # remove specimens that are family/genus
         occurrenceStatus == "PRESENT") # remove absence records 

# 169681+640870+301880 records from raw download
# 124699+462093+173909 records after first filter = lots of occurrence points without coordinates


# cleaned taxo from WORMS
current_names <- master %>% select(AphiaID, canonical, accid, id) # 11584 names

# GBIF taxo
gbif_raw_names <- GBIF_raw %>% select(family, genus, subgenus, species, 
                                      specificEpithet, infraspecificEpithet,
                                      taxonRank, taxonomicStatus) %>% 
  rename(family_gbif = family,
         genus_gbif = genus, 
         subgenus_gbif = subgenus,
         species_gbif = species, 
         specificEpithet_gbif = specificEpithet,
         infraspecificEpithet_gbif = infraspecificEpithet,
         taxonRank_gbif = taxonRank, 
         taxonomicStatus_gbif = taxonomicStatus) %>% 
  distinct() # 877+3073+2945 names


###########################################################################################################################################

gbif_raw <- fread(
  file.path("~/Documents/R/Yale_data/GBIF/BRACHYURA.21MAR2022/0187911-210914110416597/occurrence.txt"),
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
    "occurrenceStatus"
  )
) %>% 
  filter(basisOfRecord != "FOSSIL_SPECIMEN", # remove fossils
         !is.na(decimalLatitude), # remove records with lat/long
         !is.na(decimalLongitude),
         taxonRank %in% c("SPECIES","SUBSPECIES","VARIETY","UNRANKED"), # remove specimens that are family/genus
         occurrenceStatus == "PRESENT") # remove absence records 

# 491051 records from raw download
# 415254 records after first filter = lots of occurrence points without coordinates


# cleaned taxo from WORMS
current_names <- master %>% select(AphiaID, canonical, accid, id) # 11416 names

# GBIF taxo
gbif_raw_names <- gbif_raw %>% select(family, genus, subgenus, species, 
                                      specificEpithet, infraspecificEpithet,
                                      taxonRank, taxonomicStatus) %>% 
  rename(family_gbif = family,
         genus_gbif = genus, 
         subgenus_gbif = subgenus,
         species_gbif = species, 
         specificEpithet_gbif = specificEpithet,
         infraspecificEpithet_gbif = infraspecificEpithet,
         taxonRank_gbif = taxonRank, 
         taxonomicStatus_gbif = taxonomicStatus) %>% 
  distinct() # 4829 names

gbif_raw_species <- gbif_raw_names %>% select(species_gbif) %>% distinct() # 4801 species

# anti join of names
gbif_dirty_names <- anti_join(gbif_raw_names, current_names, by = c("species_gbif" = "canonical"))
# 625 unmatched names

gbif_dirty_names <- gbif_dirty_names %>% 
  mutate(taxonRank_corr = taxonRank_gbif,
         taxonRank_corr = ifelse(taxonRank_gbif == "UNRANKED" & !is.na(infraspecificEpithet_gbif),"SUBSPECIES",taxonRank_corr),
         taxonRank_corr = ifelse(taxonRank_gbif == "UNRANKED" & (!is.na(specificEpithet_gbif) & is.na(infraspecificEpithet_gbif)), "SPECIES", taxonRank_corr),
         taxonRank_corr = ifelse(taxonRank_gbif == "UNRANKED" & (is.na(specificEpithet_gbif) & !is.na(genus_gbif)), "GENUS", taxonRank_corr),
         taxonRank_corr = ifelse(taxonRank_gbif == "UNRANKED" & (is.na(genus_gbif) & !is.na(family_gbif)), "FAMILY", taxonRank_corr))

dim(subset(gbif_dirty_names, taxonRank_corr=="UNRANKED")) #0, ok!

# remove newly identified genera and families
gbif_dirty_names <- gbif_dirty_names %>% 
  filter(! taxonRank_corr %in% c("FAMILY", "GENUS"))

dim(gbif_dirty_names) # only 488 names left

gbif_dirty_names <- gbif_dirty_names %>% 
  rename(Family = family_gbif,
         Genus = genus_gbif,
         Subgenus = subgenus_gbif,
         ScientificName = species_gbif,
         Species = specificEpithet_gbif,
         SubSpecies = infraspecificEpithet_gbif)

write.csv(gbif_dirty_names, file = paste0("outputs/taxonomy/real/dirty_names_gbif_AU_",date,".csv"),
          row.names=FALSE)


################################################
### F. Integrate the GBIF synonyms in mastertaxo
################################################

master <- read.csv("outputs/taxonomy/real/BRACHYURANS_MASTER_28MAR2022.csv")

gbif_syn <- read.csv("outputs/taxonomy/real/dirty_names_gbif_au_28mar2022_matched.csv")%>% 
  filter(reason_included %in% c("misspelling", "synonym")) %>% 
  rename(taxonomicStatus = "Taxon.status",
         canonical = `ScientificName.1`) %>% 
  mutate(class = "Malacostraca",
         order = "Decapoda",
         family = `Family.1`,
         genus = `Genus.1`,
         subgenus = `Subgenus.1`,
         species = `Species.1`,
         subspecies = Subspecies,
         AphiaID = NA_integer_,
         valid_AphiaID = ifelse(is.na(valid_AphiaID), AphiaID_accepted, valid_AphiaID),
         accid = NA_integer_,
         taxonomicStatus = ifelse(is.na(taxonomicStatus), "unknown", taxonomicStatus),
         type = "synonym",
         authorship = NA_character_,
         source = "GBIF_28MAR2022",
         id = NA_integer_,
         taxonRank = ifelse(is.na(subspecies), "species", "subspecies")) %>% 
  select(AphiaID, accid, class, order, family, genus, subgenus, species,
         subspecies, canonical, taxonomicStatus, taxonRank, type, authorship,
         source, valid_AphiaID, id)

gbif_syn <- unique(gbif_syn)

nrow(gbif_syn) # 60 synonyms found in GBIF for Brachyura
identical(names(gbif_syn), names(master))

# add GBIF synonyms to the mastertaxonomy
master_gbif <- rbind(master, gbif_syn)
master_gbif$id <- c(1:nrow(master_gbif))

# associate right accid and transform subspecies in synonyms
s <- nrow(master_gbif)-nrow(gbif_syn)+1
for(i in s:nrow(master_gbif)){
  
  if(master_gbif$type[i] %in% c("synonym", "accepted_to_synonym")){
    v <- which(master_gbif$valid_AphiaID[i]==master_gbif$AphiaID)
    master_gbif$accid[i] <- master_gbif$id[v]
    rm(v)
  }
}


# save master file
write.csv(master_gbif, file=paste("outputs/taxonomy/real/BRACHYURANS_MASTER_GBIF_",date,".csv", sep=""), na="NA", row.names=FALSE)



################################################
### G. Synonyms from OBIS
################################################

master <- read.csv("outputs/taxonomy/real/BRACHYURANS_MASTER_28MAR2022.csv")
master_accepted <- master %>% filter(accid==0) # 5827
master_syn <- master %>% filter(accid>0) # 5757

obis_raw <- fread(
  file.path("~/Documents/R/Yale_data/OBIS/obis_decapoda/obis_decapoda_20220114.csv"),
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
             "taxonomicstatus")
) # 4,188,355 raws

unique(obis_raw$occurrencestatus) # all are present or unknown
unique(obis_raw$absence) # all are false, so only presence records
unique(obis_raw$taxonrank) # need to remove all non-species, subspecies and alike
unique(obis_raw$taxonomicstatus) # need to clean
unique(obis_raw$basisofrecord) # apparently no fossils there
summary(obis_raw$decimallatitude) # no NA
summary(obis_raw$decimallongitude) # no NA

obis_raw2 <- obis_raw %>% 
  filter(infraorder == "Brachyura", # 671,966 raws
         taxonrank %in% c("","Species","Subspecies")) %>%
  distinct() # 565.721

aphia_obis <- obis_raw2 %>% 
  select(aphiaid, order, family, genus, subgenus, species, subspecies, scientificnameauthorship, scientificname, taxonrank) %>% 
  rename(AphiaID = aphiaid) %>% 
  distinct()

# anti join with the master list
obis_unmatched <- anti_join(aphia_obis, master, by="AphiaID")

# save the unmatched names
write.csv2(obis_unmatched, file = paste0("outputs/taxonomy/real/unmatched_aphia_obis_",date,".csv"),
           row.names=FALSE)


# anti join with the master list
names_obis <- obis_raw2 %>% 
  select(aphiaid, order, family, genus, subgenus, species, subspecies, scientificnameauthorship, scientificname, taxonrank) %>% 
  rename(canonical = scientificname) %>% 
  distinct()
obis_unmatched <- anti_join(names_obis, master, by="canonical")
# more complex to match with canonical because would need to change the subgenus thing again
# save the unmatched names

#anti join obis and gbif synonyms

unmatched_obis <- read_csv("outputs/taxonomy/real/unmatched_aphia_obis_03may2022_matched.csv")

unmatched_gbif <- read_csv("outputs/taxonomy/real/dirty_names_gbif_au_28mar2022_matched.csv")

potent_obis_unmatched <- anti_join(unmatched_obis, unmatched_gbif, by="AphiaID")

write.csv(potent_obis_unmatched, file = paste0("outputs/taxonomy/real/unmatched_aphia_gbif_obis_",date,".csv"),
          row.names=FALSE)

##############################################################################################################
##############################################################################################################
#### 2. ANOMURANS
##############################################################################################################
##############################################################################################################


# Load worms
worms <- read_delim("~/Yale University/MOL-inverts/WORMS/WoRMS_download_2020-10-01/taxon.txt", 
                    "\t", escape_double = FALSE, trim_ws = TRUE,
                    col_types = cols(specificEpithet = col_character(),
                                     infraspecificEpithet = col_character(),
                                     license = col_character(),
                                     datasetName = col_character()))

# Load biome info from worms
biome <- read_delim('~/Yale University/MOL-inverts/WORMS/WoRMS_download_2020-10-01/speciesprofile.txt',
                    "\t", escape_double = FALSE, trim_ws = TRUE)

worms <- left_join(worms, biome, by = 'taxonID')

# Sub-select crabs
# Corresponds to Domain:	Eukaryota
# Kingdom:	Animalia
# Phylum:	Arthropoda
# Subphylum:	Crustacea
# Class:	Malacostraca
# Order:	Decapoda
# Suborder:	Pleocyemata
# Infraorder:	Anomura 3101 spp on Worms website

wannabecrabfamilies <- c('Chirostylidae','Eumunididae','Kiwaidae','Sternostylidae','Galatheidae',
                         'Munididae','Munidopsidae',
                         'Porcellanidae','Albuneidae','Blepharipodidae','Hippidae','Hapalogastridae',
                         'Lithodidae','Lomisidae',
                         'Calcinidae','Coenobitidae','Diogenidae','Paguridae','Parapaguridae','Pylochelidae',
                         'Pylojacquesidae','Xylopaguridae')


# for all Anomurans, taxon ranks are: family, genus, species, subfamily, subgenus, form, tribe
# tribes are above genera, so they can be discarded
# there is one form: http://www.marinespecies.org/aphia.php?p=taxdetails&id=952997
# and one form within that species across all Anomurans, so it will be synonymised as subspecies
# there is no subspecies in Anomurans!!
worms <- worms %>% 
  filter(order == 'Decapoda',
         isMarine == 1,
         family %in% wannabecrabfamilies,
         taxonomicStatus =="accepted",
         taxonRank %in% c('Species','Subspecies',"Form"))


################################################
### A. Formatting Master list
################################################
worms2 <- worms %>%  
  mutate(AphiaID = gsub("[a-zA-Z:. ]", "", taxonID),
         # re-write species
         species = specificEpithet,
         subspecies = infraspecificEpithet,
         # re-write spp names
         ScientificName = if_else(!is.na(subgenus),
                                  paste0(genus, " (", subgenus, ") ", specificEpithet),
                                  paste(genus, specificEpithet, sep=" ")),
         ScientificName = if_else(!is.na(infraspecificEpithet),
                                  paste(ScientificName, infraspecificEpithet, sep=" "),
                                  ScientificName),
         AuthorshipYear = gsub("\\D","", scientificNameAuthorship),
         AuthorshipYear = as.numeric(case_when(nchar(AuthorshipYear)>4 ~ substr(AuthorshipYear, start=1, stop=4),
                                               nchar(AuthorshipYear)==4 ~ AuthorshipYear)),
         Authorship = gsub(", [0-9]+", "", scientificNameAuthorship),
         Authorship = gsub("[()]", "", Authorship),
         Authorship = gsub("[A-Z]. ", "", Authorship),
         authorship = paste(Authorship, AuthorshipYear, sep=" "),
         authorship = scientificNameAuthorship,
         accid = 0,
         source = "WoRMS",
         type = "accepted") %>% 
  select(AphiaID, accid, class, order, family, genus, subgenus, species, subspecies, ScientificName, taxonomicStatus, 
         taxonRank, type, authorship, source) %>%
  distinct()

# removing species with different AphiaID, same ScientificName but with/without subgenera
# xx <- subset(worms2, duplicated(canonical))
# worms2 <- worms2 %>%
#   filter(!AphiaID %in% xx$AphiaID)
#no more duplicates with author, aphiaID and taxonomic status

# order columns
worms2 <- worms2[order(worms2$class,worms2$order,worms2$family,worms2$genus,worms2$species, worms2$subspecies),]

# 3,095 species/subspecies!
write.csv(worms2, file=paste('outputs/taxonomy/wannabe/ANOMURANS_MASTERLIST_SUBSPE_',date,'.csv', sep=''), row.names=FALSE)


################################################
### B. Synonym list, using worms2
################################################
# Select taxon id
splist <- worms2$AphiaID

# Check species list for synonyms using taxon id
temp <- taxize::synonyms(splist,
                         db='worms')

# Convert raw species list to data.frame()
mylist <- synonyms_df(temp)

# Write file with synonyms since it takes a long time to compile them!!
save(mylist, file="outputs/taxonomy/wannabe/synonyms.RData")

# Finalize the synonym file
load(file="outputs/taxonomy/wannabe/synonyms.RData")
unique(mylist$match_type) # good
unique(mylist$status) # only unaccepted names

synonyms <- mylist %>% 
  #filter(rank %in% c('Species','Subspecies')) %>% # 6% of the synonyms are forms and varieties, 
  # we can keep them in case
  # remove sub genera in parenthesis
  mutate(subgenus = ifelse(str_detect(scientificname, "\\)")==TRUE,
                           paste0(str_split(scientificname, "\\)", simplify=T)[,1],")"),
                           NA_character_),
         ScientificName = gsub("\\s*\\([^\\)]+\\)", "", scientificname),
         valid_name = gsub("\\s*\\([^\\)]+\\)", "", valid_name),
         valid_AphiaID = as.character(valid_AphiaID),
         type = "synonym",
         accid = 0,
         source = "WORMS",
         infraorder = "Anomura",
         species = str_split(ScientificName, " ", simplify=TRUE)[,2],
         subspecies = case_when(rank == "Subspecies" ~ str_split(ScientificName, " ", simplify=T)[,3],
                                rank %in% c("Forma","Variety") ~  paste(str_split(ScientificName, " ", simplify=T)[,3],
                                                                        str_split(ScientificName, " ", simplify=T)[,4], 
                                                                        sep = " "),
                                rank == "Species" ~ NA_character_)
  ) %>% 
  rename(authorship = authority,
         taxonomicStatus = status,
         taxonRank = rank) %>% 
  select(AphiaID, accid, class, order, family, genus, subgenus, 
         species, subspecies, ScientificName, taxonomicStatus, taxonRank, type,
         authorship, source, valid_AphiaID)

# save synonym file
write.csv2(synonyms, file=paste("outputs/taxonomy/wannabe/ANOMURANS_SYNONYMS_SUBSPE_",date,".csv", sep=""), quote=FALSE, row.names=TRUE)


################################################
### C. Finalize masterlist from WORMS
################################################

master <- worms2 %>%
  mutate(valid_AphiaID = NA)

identical(names(master), names(synonyms)) #TRUE 

master <- rbind(master, synonyms)  

master <- master[order(master$class,master$order,master$family,master$genus,
                       master$species, master$subspecies),]
master$id <- c(1:nrow(master))

# associate right accid and transform subspecies in synonyms
for(i in 1:nrow(master)){
  
  if(master$taxonRank[i] %in% c("subspecies","form") & master$type[i]=="accepted"){
    # transform subspecies as synonym
    master$type[i] <- "accepted_to_synonym"
    # associate right aphia ID
    v <- which(paste(master$genus[i], master$species[i], sep=" ") == master$ScientificName)
    master$valid_AphiaID[i] <- master$AphiaID[v]
    rm(v)
  }
  
  if(master$type[i] %in% c("synonym", "accepted_to_synonym")){
    v <- which(master$valid_AphiaID[i]==master$AphiaID)
    master$accid[i] <- master$id[v]
    rm(v)
  }
}

master <- master %>% rename(canonical = ScientificName)
# save master file
write.csv(master, file=paste("outputs/taxonomy/wannabe/ANOMURANS_MASTER_",date,".csv", sep=""), na="NA", row.names=FALSE)



### check subgenera effect
xx <- master %>% filter(accid==0) %>% dplyr::select(canonical) %>% 
  filter(duplicated(canonical))
dim(xx) # ok! no duplicated names with different AphiaID codes




################################################
### E. Synonyms from GBIF
################################################

master <- read.csv("outputs/taxonomy/wannabe/ANOMURANS_MASTER_29APR2022.csv")
master_accepted <- master %>% filter(accid==0) # 3191
master_syn <- master %>% filter(accid>0) # 1754


# Download of gbif occurrence records based on the following criteria:
# presents records only, all but those of fossils, only those with valid coordinates, all but those with zero coordinate issue
gbif_raw <- fread(
  file.path("~/Documents/R/Yale_data/GBIF/ANOMURA.02MAY2022/0260413-210914110416597/occurrence.txt") ,
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
    "occurrenceStatus"
  )
) %>% 
  filter(basisOfRecord != "FOSSIL_SPECIMEN", # remove fossils
         !is.na(decimalLatitude), # remove records with lat/long
         !is.na(decimalLongitude),
         taxonRank %in% c("SPECIES","SUBSPECIES","VARIETY","UNRANKED"), # remove specimens that are family/genus
         occurrenceStatus == "PRESENT") # remove absence records 

# 384,639 records from raw download
# 233,023 records after first filter = lots of occurrence points without coordinates


# cleaned taxo from WORMS
current_names <- master %>% select(AphiaID, canonical, accid, id) # 4945 names

# GBIF taxo
gbif_raw_names <- gbif_raw %>% select(family, genus, subgenus, species, 
                                      specificEpithet, infraspecificEpithet,
                                      taxonRank, taxonomicStatus) %>% 
  rename(family_gbif = family,
         genus_gbif = genus, 
         subgenus_gbif = subgenus,
         species_gbif = species, 
         specificEpithet_gbif = specificEpithet,
         infraspecificEpithet_gbif = infraspecificEpithet,
         taxonRank_gbif = taxonRank, 
         taxonomicStatus_gbif = taxonomicStatus) %>% 
  distinct() # 3462 names
gbif_raw_species <- gbif_raw_names %>% select(species_gbif) %>% distinct() # 2607 species

# anti join of names
gbif_dirty_names <- anti_join(gbif_raw_names, current_names, by = c("species_gbif" = "canonical"))
# 76 unmatched names

gbif_dirty_names <- gbif_dirty_names %>% 
  mutate(taxonRank_corr = taxonRank_gbif,
         taxonRank_corr = ifelse(taxonRank_gbif == "UNRANKED" & !is.na(infraspecificEpithet_gbif),"SUBSPECIES",taxonRank_corr),
         taxonRank_corr = ifelse(taxonRank_gbif == "UNRANKED" & (!is.na(specificEpithet_gbif) & is.na(infraspecificEpithet_gbif)), "SPECIES", taxonRank_corr),
         taxonRank_corr = ifelse(taxonRank_gbif == "UNRANKED" & (is.na(specificEpithet_gbif) & !is.na(genus_gbif)), "GENUS", taxonRank_corr),
         taxonRank_corr = ifelse(taxonRank_gbif == "UNRANKED" & (is.na(genus_gbif) & !is.na(family_gbif)), "FAMILY", taxonRank_corr))

dim(subset(gbif_dirty_names, taxonRank_corr=="UNRANKED")) #0, ok!

# remove newly identified genera and families
gbif_dirty_names <- gbif_dirty_names %>% 
  filter(! taxonRank_corr %in% c("FAMILY", "GENUS"))

dim(gbif_dirty_names) # only 38 names left

gbif_dirty_names <- gbif_dirty_names %>% 
  rename(Family = family_gbif,
         Genus = genus_gbif,
         Subgenus = subgenus_gbif,
         ScientificName = species_gbif,
         Species = specificEpithet_gbif,
         SubSpecies = infraspecificEpithet_gbif)

write.csv(gbif_dirty_names, file = paste0("outputs/taxonomy/wannabe/dirty_names_gbif_",date,".csv"),
          row.names=FALSE)


################################################
### F. Integrate the GBIF synonyms in mastertaxo
################################################

master <- read.csv("outputs/taxonomy/wannabe/ANOMURANS_MASTER_29APR2022.csv")

gbif_syn <- read.csv("outputs/taxonomy/wannabe/dirty_names_gbif_29apr2022_matched.csv") %>% 
  filter(reason_included %in% c("misspelling", "synonym")) %>% 
  rename(taxonomicStatus = "Taxon.status",
         canonical = "ScientificName") %>% 
  mutate(class = "Malacostraca",
         order = "Decapoda",
         infraorder = "Anomura",
         family = Family,
         genus = Genus,
         subgenus = Subgenus,
         species = Species,
         subspecies = Subspecies,
         AphiaID = NA_integer_,
         valid_AphiaID = ifelse(is.na(valid_AphiaID), AphiaID_accepted, valid_AphiaID),
         accid = NA_integer_,
         taxonomicStatus = ifelse(is.na(taxonomicStatus), "unknown", taxonomicStatus),
         type = "synonym",
         authorship = NA_character_,
         source = "GBIF_02MAY2022",
         id = NA_integer_,
         taxonRank = ifelse(is.na(subspecies), "species", "subspecies")) %>% 
  select(AphiaID, accid, class, order, family, genus, subgenus, species,
         subspecies, canonical, taxonomicStatus, taxonRank, type, authorship,
         source, valid_AphiaID, id)

gbif_syn <- unique(gbif_syn)


nrow(gbif_syn) # 8 synonyms found in GBIF for Anomura
identical(names(gbif_syn), names(master))

# add GBIF synonyms to the mastertaxonomy
master_gbif <- rbind(master, gbif_syn)
master_gbif$id <- c(1:nrow(master_gbif))

# associate right accid and transform subspecies in synonyms
s <- nrow(master_gbif)-nrow(gbif_syn)+1
for(i in s:nrow(master_gbif)){
  
  if(master_gbif$type[i] %in% c("synonym", "accepted_to_synonym")){
    v <- which(master_gbif$valid_AphiaID[i]==master_gbif$AphiaID)
    master_gbif$accid[i] <- master_gbif$id[v]
    rm(v)
  }
}


# save master file
write.csv(master_gbif, file=paste("outputs/taxonomy/wannabe/ANOMURANS_MASTER_GBIF_",date,".csv", sep=""), na="NA", row.names=FALSE)



################################################
### G. Synonyms from OBIS
################################################

master <- read.csv("outputs/taxonomy/wannabe/ANOMURANS_MASTER_29APR2022.csv")
master_accepted <- master %>% filter(accid==0) # 3191
master_syn <- master %>% filter(accid>0) # 1754

obis_raw <- fread(
  file.path("~/Documents/R/Yale_data/OBIS/obis_decapoda/obis_decapoda_20220114.csv"),
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
             "taxonomicstatus")
) # 4,188,355 raws

unique(obis_raw$occurrencestatus) # all are present or unknown
unique(obis_raw$absence) # all are false, so only presence records
unique(obis_raw$taxonrank) # need to remove all non-species, subspecies and alike
unique(obis_raw$taxonomicstatus) # need to clean
unique(obis_raw$basisofrecord) # apparently no fossils there
summary(obis_raw$decimallatitude) # no NA
summary(obis_raw$decimallongitude) # no NA

obis_raw2 <- obis_raw %>% 
  filter(infraorder == "Anomura", #241,558 raws
         taxonrank %in% c("","Species","Subspecies")) %>%  # 172,561
  distinct() #172,561

aphia_obis <- obis_raw2 %>% 
  select(aphiaid, order, family, genus, species, subspecies, scientificnameauthorship, scientificname, taxonrank) %>% 
  rename(AphiaID = aphiaid) %>% 
  distinct()

aphia_obis <- aphia_obis %>% mutate(AphiaID = as.character(AphiaID))

# anti join with the master list
obis_unmatched <- anti_join(aphia_obis, master, by="AphiaID")

# save the unmatched names
write.csv2(obis_unmatched, file = paste0("outputs/taxonomy/wannabe/unmatched_aphia_obis_",date,".csv"),
           row.names=FALSE)


# anti join with the master list
names_obis <- obis_raw2 %>% 
  select(aphiaid, order, infraorder, family, genus, species, subspecies, scientificnameauthorship, scientificname, taxonrank) %>% 
  rename(canonical = scientificname) %>% 
  distinct()
obis_unmatched <- anti_join(names_obis, master, by="canonical")
# more complex to match with canonical because would need to change the subgenus thing again
# save the unmatched names


################################################
### H. Integrate
################################################
