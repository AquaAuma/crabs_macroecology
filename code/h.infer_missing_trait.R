rm(list = ls())

# set date
date <- '22JUL2021'

# load libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(readxl)
library(stringr)
library(egg)


#############################
### A. Load all data required
#############################
# Brachyura
masterlist_real <- read.csv('outputs_MOL/real/taxo/BRACHYURANS_MASTER_23MAR2021.csv') %>% 
  filter(accid==0) # remove synonyms from the list

# Load expert knowledge files
ECOL_real <- read.csv("outputs_MOL/real/trait/ECOLOGY_BRACHYURANS_14JUL2021.csv")
BIOL_real <- read.csv("outputs_MOL/real/trait/BIOLOGY_BRACHYURANS_14JUL2021.csv")

# Anomura
masterlist_wanna <- read.csv('outputs_MOL/wannabe/taxo/ANOMURANS_MASTER_23MAR2021.csv') %>% 
  filter(accid==0) # remove synonyms from the list

# Load expert knowledge files
ECOL_wanna <- read.csv("outputs_MOL/wannabe/trait/ECOLOGY_ANOMURANS_14JUL2021.csv")
BIOL_wanna <- read.csv("outputs_MOL/wannabe/trait/BIOLOGY_ANOMURANS_14JUL2021.csv")

# make all common data frame
masterlist <- rbind(masterlist_real, masterlist_wanna)
ecology <- rbind(ECOL_real, ECOL_wanna)
biology <- rbind(BIOL_real, BIOL_wanna)
rm(ECOL_wanna, ECOL_real, BIOL_real, BIOL_wanna, masterlist_real, masterlist_wanna)


############################
### B. Recode habitat traits
############################
# Vertical = Oceanic/Epipelagic/Mesopelagic/Bathypelagic/Hadopelagic/Benthic/Demersal/Pelagic/Neritic
# Coastal = Intertidal/SubLittoral/LittoralZone
# HabitatSpec = Mangroves/ReefFlats/Vents/Seamounts/DeepWaterCorals/SeaGrassBeds

ecology <- ecology %>% 
  dplyr::select(SpecCode, Species, Neritic, SupraLittoralZone, Saltmarshes, 
                LittoralZone, TidePools, Saltmarshes, Intertidal, SubLittoral, 
                Oceanic, Epipelagic, Mesopelagic, Bathypelagic, Abyssopelagic,
                Hadopelagic, Estuaries, Mangroves, MarshesSwamps, Benthic, Sessile, 
                Mobile, Demersal, Endofauna, Megabenthos, Macrobenthos, Meiobenthos,
                Pelagic, SeaGrassBeds, CoralReefs, ReefFlats, ReefExclusive, Lagoons,
                Vents, Seamounts, DeepWaterCorals) %>% 
  filter(!is.na(Species)) %>% 
  distinct()

xx <- subset(ecology, duplicated(ecology$Species))
yy <- subset(ecology, Species %in% xx$Species)
yy[yy$Species=="Epigrapsus politus",]$Neritic <- 1
yy[yy$Species=="Epigrapsus politus",]$Benthic <- 1
yy[yy$Species=="Chaceon ramosae",]$Benthic <- 1
yy[yy$Species=="Chaceon ramosae",]$Mobile <- 1
yy[yy$Species=="Leptomithrax tuberculatus",]$Benthic <- 1
yy[yy$Species=="Actumnus digitalis",]$Neritic <- 1
yy[yy$Species=="Sarmatium crassum",]$Benthic <- 1
yy[yy$Species=="Sarmatium crassum",]$Neritic <- 1
yy[yy$Species=="Leptodius sanguineus",]$Neritic <- 1
yy[yy$Species=="Leptodius sanguineus",]$Mobile <- 1
yy[yy$Species=="Leptodius sanguineus",]$CoralReefs <- 1
yy <- yy %>% distinct()

ecology <- ecology %>% filter(!Species %in% xx$Species)
ecology <- rbind(ecology, yy)
rm(xx, yy)

biology <- biology %>% 
  dplyr::select(SpecCode, Species, DemersPelag, DepthRangeShallow, DepthRangeDeep) %>% 
  filter(!is.na(Species)) %>% 
  distinct()

habitat_traits <- full_join(ecology, biology, by=c("Species", "SpecCode"))

# function to transform NA into null objects


master_traits <- left_join(masterlist, habitat_traits, by=c("canonical" = "Species")) %>% 
  mutate(Benthic = ifelse(Megabenthos==1, 1, Benthic),
         Benthic = ifelse(Macrobenthos==1, 1, Benthic),
         Benthic = ifelse(Meiobenthos==1, 1, Benthic),
         Reefs = ifelse(CoralReefs==1, 1, 0),
         Reefs = ifelse(ReefFlats==1, 1, Reefs),
         #Reefs = ifelse(ReefExclusive==TRUE, 1, Reefs), # there is no reef exclusive for crabs apparently
         Intertidal = ifelse(TidePools==1, 1, Intertidal),
         LittoralZone = ifelse(Intertidal==1, 1, LittoralZone),
         Wetland = ifelse(Saltmarshes==1, 1, 0),
         Wetland = ifelse(MarshesSwamps==1, 1, Wetland),
         vertical_habitat = NA_character_,
         mobility = NA_character_,
         distance_coast_habitat = NA_character_,
         specific_association = NA_character_) %>% 
  select(-Endofauna, -Megabenthos, -Macrobenthos, -CoralReefs, -ReefFlats,
         -Meiobenthos, -TidePools, -Saltmarshes, -MarshesSwamps, -DemersPelag,
         -Intertidal) %>% 
  rename(supra_littoral = SupraLittoralZone,
         littoral = LittoralZone,
         sub_littoral = SubLittoral,
         oceanic = Oceanic,
         epipelagic = Epipelagic,
         mesopelagic = Mesopelagic,
         bathypelagic = Bathypelagic, 
         abyssopelagic = Abyssopelagic,
         hadopelagic = Hadopelagic,
         estuaries = Estuaries,
         mangroves = Mangroves,
         benthic = Benthic,
         sessile = Sessile,
         mobile = Mobile,
         demersal = Demersal,
         pelagic = Pelagic,
         seagrasses = SeaGrassBeds,
         reef_exclusive = ReefExclusive,
         lagoons = Lagoons,
         vents = Vents,
         seamounts = Seamounts,
         deep_corals = DeepWaterCorals,
         depth_range_deep = DepthRangeDeep,
         depth_range_shallow = DepthRangeShallow,
         reefs = Reefs,
         wetland = Wetland,
         neritic = Neritic)


for(s in 1:nrow(master_traits)){
  
  # vertical habitat
  trait_v <- master_traits[s,c(25:29,32,35:36)] %>%
    pivot_longer(cols = 1:8) %>% 
    filter(value==1)
  if(nrow(trait_v>0)){master_traits$vertical_habitat[s] <- paste(trait_v$name, collapse="/")}
  
  # distance to coast
  trait_c <- master_traits[s,c(20:24)] %>%
    pivot_longer(cols = 1:5) %>% 
    filter(value==1)
  if(nrow(trait_c>0)){master_traits$distance_coast_habitat[s] <- paste(trait_c$name, collapse="/")}
  
  # specific association
  trait_a <- master_traits[s,c(30:31,37,39:42,45:46)] %>%
    pivot_longer(cols = 1:9) %>% 
    filter(value==1)
  if(nrow(trait_a>0)){master_traits$specific_association[s] <- paste(trait_a$name, collapse="/")}
  
  # mobility
  trait_m <- master_traits[s,c(33:34)] %>%
    pivot_longer(cols = 1:2) %>% 
    filter(value==1)
  if(nrow(trait_m>0)){master_traits$mobility[s] <- paste(trait_m$name, collapse="/")}
  
  rm(trait_v, trait_c, trait_a, trait_m)
}

master_traits <- master_traits %>% 
  select(-c(20:42), -c(45:46)) %>% 
  mutate(vertical_habitat_clean = ifelse(vertical_habitat %in% c("benthic/demersal/pelagic","benthic/pelagic","epipelagic/benthic","epipelagic/benthic/demersal","epipelagic/benthic/pelagic",
                                                                 "epipelagic/mesopelagic/bathypelagic/benthic","epipelagic/mesopelagic/bathypelagic/benthic/demersal",
                                                                 "epipelagic/mesopelagic/bathypelagic/benthic/pelagic","epipelagic/mesopelagic/benthic","epipelagic/mesopelagic/benthic",
                                                                 "epipelagic/mesopelagic/benthic/demersal","mesopelagic/bathypelagic/benthic","mesopelagic/bathypelagic/benthic/demersal",
                                                                 "mesopelagic/benthic","mesopelagic/benthic/demersal","bathypelagic/benthic"),"benthopelagic",vertical_habitat),
         vertical_habitat_clean = ifelse(vertical_habitat_clean=="benthic/demersal","demersal",vertical_habitat_clean),
         vertical_habitat_clean = ifelse(vertical_habitat_clean=="epipelagic/pelagic","epipelagic",vertical_habitat_clean),
         vertical_habitat_clean = ifelse(vertical_habitat_clean=="epipelagic/mesopelagic/pelagic","epipelagic/mesopelagic",vertical_habitat_clean),
         vertical_habitat_clean = ifelse(vertical_habitat_clean %in% c("epipelagic/mesopelagic","epipelagic/mesopelagic/bathypelagic","mesopelagic/bathypelagic",
                                                                       "epipelagic","mesopelagic","bathypelagic"),"pelagic",vertical_habitat_clean),
         distance_coast = ifelse(distance_coast_habitat %in% c("littoral","littoral/sub_littoral","sub_littoral","neritic"),"neritic",NA_character_),
         distance_coast = ifelse(str_detect(distance_coast_habitat, pattern="neritic")==TRUE,"neritic",distance_coast),
         distance_coast = ifelse((str_detect(distance_coast_habitat, pattern="neritic") & str_detect(distance_coast_habitat, pattern="oceanic"))==TRUE, "neritic/oceanic",distance_coast),
         distance_coast = ifelse(distance_coast_habitat == "oceanic","oceanic",distance_coast),
         distance_coast_benthic = case_when(distance_coast_habitat=="littoral/sub_littoral" ~ "neritic",
                                            distance_coast_habitat=="littoral" ~ "littoral",
                                            distance_coast_habitat=="neritic" ~ "neritic",
                                            distance_coast_habitat=="neritic/littoral/oceanic" ~ "littoral",
                                            distance_coast_habitat=="neritic/littoral/sub_littoral" ~ "neritic",
                                            distance_coast_habitat=="neritic/littoral/sub_littoral/oceanic" ~ "neritic",
                                            distance_coast_habitat=="neritic/oceanic" ~ "neritic",
                                            distance_coast_habitat=="neritic/sub_littoral" ~ "sub_littoral",
                                            distance_coast_habitat=="neritic/sub_littoral/oceanic" ~ "sub_littoral",
                                            distance_coast_habitat=="neritic/supra_littoral" ~ "supra_littoral",
                                            distance_coast_habitat=="neritic/subra_littoral_littoral" ~ "supra_littoral/littoral",
                                            distance_coast_habitat=="neritic/supra_littoral/littoral/sub_littoral" ~ "supra_littoral/neritic",
                                            distance_coast_habitat=="oceanic" ~ NA_character_,
                                            distance_coast_habitat=="sub_littoral" ~ "sub_littoral",
                                            TRUE ~ NA_character_,
         )) %>% 
  rename(vertical_habitat_1 = vertical_habitat,
         vertical_habitat_2 = vertical_habitat_clean,
         distance_broad = distance_coast,
         distance_coastal = distance_coast_benthic)

write.csv(master_traits, file = "outputs_MOL/crabs/traits/master_traits_22JUL2021.csv", 
          row.names = FALSE)


#################################
### C. Summary of existing traits
#################################

### 1. Exiting traits
# is there a min. depth range?
is_there_min_depth <- master_traits %>% 
  mutate(is_there = ifelse(!is.na(depth_range_shallow),TRUE,FALSE)) %>% 
  group_by(is_there) %>% 
  summarize(number = length(is_there)) %>% 
  ggplot(aes(x = is_there, y = number)) + geom_bar(stat = "identity") +
  theme_bw() + ylab("Number of Species") + xlab("Min. depth?")

# is there a max. depth range?
is_there_max_depth <- master_traits %>% 
  mutate(is_there = ifelse(!is.na(depth_range_deep),TRUE,FALSE)) %>% 
  group_by(is_there) %>% 
  summarize(number = length(is_there)) %>% 
  ggplot(aes(x = is_there, y = number)) + geom_bar(stat = "identity") +
  theme_bw() + ylab("") + xlab("Max. depth?")

# is there a vertical habitat?
is_there_vert <- master_traits %>% 
  mutate(is_there = ifelse(!is.na(vertical_habitat_1),TRUE,FALSE)) %>% 
  group_by(is_there) %>% 
  summarize(number = length(is_there)) %>% 
  ggplot(aes(x = is_there, y = number)) + geom_bar(stat = "identity") +
  theme_bw() + ylab("") + xlab("Vertical habitat?")

# is there a distance habitat?
is_there_dist <- master_traits %>% 
  mutate(is_there = ifelse(!is.na(distance_coast_habitat),TRUE,FALSE)) %>% 
  group_by(is_there) %>% 
  summarize(number = length(is_there)) %>% 
  ggplot(aes(x = is_there, y = number)) + geom_bar(stat = "identity") +
  theme_bw() + ylab("Number of Species") + xlab("Distance_coast habitat?")


# is there a specific association trait?
is_there_asso <- master_traits %>% 
  mutate(is_there = ifelse(!is.na(specific_association),TRUE,FALSE)) %>% 
  group_by(is_there) %>% 
  summarize(number = length(is_there)) %>% 
  ggplot(aes(x = is_there, y = number)) + geom_bar(stat = "identity") +
  theme_bw() + ylab("") + xlab("Specific association?")

# is there a mobility trait?
is_there_mobi <- master_traits %>% 
  mutate(is_there = ifelse(!is.na(mobility),TRUE,FALSE)) %>% 
  group_by(is_there) %>% 
  summarize(number = length(is_there)) %>% 
  ggplot(aes(x = is_there, y = number)) + geom_bar(stat = "identity") +
  theme_bw() + ylab("") + xlab("Mobility trait?")

# summary of existing habitat traits
egg::ggarrange(is_there_min_depth, is_there_max_depth, is_there_vert,
               is_there_dist, is_there_asso, is_there_mobi,
               labels = c("","","","","",""),
               nrow=2)


### 2. Summary of categories
# distribution of min. depth
distri_min_depth <- master_traits %>% 
  filter(!is.na(depth_range_shallow)) %>% 
  ggplot() + geom_histogram(aes(x = depth_range_shallow)) +
  theme_bw() + ylab("Number of species") + xlab("Min. depth")


# distribution max. depth
distri_max_depth <- master_traits %>% 
  filter(!is.na(depth_range_deep)) %>% 
  ggplot() + geom_histogram(aes(x = depth_range_deep)) +
  theme_bw() + ylab("") + xlab("Max. depth")

egg::ggarrange(distri_min_depth, distri_max_depth, 
               labels=c("",""), nrow=1)

# levels vertical habitat
distri_vert <- master_traits %>% 
  mutate(pelagic = ifelse(str_detect(vertical_habitat, pattern = "pelagic")==TRUE,TRUE,FALSE),
         epipelagic = ifelse(str_detect(vertical_habitat, pattern = "epipelagic")==TRUE,TRUE,FALSE),
         mesopelagic = ifelse(str_detect(vertical_habitat, pattern = "mesopelagic")==TRUE,TRUE,FALSE),
         bathypelagic = ifelse(str_detect(vertical_habitat, pattern = "bathypelagic")==TRUE,TRUE,FALSE),
         abyssopelagic = ifelse(str_detect(vertical_habitat, pattern = "abyssopelagic")==TRUE,TRUE,FALSE),
         hadopelagic = ifelse(str_detect(vertical_habitat, pattern = "hadopelagic")==TRUE,TRUE,FALSE),
         demersal = ifelse(str_detect(vertical_habitat, pattern = "demersal")==TRUE,TRUE,FALSE),
         benthic = ifelse(str_detect(vertical_habitat, pattern = "benthic")==TRUE,TRUE,FALSE),
         benthic_pelagic = ifelse(str_detect(vertical_habitat, pattern = "benthic")==TRUE & str_detect(vertical_habitat, pattern = "pelagic")==TRUE,TRUE,FALSE)
  ) %>% 
  pivot_longer(cols=27:35, names_to = "vertical_habitat_u", values_to = "test") %>% 
  filter(test==TRUE) %>% 
  group_by(vertical_habitat_u) %>% 
  summarise(number = length(test)) %>% 
  ggplot() + geom_bar(aes(x = vertical_habitat_u, y = number), stat = "identity") +
  theme_bw() + xlab("") + ylab("Number of species") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# update after change in master_traits
master_traits %>%
  filter(vertical_habitat_2 != "NA") %>% 
  group_by(vertical_habitat_2) %>% 
  summarise(number = length(vertical_habitat_2)) %>% 
  ggplot() + geom_bar(aes(x = vertical_habitat_2, y = number), stat = "identity",
                      fill = "midnightblue") +
  theme_minimal() + xlab("") + ylab("Number of species") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(text = element_text(size = 24)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(expand = c(0,0))



distri_vert_clean <- master_traits2 %>% 
  mutate(pelagic = ifelse(vertical_habitat_clean=="pelagic",TRUE,FALSE),
         benthic = ifelse(vertical_habitat_clean=="benthic",TRUE,FALSE),
         demersal = ifelse(vertical_habitat_clean=="demersal",TRUE,FALSE),
         benthopelagic = ifelse(vertical_habitat_clean=="benthopelagic",TRUE,FALSE)
  ) %>% 
  pivot_longer(cols=27:30, names_to = "vertical_habitat_u", values_to = "test") %>% 
  filter(test==TRUE) %>% 
  group_by(vertical_habitat_u) %>% 
  summarise(number = length(test)) %>% 
  ggplot() + geom_bar(aes(x = vertical_habitat_u, y = number), stat = "identity") +
  theme_bw() + xlab("") + ylab("Number of species") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# level distance habitat
distri_dist <- master_traits %>% 
  mutate(supra_littoral = ifelse(str_detect(distance_coast_habitat, pattern = "supra_littoral")==TRUE,TRUE,FALSE),
         littoral = ifelse(str_detect(distance_coast_habitat, pattern = "littoral")==TRUE,TRUE,FALSE),
         intertidal = ifelse(str_detect(distance_coast_habitat, pattern = "intertidal")==TRUE,TRUE,FALSE),
         sublittoral = ifelse(str_detect(distance_coast_habitat, pattern = "sub_littoral")==TRUE,TRUE,FALSE),
         neritic = ifelse(str_detect(distance_coast_habitat, pattern = "neritic")==TRUE,TRUE,FALSE),
         oceanic = ifelse(str_detect(distance_coast_habitat, pattern = "oceanic")==TRUE,TRUE,FALSE),
         neritic_oceanic = ifelse(str_detect(distance_coast_habitat, pattern = "oceanic")==TRUE & str_detect(distance_coast_habitat, pattern = "neritic"),TRUE,FALSE)) %>% 
  pivot_longer(cols=27:33, names_to = "distance_u", values_to = "test") %>% 
  filter(test==TRUE) %>% 
  group_by(distance_u) %>% 
  summarise(number = length(test)) %>% 
  ggplot() + geom_bar(aes(x = distance_u, y = number), stat = "identity") +
  theme_bw() + xlab("") + ylab("Number of species") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

distri_dist_coast <- master_traits %>% 
  mutate(neritic = ifelse(distance_coast_habitat== "neritic",TRUE,FALSE),
         oceanic = ifelse(distance_coast_habitat=="oceanic",TRUE,FALSE),
         neritic_oceanic = ifelse(distance_coast=="neritic/oceanic",TRUE,FALSE)) %>% 
  pivot_longer(cols=29:31, names_to = "distance_u", values_to = "test") %>% 
  filter(test==TRUE) %>% 
  group_by(distance_u) %>% 
  summarise(number = length(test)) %>% 
  ggplot() + geom_bar(aes(x = distance_u, y = number), stat = "identity") +
  theme_bw() + xlab("") + ylab("Number of species") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# level specific association
distri_asso <- master_traits %>% 
  mutate(seagrasses = ifelse(str_detect(specific_association, pattern = "seagrasses")==TRUE,TRUE,FALSE),
         reefs = ifelse(str_detect(specific_association, pattern = "reefs")==TRUE,TRUE,FALSE),
         reef_exclusive = ifelse(str_detect(specific_association, pattern = "reef_exclusive")==TRUE,TRUE,FALSE),
         lagoons = ifelse(str_detect(specific_association, pattern = "lagoons")==TRUE,TRUE,FALSE),
         estuaries = ifelse(str_detect(specific_association, pattern = "estuaries")==TRUE,TRUE,FALSE),
         mangroves = ifelse(str_detect(specific_association, pattern = "mangroves")==TRUE,TRUE,FALSE),
         wetland = ifelse(str_detect(specific_association, pattern = "wetland")==TRUE,TRUE,FALSE),
         vents = ifelse(str_detect(specific_association, pattern = "vents")==TRUE,TRUE,FALSE),
         deep_corals = ifelse(str_detect(specific_association, pattern = "deep_corals")==TRUE,TRUE,FALSE),
         seamounts = ifelse(str_detect(specific_association, pattern = "seamounts")==TRUE,TRUE,FALSE)
  ) %>% 
  pivot_longer(cols=26:35, names_to = "trait", values_to = "test") %>% 
  filter(test==TRUE) %>% 
  group_by(trait) %>% 
  summarise(number = length(test)) %>% 
  ggplot() + geom_bar(aes(x = trait, y = number), stat = "identity") +
  theme_bw() + xlab("") + ylab("Number of species") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# level mobility
distri_mob <- master_traits %>% 
  mutate(mobile = ifelse(str_detect(mobility, pattern = "mobile")==TRUE,TRUE,FALSE),
         sessile = ifelse(str_detect(mobility, pattern = "sessile")==TRUE,TRUE,FALSE)
  ) %>% 
  pivot_longer(cols=26:27, names_to = "trait", values_to = "test") %>% 
  filter(test==TRUE) %>% 
  group_by(trait) %>% 
  summarise(number = length(test)) %>% 
  ggplot() + geom_bar(aes(x = trait, y = number), stat = "identity") +
  theme_bw() + xlab("") + ylab("Number of species") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



######################
### D. Trait inference
######################

genera <- sort(unique(masterlist$genus))
families <- sort(unique(masterlist$family))
infraorder <- sort(unique(masterlist$infraorder))

# genera traits
genus_traits <- data.frame(genera) %>% 
  mutate(level = "genus",
         depth_range_shallow = NA,
         depth_range_deep = NA,
         vertical_habitat = NA_character_,
         distance_broad = NA_character_,
         distance_coast_habitat = NA_character_,
         specific_association = NA_character_) %>% 
  rename(taxa = genera)
#family traits
family_traits <- data.frame(families) %>% 
  mutate(level = "family",
         depth_range_shallow = NA,
         depth_range_deep = NA,
         vertical_habitat = NA_character_,
         distance_broad = NA_character_,
         distance_coast_habitat = NA_character_,
         specific_association = NA_character_) %>% 
  rename(taxa = families)
#family traits
infraorder_traits <- data.frame(infraorder) %>% 
  mutate(level = "infraorder",
         depth_range_shallow = NA,
         depth_range_deep = NA,
         vertical_habitat = NA_character_,
         distance_broad = NA_character_,
         distance_coast_habitat = NA_character_,
         specific_association = NA_character_) %>% 
  rename(taxa = infraorder)
high_traits <- rbind(genus_traits,family_traits,infraorder_traits)
rm(genus_traits,family_traits,infraorder_traits)

# for loop
for(i in 1:nrow(high_traits)){
  
  print(i)
  
  level <- high_traits$level[i]
  if(level=="genus"){sub_master <- master_traits %>% 
    filter(genus == high_traits$taxa[i])
  }else if(level=="family"){sub_master <- master_traits %>% 
    filter(family == high_traits$taxa[i])
  }else{sub_master <- master_traits %>% 
    filter(infraorder == high_traits$taxa[i])
  }
  
  ### depth range
  if(nrow(sub_master[!is.na(sub_master$depth_range_deep),])>0){
    high_traits$depth_range_deep[i] <- max(sub_master$depth_range_deep, na.rm=TRUE)}
  if(nrow(sub_master[!is.na(sub_master$depth_range_shallow),])>0){
    high_traits$depth_range_shallow[i] <- min(sub_master$depth_range_shallow, na.rm=TRUE)}
  
  #### vertical habitat
  vert_hab <- sub_master %>% 
    select(vertical_habitat_2) %>% 
    filter(!is.na(vertical_habitat_2))
  
  # there is at least a trait
  if(nrow(vert_hab)>0){
    
    # there is one trait winning to infer
    if(length(unique(vert_hab$vertical_habitat_2))==1){
      high_traits$vertical_habitat[i] <- vert_hab$vertical_habitat_2[1]
    }
    # there are several vert traits within the taxa, infer the most abundant
    else{
      vert_hab_prop <- vert_hab %>% 
        group_by(vertical_habitat_2) %>% 
        summarize(freq = sum(length(vertical_habitat_2))) %>% 
        arrange(desc(freq))
      high_traits$vertical_habitat[i] <- vert_hab_prop$vertical_habitat_2[1]
      rm(vert_hab_prop)
    }
  }
  
  ### distance coast trait
  dist_coast <- sub_master %>% 
    select(distance_broad) %>% 
    filter(!is.na(distance_broad))
  
  if(nrow(dist_coast)>0){
    
    # there is one trait winning
    if(length(unique(dist_coast$distance_broad))==1){
      high_traits$distance_broad[i] <- dist_coast$distance_broad[1]
    }
    else{
      dist_coast_prop <- dist_coast %>% 
        group_by(distance_broad) %>% 
        summarize(freq = sum(length(distance_broad))) %>% 
        arrange(desc(freq))
      high_traits$distance_coast[i] <- dist_coast_prop$distance_broad[1]
      rm(dist_coast_prop)
    }
  }
  
  ### specific association
  spec_asso <- sub_master %>% 
    select(specific_association) %>% 
    filter(!is.na(specific_association))
  
  if(nrow(spec_asso)>0){
    
    # there is only one trait winning
    if(length(unique(spec_asso$specific_association))==1){
      high_traits$specific_association[i] <- spec_asso$specific_association[1]
    }
    else{print("Impossible to infer specific habitat")}
  }
  
  rm(sub_master, spec_asso, dist_coast, vert_hab)
}


write.csv(high_traits, file = "outputs_MOL/crabs/traits/high_traits_22JUL2021.csv", 
          row.names = FALSE)


#################################
### E. Summary of inferred traits
#################################

### 1. Exiting traits
# is there a min. depth range?
is_there_min_depth <- high_traits %>% 
  mutate(is_there = ifelse(!is.na(depth_range_shallow),TRUE,FALSE)) %>% 
  group_by(is_there, level) %>% 
  summarize(number = length(is_there)) %>% 
  ggplot(aes(x = is_there, y = number)) + geom_bar(stat = "identity") +
  theme_bw() + ylab("Number of Species") + xlab("Min. depth?") +
  facet_wrap(~ level)

# is there a max. depth range?
is_there_max_depth <- high_traits %>% 
  mutate(is_there = ifelse(!is.na(depth_range_deep),TRUE,FALSE)) %>% 
  group_by(is_there, level) %>% 
  summarize(number = length(is_there)) %>% 
  ggplot(aes(x = is_there, y = number)) + geom_bar(stat = "identity") +
  theme_bw() + ylab("") + xlab("Max. depth?") +
  facet_wrap(~ level)

# is there a vertical habitat?
is_there_vert <- high_traits %>% 
  mutate(is_there = ifelse(!is.na(vertical_habitat),TRUE,FALSE)) %>% 
  group_by(is_there, level) %>% 
  summarize(number = length(is_there)) %>% 
  ggplot(aes(x = is_there, y = number)) + geom_bar(stat = "identity") +
  theme_bw() + ylab("") + xlab("Vertical habitat?") +
  facet_wrap(~ level)

# is there a distance habitat?
is_there_dist <- high_traits %>% 
  mutate(is_there = ifelse(!is.na(distance_coast),TRUE,FALSE)) %>% 
  group_by(is_there, level) %>% 
  summarize(number = length(is_there)) %>% 
  ggplot(aes(x = is_there, y = number)) + geom_bar(stat = "identity") +
  theme_bw() + ylab("Number of Species") + xlab("Distance_coast habitat?") +
  facet_wrap(~ level)


# is there a specific association trait?
is_there_asso <- high_traits %>% 
  mutate(is_there = ifelse(!is.na(specific_association),TRUE,FALSE)) %>% 
  group_by(is_there, level) %>% 
  summarize(number = length(is_there)) %>% 
  ggplot(aes(x = is_there, y = number)) + geom_bar(stat = "identity") +
  theme_bw() + ylab("") + xlab("Specific association?") +
  facet_wrap(~ level)

# summary of existing habitat traits
egg::ggarrange(is_there_min_depth, is_there_max_depth, is_there_vert,
               is_there_dist, is_there_asso,
               labels = c("","","","",""),
               nrow=2)


### 2. Summary of categories
# distribution of min. depth
distri_min_depth <- high_traits %>% 
  filter(!is.na(depth_range_shallow)) %>% 
  ggplot() + geom_histogram(aes(x = depth_range_shallow)) +
  theme_bw() + ylab("Number of species") + xlab("Min. depth") +
  facet_wrap(~ level)


# distribution max. depth
distri_max_depth <- high_traits %>% 
  filter(!is.na(depth_range_deep)) %>% 
  ggplot() + geom_histogram(aes(x = depth_range_deep)) +
  theme_bw() + ylab("") + xlab("Max. depth") +
  facet_wrap(~ level)

egg::ggarrange(distri_min_depth, distri_max_depth, 
               labels=c("",""), nrow=1)

# levels vertical habitat
distri_vert <- high_traits %>% 
  mutate(pelagic = ifelse(str_detect(vertical_habitat, pattern = "pelagic")==TRUE,TRUE,FALSE),
         epipelagic = ifelse(str_detect(vertical_habitat, pattern = "epipelagic")==TRUE,TRUE,FALSE),
         mesopelagic = ifelse(str_detect(vertical_habitat, pattern = "mesopelagic")==TRUE,TRUE,FALSE),
         bathypelagic = ifelse(str_detect(vertical_habitat, pattern = "bathypelagic")==TRUE,TRUE,FALSE),
         abyssopelagic = ifelse(str_detect(vertical_habitat, pattern = "abyssopelagic")==TRUE,TRUE,FALSE),
         hadopelagic = ifelse(str_detect(vertical_habitat, pattern = "hadopelagic")==TRUE,TRUE,FALSE),
         demersal = ifelse(str_detect(vertical_habitat, pattern = "demersal")==TRUE,TRUE,FALSE),
         benthic = ifelse(str_detect(vertical_habitat, pattern = "benthic")==TRUE,TRUE,FALSE),
         benthic_pelagic = ifelse(str_detect(vertical_habitat, pattern = "benthic")==TRUE & str_detect(vertical_habitat, pattern = "pelagic")==TRUE,TRUE,FALSE)
  ) %>% 
  pivot_longer(cols=10:18, names_to = "vertical_habitat_u", values_to = "test") %>% 
  filter(test==TRUE) %>% 
  group_by(vertical_habitat_u, level) %>% 
  summarise(number = length(test)) %>% 
  ggplot() + geom_bar(aes(x = vertical_habitat_u, y = number), stat = "identity") +
  theme_bw() + xlab("") + ylab("Number of species") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(~level)

# level distance habitat
distri_dist <- high_traits %>% 
  mutate(neritic = ifelse(str_detect(distance_broad, pattern = "neritic")==TRUE,TRUE,FALSE),
         oceanic = ifelse(str_detect(distance_broad, pattern = "oceanic")==TRUE,TRUE,FALSE),
         neritic_oceanic = ifelse(str_detect(distance_broad, pattern = "oceanic")==TRUE & str_detect(distance_broad, pattern = "neritic"),TRUE,FALSE)) %>% 
  pivot_longer(cols=10:12, names_to = "distance_u", values_to = "test") %>% 
  filter(test==TRUE) %>% 
  group_by(distance_u, level) %>% 
  summarise(number = length(test)) %>% 
  ggplot() + geom_bar(aes(x = distance_u, y = number), stat = "identity") +
  theme_bw() + xlab("") + ylab("Number of species") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  facet_wrap(~level)

# level specific association
distri_asso <- high_traits %>% 
  mutate(seagrasses = ifelse(str_detect(specific_association, pattern = "seagrasses")==TRUE,TRUE,FALSE),
         reefs = ifelse(str_detect(specific_association, pattern = "reefs")==TRUE,TRUE,FALSE),
         reef_exclusive = ifelse(str_detect(specific_association, pattern = "reef_exclusive")==TRUE,TRUE,FALSE),
         lagoons = ifelse(str_detect(specific_association, pattern = "lagoons")==TRUE,TRUE,FALSE),
         estuaries = ifelse(str_detect(specific_association, pattern = "estuaries")==TRUE,TRUE,FALSE),
         mangroves = ifelse(str_detect(specific_association, pattern = "mangroves")==TRUE,TRUE,FALSE),
         wetland = ifelse(str_detect(specific_association, pattern = "wetland")==TRUE,TRUE,FALSE),
         vents = ifelse(str_detect(specific_association, pattern = "vents")==TRUE,TRUE,FALSE),
         deep_corals = ifelse(str_detect(specific_association, pattern = "deep_corals")==TRUE,TRUE,FALSE),
         seamounts = ifelse(str_detect(specific_association, pattern = "seamounts")==TRUE,TRUE,FALSE)
  ) %>% 
  pivot_longer(cols=10:19, names_to = "trait", values_to = "test") %>% 
  filter(test==TRUE) %>% 
  group_by(trait,level) %>% 
  summarise(number = length(test)) %>% 
  ggplot() + geom_bar(aes(x = trait, y = number), stat = "identity") +
  theme_bw() + xlab("") + ylab("Number of species") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  facet_wrap(~level)




