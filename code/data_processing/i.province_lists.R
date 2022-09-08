################################################################################
#### Code to aggregate species by region
#### Uses homogenized occurrence taxonomy from <6.filter_occ_data.R> or <4.flag_occ_data.R>
#### Coding: Aurore Maureaud, July 2021
################################################################################

rm(list = ls())

# packages
library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
world <- ne_countries(scale = "medium", returnclass = "sf")
mycol <- c("navy", "blue", "cyan", "lightcyan", "yellow", "red", "red4")
library(egg)
library(raster)
sf::sf_use_s2(FALSE)
library(RColorBrewer)

# load occ data for crabs
dat_occ_nf <- get(load("outputs_MOL/crabs/DAT_OCC_CRABS_9JUN2021.RData"))
dat_occ <- get(load("outputs_MOL/crabs/DAT_OCC_CRABS_FILTERED_9JUN2021.RData"))

# load ecoregions
eco <- st_read("~/Yale University/Marine Biogeography/outputs/quality_checks/seafloor_meow_deepsea_24Aug.shp") %>% 
  mutate(prov_n = case_when(ID=="1" ~ "Arctic Basin",
                            ID=="2" ~ "North Atlantic",
                            ID=="3" ~ "Brazil Basin",
                            ID=="4" ~ "Angola, Guinea, Sierra Leone Basins",
                            ID=="5" ~ "Argentine Basin",
                            ID=="6" ~ "Antarctica Basin",
                            ID=="7" ~ "Antarctica West",
                            ID=="8" ~ "Indian",
                            ID=="9" ~ "Chile, Peru, Guatemala Basins",
                            ID=="10" ~ "South Pacific",
                            ID=="11" ~ "Equatorial Pacific",
                            ID=="12" ~ "North Central Pacific",
                            ID=="13" ~ "North Pacific",
                            ID=="14" ~ "West Pacific Basins",
                            ID=="15" ~ "Arctic",
                            ID=="16" ~ "Northern Atlantic Boreal",
                            ID=="17" ~ "Northern Pacific Boreal",
                            ID=="18" ~ "North Atlantic",
                            ID=="19" ~ "Southeast Pacific Ridges",
                            ID=="20" ~ "New Zealand-Kermadec",
                            ID=="21" ~ "Cocos Plate",
                            ID=="22" ~ "Nazca Plate",
                            ID=="23" ~ "Antarctic",
                            ID=="24" ~ "Subantarctic",
                            ID=="25" ~ "Indian",
                            ID=="26" ~ "West Pacific",
                            ID=="27" ~ "South Pacific",
                            ID=="28" ~ "North Pacific"))

meow <- st_read("~/Yale University/Marine Biogeography/regions_base/meows/MEOW_FINAL/meow_ecos/meow_ecos.shp") %>% 
  st_drop_geometry() %>%
  dplyr::select(ECOREGION) %>% 
  pull()
eco$eco_n[1:28] <- "NA"
eco$eco_n[29:260] <- meow

# load eez with countries
eez <- st_read(dsn = "~/Yale University/Marine Biogeography/regions_base/eez/EEZ_land_union_v3_202003/EEZ_land_union_v3_202003",
               layer = "EEZ_Land_v3_202030")


eez2 <- st_read(dsn = "~/Yale University/Marine Biogeography/regions_base/eez/World_EEZ_v11_20191118",
                layer = "eez_v11")


##############################################################################################################
#### 1. GET REGIONS WHERE SPECIES OCCUR
##############################################################################################################
spp <- sort(unique(dat_occ$accepted_name))

# loop by species
for(s in 1:length(spp)){
  
  print(paste(s, spp[s], sep=" "))
  
  dat_occ_spp <- dat_occ %>% 
    filter(accepted_name == spp[s])
  
  # ggplot(world) + geom_sf() +
  #   geom_point(data = dat_occ_spp, aes(x = longitude, y = latitude), shape=3, col="red")
  
  dat_occ_spp <- st_as_sf(dat_occ_spp, coords = c("longitude","latitude"), crs = st_crs(eco))
  
  int_eco <- data.frame(unlist(st_intersects(dat_occ_spp, eco))) %>% 
    rename(eco = `unlist.st_intersects.dat_occ_spp..eco..`) %>% 
    mutate(species = spp[s]) %>% 
    group_by(eco, species) %>% 
    summarize(nb_occ_eco = length(species))
  
  int_eez <- data.frame(unlist(st_intersects(dat_occ_spp, eez))) %>% 
    rename(eez = `unlist.st_intersects.dat_occ_spp..eez..`) %>% 
    mutate(species = spp[s])%>% 
    group_by(eez, species) %>% 
    summarize(nb_occ_eez = length(species))
  
  int_eco_2 <- data.frame(st_intersects(dat_occ_spp, eco, sparse=FALSE))
  dat_occ_spp_eco <- data.frame(cbind(dat_occ_spp, int_eco_2)) %>% 
    pivot_longer(cols=X1:X260, names_to = "eco", values_to = "test") %>% 
    filter(test == TRUE) %>% 
    mutate(eco = str_remove_all(eco, pattern = "X"),
           eco = as.numeric(as.vector(eco)))
  
  int_eez_2 <- data.frame(st_intersects(dat_occ_spp, eez, sparse=FALSE))
  dat_occ_spp_eez <- data.frame(cbind(dat_occ_spp, int_eez_2)) %>% 
    pivot_longer(cols=X1:X260, names_to = "eez", values_to = "test") %>% 
    filter(test == TRUE) %>% 
    mutate(eez = str_remove_all(eez, pattern = "X"),
           eez = as.numeric(as.vector(eez)))
  
  if(s == 1){
    dat_eco <- int_eco
    dat_eez <- int_eez
    dat_occ_reg <- dat_occ_spp
  }else{
    dat_eco <- rbind(dat_eco, int_eco)
    dat_eez <- rbind(dat_eez, int_eez)
    dat_occ_reg <- rbind(dat_occ_reg, dat_occ_spp)
  }
  rm(int_eco, int_eez, int_eez_2, dat_occ_spp, int_eez_2)
}

save.image("outputs_MOL/envt_spp_by_regions.RData")


##############################################################################################################
#### 2. GET REGIONS FROM COUNTRIES
##############################################################################################################

# load country information
cnt_real <- read.csv("outputs_MOL/real/cnt/COUNTRY_BRACHYURANS_1JUN2021.csv")
cnt_wannabe <- read.csv("outputs_MOL/wannabe/cnt/COUNTRY_ANOMURANS_1JUN2021.csv")
cnt <- rbind(cnt_real, cnt_wannabe)

# explore how many regions are associated to a country
# remove countries without EEZ


##############################################################################################################
#### 3. GET REGIONS WHERE SPECIES OCCUR FROM COUNTRY LISTS
##############################################################################################################

# load country information
cnt_real <- read.csv("outputs_MOL/real/cnt/COUNTRY_BRACHYURANS_1JUN2021.csv")
cnt_wannabe <- read.csv("outputs_MOL/wannabe/cnt/COUNTRY_ANOMURANS_1JUN2021.csv")
cnt <- rbind(cnt_real, cnt_wannabe)


##############################################################################################################
#### 4. GET REGIONS WHERE SPECIES OCCUR FROM OCCURRENCES & EXPLORATION
##############################################################################################################
load("outputs_MOL/envt_spp_by_regions.RData")

# examples species randomly selected in the species list
spp_one <- sample(spp, 100, replace=FALSE)

for(s in 1:length(spp_one)){
  
  dat_eco_cs <- dat_eco %>% 
    filter(species == spp_one[s])
  
  dat_eez_cs <- dat_eez %>% 
    filter(species == spp_one[s])
  
  dat_occ_cs <- dat_occ %>% 
    filter(accepted_name == spp_one[s])
  
  dat_cnt_cs <- cnt %>% 
    filter(Species == spp_one[s])
  
  # plot ecoregions and occ pts
  #ggplot(eco[eco$ID %in% dat_eco_cs$eco,]) + geom_sf(fill="red", alpha=0.4, colour="red") +
  #  geom_point(data = dat_occ_cs, aes(x = longitude, y = latitude), shape=3, col = "black") +
  #  theme_bw()
  
  eco_spp <- left_join(eco, dat_eco_cs, by = c("ID" = "eco")) %>% 
    filter(!is.na(nb_occ_eco))
  eez_spp <- eez
  eez_spp$ID <- 1:nrow(eez_spp)
  eez_spp <- left_join(eez_spp, dat_eez_cs, by = c("ID" = "eez")) %>% 
    filter(!is.na(nb_occ_eez))
  
  # plot ecoregions
  plot_eco <- ggplot(world) + geom_sf() + theme_bw() +  
    geom_sf(data = eco_spp, aes(fill = nb_occ_eco), alpha = 0.6) +
    scale_fill_gradient(low = "blue", high = "red", trans = "log") +
    theme(legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)) +
    guides(shape = guide_legend(list(size = 1)),
           color = guide_legend(override.aes = list(size = 1)),
           fill = guide_colorbar(barwidth = 0.75), barheight = 5) +
    ylab("") + xlab("")
  
  # plot occ pts
  plot_occ <- ggplot(world) + geom_sf() + theme_bw() +  
    geom_point(data = dat_occ_cs, aes(x = longitude, y = latitude), shape=3, col = "red", size=2) +
    ylab("") + xlab("")
  
  # plot eez and cnt
  plot_eez <- ggplot(world) + geom_sf() + theme_bw() +  
    geom_sf(data = eez_spp, aes(fill = nb_occ_eez), alpha = 0.6) +
    scale_fill_gradient(low = "blue", high = "red", trans = "log") +
    theme(legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)) +
    guides(shape = guide_legend(list(size = 1)),
           color = guide_legend(override.aes = list(size = 1)),
           fill = guide_colorbar(barwidth = 0.75), barheight = 5) +
    ylab("") + xlab("")
  
  # plot cnt
  plot_cnt <- ggplot(world) + geom_sf() + theme_bw() + 
    geom_sf(data = world[world$iso_n3 %in% dat_cnt_cs$C_Code,], fill = "red", alpha = 0.4) +
    ylab("") + xlab("")
  
  # plot cnt from eez
  # eez_cs <- eez[dat_eez_cs$eez,]
  # ggplot(world) + geom_sf() + theme_bw() + 
  #   geom_sf(data = world[world$iso_a3 %in% eez_cs$ISO_SOV1,], fill = "blue", alpha = 0.4)
  
  # make summary plots
  png(paste("outputs_MOL/explore_regions/",spp_one[s],".png", sep=""),
      width = 20*300, height = 10*300, res=300)
  print(egg::ggarrange(plot_occ, plot_eco, plot_cnt, plot_eez,  
                       nrow = 2, labels = c("Occurrence points", "Seafloor regions", "Country list", "EEZs")))
  dev.off()
  
  rm(plot_cnt, plot_eco, plot_eez, plot_occ, eco_spp, eez_spp, 
     dat_eco_cs, dat_eez_cs, dat_cnt_cs, dat_occ_cs)
  
}


### All species
dat_eco_s <- dat_eco %>% 
  group_by(eco) %>% 
  summarize(nb_spp = length(species))
eco_s <- left_join(eco, dat_eco_s, by=c("ID" = "eco"))
mycol <- c("navy", "blue", "cyan", "lightcyan", "yellow", "red", "red4")

ggplot(world) + geom_sf() + theme_bw() +
  geom_sf(data = eco_s[!is.na(eco_s$nb_spp),], aes(fill = nb_spp), colour=NA) +
  scale_fill_gradientn(colours = mycol)

dat_eez_s <- dat_eez %>% 
  group_by(eez) %>% 
  summarize(nb_spp = length(species))

eez$ID <- 1:nrow(eez)
eez_s <- left_join(eez, dat_eez_s, by=c("ID" = "eez"))
mycol <- c("navy", "blue", "cyan", "lightcyan", "yellow", "red", "red4")

ggplot(world) + geom_sf() + theme_bw() +
  geom_sf(data = eez_s[!is.na(eez_s$nb_spp),], aes(fill = nb_spp), colour=NA) +
  scale_fill_gradientn(colours = mycol)

ggplot(world) + geom_sf() + theme_bw() +
  geom_point(data = dat_occ, aes(x = longitude, y = latitude), colour="black", size=0.5)

eez_s_cnt <- eez_s %>% 
  st_drop_geometry() %>% 
  select(ISO_TER1, nb_spp)
eez_s_cnt <- left_join(world, eez_s_cnt, by = c("iso_a3" = "ISO_TER1")) %>% 
  group_by(iso_a3) %>% 
  filter(!is.na(nb_spp))

ggplot(world) + geom_sf() + theme_bw() +
  geom_sf(data = eez_s_cnt, aes(fill = nb_spp)) +
  scale_fill_gradientn(colours = mycol, na.value = "grey80")

dat_cnt <- cnt %>% 
  group_by(C_Code) %>% 
  summarize(nb_spp = length(Species))
cnt_s <- left_join(world, dat_cnt, by=c("iso_n3" = "C_Code"))

ggplot(world) + geom_sf() + theme_bw() +
  geom_sf(data = cnt_s, aes(fill = nb_spp)) +
  scale_fill_gradientn(colours = mycol, na.value = "grey80")

# relationship between occ.  country data and occ country data
xx_1 <- cnt_s %>% select(iso_a3, nb_spp) %>% rename(country = nb_spp) %>% 
  st_drop_geometry()
xx_2 <- eez_s_cnt %>% select(iso_a3, nb_spp) %>% rename(occ = nb_spp) %>% 
  st_drop_geometry()
merge_cnt <- full_join(xx_1, xx_2, by = "iso_a3") %>% 
  mutate(country = ifelse(is.na(country), 0, country),
         occ = ifelse(is.na(occ), 0, occ))

ggplot(merge_cnt) + geom_point(aes(x = occ, y = country), shape=3) + theme_bw() +
  xlab("SR from Occurrences") + ylab("SR from Country lists") +
  geom_abline(slope = 1, col="red", lwd=0.5)

ggplot(merge_cnt) + geom_point(aes(x = occ, y = country), shape=3) + theme_bw() +
  xlab("SR from Occurrences") + ylab("SR from Country lists") +
  geom_abline(slope = 1, col="red", lwd=0.5) + ylim(0,100) + xlim(0,100)


### Brachyurans
master <- read.csv("outputs_MOL/real/taxo/BRACHYURANS_MASTER_GBIF_15APR2021.csv") %>% 
  mutate(canonical = gsub('"',"",canonical)) # 11,464 names
master_real <- master %>% filter(accid==0)

dat_eco_s <- dat_eco %>% 
  filter(species %in% master_real$canonical) %>% 
  group_by(eco) %>% 
  summarize(nb_spp = length(species))
eco_s <- left_join(eco, dat_eco_s, by=c("ID" = "eco"))
mycol <- c("navy", "blue", "cyan", "lightcyan", "yellow", "red", "red4")

ggplot(world) + geom_sf() + theme_bw() +
  geom_sf(data = eco_s[!is.na(eco_s$nb_spp),], aes(fill = nb_spp), colour=NA) +
  scale_fill_gradientn(colours = mycol)

dat_eez_s <- dat_eez %>% 
  filter(species %in% master_real$canonical) %>% 
  group_by(eez) %>% 
  summarize(nb_spp = length(species))

eez$ID <- 1:nrow(eez)
eez_s <- left_join(eez, dat_eez_s, by=c("ID" = "eez"))
mycol <- c("navy", "blue", "cyan", "lightcyan", "yellow", "red", "red4")

ggplot(world) + geom_sf() + theme_bw() +
  geom_sf(data = eez_s[!is.na(eez_s$nb_spp),], aes(fill = nb_spp), colour=NA) +
  scale_fill_gradientn(colours = mycol)

ggplot(world) + geom_sf() + theme_bw() +
  geom_point(data = dat_occ[dat_occ$infraorder=="Brachyura",], aes(x = longitude, y = latitude), colour="black", size=0.5)



### Anomurans
master <- read.csv("outputs_MOL/wannabe/taxo/ANOMURANS_MASTER_GBIF_15APR2021.csv") %>% 
  mutate(canonical = gsub('"',"",canonical)) # 11,464 names
master_wannabe <- master %>% filter(accid==0)

dat_eco_s <- dat_eco %>% 
  filter(species %in% master_wannabe$canonical) %>% 
  group_by(eco) %>% 
  summarize(nb_spp = length(species))
eco_s <- left_join(eco, dat_eco_s, by=c("ID" = "eco"))
mycol <- c("navy", "blue", "cyan", "lightcyan", "yellow", "red", "red4")

ggplot(world) + geom_sf() + theme_bw() +
  geom_sf(data = eco_s[!is.na(eco_s$nb_spp),], aes(fill = nb_spp), colour=NA) +
  scale_fill_gradientn(colours = mycol)

dat_eez_s <- dat_eez %>% 
  filter(species %in% master_wannabe$canonical) %>% 
  group_by(eez) %>% 
  summarize(nb_spp = length(species))

eez_s <- left_join(eez, dat_eez_s, by=c("ID" = "eez"))
mycol <- c("navy", "blue", "cyan", "lightcyan", "yellow", "red", "red4")

ggplot(world) + geom_sf() + theme_bw() +
  geom_sf(data = eez_s[!is.na(eez_s$nb_spp),], aes(fill = nb_spp), colour=NA) +
  scale_fill_gradientn(colours = mycol)

ggplot(world) + geom_sf() + theme_bw() +
  geom_point(data = dat_occ[dat_occ$infraorder=="Anomura",], aes(x = longitude, y = latitude), colour="black", size=0.5)


##############################################################################################################
#### 5. GET UNCERTAINTY AROUND DEEP SEA REGIONS
##############################################################################################################

# load master traits
master_traits <- read.csv(file = "outputs_MOL/crabs/traits/master_traits_22JUL2021.csv")
high_traits <- read.csv(file = "outputs_MOL/crabs/traits/high_traits_22JUL2021.csv")

# load filtered occ data
dat_occ <- get(load("outputs_MOL/crabs/DAT_OCC_CRABS_FILTERED_9JUN2021.RData"))
spp <- sort(unique(dat_occ$accepted_name))

# load depth shapefiles
depth_n0_s90_w180_e90 <- raster("~/Yale University/Marine Biogeography/regions_base/depth/gebco_2020_ascii/gebco_2020_n0.0_s-90.0_w-180.0_e-90.0.asc")
depth_n0_s90_w180_e90 <- aggregate(depth_n0_s90_w180_e90, fact = 4, fun = min)
depth_n0_s90_w90_e0 <- raster("~/Yale University/Marine Biogeography/regions_base/depth/gebco_2020_ascii/gebco_2020_n0.0_s-90.0_w-90.0_e0.0.asc")
depth_n0_s90_w90_e0 <- aggregate(depth_n0_s90_w90_e0, fact = 4, fun = min)
depth_n0_s90_w0_e90 <- raster("~/Yale University/Marine Biogeography/regions_base/depth/gebco_2020_ascii/gebco_2020_n0.0_s-90.0_w0.0_e90.0.asc")
depth_n0_s90_w0_e90 <- aggregate(depth_n0_s90_w0_e90, fact = 4, fun = min)
depth_n0_s90_w90_e180 <- raster("~/Yale University/Marine Biogeography/regions_base/depth/gebco_2020_ascii/gebco_2020_n0.0_s-90.0_w90.0_e180.0.asc")
depth_n0_s90_w90_e180 <- aggregate(depth_n0_s90_w90_e180, fact = 4, fun = min)
depth_n90_s0_w180_e90 <- raster("~/Yale University/Marine Biogeography/regions_base/depth/gebco_2020_ascii/gebco_2020_n90.0_s0.0_w-180.0_e-90.0.asc")
depth_n90_s0_w180_e90 <- aggregate(depth_n90_s0_w180_e90, fact = 4, fun = min)
depth_n90_s0_w90_e0 <- raster("~/Yale University/Marine Biogeography/regions_base/depth/gebco_2020_ascii/gebco_2020_n90.0_s0.0_w-90.0_e0.0.asc")
depth_n90_s0_w90_e0 <- aggregate(depth_n90_s0_w90_e0, fact = 4, fun = min)
depth_n90_s0_w0_e90 <- raster("~/Yale University/Marine Biogeography/regions_base/depth/gebco_2020_ascii/gebco_2020_n90.0_s0.0_w0.0_e90.0.asc")
depth_n90_s0_w0_e90 <- aggregate(depth_n90_s0_w0_e90, fact = 4, fun = min)
depth_n90_s0_w90_e180 <- raster("~/Yale University/Marine Biogeography/regions_base/depth/gebco_2020_ascii/gebco_2020_n90.0_s0.0_w90.0_e180.0.asc")
depth_n90_s0_w90_e180 <- aggregate(depth_n90_s0_w90_e180, fact = 4, fun = min)

depth_raster <- raster::merge(depth_n0_s90_w180_e90, depth_n0_s90_w90_e0, 
                              depth_n0_s90_w0_e90, depth_n0_s90_w90_e180,
                              depth_n90_s0_w180_e90, depth_n90_s0_w90_e0,
                              depth_n90_s0_w0_e90, depth_n90_s0_w90_e180)
rm(depth_n0_s90_w180_e90, depth_n0_s90_w90_e0, depth_n0_s90_w0_e90,
   depth_n0_s90_w90_e180, depth_n90_s0_w90_e0, depth_n90_s0_w0_e90,
   depth_n90_s0_w90_e180, depth_n90_s0_w180_e90)

# check regions selected per species
# there is a problem with species s=4502
for(s in 1:length(spp)){
  
  print(s)
  
  j_spp <- which(master_traits$canonical == spp[s])
  j_gen <- which(high_traits$taxa == master_traits$genus[j_spp])
  j_fam <- which(high_traits$taxa == master_traits$family[j_spp])
  
  spp_traits <- master_traits[j_spp,]
  gen_traits <- high_traits[j_gen,]
  fam_traits <- high_traits[j_fam,]
  
  dat_occ_spp <- dat_occ %>% filter(accepted_name == spp[s]) %>% 
    mutate(check_depth = NA_character_,
           check_dist = NA_character_)
  
  ### CHECK DEPTH RANGE
  # get depth rasters required
  dat_occ_spp_sf <- st_as_sf(dat_occ_spp, coords = c("longitude", "latitude"))
  dat_occ_spp_sp <- as_Spatial(dat_occ_spp_sf)
  gebco <- raster::extract(depth_raster, dat_occ_spp_sp)
  dat_occ_spp <- cbind(dat_occ_spp,gebco)
  
  # add gebco minimum depth in the point file
  # if depth range is known at the species level
  if(!spp_traits$vertical_habitat_2 %in% c("pelagic","benthopelagic")){
    if(!is.na(spp_traits$depth_range_deep[1])){
      dat_occ_spp <- dat_occ_spp %>% 
        mutate(check_depth = ifelse(gebco<(-spp_traits$depth_range_deep[1]-0.15*spp_traits$depth_range_deep[1]),
                                    "depth_too_high",NA_character_),
               check_depth = ifelse(gebco>0, "min_depth>0",check_depth))
      
    } else if (is.na(spp_traits$depth_range_deep[1]) & !is.na(gen_traits$depth_range_deep[1])){
      dat_occ_spp <- dat_occ_spp%>% 
        mutate(check_depth = ifelse(gebco<(-gen_traits$depth_range_deep[1]-0.15*gen_traits$depth_range_deep[1]),
                                    "depth_too_high_gen",NA_character_),
               check_depth = ifelse(gebco>0, "min_depth>0",check_depth))
      
    } else if (is.na(gen_traits$depth_range_deep[1]) & !is.na(fam_traits$depth_range_deep[1])) {
      dat_occ_spp <- dat_occ_spp %>% 
        mutate(check_depth = ifelse(gebco<(-fam_traits$depth_range_deep[1]-0.15*fam_traits$depth_range_deep[1]),
                                    "depth_too_high_fam",NA_character_),
               check_depth = ifelse(gebco>0, "min_depth>0",check_depth))
      
    } else if (is.na(fam_traits$depth_range_deep[1])) {
      dat_occ_spp <- dat_occ_spp %>% 
        mutate(check_depth = "no_expert_depth")
    }
  }
  
  ### CHECK COASTAL DISTANCE
  if(!spp_traits$distance_broad %in% c("oceanic","neritic/oceanic")){
    if(!is.na(spp_traits$distance_broad[1]) & spp_traits$distance_broad[1]=="neritic"){
      dat_occ_spp <- dat_occ_spp %>% 
        mutate(check_dist = ifelse(gebco<(-200),"depth>200",NA_character_),
               check_dist = ifelse(gebco<(-800),"depth>800",NA_character_))
    } else if (!is.na(gen_traits$distance_broad[1]) & gen_traits$distance_broad[1]=="neritic"){
      dat_occ_spp <- dat_occ_spp %>% 
        mutate(check_dist = ifelse(gebco<(-200),"depth>200_gen",NA_character_),
               check_dist = ifelse(gebco<(-800),"depth>800_gen",NA_character_))
    } else if (!is.na(fam_traits$distance_broad[1]) & fam_traits$distance_broad[1]=="neritic"){
      dat_occ_spp <- dat_occ_spp %>% 
        mutate(check_dist = ifelse(gebco<(-200),"depth>200_fam",NA_character_),
               check_dist = ifelse(gebco<(-800),"depth>800_fam",NA_character_))
    } else if (is.na(fam_traits$distance_broad[1])){
      dat_occ_spp <- dat_occ_spp %>% 
        mutate(check_dist = "no_expert_dist")
    }
  }
  
  if(s==1){dat_occ_hab <- dat_occ_spp
  } else {dat_occ_hab <- rbind(dat_occ_hab, dat_occ_spp)}
  
  rm(dat_occ_spp, dat_occ_spp_sf, dat_occ_spp_sp, gebco, j_spp, j_gen, j_fam,
     spp_traits, gen_traits, fam_traits)
} 

save(dat_occ_hab, file = "outputs_MOL/crabs/DAT_OCC_CRABS_FLAG_EXPRT_26JUL2021.RData")



##############################################################################################################
#### 6. GET REGIONS WHERE SPECIES OCCUR with expert flags
##############################################################################################################

# loop by species
for(s in 1:length(spp)){
  
  print(paste(s, spp[s], sep=" "))
  
  dat_occ_spp <- dat_occ %>% 
    filter(accepted_name == spp[s])
  
  # ggplot(world) + geom_sf() +
  #   geom_point(data = dat_occ_spp, aes(x = longitude, y = latitude), shape=3, col="red")
  
  dat_occ_spp <- st_as_sf(dat_occ_spp, coords = c("longitude","latitude"), crs = st_crs(eco))
  
  int_eco <- data.frame(unlist(st_intersects(dat_occ_spp, eco))) %>% 
    rename(eco = `unlist.st_intersects.dat_occ_spp..eco..`) %>% 
    mutate(species = spp[s]) %>% 
    group_by(eco, species) %>% 
    summarize(nb_occ_eco = length(species))
  
  int_eez <- data.frame(unlist(st_intersects(dat_occ_spp, eez))) %>% 
    rename(eez = `unlist.st_intersects.dat_occ_spp..eez..`) %>% 
    mutate(species = spp[s])%>% 
    group_by(eez, species) %>% 
    summarize(nb_occ_eez = length(species))
  
  int_eco_2 <- data.frame(st_intersects(dat_occ_spp, eco, sparse=FALSE))
  dat_occ_spp_eco <- data.frame(cbind(dat_occ_spp, int_eco_2)) %>% 
    pivot_longer(cols=X1:X260, names_to = "eco", values_to = "test") %>% 
    filter(test == TRUE) %>% 
    mutate(eco = str_remove_all(eco, pattern = "X"),
           eco = as.numeric(as.vector(eco)))
  
  int_eez_2 <- data.frame(st_intersects(dat_occ_spp, eez, sparse=FALSE))
  dat_occ_spp_eez <- data.frame(cbind(dat_occ_spp, int_eez_2)) %>% 
    pivot_longer(cols=X1:X260, names_to = "eez", values_to = "test") %>% 
    filter(test == TRUE) %>% 
    mutate(eez = str_remove_all(eez, pattern = "X"),
           eez = as.numeric(as.vector(eez)))
  
  if(s == 1){
    dat_eco <- int_eco
    dat_eez <- int_eez
    dat_occ_reg <- dat_occ_spp_eco
  }else{
    dat_eco <- rbind(dat_eco, int_eco)
    dat_eez <- rbind(dat_eez, int_eez)
    dat_occ_reg <- rbind(dat_occ_reg, dat_occ_spp_eco)
  }
  rm(int_eco, int_eez, int_eez_2, dat_occ_spp, int_eez_2)
}

save(dat_eco, file = "outputs_MOL/crabs/regions/DAT_ECO_CRABS_26JUL2021.RData")
save(dat_eez, file = "outputs_MOL/crabs/regions/DAT_EEZ_CRABS_26JUL2021.RData")
save(dat_occ_reg, file = "outputs_MOL/crabs/regions/DAT_OCC_ECO_CRABS_26JUL2021.RData")


# integrate flags and occ data
load("outputs_MOL/crabs/DAT_OCC_CRABS_FLAG_EXPRT_26JUL2021.RData")
load("outputs_MOL/crabs/regions/DAT_OCC_ECO_CRABS_26JUL2021.RData")
head(dat_occ_hab)

dat_occ_reg_2 <- left_join(dat_occ_reg, dat_occ_hab, 
                           by = c("year","month","day",
                                  "coordinate_error","depth","depth_error","institution_code",
                                  "collection_code","catalog_number","dataset_name","dataset_id",
                                  "country_code","accepted_name","aphia_id","class","order",
                                  "infraorder","family","genus","subgenus","species","subspecies",
                                  "taxonomic_status","taxon_rank","type","authorship","source",
                                  "id","flag_year","flag_month","flag_day","flag_coordinate",
                                  "flag_outlier","flag_coord_uncertainty","flag_depth",
                                  "flag_realm","occ_id"))


# data_with_flags
dat_occ_reg_filt <- dat_occ_reg_2 %>% 
  filter(!check_depth %in% c("depth_too_high","min_depth>0","depth_too_high_fam",
                             "depth_too_high_gen"),
         !check_dist %in% c("depth>800","depth>800_gen","depth>800_fam")) %>% 
  group_by(eco) %>% 
  summarize(nb_spp_f = length(unique(accepted_name)))

eco_filt <- left_join(eco, dat_occ_reg_filt, by=c("ID"="eco"))

mycol <- brewer.pal(9, name = "YlOrBr")
ggplot() + geom_sf(data = eco_filt[!is.na(eco_filt$nb_spp_f),], aes(fill = nb_spp_f), colour=NA) +
  scale_fill_gradientn(colours = mycol) + theme_minimal() +
  geom_sf(data = world, color=NA) +
  coord_sf(crs = '+proj=moll') +
  theme(legend.position = "bottom", legend.title = element_blank())

dat_occ_reg_non_filt <- dat_occ_reg_2 %>% 
  group_by(eco) %>% 
  summarize(nb_spp = length(unique(accepted_name)))

eco_non_filt <- left_join(eco, dat_occ_reg_non_filt, by=("ID" = "eco"))

compare_eco_sr <- full_join(dat_occ_reg_filt, dat_occ_reg_non_filt, by = "eco") %>% 
  mutate(nb_spp = ifelse(is.na(nb_spp), 0, nb_spp),
         nb_spp_f = ifelse(is.na(nb_spp_f), 0, nb_spp_f))

ggplot(compare_eco_sr, aes(x = nb_spp, y = nb_spp_f)) + geom_point() + theme_bw() +
  xlab("Number of species before filtering") + ylab("Number of species after filtering") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1.5)

# check number of occurrences in the deep sea compared to coastal areas
dat_occ_reg_filt <- dat_occ_reg_2 %>% 
  filter(!check_depth %in% c("depth_too_high","min_depth>0","depth_too_high_fam",
                             "depth_too_high_gen"),
         !check_dist %in% c("depth>800","depth>800_gen","depth>800_fam")) %>% 
  group_by(eco) %>% 
  summarize(nb_occ_f = length(occ_id))

eco_filt <- left_join(eco, dat_occ_reg_filt, by=c("ID"="eco"))

ggplot(world) + geom_sf() + theme_bw() +
  geom_sf(data = eco_filt[!is.na(eco_filt$nb_occ_f),], aes(fill = nb_occ_f), colour=NA) +
  scale_fill_gradientn(colours = mycol, trans = "log10")

