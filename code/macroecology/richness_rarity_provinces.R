################################################################################
#### Code to calculate richness and rarity of species
#### Uses homogenized occurrence taxonomy from <6.filter_occ_data.R> or <4.flag_occ_data.R>
#### Coding: Aurore Maureaud, October 2021
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
#### LOAD FILES
##############################################################################################################

# load occ data for crabs
dat_occ_nf <- get(load("outputs_MOL/crabs/DAT_OCC_CRABS_9JUN2021.RData"))
dat_occ <- get(load("outputs_MOL/crabs/DAT_OCC_CRABS_FILTERED_9JUN2021.RData"))

# load ecoregions
# eco <- st_read("~/Yale University/Marine Biogeography/outputs/quality_checks/seafloor_meow_deepsea_24Aug.shp") %>% 
#   mutate(prov_n = case_when(ID=="1" ~ "Arctic Basin",
#                             ID=="2" ~ "North Atlantic",
#                             ID=="3" ~ "Brazil Basin",
#                             ID=="4" ~ "Angola, Guinea, Sierra Leone Basins",
#                             ID=="5" ~ "Argentine Basin",
#                             ID=="6" ~ "Antarctica Basin",
#                             ID=="7" ~ "Antarctica West",
#                             ID=="8" ~ "Indian",
#                             ID=="9" ~ "Chile, Peru, Guatemala Basins",
#                             ID=="10" ~ "South Pacific",
#                             ID=="11" ~ "Equatorial Pacific",
#                             ID=="12" ~ "North Central Pacific",
#                             ID=="13" ~ "North Pacific",
#                             ID=="14" ~ "West Pacific Basins",
#                             ID=="15" ~ "Arctic",
#                             ID=="16" ~ "Northern Atlantic Boreal",
#                             ID=="17" ~ "Northern Pacific Boreal",
#                             ID=="18" ~ "North Atlantic",
#                             ID=="19" ~ "Southeast Pacific Ridges",
#                             ID=="20" ~ "New Zealand-Kermadec",
#                             ID=="21" ~ "Cocos Plate",
#                             ID=="22" ~ "Nazca Plate",
#                             ID=="23" ~ "Antarctic",
#                             ID=="24" ~ "Subantarctic",
#                             ID=="25" ~ "Indian",
#                             ID=="26" ~ "West Pacific",
#                             ID=="27" ~ "South Pacific",
#                             ID=="28" ~ "North Pacific"))
# 
# meow <- st_read("~/Yale University/Marine Biogeography/regions_base/meows/MEOW_FINAL/meow_ecos/meow_ecos.shp") %>% 
#   st_drop_geometry() %>%
#   dplyr::select(ECOREGION) %>% 
#   pull()
# eco$eco_n[1:28] <- "NA"
# eco$eco_n[29:260] <- meow

# eco <- eco %>% 
#   mutate(area = drop_units(st_area(geometry))/1e6)

eco <- st_read("~/Yale University/Marine Biogeography/outputs/remove_hadal/seafloor_meow_deepsea_w_hadal_correctedgeom_10202021.shp")
eco <- eco %>% arrange(ID)

# include areas of biogeographic units - for now including coastlines of EEZs...
int_griffy <- st_read("~/Yale University/Marine Biogeography/outputs/shapefile_ints/seafloor_meow_deepsea_EEZ_ABJ_10202021.shp")
biog_areas <- int_griffy %>% 
  mutate(area_biog = drop_units(st_area(geometry))/1e6) %>% 
  dplyr::select(ID, area_biog) %>% st_drop_geometry() %>%
  group_by(ID) %>% 
  summarize(area_biog = sum(area_biog))

eco <- left_join(eco, biog_areas, by = "ID")


##############################################################################################################
#### FILTER & GET BIODIVERSITY METRICS FROM OCC DATA
##############################################################################################################

# integrate flags and occ data
load("outputs_MOL/crabs/DAT_OCC_CRABS_FLAG_EXPRT_26JUL2021.RData")
load("outputs_MOL/crabs/regions/DAT_OCC_ECO_CRABS_26JUL2021.RData")
head(dat_occ_hab)

# re-work intersection with ecoregions
dat_occ_spp <- st_as_sf(dat_occ_hab, coords = c("longitude","latitude"), crs = st_crs(eco))
int_eco <- st_intersects(dat_occ_spp, eco, sparse=FALSE)

int_eco_vec <- c()
for(i in 1:nrow(int_eco)){
  x <- which(int_eco[i,]==TRUE)
  int_eco_vec <- c(int_eco_vec, x[1])
  if(i %in% c(1000,50000,100000,200000,300000,400000,500000,600000,700000)){
    print(i)
  }
}
int_eco_vec <- data.frame(int_eco_vec)
dat_occ_reg <- data.frame(cbind(dat_occ_spp, int_eco_vec))

dat_occ_reg_2 <- left_join(dat_occ_reg, dat_occ_hab, 
                           by = c("year","month","day",
                                  "coordinate_error","depth","depth_error","institution_code",
                                  "collection_code","catalog_number","dataset_name","dataset_id",
                                  "country_code","accepted_name","aphia_id","class","order",
                                  "infraorder","family","genus","subgenus","species","subspecies",
                                  "taxonomic_status","taxon_rank","type","authorship","source",
                                  "id","flag_year","flag_month","flag_day","flag_coordinate",
                                  "flag_outlier","flag_coord_uncertainty","flag_depth",
                                  "flag_realm","occ_id", "check_depth", "check_dist")) %>% 
  filter(!check_depth %in% c("depth_too_high","min_depth>0","depth_too_high_fam","depth_too_high_gen"),
         !check_dist %in% c("depth>800","depth>800_gen","depth>800_fam"))
dat_occ_reg_2$geometry <- NULL

# total range area per species
range_areas_spp <- left_join(dat_occ_reg_2, biog_areas, by = c("int_eco_vec" = "ID")) %>% 
  rename(eco = int_eco_vec) %>% 
  dplyr::select(accepted_name, eco, area_biog) %>% 
  distinct() %>% 
  group_by(accepted_name) %>% 
  summarize(area = sum(area_biog))

eco_spp <- left_join(dat_occ_reg_2, biog_areas, by = c("int_eco_vec" = "ID")) %>% 
  rename(eco = int_eco_vec) %>%
  dplyr::select(accepted_name, eco, area_biog) %>% 
  distinct()

# calculate rarity
biodiversity <- left_join(range_areas_spp, eco_spp, by = "accepted_name") %>% 
  rename(ID = eco) %>%
  mutate(ratio_eco_range = area_biog/area) %>% 
  group_by(ID, area_biog) %>% 
  summarize(total_rarity = sum(ratio_eco_range),
            richness = length(unique(accepted_name)),
            avg_rarity = sum(ratio_eco_range)/richness)

# map biodiversity outcomes
mycol <- brewer.pal(9, name = "Oranges")
plot_biodiversity <- left_join(eco, biodiversity, by = "ID")
ggplot() + geom_sf(data = plot_biodiversity, aes(fill = richness), colour=NA) +
  scale_fill_gradientn(colours = mycol, na.value = "white") + theme_minimal() +
  geom_sf(data = world, color=NA) +
  coord_sf(crs = '+proj=moll') +
  theme(legend.position = "bottom", legend.title = element_blank())

ggplot() + geom_sf(data = plot_biodiversity, aes(fill = total_rarity), colour=NA) +
  scale_fill_gradientn(colours = mycol, na.value = "white") + theme_minimal() +
  geom_sf(data = world, color=NA) +
  coord_sf(crs = '+proj=moll') +
  theme(legend.position = "bottom", legend.title = element_blank())

ggplot() + geom_sf(data = plot_biodiversity, aes(fill = avg_rarity), colour=NA) +
  scale_fill_gradientn(colours = mycol, na.value = "white") + theme_minimal() +
  geom_sf(data = world, color=NA) +
  coord_sf(crs = '+proj=moll') +
  theme(legend.position = "bottom", legend.title = element_blank())


png(filename = "outputs_biodiversity/figures_half_earth_day/richness_crabs_occ_fixh.png",
    width = 16*200, height = 10*200, res = 200)
print(ggplot() + geom_sf(data = plot_biodiversity, aes(fill = richness), colour=NA) +
        scale_fill_gradientn(colours = mycol, na.value = "white") + theme_minimal() +
        geom_sf(data = world, color=NA) +
        coord_sf(crs = '+proj=moll') +
        theme(legend.position = "bottom", legend.title = element_blank()))
dev.off()

png(filename = "outputs_biodiversity/figures_half_earth_day/tot_rarity_crabs_occ_fixh.png",
    width = 16*200, height = 10*200, res = 200)
print(ggplot() + geom_sf(data = plot_biodiversity, aes(fill = total_rarity), colour=NA) +
        scale_fill_gradientn(colours = mycol, na.value = "white") + theme_minimal() +
        geom_sf(data = world, color=NA) +
        coord_sf(crs = '+proj=moll') +
        theme(legend.position = "bottom", legend.title = element_blank()))
dev.off()

png(filename = "outputs_biodiversity/figures_half_earth_day/avg_rarity_crabs_occ_fixh.png",
    width = 16*200, height = 10*200, res = 200)
print(ggplot() + geom_sf(data = plot_biodiversity, aes(fill = avg_rarity), colour=NA) +
        scale_fill_gradientn(colours = mycol, na.value = "white") + theme_minimal() +
        geom_sf(data = world, color=NA) +
        coord_sf(crs = '+proj=moll') +
        theme(legend.position = "bottom", legend.title = element_blank()))
dev.off()

png(filename = "outputs_biodiversity/figures_half_earth_day/rel_richness_tot_area.png",
    width = 16*200, height = 10*200, res = 200)
ggplot(biodiversity) + geom_point(aes(x = richness, y = total_rarity, color = area_biog), size = 3) +
  theme_bw() +
  scale_color_gradientn(colours = mycol, na.value = "grey", trans = "log10",name = "Area (log10)") +
  xlab("Richness") + ylab("Total rarity") +
  theme(text = element_text(size = 25))
dev.off()

png(filename = "outputs_biodiversity/figures_half_earth_day/rel_richness_area.png",
    width = 16*200, height = 10*200, res = 200)
ggplot(biodiversity) + geom_point(aes(y = richness, x = area_biog), size = 3) +
  theme_bw() + scale_y_log10() + scale_x_log10() +
  xlab("Area spatial unit") + ylab("Richness") +
  theme(text = element_text(size = 25))
dev.off()

png(filename = "outputs_biodiversity/figures_half_earth_day/rel_total_rarity_area.png",
    width = 16*200, height = 10*200, res = 200)
ggplot(biodiversity) + geom_point(aes(y = total_rarity, x = area_biog), size = 3) +
  theme_bw() + scale_y_log10() + scale_x_log10() +
  xlab("Area spatial unit") + ylab("Total rarity") +
  theme(text = element_text(size = 25))
dev.off()

png(filename = "outputs_biodiversity/figures_half_earth_day/rel_richness_tot_rarity.png",
    width = 16*200, height = 10*200, res = 200)
print(ggplot(biodiversity) + geom_point(aes(x = richness, y = total_rarity), size = 3) +
        theme_bw() + xlab("Richness") + ylab("Total rarity") +
        theme(text = element_text(size = 25)))
dev.off()

png(filename = "outputs_biodiversity/figures_half_earth_day/rel_richness_avg_rarity.png",
    width = 16*200, height = 10*200, res = 200)
ggplot(biodiversity) + geom_point(aes(x = richness, y = avg_rarity), size = 3) +
  theme_bw() + 
  xlab("Richness") + ylab("Average Rarity") +
  theme(text = element_text(size = 25))
dev.off()


##############################################################################################################
#### NUMBER OF SPECIES AND OCC PER GRID CELL
##############################################################################################################

# load MOL grid - 720*228
grid_720 <- st_read(dsn = "E:/Yale data/Grids/Grids v2/720x228_grid_v2/720x228global_v2_20200527", layer = "720x228global_20200527")

dat_occ_reg_2 <- dat_occ_reg_2 %>% filter(!is.na(latitude))
sppdat_sf <- st_as_sf(dat_occ_reg_2, coords = c("longitude", "latitude"), crs = 4326)
sppdat_sf <- st_transform(sppdat_sf, crs = st_crs(grid_720))
sppdat_sf_720 <- st_join(sppdat_sf, grid_720, left=TRUE, largest=FALSE)
sppdat_sf_720 <- sppdat_sf_720 %>% 
  st_drop_geometry() %>% 
  group_by(ID_720) %>% 
  summarise(nbr_occ = length(id), nbr_spp = length(unique(accepted_name)))

grid_720_crabs <- left_join(grid_720, sppdat_sf_720, by = "ID_720") %>% 
  filter(!is.na(nbr_spp))

mycol <- brewer.pal(9, name = "Oranges")
png(filename = "outputs_biodiversity/figures_half_earth_day/crab_richness_grid_720_occ.png",
    width = 16*200, height = 10*200, res = 200)
print(ggplot() + geom_sf(data = grid_720_crabs, aes(fill = nbr_spp), colour=NA) +
        scale_fill_gradientn(colours = mycol, na.value = "white", trans = "log10") + theme_minimal() +
        geom_sf(data = world, color=NA) +
        coord_sf(crs = '+proj=moll') +
        theme(legend.position = "bottom", legend.title = element_blank()))
dev.off()

png(filename = "outputs_biodiversity/figures_half_earth_day/crab_occurrences_grid_720_occ.png",
    width = 16*200, height = 10*200, res = 200)
print(ggplot() + geom_sf(data = grid_720_crabs, aes(fill = nbr_occ), colour=NA) +
        scale_fill_gradientn(colours = mycol, na.value = "white", trans = "log10") + theme_minimal() +
        geom_sf(data = world, color=NA) +
        coord_sf(crs = '+proj=moll') +
        theme(legend.position = "bottom", legend.title = element_blank()))
dev.off()

# relation between number of points and richness
png(filename = "outputs_biodiversity/figures_half_earth_day/crab_occurrences_grid_720_occ_pts.png",
    width = 10*200, height = 10*200, res = 200)
ggplot(grid_720_crabs) + geom_point(aes(x = nbr_occ, y = nbr_spp)) +
  scale_x_log10() + theme_bw() +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 25))+
  xlab("Number of points") + ylab("Number of species")
dev.off()


##############################################################################################################
#### Compare regions depending on their richness and sampling
##############################################################################################################
# Madagascar versus Gulf of Mexico

# Madagascar
dat_occ_reg_MDG <- dat_occ_reg_2 %>% 
  filter(int_eco_vec == 128,
         !is.na(latitude))
sppdat_sf <- st_as_sf(dat_occ_reg_MDG, coords = c("longitude", "latitude"), crs = 4326)
sppdat_sf <- st_transform(sppdat_sf, crs = st_crs(grid_720))
sppdat_sf_720 <- st_join(sppdat_sf, grid_720, left=TRUE, largest=FALSE) %>% 
  dplyr::select(ID_720, accepted_name) %>% 
  group_by(ID_720, accepted_name) %>% 
  summarize(nb_occ = length(accepted_name)) %>% 
  st_drop_geometry() %>% 
  spread(key = accepted_name, value = nb_occ, fill = 0)

library(vegan)
sp1 <- specaccum(sppdat_sf_720, method = "random")
summary(sp1)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", xlim = c(0,140))
boxplot(sp1, col="yellow", add=TRUE, pch="+", xlim = c(0,140))


# Gulf of Mexico
dat_occ_reg_MEX <- dat_occ_reg_2 %>% 
  filter(int_eco_vec == 71,
         !is.na(latitude))
sppdat_sf <- st_as_sf(dat_occ_reg_MEX, coords = c("longitude", "latitude"), crs = 4326)
sppdat_sf <- st_transform(sppdat_sf, crs = st_crs(grid_720))
sppdat_sf_720 <- st_join(sppdat_sf, grid_720, left=TRUE, largest=FALSE) %>% 
  dplyr::select(ID_720, accepted_name) %>% 
  group_by(ID_720, accepted_name) %>% 
  summarize(nb_occ = length(accepted_name)) %>% 
  st_drop_geometry() %>% 
  spread(key = accepted_name, value = nb_occ, fill = 0)

sp1 <- specaccum(sppdat_sf_720, method = "random")
summary(sp1)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", xlim = c(0,140))
boxplot(sp1, col="yellow", add=TRUE, pch="+", xlim = c(0,140))


##############################################################################################################
#### MPAs & crabs
##############################################################################################################

# mpa <- st_read("C:/Users/auror/Downloads/seafloor_meow_deepsea_MPA_intersectonly_2021_1013/seafloor_meow_deepsea_MPA_intersectonly_2021_1013.shp")
mpa_int <- st_read("E:/Yale data/Biogeography/Intersections_OCT_2021/seafloor_meow_deepsea_MPA_clipped/seafloor_meow_deepsea_MPA_clipped.shp")

biog_areas <- mpa_int %>% 
  st_drop_geometry() %>% 
  group_by(ID) %>% 
  summarize(biog_area = sum(Shape_Area, na.rm=TRUE))

mpa_in_biog_areas <- mpa_int %>% 
  st_drop_geometry() %>% 
  filter(FID_main_P > 0) %>% 
  group_by(ID) %>% 
  summarize(mpa_in_biog_area = sum(Shape_Area, na.rm=TRUE))

mpa_coverage <- full_join(biog_areas, mpa_in_biog_areas, by = "ID") %>% 
  mutate(biog_area = biog_area/1e6,
         mpa_in_biog_area = mpa_in_biog_area/1e6,
         prop_protected = mpa_in_biog_area/biog_area*100,
         prop_protected = ifelse(is.na(prop_protected), 0, prop_protected)
  )

eco_crabs_protected <- left_join(plot_biodiversity, mpa_coverage, by = "ID") %>% 
  mutate(type = ifelse(type=="MEOW", "coastal", type))

# scatter plots of crabs and protection
png(filename = "outputs_biodiversity/figures_half_earth_day/scatter_crab_mpa_hadal.png",
    width = 13*200, height = 10*200, res = 200)
print(ggplot(eco_crabs_protected) +
        geom_point(aes(y = richness, x = prop_protected, colour = type), alpha = 0.7,
                   size = 7) +
        theme_minimal() +
        xlab("% Regional protection") + ylab("Regional Richness") +
        scale_colour_manual(values = c(rev(brewer.pal(9, "Blues"))[c(1,3,6)],"black")) +
        theme(axis.line = element_line(colour = "black"),
              text = element_text(size = 50),
              legend.title = element_blank(),
              legend.position = "none"))
dev.off()

# scatter plots of crabs and protection
eco_crabs_protected_2 <- left_join(plot_biodiversity, mpa_coverage, by = "ID") %>% 
  mutate(type = ifelse(type=="MEOW", "coastal", type),
         richness = ifelse(is.na(richness), 0, richness))

png(filename = "outputs_biodiversity/figures_half_earth_day/scatter_crab_mpa_w_hadal.png",
    width = 13*200, height = 10*200, res = 200)
print(ggplot(eco_crabs_protected_2) +
        geom_point(aes(y = richness, x = prop_protected, colour = type), alpha = 0.7,
                   size = 7) +
        theme_minimal() +
        xlab("% Regional protection") + ylab("Regional Richness") +
        scale_colour_manual(values = c(rev(brewer.pal(9, "Blues"))[c(1,3,6)],"black")) +
        theme(axis.line = element_line(colour = "black"),
              text = element_text(size = 50),
              legend.title = element_blank(),
              legend.position = "none"))
dev.off()

explore_pts <- eco_crabs_protected %>% 
  filter(richness>0) %>%
  arrange(desc(prop_protected))

# scatter plots of crabs and protection - total rarity
ggplot(eco_crabs_protected) +
  geom_point(aes(y = total_rarity, x = prop_protected, colour = type), alpha = 0.7,
             size = 3) +
  theme_minimal() +
  xlab("% protected by MPAs") + ylab("Total rarity") +
  scale_colour_manual(values = rev(brewer.pal(9, "Blues"))[c(1,3,6)]) +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 20),
        legend.title = element_blank())
