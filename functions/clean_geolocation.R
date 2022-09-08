clean_geolocation <- function(dat_occ, input_source = "NA"){
  
  require(CoordinateCleaner)
  # only useful options from coordinate cleaner are: equal, outliers, zeros
  
  # for GBIF
  if(input_source=="GBIF"){
    dat_occ <- clean_coordinates(dat_occ, lon = "decimalLongitude", lat = "decimalLatitude",
                            species = "accepted_name",
                            tests = c("equal","outliers","zeros"), outliers_method = "quantile") %>% 
      mutate(# Clean and flag coordinates
             flag_coordinate = NA_character_,
             flag_coordinate = case_when(`.equ`==FALSE ~ "equal_coordinates",
                                         `.zer`==FALSE ~ "zero_island",
                                         `.val`==FALSE ~ "wrong_coordinates",
                                         TRUE ~ NA_character_),
             flag_outlier = case_when(`.otl`==FALSE ~ "geographical_outlier",
                                      TRUE ~ NA_character_),
             
             # Flat coordinate uncertainties
             flag_coord_uncertainty = case_when(is.na(coordinateUncertaintyInMeters) ~ "missing_uncertainty",
                                                     coordinateUncertaintyInMeters > 50000 ~ "high_uncertainty"),
             # Depth flags
             flag_depth = case_when(is.na(depth) ~ "missing_depth",
                                    depthAccuracy>50 ~ "depth_high_uncertainty",
                                    TRUE ~ NA_character_)) %>% 
      select(-`.val`, -`.equ`, -`.zer`, -`.otl`, -`.summary`)
  }
  
  # for OBIS
  if(input_source=="OBIS"){
    dat_occ <- clean_coordinates(dat_occ, lon = "decimallongitude", lat = "decimallatitude",
                                 species = "accepted_name",
                                 tests = c("equal","outliers","zeros"), outliers_method = "quantile") %>% 
      mutate(# Clean and flag coordinates
             flag_coordinate = NA_character_,
             flag_coordinate = case_when(`.equ`==FALSE ~ "equal_coordinates",
                                         `.zer`==FALSE ~ "zero_island",
                                         `.val`==FALSE ~ "wrong_coordinates",
                                         TRUE ~ NA_character_),
             flag_outlier = case_when(`.otl`==FALSE ~ "geographical_outlier",
                                      TRUE ~ NA_character_),
             
             # Flat coordinate uncertainties
             flag_coord_uncertainty = case_when(is.na(coordinateuncertaintyinmeters) ~ "missing_uncertainty",
                                                coordinateuncertaintyinmeters > 50000 ~ "high_uncertainty"),
             
             # Depth flags
             depthAccuracy = maximumdepthinmeters - minimumdepthinmeters,
             flag_depth = case_when(depthAccuracy>50 ~ "depth_high_uncertainty",
                                    is.na(verbatimdepth) ~ "missing_depth",
                                    TRUE ~ NA_character_)) %>% 
      select(-`.val`, -`.equ`, -`.zer`, -`.otl`, -`.summary`)
  }
  return(dat_occ)
}