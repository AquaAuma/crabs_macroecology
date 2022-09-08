clean_realm <- function(dat_occ, input_source = "NA"){
  
  require(sf)
  
  # load ocean layer
  ocean <- st_read("~/Yale University/MOL-inverts/Geography/Realm_layers/Ocean_layer/ocean_layer_2021-04-29.gpkg")
  #st_is_valid(ocean), yes, ocean is valid!
  
  if(input_source == "GBIF"){
    dat_pts <- st_as_sf(dat_occ, coords = c("decimalLongitude","decimalLatitude"),
                        crs = st_crs(ocean))
    tt <- data.frame(st_intersects(dat_pts, ocean, sparse = FALSE))
    names(tt) <- "tt"
    dat_occ <- cbind(dat_occ, tt) %>% 
      mutate(flag_realm = ifelse(tt==FALSE, "not_marine", NA_character_)) %>% 
      select(-tt)
  }
  
  if(input_source == "OBIS"){
    dat_pts <- st_as_sf(dat_occ, coords = c("decimallongitude","decimallatitude"),
                        crs = st_crs(ocean))
    tt <- data.frame(st_intersects(dat_pts, ocean, sparse = FALSE))
    names(tt) <- "tt"
    dat_occ <- cbind(dat_occ, tt) %>% 
      mutate(flag_realm = ifelse(tt==FALSE, "not_marine", NA_character_)) %>% 
      select(-tt)
  }
  
  return(dat_occ)
  
}

# # testing st_within
# s_time <- Sys.time()
# ss <- st_within(dat_pts, ocean, sparse = FALSE)
# e_time <- Sys.time()
# print(s_time-e_time) 17min
# 
# # testing st_intersects
# s_time <- Sys.time()
# tt <- st_intersects(dat_pts, ocean, sparse = FALSE)
# e_time <- Sys.time()
# print(s_time-e_time) 18sec
# st_intersects MUCH FASTER and results are the same