clean_countries <- function(dat_occ, input_source = "NA"){
  
  require(countrycode)
  
  # for GBIF
  if(input_source=="GBIF"){
    dat_occ <- dat_occ %>% 
      mutate(country_code = as.character(str_to_upper(countryCode)),
             country = case_when(str_length(country_code)==2 ~ countrycode(sourcevar = country_code, origin = "iso2c", destination = "iso.name.en"),
                                 str_length(country_code)==3 ~ countrycode(sourcevar = country_code, origin = "iso3c", destination = "iso.name.en"),
                                 TRUE ~ NA_character_),
             flag_country = ifelse(is.na(country_code) || country_code=="", "missing_country", NA_character_)) %>% 
      select(-countryCode)
  }
  
  # for OBIS
  if(input_source=="OBIS"){
    dat_occ <- dat_occ %>% 
      mutate(country_code = str_to_upper(as.character(countrycode)),
             country = case_when(str_length(country_code)==2 ~ countrycode(sourcevar = country_code, origin = "iso2c", destination = "iso.name.en"),
                                 str_length(country_code)==3 ~ countrycode(sourcevar = country_code, origin = "iso3c", destination = "iso.name.en"),
                                 TRUE ~ NA_character_),
             flag_country = ifelse(is.na(country_code) || country_code=="", "missing_country", NA_character_)
             ) %>% 
      select(-countrycode)
  }
  
  return(dat_occ)
  
}