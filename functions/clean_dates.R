clean_dates <- function(dat_occ, input_source = "NA"){
  
  # load required libraries
  require(tidyverse)
  require(lubridate)
  
  # according to data types
  if(input_source=="OBIS"){
    
    dat_occ <- dat_occ %>% 
     mutate(# flag problems with years and correct
            year = as.numeric(as.vector(year)),
            date_year = as.numeric(as.vector(date_year)),
            year = coalesce(year, date_year),
            flag_year = case_when(is.na(year) ~ "missing_year",
                                  year=="NA" ~ "missing_year",
                                  year<1970 ~ "year_prior_1970",
                                  year<1700 ~ "wrong_year",
                                  TRUE ~ NA_character_),
            
            # flag problems with months and correct
            month = as.numeric(as.vector(month)),
            flag_month = case_when(is.na(month) ~ "missing_month",
                                   month=="NA" ~ "missing_month",
                                   !month %in% c(1:12) ~ "wrong_month",
                                   TRUE ~ NA_character_),
            #month = ifelse(!month %in% c(1:12), NA, month),
            
            # flag problems with days and correct
            day = as.numeric(as.vector(day)),
            flag_day = case_when(is.na(day) ~ "missing_day",
                                 day=="NA" ~ "missing_day",
                                 !day %in% c(1:31) ~ "wrong_day",
                                 TRUE ~ NA_character_)) %>% 
            #day = ifelse(!day %in% c(1:31), NA, day)) %>% 
      select(-date_year)
  }
  
  if(input_source=="GBIF"){
    
    dat_occ <- dat_occ %>% 
      mutate(# flag problems with years and correct
             event_year = lubridate::year(eventDate),
             year = coalesce(year, event_year),
             flag_year = case_when(is.na(year) ~ "missing_year",
                                   year=="NA" ~ "missing_year",
                                   year<1970 ~ "year_prior_1970",
                                   year<1700 ~ "wrong_year",
                                   TRUE ~ NA_character_),
             
             # flag problems with months and correct
             event_month = lubridate::month(eventDate),
             month = coalesce(month, event_month),
             flag_month = case_when(is.na(month) ~ "missing_month",
                                   month=="NA" ~ "missing_month",
                                   !month %in% c(1:12) ~ "wrong_month",
                                   TRUE ~ NA_character_),
             #month = ifelse(!month %in% c(1:12), NA, month),
             
             # flag problems with days and correct
             event_day = lubridate::day(eventDate),
             day = coalesce(day, event_day),
             flag_day = case_when(is.na(day) ~ "missing_day",
                                    day=="NA" ~ "missing_day",
                                    !day %in% c(1:31) ~ "wrong_day",
                                    TRUE ~ NA_character_)
             #day = ifelse(!day %in% c(1:31), NA, day)
      ) %>% 
      select(-event_year, -event_month, -event_day)
  }
  
  return(dat_occ)
}