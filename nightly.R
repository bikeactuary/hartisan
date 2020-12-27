library(RSocrata)
library(dplyr)
library(tidygeocoder)
# library(ggmap)
library(magrittr)
library(cwi)
library(sf)

date <- Sys.Date() - 365
url <- paste0("https://data.hartford.gov/resource/p2vw-4aab.csv?$where=apply_date>'", date, "'")
dfPermits <- read.socrata(url)

new_addr <- dfPermits %>%
  select(address, unit_or_suite) %>%
  mutate(pos = str_locate(toupper(address), pattern = "UNIT|BLDG|BLD|#|ATRIUM")[, 1] ) %>%
  # mutate(pos = case_when(is.na(pos) ~ str_locate(substr(address, str_locate(address, " ST| AV| AVE")[, 2] + 1, str_length(address)),
  #                                                pattern = "[0-9]" )[, 1] +  str_locate(address, " ST| AV| AVE")[, 2],
  #                        !is.na(pos) ~ pos)) %>%
  # mutate(pos2 = case_when(is.na(pos) ~ str_locate(substr(address, 
  #                                                        str_locate(address, "([1-9][0-9][0-9][0-9]|[1-9][0-9][0-9]|[1-9][0-9]|[1-9])")[, 2] + 2,
  #                                                        str_length(address)),
  #                                                 " AV| ST EXT| ST| AVE| BLVD| BLV| DR| WAY| LA")[, 2] + str_locate(address, "([1-9][0-9][0-9][0-9]|[1-9][0-9][0-9]|[1-9][0-9]|[1-9])")[, 2] + 2,
  #                         !is.na(pos) ~ NA_real_),
  mutate(pos2 = str_locate(substr(toupper(address), 
                                  str_locate(address, "([1-9][0-9][0-9][0-9]|[1-9][0-9][0-9]|[1-9][0-9]|[1-9])")[, 2] + 2,
                                  str_length(address)),
                           " AV| ST EXT| ST| AVE| BLVD| BLV| DR| WAY| LA| PLC")[, 2] + str_locate(address, "([1-9][0-9][0-9][0-9]|[1-9][0-9][0-9]|[1-9][0-9]|[1-9])")[, 2] + 2) %>%
  mutate(pos2 = case_when(pos2 > str_length(address) ~ NA_integer_,
                          TRUE ~ pos2 %>% as.integer),
         pos = case_when(is.na(pos) | pos2 < pos ~ pos2,
                         TRUE ~ pos),
         pos = case_when(pos > str_length(address) ~ NA_integer_,
                         TRUE ~ pos)) %>%
  mutate(address_fix = case_when(is.na(pos) ~ address,
                                 TRUE ~ substr(address, 1, pos - 1))) %>%
  mutate(unit_fix = case_when( (is.na(unit_or_suite) | str_length(trimws(unit_or_suite)) == 0) & !is.na(pos) ~ substr(address, pos, str_length(address)),
                               str_length(trimws(unit_or_suite)) > 0 ~ unit_or_suite )) %>%
  mutate(address_fix = case_when(toupper(trimws(unit_fix)) %in% c("EAST", "WEST", "NORTH", "SOUTH", "E", "W", "N", "S") ~ paste0(address_fix, " ", unit_fix),
                                 TRUE ~ address_fix),
         unit_fix  = case_when(toupper(trimws(unit_fix)) %in% c("EAST", "WEST", "NORTH", "SOUTH", "E", "W", "N", "S") ~ NA_character_,
                               TRUE ~ unit_fix) ) %>%
  mutate(city = "Hartford",
         state = "Connecticut",
         country = "US")


## OSM encode
coords <- tidygeocoder::geo(street = new_addr$address_fix,
                            city = new_addr$city,
                            state = new_addr$state,
                            country = new_addr$country,
                            method = 'osm',
                            lat = latitude,
                            long = longitude)



## geom value is from city 
sfPermits <- dfPermits %>%
  cbind(., coords[, c("street", "latitude", "longitude")]) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    agr = "constant",
    crs = 4269,        # nad83
    # stringsAsFactors = FALSE,
    remove = FALSE,
    na.fail = FALSE
  ) %>%
  st_set_crs(st_crs(cwi::hartford_sf)) %>%
  st_join(., cwi::hartford_sf, join = st_within) %>%
  mutate(key = row_number()) %>% relocate(key)

nohit <- sfPermits %>%
  filter(is.na(name)) %>%
  as_tibble() %>%
  select(-c(name, town, geometry)) %>%
  mutate(latitude = geom %>% str_match(., "\\((.*?),") %>% subset(TRUE, c(FALSE, TRUE)) %>% as.vector %>% as.numeric,
         longitude = geom %>% str_match(., "(?<=, )(.*?)(?=\\))") %>% subset(TRUE, c(FALSE, TRUE)) %>% as.vector %>% as.numeric) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    agr = "constant",
    crs = 4269,        # nad83
    # stringsAsFactors = FALSE,
    remove = FALSE,
    na.fail = FALSE
  ) %>%
  st_set_crs(st_crs(cwi::hartford_sf)) %>%
  st_join(., cwi::hartford_sf, join = st_within)

sfPermits %<>% 
  filter(!is.na(name)) %>%
  rbind(., nohit) %>%
  arrange(key) %>%
  group_split(is.na(name), .keep=FALSE)

saveRDS(sfPermits, "permit_data.RDS")
