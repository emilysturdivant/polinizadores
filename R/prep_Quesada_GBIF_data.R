# Pre-process GBIF data compiled by Mauricio Quesada's team
# parameters: https://www.gbif.org/developer/occurrence#parameters

# Load libraries ---------------------------------------------------------------
library(tabulizer)
library(sf)
library(rgdal)
library(tools)
library(raster)
library(scales)
library(taxize)
library(tidyverse)

# Initialize -------------------------------------------------------------------
fig_dir <- 'figures/pol_exploration_no_iNaturalist/date_gt2009'
name <- 'Abejas'
# fp_out <- file.path('data/data_out/pollinator_points/no_duplicates', str_c(name, '.gpkg'))
# df <- st_read(fp_out)

# Pollinator database ----
data_dir <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/Completos'
fps <- list.files(data_dir, full.names = T)
fp <- fps[[5]]

(name <- fp %>% 
    basename %>% file_path_sans_ext %>% 
    str_split('_') %>% last %>% last)

# Load data ----
temp_fp <- file.path(data_dir, 'extras', str_c(name, '.csv'))
if(!file.exists(temp_fp)){
  
  dat <- read_csv(fp)
  
  fam_list <- dat %>% 
    dplyr::select(family) %>% 
    distinct %>% 
    mutate(superfamily = tax_name(family, get='superfamily', db='ncbi')[[3]])
  
  dat <- dat %>% 
    left_join(fam_list) 
  
  if(name == 'Mariposas'){
    dat <- dat %>% 
      mutate(nocturna = case_when(
        superfamily %in% c('Papilionoidea', 'Hesperioidea', 'Hedyloidea') ~ 'diurna',
        !is.na(superfamily) ~ 'nocturna'
      ))
  }
  
  dat %>% write_csv(temp_fp)
  
} else {
  
  dat <- read_csv(temp_fp)
  
}

# Data tidying steps: drop coords with NAs, drop duplicates, flag/drop imprecise
vars <- c('species', 'genus', 'family', 'superfamily', 'order', 'class', 'nocturna', 
          'decimalLongitude', 'decimalLatitude', 
          'eventDate', 'coordinateUncertaintyInMeters', 'habitat', 
          'basisOfRecord', 'country', 'stateProvince', 'institutionCode')

dat %>% dplyr::select(matches(vars)) %>% distinct %>% nrow
df <- dat %>% 
  drop_na(decimalLongitude, decimalLatitude) %>% 
  filter(decimalLongitude != 0 & decimalLatitude != 0) %>% 
  filter(!str_detect(issues, 'COUNTRY_COORDINATE_MISMATCH')) %>% 
  filter(coordinateUncertaintyInMeters < 1000 | is.na(coordinateUncertaintyInMeters)) %>% 
  filter(institutionCode != 'iNaturalist') %>% 
  dplyr::select(matches(vars))

sf_df <- df %>% 
  st_as_sf(x = .,                         
           coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

# Save
fp_out <- file.path('data/data_out/pollinator_points/with_duplicates', 
                    str_c(name, '.gpkg'))
sf_df %>% st_write(fp_out, delete_dsn=T)

# drop duplicates ----
sf_df2 <- df %>% 
  distinct %>% 
  st_as_sf(x = .,                         
           coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

# Save
fp_out <- file.path('data/data_out/pollinator_points/no_duplicates',
                    str_c(name, '.gpkg'))
sf_df2 %>% st_write(fp_out, delete_dsn=T)
