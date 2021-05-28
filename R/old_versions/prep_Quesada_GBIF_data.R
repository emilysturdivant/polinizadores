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
fp <- fps[[4]]

(name <- fp %>% 
    basename %>% file_path_sans_ext %>% 
    str_split('_') %>% last %>% last)

# Load data ----
temp_fp <- file.path(data_dir, 'extras', str_c(name, '.csv'))
if(!file.exists(temp_fp)){
  
  dat <- read_csv(fp)
  
  # List unique genuses
  gen_list <- dat %>% 
    dplyr::select(genus) %>% 
    distinct %>% 
    filter(!is.na(genus))
  
  # Use taxize to get tribe and superfamily
  new_taxons <- gen_list %>% 
    purrr::map_dfr(taxize::tax_name, get=c('tribe', 'superfamily'), db='both')
  
  # Tidy results
  new_taxons <- new_taxons %>% 
    group_by(query) %>% 
    fill(tribe, superfamily, .direction = 'down') %>% 
    fill(tribe, superfamily, .direction = 'up') %>% 
    slice(1) %>% 
    ungroup %>% 
    dplyr::select(genus = query, tribe, superfamily)
  
  # Join to df
  dat <- dat %>% 
    left_join(new_taxons) 
  
  if(name == 'Mariposas'){
    dat <- dat %>% 
      mutate(nocturna = case_when(
        superfamily %in% c('Papilionoidea', 'Hesperioidea', 'Hedyloidea') ~ 'diurna',
        !is.na(superfamily) ~ 'nocturna'
      ))
  }
  
  # Save
  dat %>% write_csv(temp_fp)
  
} else {
  
  dat <- read_csv(temp_fp)
  
}

# Data tidying steps: drop coords with NAs, drop duplicates, flag/drop imprecise
vars <- c('species', 'genus', 'tribe', 'family', 'superfamily', 'order', 'class', 'nocturna', 
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
sf_df %>% st_write(fp_out, append = FALSE)

# drop duplicates ----
sf_df2 <- df %>% 
  distinct %>% 
  st_as_sf(x = .,                         
           coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

# Save
fp_out <- file.path('data/data_out/pollinator_points/no_duplicates',
                    str_c(name, '.gpkg'))
sf_df2 %>% st_write(fp_out, append = FALSE)

# Get list of unique species and join to species in Informacion_general ----
pol_group <- 'Abejas'

# Load pollinator file
pol_dir <- 'data/data_out/pollinator_points/with_duplicates'
pol_fp <- file.path(pol_dir, str_c(pol_group, '.gpkg'))

# get list of species and match to those in Quesada table
sp_df <- st_read(pol_fp) 

sp_lst <- sp_df %>% 
  st_drop_geometry() %>% 
  distinct(species, genus, family, order)

distinct(sp_lst, family)

# Read Informacion_general
infogen <- readxl::read_excel('data/input_data/Quesada_bioclim_pol_y_cultivos/Informacion_general.xlsx', 
                              'Polinizadores', skip=1)

# Get bee species
info_abejas <- infogen %>% filter(Orden == 'Hymenoptera')
info_sin <- info_abejas %>% 
  filter(!is.na(Sinonimias)) %>% 
  mutate(Especie = Sinonimias)
info_abejas <- bind_rows(info_abejas, info_sin)

# Get bee species
info_abejas <- infogen %>% filter(Orden == 'Hymenoptera')
info_sin <- info_abejas %>% 
  filter(!is.na(Sinonimias)) %>% 
  mutate(Especie = Sinonimias)
info_abejas <- bind_rows(info_abejas, info_sin)

# Check matches
semi_join(info_abejas, sp_lst, by=c(Especie='species'))
anti_join(info_abejas, sp_lst, by=c(Especie='species'))

sp_lst <- left_join(sp_lst, 
                    select(info_abejas, Especie, ID_numerico, ID_código), 
                    by=c(species='Especie'))
sp_lst %>% arrange(ID_código) %>% 
  write_csv('data/input_data/Quesada_bioclim_pol_y_cultivos/intermediate/Abejas_especies_unicas.csv',
            na='')
