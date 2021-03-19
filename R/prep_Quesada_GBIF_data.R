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
  
  # Use taxize to get superfamily (because not included in )
  fam_list <- dat %>% 
    dplyr::select(family) %>% 
    distinct %>% 
    mutate(superfamily = taxize::tax_name(family, get='superfamily', db='ncbi')[[3]])
  
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
