
# Load libraries ----
library(sf)
# library(magrittr)
# library(rvest)
# library(tools)
# library(mapview)
library(tmap)
# library(units)
library(stringdist)
library(tidyverse)

# Load pre-created objects (from prep_SIAP_data.R)
load('data/helpers/initial_vars.RData')
load('data/helpers/functions.RData')
crops_dir <- 'data/data_out/polys_ag_INEGI_wFMG_pcts'
pol_pt_dir <- 'data/data_out/pollinator_points'

# Choose a crop ----------------------------------------------------------------
cult <- 'Melón'
cult <- 'Cucumis melo'

# Get pollinator points for given crop -----------------------------------------
# List relevant pollinators ----
# Import CSV (sep=';')f rom Apendice2
fname <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice2.csv'
df <- read_delim(fname, delim=';', trim_ws=T) %>% 
  fill(Familia, Cultivo) %>% 
  mutate(genus = str_replace_all(Polinizador, ' .*$', ''))
# df %>% dplyr::select(Grupo) %>% distinct

# Get list of pollinators for chosen crop
pols <- df %>% 
  filter(Cultivo == cult) %>% 
  dplyr::select(Polinizador, genus) 

# Get the pollinator distribution ----
# Create dir for files
cultivo_dir <- file.path(pol_pt_dir, 'pollinators_by_crop', 
                         str_replace_all(cult, ' ', '_'))
unlink(cultivo_dir, recursive=T)
dir.create(cultivo_dir)

# Points for one group
filter_pts <- function(fp, pols, cultivo_dir){
  
  pts <- st_read(fp)
  
  # Filter points to genus in list
  pts_no_geom <- pts %>% 
    st_set_geometry(NULL)
  
  pts_gen_list <- pts_no_geom %>% 
    dplyr::select(genus) %>% 
    distinct %>% 
    deframe
  
  pols_filt <- pols %>% 
    filter(
      genus %in% pts_gen_list
      )
  
  if(nrow(pols_filt) < 1) return()
  
  # Dissolve points by species
  pts_diss <- pts %>% 
    group_by(species, genus, family, order) %>% 
    summarize %>% 
    ungroup
  
  # Separate pollinators into those with species match vs. those with genus match
  pts_spec_list <- pts_no_geom %>% 
    dplyr::select(species) %>% 
    distinct %>% 
    deframe
  
  pols_specs_filt <- pols %>% 
    filter(Polinizador %in% pts_spec_list)
  
  pols_gen_filt <- pols_filt %>% 
    filter(!Polinizador %in% pts_spec_list)
  
  # Filter points conditionally to genus or species
  out_pts <- pts_diss %>% 
    filter(
      genus %in% pols_gen_filt$genus | 
      species %in% pols_specs_filt$Polinizador
      )

  # Create file name
  name <- fp %>% 
      basename %>% 
      file_path_sans_ext
  fp_out <- file.path(cultivo_dir, str_c(
      name, '_', str_replace_all(cult, ' ', '_'), '.geojson')
    )
  
  # Write file
  out_pts %>% st_write(fp_out, delete_dsn=T, driver='GeoJSON')
  
  # Return filename
  return(fp_out)
}

# Get matching points from each pollinator group
fps <- list.files(pol_pt_dir, pattern='.geojson', full.names = T)
fps %>% map(filter_pts, pols, cultivo_dir)

# Merge files from individual municipios
out_pts <- list.files(cultivo_dir, full.names=T) %>% 
  map(st_read) %>% 
  mapedit:::combine_list_of_sf()

# Save file
fp_out <- file.path(pol_pt_dir, 'pollinators_by_crop', 
                    str_c(str_replace_all(cult, ' ', '_'), '.geojson'))
out_pts %>% st_write(fp_out, delete_dsn=T, driver='GeoJSON')
out_pts <- st_read(fp_out)

# Get crop polygons ------------------------------------------------------------
# Get states where crop is grown ----
# Load 
load("data/data_out/r_data/area_sembrada_by_season_2019.RData")

# Get matching crop name
vars <- area_cult_primperen %>% colnames
colnum <- amatch('Melón', vars, maxDist=1)
(var <- vars[[colnum]])

# Get states where crop is grown
state_cves <- area_cult_primperen %>% 
  filter(across(all_of(var), ~ !is.na(.x))) %>% 
  select(CVE_ENT) %>% 
  distinct

# Get state codes
est_to_cve_tbl <- est_to_cve %>% 
  as_tibble(rownames = 'estado') %>% 
  filter(value %in% state_cves$CVE_ENT)

# Get files matching state codes
search_str <- str_c('.*primperen.*(', 
                str_c(est_to_cve_tbl$estado, collapse='|'), 
                ')\\.geojson')
fps <- list.files(crops_dir, pattern=search_str, full.names=T)

fp <- fps[[1]]

filter_polys <- function(fp, var){
  # Load polygons for state
  polys <- st_read(fp)
  
  # Filter to crop of interest
  polys %>%
    filter(across(var, ~ !is.na(.x))) %>%
    select(CVE_ENT:total_noFMG, all_of(var))
}

polys_crop <- filter_polys(fp)

polys_all <- fps %>% 
  map(filter_polys, var) %>%
  mapedit:::combine_list_of_sf()

var <- 'Melon'
fp_out <- file.path('data/data_out/polys_ag_INEGI_wFMG_pcts/specific_crops', 
                    str_c(var, '.geojson'))
polys_all %>% st_write(fp_out)

# Map --------------------------------------------------------------------------
mex <- raster::getData('GADM', country='MEX', level=2, 
    path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) 

tmaptools::palette_explorer()
tmap_mode('view')
(tm <- tm_shape(mex) +
    tm_borders(col='darkgray') + 
    tm_shape(polys_all) +
    tm_fill(col = 'Melón', palette='YlGnBu', legend.show=T) +
    tm_shape(out_pts) +
    tm_dots(alpha=0.4, size=.1, col = 'family', palette=c('#b10026', '#0c2c84'), legend.show=F) 
    # tm_dots(alpha=0.4, size=.1, col = 'black') +
    # tm_facets(by = 'genus', free.coords=F)
    )
# tmap_save(tm, filename = file.path('figures', str_c('pol_', name, '_map_species_25.png')))

# 
