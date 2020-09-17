# 
# Join pollinator data to Areas Naturales Protegidas (ANPs)
# 

# Load libraries ---------------------------------------------------------------
library(sf)
library(units)
library(tidyverse)

library(mapview)

# Initialize -------------------------------------------------------------------
buffer_distance <- set_units(1, 'km')
anp_fp <- 'data/input_data/context_Mexico/SHAPE_ANPS/182ANP_Geo_ITRF08_Julio04_2019.shp'
pol_groups <- c('Abejas', 'Avispas', 'Colibries', 'Mariposas', 'Moscas', 'Murcielagos')
date_min <- as.POSIXct("2010-01-01")
date_max <- as.POSIXct("2020-06-01")


# Load data --------------------------------------------------------------------
# Mexico 
# mex <- raster::getData(
#   'GADM', country='MEX', level=0, path='data/input_data/context_Mexico') %>% 
#   st_as_sf(crs=4326) %>% 
#   st_simplify(dTolerance = 0.02)
# mex_munis <- raster::getData(
#   'GADM', country='MEX', level=2, path='data/input_data/context_Mexico') %>% 
#   st_as_sf(crs=4326) 
# mex_est <- raster::getData(
#   'GADM', country='MEX', level=1, path='data/input_data/context_Mexico') %>% 
#   st_as_sf(crs=4326) 
# 
# # Biomes
# data_dir <- 'data/input_data/environment_variables/TEOW_WWF_biome'
# shp_fp <- list.files(data_dir, '.shp$', full.names = T, recursive = T)
# biomes_crop <- st_read(shp_fp) %>% 
#   st_crop(st_bbox(mex) )
# 
# # Elevation
# alt <- raster::getData(
#   'alt', country='MEX', path='data/input_data/context_Mexico')

# Functions --------------------------------------------------------------------
count_pollinators_in_polys <- function(
  pts_sfc, polys_sfc, polys_id_fld='ID_ANP', polys_area_fld='S_TERRES'
){
  # Group pollinator points by intersecting ANP 
  pts_sfc %>% 
    # Group pollinator points by intersecting ANP 
    st_transform(crs = st_crs(polys_sfc)) %>% 
    st_join(polys_sfc, left=F) %>% 
    st_set_geometry(NULL) %>% 
    group_by(.data[[polys_id_fld]], .data[[polys_area_fld]]) %>% 
    # Get diversity and abundance of species
    summarize(no_spcs = length(unique(species)), 
              no_obs = length(species)) %>% 
    ungroup %>% 
    # Normalize by terrestrial area
    mutate(spcs_per_ha = no_spcs/.data[[polys_area_fld]], 
           obs_per_ha = no_obs/.data[[polys_area_fld]])
}

# ------------------------------------------------------------------------------
# ANPs
anps <- st_read(anp_fp)

# Prep ANP polys for processing
anps_proj <- anps %>% 
  select(ID_ANP, S_TERRES) %>% 
  st_transform(crs=6372) %>% 
  st_set_precision(1e5)  %>% 
  st_make_valid %>% 
  st_collection_extract('POLYGON') %>% 
  st_simplify(dTolerance=20)

 
get_pollinators_in_anp_zones <- function(name, date_range, anps_proj, buffer_distance){
  
  # Convert date range
  date_min <- as.POSIXct(str_c(date_range[[1]], "-01-01"))
  date_max <- as.POSIXct(str_c(date_range[[2]], "-12-31"))
  
  # Load pollinator file
  pol_fp <- file.path('data/data_out/pollinator_points', str_c(name, '.geojson'))
  df <- st_read(pol_fp) %>% 
    # Filter to date range
    filter(eventDate >= date_min & eventDate <= date_max) 
  
  # Get diversity and abundance of pollinators within NPA
  anps2 <- count_pollinators_in_polys(df, anps_proj)
  
  # NPA buffer ---- 
  buffer_fp <- str_c('data/intermediate_data/anps_buffer_', buffer_distance,'km.geojson')
  
  if(file.exists(buffer_fp)) {
    
    # Load buffer
    anps_buff <- st_read(buffer_fp)
    
  } else {
    
    # Create buffer
    anps_buff <- anps_proj %>% 
      st_buffer(buffer_distance, validate=T) %>% 
      st_difference(st_union(anps_proj))
    
    # Get area
    anps_buff$buff_area_ha <- anps_buff %>% 
      st_area %>% 
      set_units('ha') %>% 
      set_units(NULL)
    
    # Save
    anps_buff %>% st_write(buffer_fp, delete_dsn=T)
    }
  
  # Get diversity and abundance of pollinators in NPA buffer
  anps3 <- count_pollinators_in_polys(df, anps_buff, polys_area_fld='buff_area_ha') %>% 
    full_join(anps2, ., by = 'ID_ANP', suffix=c('', '_cerca'))
  
  anps3$pol_group <- name
  
  # Save
  fp_out <- file.path('data/data_out/ANPs', 
                      str_c('anps_', name, '_', strftime(date_min, format="%Y"), 
                            '_to_', strftime(date_max, format="%Y"), '_buffer', 
                            buffer_distance, 'km.csv'))
  anps3 %>% write_csv(fp_out)
  
  # Return
  return(anps3)
}

# Pollinators
date_range <- c(2010, 2020)
out_anps <- pol_groups %>% 
  map(get_pollinators_in_anp_zones, date_range, anps_proj, buffer_distance)
  
out_allpols <- out_anps %>% reduce(rbind) %>% 
  pivot_wider(id_cols = ID_ANP, names_from = pol_group, values_from = no_spcs:obs_per_ha_cerca)


# ----
# Look
mapview(anps3, zcol='obs_per_ha') +
  mapview(df)


