# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(units)
library(tmap)
tmap_mode('view')
library(mapview)
library(patchwork)
library(vegan)
library(viridis)

# Initialize -------------------------------------------------------------------
buffer_distance <- set_units(10, 'km')
date_range <- c(2000, 2020)
crs <- 6362 # used by INEGI for all of Mexico
# crs <- 6372 # more western focus? This is the one I was using for processing

anp_fp <- 'data/input_data/context_Mexico/SHAPE_ANPS/182ANP_Geo_ITRF08_Julio04_2019.shp'
anp_dir <- 'data/data_out/ANPs'
anp_terr_fp <- file.path(anp_dir, 'ANPs_terr_singlepart.geojson')
anps_biom_fp <- file.path(anp_dir, 'ANPs_with_biomes.geojson')

pol_groups <- c('Abejas', 'Avispas', 'Colibries', 'Mariposas', 'Moscas', 'Murcielagos')

anp_stats_fp <- file.path(anp_dir, 
                          str_c('anps_allpols_', date_range[[1]], '_to_', 
                                date_range[[2]], '_buffer', buffer_distance, 'km.csv'))

# Mexico
mex <- st_read('data/input_data/context_Mexico/SNIB_divisionpolitica/dest2018gw/dest2018gw.shp') %>% 
  st_transform(crs=crs) %>% 
  st_simplify(dTolerance=20, preserveTopology=T) 

# Fill in rivers and lakes (but it removes islands as well and I haven't figured out how to effectively address that)
# mex_fill <- mex %>% 
#   st_buffer(300) %>% st_buffer(-300) %>% 
#   rmapshaper::ms_filter_islands(10000000) %>% 
#   nngeo::st_remove_holes() %>% 
#   st_make_valid() %>% 
#   st_simplify(dTolerance=20, preserveTopology=T) 
# islas <- mex %>% 
#   st_collection_extract("POLYGON") %>% 
#   st_cast("POLYGON") %>% 
#   st_filter(mex_fill, .predicate=st_disjoint())

# Load pollinator file
name <- 'Abejas'
pol_dir = 'data/data_out/pollinator_points/no_duplicates'

# Load
pol_fp <- file.path(pol_dir, str_c(name, '.geojson'))
pol_df <- st_read(pol_fp)

# Convert date range
date_min <- as.POSIXct(str_c(date_range[[1]], "-01-01"))
date_max <- as.POSIXct(str_c(date_range[[2]], "-12-31"))

# Filter to date range
pol_df <- pol_df %>% filter(eventDate >= date_min & eventDate <= date_max) 

# Create subset polygons =======================================================
ests <- c('Chiapas', 'Tabasco')

# Create subset polygons
sub1 <- mex %>% filter(NOM_ENT %in% ests)

# Fill in rivers and lakes
sub1 <- sub1 %>% 
  st_buffer(300) %>% st_buffer(-300) %>% 
  rmapshaper::ms_filter_islands(10000000) %>% 
  nngeo::st_remove_holes() %>% 
  st_make_valid()

# Subset other layers ----------------------------------------------------------
# Load all zones and filter to region
bind_fp <- file.path(anp_dir, str_c('ANPs_allzones_biomes_buffer_', 
                 buffer_distance,'km.gpkg'))
anps_biom_bind <- st_read(bind_fp) %>% 
  st_transform(crs) %>% 
  st_filter(sub1)

# Create land cover polygons (natural, cropland, other) ========================
# fp_usv <- "data/input_data/ag_INEGI_2017/conjunto_de_datos/usv250s6g.shp"
# usv <- st_read(fp_usv, crs=6362) %>% 
#   st_make_valid() %>% 
#   st_crop(sub1) %>% 
#   st_simplify(dTolerance=20, preserveTopology=T)
# 
# usv %>% object.size() %>% print(units='MB')
# usv %>% tbl_vars()
# usv %>% st_drop_geometry() %>% distinct(CLAVE, TIP_INFO, TIPAGES, TIP_CUL1)
# usv %>% st_drop_geometry() %>% filter(CLAVE=='IEFF')
# 
# usv_diss <- usv %>% 
#   group_by(TIP_INFO, TIPAGES, CLAVE) %>% 
#   summarize()
# 
# usv_diss %>% object.size() %>% print(units='MB')
# 
# fn <- tools::file_path_sans_ext(basename(fp_usv))
# fn <- str_c(str_c(fn, 'diss', str_c(ests, collapse=''), sep='_'), '.gpkg')
# fp <- file.path("data/intermediate_data/ag_by_region", fn)
# 
# usv_diss %>% st_write(fp, delete_dsn=T)
# 
# # Look
# tm_shape(usv_diss) + tm_polygons()

# From CONABIO ----
fp_usv <- "data/input_data/ag_INEGI_2017/other_sources/usv250s6gw.shp"

fn <- str_c(str_c(tools::file_path_sans_ext(basename(fp_usv)), 
                  'tipo', '7classes', str_c(ests, collapse=''), sep='_'), '.gpkg')
crop_class_fp <- file.path("data/intermediate_data/ag_by_region", fn)

fn <- str_c(str_c(tools::file_path_sans_ext(basename(fp_usv)), 
                  'diss', '3class', str_c(ests, collapse=''), sep='_'), '.gpkg')
diss_fp <- file.path("data/intermediate_data/ag_by_region", fn)

# Load file
usv <- st_read(fp_usv) %>% 
  st_make_valid() %>% 
  st_transform(crs) %>% 
  st_crop(sub1) %>% 
  st_simplify(dTolerance=20, preserveTopology=T) %>% 
  nngeo::st_remove_holes(100000)

usv %>% object.size() %>% print(units='MB')

# classify into 7 classes
usv <- usv %>% 
  mutate(tipo = case_when(
    str_detect(DESCRIPCIO, 'INDUCIDO$') ~ 'inducido',
    str_detect(DESCRIPCIO, '^PASTIZAL') ~ 'pastizal',
    str_detect(CVE_UNION, '^V|^B|^S|^P') ~ 'vegetacion',# bosque, selva, pastizal, sabana, palmar, etc.
    str_detect(DESCRIPCIO, '^AGRICULTURA') ~ 'agricultura', # does not include shifting cultivation (nómada)
    str_detect(CVE_UNION, '^ACUI|^H2O') ~ 'agua',
    str_detect(CVE_UNION, '^ADV|^DV') ~ 'sin_veg',
    str_detect(CVE_UNION, '^AH') ~ 'construido',
    TRUE ~ 'otro'
  ))

usv %>% object.size() %>% print(units='MB')

# Save
usv %>% st_write(crop_class_fp, delete_dsn=T)
usv <- sf::st_read(crop_class_fp)

# Reclass and dissolve
key <- c(inducido='veg', pastizal='veg', vegetacion='veg', agricultura='ag', 
         agua='otro', sin_veg='otro', construido='otro')
usv_diss <- usv %>% 
  mutate(tipo =  recode(tipo, !!!key)) %>% 
  group_by(tipo) %>% 
  summarize() %>% 
  st_simplify(dTolerance=20, preserveTopology=T) %>% 
  nngeo::st_remove_holes(100000) 

usv_diss %>% object.size() %>% print(units='MB')

# Save
usv_diss %>% st_write(diss_fp, delete_dsn=T)
usv_diss <- st_read(diss_fp)

# all agricultural polygons (ag and nómada) ----
fn <- str_c(str_c('polys_ag_INEGI', str_c(ests, collapse=''), sep='_'), '.gpkg')
fp <- file.path("data/intermediate_data/ag_by_region", fn)

if(file.exists(fp)) {
  
  polys <- st_read(fp)
  
} else {
  
  polys_fp <- file.path("data/intermediate_data", 'polys_ag_INEGI.gpkg')
  polys <- st_read(polys_fp) %>% 
    st_filter(sub1)
  
  polys %>% st_write(fp)
  
}

nma_diss <- polys %>% 
  filter(CLAVE == 'NMA') %>% 
  st_crop(sub1) %>% 
  group_by(TIPAGES) %>% 
  summarize()

fn <- str_c(str_c(tools::file_path_sans_ext(basename(fn)), 
                  'diss', '3classes', str_c(ests, collapse=''), sep='_'), '.gpkg')
nma_fp <- file.path("data/intermediate_data/ag_by_region", 'polys_nma_ChiapasTabasco.gpkg')

nma_diss %>% st_write(fp, delete_dsn=T)


# Look
# tm_shape(sub1) + tm_polygons() + 
  tm_shape(anps_biom_bind2) + tm_polygons(col='zone') +
  tm_shape(polys) + tm_polygons(col='TIPAGES')
  
# ext <- st_bbox(c(xmin=3431067.2, ymin=419149.2, 
#          xmax=3543888.6, ymax=573014.8))
# usv_diss_crop <- usv_diss %>% 
#   st_crop(ext)
# tm_shape(usv_diss_crop) + tm_fill(col='pink') 
  

