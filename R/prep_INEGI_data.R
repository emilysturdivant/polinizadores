# Load libraries ----
box::use(R/functions[...])
library(sf)
library(rvest)
library(tools)
library(mapview)
library(units)
library(mapedit)
library(tidyverse)

# f.usv.16 <- 'http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/tematicas/uso_suelo/889463173359_s.zip'
# Links from https://www.inegi.org.mx/temas/usosuelo/default.html#Descargas
usv_url <- 'http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/tematicas/uso_suelo/1_250_000/serie_VI/889463598459_s.zip'
data_dir <- 'data/input_data/ag_INEGI_2017'

# Municipio polygons
f.munis <- 'data/input_data/context_Mexico/SNIB_divisionpolitica/muni_2018gw/muni_2018gw.shp'
munis <- st_read(f.munis) %>% 
  mutate(CVE_ENT=as.integer(CVE_ENT),
         CVE_MUN=as.integer(CVE_MUN))

# Load INEGI agricultural polygons ---------------------------------------------
fp_usv <- list.files(path=data_dir, pattern='usv250s6g.shp$', 
                     full.names=T, recursive=T)

if(length(fp_usv)!=1){
  # Downlaod zip and extract usv250s6g.shp
  fn <- usv_url %>% basename()
  download.file(usv_url, dest=fn)
  fp_list <- unzip(fn, exdir=data_dir)
  unlink(fn)
  # Load the useful file as SF dataframe
  fp_usv <- fp_list %>% str_subset('usv250s6g.shp$')  
}

# Get Agriculture polygons -----------------------------------------------------
usv <- st_read(fp_usv, crs=6362) %>% 
  st_make_valid %>% 
  st_transform(4326)
polys_ag <- usv %>% 
  filter(TIP_INFO == 'AGRÍCOLA-PECUARIA-FORESTAL', 
         CLAVE != 'ACUI')

# Nómada polygons from INEGI - all are nómada (shifting cultivation) -----------
f.usv <- file.path(data_dir, 'conjunto_de_datos', 'usv250s6n.shp')
polys_nom <- st_read(f.usv, crs=6362) %>%  
  st_make_valid %>% 
  st_transform(4326)

# Merge Nómada with Agriculture
polys_nom %<>% 
  mutate(TIPAGES='AGRICULTURA NÓMADA', TIP_CUL1='NÓMADA') %>% 
  dplyr::select(!NOM)
polys <- list(polys_ag, polys_nom) %>% 
  mapedit:::combine_list_of_sf() %>% 
  st_make_valid

saveRDS(polys, 'data/data_out/r_data/polys_ag_INEGI.rds')

polys <- readRDS('data/data_out/r_data/polys_ag_INEGI.rds')
polys_fp <- file.path("data/intermediate_data", 'polys_ag_INEGI.gpkg')
polys %>% 
  st_transform(crs) %>% 
  st_write(polys_fp)

# Exploration... look at polygon counts in different categories ----
polys_ag %>% 
  st_drop_geometry() %>%
  group_by(TIP_CUL1) %>% 
  summarise(no_rows = length(TIP_CUL1)) %>% 
  View()
polys_ag %>% 
  st_drop_geometry() %>%
  group_by(TIP_CUL2) %>% 
  summarise(no_rows = length(TIP_CUL1)) %>% 
  View()
polys_ag %>% 
  st_drop_geometry() %>% 
  filter(grepl('.+TEMPORAL', TIPAGES, ignore.case=TRUE)) %>% 
  group_by(CLAVE) %>% 
  summarise(no_rows = length(CLAVE)) %>% 
  View()

# Exploration 2
polys %>% 
  st_set_geometry(NULL) %>% 
  filter(CLAVE == 'PC') %>% 
  distinct
polys %>% mapview
inegi_polys %>% 
  filter(CVE_ENT == 26) %>% 
  mapview

# Intersect with municipios ----------------------------------------------------
# polys_mun <- st_intersection(polys, munis) %>% 
#   mutate(across(where(is.character), 
#                 ~ stringi::stri_trans_general(str=.x, id='Latin-ASCII') %>% 
#                   str_trim %>% 
#                   str_to_title),
#          NOM_ENT = str_replace_all(NOM_ENT, 'Distrito Federal', 'Ciudad De Mexico'),
#          NOM_MUN = str_replace_all(NOM_MUN, 'Parras De La Fuente', 'Parras')) 

# Prepare munis for intersection
mun_for_join <- munis %>%
  mutate(CVE_ENT=as.integer(CVE_ENT),
         CVE_MUN=as.integer(CVE_MUN)) %>% 
  dplyr::select(-AREA, -PERIMETER, -COV_, -COV_ID)

inegi_polys <- st_intersection(polys, mun_for_join)
st_write(inegi_polys, 'data/data_out/polys_ag_INEGI.geojson', delete_dsn=T)

# Load pre-created objects (from prep_SIAP_data.R) -----------------------------
load('data/helpers/initial_vars.RData')
load('data/helpers/functions.RData')
out_dir <- 'data/data_out/polys_ag_INEGI_noFMG'
fmg_dir <- 'data/data_out/polys_fmg'

# Remove frijol, maiz, granos from INEGI polygons ----
# INEGI
inegi_polys <- st_read('data/data_out/polys_ag_INEGI.geojson')
# Municipio polygons 
fp_munis <- 'data/input_data/context_Mexico/SNIB_divisionpolitica/muni_2018gw/muni_2018gw.shp'
munis <- st_read(fp_munis) %>% 
  mutate(CVE_ENT=as.integer(CVE_ENT),
         CVE_MUN=as.integer(CVE_MUN))

# Run per state
estado = 'PUE' 

# Separate by muni
remove_FMG_from_ag_INEGI_largefile(estado, inegi_polys, fmg_dir, municipios=munis)

# ! Join SIAP crop stats to polygons ---------------------------------------------
# This section creates the polygons pcts_by_state/inegi_pcts_[season]_[est].geojson 
# that are used in process_agricultural_polys.R
load("data/data_out/r_data/area_sembrada_by_season_2019.RData")

join_siap_crop_stats_to_polys <- function(estado, 
                                          est_to_cve, 
                                          fmg_dir, 
                                          munis, 
                                          area_cult_season, 
                                          season){
  
  cve_ent <- est_to_cve[[estado]]
  final_fp_out <- file.path('data/data_out/polys_ag_INEGI_wFMG_pcts', 
                            'pcts_by_state', 
                            str_c('inegi_pcts_', season, '19_', estado, '.gpkg'))
  if (file.exists(final_fp_out)) return(final_fp_out)
  
  # Add polys from FMG with pct == 1.0 ----
  sup_fmg <- 
    try({
      sup_fmg <- load_and_preprocess_fmg(estado, fmg_dir, T, munis) %>% 
        mutate(CVE_MUN = as.integer(CVE_MUN),
               CVE_ENT = as.integer(CVE_ENT)) %>% 
        filter(CVE_ENT == as.integer(cve_ent))
      
      # Create FMG columns
      try({
        sup_fmg <- sup_fmg %>%
          rename('area' = 'area_ha')
      })
      
      sup_fmg <- sup_fmg %>%
        mutate(CULTIVO = 
                 stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
                 stringr::str_trim() %>%
                 stringr::str_to_lower(),
               'Frijol' = recode(CULTIVO, 'frijol' = 1.0, .default=NA_real_), 
               'Maíz grano' = recode(CULTIVO, 'maiz grano' = 1.0, .default=NA_real_), 
               'Sorgo grano' = recode(CULTIVO, 'sorgo grano' = 1.0, .default=NA_real_),
               'Trigo grano' = recode(CULTIVO, 'trigo grano' = 1.0, .default=NA_real_),
               count_crops = 1, 
               total_sembrada = area) %>% 
        dplyr::select(-contains(c('CULTIVO', 'NOM_MUN', 'DELEGACIÓN', 'Name', 'NOM_ENT', 'CVEGEO')))
    })
  
  # Ag polys ----
  # Get path for input file
  ag_dir <- 'data/data_out/polys_ag_INEGI_noFMG'
  ag_fp <- file.path(ag_dir, str_c('inegi_noFMG_', estado, '.geojson'))
  
  # Read data
  ag_noFMG <- st_read(ag_fp)
  
  # create column that indicates Temporal o Riego (Idmodalidad) based on CLAVE
  ag_noFMG <- ag_noFMG %>% 
    mutate(CLAVE = str_replace(CLAVE, 'H', 'T'), 
           Idmodalidad = str_extract(CLAVE, '[HNTR]') %>% 
             str_replace_all('H|N', 'T')) %>% 
    filter(str_detect(CLAVE, '^[NRT]')) 
  
  # SIAP stats to percents ------
  # Filter [primavera] to state, remove and columns with all NA values
  df <- area_cult_season %>% 
    filter(CVE_ENT == cve_ent) %>% 
    dplyr::select_if(~sum(!is.na(.)) > 0)
  
  # Remove FMG columns if FMG data exists
  if(class(sup_fmg) != 'try-error'){
    df <- df %>% 
      dplyr::select(!contains(c('maíz', 'frijol', 'sorgo', 'trigo', 'triticale'))) 
  }
  
  # List columns
  vars <- df %>% 
    dplyr::select(-CVE_ENT:-total_sembrada) %>% 
    colnames()
  
  # Get new total area 
  df$total_noFMG <- df %>% 
    dplyr::select(all_of(vars)) %>% 
    rowSums(na.rm=T)
  df <- df %>% relocate(total_noFMG, .after=total_sembrada)
  
  # Get number of different crops planted
  df$count_crops <- df %>% 
    dplyr::select(all_of(vars)) %>% 
    is.na() %>% 
    `!` %>% 
    rowSums
  df <- df %>% relocate(count_crops, .before=total_sembrada)
  
  # Get percent of agricultural land of each crop 
  pct_sembrada_noFMG <- df %>% 
    mutate(across(
      .cols = all_of(vars), 
      .fns = ~ .x / total_noFMG
    )) 
  
  # Join ------
  ag_pct_cult_noFMG <- left_join(ag_noFMG, pct_sembrada_noFMG, 
                                 by=c('CVE_ENT', 'CVE_MUN', 'Idmodalidad'))
  
  # Combine with general ag
  if(class(sup_fmg) != 'try-error'){
    sup_new <- mapedit:::combine_list_of_sf(list(ag_pct_cult_noFMG, sup_fmg))
  } else {
    sup_new <- ag_pct_cult_noFMG
  }
  
  # mapview(sup_new) #+ mapview(sup_fmg)
  
  # SAVE 
  sup_new %>% st_write(final_fp_out, delete_dsn=T)
  
  return(TRUE)
}

estado <- 'ZAC'
season <- 'peren'
join_siap_crop_stats_to_polys(estado, 
                              est_to_cve, 
                              fmg_dir, 
                              munis, 
                              area_cult_peren, 
                              season)

for(estado in names(est_to_cve)){
  join_siap_crop_stats_to_polys(estado, 
                                est_to_cve, 
                                fmg_dir, 
                                munis, 
                                area_cult_peren, 
                                season)
}


# *****************************************************************************
# Remove frijol, maiz, granos from INEGI polygons ----
load('data/helpers/initial_vars.RData')
load('data/helpers/functions.RData')
out_dir <- 'data/data_out/polys_ag_INEGI_noFMG'
fmg_dir <- 'data/data_out/polys_fmg'

# Load polygons
# INEGI
inegi_polys <- st_read('data/data_out/polys_ag_INEGI.geojson')
# Municipio polygons
fp_munis <- 'data/input_data/context_Mexico/SNIB_divisionpolitica/muni_2018gw/muni_2018gw.shp'
munis <- st_read(fp_munis) %>% 
  mutate(CVE_ENT=as.integer(CVE_ENT),
         CVE_MUN=as.integer(CVE_MUN))

# Run per state ----
estado = 'VER'

# Separate by muni
remove_FMG_from_ag_INEGI_largefile(estado, inegi_polys, fmg_dir, municipios=munis)

# Check results
fp_out <- file.path(out_dir, str_c('inegi_noFMG_', estado, '.geojson'))
inegi_noFMG <- st_read(fp_out)
sup_fmg <- load_and_preprocess_fmg(estado, fmg_dir, T, prefix='fmg_siap15_') 
cve_ent <- est_to_cve[[estado]]
cve_mun <- 2
mapview(filter(inegi_polys, CVE_ENT == cve_ent, CVE_MUN == cve_mun)) +
  mapview(filter(inegi_noFMG, CVE_MUN == cve_mun)) +
  mapview(filter(sup_fmg, CVE_MUN == cve_mun))

# *****************************************************************************
# FIX PROBLEMS FOUND IN QC. Rerun after fixing. ----
# Fix HGO ----
# Merge files from individual municipios
estado = 'HGO'
state_dir <- file.path(out_dir, estado)
fps <- list.files(state_dir, full.names=T)
ag_noFMG <- fps %>% 
  lapply(st_read) %>% 
  mapedit:::combine_list_of_sf()
final_fp_out <- file.path(out_dir, str_c('inegi_noFMG_', estado, '.geojson'))
ag_noFMG %>% st_write(final_fp_out)

# Fix MICH fmg polys ----
estado <- 'MICH'
cve_ent <- est_to_cve[[estado]]
cve_mun <- 71

fmg_1 <- st_read(file.path('data/input_data/SIAP/frijol_maiz_granos', 
                           str_c('ESA_PV2015_', estado, '.geojson')))  %>% 
  rename('NOM_ENT' = 'DELEGACIÓN') %>% 
  dplyr::select(CULTIVO, CVE_ENT, CVE_MUN, NOM_ENT, NOM_MUN, area_ha) %>% 
  mutate(CVEGEO = str_c(str_pad(CVE_ENT,2), str_pad(CVE_MUN, 3, pad='0')))
fmg_2 <- st_read(file.path('data/data_out/polys_fmg', 
                           str_c('fmg_siap15_', estado, '.geojson'))) 

# Filter and Simplify to remove jagged edges and decrease size
fmg1 <- fmg_1 %>% filter(CVE_MUN == cve_mun, CULTIVO == 'Maiz Grano') %>% 
  st_simplify(preserveTopology = T, dTolerance = .0001)
fmg2 <- fmg_2 %>% filter(!(CVE_MUN == cve_mun & CULTIVO == 'Maiz Grano'))

# Combine
fmg_3 <- mapedit:::combine_list_of_sf(list(fmg2, fmg1))

# Check
mapview(filter(fmg_2, CVE_MUN == cve_mun)) +
  mapview(fmg1) +
  mapview(fmg_3)

st_write(fmg_3, file.path('data/data_out/polys_fmg', 
                          str_c('fmg_siap15_', estado, '_redo.geojson')))

# Fix CHIS fmg polys ----
estado <- 'CHIS'
cve_ent <- est_to_cve[[estado]]
cve_mun <- c(6, 10, 34)

fmg_1 <- st_read(file.path('data/input_data/SIAP/frijol_maiz_granos', 
                           str_c('ESA_PV2015_', estado, '.geojson')))  %>% 
  rename('NOM_ENT' = 'DELEGACIÓN') %>% 
  dplyr::select(CULTIVO, CVE_ENT, CVE_MUN, NOM_ENT, NOM_MUN, area_ha) %>% 
  mutate(CVEGEO = str_c(str_pad(CVE_ENT,2), str_pad(CVE_MUN, 3, pad='0')), 
         CVE_ENT = as.integer(CVE_ENT), 
         CVE_MUN = as.integer(CVE_MUN),
         CULTIVO = 
           stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
           str_trim %>%
           stringr::str_to_lower)
fmg_2 <- st_read(file.path('data/data_out/polys_fmg', 
                           str_c('fmg_siap15_', estado, '.geojson'))) %>% 
  mutate(CULTIVO = 
           stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
           str_trim %>%
           stringr::str_to_lower, 
         CVE_ENT = as.integer(CVE_ENT), 
         CVE_MUN = as.integer(CVE_MUN))

# Filter
fmg2 <- fmg_2 %>% filter(!(CVE_MUN %in% cve_mun & CULTIVO == 'maiz grano'))
fmg1 <- fmg_1 %>% filter(CVE_MUN %in% cve_mun & CULTIVO == 'maiz grano')

box <- c(xmin=-92.21, ymin=15.48, xmax=-92.06, ymax=15.69)
mapview(st_crop(munis, box)) +
  mapview(st_crop(fmg1, box)) +
  mapview(st_crop(fmg2, box)) 

# Simplify to remove jagged edges and decrease size
fmg1d <- fmg1 %>% 
  st_simplify(preserveTopology = T, dTolerance = .0001)
object.size(fmg1) %>% print(units='MB')
object.size(fmg1d) %>% print(units='MB')

# Get intersection with municipios
fmg_3 <- mapedit:::combine_list_of_sf(list(fmg2, fmg1d))

mapview(st_crop(fmg_2, box)) +
  mapview(st_crop(fmg1, box)) +
  mapview(st_crop(fmg_3, box)) 

st_write(fmg_3, file.path('data/data_out/polys_fmg', 
                          str_c('fmg_siap15_', estado, '_redo.geojson')))

# Fix ZAC fmg polys ----
estado <- 'ZAC'
cve_ent <- est_to_cve[[estado]]
cve_mun <- c(42,51)

fmg_1 <- st_read(file.path('data/input_data/SIAP/frijol_maiz_granos', 
                           str_c('ESA_PV2015_', estado, '.geojson')))  %>% 
  rename('NOM_ENT' = 'DELEGACIÓN') %>% 
  dplyr::select(CULTIVO, CVE_ENT, CVE_MUN, NOM_ENT, NOM_MUN, area_ha) %>% 
  mutate(CVEGEO = str_c(str_pad(CVE_ENT,2), str_pad(CVE_MUN, 3, pad='0')), 
         CVE_ENT = as.integer(CVE_ENT), 
         CVE_MUN = as.integer(CVE_MUN),
         CULTIVO = 
           stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
           str_trim %>%
           stringr::str_to_lower)
fmg_2 <- st_read(file.path('data/data_out/polys_fmg', 
                           str_c('fmg_siap15_', estado, '.geojson'))) %>% 
  mutate(CULTIVO = 
           stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
           str_trim %>%
           stringr::str_to_lower, 
         CVE_ENT = as.integer(CVE_ENT), 
         CVE_MUN = as.integer(CVE_MUN))

# Filter
fmg2 <- fmg_2 %>% filter(!(CVE_MUN %in% cve_mun & CULTIVO == 'frijol')) %>% st_make_valid
fmg1 <- fmg_1 %>% filter(CVE_MUN %in% cve_mun & CULTIVO == 'frijol') %>% st_make_valid

mapview(filter(munis, CVE_ENT == cve_ent, CVE_MUN %in% cve_mun))
box <- c(xmin=-103.69, ymin=23.2, xmax=-102.35, ymax=23.6)
mapview(st_crop(munis, box)) +
  mapview(st_crop(fmg1, box)) +
  mapview(st_crop(fmg2, box)) 

# Simplify to remove jagged edges and decrease size
fmg1d <- fmg1 %>% 
  st_simplify(preserveTopology = T, dTolerance = .0001)
object.size(fmg1) %>% print(units='MB')
object.size(fmg1d) %>% print(units='MB')

# Get intersection with municipios
fmg_3 <- mapedit:::combine_list_of_sf(list(fmg2, fmg1d))
mapview(st_crop(fmg_2, box)) +
  mapview(st_crop(fmg1, box)) +
  mapview(st_crop(fmg_3, box)) 

st_write(fmg_3, file.path('data/data_out/polys_fmg', 
                          str_c('fmg_siap15_', estado, '_redo.geojson')))

# Fix VER fmg polys ----
estado <- 'VER'
cve_ent <- est_to_cve[[estado]]
cve_mun <- c(100, 149)

fmg_1 <- st_read(file.path('data/input_data/SIAP/frijol_maiz_granos', 
                           str_c('ESA_PV2015_', estado, '.geojson')))  %>% 
  rename('NOM_ENT' = 'DELEGACIÓN') %>% 
  dplyr::select(CULTIVO, CVE_ENT, CVE_MUN, NOM_ENT, NOM_MUN, area_ha) %>% 
  mutate(CVEGEO = str_c(str_pad(CVE_ENT,2), str_pad(CVE_MUN, 3, pad='0')), 
         CVE_ENT = as.integer(CVE_ENT), 
         CVE_MUN = as.integer(CVE_MUN),
         CULTIVO = 
           stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
           str_trim %>%
           stringr::str_to_lower)
fmg_2 <- st_read(file.path('data/data_out/polys_fmg', 
                           str_c('fmg_siap15_', estado, '.geojson'))) %>% 
  mutate(CULTIVO = 
           stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
           str_trim %>%
           stringr::str_to_lower, 
         CVE_ENT = as.integer(CVE_ENT), 
         CVE_MUN = as.integer(CVE_MUN))

# Filter
fmg2 <- fmg_2 %>% filter(!(CVE_MUN %in% cve_mun & CULTIVO == 'maiz grano')) %>% st_make_valid
fmg1 <- fmg_1 %>% filter(CVE_MUN %in% cve_mun & CULTIVO == 'maiz grano') %>% st_make_valid

mapview(filter(munis, CVE_ENT == cve_ent, CVE_MUN %in% cve_mun))
mapview(filter(fmg1, CVE_MUN %in% c(110, 100, 149))) +
  mapview(filter(fmg2, CVE_MUN %in% c(110, 100, 149)))

# Simplify to remove jagged edges and decrease size
fmg1d <- fmg1 %>% 
  st_simplify(preserveTopology = T, dTolerance = .0001)
object.size(fmg1) %>% print(units='MB')
object.size(fmg1d) %>% print(units='MB')

# Get intersection with municipios
fmg_3 <- mapedit:::combine_list_of_sf(list(fmg2, fmg1d))

mapview(filter(fmg_3, CVE_MUN %in% c(100, 149)))

st_write(fmg_3, file.path('data/data_out/polys_fmg', 
                          str_c('fmg_siap15_', estado, '_redo.geojson')))

# Fix MICH ag polys ----
estado <- 'MICH'
cve_ent <- est_to_cve[[estado]]
cve_mun <- 7
clave <- 'TA'

# Load ag and filter out the bad muni
fp_out <- file.path(out_dir, str_c('inegi_noFMG_', estado, '.geojson'))
ag_out <- st_read(fp_out) %>% 
  filter(CVE_MUN != cve_mun)

# Fix the file for the muni
fp_out <- file.path(out_dir, estado, str_c(estado, '_7.geojson'))
ag_2 <- st_read(fp_out) %>% 
  st_collection_extract('POLYGON') %>% 
  st_make_valid
ag_2 %>% st_write(fp_out)

# Combine the new muni with the rest of the ag.
ag_out <- mapedit:::combine_list_of_sf(list(ag_out, ag_2))
# Save
fp_out <- file.path(out_dir, str_c('inegi_noFMG_', estado, '.geojson'))
ag_out %>% st_write(fp_out)


















# QC -----
# Check numbers of polygons in each class (TIPAGES)
(polys_mun %>% 
  st_drop_geometry() %>%
  group_by(CLAVE) %>% 
  summarise(no_rows = length(CLAVE)) )

colnames(polys_mun)
mapview(polys_mun %>% filter(CVE_ENT==1)) 

# Cultivo points ----
# Cultivo points from INEGI - all are Agricola-Pecuaria-Forestal
f.usv <- file.path(data_dir, 'conjunto_de_datos', 'usv250s6c.shp')
pts_cult <- st_read(f.usv, crs=6362)
colnames(pts_cult)
# Decode cultivos for most important pollinator crops
key <- read_csv(file.path(data_dir, 'metadatos', 'INEGI_codes.csv'))
key <- key %>% pmap(~set_names(..2, ..1)) %>% unlist() %>% 
  c(.default = NA_character_)
pts_cult <- pts_cult %>% mutate(Culti1=recode(Culti1, !!!key),
                     Culti2=recode(Culti2, !!!key),
                     Culti3=recode(Culti3, !!!key))
# Look at numbers of points per important crop
cults <- pts_cult %>% dplyr::select(starts_with('Culti')) 
st_geometry(cults) <- NULL
culti_ct <- cults %>% 
  gather('name', 'cultivo') %>% 
  group_by(cultivo) %>% 
  summarise(no_rows = length(cultivo)) %>%
  arrange(desc(no_rows)) %>% 
  View()

# Forestales points ----
# Forestales points from INEGI - all are Agricola-Pecuaria-Forestal
f.usv <- file.path(data_dir, 'conjunto_de_datos', 'usv250s6f.shp')
pts_for <- st_read(f.usv, crs=6362)
colnames(pts_for)

# Look at numbers of points per important crop
grps <- pts_for %>%
  group_by(Clave) %>% 
  summarise(no_rows = length(Clave)) %>% 
  View()

