
# Load libraries ----
library(sf)
library(tidyverse)
library(magrittr)
library(rvest)
library(tools)
library(mapview)
library(units)

# Functions ----
html_to_df <- function(x){
  # get values from description field
  x %>% 
    minimal_html() %>% 
    html_node('table') %>% 
    html_table(fill=T) %>% 
    select(X1, X2) %>% 
    slice(-1:-2) %>% 
    pivot_wider(names_from=X1, values_from=X2)
}

load_kmz_as_sf <- function(kmz_url, data_dir, out_fp, col_types=NULL){
  # Download KMZ and convert to sf dataframe
  kmz_name <- kmz_url %>% basename()
  download.file(kmz_url, dest=kmz_name)
  sf <- kmz_name %>% 
    unzip(list=F, exdir=data_dir) %>% 
    st_read()
  unlink(kmz_name)
  
  # Convert KML description field to DF
  df <- sf$description %>%
    map_dfr(html_to_df) 
  if(length(col_types)>0){
    df %<>% 
      type_convert(col_types=col_types, trim_ws=T)
  }
  
  # Replace KML variables with DF
  sf %<>%
    select(Name) %>%
    cbind(df) %>%
    st_zm(drop=T, what='ZM') %>% 
    st_make_valid()

  return(sf)
}

intersect_with_munis <- function(sup_ag, municipios){
  # Intersect with municipios and simplify
  ag_munis <- sup_ag  %>%
    st_simplify(dTolerance = 0.0001) %>% 
    st_intersection(municipios)  %>% 
    group_by(CVE_ENT, CVE_MUN, MOD_AGR) %>% 
    summarize(geometry = st_union(geometry)) %>% 
    as_tibble() %>% 
    st_as_sf()
  
  # get area for new polygons in ha
  ag_munis$area <- ag_munis %>% 
    st_transform(crs=6362) %>% 
    st_area() %>% 
    set_units('ha') %>% 
    set_units(NULL)
  
  return(ag_munis)
}

load_and_preprocess_ag <- function(region, data_dir, municipios, return_data=T){
  fp_ag <- file.path(data_dir, str_c('FASII_', region, '.geojson'))
  
  if(!file.exists(fp_ag)){
    url_ag <- str_c('http://infosiap.siap.gob.mx/gobmx/datosAbiertos/', 
                    'frontera-agricola/FASII_', region, '.kmz')
    sup_ag <- load_kmz_as_sf(url_ag, data_dir, 'iicicicdcc')
    
    # Intersect with municipios
    sup_ag <- intersect_with_munis(sup_ag, municipios)
    
    # Save to file
    sup_ag %>% st_write(fp_ag, delete_dsn=T)
  } else {
    if(return_data) return(st_read(fp_ag)) else return(fp_ag)
  } 
  if(return_data) return(sup_ag) else return(fp_ag)
}

preprocess_fmg <- function(sup_frij_gran, lookup){
  # Simplify to remove jagged edges and decrease size
  sup_frij_gran <- st_simplify(sup_frij_gran, preserveTopology = T, dTolerance = .0001)
  
  # Add area column
  sup_frij_gran$area_ha <- sup_frij_gran %>% 
    st_transform(crs=6362) %>% 
    st_area() %>% 
    set_units('ha') %>% 
    set_units(NULL)
  
  # Recode Municipios to CVE code
  if(missing(lookup)) {
    lookup <- readRDS('data/helpers/lookup_municipio_codes.rds')
  }
  
  sup_frij_gran <- sup_frij_gran %>% 
    mutate(CVE_ENT=as.integer(CVE_ENT),
           CVE_MUN=as.integer(CVE_MUN), 
           NOM_MUN = str_to_title(str_trim(NOM_MUN)),
           DELEGACIÓN = str_replace_all(DELEGACIÓN, 'Distrito Federal', 'Ciudad De México')) %>% 
    left_join(lookup, by=c('DELEGACIÓN'='NOM_ENT', 'NOM_MUN')) %>% 
    mutate(CVE_ENT=as.integer(CVE_ENT),
           CVE_MUN=as.integer(CVE_MUN))
  
  if(!all(!is.na(sup_frij_gran$CVE_MUN))){
    print('WARNING: not all municipios matched')
  }
  
  return(sup_frij_gran)
}

load_and_preprocess_fmg <- function(estado_code, data_dir, return_df, lookup, fp_fmg){
  # Get filename
  if(missing(fp_fmg)){
    fp_fmg <- file.path(data_dir, str_c('ESA_PV2015_', estado_code, '.geojson'))
  }
  
  if(file.exists(fp_fmg)){
    # Read file
    sup_frij_gran <- st_read(fp_fmg) %>% 
      mutate(CVE_ENT=as.integer(CVE_ENT),
             CVE_MUN=as.integer(CVE_MUN))
  } else {
    # Download and convert from KMZ
    url_fmg <- str_c('http://infosiap.siap.gob.mx/gobmx/datosAbiertos/', 
                     'estimacion-superficie-agricola/ESA_PV2015_', estado_code, '.kmz')
    sup_frij_gran <- load_kmz_as_sf(url_fmg, data_dir)
    
    # Process
    sup_frij_gran <- preprocess_fmg(sup_frij_gran, lookup)
    
    # Save
    st_write(sup_frij_gran, fp_fmg, delete_dsn=T)
  }
  if(return_df) return(sup_frij_gran) else return(TRUE)
}

get_area_planted <- function(data, state_id_fld, muni_id_fld, crop_name_fld, 
                             riego_id_fld, planted_fld, planted_area_fld){
  # Summarize crops by municipio, percent of total planted area

  # get total sembrada for each crop by municipio
  cultivo_totals <- data %>% 
    group_by({{state_id_fld}}, {{ muni_id_fld }}, {{ crop_name_fld }}, {{riego_id_fld}}) %>% 
    summarize({{planted_fld}} := sum({{planted_fld}}, na.rm=T)) %>%
    spread({{ crop_name_fld }}, {{planted_fld}})
  
  # Quote variable name arguments as quosures
  state_id_fld <- rlang::enquo(state_id_fld)
  muni_id_fld <- rlang::enquo(muni_id_fld)
  riego_id_fld <- rlang::enquo(riego_id_fld)
  
  # get total sembrada area by municipio
  data %>% 
    group_by({{state_id_fld}}, {{muni_id_fld}}, {{riego_id_fld}}) %>% 
    summarize({{planted_area_fld}} := sum({{planted_fld}})) %>% 
    full_join(cultivo_totals, 
              by=c(quo_name(state_id_fld), quo_name(muni_id_fld), quo_name(riego_id_fld))) %>% 
    as_tibble()
}

clip_out_polys_from_ag <- function(ag_by_munis, spec_polys){
  # Get CVE for state of FMG data
  cve_ent <- spec_polys %>% 
    st_set_geometry(NULL) %>% 
    select(CVE_ENT) %>% 
    distinct %>% 
    deframe
  
  # Remove FMG from agricultural areas
  ag_noFMG <- st_difference(
    filter(ag_by_munis, CVE_ENT==cve_ent), 
    st_union(spec_polys)
  )
  
  # Tidy the polygons - drop crumbs and fill pinholes
  ag_noFMG <- ag_noFMG %>%
    st_transform(crs=6362) %>% 
    smoothr::drop_crumbs(threshold=1000) %>%
    smoothr::fill_holes(threshold=100)  %>%
    st_transform(crs=4326) %>% 
    st_make_valid()
  
  # Get area for new polygons
  ag_noFMG$area <- ag_noFMG %>% 
    st_transform(crs=6362) %>% 
    st_area() %>% 
    set_units('ha') %>% 
    set_units(NULL)
  
  return(ag_noFMG)
}

# Filenames ---
# SIAP Datos Abiertos page: http://infosiap.siap.gob.mx/gobmx/datosAbiertos.php

# Load SIAP Cultivos - area sembrada -------------------------------------------
url_cult_stats <- str_c('http://infosiap.siap.gob.mx/gobmx/datosAbiertos/',
                     'ProduccionAgricola/Cierre_agricola_mun_2019.csv')
cultivos <- read_csv(url_cult_stats, col_types = 'iicicicicicicicicnnnnnnn', 
                     locale=locale(encoding='latin1')) %>% 
  rename(CVE_ENT=Idestado, CVE_MUN=Idmunicipio)

# filter to year-round
area_cult_peren <- cultivos %>% 
  filter(Idciclo==3) %>% 
  get_area_planted(CVE_ENT, CVE_MUN, Nomcultivo, Idmodalidad, Sembrada, total_sembrada) %>% 
  mutate(Idmodalidad=recode(Idmodalidad, '1' = "R", '2' = "T"))

# filter to spring 
area_cult_prim <- cultivos %>% 
  filter(Idciclo==2) %>% 
  get_area_planted(CVE_ENT, CVE_MUN, Nomcultivo, Idmodalidad, Sembrada, total_sembrada) %>% 
  mutate(Idmodalidad=recode(Idmodalidad, '1' = "R", '2' = "T"))

# filter to autumn
area_cult_otono <- cultivos %>% 
  filter(Idciclo==1) %>% 
  get_area_planted(CVE_ENT, CVE_MUN, Nomcultivo, Idmodalidad, Sembrada, total_sembrada) %>% 
  mutate(Idmodalidad=recode(Idmodalidad, '1' = "R", '2' = "T"))

# Save
save(area_cult_peren, area_cult_prim, area_cult_otono, 
     file = "data/data_out/r_data/area_sembrada_by_season.RData")

# Municipio polygons -----------------------------------------------------------
fp_munis <- 'data/input_data/context_Mexico/SNIB_divisionpolitica/muni_2018gw/muni_2018gw.shp'
munis <- st_read(fp_munis) %>% 
  mutate(CVE_ENT=as.integer(CVE_ENT),
         CVE_MUN=as.integer(CVE_MUN))

# Get lookup for Estado and Municipio to CVE code
lookup <- munis %>% 
  st_set_geometry(NULL) %>% 
  select(NOM_ENT, NOM_MUN, CVE_ENT, CVE_MUN) %>% 
  mutate(across(where(is.character), ~ str_to_title(.x)))
lookup %>% saveRDS('data/helpers/lookup_municipio_codes.rds')

# Pre-process all SIAP agricultural coverage KMZs (2010) -----------------------
ag_dir <- 'data/input_data/SIAP/FASII'
regions <- c('CW_Col_Jal_Ags_Gto_Mich', 'C_Qro_Hgo_Mex_Tlax_Pue_Mor_DF', 
             'CE_Tab_Ver', 'N_Chih_Coah_Dgo__SLP_Zac', 'NE_Tams_NL', 
             'NW_BC_BCS_Son_Sin_Nay', 'S_Gro_Oax_Chis', 'SE_Camp_QRoo_Yuc')

# Download and convert from KMZ
regions %>% sapply(load_and_preprocess_ag, ag_dir, munis, F)

# Load frijol-maiz-granos polygons ---------------------------------------------
fmg_dir <- 'data/input_data/SIAP/frijol_maiz_granos'
est_codes <- c('AGS', 'BC', 'BCS', 'CAM', 'CHH', 'CHIS', 'COAH', 'COL', 'DF', 
               'DGO', 'GRO', 'HGO', 'JAL', 'MICH', 'MOR', 'NAY', 'NVL', 'OAX', 
               'QRO', 'QROO', 'RL', 'SLP', 'SON', 'TAB', 'TAM', 'TLX', 'VER',
               'YUC', 'ZAC')

# Download and convert from KMZ
est_codes %>% sapply(load_and_preprocess_fmg, fmg_dir, FALSE)

# Fix Region Lagunera to match the others 
estado = 'RL'
sup_fmg <- load_and_preprocess_fmg(estado, fmg_dir, T)
sup_fmg <- sup_fmg %>% 
  select(Name, CULTIVO, DELEGACIÓN) %>% 
  st_join(select(munis, NOM_ENT, NOM_MUN, CVE_ENT, CVE_MUN), left=T) %>% 
  rename('DEL_orig'='DELEGACIÓN',
         'DELEGACIÓN' = 'NOM_ENT') 

# Get area for new polygons
sup_fmg$area_ha <- sup_fmg %>% 
  st_transform(crs=6362) %>% 
  st_area() %>% 
  set_units('ha') %>% 
  set_units(NULL)

fp_fmg <- file.path(fmg_dir, str_c('ESA_PV2015_', estado, '.geojson'))
st_write(sup_fmg, fp_fmg, delete_dsn=T)

# Merge parts of RL into respective states
sup_fmgR <- load_and_preprocess_fmg('RL', fmg_dir, T)

sup_fmgD <- load_and_preprocess_fmg('DGO', fmg_dir, T) 
mapview(sup_fmgD)
dur <- sup_fmgR %>% filter(DELEGACIÓN=='Durango')
sup_fmg <- list(sup_fmgD, dur) %>% 
  mapedit:::combine_list_of_sf()
fp_fmg <- file.path(fmg_dir, str_c('ESA_PV2015_', 'DGO', '.geojson'))
st_write(sup_fmg, fp_fmg, delete_dsn=T)

sup_fmgZ <- load_and_preprocess_fmg('ZAC', fmg_dir, T)
polys <- sup_fmgR %>% filter(DELEGACIÓN=='Zacatecas')
sup_fmg <- list(sup_fmgZ, polys) %>% 
  mapedit:::combine_list_of_sf()
fp_fmg <- file.path(fmg_dir, str_c('ESA_PV2015_', 'ZAC', '.geojson'))
st_write(sup_fmg, fp_fmg, delete_dsn=T)

sup_fmgC <- load_and_preprocess_fmg('COAH', fmg_dir, T) 
polys <- sup_fmgR %>% filter(DELEGACIÓN=='Zacatecas')
sup_fmg <- list(sup_fmgZ, polys) %>% 
  mapedit:::combine_list_of_sf()
fp_fmg <- file.path(fmg_dir, str_c('ESA_PV2015_', 'COAH', '.geojson'))
st_write(sup_fmg, fp_fmg, delete_dsn=T)

# ------------------------------------------------------------------------------
# Get frijol and save
# sup_frijol <- sup_frij_gran %>% 
#   filter(CULTIVO=='Frijol') %>% 
#   mutate(Frijol = recode(CULTIVO, 'Frijol'=1.0))
# fp <- file.path(data_dir, 'frijol', str_c('ESA_PV2015_frijol_', estado_code, '.geojson'))
# st_write(sup_frijol, fp, delete_dsn=T)

# Save
# save(sup_frij_gran, file='data/data_out/r_data/sup_frij_gran.RData')

# Extract frijol, maíz, and granos from general agriculture --------------------
states_by_region <- list(
  'CW_Col_Jal_Ags_Gto_Mich' = c('AGS', 'JAL', 'MICH','COL'),
  'C_Qro_Hgo_Mex_Tlax_Pue_Mor_DF' = c('QRO', 'HGO', 'TLX', 'MOR', 'DF'), 
  'CE_Tab_Ver' = c('TAB', 'VER'), 
  'N_Chih_Coah_Dgo__SLP_Zac' = c('CHH', 'COAH', 'DGO', 'SLP', 'ZAC', 'RL'), 
  'NE_Tams_NL' = c('TAM', 'NVL'), 
  'NW_BC_BCS_Son_Sin_Nay' = c('BC', 'BCS', 'SON', 'NAY'), 
  'S_Gro_Oax_Chis' = c('GRO', 'OAX', 'CHIS'), 
  'SE_Camp_QRoo_Yuc' = c('CAM', 'QROO', 'YUC')
) %>% enframe('region', 'state') %>% unnest_longer(state)

estado <- 'DGO'
region <- states_by_region %>% 
  filter(state==estado) %>% 
  select(region) %>% 
  deframe

# Load specific zone from ag data
ag <- load_and_preprocess_ag(region, ag_dir, munis)

# Load specific state from FMG data
sup_fmg <- load_and_preprocess_fmg(estado, fmg_dir, T)
mapview(ag, zcol='MOD_AGR')+
  mapview(sup_fmg, zcol='CULTIVO')

# Remove
siap_noFMG <- clip_out_polys_from_ag(ag, sup_fmg)
mapview(siap_noFMG)

# Save
save(siap_noFMG, file=str_c('data/data_out/r_data/polys_ag_noFMG_', estado, '.RData'))

# LOAD DATA --------------------------------------------------------------------
load("data/data_out/r_data/area_sembrada_by_season.RData")
load('data/data_out/r_data/ag_munis.RData')
load('data/data_out/r_data/sup_frij_gran.RData')
load('data/data_out/r_data/polys_ag_noFMG_AGS.RData')

# Select crops with high pollinator importance
important_crops <- c("aguacate", 'jitomate', 'café','mango',
                     'sandía','manzana','melón', 'calabaza', 
                     'pepino', 'brócoli', 'cacao', 'zarzamora', 
                     'fresa', 'zanahoria', 'soya', 'frijol')

# Look (at Aguascalientes)
mapview(ag_munis %>% filter(CVE_ENT==1)) 

# Remove FMG columns and calculate columns ------------------------------------
# Use primavera 
area_sembrada <- area_cult_prim
# Filter to Aguascalientes
area_sembrada <- area_sembrada %>% filter(CVE_ENT==1)

# Remove FMG columns
area_sembrada_noFMG <- area_sembrada %>% 
  select(!contains(c('maíz grano', 'frijol', 'sorgo grano', 'trigo grano')))

# List columns
vars <- area_sembrada_noFMG %>% 
  select(-CVE_ENT:-total_sembrada) %>% 
  colnames()

# Get new total area 
area_sembrada_noFMG$total_noFMG <- area_sembrada_noFMG %>% 
  select(all_of(vars)) %>% 
  rowSums(na.rm=T)
area_sembrada_noFMG %<>% relocate(total_noFMG, .after=total_sembrada)

# Get number of different crops planted
area_sembrada_noFMG$count_crops <- area_sembrada_noFMG %>% 
  select(all_of(vars)) %>% 
  is.na %>% 
  `!` %>% 
  rowSums
area_sembrada_noFMG %<>% relocate(count_crops, .before=total_sembrada)

# Get percent of agricultural land of each crop 
pct_sembrada_noFMG <- area_sembrada_noFMG %>% 
  mutate(across(
    .cols = all_of(vars), 
    .fns = ~ .x / total_noFMG
  ))

# Join -------------------------------------------------------------------------
ag_pct_cult_noFMG <- left_join(ag_noFMG, pct_sembrada_noFMG, 
                               by=c('CVE_ENT', 'CVE_MUN', 'MOD_AGR'='Idmodalidad'))

# ***** SAVE ***** ----
ag_pct_cult_noFMG %>% 
  st_write(dsn='data/data_out/polys_ag_noFMG_AGS.geojson', delete_dsn=T)

# ***** SAVE with only top 15 important crops ***** ----
ag_pct_cult_noFMG_important <- select(ag_pct_cult_noFMG, 
                                      contains(all_of(important_crops)))
ag_pct_cult_noFMG_important %>% 
  st_write(dsn='data/data_out/polys_ag_noFMG_top15_AGS.geojson', delete_dsn=T)



# EXTRA: Compare areas for frijol / maiz between polygons and statistics -------
load('data/data_out/sup_frij_gran.RData')
load("data/data_out/r_data/area_sembrada_by_season.RData")

# Use primavera 
area_sembrada <- area_cult_prim
# Filter to Aguascalientes
area_sembrada <- area_sembrada %>% filter(CVE_ENT==1)

# Get totals for municipios (combine T and R modalidad)
area_sembrada_FMG <- area_sembrada %>% 
  group_by(CVE_ENT, CVE_MUN) %>% 
  summarize(across(
    contains(c('total_sembrada', 'maíz grano', 'frijol', 'sorgo grano', 'trigo grano')), 
    ~sum(.x, na.rm=T)
    )) %>% 
  pivot_longer(contains(c('maíz grano', 'frijol', 'sorgo grano', 'trigo grano')),
               names_to="CULTIVO",
               values_to='area_sembrada')

# Join
ag_FMG <- left_join(area_sembrada_FMG, sfg, by=c('CVE_MUN', 'CULTIVO'))

# EXTRA: Drop columns with only NA values --------------------------------------
ag_cults1 <- ag_cultivos %>% 
  select_if(~sum(!is.na(.)) > 0) 
ag_cults1 %>% colnames()

crop <- "Ajo"
mapview(ag_cultivos, zcol=crop, layer.name='Area') +
  mapview(pct_sembrada, zcol=crop, layer.name='Pct crop area') 

# EXTRA: Drop municipios without important crops -------------------------------
ag_important_cults <- ag_cultivos %>% 
  select(CVE_ENT:total_sembrada, contains(important_crops)) %>% 
  select_if(~sum(!is.na(.)) > 0)  %>% 
  filter_at(vars(-CVE_ENT:-total_sembrada, -geometry), any_vars(!is.na(.))) 

# Get crop column names
vars <- ag_important_cults %>% 
  select(-CVE_ENT:-total_sembrada, -geometry) %>% 
  colnames()

# Replace NAs
ag_important_cults2 <- ag_important_cults %>% 
  mutate(
    across(all_of(vars), ~replace_na(.x, 0))
    )

# Look ----
ag_important_cults2 %>% 
  tm_shape()+
  tm_polygons()+
  tm_facets(sync = TRUE, ncol = 3)

# By state
munis_cult_mich <- munis_cult %>% filter(NOM_ENT=='Michoacán de Ocampo')
# object.size(munis_cult_mich) %>% print(units="MB")
munis_cult_mich %>% 
  select(one_of('Tomate rojo (jitomate)', 'Pepino', 'Zarzamora', 'Melón')) %>% 
  tm_shape()+
  tm_polygons(c('Tomate rojo (jitomate)', 'Pepino', 'Zarzamora', 'Melón'))+
  tm_facets(sync = TRUE, ncol = 2)







# Agrícultural protegida ----
# Percent coverage per municipio
# agripro <- read.csv(f.stat.agripro, header=TRUE, fileEncoding="latin1")
f.stat.agripro <- 'http://infosiap.siap.gob.mx/gobmx/datosAbiertos/Sup_Agricul_Protegida/Agricultura_Protegida_2015.csv'
agripro <- read_csv(f.stat.agripro, col_types = 'iciciciciccccinn', locale=locale(encoding='latin1'))
pct_agripro <- get_pct_planted(agripro, CVE_EDO, CVE_MUN, CULTIVOS, 
                                       SUP_HAS, sembrada_area, sembrada_pct)

# agg_agripro <- agripro %>% 
#   group_by(CVE_EDO, CVE_MUN, CULTIVOS) %>% 
#   summarize(area=sum(SUP_HAS)) %>% 
#   group_by(CVE_MUN) %>% 
#   mutate(total_area=sum(area), 
#          porcentaje=area/total_area)
# pct_agripro <- agg_agripro %>% 
#   select(CVE_EDO, CVE_MUN, CULTIVOS, porcentaje) %>% 
#   spread(CULTIVOS, porcentaje)

# Join AgriPro and regular Cultivos -----
# There are MANY items listed for crops, especially for AgriPro, 
# where there is often a list of multiple crops
pct_agr <- pct_cult %>% 
  full_join(pct_agripro, 
            by=c('Idestado'='CVE_EDO', 'Idmunicipio'='CVE_MUN'),
            suffix=c('',', protegida'))
colnames(pct_agr)


