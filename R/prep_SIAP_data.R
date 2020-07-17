
# Load libraries ----
library(sf)
library(tidyverse)
library(magrittr)
library(rvest)
library(tools)
library(mapview)
library(units)

# Filenames ---
# f.stat.cult <- 'data/input_data/SIAP/Cierre_agricola_mun_2018.csv'
# f.stat.agripro <- 'data/input_data/SIAP/AgriPro_Agricultura_Protegida_2015_porMunicipio.csv'
# f.sup.agri <- 'data/input_data/SIAP/FASII_CW_Col_Jal_Ags_Gto_Mich.kmz'

f.munis <- 'data/input_data/context_Mexico/SNIB_divisionpolitica/muni_2018gw/muni_2018gw.shp'
# SIAP Datos Abiertos page: http://infosiap.siap.gob.mx/gobmx/datosAbiertos.php
f.stat.cult <- 'http://infosiap.siap.gob.mx/gobmx/datosAbiertos/ProduccionAgricola/Cierre_agricola_mun_2019.csv'
f.stat.agripro <- 'http://infosiap.siap.gob.mx/gobmx/datosAbiertos/Sup_Agricul_Protegida/Agricultura_Protegida_2015.csv'
f.sup.agri <- 'http://infosiap.siap.gob.mx/gobmx/datosAbiertos/frontera-agricola/FASII_CW_Col_Jal_Ags_Gto_Mich.kmz'
region <- 'CW'

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

load_kmz_as_sf <- function(kmz_url, data_dir, col_types=NULL){
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
  
  # Save
  out_fp <- file.path(data_dir, str_c(file_path_sans_ext(kmz_name), '.geojson'))
  sf %>% 
    st_write(out_fp, delete_dsn=T)
  
  return(sf)
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

# Load SIAP Cultivos - area sembrada -------------------------------------------
cultivos <- read_csv(f.stat.cult, col_types = 'iicicicicicicicicnnnnnnn', 
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
     file = "data/data_out/area_sembrada_by_season.RData")

# Load SIAP agricultural coverage (2010) from KMZ ------------------------------
# Read
kmz_name <- basename(f.sup.agri)
out_fp <- file.path('data/input_data/SIAP/FASII', 
                    str_c(file_path_sans_ext(kmz_name), '.geojson'))
sup_ag <- st_read(out_fp)
if(!exists('sup_ag')){
  sup_ag <- load_kmz_as_sf(f.sup.agri, 'data/input_data/SIAP/FASII', 'iicicicdcc')
}

# Combine with Municipios ------------------------------------------------------
# Municipio polygons
munis <- st_read(f.munis) %>% 
  mutate(CVE_ENT=as.integer(CVE_ENT),
         CVE_MUN=as.integer(CVE_MUN))

# Intersect with municipios and simplify
ag_munis <- sup_ag  %>%
  st_simplify(dTolerance = 0.0001) %>% 
  st_intersection(munis)  %>% 
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

# Save to file
out_fp <- file.path('data/input_data/SIAP/FASII',
                    str_glue('FASII_{region}_by_municipio.geojson'))
ag_munis %>% st_write(out_fp, delete_dsn=T)

# Save to R data
save(ag_munis, file='data/data_out/ag_munis.RData')

# Load frijol-maiz-granos polygons ----------------------------------------------------------
data_dir <- 'data/input_data/SIAP/ESA_frijol_maiz_granos'

# Load data
f.sup.frij.gran <- 'http://infosiap.siap.gob.mx/gobmx/datosAbiertos/estimacion-superficie-agricola/ESA_PV2015_AGS.kmz'
frij_gran_fp <- file.path(data_dir, 
                          str_c(file_path_sans_ext(basename(f.sup.frij.gran)), 
                                '.geojson'))
sup_frij_gran <- st_read(frij_gran_fp)
if(!exists('sup_frij_gran')){
  sup_frij_gran <- load_kmz_as_sf(f.sup.frij.gran, data_dir)
}

# Simplify to remove jagged edges and decrease size
sup_frij_gran <- st_simplify(sup_frij_gran, preserveTopology = T, dTolerance = .0001)

# Add area column
sup_frij_gran$area_ha <- sup_frij_gran %>% 
  st_transform(crs=6362) %>% 
  st_area() %>% 
  set_units('ha') %>% 
  set_units(NULL)

# Recode Municipios to CVE code - make named list for lookup
lookup_cve_to_mun <- cultivos %>% 
  filter(CVE_ENT==1) %>% 
  select(Nommunicipio, CVE_MUN) %>% 
  mutate(Nommunicipio = str_to_title(Nommunicipio)) %>% 
  distinct %>% 
  deframe()

# Recode Municipios to CVE code - perform recode
sup_frij_gran <- sup_frij_gran %>% 
  mutate(NOM_MUN = str_to_title(NOM_MUN)) %>% 
  mutate(CVE_MUN = recode(NOM_MUN, !!!lookup_cve_to_mun))

# Get frijol and save
sup_frijol <- sup_frij_gran %>% 
  filter(CULTIVO=='Frijol') %>% 
  mutate(Frijol = recode(CULTIVO, 'Frijol'=1.0))
st_write(sup_frijol, 'data/data_out/polys_frijol_AGS.geojson')

# Save
save(sup_frij_gran, file='data/data_out/sup_frij_gran.RData')

# Extract frijol, maíz, and granos from general agriculture --------------------
# Clip out FMG polygons -----
# Filter to Aguascalientes
ag_munis <- ag_munis %>% filter(CVE_ENT==1)

# Difference
ag_noFMG <- st_difference(
    ag_munis, 
    st_union(sup_frij_gran)
  )

# Tidy the polygons - drop crumbs and fill pinholes
ag_noFMG <- ag_noFMG %>%
  st_transform(crs=6362) %>% 
  smoothr::drop_crumbs(threshold=1000) %>%
  smoothr::fill_holes(threshold=100)  %>%
  st_transform(crs=4326) %>% 
  st_make_valid()

mapview(ag_noFMG)

# Get area for new polygons
ag_noFMG$area <- ag_noFMG %>% 
  st_transform(crs=6362) %>% 
  st_area() %>% 
  set_units('ha') %>% 
  set_units(NULL)

# Save
save(ag_noFMG, file='data/data_out/polys_ag_noFMG_AGS.RData')

# LOAD DATA --------------------------------------------------------------------
load("data/data_out/area_sembrada_by_season.RData")
load('data/data_out/ag_munis.RData')
load('data/data_out/sup_frij_gran.RData')

# Select crops with high pollinator importance
important_crops <- c("aguacate", 'jitomate', 'café','mango',
                     'sandía','manzana','melón', 'calabaza', 
                     'pepino', 'brócoli', 'cacao', 'zarzamora', 
                     'fresa', 'zanahoria', 'soya', 'frijol')

# Look
mapview(ag_munis) +
  mapview(ag_noFMG) +
  mapview(sup_fmg, zcol=('CULTIVO'))

# Remove FMG colummns and calculate columns ------------------------------------
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
load("data/data_out/area_sembrada_by_season.RData")

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


