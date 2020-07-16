
# Load libraries ----
library(sf)
library(tidyverse)
library(magrittr)
library(tmap)
tmap_mode('view')
library(rmapshaper)
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
    summarize({{planted_fld}} := sum({{planted_fld}})) %>%
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
              by=c(quo_name(state_id_fld), quo_name(muni_id_fld), quo_name(riego_id_fld)))
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

# Intersect with municipios
ag_munis <- sup_ag %>% 
  st_intersection(munis)  %>% 
  group_by(CVE_ENT, CVE_MUN, MOD_AGR) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  as_tibble() %>% 
  st_as_sf() 

# get area for new polygons
ag_munis$area <- ag_munis %>% 
  st_transform(crs=6362) %>% 
  st_area() %>% 
  set_units('ha') %>% 
  set_units(NULL)

# Save to file
out_fp <- file.path('data/input_data/SIAP/FASII',
                    str_glue('FASII_{region}_by_municipio.geojson'))
ag_munis %>% st_write(out_fp, delete_dsn=T)

save(ag_munis, file='data/data_out/ag_munis.RData')

# Get frijol polygons ----------------------------------------------------------
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

# Save
save(sup_frij_gran, file='data/data_out/sup_frij_gran.RData')

# Join crop area by municipio --------------------------------------------------
# Load data
load("data/data_out/area_sembrada_by_season.RData")
load('data/data_out/ag_munis.RData')
load('data/data_out/sup_frij_gran.RData')

# Filter to Aguascalientes for testing 
ag_munis <- ag_munis %>% filter(CVE_ENT==1)

# Select crops with high pollinator importance
important_crops <- c("aguacate", 'jitomate', 'café','mango',
                     'sandía','manzana','melón', 'calabaza', 
                     'pepino', 'brócoli', 'cacao', 'zarzamora', 
                     'fresa', 'zanahoria', 'soya', 'frijol')

# Use primavera 
area_sembrada <- area_cult_prim

# Get percent of agricultural land of each crop 
pct_sembrada <- area_sembrada %>% 
  mutate(across(
    .cols = -CVE_ENT:-total_sembrada, 
    .fns = ~ .x / total_sembrada
  ))

# Get number of differenet crops planted
area_sembrada$count_crops <- area_sembrada %>% 
    select(-CVE_ENT:-total_sembrada) %>% 
    is.na %>% 
    `!` %>% 
    rowSums
area_sembrada %<>% relocate(count_crops, .after=total_sembrada)

  
# Join
ag_cultivos <- left_join(ag_munis, area_sembrada, 
                             by=c('CVE_ENT', 'CVE_MUN', 'MOD_AGR'='Idmodalidad'))

# Extract frijol, maíz, and granos from general agriculture --------------------

# Crop for testing
sup_frij_gran %>% object.size %>% print(units='MB')

bb <- st_bbox(sup_frij_gran)
sup_fmg <- sup_frij_gran %>% 
  st_crop(bb + c(.5, .5, -.3, -.2))
ag_munis <- ag_munis %>% 
  st_crop(bb + c(.5, .5, -.3, -.2))

# Dissolve
sup_fmg_diss <- st_union(sup_fmg)

# Difference
ag_noFMG <- st_difference(ag_munis, sup_fmg_diss)

# Look
mapview(ag_munis) +
  mapview(ag_noFMG) +
  mapview(ag_cultivos, zcol='Frijol') +
  mapview(pct_sembrada, zcol='Frijol') +
  mapview(sup_fmg, zcol=('CULTIVO'))

# Remove FMG from ag -----
area_sembrada_noFMG <- area_sembrada %>% 
  select(!contains(c('maíz grano', 'frijol', 'sorgo grano', 'trigo grano')))
pct_sembrada_noFMG <- pct_sembrada %>% 
  select(!contains(c('maíz grano', 'frijol', 'sorgo grano', 'trigo grano')))

# Join
ag_cult_noFMG <- left_join(ag_noFMG, area_sembrada_noFMG, 
                         by=c('CVE_ENT', 'CVE_MUN', 'MOD_AGR'='Idmodalidad'))
ag_pct_cult_noFMG <- left_join(ag_noFMG, pct_sembrada_noFMG, 
                           by=c('CVE_ENT', 'CVE_MUN', 'MOD_AGR'='Idmodalidad'))

# Look
mapview(ag_pct_cult_noFMG, zcol='Fresa') +
  mapview(sup_fmg, zcol=('CULTIVO'))





# meow ----
# Drop columns with only NA values
ag_cults1 <- ag_cultivos %>% 
  select_if(~sum(!is.na(.)) > 0) 
ag_cults1 %>% colnames()
# Select only the important crops
ag_cults2 <- ag_cults1 %>% 
  select(CVE_ENT:total_sembrada, contains(important_crops))
ag_cults2 %>% colnames()

crop <- "Ajo"
mapview(ag_cultivos, zcol=crop, layer.name='Area') +
  mapview(pct_sembrada, zcol=crop, layer.name='Pct crop area') 




# Important crops --------------------------------------------------------------


# Drop municipios without important crops
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

ag_important_cults2 %>% 
  tm_shape()+
  tm_polygons()+
  tm_facets(sync = TRUE, ncol = 3)

ag_cultivos %>% colnames
ag_cultivos %>% 
  # select(Aguacate, Frijol) %>% 
  mapview(zcol=c("Avena.forrajera.en.verde", "Frijol"))

  
  
  
  
  

pct_cult_important <- pct_cult %>% 
  select(Idestado, Idmunicipio, contains(important_crops))

# Check out plot
plot(select(munis_cult, contains('jitomate')))

# By state
munis_cult_mich <- munis_cult %>% filter(NOM_ENT=='Michoacán de Ocampo')
# object.size(munis_cult_mich) %>% print(units="MB")
munis_cult_mich %>% 
  select(one_of('Tomate rojo (jitomate)', 'Pepino', 'Zarzamora', 'Melón')) %>% 
  tm_shape()+
  tm_polygons(c('Tomate rojo (jitomate)', 'Pepino', 'Zarzamora', 'Melón'))+
  tm_facets(sync = TRUE, ncol = 2)

# Intersect municipios with agricultural coverage -----



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





# Old -----
f.sup.frij.gran <- 'data/Michoacan_beta/ESA_PV_2015_NACIONAL.kml'
f.sup.frij.gran <- 'data/Michoacan_beta/ESA_PV_2015_NACIONAL.geojson'

# Load SIAP superficie de frijol (two options)
# GeoJSON, converted from KMZ in QGIS using Expand HTML description field (KML Tool)
# f.sup.frij.gran <- 'data/Michoacan_beta/ESA_PV_2015_NACIONAL.geojson'
# sup.frij.gran <- st_read(f.sup.frij.gran)
# summary(sup.frij.gran)

# KML - KMZ converted to KML in QGIS
f.sup.frij.gran <- 'data/input_data/SIAP/ESA_PV2015_MICHq.kml'
sup.frij.gran <- st_read(f.sup.frij.gran)
sup.frij.gran$Cultivo <- sup.frij.gran$description %>% 
  as.character() %>% 
  str_extract('Maíz grano|Sorgo grano|Trigo grano|Frijol')
sup.frij.gran %<>% select(Name, Cultivo, geometry)
object.size(sup.frij.gran) %>% print(units='MB')

# Extract Frijol
frijol <- sup.frij.gran %>% filter(Cultivo=='Frijol')
plot(frijol[2])

# Extract Granos (all except frijol)
granos <- sup.frij.gran %>% filter(!grepl('Frijol', Cultivo))
plot(granos[2])

# Simplify polygons
simplepolys <- rmapshaper::ms_simplify(input = sup.frij.gran) %>%
  st_as_sf()
plot(simplepolys[2]) # plot by Cultivo (2nd column)
object.size(simplepolys) %>% print(units='MB')
tm_shape(simplepolys) + tm_polygons(col='Cultivo')

simplepolys %>% st_write('data/input_data/SIAP/ESA_PV2015_MICHq_simpleR.gpkg')

#----
# Compare INEGI ag and SIAP select ag

#----
# From INEGI agricultura uso de suelo, extract SIAP superficie de frijol


#----
# Then we have superficie de frijol and of everything else. For the everything else, assign probabilities

