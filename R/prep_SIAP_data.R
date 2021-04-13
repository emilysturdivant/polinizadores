# SIAP Datos Abiertos page: http://infosiap.siap.gob.mx/gobmx/datosAbiertos.php
# Catalogos: http://www.agricultura.gob.mx/siap/catalogos

# Load libraries ----
box::use(R/functions[get_area_planted, 
                     load_and_preprocess_fmg, 
                     est_to_cve])

# library(sf)
# library(tidyverse)
# library(rvest)
# library(tools)
# library(mapview)
# library(units)

# Load pre-created objects (from functions.R) ----------------------------------
# load('data/helpers/initial_vars.RData')
# load('data/helpers/functions.RData')
out_dir <- 'data/data_out/polys_ag_INEGI_noFMG'
fmg_dir <- 'data/data_out/polys_fmg'

# Load SIAP Cultivos - area sembrada -------------------------------------------
year <- '2019'
url_cult_stats <- str_c('http://infosiap.siap.gob.mx/gobmx/datosAbiertos/',
                     'ProduccionAgricola/Cierre_agricola_mun_', year, '.csv')
cultivos <- read_csv(url_cult_stats, col_types = 'iicicicicicicicicnnnnnnn', 
                     locale=locale(encoding='latin1')) %>% 
  rename(CVE_ENT=Idestado, CVE_MUN=Idmunicipio)

# filter to year-round
area_cult_peren <- cultivos %>% 
  filter(Idciclo==3) %>% 
  get_area_planted(CVE_ENT, CVE_MUN, Nomcultivo, Idmodalidad, Sembrada, total_sembrada) %>% 
  mutate(Idmodalidad=recode(Idmodalidad, '1' = "R", '2' = "T"))

area_cult_peren %>% colnames
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

# filter to spring and year-round
area_cult_primperen <- cultivos %>% 
  filter(Idciclo %in% c(2,3)) %>% 
  get_area_planted(CVE_ENT, CVE_MUN, Nomcultivo, Idmodalidad, Sembrada, total_sembrada) %>% 
  mutate(Idmodalidad=recode(Idmodalidad, '1' = "R", '2' = "T"))

# filter to fall and year-round
area_cult_otoperen <- cultivos %>% 
  filter(Idciclo %in% c(1,3)) %>% 
  get_area_planted(CVE_ENT, CVE_MUN, Nomcultivo, Idmodalidad, Sembrada, total_sembrada) %>% 
  mutate(Idmodalidad=recode(Idmodalidad, '1' = "R", '2' = "T"))

# Save
save(area_cult_peren, area_cult_prim, area_cult_otono, area_cult_primperen, 
     area_cult_otoperen, cultivos,
     file = "data/data_out/r_data/area_sembrada_by_season_2019.RData")

area_cult_peren %>% saveRDS("data/data_out/r_data/area_sembrada_peren_2019.RDS")
area_cult_primperen %>% saveRDS("data/data_out/r_data/area_sembrada_primperen_2019.RDS")
area_cult_otoperen %>% saveRDS("data/data_out/r_data/area_sembrada_otoperen_2019.RDS")

# Municipio polygons -----------------------------------------------------------
fp_munis <- 'data/input_data/context_Mexico/SNIB_divisionpolitica/muni_2018gw/muni_2018gw.shp'
munis <- st_read(fp_munis) %>% 
  mutate(CVE_ENT=as.integer(CVE_ENT),
         CVE_MUN=as.integer(CVE_MUN))

# Get lookup for Estado and Municipio to CVE code
lookup <- munis %>% 
  st_set_geometry(NULL) %>% 
  select(NOM_ENT, NOM_MUN, CVE_ENT, CVE_MUN) %>% 
  mutate(CVE_ENT = as.integer(CVE_ENT),
         CVE_MUN = as.integer(CVE_MUN),
         across(where(is.character), ~ str_to_title(.x)),
         across(where(is.character), 
                ~ stringi::stri_trans_general(str=.x, id='Latin-ASCII')),
         NOM_ENT = str_replace(NOM_ENT, 'Michoacan De Ocampo', 'Michoacan'),
         NOM_ENT = str_replace(NOM_ENT, 'Veracruz De Ignacio De La Llave', 'Veracruz'))
lookup %>% saveRDS('data/helpers/lookup_municipio_codes.rds')

# Download and convert from KMZ
regions %>% sapply(load_and_preprocess_ag, ag_dir, munis, F)

# Load frijol-maiz-granos polygons ---------------------------------------------
# Download and convert from KMZ
est_codes <- names(est_to_cve)
est_codes %>% sapply(load_and_preprocess_fmg, fmg_dir, FALSE, munis)

# Fix Region Lagunera to match the others 
estado = 'RL'
sup_fmg <- load_and_preprocess_fmg(estado, fmg_dir, T, munis)
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
sup_fmgR <- load_and_preprocess_fmg('RL', fmg_dir, T, munis)

sup_fmgD <- load_and_preprocess_fmg('DGO', fmg_dir, T, munis) 
mapview(sup_fmgD)
dur <- sup_fmgR %>% filter(DELEGACIÓN=='Durango')
sup_fmg <- list(sup_fmgD, dur) %>% 
  mapedit:::combine_list_of_sf()
fp_fmg <- file.path(fmg_dir, str_c('ESA_PV2015_', 'DGO', '.geojson'))
st_write(sup_fmg, fp_fmg, delete_dsn=T)

sup_fmgZ <- load_and_preprocess_fmg('ZAC', fmg_dir, T, munis)
polys <- sup_fmgR %>% filter(DELEGACIÓN=='Zacatecas')
sup_fmg <- list(sup_fmgZ, polys) %>% 
  mapedit:::combine_list_of_sf()
fp_fmg <- file.path(fmg_dir, str_c('ESA_PV2015_', 'ZAC', '.geojson'))
st_write(sup_fmg, fp_fmg, delete_dsn=T)

sup_fmgC <- load_and_preprocess_fmg('COAH', fmg_dir, T, munis) 
polys <- sup_fmgR %>% filter(DELEGACIÓN=='Zacatecas')
sup_fmg <- list(sup_fmgZ, polys) %>% 
  mapedit:::combine_list_of_sf()
fp_fmg <- file.path(fmg_dir, str_c('ESA_PV2015_', 'COAH', '.geojson'))
st_write(sup_fmg, fp_fmg, delete_dsn=T)

# Fix AGS - add area column
estado = 'AGS'
sup_fmg <- load_and_preprocess_fmg(estado, fmg_dir, T, 
                                   munis, prefix='fmg_siap15_')

# Get area for new polygons
sup_fmg$area <- sup_fmg %>% 
  st_transform(crs=6362) %>% 
  st_area() %>% 
  set_units('ha') %>% 
  set_units(NULL)

fp_fmg <- file.path(fmg_dir, str_c('fmg_siap15_', estado, '.geojson'))
st_write(sup_fmg, fp_fmg, delete_dsn=T)

# Save frijol polys separately -------------------------------------------------
# sup_frijol <- sup_frij_gran %>% 
#   filter(CULTIVO=='Frijol') %>% 
#   mutate(Frijol = recode(CULTIVO, 'Frijol'=1.0))
# fp <- file.path(data_dir, 'frijol', str_c('ESA_PV2015_frijol_', estado_code, '.geojson'))
# st_write(sup_frijol, fp, delete_dsn=T)

# Save
# save(sup_frij_gran, file='data/data_out/r_data/sup_frij_gran.RData')

# Extract FMG from general agriculture -----------------------------------------
for(estado in est_codes){
  remove_FMG_from_ag_by_state(estado, ag_dir, fmg_dir)
}
remove_FMG_from_ag_by_state(estado, ag_dir, fmg_dir)

# Look
mapview(ag, zcol='MOD_AGR')+
  mapview(sup_fmg, zcol='CULTIVO')+
  mapview(siap_noFMG)

# LOAD DATA --------------------------------------------------------------------
load("data/data_out/r_data/area_sembrada_by_season_2019.RData")

# Select crops with high pollinator importance
important_crops <- c("aguacate", 'jitomate', 'café','mango',
                     'sandía','manzana','melón', 'calabaza', 
                     'pepino', 'brócoli', 'cacao', 'zarzamora', 
                     'fresa', 'zanahoria', 'soya', 'frijol')

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
# ag_pct_cult_noFMG <- left_join(ag_noFMG, pct_sembrada_noFMG, 
#                                by=c('CVE_ENT', 'CVE_MUN', 'MOD_AGR'='Idmodalidad'))

estado <- 'AGS'
siap_noFMG <- st_read(file.path(out_dir, str_c('siap_noFMG_', estado, '.geojson')))
ag_pct_cult_noFMG <- left_join(siap_noFMG, pct_sembrada_noFMG, 
                               by=c('CVE_ENT', 'CVE_MUN', 'MOD_AGR'='Idmodalidad'))
# ***** SAVE ***** ----
ag_pct_cult_noFMG %>% 
  st_write(dsn='data/data_out/polys_ag_noFMG_AGS_percents.geojson', delete_dsn=T)

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


# Agrícultural protegida -------------------------------------------------------
# AgriPro shapefile from Jean Francois: http://lae.ciga.unam.mx/recursos/Agricultura_Protegida_2018.rar

# agripro <- read.csv(f.stat.agripro, header=TRUE, fileEncoding="latin1")
f.stat.agripro <- str_c('http://infosiap.siap.gob.mx/gobmx/datosAbiertos/',
                        'Sup_Agricul_Protegida/Agricultura_Protegida_2015.csv')
agripro <- read_csv(f.stat.agripro, col_types = 'iciciciciccccinn', locale=locale(encoding='latin1'))
save(agripro, file='data/data_out/r_data/agripro.RData')

# Load data
load('data/data_out/r_data/agripro.RData')

# Separate CULTIVO column into multiple rows for each item in list. ----
t2 <- agripro %>% 
  rowid_to_column %>% 
  mutate(CULTIVOS = 
           str_to_lower(CULTIVOS) %>% 
           replace_na('unknown')) %>% 
  separate_rows(CULTIVOS, sep=' y |, ') %>% 
  left_join(y=count(., rowid), 
            by='rowid') %>% 
  mutate(SUP_HAS = SUP_HAS/n)
t3 <- t2 %>% 
  group_by(across(-rowid & -n & -AÑO_VERIF & -SUP_HAS & -SUP_M2)) %>%
  summarize(sup_has = sum(SUP_HAS)) %>% 
  ungroup

# Look ----
t2 %>%
  # filter(rowid %in% seq(232, 234)) %>% 
  filter(CVE_EDO == 11, CVE_MUN == 44, CULTIVOS=='tomate') %>% 
  select(-CVE_EDO:-NOM_MUN) 

t3 %>%
  filter(CVE_EDO == 11, CVE_MUN == 44) %>%
  select(-CVE_EDO:-NOM_MUN) 

# standardize CULTIVOS names (and then move to before grouping) ----
t3 %>% select(CULTIVOS) %>% distinct

# Percent coverage per municipio
total_by_muni <- t3 %>% 
  group_by(across(-CULTIVOS & -sup_has)) %>% 
  summarize(total_has = sum(sup_has)) %>% 
  ungroup
vars <- total_by_muni %>% 
  select(-total_has) %>% 
  colnames  

t4 <- t3 %>% 
  pivot_wider(names_from = CULTIVOS, values_from = sup_has) %>% 
  left_join(total_by_muni, by=vars) %>% 
  relocate(total_has, .after=ESTATUS)

# Look
t4 %>%
  filter(CVE_EDO == 11, CVE_MUN == 44) %>%
  select(-CVE_EDO:-NOM_MUN) 

# Get percent of agricultural land of each crop 
vars <- t4 %>% 
  select(-CVE_EDO:-total_has) %>% 
  colnames 
pcts <- t4 %>% 
  mutate(across(
    .cols = all_of(vars), 
    .fns = ~ .x / total_has
  )) 

# Look ----
pcts %>%
  filter(CVE_EDO == 11, CVE_MUN == 44) %>%
  pivot_longer(all_of(vars), names_to='cultivo', values_drop_na=T) %>% 
  select(-CVE_EDO:-NOM_MUN) 





# Scope it out
agripro %>% 
  select(CULTIVOS) %>% 
  distinct %>% 
  deframe
cults <- agripro %>% 
  select(CULTIVOS) %>% 
  distinct %>% 
  sample_n(20)
c_split <- cults$CULTIVOS %>% str_split(' y |, ')
c_split[[1]]

agripro %>% 
  filter(CVE_EDO == 11, CVE_MUN == 44) %>% 
  select(-CVE_EDO:-NOM_MUN) 

agripro %>% 
  slice(232:234) %>% 
  select(CVE_EDO:NOM_MUN) 







