
# Load libraries ----
library(sf)
library(tidyverse)
library(magrittr)
library(rvest)
library(tools)
library(mapview)
library(units)


load('data/helpers/initial_vars.RData')
load('data/helpers/functions.RData')
out_dir <- 'data/data_out/polys_ag_INEGI_noFMG'
fmg_dir <- 'data/data_out/polys_fmg'

# Remove frijol, maiz, granos from INEGI polygons (cross-check first)
# INEGI ----
inegi_polys <- st_read('data/data_out/polys_ag_INEGI.geojson')
# Municipio polygons 
fp_munis <- 'data/input_data/context_Mexico/SNIB_divisionpolitica/muni_2018gw/muni_2018gw.shp'
munis <- st_read(fp_munis) %>% 
  mutate(CVE_ENT=as.integer(CVE_ENT),
         CVE_MUN=as.integer(CVE_MUN))

# Run per state ----
estado = 'ZAC'

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

# Fix MICH fmg polys ----
estado <- 'MICH'
fmg_1 <- st_read(file.path('data/input_data/SIAP/frijol_maiz_granos', 
                           str_c('ESA_PV2015_', estado, '.geojson')))  %>% 
  rename('NOM_ENT' = 'DELEGACIÓN') %>% 
  select(CULTIVO, CVE_ENT, CVE_MUN, NOM_ENT, NOM_MUN, area_ha) %>% 
  mutate(CVEGEO = str_c(str_pad(CVE_ENT,2), str_pad(CVE_MUN, 3, pad='0')))
fmg_2 <- st_read(file.path('data/data_out/polys_fmg', 
                           str_c('fmg_siap15_', estado, '.geojson'))) 

cve_ent <- est_to_cve[[estado]]
cve_mun <- 71

# Filter
fmg1 <- fmg_1 %>% filter(CVE_MUN == cve_mun, CULTIVO == 'Maiz Grano')

# Simplify to remove jagged edges and decrease size
fmg1d <- fmg1 %>% 
  st_simplify(preserveTopology = T, dTolerance = .0001)

# Combine
fmg_3 <- mapedit:::combine_list_of_sf(list(fmg_2, fmg1))

# Check
mapview(filter(fmg_2, CVE_MUN == cve_mun)) +
  mapview(fmg1) +
  mapview(fmg_3)

st_write(fmg_3, file.path('data/data_out/polys_fmg', 
                          str_c('fmg_siap15_', estado, '_redo.geojson')))

# Fix CHIS fmg polys ----
estado <- 'CHIS'
fmg_1 <- st_read(file.path('data/input_data/SIAP/frijol_maiz_granos', 
                           str_c('ESA_PV2015_', estado, '.geojson')))  %>% 
  rename('NOM_ENT' = 'DELEGACIÓN') %>% 
  select(CULTIVO, CVE_ENT, CVE_MUN, NOM_ENT, NOM_MUN, area_ha) %>% 
  mutate(CVEGEO = str_c(str_pad(CVE_ENT,2), str_pad(CVE_MUN, 3, pad='0')), 
         CVE_ENT = as.integer(CVE_ENT), 
         CVE_MUN = as.integer(CVE_MUN),
         CULTIVO = 
             stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
             str_trim %>%
             str_to_lower)
fmg_2 <- st_read(file.path('data/data_out/polys_fmg', 
                           str_c('fmg_siap15_', estado, '.geojson'))) %>% 
  mutate(CULTIVO = 
            stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
            str_trim %>%
            str_to_lower, 
         CVE_ENT = as.integer(CVE_ENT), 
         CVE_MUN = as.integer(CVE_MUN))

cve_ent <- est_to_cve[[estado]]
cve_mun <- c(6, 10, 34)

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
# mapview(filter(fmg_2, CVE_MUN == cve_mun)) +
#   mapview(fmg1) +
#   mapview(fmg_3)
mapview(st_crop(fmg_2, box)) +
  mapview(st_crop(fmg1, box)) +
  mapview(st_crop(fmg_3, box)) 

st_write(fmg_3, file.path('data/data_out/polys_fmg', 
                          str_c('fmg_siap15_', estado, '_redo.geojson')))

# Fix ZAC fmg polys ----
estado <- 'ZAC'
fmg_1 <- st_read(file.path('data/input_data/SIAP/frijol_maiz_granos', 
                           str_c('ESA_PV2015_', estado, '.geojson')))  %>% 
  rename('NOM_ENT' = 'DELEGACIÓN') %>% 
  select(CULTIVO, CVE_ENT, CVE_MUN, NOM_ENT, NOM_MUN, area_ha) %>% 
  mutate(CVEGEO = str_c(str_pad(CVE_ENT,2), str_pad(CVE_MUN, 3, pad='0')), 
         CVE_ENT = as.integer(CVE_ENT), 
         CVE_MUN = as.integer(CVE_MUN),
         CULTIVO = 
           stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
           str_trim %>%
           str_to_lower)
fmg_2 <- st_read(file.path('data/data_out/polys_fmg', 
                           str_c('fmg_siap15_', estado, '.geojson'))) %>% 
  mutate(CULTIVO = 
           stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
           str_trim %>%
           str_to_lower, 
         CVE_ENT = as.integer(CVE_ENT), 
         CVE_MUN = as.integer(CVE_MUN))

cve_ent <- est_to_cve[[estado]]
cve_mun <- c(42,51)

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
# mapview(filter(fmg_2, CVE_MUN == cve_mun)) +
#   mapview(fmg1) +
#   mapview(fmg_3)
mapview(st_crop(fmg_2, box)) +
  mapview(st_crop(fmg1, box)) +
  mapview(st_crop(fmg_3, box)) 

st_write(fmg_3, file.path('data/data_out/polys_fmg', 
                          str_c('fmg_siap15_', estado, '_redo.geojson')))

# Fix VER fmg polys ----
estado <- 'VER'
fmg_1 <- st_read(file.path('data/input_data/SIAP/frijol_maiz_granos', 
                           str_c('ESA_PV2015_', estado, '.geojson')))  %>% 
  rename('NOM_ENT' = 'DELEGACIÓN') %>% 
  select(CULTIVO, CVE_ENT, CVE_MUN, NOM_ENT, NOM_MUN, area_ha) %>% 
  mutate(CVEGEO = str_c(str_pad(CVE_ENT,2), str_pad(CVE_MUN, 3, pad='0')), 
         CVE_ENT = as.integer(CVE_ENT), 
         CVE_MUN = as.integer(CVE_MUN),
         CULTIVO = 
           stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
           str_trim %>%
           str_to_lower)
fmg_2 <- st_read(file.path('data/data_out/polys_fmg', 
                           str_c('fmg_siap15_', estado, '.geojson'))) %>% 
  mutate(CULTIVO = 
           stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
           str_trim %>%
           str_to_lower, 
         CVE_ENT = as.integer(CVE_ENT), 
         CVE_MUN = as.integer(CVE_MUN))

cve_ent <- est_to_cve[[estado]]
cve_mun <- c(100, 149)

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


# HGO ----
# Merge files from individual municipios
estado = 'HGO'
state_dir <- file.path(out_dir, estado)
fps <- list.files(state_dir, full.names=T)
ag_noFMG <- fps %>% 
  lapply(st_read) %>% 
  mapedit:::combine_list_of_sf()
final_fp_out <- file.path(out_dir, str_c('inegi_noFMG_', estado, '.geojson'))
ag_noFMG %>% st_write(final_fp_out)

# Join SIAP crop stats to polygons ---------------------------------------------
load("data/data_out/r_data/area_sembrada_by_season_2019.RData")
estado <- 'COL'

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

# get CVE_ENT from ag
cve_ent <- ag_noFMG %>% 
  st_set_geometry(NULL) %>% 
  select(CVE_ENT) %>% 
  distinct %>% 
  deframe

# SIAP stats to percents ------
# Filter [primavera] to state, remove FMG columns and any with all NA values
df <- area_cult_prim %>% 
  filter(CVE_ENT==cve_ent) %>% 
  select(!contains(c('maíz', 'frijol', 'sorgo', 'trigo', 'triticale'))) %>% 
  select_if(~sum(!is.na(.)) > 0)

# List columns
vars <- df %>% 
  select(-CVE_ENT:-total_sembrada) %>% 
  colnames()

# Get new total area 
df$total_noFMG <- df %>% 
  select(all_of(vars)) %>% 
  rowSums(na.rm=T)
df %<>% relocate(total_noFMG, .after=total_sembrada)

# Get number of different crops planted
df$count_crops <- df %>% 
  select(all_of(vars)) %>% 
  is.na %>% 
  `!` %>% 
  rowSums
df %<>% relocate(count_crops, .before=total_sembrada)

# Get percent of agricultural land of each crop 
pct_sembrada_noFMG <- df %>% 
  mutate(across(
    .cols = all_of(vars), 
    .fns = ~ .x / total_noFMG
  )) 

# Join ------
ag_pct_cult_noFMG <- left_join(ag_noFMG, pct_sembrada_noFMG, 
                               by=c('CVE_ENT', 'CVE_MUN', 'Idmodalidad'))

# Add polys from FMG with pct == 1.0 ----
# Load specific state from FMG
sup_fmg <- load_and_preprocess_fmg(estado, fmg_dir, T) %>% 
  mutate(CVE_MUN = as.integer(CVE_MUN), 
         CVE_ENT = as.integer(CVE_ENT))

# Create FMG columns
sup_fmg <- sup_fmg %>%
  rename('area' = 'area_ha') %>% 
  mutate('Frijol' = recode(CULTIVO, 'Frijol' = 1.0, .default=NA_real_), 
         'Maíz grano' = recode(CULTIVO, 'Maíz grano' = 1.0, .default=NA_real_), 
         'Sorgo grano' = recode(CULTIVO, 'Sorgo grano' = 1.0, .default=NA_real_),
         'Trigo grano' = recode(CULTIVO, 'Trigo grano' = 1.0, .default=NA_real_),
         count_crops = 1, 
         total_sembrada = area) %>% 
  select(-CULTIVO, -NOM_MUN, -DELEGACIÓN, -Name) 

# Combine with general ag
sup_new <- mapedit:::combine_list_of_sf(list(ag_pct_cult_noFMG, sup_fmg))
mapview(sup_new) + mapview(sup_fmg)

# ***** SAVE ***** ----
fp_out <- file.path('data/data_out/polys_ag_INEGI_noFMG_pcts', 
                    str_c('inegi_noFMG_', estado, '_pcts.geojson'))
ag_pct_cult_noFMG %>% 
  st_write(fp_out, delete_dsn=T)

