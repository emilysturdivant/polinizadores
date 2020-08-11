# Convert Apendice 1 to a dataframe. It looks like this produces some errors. Better just to ask for 
# library(rgdal)
library(sf)
library(tidyverse)
library(magrittr)
library(mapedit)

# f.usv.16 <- 'http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/tematicas/uso_suelo/889463173359_s.zip'
# Links from https://www.inegi.org.mx/temas/usosuelo/default.html#Descargas
usv_url <- 'http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/tematicas/uso_suelo/1_250_000/serie_VI/889463598459_s.zip'
data_dir <- 'data/input_data/INEGI_2017'

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
f.usv <- 'data/input_data/INEGI_2017/conjunto_de_datos/usv250s6n.shp'
polys_nom <- st_read(f.usv, crs=6362) %>%  
  st_make_valid %>% 
  st_transform(4326)

# Merge Nómada with Agriculture
polys_nom %<>% 
  mutate(TIPAGES='AGRICULTURA NÓMADA', TIP_CUL1='NÓMADA') %>% 
  select(!NOM)
polys <- list(polys_ag, polys_nom) %>% 
  mapedit:::combine_list_of_sf() %>% 
  st_make_valid

saveRDS(polys, 'data/data_out/r_data/polys_ag_INEGI.rds')
polys <- readRDS('data/data_out/r_data/polys_ag_INEGI.rds')

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
  select(-AREA, -PERIMETER, -COV_, -COV_ID)

inegi_polys <- st_intersection(polys, mun_for_join)
inegi_polys %>% colnames
inegi_polys %>% select(CLAVE)
st_write(inegi_polys, 'data/data_out/polys_ag_INEGI.geojson', delete_dsn=T)


saveRDS(polys_mun, 'data/data_out/r_data/polys_ag_INEGI_munis.rds')


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
f.usv <- 'data/input_data/INEGI_usodesuelo_2017/conjunto_de_datos/usv250s6c.shp'
pts_cult <- st_read(f.usv, crs=6362)
colnames(pts_cult)
# Decode cultivos for most important pollinator crops
key <- read_csv('data/input_data/INEGI_usodesuelo_2017/metadatos/INEGI_codes.csv')
key %<>% pmap(~set_names(..2, ..1)) %>% unlist() %>% 
  c(.default = NA_character_)
pts_cult %<>% mutate(Culti1=recode(Culti1, !!!key),
                     Culti2=recode(Culti2, !!!key),
                     Culti3=recode(Culti3, !!!key))
# Look at numbers of points per important crop
cults <- pts_cult %>% select(starts_with('Culti')) 
st_geometry(cults) <- NULL
culti_ct <- cults %>% 
  gather('name', 'cultivo') %>% 
  group_by(cultivo) %>% 
  summarise(no_rows = length(cultivo)) %>%
  arrange(desc(no_rows)) %>% 
  View()

# Forestales points ----
# Forestales points from INEGI - all are Agricola-Pecuaria-Forestal
f.usv <- 'data/input_data/INEGI_usodesuelo_2017/conjunto_de_datos/usv250s6f.shp'
pts_for <- st_read(f.usv, crs=6362)
colnames(pts_for)

# Look at numbers of points per important crop
grps <- pts_for %>%
  group_by(Clave) %>% 
  summarise(no_rows = length(Clave)) %>% 
  View()

