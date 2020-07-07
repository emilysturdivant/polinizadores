# Convert Apendice 1 to a dataframe. It looks like this produces some errors. Better just to ask for 
# library(rgdal)
library(sf)
library(tidyverse)
library(magrittr)
library(mapedit)

#----
# Get Agricultural polygons
f.usv <- '~/PROJECTS/Pollinator_services/input_data/INEGI_usodesuelo_2017/conjunto_de_datos/usv250s6g.shp'
usv <- st_read(f.usv)
st_crs(st_geometry(usv)) <- 6362 # set EPSG code
# Filter TIP_INFO == AGRÍCOLA-PECUARIA-FORESTAL
polys_ag <- usv %>% filter(TIP_INFO == 'AGRÍCOLA-PECUARIA-FORESTAL')

# Look at numbers of polygons in each cultivo
polys_ag %>% 
  st_drop_geometry() %>%
  group_by(TIP_CUL1) %>% 
  summarise(no_rows = length(TIP_CUL1)) %>% 
  View()
polys_ag %>% 
  st_drop_geometry() %>% 
  filter(grepl('.+TEMPORAL', TIPAGES, ignore.case=TRUE)) %>% 
  group_by(CLAVE) %>% 
  summarise(no_rows = length(CLAVE)) %>% 
  View()

# Nómada polygons from INEGI - all are nómada (shifting cultivation)
f.usv <- '~/PROJECTS/Pollinator_services/input_data/INEGI_usodesuelo_2017/conjunto_de_datos/usv250s6n.shp'
polys_nom <- st_read(f.usv)
st_crs(st_geometry(polys_nom)) <- 6362 # set EPSG code

# Merge Nómada with Agriculture
polys_nom %<>% mutate(TIPAGES='AGRICULTURA NÓMADA', TIP_CUL1='NÓMADA') %>% 
  select(!NOM)
polys <- list(polys_ag, polys_nom) %>% mapedit:::combine_list_of_sf()

# Check numbers of polygons in each class (TIPAGES)
polys %>% 
  st_drop_geometry() %>%
  group_by(CLAVE) %>% 
  summarise(no_rows = length(CLAVE)) %>% 
  View()

saveRDS(polys, '~/GitHub/polinizadores/data_out/polys_ag_INEGI.rds')

#----
# Cultivo points from INEGI - all are Agricola-Pecuaria-Forestal
f.usv <- '~/PROJECTS/Pollinator_services/input_data/INEGI_usodesuelo_2017/conjunto_de_datos/usv250s6c.shp'
pts_cult <- st_read(f.usv)
st_crs(st_geometry(pts_cult)) <- 6362 # set EPSG code
colnames(pts_cult)
# Decode cultivos for most important pollinator crops
key <- read_csv('~/PROJECTS/Pollinator_services/input_data/INEGI_usodesuelo_2017/metadatos/INEGI_codes.csv')
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

#----
# Forestales points from INEGI - all are Agricola-Pecuaria-Forestal
f.usv <- '~/PROJECTS/Pollinator_services/input_data/INEGI_usodesuelo_2017/conjunto_de_datos/usv250s6f.shp'
pts_for <- st_read(f.usv)
st_crs(st_geometry(pts_for)) <- 6362 # set EPSG code
colnames(pts_for)

# Look at numbers of points per important crop
grps <- pts_for %>%
  group_by(Clave) %>% 
  summarise(no_rows = length(Clave)) %>% 
  View()

View(agri_nogeom %>% 
       filter(grepl('.+TEMPORAL', TIPAGES, ignore.case=TRUE)) %>% 
       group_by(CLAVE) %>% 
       summarise(no_rows = length(CLAVE)))
