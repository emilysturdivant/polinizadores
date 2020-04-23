# Convert Apendice 1 to a dataframe. It looks like this produces some errors. Better just to ask for 
# library(rgdal)
library(sf)
library(tidyverse)
library(magrittr)

#---
# Michoacán testing
f.munis <- '~/PROJECTS/Pollinator_services/input_data/context_Mexico/SNIB_divisionpolitica/muni_2018gw/muni_2018gw.shp'
f.stat.cult <- '~/PROJECTS/Pollinator_services/input_data/SIAP/Cierre_agricola_mun_2018.csv'
f.stat.agripro <- '~/PROJECTS/Pollinator_services/input_data/SIAP/AgriPro_Agricultura_Protegida_2015_porMunicipio.csv'
f.sup.frij.gran <- '~/PROJECTS/Pollinator_services/Michoacan_beta/ESA_PV_2015_NACIONAL.kml'
f.sup.frij.gran <- '~/PROJECTS/Pollinator_services/Michoacan_beta/ESA_PV_2015_NACIONAL.geojson'
f.usv.agri <- '~/PROJECTS/Pollinator_services/input_data/INEGI_usodesuelo_2017/conjunto_de_datos/usv250s6g.shp'

f.stat.cult <- 'http://infosiap.siap.gob.mx/gobmx/datosAbiertos/ProduccionAgricola/Cierre_agricola_mun_2018.csv'
f.stat.agripro <- 'http://infosiap.siap.gob.mx/gobmx/datosAbiertos/Sup_Agricul_Protegida/Agricultura_Protegida_2015.csv'
#f.sup.frij.gran <- 'http://infosiap.siap.gob.mx/gobmx/datosAbiertos/estimacion-superficie-agricola/ESA_PV2015_AGS.kmz'

#----
# Join SIAP data (Estadísticas agrícolas and Agricultura Protegida) to Municipios

# Load municipios
munis <- st_read(f.munis)
# Load SIAP tables
cultivos <- read.csv(f.stat.cult, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
agripro <- read.csv(f.stat.agripro, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

# Summarize crops by municipio
# For each Municipio, get percent of total planted area covered by each crop
agg_cultivos <- cultivos %>% 
  group_by(Nomestado, Nommunicipio, Nomcultivo) %>% 
  summarize(Sembrada=sum(Sembrada)) %>% 
  group_by(Nommunicipio) %>% 
  mutate(total_sembrada=sum(Sembrada), 
         porcentaje=Sembrada/total_sembrada)
pct_cult <- agg_cultivos %>% 
  select(Nomestado, Nommunicipio, Nomcultivo, porcentaje) %>% 
  spread(Nomcultivo, porcentaje)

colnames(pct_cult)
pct_cult_important <- pct_cult %>% 
  select(Nommunicipio, 
         contains(c("aguacate", 'jitomate', 'café','mango',
                    'sandía','manzana','melón', 'calabaza', 
                    'pepino', 'brócoli', 'cacao', 'zarzamora', 
                    'fresa', 'zanahoria', 'soya', 'frijol')))



# For each Municipio, get percent of total planted area covered by each crop
agg_agripro <- agripro %>% 
  group_by(NOM_EDO, NOM_MUN, CULTIVOS) %>% 
  summarize(area=sum(SUP_HAS)) %>% 
  group_by(NOM_MUN) %>% 
  mutate(total_area=sum(area), 
         porcentaje=area/total_area)
pct_agripro <- agg_agripro %>% 
  select(NOM_EDO, NOM_MUN, CULTIVOS, porcentaje) %>% 
  spread(CULTIVOS, porcentaje)

# Join
pct_agr <- pct_cult %>% 
  full_join(pct_agripro, 
            by=c('Nomestado'='NOM_EDO', 'Nommunicipio'='NOM_MUN'),
            suffix=c('',', protegida'))
colnames(pct_agr)
# There are MANY items listed for crops, especially for AgriPro, 
# where there is often a list of multiple crops





# Join cultivos percent agricultural area to municipios
munis_cult <- munis %>% 
  full_join(pct_cult, 
            by=c('NOM_ENT'='Nomestado', 'NOM_MUN'='Nommunicipio'))



#----
# From INEGI agricultura uso de suelo, extract SIAP superficie de frijol

#----
# Load SIAP superficie de frijol (two options)
# GeoJSON, converted from KMZ in QGIS using Expand HTML description field (KML Tool)
f.sup.frij.gran <- '~/PROJECTS/Pollinator_services/Michoacan_beta/ESA_PV_2015_NACIONAL.geojson'
sup.frij.gran <- st_read(f.sup.frij.gran)
sup.frij.gran[1]

# KML - KMZ converted to KML in QGIS
f.sup.frij.gran <- '~/PROJECTS/Pollinator_services/input_data/SIAP/ESA_PV2015_MICHq.kml'
sup.frij.gran <- st_read(f.sup.frij.gran)
sup.frij.gran$Cultivo <- sup.frij.gran$Description %>% 
  as.character() %>% 
  str_extract('Maíz grano|Sorgo grano|Trigo grano|Frijol')
sup.frij.gran %<>% select(Name, Cultivo, geometry)

# Extract Frijol
frijol <- sup.frij.gran %>% filter(Cultivo=='Frijol')
plot(frijol[2])

# Extract Granos (all except frijol)
granos <- sup.frij.gran %>% filter(!grepl('Frijol', Cultivo))
plot(granos[2])

# Simplify polygons
library(rmapshaper)
simplepolys <- rmapshaper::ms_simplify(input = sup.frij.gran) %>%
  st_as_sf()
plot(simplepolys[2]) # plot by Cultivo (2nd column)

#----
# Load INEGI uso de suelo
usv <- st_read(f.usv)
st_crs(st_geometry(usv)) <- 6362 # set EPSG code

# Filter TIP_INFO == AGRÍCOLA-PECUARIA-FORESTAL
usv.agri <- usv %>% filter(TIP_INFO == 'AGRÍCOLA-PECUARIA-FORESTAL')

#----
# Compare INEGI ag and SIAP select ag




#----
# Then we have superficie de frijol and of everything else. For the everything else, assign probabilities