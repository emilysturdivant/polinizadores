
# Load libraries ----
library(sf)
library(tidyverse)
library(tools)
library(mapview)
library(units)

devtools::install_github("ropensci/rgbif")
library(rgbif)

occ_count(taxonKey = 2433207, georeferenced = T)
mex_code <- isocodes[grep("Mexico", isocodes$name), "code"]
occ_count(country=mex_code)

taxrank()
out <- name_lookup(query = 'apis')
names(out)
out$meta
head(out$data)
out$facets
out$hierarchies[1:2]
out$names

head(name_lookup(query='Apis', rank="genus", return="data"))
head(name_lookup(query='Apis mellifera', rank="species", return="data"))

library("plyr")
out <- name_usage(key=2433207, language="SPANISH", data='vernacularNames')
head(out$data)

name_backbone(name='Apis', rank='genus', kingdom='Insecta')
head( name_suggest(q='Apis mellifera') )
head( name_suggest(q='Choeronycteris') )

out <- occ_get(key=c(2433206, 2433207, 8339697), return='data')

out <- name_suggest(q='Choeronycteris mexicana', rank='species')$data
key <- out$key[1]
occ_search(taxonKey=key, country=mex_code, return='meta')

pols <- readxl::read_excel(
  'data/input_data/Quesada_bioclim_pol_y_cultivos/Informacion_general.xlsx', 
  sheet='Polinizadores', skip=1)
species <- pols %>% select(Especie) %>% deframe



