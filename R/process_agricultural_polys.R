
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

# Remove frijol, maiz, granos from INEGI polygons (cross-check first)
# INEGI ----
inegi_polys <- st_read('data/data_out/polys_ag_INEGI_munis.geojson')

# Run per state ----
estado = 'VER'
# remove_FMG_from_ag_INEGI(estado, inegi_polys, fmg_dir)
# Separate by muni
remove_FMG_from_ag_INEGI_largefile(estado, inegi_polys, fmg_dir)

# Check results
# estado = 'MOR'
fp_out <- file.path(out_dir, str_c('inegi_noFMG_', estado, '.geojson'))
inegi_noFMG <- st_read(fp_out)
mapview(inegi_noFMG)

inegi_noFMG %>% filter(CVE_MUN == 16) %>% mapview

