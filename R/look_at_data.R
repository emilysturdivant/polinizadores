# Load libraries ----
library(sf)
library(tidyverse)
library(magrittr)
library(rvest)
library(tools)
library(mapview)
library(units)

# Load data ----
# Initial variables/helpers: ag_dir, regions, fmg_dir, est_codes, states_by_region
load('data/helpers/initial_vars.RData') 
# Crop statistics: area_cult_peren, area_cult_prim, area_cult_otono, cultivos
load("data/data_out/r_data/area_sembrada_by_season_2019.RData") 
lookup <- readRDS('data/helpers/lookup_municipio_codes.rds')

# Cultivos: compare SIAP data to QRO SEDEA for QRO ----
# List columns
area_cult_prim %>% colnames
vars <- area_cult_prim %>% 
  select(-CVE_ENT:-total_sembrada) %>% 
  colnames()
# Extract
prim <- area_cult_prim %>%
  left_join(lookup, by=c('CVE_ENT', 'CVE_MUN')) %>% 
  filter(NOM_ENT == 'Querétaro', Idmodalidad == 'R') %>% 
  pivot_longer(all_of(vars), names_to='Cultivo', values_to='Sup_sembrada', values_drop_na=T)
prim %>% filter(CVE_MUN == 11) %>% arrange(Cultivo)
