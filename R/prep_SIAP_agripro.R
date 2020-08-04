# AgriPro shapefile from Jean Francois: http://lae.ciga.unam.mx/recursos/Agricultura_Protegida_2018.rar
# Load libraries ----
library(sf)
library(tidyverse)
library(magrittr)
library(rvest)
library(tools)
library(mapview)
library(units)

# Agrícultural protegida ----
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

