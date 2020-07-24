# Load libraries ----


# Agrícultural protegida ----
# agripro <- read.csv(f.stat.agripro, header=TRUE, fileEncoding="latin1")
f.stat.agripro <- str_c('http://infosiap.siap.gob.mx/gobmx/datosAbiertos/',
                        'Sup_Agricul_Protegida/Agricultura_Protegida_2015.csv')
agripro <- read_csv(f.stat.agripro, col_types = 'iciciciciccccinn', locale=locale(encoding='latin1'))

# Break cultivos into individual crops (Lechuga y espinaca)
agripro %>% select(CULTIVOS) %>% distinct %>% deframe
cults <- agripro %>% select(CULTIVOS) %>% distinct %>% sample_n(20)
c_split <- cults$CULTIVOS %>% str_split(' y |, ')
c_split[[1]]

t1 <- agripro %>% 
  # slice(232:233) %>% # for testing
  rowid_to_column %>% 
  mutate(CULTIVOS = str_to_lower(CULTIVOS)) %>% 
  separate_rows(CULTIVOS, sep=' y |, ') %>%
  group_by(rowid, TIPO, CULTIVOS, SUP_HAS, SUP_M2) %>% 
  summarize(sup_has = sum(!is.na(rowid))) %>% 
  ungroup %>% 
  select(TIPO, CULTIVOS, SUP_HAS, SUP_M2, rowid, sup_has) # for looking
  # select(CULTIVOS) %>% distinct %>% deframe

t1 %>% select(CULTIVOS) %>% distinct() %>% sample_n(20)

# Percent coverage per municipio
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


