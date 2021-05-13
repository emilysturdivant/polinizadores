# Get master list of the crops I'm interested in


# Load libraries
library('tidyverse')

# ----
# Fxn to standardize crop species names 
standardize_crop_species <- function(x) {
  str_trim(x) %>% 
    str_replace_all("\\s+", " ") %>% 
    str_to_sentence() %>% 
    str_replace_all('frutescen\\b', 'frutescens') %>% 
    str_replace_all('\\bannum\\b', 'annuum') %>% 
    str_replace_all('\\bparadisii\\b', 'paradisi') %>% 
    str_replace_all('Musa paradisiaca', 'Musa x paradisiaca') %>% 
    str_replace_all('\\bficus-.*indica\\b', 'ficus-indica') %>% 
    str_replace_all(regex('pecten-.*aboriginum', ignore_case = TRUE), 
                    'pecten-aboriginum') %>% 
    str_replace_all('\\bliebmanii\\b', 'liebmannii') %>% 
    str_replace_all('Solanum lycopersicum \\(lycopersicon esculentum\\)', 'Solanum lycopersicum') %>% 
    str_replace_all('Lycopersicon esculentum', 'Solanum lycopersicum') %>% 
    str_replace_all('Citrus (?=limon|aurantifolia|latifolia|tangelo)', 'Citrus x ') %>% 
    str_replace_all('Citrus x (?=paradisi|sinensis|reticulata)', 'Citrus ') %>% 
    str_replace_all('Mentha spicata l\\.', 'Mentha spicata') %>% 
    str_replace_all('Mentha viridis', 'Mentha spicata') %>% 
    str_replace_all('Nopalxochia phyllantoides', 'Nopalxochia phyllanthoides') %>% 
    str_replace_all('Lecythis sp\\.', 'Lecythis usitata') %>% 
    str_replace_all('Luma apiculata', 'Psidium sartorianum') %>% 
    str_replace_all('Macadamia .*', 'Macadamia spp.') %>% 
    str_replace_all('Mastichodendron .*', 'Sideroxylon palmeri') %>% 
    str_remove_all('\\(.*\\)') %>% 
    str_replace_all("\\s+", " ") %>% 
    str_trim()
}

# Apendice1, crop production values and importance of pollination ----
# DD = uso directo y dependiente de polinizador
ap1_xls_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice1.xlsx'
ap1_sheets <- list(
  readxl::read_excel(ap1_xls_fp, na = c('- -', '.'), .name_repair = 'universal'),
  readxl::read_excel(ap1_xls_fp, 2, na = c('- -', '.'), .name_repair = 'universal'),
  readxl::read_excel(ap1_xls_fp, 3, na = c('- -', '.'), .name_repair = 'universal'),
  readxl::read_excel(ap1_xls_fp, 4, na = c('- -', '.'), .name_repair = 'universal'))

ap1_df <- plyr::join_all(ap1_sheets, by = 'Especie', type = 'full')
ap1_df <- ap1_df %>% mutate(Cultivo = standardize_crop_species(Especie))
crops_ap1 <- ap1_df %>% select(Cultivo)

# Get crops with highest pollinator value (Valor del polinizador) 
top10_ap1 <- ap1_df %>% 
  filter(str_detect(Importancia.de.la.polinización, 'DD|D\\?')) %>%
  arrange(desc(Valor.del.polinizador.en.2010..MEX..)) %>% 
  head(10) 

# Apendice2, crop to pollinator table ----
crp_pllntrs_fp <- 'data/tidy/Quesada/crop_pllntrs_from_ap2_updated.csv'
ap2_df <- tidy_croppllntrs_tbl(crp_pllntrs_fp) %>% 
  mutate(Cultivo = standardize_crop_species(Cultivo))

# Extract species of top crops from ap2
top10_pols <- semi_join(ap2_df, top10_ap1, by = "Cultivo")
top10_pols %>% distinct(Cultivo)

# Look at genera listed for each crop
crop_list <- top10_pols %>% 
  group_by(Cultivo) %>% 
  distinct(genus)


# Join ap1 to ash_df and narrow to high-priority crops ----
# Which crops will be left out?
ap1_missing <- anti_join(ash_df, ap1_df, by = 'Cultivo')
ash_missing <- anti_join(ap1_df, ash_df, by = 'Cultivo')

# Join Ashworth to Apendice1
ap1_ash <- left_join(ap1_df, ash_df, by = 'Cultivo')

# Variables: 
ap1_ash %>% tbl_vars
dependent_df <- ap1_ash %>% 
  filter(str_detect(Importancia.de.la.polinización, 'DD|D\\?'))

# Get 20 crops with highest pollinator value (Valor del polinizador)
top10_ap1 <- dependent_df %>%
  arrange(desc(Valor.del.polinizador.en.2010..MEX..)) %>% 
  head(10) %>% 
  select(Cultivo, Nombre.común.en.español, Importancia.de.la.polinización, 
         `Level of pollinator dependence`, Valor.del.polinizador.en.2010..MEX.., 
         Valor.de.producción.en.2010..MEX.., 
         Valor.de.producción.esperada.al.2050.según.cambio.climático..MEX..)

# Extract species of top crops from ap2
top10_ap1 %>% anti_join(ap2_df, by = "Cultivo") %>% nrow
top10_pols <- semi_join(ap2_df, top10_ap1, by = "Cultivo")

# Look at genera listed for each crop
crop_list <- top10_pols %>% 
  group_by(Cultivo) %>% 
  distinct(genus)
crop <- crop_list[[1]]

top10_pols %>% distinct(Cultivo)


# Ashworth crop pollinator dependence, crop species, common names, dependence on pollinators
# E: essential (pollinator absence produce a reduction of > 90% of crop production compared with pollinator presence), 
# H: high (40-< 90% of reduction in absence of pollinators), 
# M: modest (10-< 40% of reduction), 
# L: little (>0-< 10 % of reduction),
# NI: no increase (no yield increase with animal mediated pollination).
ashworth_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/BaseCultivosAsworth_MXname.csv'
ash_df <- read_delim(ashworth_fp, delim = ';') %>% 
  mutate(Cultivo = standardize_crop_species(`Scientific name`))
crops_ash <- ash_df %>% select(Cultivo)

# Informacion_general, crop biological information
igcult_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/Info_general_cultivos.csv'
igcult_df <- read_delim(igcult_fp, delim = ';', skip = 1) %>% 
  mutate(Cultivo = standardize_crop_species(Especie))
crops_igcult <- igcult_df %>% 
  select(Cultivo)

# compare to Info general
anti_join(crops_ap1, crops_igcult)
anti_join(crops_igcult, crops_ap1)

# Compare the four crop lists ----
# merge
bound <- bind_rows(
  mutate(distinct(ap2_df, Cultivo), source='ap2'),
  mutate(crops_ash, source = 'ashworth'), 
  mutate(crops_igcult, source = 'info_general'), 
  mutate(crops_ap1, source = 'ap1')
) %>% 
  arrange(Cultivo)

bound2 <- bound %>% group_by(Cultivo) %>% count() %>% ungroup %>% 
  arrange(Cultivo)


# Random comparisons

ash_missing <- anti_join(crops_ash, crops_ap2)
anti_join(ash_missing, crops_igcult)

anti_join(crops_igcult, crops_ap2)

anti_join(crops_igcult, crops_ash)

ap_missing <- anti_join(crops_ap2, crops_ash)
anti_join(ap_missing, crops_igcult)
anti_join(crops_ap2, crops_igcult)



# Join ap2 to ash_df and narrow to high-priority crops 
# Which crops will be left out?
ashworth_wo_pllntrs <- anti_join(ash_df, ap2_df, by = 'Cultivo')

ashworth_pollinators <- left_join(ash_df, ap2_df, by = 'Cultivo')
