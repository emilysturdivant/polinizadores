# Get master list of the crops I'm interested in


# Load libraries
library('rgbif')
library('tidyverse')




# ----
# Fxn to standardize crop species names 
standardize_species <- function(x) {
  str_trim(x) %>% 
    str_to_sentence() %>% 
    str_replace_all('frutescen\\b', 'frutescens') %>% 
    str_replace_all('\\bannum\\b', 'annuum') %>% 
    str_replace_all('\\bparadisii\\b', 'paradisi') %>% 
    str_replace_all('Musa paradisiaca', 'Musa x paradisiaca') %>% 
    str_replace_all('\\bficus-.*indica\\b', 'ficus-indica') %>% 
    str_replace_all('pecten-.*aboriginum', 'pecten-aboriginum') %>% 
    str_replace_all('\\bliebmanii\\b', 'liebmannii') %>% 
    str_replace_all('Solanum lycopersicum \\(lycopersicon esculentum\\)', 'Lycopersicon esculentum') %>% 
    str_remove_all('\\(.*\\)') %>% 
    str_trim()
}

# Load all crop tables data ----
# Apendice2, crop to pollinator table
crp_pllntrs_fp <- 'data/tidy/Quesada/crop_pllntrs_from_appendix2.csv'
ap2_df <- read_csv(crp_pllntrs_fp) %>% 
  mutate(Cultivo = standardize_species(Cultivo))
crops_ap2 <- ap2_df %>% 
  distinct(Cultivo)

# Ashworth crop pollinator dependence, crop species, common names, dependence on pollinators
ashworth_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/BaseCultivosAsworth_MXname.csv'
ash_df <- read_delim(ashworth_fp, delim = ';') %>% 
  mutate(Cultivo = standardize_species(`Scientific name`))
crops_ash <- ash_df %>% select(Cultivo)

# Informacion_general, crop biological information
igcult_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/Info_general_cultivos.csv'
igcult_df <- read_delim(igcult_fp, delim = ';', skip = 1) %>% 
  mutate(Cultivo = standardize_species(Especie))
crops_igcult <- igcult_df %>% 
  select(Cultivo)

# Apendice1, crop production values and importance of pollination
# DD = uso directo y dependiente de polinizador
ap1_xls_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice1.xlsx'
ap1_sheets <- list(
  readxl::read_excel(ap1_xls_fp, na = '- -'),
  readxl::read_excel(ap1_xls_fp, 2, na = '- -'),
  readxl::read_excel(ap1_xls_fp, 3, na = '- -'),
  readxl::read_excel(ap1_xls_fp, 4, na = '- -'))

ap1_df <- plyr::join_all(ap1_sheets, by = 'Especie', type = 'full')
ap1_df <- ap1_df %>% mutate(Cultivo = standardize_species(Especie))
crops_ap1 <- ap1_df %>% select(Cultivo)

# compare to Info general
anti_join(crops_ap1, crops_igcult)
anti_join(crops_igcult, crops_ap1)

# merge
bound <- bind_rows(
  mutate(crops_ap2, source='ap2'),
  mutate(crops_ash, source = 'ashworth'), 
  mutate(crops_igcult, source = 'info_general'), 
  mutate(crops_ap1, source = 'info_general')
) %>% 
  arrange(Cultivo)

bound2 <- bound %>% group_by(Cultivo) %>% count() %>% ungroup %>% 
  arrange(Cultivo)
length(bound2[27,1] %>% deframe)




ash_missing <- anti_join(crops_ash, crops_ap2)
anti_join(ash_missing, crops_igcult)

anti_join(crops_igcult, crops_ap2)

anti_join(crops_igcult, crops_ash)

ap_missing <- anti_join(crops_ap2, crops_ash)
anti_join(ap_missing, crops_igcult)
anti_join(crops_ap2, crops_igcult)



# Join ap2 to ash_df and narrow to high-priority crops ----
# Which crops will be left out?
ashworth_wo_pllntrs <- anti_join(ash_df, ap2_df, by = 'Cultivo')

ashworth_pollinators <- left_join(ash_df, ap2_df, by = 'Cultivo')

