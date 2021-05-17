# Create tidy DF matching crops to pollinator species
# convert table from Apendice2, shared by Dr. Quesada 

# Load libraries
library("tidyverse")

# Functions ----
tidy_croppllntrs_tbl <- function(in_fp, out_fp=NULL) {
  
  # Load table and propogate crop and family down 
  df <- read_csv(in_fp, trim_ws=T) %>% 
    fill(Familia, Cultivo)
  
  # Extract taxonomic classification from the pollinator variable
  df <- df %>% 
    mutate(Polinizador = str_trim(Polinizador),
           species = Polinizador %>% 
             str_remove_all("s. str.") %>% 
             str_remove_all("\\(.*\\)") %>% 
             str_replace_all("\\s+", " ") %>% 
             str_extract("^[:upper:]\\w*\\s\\w*"),
           genus = str_extract(Polinizador, '^[:upper:]\\w*') %>% 
             str_remove_all("[:punct:]"), # assume first word is genus if starts with capital letter
           subgenus = Polinizador %>% 
             str_extract('(?<=\\()[:upper:]\\w+\\b'),
           family = str_extract(Grupo, '\\w*idae'),
           order = str_extract(Grupo, '\\w*ptera|\\w*formes'), 
           class = case_when(
             str_detect(Grupo, regex('aves', ignore_case = TRUE)) |
               str_detect(Polinizador, regex('aves', ignore_case = TRUE)) ~ 'Aves',
             str_detect(Polinizador, regex('insectos', ignore_case = TRUE)) ~ 'Insecta',
             TRUE ~ as.character(NA)), 
           common_group = case_when(
             str_detect(Polinizador, regex('abeja', ignore_case = TRUE)) ~ 'Abejas', 
             str_detect(Polinizador, regex('colibri', ignore_case = TRUE)) ~ 'Colibries', 
             str_detect(Polinizador, regex('mariposa', ignore_case = TRUE)) ~ 'Mariposas', 
             str_detect(Polinizador, regex('murci.+lago', ignore_case = TRUE)) ~ 'Murcielagos', 
             str_detect(Polinizador, regex('avispa', ignore_case = TRUE)) ~ 'Avispas', 
             str_detect(Polinizador, regex('mosca', ignore_case = TRUE)) ~ 'Moscas',
             TRUE ~ as.character(NA)),
           tribe = case_when(
             str_detect(Polinizador, regex('abejas sin aguij.+n', ignore_case = TRUE)) ~ 'Meliponini',
             TRUE ~ as.character(NA)),
           subgroup_a = case_when(
             str_detect(Polinizador, regex('abejas solitarias', ignore_case = TRUE)) ~ 'Solitaria',
             str_detect(Polinizador, regex('abejas sociales', ignore_case = TRUE)) ~ 'Social',
             TRUE ~ as.character(NA)))
  
  # Save file
  if(is.character(out_fp)) write_csv(df, out_fp)
  
  # Return
  return(df)
}

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

# Filepaths ----
ap1_xls_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice1.xlsx'
# crp_pllntrs_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice2.csv'
crp_pllntrs_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/crop_pllntrs_from_ap2_updated.csv'

# List crops with highest pollinator value ----
# Apendice1, crop production values and importance of pollination
# DD = uso directo y dependiente de polinizador
ap1_sheets <- list(
  readxl::read_excel(ap1_xls_fp, na = c('- -', '.'), .name_repair = 'universal'),
  readxl::read_excel(ap1_xls_fp, 2, na = c('- -', '.'), .name_repair = 'universal'),
  readxl::read_excel(ap1_xls_fp, 3, na = c('- -', '.'), .name_repair = 'universal'),
  readxl::read_excel(ap1_xls_fp, 4, na = c('- -', '.'), .name_repair = 'universal'))

# Convert to single DF
ap1_df <- plyr::join_all(ap1_sheets, by = 'Especie', type = 'full')
ap1_df <- ap1_df %>% mutate(Cultivo = standardize_crop_species(Especie))

# Get crops with highest pollinator value (Valor del polinizador) 
top10_ap1 <- ap1_df %>% 
  filter(str_detect(Importancia.de.la.polinización, 'DD|D\\?')) %>%
  arrange(desc(Valor.del.polinizador.en.2010..MEX..)) %>% 
  head(10) 

top10_ap1 %>% distinct(Cultivo)

# Get pollinator species for top crops ----

# Apendice2, crop to pollinator table
ap2_df <- tidy_croppllntrs_tbl(crp_pllntrs_fp) %>% 
  mutate(Cultivo = standardize_crop_species(Cultivo))

# Extract species of top crops from ap2
top10_pols <- semi_join(ap2_df, top10_ap1, by = "Cultivo")

# Look at genera listed for each crop
crop_list <- top10_pols %>% 
  group_by(Cultivo) %>% 
  distinct(genus)

# ~ GBIF download ----
