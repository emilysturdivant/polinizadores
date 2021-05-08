# Create tidy DF matching crops to pollinator species
# convert table from Apendice2, shared by Dr. Quesada 

# Load libraries
library("tidyverse")

# Filepaths
apndx2_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice2.csv'
crp_pllntrs_fp <- 'data/tidy/Quesada/crop_pllntrs_from_appendix2.csv'

# Load table and propogate crop and family down 
df <- read_csv(apndx2_fp, trim_ws=T) %>% 
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
           str_detect(Polinizador, regex('abejas sin aguijón', ignore_case = TRUE)) ~ 'Meliponini',
           TRUE ~ as.character(NA)),
         subgroup_a = case_when(
           str_detect(Polinizador, regex('abejas solitarias', ignore_case = TRUE)) ~ 'Solitaria',
           str_detect(Polinizador, regex('abejas sociales', ignore_case = TRUE)) ~ 'Social',
           TRUE ~ as.character(NA)))

# Replace genus with that in parentheses
parens_df <- df %>% 
  filter(str_detect(Polinizador, "\\([:upper:].*\\)") & 
           !str_detect(Polinizador, "\\(\\d.*\\)"))  %>% 
  mutate(species = Polinizador %>% 
           str_remove_all("s. str.") %>% 
           str_remove_all("^.*\\(") %>%
           str_remove_all("\\)") %>% 
           str_replace_all("\\s+", " ") %>% 
           str_extract("^[:upper:]\\w*\\s\\w*"),
         genus = str_extract(species, '^[:upper:]\\w*') %>% 
           str_remove_all("[:punct:]"))

# Join parenthetical parts removed from parentheses
df <- bind_rows(df, parens_df)

# Save file
df %>% write_csv(crp_pllntrs_fp)
