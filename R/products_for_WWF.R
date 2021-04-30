library("sf")
library("tidyverse")

# Make base de datos de polinizadores de México ----
# List pre-processed pollinator point files
fps <- list.files('data/data_out/pollinator_points/no_duplicates', 'gpkg$', full.names = TRUE)

# Function to load points, strip geometry and count observations
load_for_bind <- function(fp) {
  pol_group <- basename(file_path_sans_ext(fp))
  
  # Read
  df <- st_read(fp) %>% st_drop_geometry()
  df_ct <- df %>% count(species, genus, family, superfamily, order, class)
  df_grp <- df_ct %>% mutate(common_group = pol_group)
  
  return(df_grp)
}

# Load and bind pollinator species counts
df <- fps %>% map_dfr(load_for_bind)
df <- df %>% rename(nobs_gbif = n)

# Save output dataframe
df %>% write_csv('data/data_out/for_wwf/polinizadores_v1.csv')

# ~ Add supplemental information ----
supp_dir <- 'data/input_data/Quesada_bioclim_pol_y_cultivos'

# Abejas: Sociabilidad and Anidación ----
abejas_class_fp <- file.path(supp_dir, 'datos_puros', 'Abejas_especies_unicasODC.xlsx')

abeja_subgroups <- readxl::read_excel(abejas_class_fp, na = c("", "-"))
abeja_subgroups <- abeja_subgroups %>% 
  select(species, subgroup_a = Sociabilidad, subgroup_b = Anidacion) %>% 
  mutate(species = str_remove_all(species, "\\(.*\\) "))

df2 <- df %>% 
  left_join(abeja_subgroups, by = 'species')
  
# Check for non-matching rows
abejas <- df %>% 
  filter(common_group == 'Abejas')

ab <- abeja_subgroups %>% 
  mutate(species = str_remove_all(species, "\\(.*\\) "))

# Are there any duplicates?
ab %>% nrow()
ab %>% count(species) %>% nrow()

anti_join(abejas, ab, by = 'species')
anti_join(ab, abejas, by = 'species')

# Mariposas: diurna o nocturna? ----
df3 <- df2 %>% 
  mutate(subgroup_a = case_when(
    
    common_group == 'Mariposas' & 
      superfamily %in% c('Papilionoidea', 'Hesperioidea', 'Hedyloidea') ~ 'diurna',
    
    common_group == 'Mariposas' ~ 'nocturna', 
    
    TRUE ~ subgroup_a
  )
  )

# Save output dataframe
df3 %>% write_csv('data/data_out/for_wwf/polinizadores_subgroups.csv')

# Appendix 2: polinizadores by crop ----
# Import CSV from Apendice2
crop_to_pols_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice2.csv'

df <- read_csv(crop_to_pols_fp, trim_ws=T) %>% 
  fill(Familia, Cultivo) %>% 
  mutate(genus = str_extract(Polinizador, '^[:upper:]\\S*'), # assume first word is genus
         family = str_extract(Grupo, '\\S*idae'),
         order = str_extract(Grupo, '\\S*ptera'), 
         class = str_extract(Grupo, 'Aves'), 
         common_group = case_when(
           str_detect(Polinizador, 'abeja') ~ 'Abejas', 
           str_detect(Polinizador, 'colibri') ~ 'Colibries', 
           str_detect(Polinizador, 'mariposa') ~ 'Mariposas', 
           str_detect(Polinizador, 'murcielago') ~ 'Murcielagos', 
           str_detect(Polinizador, 'avispa') ~ 'Avispas', 
           str_detect(Polinizador, 'mosca') ~ 'Moscas',
           TRUE ~ as.character(NA)),
         subgroup_a = case_when(
           str_detect(Polinizador, 'abejas solitarias') ~ 'Solitaria',
           TRUE ~ as.character(NA)))

# Get list of pollinators for all crops
crop_pols <- df %>% 
  count(Polinizador, genus, family, order, class, common_group, subgroup_a)

# Convert pollinator description to species names
extract_species_from_crops_table <- function(pol_df, crop_pols){
  
  # Filter pollinators list to only those in current pollinator points 
  pols_gen_filt <- semi_join(pols, pol_df, by='genus')
  pols_fam_filt <- semi_join(pols, pol_df, by='family')
  pols_order_filt <- semi_join(pols, pol_df, by='order')
  pols_subgrp_filt <- semi_join(pols, pol_df, by=c('common_group', 'subgroup_a'))
  pols_grp_filt <- semi_join(pols, pol_df, by=c('common_group'))
  pols_class_filt <- semi_join(pols, pol_df, by='class')
  
  # If there are no matching genera or families, stop function (move to next file)
  if(all(nrow(pols_gen_filt) < 1, nrow(pols_fam_filt) < 1, 
         nrow(pols_order_filt) < 1, nrow(pols_class_filt) < 1)) return()
  
  # Separate pollinators into those with species match vs. those with genus match
  
  # Get pollinators with species match
  pols_specs_filt <- pols %>% 
    semi_join(pol_df, by=c(Polinizador='species'))
  
  # Pollinators with genus match
  pols_gen_filt <- pols_gen_filt %>% 
    anti_join(pols_specs_filt)
  
  # Pollinators with family match
  pols_fam_filt <- pols_fam_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt)
  
  # Pollinators with species match
  pols_order_filt <- pols_order_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt) %>% 
    anti_join(pols_fam_filt)
  
  # Pollinators with subgroup match
  pols_subgrp_filt <- pols_subgrp_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt) %>% 
    anti_join(pols_fam_filt) %>% 
    anti_join(pols_order_filt)
  
  # Pollinators with group match
  pols_grp_filt <- pols_grp_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt) %>% 
    anti_join(pols_fam_filt) %>% 
    anti_join(pols_order_filt) %>% 
    anti_join(pols_subgrp_filt)
  
  # Pollinators with class match
  pols_class_filt <- pols_class_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt) %>% 
    anti_join(pols_fam_filt) %>% 
    anti_join(pols_order_filt) %>% 
    anti_join(pols_subgrp_filt) %>% 
    anti_join(pols_grp_filt)
  
  # Dissolve points by species
  pts_diss <- pol_df %>% 
    distinct(species, genus, family, order, class, common_group, subgroup_a)
  
  # Filter points conditionally to genus or species
  out_pts <- pts_diss %>% 
    dplyr::filter(
      genus %in% pols_gen_filt$genus | 
        species %in% pols_specs_filt$Polinizador |
        family %in% pols_fam_filt$family |
        common_group %in% pols_grp_filt$common_group |
        subgroup_a %in% pols_subgrp_filt$subgroup_a |
        class %in% pols_class_filt$class
    )
  
  # Return filename
  return(out_pts)
}

pol_species <- extract_species_from_crops_table(df3, crop_pols)

# Save file
pol_species %>% saveRDS('data/data_out/for_wwf/polinizadores_filtered_to_crops.csv')

# NEXT ----
# for each species count how many crops it pollinates