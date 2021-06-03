# Create database to comply with first WWF deliverable
# Follows processing in prep_Quesada_GBIF_data.R

library("sf")
library("tidyverse")

df1_fp <- 'data/data_out/for_wwf/polinizadores_v1.csv'
df2_fp <- 'data/data_out/for_wwf/polinizadores_subgroups.csv'
crp_pllntrs_fp <- 'data/tidy/Quesada/crop_pllntrs_from_appendix2.csv'
gbif_df_fp <- 'data/input_data/GBIF/gbif_crop_pllntrs_slim.rds'

# Make base de datos de polinizadores de México ----
# List pre-processed pollinator point files
fps <- list.files('data/data_out/pollinator_points/no_duplicates', 'gpkg$', full.names = TRUE)

# Function to load points, strip geometry and count observations
load_for_bind <- function(fp) {
  pol_group <- basename(tools::file_path_sans_ext(fp))
  
  # Read
  df <- st_read(fp) %>% st_drop_geometry()
  df_ct <- df %>% count(species, genus, tribe, family, superfamily, order, class)
  df_grp <- df_ct %>% mutate(common_group = pol_group)
  
  return(df_grp)
}

# Load and bind pollinator species counts
df1 <- fps %>% map_dfr(load_for_bind) %>% rename(nobs_gbif = n)

# Save output dataframe
df1 %>% write_csv(df1_fp)

# ~ Add supplemental information ----
df1 <- read_csv(df1_fp)
supp_dir <- 'data/input_data/Quesada_bioclim_pol_y_cultivos'

# Abejas: Sociabilidad and Anidación ----
abejas_class_fp <- file.path(supp_dir, 'datos_puros', 'Abejas_especies_unicasODC.xlsx')

abeja_subgroups <- readxl::read_excel(abejas_class_fp, na = c("", "-"))
abeja_subgroups <- abeja_subgroups %>% 
  select(species, subgroup_a = Sociabilidad, subgroup_b = Anidacion) %>% 
  mutate(species = str_remove_all(species, "\\(.*\\) "))

df2 <- df1 %>% 
  left_join(abeja_subgroups, by = 'species')
  
# Check for non-matching rows
abejas <- df1 %>% filter(common_group == 'Abejas')

ab <- abeja_subgroups %>% 
  mutate(species = str_remove_all(species, "\\(.*\\) "))

# Are there any duplicates?
t0 <- nrow(ab) - nrow(distinct(ab, species))
t1 <- nrow(anti_join(abejas, ab, by = 'species'))
t2 <- nrow(anti_join(ab, abejas, by = 'species'))
if(t0 + t1 + t2 > 0) print('WARNING: something looks off')

# Mariposas: diurna o nocturna? ----
df2 <- df2 %>% 
  mutate(subgroup_a = case_when(
    
    common_group == 'Mariposas' & 
      superfamily %in% c('Papilionoidea', 'Hesperioidea', 'Hedyloidea') ~ 'diurna',
    
    common_group == 'Mariposas' ~ 'nocturna', 
    
    TRUE ~ subgroup_a
  )
  )

# Save output dataframe
df2 %>% write_csv(df2_fp)

# Appendix 2: polinizadores by crop ----
df2 <- read_csv(df2_fp)

# Separate multiple values for subgroups
df2b <- df2 %>% 
  separate_rows(subgroup_a, sep = "/") %>% 
  mutate(subgroup_a = subgroup_a %>% 
           str_replace_all(regex("Solitarias", ignore_case = TRUE), "Solitaria") %>% 
           str_replace_all(regex("Sociales", ignore_case = TRUE), "Social") %>% 
           str_replace_all(regex("semisociales", ignore_case = TRUE), "semisocial"))

# Import CSV from Apendice2, created in prep_crop_pllntrs_apndx2.R 
ap2_df <- read_csv(crp_pllntrs_fp)

# Convert pollinator description to species names ----
extract_species_from_crops_table <- function(pol_df, crop_pols){
  
  # Filter pollinators list to only those in current pollinator points 
  pols_gen_filt <- semi_join(crop_pols, pol_df, by='genus')
  pols_tribe_filt <- semi_join(crop_pols, pol_df, by='tribe')
  pols_fam_filt <- semi_join(crop_pols, pol_df, by='family')
  pols_order_filt <- semi_join(crop_pols, pol_df, by='order')
  pols_subgrp_filt <- semi_join(crop_pols, pol_df, by=c('common_group', 'subgroup_a'))
  pols_grp_filt <- semi_join(crop_pols, pol_df, by=c('common_group'))
  pols_class_filt <- semi_join(crop_pols, pol_df, by='class')
  
  # If there are no matching genera or families, stop function (move to next file)
  if(all(nrow(pols_gen_filt) < 1, nrow(pols_fam_filt) < 1, 
         nrow(pols_order_filt) < 1, nrow(pols_class_filt) < 1)) return()
  
  # Separate pollinators into those with species match vs. those with genus match
  
  # Get pollinators with species match
  pols_specs_filt <- crop_pols %>% 
    semi_join(pol_df, by = 'species')
  
  # Pollinators with genus match
  pols_gen_filt <- pols_gen_filt %>% 
    anti_join(pols_specs_filt)
  
  # Pollinators with family match
  pols_tribe_filt <- pols_tribe_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt)
  
  # Pollinators with family match
  pols_fam_filt <- pols_fam_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt) %>% 
    anti_join(pols_tribe_filt)
  
  # Pollinators with species match
  pols_order_filt <- pols_order_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt) %>% 
    anti_join(pols_tribe_filt) %>% 
    anti_join(pols_fam_filt)
  
  # Pollinators with subgroup match
  pols_subgrp_filt <- pols_subgrp_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt) %>% 
    anti_join(pols_tribe_filt) %>% 
    anti_join(pols_fam_filt) %>% 
    anti_join(pols_order_filt)
  
  # Pollinators with group match
  pols_grp_filt <- pols_grp_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt) %>% 
    anti_join(pols_tribe_filt) %>% 
    anti_join(pols_fam_filt) %>% 
    anti_join(pols_order_filt) %>% 
    anti_join(pols_subgrp_filt)
  
  # Pollinators with class match
  pols_class_filt <- pols_class_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt) %>% 
    anti_join(pols_tribe_filt) %>% 
    anti_join(pols_fam_filt) %>% 
    anti_join(pols_order_filt) %>% 
    anti_join(pols_subgrp_filt) %>% 
    anti_join(pols_grp_filt)
  
  # Dissolve points by species
  pts_diss <- pol_df %>% distinct()
  
  # Filter points conditionally to genus or species
  out_pts <- pts_diss %>% 
    dplyr::filter(
      genus %in% pols_gen_filt$genus | 
        species %in% pols_specs_filt$species |
        tribe %in% pols_tribe_filt$tribe |
        family %in% pols_fam_filt$family |
        common_group %in% pols_grp_filt$common_group |
        subgroup_a %in% pols_subgrp_filt$subgroup_a |
        class %in% pols_class_filt$class
    )
  
  # Return filename
  return(out_pts)
}

# Get list of pollinators for all crops
crop_pols <- ap2_df %>% 
  count(species, genus, tribe, family, order, class, common_group, subgroup_a)

pol_species <- extract_species_from_crops_table(df2b, crop_pols)

# Save file
pol_species %>% write_csv('data/data_out/for_wwf/polinizadores_filtered_to_crops.csv')

pol_sp <- pol_species %>% 
  mutate(crop_documented = TRUE) %>% 
  select(-subgroup_a) %>% 
  distinct()

df3 <- df2 %>% left_join(pol_sp)

df3_fp <- 'data/data_out/for_wwf/base_polinizadores.csv'
df3 %>% write_csv(df3_fp)

df3 <- read_csv(df3_fp)
# NEXT ----
# for each species count how many crops it pollinates
crop_pols <- ap2_df %>% 
  count(species, genus, tribe, family, order, class, common_group, subgroup_a)

pol_df <- df2b

# Convert pollinator description to species names
extract_species_from_crops_table <- function(pol_df, crop_pols){
  
  # Filter pollinators list to only those in current pollinator points 
  pols_join_spec <- full_join(select(crop_pols, species, n),
                              pol_df, 
                              by= 'species')
  pols_join_gen <- full_join(select(crop_pols, genus, n),
                              pol_df, 
                              by= 'genus')
  pols_join_fam <- full_join(select(crop_pols, family, n),
                             pol_df, 
                             by= 'family')
  
  # Try with row_patch
  pols_patch_spec <- rows_patch(pols_join_spec, select(crop_pols, genus, n),
                              by= 'genus')
  
  pols_fam_filt <- semi_join(crop_pols, pol_df, by='family')
  pols_order_filt <- semi_join(crop_pols, pol_df, by='order')
  pols_subgrp_filt <- semi_join(crop_pols, pol_df, by=c('common_group', 'subgroup_a'))
  pols_grp_filt <- semi_join(crop_pols, pol_df, by=c('common_group'))
  pols_class_filt <- semi_join(crop_pols, pol_df, by='class')
  
  # List crop pollinators not found in pollinators df
  # pols_gen_filt <- anti_join(crop_pols, pol_df, by='genus')
  # pols_fam_filt <- anti_join(crop_pols, pol_df, by='family')
  # pols_order_filt <- anti_join(crop_pols, pol_df, by='order')
  # pols_subgrp_filt <- anti_join(crop_pols, pol_df, by=c('common_group', 'subgroup_a'))
  # pols_grp_filt <- anti_join(crop_pols, pol_df, by=c('common_group'))
  # pols_class_filt <- anti_join(crop_pols, pol_df, by='class')
  
  # If there are no matching genera or families, stop function (move to next file)
  if(all(nrow(pols_gen_filt) < 1, nrow(pols_fam_filt) < 1, 
         nrow(pols_order_filt) < 1, nrow(pols_class_filt) < 1)) return()
  
  # Separate pollinators into those with species match vs. those with genus match
  
  # Get pollinators with species match
  pols_specs_filt <- crop_pols %>% 
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
  pts_diss <- pol_df %>% distinct()
  
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

pol_species <- extract_species_from_crops_table(df2, ap2_df)





# Load GBIF data ----
# Pre-processed in prep_GBIF_data_v2.R
pol_df <- readRDS(gbif_df_fp)
gbif_sp <- pol_df %>% distinct(species, genus, family, order)

# Compare pollinator datasets ----
quesada_sp <- read_csv('data/data_out/for_wwf/polinizadores_filtered_to_crops.csv')

# species that are not in Quesada
gbif_additionals <- gbif_sp %>% anti_join(quesada_sp, by='species')

# species that are not in GBIF
quesada_additionals <- quesada_sp %>% anti_join(gbif_sp, by='species')

# Compare to apendice2 data ----
c2p_df <- read_csv(crp_pllntrs_fp)

c2p_fams <- c2p_df %>% distinct(family)


c2p_fams %>% anti_join(quesada_sp)
c2p_fams %>% anti_join(gbif_sp)
