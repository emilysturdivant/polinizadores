# Create database to comply with first WWF deliverable
# Follows processing in prep_Quesada_GBIF_data.R

# Load libraries ----
library("sf")
library("tidyverse")

# Initialize ----
df1_fp <- 'data/data_out/for_wwf/polinizadores_v2.csv'
df2_fp <- 'data/data_out/for_wwf/polinizadores_subgroups_v2.csv'
crp_pllntrs_fp <- 'data/tidy/Quesada/crop_pllntrs_updated_top10.csv'
gbif_df_fp <- 'data/input_data/GBIF/gbif_top10crops_genera_extra_ranks.rds'

# Make base de datos de polinizadores de México ----
# Load
df1 <- readRDS(gbif_df_fp)

# ~ Add supplemental information -----------------------------------------------
supp_dir <- 'data/input_data/Quesada_bioclim_pol_y_cultivos'
abejas_class_fp <- file.path(supp_dir, 'datos_puros', 'Abejas_especies_unicasODC.xlsx')

# Abejas: Sociabilidad and Anidación ----
abeja_sub1 <- readxl::read_excel(abejas_class_fp, na = c("", "-"))

# Join bee sociabilidad and anidacion to DF by genus ----
abeja_supp_species <- abeja_sub1 %>% 
  distinct(species, subgroup_a = Sociabilidad, subgroup_b = Anidacion)

# Join
df2_spec <- df1 %>% 
  left_join(abeja_supp_species, by = 'species')

# # Check for non-matching rows
# abejas <- df1 %>% filter(superfamily == 'Apoidea')
# 
# # Are there any duplicates?
# t0 <- nrow(abeja_supp_species) - nrow(distinct(abeja_supp_species, species)) # duplicates?
# missing_from_supp <- anti_join(abejas, abeja_supp_species, by = 'species')
# missing_from_df <- anti_join(abeja_supp_species, abejas, by = 'species')
# 
# if(t0 + nrow(missing_from_supp) + nrow(missing_from_df) > 0) print('WARNING: something looks off')

# Join bee sociabilidad and anidacion to DF by genus ----
abeja_supp_genus <- abeja_sub1 %>% 
  distinct(genus, subgroup_a = Sociabilidad, subgroup_b = Anidacion)

abeja_supp_genus <- abeja_supp_genus %>% 
  group_by(genus) %>% 
  filter(n() < 2)

# Join
df2_gen <- df1 %>% 
  left_join(abeja_supp_genus, by = 'genus')

# Merge the species version and the genus version
df2_gen <- df2_gen %>% filter(!is.na(subgroup_a) | !is.na(subgroup_b))
df2 <- df2 %>% filter(!is.na(subgroup_a) | !is.na(subgroup_b))

df2_combo <- bind_rows(df2_gen, df2)
df2_combo <- df2_combo %>% distinct()

df2_combo <- df2_gen %>% rows_update(df2, by = 'species')

df2_combo <- df2 %>% rows_patch(df2_gen, by = 'genus')

# Join bee sociabilidad and anidacion to DF by genus ----
abeja_subgroups <- abeja_sub1 %>% 
  distinct(genus, subgroup_a = Sociabilidad, subgroup_b = Anidacion)

# Join
df2 <- df1 %>% 
  left_join(abeja_subgroups, by = 'genus')
  
# Check for non-matching rows
abejas <- df1 %>% filter(superfamily == 'Apoidea')

# Are there any duplicates?
t0 <- nrow(abeja_subgroups) - nrow(distinct(abeja_subgroups, genus))
t1 <- nrow(anti_join(abejas, abeja_subgroups, by = 'genus')) # species in DF that aren't in 
t2 <- nrow(anti_join(abeja_subgroups, abejas, by = 'genus'))
if(t0 + t1 + t2 > 0) print('WARNING: something looks off')

ab_ct <- abeja_subgroups %>% count(genus)

ab_species_specific <- abeja_sub1 %>% 
  distinct(species, genus, Sociabilidad, Anidacion) %>% 
  semi_join(filter(ab_ct, n > 1), by = 'genus')

# Mariposas: diurna o nocturna? ----
df2 <- df2 %>% 
  mutate(subgroup_a = case_when(
    
    order == 'Lepidoptera' & 
      superfamily %in% c('Papilionoidea', 'Hesperioidea', 'Hedyloidea') ~ 'diurna',
    
    order == 'Lepidoptera' ~ 'nocturna', 
    
    TRUE ~ subgroup_a
  )
  )

# Save output dataframe
df2 %>% write_csv(df2_fp)

# Appendix 2: polinizadores by crop --------------------------------------------
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
