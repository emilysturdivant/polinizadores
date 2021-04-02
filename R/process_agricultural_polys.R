
# Load libraries ----
library(sf)
library(tools)
# library(mapview)
# library(tmap)
# library(units)
library(stringdist)
library(tidyverse)
# library(magrittr)
# library(ggplot2)
# theme_set(theme_minimal())
# library(ggnewscale)


# Load pre-created objects (from prep_SIAP_data.R)
# load('data/helpers/initial_vars.RData')
# load('data/helpers/functions.RData')
# box::use(R/process_SDM[model_species_rf, predict_distribution_rf, stack_sdms])
box::use(R/functions[model_species_rf, predict_distribution_rf, stack_sdms])
crops_dir <- 'data/data_out/polys_ag_INEGI_wFMG_pcts/pcts_by_state'
ag_by_crop_dir <- 'data/data_out/polys_ag_INEGI_wFMG_pcts/specific_crops'
pol_pt_dir <- 'data/data_out/pollinator_points'
crop_to_pols_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice2.csv'

dfilt_code <- 'alldates'

# Functions --------------------------------------------------------------------
convert_species_names <- function(fp, pols){
  
  # Load points and drop geometry
  pts_no_geom <- st_read(fp) %>% 
    st_drop_geometry()
  
  # Filter pollinators list to only those in current pollinator points 
  pols_gen_filt <- semi_join(pols, pts_no_geom, by='genus')
  pols_fam_filt <- semi_join(pols, pts_no_geom, by='family')
  pols_order_filt <- semi_join(pols, pts_no_geom, by='order')
  pols_class_filt <- semi_join(pols, pts_no_geom, by='class')
  
  # If there are no matching genera or families, stop function (move to next file)
  if(all(nrow(pols_gen_filt) < 1, nrow(pols_fam_filt) < 1, 
         nrow(pols_order_filt) < 1, nrow(pols_class_filt) < 1)) return()
  
  # Separate pollinators into those with species match vs. those with genus match
  # Get pollinators with species match
  pols_specs_filt <- pols %>% 
    semi_join(pts_no_geom, by=c(Polinizador='species'))
  
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
  
  # Pollinators with class match
  pols_class_filt <- pols_class_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt) %>% 
    anti_join(pols_fam_filt) %>% 
    anti_join(pols_order_filt)
  
  # Dissolve points by species
  pts_diss <- pts_no_geom %>% 
    distinct(species, genus, family, order)
  
  # Filter points conditionally to genus or species
  out_pts <- pts_diss %>% 
    filter(
      genus %in% pols_gen_filt$genus | 
        species %in% pols_specs_filt$Polinizador |
        family %in% pols_fam_filt$family
    )
  
  # Create file name
  name <- fp %>% 
    basename %>% 
    file_path_sans_ext
  
  out_pts <- out_pts %>% 
    mutate(pol_group = name)
  
  # Return filename
  return(out_pts)
}

filter_pts <- function(fp, pols, cultivo_dir){
  
  # Load points
  pts <- st_read(fp)
  
  # Get table without geometry
  pts_no_geom <- pts %>% 
    st_set_geometry(NULL)
  
  # Filter pollinators list to only those in current pollinator points 
  pols_gen_filt <- semi_join(pols, pts_no_geom, by='genus')
  pols_fam_filt <- semi_join(pols, pts_no_geom, by='family')
  pols_order_filt <- semi_join(pols, pts_no_geom, by='order')
  pols_class_filt <- semi_join(pols, pts_no_geom, by='class')
  
  # If there are no matching genera or families, stop function (move to next file)
  if(all(nrow(pols_gen_filt) < 1, nrow(pols_fam_filt) < 1, 
         nrow(pols_order_filt) < 1, nrow(pols_class_filt) < 1)) return()

  # Separate pollinators into those with species match vs. those with genus match
  # Get pollinators with species match
  pols_specs_filt <- pols %>% 
    semi_join(pts_no_geom, by=c(Polinizador='species'))
  
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
  
  # Pollinators with class match
  pols_class_filt <- pols_class_filt %>% 
    anti_join(pols_specs_filt) %>% 
    anti_join(pols_gen_filt) %>% 
    anti_join(pols_fam_filt) %>% 
    anti_join(pols_order_filt)
  
  # # Dissolve points by species
  pts_diss <- pts %>%
    group_by(species, genus, family, order) %>%
    summarize %>%
    ungroup
  
  pts_diss <- pts_no_geom %>% 
    distinct(species, genus, family, order)
  
  # Filter points conditionally to genus or species
  out_pts <- pts_diss %>% 
    filter(
      genus %in% pols_gen_filt$genus | 
        species %in% pols_specs_filt$Polinizador |
        family %in% pols_fam_filt$family
    )
  
  # Create file name
  name <- fp %>% 
    basename %>% 
    file_path_sans_ext
  fp_out <- file.path(cultivo_dir, str_c(
    name, '_', str_replace_all(cult, ' ', '_'), '.geojson')
  )
  
  # Write file
  out_pts %>% st_write(fp_out, delete_dsn=T, driver='GeoJSON')
  
  # Return filename
  return(out_pts)
}

get_pol_pts_for_crop <- function(fname, pol_pt_dir, cult){
  # Import CSV (sep=';')f rom Apendice2
  df <- read_delim(fname, delim=';', trim_ws=T) %>% 
    fill(Familia, Cultivo) %>% 
    mutate(genus = str_extract(Polinizador, '^[:upper:]\\S*'), # assume first word is genus
           family = str_extract(Grupo, '\\S*idae'),
           order = str_extract(Grupo, '\\S*ptera'), 
           class = str_extract(Grupo, 'Aves'))
  
  # test <- df %>% dplyr::select(Grupo) %>% distinct
  
  # Get list of pollinators for chosen crop
  (pols <- df %>% 
      filter(Cultivo == cult) %>% 
      dplyr::select(Polinizador, genus, family, order, class) )
  
  # Get the pollinator distribution ----
  # Create dir for files
  cultivo_dir <- file.path(pol_pt_dir, 'pollinators_by_crop', 
                           str_replace_all(cult, ' ', '_'))
  unlink(cultivo_dir, recursive=T)
  dir.create(cultivo_dir)
  
  # Get matching points from each pollinator group
  fps <- list.files(pol_pt_dir, pattern='.geojson', full.names = T)
  
  # Run for all relevant states
  out_pts <- tryCatch(
    fps %>% 
      map(filter_pts, pols, cultivo_dir) %>% 
      mapedit:::combine_list_of_sf(), 
    error = function(...){
      print("**********Error combining features.**************")
      list.files(cultivo_dir, full.names=T) %>% 
        map(st_read) %>% 
        mapedit:::combine_list_of_sf()
    }
  )
  
  if(missing(out_pts)){
    out_pts <- list.files(cultivo_dir, full.names=T) %>% 
      map(st_read) %>% 
      mapedit:::combine_list_of_sf()
  }
  
  # Save file
  fp_out <- file.path(pol_pt_dir, 'pollinators_by_crop', 
                      str_c(str_replace_all(cult, ' ', '_'), '.geojson'))
  out_pts %>% st_write(fp_out, delete_dsn=T, driver='GeoJSON')
}

get_crop_polys <- function(var, season, crops_dir, ag_by_crop_dir){
  # Get states where crop is grown ----
  # Load 
  area_cult_table <- readRDS(file.path("data/data_out/r_data",
                                       str_c('area_sembrada_', season, '_2019.RDS')))
  
  # Get matching crop name
  vars <- area_cult_table %>% colnames
  (var <- vars[[amatch(var, vars, maxDist=1)]])
  
  # Get states where crop is grown
  state_cves <- area_cult_table %>% 
    filter(across(all_of(var), ~ !is.na(.x))) %>% 
    select(CVE_ENT) %>% 
    distinct
  
  # Get state codes
  est_to_cve_tbl <- est_to_cve %>% 
    as_tibble(rownames = 'estado') %>% 
    filter(value %in% state_cves$CVE_ENT)
  
  # Get files matching state codes ----
  filter_polys <- function(fp, var, temp_dir){
    # Load polygons for state
    polys <- st_read(fp) %>% 
      mutate(total_noFMG = as.numeric(total_noFMG))
    
    # Get matching crop name
    vars <- polys %>% colnames
    print(var <- vars[[amatch(var, vars, maxDist=4)]])
    
    # Filter to crop of interest
    out_polys <- polys %>%
      filter(across(all_of(var), ~ !is.na(.x))) %>%
      select(CVE_ENT:total_noFMG, all_of(var))
    
    # Save
    (state <- fp %>% 
        basename %>% file_path_sans_ext %>% 
        str_split('_') %>% last %>% last )
    fp_out <- file.path(temp_dir, str_c(var, '_', state, '.geojson'))
    out_polys %>% st_write(fp_out, delete_dsn=T)
    
    # Return
    return(out_polys)
  }
  
  # Create temp dir
  temp_dir <- file.path(ag_by_crop_dir, 'temp')
  unlink(temp_dir, recursive=T)
  dir.create(temp_dir)
  
  # List files
  search_str <- str_c('.*', season, '.*(', 
                      str_c(est_to_cve_tbl$estado, collapse='|'), 
                      ')\\.geojson')
  fps <- list.files(crops_dir, pattern=search_str, full.names=T)
  
  # Test
  # fp <- fps[[1]]
  # polys_crop <- filter_polys(fp, var, temp_dir)
  
  # Run for all relevant states
  polys_all <- tryCatch(
    fps %>% 
      map(filter_polys, var, temp_dir) %>%
      mapedit:::combine_list_of_sf(), 
    error = function(...){
      print("\nError combining features.\n")
      list.files(temp_dir, pattern='.geojson', full.names = T) %>% 
        map(st_read) %>% 
        mapedit:::combine_list_of_sf()
    }
  )
  
  # Change probabilities variable name to be consistent
  (prob_name <- polys_all %>% colnames %>% nth(9))
  polys_all[['crop_prob']] <- polys_all[[prob_name]]
  
  # Calculate area of crop (instead of percentage)
  polys_all <- polys_all %>% 
    select(-9) %>% # select(matches(prob_name))
    mutate(total_noFMG = as.numeric(total_noFMG), 
           crop_area = total_noFMG * crop_prob)
  
  # Save
  fp_out <- file.path(ag_by_crop_dir, 
                      str_c(prob_name, '_', season, '.geojson'))
  polys_all %>% st_write(fp_out, delete_dsn=T)
  
  # Return
  return(polys_all)
}

# Load crop to pollinator table -------------------------------------------------------------
# 20_cultivos_mas_importantes_de_Mexico.xlsx lists the 20 cultivos with common name and SIAP variable name

# BaseCultivosAsworth_MXname.xlsx matches species to common name
# app1_cultivos also matches species to common name
cultivos_nombre_fp <- 'data/helpers/cultivos_to_MXname_Ashworth.xlsx'
cult_nom_df <- readxl::read_excel(cultivos_nombre_fp)

cultivos_importantes_fp <- 'data/helpers/20_cultivos_mas_importantes_de_Mexico.xlsx'
cult_df <- readxl::read_excel(cultivos_importantes_fp)
cult_df <- cult_df %>% 
  mutate(Especie = str_replace_all(Especie, 'Fragaria vesca', 'Fragaria ssp.'))

# for(i in seq(nrow(cult_df))){
i <- 10
(cult <- cult_df[[i, 'Especie']])
(var <- cult_df[[i, 'Cultivo']])
(safe_var_name <- cult_df[[i, 'Nombre_SIAP']])

# ~ Get pollinator richness for given crop ---------------------------------------
cultivo_dir <- file.path(pol_pt_dir, 'sdms_by_crop', str_replace_all(cult, ' ', '_'))
pol_species_fp <- file.path(cultivo_dir, 'pollinator_species.rds')

if(!file.exists(pol_species_fp)){
  # Import CSV from Apendice2
  df <- read_csv(crop_to_pols_fp, trim_ws=T) %>% 
    fill(Familia, Cultivo) %>% 
    mutate(genus = str_extract(Polinizador, '^[:upper:]\\S*'), # assume first word is genus
           family = str_extract(Grupo, '\\S*idae'),
           order = str_extract(Grupo, '\\S*ptera'), 
           class = str_extract(Grupo, 'Aves'))
  
  # List of unique crops in appendix
  crops <- df %>% dplyr::select(Cultivo) %>% distinct %>% deframe
  
  # Get crop name as listed in appendix
  idx <- amatch(cult, crops, maxDist=1)
  if(is.na(idx)){
    print(str_glue('***WARNING*** "{cult}" ({var}) not found in Appendix table.'))
  }
  (cult <- crops[[idx]])
  
  # Get list of pollinators for chosen crop
  (crop_pols <- df %>% 
      filter(Cultivo == cult) %>% 
      dplyr::select(Polinizador, genus, family, order, class) )
  
  # Create dir for files
  dir.create(cultivo_dir, recursive=T, showWarnings = F)
  
  # Get species names from each pollinator group
  # fps <- list.files(
  #   file.path(pol_pt_dir, 'no_duplicates', str_c(dfilt_code, '_gt24perSpecies')), 
  #   pattern='gpkg$', full.names = T)
  fps <- list.files(
    file.path(pol_pt_dir, 'no_duplicates'),
    pattern='gpkg$', full.names = T)
  
  pol_species <- fps %>% 
    map_dfr(convert_species_names, crop_pols) %>% 
    tibble() %>% 
    mutate(sp_nospc = str_replace(species, ' ', '_'))
  
  # Save file
  pol_species %>% saveRDS(pol_species_fp)
  
} else {
  pol_species <- readRDS(pol_species_fp)
  
}

# Initialize richness processing ----
unq_cells = TRUE
mutually_exclusive_pa = TRUE
filt_dates = FALSE

# Mexico
mex <- raster::getData('GADM', country='MEX', level=1,
               path='data/input_data/context_Mexico') %>% 
  st_as_sf()

# Load environment variables 
crop_dir <- file.path('data', 'input_data', 'environment_variables', 'cropped')
predictors <- raster::stack(list.files(crop_dir, 'tif$', full.names=T))

# Remove layers from predictors
drop_lst <- c('biomes_CVEECON2', 'biomes_CVEECON1', 'biomes_CVEECON4',
              'ESACCI.LC.L4.LC10.Map.10m.MEX_2016_2018', 
              'usv250s6gw_USV_SVI')
pred <- predictors[[- which(names(predictors) %in% drop_lst) ]]

# directory paths
unq_code <- ifelse(unq_cells, 'unq_cells', 'unq_pts')
unq_code <- ifelse(mutually_exclusive_pa, 'unq_cells_exclusive', unq_code)
dfilt_code <- ifelse(filt_dates, '2000to2020', 'alldates')
parent_pred_dir <- file.path('data', 'data_out', 'sdm', str_c(unq_code, '_', dfilt_code), 'rf1')
pol_dir <- 'data/data_out/pollinator_points/no_duplicates'

# Get filepaths for the different outputs for each species
pol_df <- st_read(file.path(pol_dir, 'combined', str_c(dfilt_code, '_gt24perSpecies.gpkg')))
pol_sp_fps <- pol_species %>% 
  mutate(model_fp = file.path(parent_pred_dir, pol_group, 'models', str_c(sp_nospc, '.rds')), 
         model_eval_fp = file.path(parent_pred_dir, pol_group, 'model_evals', str_c(sp_nospc, '.csv')),
         model_erf_fp = file.path(parent_pred_dir, pol_group, 'model_evals', str_c(sp_nospc, '.rds')),
         lik_tif_fp = file.path(parent_pred_dir, pol_group, 'likelihood', str_c(sp_nospc, '.tif')),
         pa_tif_fp = file.path(parent_pred_dir, pol_group, 'binned_spec_sens', str_c(sp_nospc, '.tif')),
         varimp_png_fp = file.path('figures', 'sdm', str_c(unq_code, '_', dfilt_code), 
                                   'rf1', pol_group, 'var_importance', 
                                   str_c(sp_nospc, '.png'))) %>% 
  # Filter species/file paths to only those for which we have usable data
  semi_join(st_drop_geometry(pol_df))

# Create and evaluate model (if not already created) ----
for( i in 1:nrow(pol_sp_fps)) {
  model_fp <- pol_sp_fps[[i, 'model_fp']]
  sp_name <- pol_sp_fps[[i, 'species']]
  eval_fp <- pol_sp_fps[[i, 'model_eval_fp']]
  erf_fp <- pol_sp_fps[[i, 'model_erf_fp']]
  likelihood_fp <- pol_sp_fps[[i, 'lik_tif_fp']]
  binned_fp <- pol_sp_fps[[i, 'pa_tif_fp']]
  varimp_fp <- pol_sp_fps[[i, 'varimp_png_fp']]
  
  if (file.exists(model_fp)) {
    print(str_c(sp_name, ': Model already created.'))
    # next
  } else {
    # Filter to species
    sp_df <- pol_df %>% filter(species == sp_name)
    
    if(nrow(sp_df) < 1) {
      print(str_c(sp_name, ':No rows for the given species.'))
      next
    } 
    
    # Model
    print(str_c(sp_name, ': Modelling...\n'))
    erf <- model_species_rf(sp_df,
                            pred, 
                            model_fp, 
                            sp_name,
                            eval_fp,
                            erf_fp,
                            mutually_exclusive_pa,
                            unq_cells,
                            varimp_fp)
  }
  
  # Filepaths  
  if(file.exists(likelihood_fp) & file.exists(binned_fp)){
    print(str_c(sp_name, ': Rasters already created.'))
    next
  } 
  
  # Make TIFs of likelihood and presence/absence
  print(str_c(sp_name, ': Creating prediction rasters... \n'))
  predict_distribution_rf(model_fp, erf_fp, likelihood_fp, binned_fp, extent(mex))
}

# Look at model statistics together
eval_csv_fps <- pol_sp_fps$model_eval_fp
eval_tbl_fp <- file.path(cultivo_dir, 
                         str_c('model_evals_', length(eval_csv_fps), 'species.csv'))
eval_tbl <- eval_csv_fps %>% purrr::map_dfr(read.csv)
eval_tbl %>% write_csv(eval_tbl_fp)

# Sum to richness ----
# Likelihood
sp_fps <- pol_sp_fps$lik_tif_fp

rich_fp_prefix <- file.path(cultivo_dir,
                              str_glue('Likhd_rich_{dfilt_code}_{length(sp_fps)}species'))
rich_plot_fp <-str_glue('{rich_fp_prefix}.png')
rich_tif_fp <- str_glue('{rich_fp_prefix}.tif')
  
stack_sdms(sp_fps, rich_tif_fp, rich_plot_fp, mex)
 
# Presence/Absence
sp_fps <- pol_sp_fps$pa_tif_fp

rich_fp_prefix <- file.path(cultivo_dir, 
                              str_glue('PA_rich_{dfilt_code}_{length(sp_fps)}species'))
rich_tif_fp <- str_glue('{rich_fp_prefix}.tif')
rich_plot_fp <-str_glue('{rich_fp_prefix}.png')

stack_sdms(sp_fps, rich_tif_fp, rich_plot_fp, mex)





















# ~ OLD - Get pollinator points for given crop -----------------------------------------
ap2_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice2.csv'

# get_pol_pts_for_crop(ap2_fp, pol_pt_dir, cult)

# Import CSV from Apendice2
df <- read_csv(ap2_fp, trim_ws=T) %>% 
  fill(Familia, Cultivo) %>% 
  mutate(genus = str_extract(Polinizador, '^[:upper:]\\S*'), # assume first word is genus
         family = str_extract(Grupo, '\\S*idae'),
         order = str_extract(Grupo, '\\S*ptera'), 
         class = str_extract(Grupo, 'Aves'))

# List of unique crops in appendix
crops <- df %>% select(Cultivo) %>% distinct %>% deframe

# Get crop name as listed in appendix
(cult <- crops[[amatch(cult, crops, maxDist=1)]])

# Get list of pollinators for chosen crop
(pols <- df %>% 
    filter(Cultivo == cult) %>% 
    dplyr::select(Polinizador, genus, family, order, class) )

# Get the pollinator distribution ----
# Create dir for files
cultivo_dir <- file.path(pol_pt_dir, 'pollinators_by_crop', 
                         str_replace_all(cult, ' ', '_'))
unlink(cultivo_dir, recursive=T)
dir.create(cultivo_dir)

# Get matching points from each pollinator group
fps <- list.files(pol_pt_dir, 'no_duplicates', pattern='.gpkg$', full.names = T)
out_pts <- filter_pts(fps[[1]], pols, cultivo_dir)

# Run for all relevant states
out_pts <- tryCatch(
  fps %>% 
    map(filter_pts, pols, cultivo_dir) %>% 
    mapedit:::combine_list_of_sf(), 
  error = function(...){
    print("**********Error combining features.**************")
    list.files(cultivo_dir, full.names=T) %>% 
      map(st_read) %>% 
      mapedit:::combine_list_of_sf()
  }
)

# Save file
fp_out <- file.path(pol_pt_dir, 'pollinators_by_crop', 
                    str_c(str_replace_all(cult, ' ', '_'), '.geojson'))
out_pts %>% st_write(fp_out, delete_dsn=T, driver='GeoJSON')

# Get pollinator richness for given crop ---------------------------------------
ap2_fp <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice2.csv'

# Import CSV from Apendice2
df <- read_csv(ap2_fp, trim_ws=T) %>% 
  fill(Familia, Cultivo) %>% 
  mutate(genus = str_extract(Polinizador, '^[:upper:]\\S*'), # assume first word is genus
         family = str_extract(Grupo, '\\S*idae'),
         order = str_extract(Grupo, '\\S*ptera'), 
         class = str_extract(Grupo, 'Aves'))

# List of unique crops in appendix
crops <- df %>% select(Cultivo) %>% distinct %>% deframe

# Get crop name as listed in appendix
(cult <- crops[[amatch(cult, crops, maxDist=1)]])

# Get list of pollinators for chosen crop
(pols <- df %>% 
    filter(Cultivo == cult) %>% 
    dplyr::select(Polinizador, genus, family, order, class) )

# Get pollinator richness ----
# Create dir for files
cultivo_dir <- file.path(pol_pt_dir, 'sdms_by_crop', 
                         str_replace_all(cult, ' ', '_'))
dir.create(cultivo_dir, recursive=T, showWarnings = F)

# Get species names from each pollinator group
fps <- list.files(pol_pt_dir, pattern='gpkg$', full.names = T)
out_pts <- fps %>% 
  map_dfr(convert_species_names, pols) %>% 
  mutate(sp_nospc = str_replace(species, ' ', '_'))

# Save file
fp_out <- file.path(cultivo_dir, 'pollinator_species.rds')
out_pts %>% saveRDS(fp_out)

# Input pollinator richness 
unq_code <- 'unq_cells_exclusive'
dfilt_code <- 'alldates'
pred_dir <- here::here('data', 'data_out', 'sdm', 
                       str_c(unq_code, '_', dfilt_code), 'rf1')

# List all available rasters
fps <- list.files(pred_dir, pattern='tif$', recursive=T, full.names=T) %>% 
  str_subset('likelihood')

# Filter to only those that match the pollinator species list
rich_fps <- out_pts$sp_nospc %>% 
  map(~str_subset(.y, .x), fps) %>% 
  flatten_chr()







# Get crop polygons ------------------------------------------------------------
season <- 'otoperen'
polys_oto <- get_crop_polys(safe_var_name, season, crops_dir, ag_by_crop_dir)

season <- 'primperen'
polys_prim <- get_crop_polys(safe_var_name, season, crops_dir, ag_by_crop_dir)

# # Trying to fix problem with frijol
# temp_dir <- file.path(ag_by_crop_dir, 'temp')
# fix_col <- function(x){
#   st_read(x) %>% 
#     mutate(total_noFMG = as.numeric(total_noFMG))
# }
# polys_all <- list.files(temp_dir, pattern='.geojson', full.names = T) %>% 
#   map(fix_col)%>% 
#   mapedit:::combine_list_of_sf()


# Map --------------------------------------------------------------------------
# Load political boundaries
mex <- raster::getData('GADM', country='MEX', level=1, 
    path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) 

# Load crop polygons
season <- 'primperen'
fp_out <- file.path(ag_by_crop_dir, 
                    str_c(safe_var_name, '_', season, '.geojson'))
polys_prim <- st_read(fp_out)

# Load crop polygons
season <- 'otoperen'
fp_out <- file.path(ag_by_crop_dir, 
                    str_c(safe_var_name, '_', season, '.geojson'))
polys_oto <- st_read(fp_out)

# Load pollinator points
crop_fp <- file.path(pol_pt_dir, 'pollinators_by_crop', 
                     str_c(str_replace_all(cult, ' ', '_'), '.geojson'))
pol_pts <- st_read(crop_fp)

pol_pts <- pol_pts %>% filter(species != 'Apis mellifera')

# Plot with ggplot: pollinator point density and ag polygons in choropleth
(gm <- ggplot() +
    
    pol_pts %>% st_coordinates %>% as.data.frame %>% 
    stat_density2d(
      aes(x = X, y = Y, 
          fill = ..level..,  
          # alpha = ..level..
      ),
      size = 2, bins = 10, 
      data = .,
      geom = "polygon", 
      show.legend = T
    ) + 
    
    scale_colour_distiller(
      name='Densidad de polinizadores',
      type='seq', 
      palette='Greys',
      # palette='PuRd', 
      aesthetics='fill', direction=1
    ) +
    
    pol_pts %>%
    geom_sf(
      data=.,
      size=.1,
      color = gray(.2), 
      # color='magenta4', 
      alpha=.3) +
    
    geom_sf(data = mex, 
            color='lightgray', 
            fill=NA
    ) +
    
    new_scale_color() + 
    new_scale_fill() + 
    
    polys_oto %>%
    geom_sf(data = ., aes(fill=crop_prob, color=crop_prob),
            alpha=0.5) +
    
    polys_prim %>%
    geom_sf(data=., aes(fill=crop_prob, color=crop_prob),
            alpha=0.5,
            show.legend = FALSE) +
    
    # scale_colour_distiller(
    #     type='seq', palette='BuGn', direction=-1
    #   ) +
    scale_fill_viridis_c(name='Proporción del cultivo',
                         limits=c(0, 1), 
                         aesthetics=c('fill', 'color'), 
                         begin=0.35, end=1,
                         trans='sqrt', alpha = .4) +
    
    # theme(legend.direction = 'horizontal') + 
    
    labs(title = var,
         x = NULL, y = NULL)
)

ggsave(file.path('figures/crop_and_pollinator_exploration', 
                 str_c(var, '_noApis.png')), 
       width = 9.15, height=6.03)


(tm <- tm_shape(mex) +
    tm_borders(col='darkgray') + 
    tm_shape(polys_prim) +
    tm_fill(col = 'crop_prob', palette='YlGnBu', legend.show=T) +
    tm_shape(polys_oto) +
    tm_fill(col = 'crop_prob', palette='YlGnBu', legend.show=F) 
    # tm_shape(pol_pts) +
    # tm_dots(alpha=0.4, size=.1, col = 'family', palette=c('#b10026', '#0c2c84'), legend.show=T) 
    # tm_dots(alpha=0.4, size=.1, col = 'black') +
    # tm_facets(by = 'genus', free.coords=F)
    )
# tmap_save(tm, filename = file.path('figures', str_c('pol_', name, '_map_species_25.png')))

tmap_mode('view')
(tm <- 
    st_centroid(polys_prim) %>% 
    tm_shape(name='primavera') + tm_dots(
      alpha=0.8, size='crop_area', 
      col = 'crop_prob', palette='YlGnBu', 
      legend.show=T
      ) +

    st_centroid(polys_oto) %>% 
    tm_shape(name='otoño') + tm_dots(
      alpha=0.8, size='crop_area', 
      col = 'crop_prob', palette='YlGnBu', 
      legend.show=F
    ) +
    
    tm_shape(pol_pts) + 
      tm_dots(
        alpha=0.3, size=.01, 
        # clustering = T,
        legend.show=T
        ) 
)

tmap_mode('plot')
(tm <- 
    tm_shape(mex) + 
      # tm_fill(col='lightgray') +
      tm_borders(col='lightgray', lwd=0.5) +
    
    pol_pts %>% 
      tm_shape(name='Polinizador') + tm_dots(
        # alpha=0.4, size=.1, col = 'genus', palette=c('#b10026', '#0c2c84'), legend.show=T
        alpha=0.4, size=.04, col = '#666666',
        legend.show=T, title='Pollinators'
      ) +
    
    polys_prim %>% st_centroid() %>% 
      tm_shape(name='primavera') +
      tm_bubbles(
        size='crop_area', title.size='Crop area (ha)',
        col='#43AB5D', alpha=0.5,
        title.col='Probability of crop', 
        border.col='white', border.alpha=0.9, border.lwd=.5
      ) +
    
    polys_oto %>% st_centroid() %>% 
      tm_shape(name='otoño') +
      tm_bubbles(
        size='crop_area',  
        col = '#43AB5D', alpha=0.5,
        border.col='white', border.alpha=0.9, border.lwd=.5,
        legend.col.show=F, legend.size.show=F
      ) +
    
    tm_layout(title=prob_name, 
              title.position=c('right', 'top'),
              legend.position=c('left', 'bottom')
              )
)

