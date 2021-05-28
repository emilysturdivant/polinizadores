# Load libraries ----
library(sf)
library(tools)
library(maptools)
library(mapview)
library(units)
library(rgbif)
library(scrubr)
library(dismo)
library(raster)
library(stars)
library(tidyverse)
library(tmap)
tmap::tmap_mode('view')

# Initialize -----
# Mexico
mex <- getData('GADM', country='MEX', level=1,
               path='data/input_data/context_Mexico') %>% 
  st_as_sf()

# Date range
date_range <- c(2000, 2020)
pol_groups <- c('Abejas', 'Avispas', 'Colibries', 'Mariposas', 'Moscas', 'Murcielagos')

# Load environment variables ----
crop_dir <- file.path('data', 'input_data', 'environment_variables', 'cropped')
predictors <- raster::stack(list.files(crop_dir, 'tif$', full.names=T))

# Remove layers from predictors
drop_lst <- c('biomes_CVEECON2', 'biomes_CVEECON1', 'biomes_CVEECON4',
              'ESACCI.LC.L4.LC10.Map.10m.MEX_2016_2018', 
              'usv250s6gw_USV_SVI')
pred <- predictors[[- which(names(predictors) %in% drop_lst) ]]

# Set extent for testing
ext <- extent(mex)

#' @export
model_species_rf <- function(sp_df,
                             pred, 
                             model_fp, 
                             sp_name,
                             eval_fp,
                             erf_fp,
                             mutually_exclusive_pa=TRUE,
                             unq_cells=TRUE,
                             rf_fig_dir=NULL) {
  
  # Presence points
  
  # Convert to coords DF
  sp1 <- sp_df %>% 
    mutate(lon = unlist(map(.$geom, 1)), 
           lat = unlist(map(.$geom, 2))) %>% 
    st_drop_geometry() %>% 
    dplyr::select(lon, lat)
  
  # Background points
  pres_pts <- as_Spatial(sp_df)
  
  set.seed(10)
  if(mutually_exclusive_pa) {
    
    backg <- randomPoints(mask = pred, n=1000, p = pres_pts
                          # prob = T, # use mask as sampling bias grid
    )
  } else {
    
    backg <- randomPoints(pred, n=1000) 
  }
  
  backg <- backg %>% 
    as_tibble() %>% 
    rename(lon = 'x', lat = 'y')
  
  # Split presence into training and testing 
  set.seed(0)
  group <- kfold(sp1, 5)
  train_1 <- sp1[group != 1, ]
  test_1 <- sp1[group == 1, ]
  
  # Split background into training and testing
  set.seed(0)
  group <- kfold(backg, 5)
  train_0 <- backg[group != 1, ]
  test_0 <- backg[group == 1, ]
  
  # Extract environmental data 
  # Training dataset 
  train <- bind_rows(train_1, train_0)
  envtrain1 <- raster::extract(pred, train, cellnumbers=T, df=T)
  
  pb_train <- c(rep(1, nrow(train_1)), rep(0, nrow(train_0)))
  envtrain1 <- data.frame( cbind(pa = pb_train, envtrain1) ) %>% 
    mutate(across(starts_with(c('biomes', 'ESA', 'usv')), as.factor)) %>% 
    dplyr::select(-ID)
  
  # Remove duplicated cells
  if(unq_cells) {
    envtrain1 <- envtrain1 %>% distinct()
  }
  
  envtrain <- envtrain1 %>% dplyr::select(-cells)
  
  # Testing datasets - get predictors for test presence and background points
  testpres <- data.frame( raster::extract(pred, test_1) ) %>%
    mutate(across(starts_with(c('biomes', 'ESA', 'usv')), as.factor))
  
  testbackg <- data.frame( raster::extract(pred, test_0) ) %>%
    mutate(across(starts_with(c('biomes', 'ESA', 'usv')), as.factor))
  
  # Set factor levels for test DFs to match training data
  vars <- envtrain %>% dplyr::select(where(is.factor)) %>% tbl_vars
  for(var in vars){
    levels(testpres[[var]]) <- levels(envtrain[[var]])
    levels(testbackg[[var]]) <- levels(envtrain[[var]])
  }
  
  # Random forest model
  rf1 <- randomForest::randomForest(
    pa ~ . -pa,
    data = envtrain,
    na.action = na.exclude,
    importance=T, 
    ntree=1000)
  
  # Save
  dir.create(dirname(model_fp), recursive = T, showWarnings = F)
  saveRDS(rf1, model_fp)
  # rf1 <- readRDS(model_fp)
  
  # Filenames
  dir.create(dirname(eval_fp), recursive = T, showWarnings = F)
  
  # Evaluate model with test data
  erf <- dismo::evaluate(testpres, testbackg, rf1)
  
  # Save model evaluation
  saveRDS(erf, erf_fp)
  
  # Save simple statistics in CSV
  spc_eval <- tibble(
    species=sp_name,
    N_unq_pts=nrow(sp_df),
    N_unq_cells=nrow(filter(envtrain, pa == 1)),
    np=erf@np, na=erf@na, auc=erf@auc,
    cor=erf@cor, pcor=erf@pcor, 
    spec_sens=dismo::threshold(erf, "spec_sens"))
  
  spc_eval %>% write_csv(eval_fp)
  
  if(!is.null(rf_fig_dir)){
    # Get plot directory
    plot_fp <- file.path(rf_fig_dir, 'var_importance', str_c(sp_nospc, '.png'))
    dir.create(dirname(plot_fp), recursive = T, showWarnings = F)
    
    # Save plot
    png(plot_fp)
    randomForest::varImpPlot(rf1, type=1, sort=F, 
                             main=sp_name,
                             pt.cex=1,
                             bg='black')
    dev.off()
  }
  
  # Return
  return(erf)
}

#' @export
predict_distribution_rf <- function(rf_fp, erf_fp, likelihood_fp, binned_fp, ext){
  
  # Load model
  if(is.character(rf_fp)){
    rf1 <- readRDS(rf_fp)
  } else {
    rf1 <- rf_fp
  }
  
  # Create map and interpolate to fill holes
  pr_rf1 <- dismo::predict(predictors, rf1, ext=ext)
  pr_rf1 <- raster::focal(pr_rf1, 
                          w=matrix(1,nrow=3, ncol=3), 
                          fun=mean, 
                          NAonly=TRUE, 
                          na.rm=TRUE) 
  
  # Save likelihood raster
  dir.create(dirname(likelihood_fp), recursive = T, showWarnings = F)
  writeRaster(pr_rf1, likelihood_fp, overwrite=T, 
              options=c("dstnodata=-99999"), wopt=list(gdal='COMPRESS=LZW'))
  
  # Apply threshold from max TPR+TNR and save
  erf <- readRDS(erf_fp)
  tr <- threshold(erf, 'spec_sens')
  pa_rf1 <- pr_rf1 > tr
  
  # Save
  dir.create(dirname(binned_fp), recursive = T, showWarnings = F)
  writeRaster(pa_rf1, binned_fp, overwrite=T,
              options=c("dstnodata=-99999"), wopt=list(gdal='COMPRESS=LZW'))
}

#' @export
stack_sdms <- function(sp_fps, rich_tif_fp, rich_plot_fp, mex){
  
  pol_stack <- stack(sp_fps)
  
  # Sum layers and save 
  pol_rich <- sum(pol_stack, na.rm=T)
  pol_rich_msk <- mask(pol_rich, as_Spatial(mex))
  writeRaster(pol_rich_msk, rich_tif_fp, overwrite=T,
              options=c("dstnodata=-99999"), wopt=list(gdal='COMPRESS=LZW'))
  
  # Plot richness
  pol_rich_stars <- st_as_stars(pol_rich_msk)
  rich_plot  <- ggplot() +
    geom_stars(data=pol_rich_stars) +
    geom_sf(data = mex, fill = "transparent", size = 0.2, color = alpha("lightgray", 0.2)) +
    colormap::scale_fill_colormap(str_glue("Richness\n(N = {nlayers(pol_stack)})"), 
                                  na.value = "transparent", 
                                  colormap = colormap::colormaps$viridis) +
    ggthemes::theme_hc() +
    theme(legend.position=c(.95, 1), legend.title.align=0, legend.justification = c(1,1)) +
    labs(x = NULL, y = NULL)
  
  # Save
  ggsave(rich_plot_fp, rich_plot, width=9, height=5.7, dpi=120)
  
  # Facet individual PA maps
  # pa_facets  <- rasterVis::gplot(pol_stack) +
  #   geom_tile(aes(fill = value)) +
  #   colormap::scale_fill_colormap(str_glue("Occupancy likelihood"), na.value = "transparent",
  #                                 colormap = colormap::colormaps$viridis) +
  #   theme_minimal() +
  #   theme(legend.position='bottom') +
  #   labs(x = NULL, y = NULL) +
  #   coord_equal() +
  #   facet_wrap(~ variable, nrow=3)
  # 
  # # Save
  # ggsave(file.path(rf_fig_dir, str_glue('Likhd_{nlayers(pol_stack)}species.png')), pa_facets, width=9, height=5)
  
}

# ~ Standard random forest (from R-Spatial https://rspatial.org/raster/sdm) ----
unq_cells = TRUE
mutually_exclusive_pa = TRUE
filt_dates = FALSE
pol_group <- 'Mariposas'
nspecies <- 15

# directory paths
unq_code <- ifelse(unq_cells, 'unq_cells', 'unq_pts')
unq_code <- ifelse(mutually_exclusive_pa, 'unq_cells_exclusive', unq_code)
dfilt_code <- ifelse(filt_dates, '2000to2020', 'alldates')

fp_tail <- file.path('sdm', 'rf1', str_c(unq_code, '_', dfilt_code), pol_group)
pred_dir <- file.path('data', 'data_out', fp_tail)
rf_fig_dir <- file.path('figures', fp_tail)
dir.create(pred_dir, recursive=T, showWarnings = F)
dir.create(rf_fig_dir, recursive=T, showWarnings = F)

# Prep/tidying ----
# Rename files to include pollinator name
(fps <- list.files(pred_dir, 'tif$', full.name=T))
(pol_name <- basename(pred_dir))
(dfilt_code <- str_extract(pred_dir, '2000to2020|alldates'))
(new_fps <- fps %>% str_replace('richness_', str_glue('rich_{pol_name}_{dfilt_code}_')))
file.rename(fps, new_fps)

(fps <- list.files(rf_fig_dir, 'png$', full.name=T))
(pol_name <- basename(rf_fig_dir))
(dfilt_code <- str_extract(rf_fig_dir, '2000to2020|alldates'))
(new_fps <- fps %>% str_replace('richness_', str_glue('rich_{pol_name}_{dfilt_code}_')))
file.rename(fps, new_fps)

# Filter pollinator points to only those species with >= 25 unique observations
for(pol_group in pol_groups){
  pol_dir <- 'data/data_out/pollinator_points/no_duplicates'
  pol_fp_out <- file.path(pol_dir, str_c(dfilt_code, '_gt24perSpecies'), 
                          str_c(pol_group, '.gpkg'))
  
  if(!file.exists(pol_fp_out)){
    
    # Load all pollinator points
    pol_fp <- file.path(pol_dir, str_c(pol_group, '.gpkg'))
    pol_df1 <- st_read(pol_fp)
    
    # Optionally filter to date range
    if(filt_dates) {
      # Dates
      date_min <- as.POSIXct(str_c(date_range[[1]], "-01-01"))
      date_max <- as.POSIXct(str_c(date_range[[2]], "-12-31"))
      
      pol_df1 <- pol_df1 %>% 
        filter(eventDate >= date_min & eventDate <= date_max)
    }
    
    # Remove species with less than 25 distinct observations (based on Koch et al. 2017)
    pol_df2 <- pol_df1 %>% 
      distinct(species) %>%
      st_transform(st_crs(predictors)) %>% 
      group_by(species) %>% 
      filter(n() > 24) %>% 
      ungroup()
    
    # Save
    dir.create(dirname(pol_fp_out), recursive=TRUE, showWarnings =FALSE)
    pol_df2 %>% st_write(pol_fp_out)
  } 
}

pol_dir_out <- file.path(pol_dir, str_c(dfilt_code, '_gt24perSpecies'))
fps <- list.files(pol_dir_out, 'gpkg$', full.names=T)
combo <- fps %>% map_dfr(st_read)
combo %>% st_write(file.path(pol_dir, 'combined', str_c(dfilt_code, '_gt24perSpecies.gpkg')))

# Get list of species in Informacion_general 
# infogen <- readxl::read_excel('data/input_data/Quesada_bioclim_pol_y_cultivos/Informacion_general.xlsx', 
#                               'Polinizadores', skip=1)
# # infogen %>% distinct(Orden)
# # info_abejas <- infogen %>% filter(Orden == pol_order)
# info_sin <- infogen %>% 
#   filter(!is.na(Sinonimias)) %>% 
#   mutate(Especie = Sinonimias)
# infogen <- bind_rows(infogen, info_sin)
# sp_list <- infogen %>% distinct(Especie) %>% deframe

# Load pollinator points ----
pol_dir <- 'data/data_out/pollinator_points/no_duplicates'
pol_fp_out <- file.path(pol_dir, str_c(dfilt_code, '_gt24perSpecies'), 
                        str_c(pol_group, '.gpkg'))
pol_df2 <- st_read(pol_fp_out)

# TESTING ----
sp_list <- pol_species %>% 
  semi_join(distinct(st_drop_geometry(pol_df2), species)) %>% 
  dplyr::select(species) %>% deframe
sp_nospc_list <- str_replace(sp_list, ' ', '_')
(sp_name <- sp_list[[1]])

# Filepath
sp_nospc <- str_replace(sp_name, ' ', '_')
model_fp <- file.path(pred_dir, 'models', str_c(sp_nospc, '.rds'))

# Filter to species
sp_df <- pol_df2 %>% 
  filter(species == sp_name)

# box::use(R/process_SDM[model_species_rf, eval_species_rf])
erf <- model_species_rf(sp_df,
                        pred, 
                        model_fp, 
                        sp_name,
                        pred_dir,
                        mutually_exclusive_pa,
                        unq_cells,
                        rf_fig_dir)

# LOOP through species (15 most prolific) ----
sp_counts <- pol_df2 %>% st_drop_geometry %>% count(species) %>% arrange(desc(n))
sp_list <- sp_counts %>% slice(1:nspecies) %>% dplyr::select(species) %>% deframe
sp_nospc_list <- str_replace(sp_list, ' ', '_')

for(sp_name in sp_list) {
  
  print(sp_name)
  
  # Filepath
  sp_nospc <- str_replace(sp_name, ' ', '_')
  model_fp <- file.path(pred_dir, 'models', str_c(sp_nospc, '.rds'))
  if (file.exists(model_fp)) {
    print(str_c(sp_name, ': Model already created.'))
    next
  }
  
  # Filter to species
  sp_df <- pol_df2 %>% 
    filter(species == sp_name)
  
  if(nrow(sp_df) < 1) {
    print(str_c(sp_name, ':No rows for the given species.'))
    next
  }
  
  sp_nospc <- str_replace(sp_name, ' ', '_')
  eval_fp <- file.path(pred_dir, 'model_evals', str_c(sp_nospc, '.csv'))
  erf_fp <- file.path(pred_dir, 'model_evals', str_c(sp_nospc, '.rds'))
  
  erf <- model_species_rf(sp_df,
                          pred, 
                          model_fp, 
                          sp_name,
                          eval_fp,
                          erf_fp,
                          mutually_exclusive_pa,
                          unq_cells,
                          rf_fig_dir)
}

# Look at model statistics together
fps <- list.files(file.path(pred_dir, 'model_evals'), '*.csv$', full.names = T)
if(length(fps) > 0) {
  eval_tbl <- fps %>% purrr::map_dfr(read.csv)
  eval_tbl_fp <- file.path(pred_dir, str_c('model_evals_', length(fps), 'species.csv'))
  eval_tbl %>% write_csv(eval_tbl_fp)
}

# Save TIFs of likelihood and presence/absence ----
fps <- list.files(file.path(pred_dir, 'models'), '*.rds', full.names = T)

# filter filepaths to species list
sp_fps <- sp_nospc_list %>% 
  map(~str_subset(.y, .x), fps) %>% 
  flatten_chr()

for(rf_fp in sp_fps){
  
  # Filepaths  
  sp_nospc <- file_path_sans_ext(basename(rf_fp))
  print(sp_nospc)
  
  likelihood_fp <- file.path(pred_dir, 'likelihood', str_glue(sp_nospc, '.tif'))
  binned_fp <- file.path(pred_dir, 'binned_spec_sens', str_glue(sp_nospc, '.tif'))
  
  if(file.exists(likelihood_fp) & file.exists(binned_fp)){
    next
  } 
  
  erf_fp <- file.path(pred_dir, 'model_evals', str_c(sp_nospc, '.rds'))
  
  # Make TIFs of likelihood and presence/absence
  predict_distribution_rf(rf_fp, erf_fp, likelihood_fp, binned_fp)
  
}

# CROP - sum to richness ----
# Likelihood
# List TIFs and filter to species list
fps <- list.files(file.path(pred_dir, 'likelihood'), 'tif$', full.name=T)
sp_fps <- sp_nospc_list %>% 
  map(~str_subset(.y, .x), fps) %>% 
  flatten_chr()

if(length(sp_fps) > 0) {
  
  rich_fp_prefix <- file.path('/home/esturdivant/PROJECTS/polinizadores/data/data_out/pollinator_points/sdms_by_crop/Cucurbita_argyrosperma', 
                              str_glue('Likhd_rich_{dfilt_code}_{length(sp_fps)}species'))
  rich_plot_fp <-str_glue('{rich_fp_prefix}.png')
  rich_tif_fp <- str_glue('{rich_fp_prefix}.tif')
  
  stack_sdms(sp_fps, rich_tif_fp, rich_plot_fp, mex)
  
}

# Presence/Absence
# List TIFs and filter to species list
fps <- list.files(file.path(pred_dir, 'binned_spec_sens'), 'tif$', full.name=T)
sp_fps <- sp_nospc_list %>% 
  map(~str_subset(.y, .x), fps) %>% 
  flatten_chr()

# Run for crop
if(length(fps) > 0) {
  
  rich_fp_prefix <- file.path('/home/esturdivant/PROJECTS/polinizadores/data/data_out/pollinator_points/sdms_by_crop/Cucurbita_argyrosperma', 
                              str_glue('PA_rich_{dfilt_code}_{length(sp_fps)}species'))
  rich_tif_fp <- str_glue('{rich_fp_prefix}.tif')
  rich_plot_fp <-str_glue('{rich_fp_prefix}.png')
  
  stack_sdms(sp_fps, rich_tif_fp, rich_plot_fp, mex)
  
}

# Likelihood maps ----
fps <- list.files(file.path(pred_dir, 'likelihood'), '*.tif', full.names=T)

for (fp in fps) {
  
  # Filepaths
  sp_nospc <- file_path_sans_ext(basename(fp))
  plot_fp <- file.path(rf_fig_dir, 'map_predictions', str_c(sp_nospc, '_likelihood.png'))
  dir.create(dirname(plot_fp), recursive = T, showWarnings = F)
  
  # Load TIFF as stars
  pr_rf1_stars <- read_stars(fp)

  # Plot
  likelihood_plot <- ggplot() +
    geom_stars(data=pr_rf1_stars) +
    geom_sf(data = mex, fill = "transparent", size = 0.2, color = "black") +
    colormap::scale_fill_colormap("Occupancy\nlikelihood", na.value = "transparent", 
                                  colormap = colormap::colormaps$viridis) +
    ggthemes::theme_hc() +
    theme(legend.position=c(.95, 1), legend.title.align=0, legend.justification = c(1,1)) +
    labs(x = NULL, y = NULL)
  
  # Save
  ggsave(plot_fp, likelihood_plot, width=9, height=5.7, dpi=120)
  
}

# Presence maps ----
fps <- list.files(file.path(pred_dir, 'binned_spec_sens'), '*.tif', full.names=T)
for (fp in fps) {
  
  # Filepaths
  fn <- basename(fp)
  sp_nospc <- file_path_sans_ext(fn)
  plot_fp <- file.path(rf_fig_dir, 'map_predictions', str_c(sp_nospc, '_bin_specsens.png'))
  dir.create(dirname(plot_fp), recursive = T, showWarnings = F)
  
  # Load TIFF as stars
  pa_rf1_stars <- read_stars(fp)
  
  # Plot
  binned_map <- ggplot() +
    geom_stars(data=pa_rf1_stars) +
    geom_sf(data = mex, fill = "transparent", size = 0.2, color = "black") +
    colormap::scale_fill_colormap("likely present", na.value = "transparent", 
                                  colormap = colormap::colormaps$viridis) +
    ggthemes::theme_hc() +
    theme(legend.position=c(.95, 1), legend.title.align=0, legend.justification = c(1,1))+
    labs(x = NULL, y = NULL) 
  
  # Save
  ggsave(plot_fp, binned_map, width=9, height=5.7, dpi=120)
  
}

# COMBINE Sum likelihood maps ----
# list species (such as nocturnal butterflies)
if(pol_group == 'Mariposas'){
  sp_groups <- pol_df1 %>% st_drop_geometry %>% distinct(species, nocturna)
  list1 <- sp_groups %>%
    filter(nocturna=='nocturna') %>%
    transmute(species = str_replace_all(species, ' ', '_')) %>% 
    deframe
  
  l1_pattern <- str_c(list1, collapse='|')
  
  fps <- list.files(file.path(pred_dir, 'likelihood'), 'tif$', full.name=T) %>% 
    str_subset(str_glue('{l1_pattern}'))
}

# List TIFs and filter to species list
fps <- list.files(file.path(pred_dir, 'likelihood'), 'tif$', full.name=T)
sp_fps <- sp_nospc_list %>% 
  map(~str_subset(.y, .x), fps) %>% 
  flatten_chr()

if(length(sp_fps) > 0) {
  
  rich_fn <- str_glue('Likhd_rich_{pol_group}_{dfilt_code}_{length(sp_fps)}species')
  rich_plot_fp <- file.path(rf_fig_dir, str_glue('{rich_fn}.png'))
  rich_tif_fp <- file.path(pred_dir, str_glue('{rich_fn}.tif'))
  
  stack_sdms(sp_fps, rich_tif_fp, rich_plot_fp, mex)
  
}

# Sum presence/absence maps ----
# Use raster package
fps <- list.files(file.path(pred_dir, 'binned_spec_sens'), 'tif$', full.name=T)
sp_fps <- sp_nospc_list %>% 
  map(~str_subset(.y, .x), fps) %>% 
  flatten_chr()

if(length(fps) > 0) {
  
  fn <- str_glue('PA_rich_{pol_group}_{dfilt_code}_{nlayers(pol_stack)}species')
  binned_rich_plot_fp <- file.path(rf_fig_dir, str_glue('{fn}.png'))
  binned_rich_tif_fp <- file.path(pred_dir, str_glue('{fn}.tif'))

  stack_sdms(sp_fps, binned_rich_tif_fp, binned_rich_plot_fp, mex)

}





# ~ TESTING: make uncertainty maps ----
# ModelMap?
# ModelMap can create an SD map using model.mapmake(..., map.sd=T)
ModelMap::model.mapmake(rf1, 
                        folder=pred_dir,
                        MODELfn=,
                        rastLUTfn=,
                        na.action='na.omit',
                        map.sd=T)


# Load environment variables ----
crop_dir <- file.path('data', 'input_data', 'environment_variables', 'cropped')
pred_fps <- list.files(crop_dir, 'tif$', full.names=T)
# Remove layers from predictors
drop_lst <- c('biomes_CVEECON2', 'biomes_CVEECON1', 'biomes_CVEECON4',
              'ESACCI.LC.L4.LC10.Map.10m.MEX_2016_2018', 
              'usv250s6gw_USV_SVI')
drop_pattern <- str_c(drop_lst, collapse='|')
pred_fps <- pred_fps[-str_which(pred_fps, drop_pattern)]

# predict? use predict.all argument to generate predictions for all trees?
ext1 <- extent(c(xmin=-100, xmax=-99, ymin=24, ymax=25))
pred_crop <- crop(predictors, ext1)
pr_rf1 <- dismo::predict(pred_crop, rf1, ext=ext1)
pr_rf1 <- raster::focal(pr_rf1, 
                        w=matrix(1,nrow=3, ncol=3), 
                        fun=mean, 
                        NAonly=TRUE, 
                        na.rm=TRUE) 





# Load TIFF as stars
pr_rf1_stars <- st_as_stars(pred_crop[[1]])

# Plot
mex1 <- st_crop(mex, st_bbox(ext1))
(likelihood_plot <- ggplot() +
  geom_stars(data=pr_rf1_stars) +
  geom_sf(data = mex1, fill = "transparent", size = 0.2, color = "black") +
  colormap::scale_fill_colormap("Occupancy\nlikelihood", na.value = "transparent", 
                                colormap = colormap::colormaps$viridis) +
  ggthemes::theme_hc() +
  theme(legend.position=c(.95, 1), legend.title.align=0, legend.justification = c(1,1)) +
  labs(x = NULL, y = NULL))






# ~ blockCV ----
browseVignettes('blockCV') 
# remotes::install_github("rvalavi/blockCV", dependencies = TRUE)
library(blockCV)

# Create presence-background species data 
# Format presence points
sp1 <- sp_df %>% 
  distinct() %>% 
  st_transform(crs = crs(pred)) %>% 
  mutate(lon = unlist(map(.$geom, 1)), 
         lat = unlist(map(.$geom, 2))) %>% 
  st_drop_geometry() %>% 
  dplyr::select(lon, lat) %>% 
  mutate(Species = 1)

# Get background points 
set.seed(10)
backg <- randomPoints(pred, n=1000, 
                      # p = presence points
                      # ext=ext, 
                      # extf = 1.25
) %>% 
  as_tibble() %>% 
  rename(lon='x', lat='y') %>% 
  mutate(Species = 0)

pb_data <- bind_rows(sp1, backg) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = crs(pred))

# number of presence and background records
table(pb_data$Species)

# plot species data on the map
plot(pred[['wc2.1_30s_bio_1']]) # plot raster data
plot(pb_data[which(pb_data$Species==0), ], pch = 16, col="blue", add=TRUE) # add absence points
plot(pb_data[which(pb_data$Species==1), ], pch = 16, col="red", add=TRUE) # add presence points
legend(x=500000, y=8250000, legend=c("Presence","Absence"), col=c(2, 4), pch=c(16,16), bty="n")

# Spatial block ----
# Explore autocorrelation
sac <- spatialAutoRange(rasterLayer = pred,
                        sampleNumber = 5000,
                        doParallel = TRUE,
                        showPlots = TRUE)
summary(sac)
library(automap)
plot(sac$variograms[[2]])
plot(pred[['wc2.1_30s_bio_5']])
# bio 5 is best with blocks of 162,000 m (effective range of spatial autocorrelation)

# Explore block size
rangeExplorer(rasterLayer = pred,
              speciesData = pb_data,
              species = "Species",
              rangeTable = NULL,
              minRange = 80000, # limit the search domain
              maxRange = 500000)

# Spatial blocking by specified range
sb <- spatialBlock(speciesData = pb_data,
                   species = "Species",
                   rasterLayer = pred,
                   theRange = 200000, # size of the blocks
                   k = 5,
                   selection = "random", # Robinson et al. 2018 use random
                   iteration = 100, # find evenly dispersed folds
                   numLimit = 0, # find most evenly dispersed number of records
                   biomod2Format = TRUE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)

# Explore generated folds
foldExplorer(blocks = sb, 
             rasterLayer = pred, 
             speciesData = pb_data)

# Buffering ----
# buffering with presence-background data
bf2 <- buffering(speciesData = pb_data, # presence-background data
                 theRange = 200000,
                 species = "Species",
                 spDataType = "PB", # presence-background data type
                 addBG = TRUE, # add background data to testing folds
                 progress = TRUE)

# # Environmental block ----
# # environmental clustering
# eb <- envBlock(rasterLayer = awt,
#                speciesData = pb_data,
#                species = "Species",
#                k = 5,
#                standardization = "standard", # rescale variables between 0 and 1
#                rasterBlock = FALSE,
#                numLimit = 50)

# Evaluate PB models with blocks ----
# Spatial blocking to evaluate random forest model ----
# loading the libraries
library(randomForest)
library(precrec)

# extract the raster values for the species points as a dataframe
mydata <- raster::extract(pred, pb_data, df = TRUE)
mydata <- mydata[,-1]

# Add species column to the dataframe and remove extra column (ID)
mydata$Species <- pb_data$Species

# Extract the fold indices created in the previous section
# the folds (list) works for all three blocking strategies
folds <- sb$folds
folds <- sb$foldID

# create a data.frame to store the prediction of each fold (record)
testTable <- pb_data
testTable$pred <- NA

# Regression RF
for(k in seq_len(5)){
  # Get training and testing indices for this fold
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  # Train on this fold's training set
  rf <- randomForest(Species ~ ., 
                     mydata[trainSet, ], 
                     ntree = 250, 
                     na.action = na.exclude) # model fitting on training set
  # Predict the test set for this fold and add to test results table
  testTable$pred[testSet] <- predict(rf, mydata[testSet, ])
}

# calculate Area Under the ROC and PR curves and plot the result
precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Species)
autoplot(precrec_obj)

# Classification RF
# Add species column to the dataframe and remove extra column (ID)
mydata$Species <- as.factor(pb_data$Species)

for(k in seq_len(5)){
  # extracting the training and testing indices
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  rfc <- randomForest(Species ~ ., 
                     mydata[trainSet, ], 
                     ntree = 250, 
                     na.action = na.exclude) # model fitting on training set
  # predict the test set
  testTable$pred[testSet] <- predict(rfc, mydata[testSet, ], type = "prob")[,2] 
}

# calculate Area Under the ROC and PR curves and plot the result
precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Species)
autoplot(precrec_obj)

# Buffer folds (leave one out cross-validation) ----
# extract the raster values for the species points as a dataframe
mydata <- raster::extract(pred, pb_data, df = TRUE)
mydata <- mydata[,-1]

# adding species column to the dataframe
mydata$Species <- pb_data$Species

# extract the fold indices from buffering object 
# created in the previous section
# the folds (list) works for all three blocking strategies
folds <- bf2$folds

# create a data.frame to store the prediction of each fold (record)
testTable <- pb_data
testTable$pred <- NA

thresholds <- vector(mode='numeric', length=length(folds))

for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(Species~., 
                     mydata[trainSet, ], 
                     ntree = 250, 
                     na.action = na.exclude) # model fitting on training set
  testTable$pred[testSet] <- predict(rf, mydata[testSet, ]) # predict the test set
  # Get threshold
  testpres <- filter(mydata[testSet, ], Species == 1)
  testbackg <- filter(mydata[testSet, ], Species == 0)
  erf <- dismo::evaluate(testpres, testbackg, rf)
  thresholds[[k]] <- threshold(erf, 'spec_sens')
}

# calculate Area Under the ROC and PR curves and plot the result
precrec_obj <- precrec::evalmod(scores = testTable$pred, labels = testTable$Species)
autoplot(precrec_obj)
precrec_stats <- precrec::evalmod(scores = testTable$pred, 
                                  labels = testTable$Species, mode='basic')
precrec_stats

pROC::auc(testTable$pred)
summary(thresholds)

# Classification
mydata$Species <- as.factor(pb_data$Species)
for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(Species~., 
                     mydata[trainSet, ], 
                     ntree = 250, 
                     na.action = na.exclude) # model fitting on training set
  testTable$pred[testSet] <- predict(rf, mydata[testSet, ], type = "prob")[,2] # predict the test set
}

# calculate Area Under the ROC and PR curves and plot the result
precrec_obj <- precrec::evalmod(scores = testTable$pred, labels = testTable$Species)
precrec::evalmod(scores = testTable$pred, labels = testTable$Species, mode='basic')
autoplot(precrec_obj)

# Map suitability prediction as continuous and presence/absence ----
pr_rf <- predict(pred, rf)
plot(pr_rf)

tm_shape(pr_rf) + tm_raster(palette = 'viridis', n=2, style='cat') +
  # tm_layout(aes.palette = 'cat') +
  tm_shape(pb_data %>% filter(Species==1)) + tm_dots(size=0.005) +
  tm_shape(mex) + tm_borders()

tm_shape(predictors[[3]]) + tm_raster()

















# ~ Process from eBird Best Practices ----
# Summarize environmental data within a neighborhood of the point
# PLAND from FRAGSTATS for land cover
# Use 2.5 x 2.5 km neighborhood for birds

# 1. get points, convert to sf, and project to match environmental data
# 2. buffer points
neighborhood_radius <- 5 * ceiling(max(res(landcover))) / 2
ebird_buff <- ebird %>% 
  distinct(year = format(observation_date, "%Y"),
           locality_id, latitude, longitude) %>% 
  # for 2019 use 2018 landcover data
  mutate(year_lc = if_else(as.integer(year) > max_lc_year, 
                           as.character(max_lc_year), year),
         year_lc = paste0("y", year_lc)) %>% 
  # convert to spatial features
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  # transform to modis projection
  st_transform(crs = projection(landcover)) %>% 
  # buffer to create neighborhood around each point
  st_buffer(dist = neighborhood_radius) %>% 
  # nest by year
  nest(data = c(year, locality_id, geometry))

# 3. extract raster values within neighborhood and count cells in each landcover class
# function to summarize landcover data for all checklists in a given year
calculate_pland <- function(yr, regions, lc) {
  locs <- st_set_geometry(regions, NULL)
  exact_extract(lc[[yr]], regions, progress = FALSE) %>% 
    map(~ count(., landcover = value)) %>% 
    tibble(locs, data = .) %>% 
    unnest(data)
}
# iterate over all years extracting landcover for all checklists in each
lc_extract <- ebird_buff %>% 
  mutate(pland = map2(year_lc, data, calculate_pland, lc = landcover)) %>% 
  dplyr::select(pland) %>% 
  unnest(cols = pland)

# 4. Calculate PLAND
pland <- lc_extract %>% 
  # calculate proporiton
  group_by(locality_id, year) %>% 
  mutate(pland = n / sum(n)) %>% 
  ungroup() %>% 
  dplyr::select(-n) %>% 
  # remove NAs after tallying so pland is relative to total number of cells
  filter(!is.na(landcover))

# 5. Transform data wide format with var names from LC descriptions
# convert names to be more descriptive
lc_names <- tibble(landcover = 0:15,
                   lc_name = c("pland_00_water", 
                               "pland_01_evergreen_needleleaf", 
                               "pland_02_evergreen_broadleaf", 
                               "pland_03_deciduous_needleleaf", 
                               "pland_04_deciduous_broadleaf", 
                               "pland_05_mixed_forest",
                               "pland_06_closed_shrubland", 
                               "pland_07_open_shrubland", 
                               "pland_08_woody_savanna", 
                               "pland_09_savanna", 
                               "pland_10_grassland", 
                               "pland_11_wetland", 
                               "pland_12_cropland", 
                               "pland_13_urban", 
                               "pland_14_mosiac", 
                               "pland_15_barren"))
pland <- pland %>% 
  inner_join(lc_names, by = "landcover") %>% 
  arrange(landcover) %>% 
  dplyr::select(-landcover)

# tranform to wide format, filling in implicit missing values with 0s%>% 
pland <- pland %>% 
  pivot_wider(names_from = lc_name, 
              values_from = pland, 
              values_fill = list(pland = 0))

# save
write_csv(pland, "data/modis_pland_location-year.csv")

# Convert landcover classes into percent of X landcover ----
# 6. calculate PLAND metrics for all landcover raster
# get cell centers and create neighborhoods
r_centers <- rasterToPoints(r, spatial = TRUE) %>% 
  st_as_sf() %>% 
  transmute(id = row_number())
r_cells <- st_buffer(r_centers, dist = neighborhood_radius)

# extract landcover values within neighborhoods, only needed most recent year
lc_extract_pred <- landcover[[paste0("y", max_lc_year)]] %>% 
  exact_extract(r_cells, progress = FALSE) %>% 
  map(~ count(., landcover = value)) %>% 
  tibble(id = r_cells$id, data = .) %>% 
  unnest(data)

# calculate the percent for each landcover class
pland_pred <- lc_extract_pred %>% 
  count(id, landcover) %>% 
  group_by(id) %>% 
  mutate(pland = n / sum(n)) %>% 
  ungroup() %>% 
  dplyr::select(-n) %>% 
  # remove NAs after tallying so pland is relative to total number of cells
  filter(!is.na(landcover))

# convert names to be more descriptive
pland_pred <- pland_pred %>% 
  inner_join(lc_names, by = "landcover") %>% 
  arrange(landcover) %>% 
  dplyr::select(-landcover)

# tranform to wide format, filling in implicit missing values with 0s
pland_pred <- pland_pred %>% 
  pivot_wider(names_from = lc_name, 
              values_from = pland, 
              values_fill = list(pland = 0)) %>% 
  mutate(year = max_lc_year) %>% 
  dplyr::select(id, year, everything())

# join in coordinates
pland_coords <- st_transform(r_centers, crs = 4326) %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  cbind(id = r_centers$id, .) %>% 
  rename(longitude = X, latitude = Y) %>% 
  inner_join(pland_pred, by = "id")

# For continuous covariates, calculate the mean, median, and SD within each neighborhood.

# Model occupancy probability (encounter rate) ----
library(sf)
library(raster)
library(dggridR)
library(lubridate)
library(ranger)
library(scam)
library(PresenceAbsence)
library(verification)
library(ebirdst)
library(fields)
library(gridExtra)
library(tidyverse)

# resolve namespace conflicts
select <- dplyr::select
map <- purrr::map
projection <- raster::projection





# generate hexagonal grid with ~ 5 km between cells
dggs <- dgconstruct(spacing = 5)
# get hexagonal cell id and week number for each checklist
checklist_cell <- ebird_habitat %>% 
  mutate(cell = dgGEO_to_SEQNUM(dggs, longitude, latitude)$seqnum)
# sample one checklist per grid cell per week
# sample detection/non-detection independently 
ebird_ss <- checklist_cell %>% 
  group_by(species_observed, cell) %>% 
  sample_n(size = 1) %>% 
  ungroup()



# OLD Get GBIF data (previously process) and filter to one species ----
df <- readRDS('data/data_out/r_data/gbif_pol_points.rds')
sp <- 'Apis mellifera'
df_sp <- df %>% 
  filter(species == sp)

# Create raster stack of the predictor variables
fps <- list.files(crop_dir, '.tif', full.names=T)
predictors <- stack(fps)
names(predictors) <- names(predictors) %>% 
  str_sub(start=-6) %>% 
  str_replace_all('_', '') %>% 
  str_replace_all('skalt', 'alt') %>% 
  str_replace_all('.*cds$', 'lc')
predictors[['lc']] <- predictors[['lc']] %>% as.factor


mapview(predictors)

# Extract values for ANPs ------------------------------------------------------
anp_fp <- 'data/input_data/context_Mexico/SHAPE_ANPS/182ANP_Geo_ITRF08_Julio04_2019.shp'
anps <- st_read(anp_fp) %>% 
  st_transform(crs=4326) %>% 
  as('Spatial')

lyrs <- c('lc', # ESA land cover (LCCS product)
          'alt', # SRTM altitude
          'bio1', # mean annual temp
          'bio4', # temp seasonality
          'bio12', # total annual precipitation
          'bio15' # precipitation seasonality
)
predictors_slim <- predictors[[lyrs]]
predictors_num <- predictors_slim[[-1]]
anp_vals <- raster::extract(predictors_num, anps, fun=mean, na.rm=T, sp=T)
anp_vals %>% head

# Extract values for (basic) modeling ------------------------------------------
presvals <- raster::extract(predictors, df_sp)

# Create pseudo-absence points ----
# Sampling bias
# - basic option is subsampling with gridding; sample within radius
# - Phillips et al. (2009) propose target-group background data

# Sample within radius of presence points
set.seed(10)
# Create buffer zone around points and within Mex
# Mexico boundaries
mex <- mex %>% 
  st_as_sf %>% 
  st_transform(crs=6372) %>% 
  st_simplify(dTolerance=5000)
# Buffer zone
buff_zone <- df_sp %>% 
  st_transform(crs=6372) %>% 
  st_buffer(50000) %>% 
  group_by %>% 
  summarize %>% 
  st_intersection(mex)

# Look
mapview(predictors[[1]]) + mapview(buff_zone) + mapview(mex)

# Sample randomly
samp1 <- buff_zone %>% 
  st_sample(size=1100) %>% 
  st_transform(crs=4326) %>% 
  as_Spatial()

# Use random points to sample grid cells
mask <- predictors[[2]]
cells <- cellFromXY(mask, xy=samp1) %>% 
  unique
xy <- xyFromCell(mask, cells) %>% 
  as_tibble %>% 
  drop_na %>% 
  st_as_sf(x = .,                         
           coords = c('x', "y"),
           crs = 4326)

# Look
tm_shape(mask) + tm_raster() +
  tm_shape(buff_zone) + tm_borders() +
  tm_shape(xy) + tm_dots()

# Extract background values ----
absvals <- raster::extract(predictors, xy)

# Create DF for SDM with presence-background column
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals))) %>% 
  mutate(lc = as.factor(lc))
head(sdmdata)

# Visually check for colinearity - see Dormann et al. 2013
pairs(sdmdata[,3:6], cex=0.1)

saveRDS(sdmdata, "data/data_out/r_data/sdm.Rds")
saveRDS(presvals, "data/data_out/r_data/pvals.Rds")

# Model fitting ----------------------------------------------------------------
sdmdata <- readRDS("data/data_out/r_data/sdm.Rds")
presvals <- readRDS("data/data_out/r_data/pvals.Rds")

# GLM
m1 <- glm(pb ~ lc + alt + bio1 + bio4 + bio12 + bio15, data=sdmdata)
summary(m1)
m2 <- glm(pb ~ ., data=sdmdata)
summary(m2)
pairs(sdmdata, cex=0.1)

# Look at dismo::bioclim, which only uses presense points
bio_vars <- presvals %>% colnames
bc <- bioclim(presvals[,bio_vars[-1]])
response(bc)

# Suitability map from Bioclim model (without biomes)
p_bc <- predict(predictors[[-1]], bc)

# Bioclim with all predictors
bc <- bioclim(presvals)
class(bc)
bc

# Make suitability map
p <- predict(predictors, m1)
p2 <- predict(predictors, m2)
p_bc <- predict(predictors, bc)
mapview(p) + 
  mapview(p_bc) +
  mapview(df_sp, cex=.2)

# Model evaluation -----------
# Does the model make sense ecologically?
# Do the fitted functions make sense?
# Do the predictions seem reasonable?
# Are there spatial patterns in the residuals?


# MaxEnt -----------------------------------------------------------------------
# Split presence points into training and testing sets
set.seed(0)
group <- kfold(df_sp, 5)
pres_train <- df_sp[group != 1, ] %>% as('Spatial')
pres_test <- df_sp[group == 1, ] %>% as('Spatial')

# Background data for training and testing
set.seed(10)
# random points throughout Mexico
pred_nf <- dropLayer(predictors, c('lc', 'biomes'))
ext <- extent(mex)
backg <- randomPoints(pred_nf, n=1000, ext=ext, extf = 1.25)
backg %>% 
  as_tibble %>% 
  drop_na %>% 
  st_as_sf(x = .,                         
           coords = c('x', "y"),
           crs = 4326) %>% 
  mapview(cex=4) + mapview(xy, cex=4)
colnames(backg) = c('lon', 'lat')
# random points from within buffer of presence points
backg <- xy
# divide into train and test
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ] %>% as('Spatial')
backg_test <- backg[group == 1, ] %>% as('Spatial')
mapview(backg_test) + mapview(backg_train)

# Fit MaxEnt model ----
# All predictors, all points
xm <- maxent(predictors, pres_train, factors=c('lc', 'biomes'))
plot(xm)
response(xm)
e <- evaluate(pres_test, backg_test, xm, predictors)

# All predictors, remove duplicate points
xm_rm <- maxent(predictors, pres_train, factors=c('lc', 'biomes'), removeDuplicates=T)
plot(xm_rm)
response(xm_rm)
e <- evaluate(pres_test, backg_test, xm_rm, predictors)
px <- predict(predictors, xm_rm, ext=ext, progress='')
(tr <- threshold(e, 'spec_sens'))
mapview(px) + mapview(px > tr)

# Slimmed predictors
# Variables in Nogué et al. 2016: 
lyrs <- c('lc', # ESA land cover (LCCS product)
          'alt', # SRTM altitude
          'bio1', # mean annual temp
          'bio4', # temp seasonality
          'bio12', # total annual precipitation
          'bio15' # precipitation seasonality
)
predictors_slim <- predictors[[lyrs]]
xm_slim <- maxent(predictors_slim, pres_train, factors='lc', removeDuplicates=T)
plot(xm_slim)
response(xm_slim)
e_slim <- evaluate(pres_test, backg_test, xm_slim, predictors)
px_slim <- predict(predictors, xm_slim, ext=ext, progress='')
(tr <- threshold(e_slim, 'spec_sens'))
mapview(px_slim) + mapview(px_slim > tr)
