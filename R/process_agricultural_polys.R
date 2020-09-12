
# Load libraries ----
library(sf)
# library(magrittr)
# library(rvest)
library(tools)
library(mapview)
library(tmap)
# library(units)
library(stringdist)
library(tidyverse)
library(ggplot2)
theme_set(theme_minimal())
library(ggnewscale)


# Load pre-created objects (from prep_SIAP_data.R)
load('data/helpers/initial_vars.RData')
load('data/helpers/functions.RData')
crops_dir <- 'data/data_out/polys_ag_INEGI_wFMG_pcts/pcts_by_state'
ag_by_crop_dir <- 'data/data_out/polys_ag_INEGI_wFMG_pcts/specific_crops'
pol_pt_dir <- 'data/data_out/pollinator_points'

# Functions --------------------------------------------------------------------
filter_pts <- function(fp, pols, cultivo_dir){
  
  # Load points
  pts <- st_read(fp)
  
  # Get table without geometry
  pts_no_geom <- pts %>% 
    st_set_geometry(NULL)
  
  # Filter list of genera for X crop to those in current pollinators file ----
  pts_gen_list <- pts_no_geom %>% 
    dplyr::select(genus) %>% 
    distinct %>% 
    deframe
  
  # Filter list to those in points
  pols_gen_filt <- pols %>% 
    filter(!is.na(genus) & genus %in% pts_gen_list)
  
  # FAMILY: Filter list of families for given crop to those in points ----
  pts_fam_list <- pts_no_geom %>%
    dplyr::select(family) %>%
    distinct %>%
    deframe
  
  # Filter pollinators list to families in currently loaded pollinators file
  pols_fam_filt <- pols %>% 
    filter(!is.na(family) & family %in% pts_fam_list)
  
  # ORDER: Filter list of orders for given crop to those in points ----
  pts_order_list <- pts_no_geom %>%
    dplyr::select(order) %>%
    distinct %>%
    deframe
  
  # Filter pollinators list to orders in currently loaded pollinators file
  pols_order_filt <- pols %>% 
    filter(!is.na(order) & order %in% c(pts_order_list))
  
  # CLASS: Filter list of classes for given crop to those in points ----
  pts_class_list <- pts_no_geom %>%
    dplyr::select(class) %>%
    distinct %>%
    deframe
  
  # Filter pollinators list to classes in currently loaded pollinators file
  pols_class_filt <- pols %>% 
    filter(!is.na(class) & class %in% pts_class_list)
  
  
  # If there are no matching genera or families, stop function (move to next file)
  if(nrow(pols_gen_filt) < 1 & 
     nrow(pols_fam_filt) < 1 & 
     nrow(pols_order_filt) < 1 & 
     nrow(pols_class_filt) < 1) return()
  
  # Dissolve points by species
  pts_diss <- pts %>% 
    group_by(species, genus, family, order) %>% 
    summarize %>% 
    ungroup
  
  # Separate pollinators into those with species match vs. those with genus match
  pts_spec_list <- pts_no_geom %>% 
    dplyr::select(species) %>% 
    distinct %>% 
    deframe
  
  pols_specs_filt <- pols %>% 
    filter(Polinizador %in% pts_spec_list)
  
  pols_gen_filt <- pols_gen_filt %>% 
    filter(!Polinizador %in% pts_spec_list & 
             genus %in% pts_gen_list)
  
  pols_fam_filt <- pols_fam_filt %>% 
    filter(!Polinizador %in% pts_spec_list &
             !Polinizador %in% pts_gen_list &
             family %in% pts_fam_list)
  
  pols_order_filt <- pols_order_filt %>% 
    filter(!Polinizador %in% pts_spec_list &
             !Polinizador %in% pts_gen_list &
             !Polinizador %in% pts_fam_list &
             order %in% pts_order_list)
  
  pols_class_filt <- pols_class_filt %>% 
    filter(!Polinizador %in% pts_spec_list &
             !Polinizador %in% pts_gen_list &
             !Polinizador %in% pts_fam_list &
             !Polinizador %in% pts_order_list &
             class %in% pts_class_list)
  
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

# Choose a crop ----------------------------------------------------------------
# cult <- 'Persea americana' 
# var <- 'Aguacate'
# safe_var_name <- var
# 
cult <- 'Phaseolus vulgaris' 
var <- 'Frijol'
safe_var_name <- var

cult <- 'Capsicum annuum' 
var <- 'Pimiento'
safe_var_name <- 'Chile.verde'

# cult <- 'Cucumis melo' # melón
# var <- 'Melón'
# 
# cult <- 'Lycopersicon esculentum' # jitomate
# var <- 'Tomate.rojo..jitomate.'
# 
# cult <- 'Cucurbita pepo' # calabacita
# var <- 'Calabacita'
# 
# cult <- 'Citrullus lanatus' # sandia
# var <- 'Sandía'
# safe_var_name <- var
# 
# cult <- 'Coffea arabica'
# var <- 'Café cereza'
# safe_var_name <- 'Café.cereza'

# cult <- 'Mangifera indica'
# var <- 'Mango'
# safe_var_name <- var
# 
# cult <- 'Agave salmiana'
# var <- 'Agave'
# safe_var_name <- var

cultivos_importantes_fp <- 'data/helpers/20_cultivos_mas_importantes_de_Mexico.xlsx'
cult_df <- readxl::read_excel(cultivos_importantes_fp)

# Work with tables -------------------------------------------------------------
# 20_cultivos_mas_importantes_de_Mexico.xlsx lists the 20 cultivos with common name and SIAP variable name

# BaseCultivosAsworth_MXname.xlsx matches species to common name
# app1_cultivos also matches species to common name
cultivos_nombre_fp <- 'data/helpers/cultivos_to_MXname_Ashworth.xlsx'
cult_nom_df <- readxl::read_excel(cultivos_nombre_fp)

# Apendice2 links crops to pollinators



i <- 7

# for(i in seq(nrow(cult_df))){

cult <- cult_df[i, 'Especie']
var <- cult_df[i, 'Cultivo']
safe_var_name <- cult_df[i, 'Nombre_SIAP']

# Get pollinator points for given crop -----------------------------------------
fname <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice2.csv'
# get_pol_pts_for_crop(fname, pol_pt_dir, cult)
# Import CSV (sep=';')f rom Apendice2
df <- read_delim(fname, delim=';', trim_ws=T) %>% 
  fill(Familia, Cultivo) %>% 
  mutate(genus = str_extract(Polinizador, '^[:upper:]\\S*'), # assume first word is genus
         family = str_extract(Grupo, '\\S*idae'),
         order = str_extract(Grupo, '\\S*ptera'), 
         class = str_extract(Grupo, 'Aves'))

# test <- df %>% dplyr::select(Grupo) %>% distinct
crops <- df %>% select(Cultivo) %>% distinct %>% deframe
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

# Save file
fp_out <- file.path(pol_pt_dir, 'pollinators_by_crop', 
                    str_c(str_replace_all(cult, ' ', '_'), '.geojson'))
out_pts %>% st_write(fp_out, delete_dsn=T, driver='GeoJSON')

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

