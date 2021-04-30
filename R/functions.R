# Load libraries ----
library(sf)
library(raster)
library(stars)
library(dismo)
library(rvest)
library(tools)
library(mapview)
library(units)
library(tidyverse)

#' @export
box::use(./functions)

# Functions ----
#' @export
html_to_df <- function(x){
  # get values from description field
  x %>% 
    minimal_html %>% 
    html_node('table') %>% 
    html_table(fill=T) %>% 
    dplyr::select(X1, X2) %>% 
    slice(-1:-2) %>% 
    pivot_wider(names_from=X1, values_from=X2)
}

#' @export
load_kmz_as_sf <- function(kmz_url, data_dir, out_fp, col_types=NULL){
  # Download KMZ and convert to sf dataframe
  kmz_name <- kmz_url %>% basename()
  download.file(kmz_url, dest=kmz_name)
  sf <- kmz_name %>% 
    unzip(list=F, exdir=data_dir) %>% 
    sf::st_read
  unlink(kmz_name)
  
  # Convert KML description field to DF
  df <- sf$description %>%
    purrr::map_dfr(html_to_df) 
  if(length(col_types)>0){
    df <- df %>% 
      type_convert(col_types=col_types, trim_ws=T)
  }
  
  # Replace KML variables with DF
  sf <- sf %>%
    dplyr::select(Name) %>%
    cbind(df) %>%
    sf::st_zm(drop=T, what='ZM') %>% 
    sf::st_make_valid

  return(sf)
}

#' @export
intersect_with_munis <- function(sup_ag, municipios){
  # Intersect with municipios and simplify
  ag_munis <- sup_ag  %>%
    sf::st_simplify(dTolerance = 0.0001) %>% 
    sf::st_intersection(municipios)  %>% 
    dplyr::group_by(CVE_ENT, CVE_MUN, MOD_AGR) %>% 
    summarize(geometry = sf::st_union(geometry)) %>% 
    as_tibble %>% 
    sf::st_as_sf
  
  # get area for new polygons in ha
  ag_munis$area <- ag_munis %>% 
    sf::st_transform(crs=6362) %>% 
    sf::st_area %>% 
    set_units('ha') %>% 
    set_units(NULL)
  
  return(ag_munis)
}

#' @export
load_and_preprocess_ag <- function(region, data_dir, municipios, return_df=T){
  fp_ag <- file.path(data_dir, stringr::str_c('FASII_', region, '.geojson'))
  
  if(!file.exists(fp_ag)){
    url_ag <- stringr::str_c('http://infosiap.siap.gob.mx/gobmx/datosAbiertos/', 
                    'frontera-agricola/FASII_', region, '.kmz')
    sup_ag <- load_kmz_as_sf(url_ag, data_dir, 'iicicicdcc') 
    if ('CVE_EDO' %in% colnames(sup_ag)) {
      sup_ag <- sup_ag %>% rename(CVE_ENT=CVE_EDO)
    }
    
    # Intersect with municipios
    sup_ag <- intersect_with_munis(sup_ag, municipios)
    
    # Save to file
    sup_ag %>% sf::st_write(fp_ag, delete_dsn=T)
  } else {
    if(return_df) return(sf::st_read(fp_ag)) else return(fp_ag)
  } 
  if(return_df) return(sup_ag) else return(fp_ag)
}

#' @export
preprocess_fmg <- function(sup_frij_gran, munis){
  # Prepare munis for intersection
  mun_for_join <- munis %>%
    dplyr::mutate(CVE_ENT=as.integer(CVE_ENT),
           CVE_MUN=as.integer(CVE_MUN)) %>% 
    dplyr::select(-AREA, -PERIMETER, -COV_, -COV_ID)
  
  # Simplify to remove jagged edges and decrease size
  sup_frij_gran <- sup_frij_gran %>% 
    sf::st_simplify(preserveTopology = T, dTolerance = .0001)
  
  # Get intersection with municipios
  sup_frij_gran <- sup_frij_gran %>% 
    dplyr::group_by(CULTIVO) %>% 
    summarize(geometry = sf::st_union(geometry)) %>% 
    sf::st_intersection(mun_for_join) 
  
  # Add area column
  sup_frij_gran$area_ha <- sup_frij_gran %>% 
    sf::st_transform(crs=6362) %>% 
    sf::st_area %>% 
    set_units('ha') %>% 
    set_units(NULL)
  
  if(!all(!is.na(sup_frij_gran$CVE_MUN))){
    print('WARNING: not all municipios matched')
  }
  
  return(sup_frij_gran)
}

#' @export
load_and_preprocess_fmg <- function(estado_code, data_dir, return_df, 
                                    municipios, fp_fmg, prefix='ESA_PV2015_'){
  # Get filename
  if(missing(fp_fmg)){
    fp_fmg <- file.path(data_dir, 
                        stringr::str_c(prefix, estado_code, '.geojson'))
    print(stringr::str_glue("fp_fmg not supplied so we'll try {fp_fmg}"))
  }
  
  if(file.exists(fp_fmg)){
    # Read file
    sup_frij_gran <- sf::st_read(fp_fmg) 
  } else {
    print(stringr::str_glue("{fp_fmg} doesn't exist"))
    
    try({
      # Download and convert from KMZ
      url_fmg <- stringr::str_c('http://infosiap.siap.gob.mx/gobmx/datosAbiertos/', 
                       'estimacion-superficie-agricola/ESA_PV2015_', estado_code, '.kmz')
      sup_frij_gran <- load_kmz_as_sf(url_fmg, data_dir)
    
      # Process
      sup_frij_gran <- preprocess_fmg(sup_frij_gran, municipios)
    
      # Save
      sf::st_write(sup_frij_gran, fp_fmg, delete_dsn=T)
    })
  }
  if(return_df) return(sup_frij_gran) else return(TRUE)
}


#' @export
get_area_planted <- function(data, state_id_fld, muni_id_fld, crop_name_fld, 
                             riego_id_fld, planted_fld, planted_area_fld){
  # Summarize crops by municipio, percent of total planted area

  # get total sembrada for each crop by municipio
  cultivo_totals <- data %>% 
    dplyr::group_by({{state_id_fld}}, {{ muni_id_fld }}, {{ crop_name_fld }}, {{riego_id_fld}}) %>% 
    summarize({{planted_fld}} := sum({{planted_fld}}, na.rm=T)) %>%
    spread({{ crop_name_fld }}, {{planted_fld}})
  
  # Quote variable name arguments as quosures
  state_id_fld <- rlang::enquo(state_id_fld)
  muni_id_fld <- rlang::enquo(muni_id_fld)
  riego_id_fld <- rlang::enquo(riego_id_fld)
  
  # get total sembrada area by municipio
  data %>% 
    dplyr::group_by({{state_id_fld}}, {{muni_id_fld}}, {{riego_id_fld}}) %>% 
    summarize({{planted_area_fld}} := sum({{planted_fld}})) %>% 
    full_join(cultivo_totals, 
              by=c(quo_name(state_id_fld), quo_name(muni_id_fld), quo_name(riego_id_fld))) %>% 
    as_tibble()
}

#' @export
remove_FMG_from_ag_by_state <- function(estado, ag_dir, fmg_dir, out_dir, states_by_region){
  # Get path for output file
  if(missing(out_dir)) out_dir <- 'data/data_out/polys_ag_SIAP_noFMG'
  fp_out <- file.path(out_dir, stringr::str_c('siap_noFMG_', estado, '.geojson'))
  
  # Don't proceed if file already exists
  if (file.exists(fp_out)) return(fp_out)
  
  # Get region code (ag) from state code (sup_fmg)
  if(missing(states_by_region)) {
    states_by_region <- readRDS('data/helpers/states_by_region.rds')
  }
  region <- states_by_region %>% 
    dplyr::filter(state==estado) %>% 
    dplyr::select(region) %>% 
    deframe
  
  # Load specific zone from ag data and specific state from FMG
  ag <- load_and_preprocess_ag(region, ag_dir)
  sup_fmg <- load_and_preprocess_fmg(estado, fmg_dir, T, municipios)
  
  # Get CVE for state of FMG data
  cve_ent <- sup_fmg %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(CVE_ENT) %>% 
    dplyr::distinct %>% 
    deframe
  ag <- ag %>% 
    dplyr::mutate(CVE_ENT = as.integer(CVE_ENT)) %>% 
    dplyr::filter(CVE_ENT == cve_ent)
  
  # Remove FMG from ag
  siap_noFMG <- clip_out_polys_from_ag(ag, sup_fmg)
  
  # Save 
  siap_noFMG %>% sf::st_write(fp_out, delete_dsn=T)
  return(fp_out)
}

#' @export
remove_FMG_from_ag_INEGI <- function(estado, ag, fmg_dir, out_dir){
  # Get path for output file
  if(missing(out_dir)) out_dir <- 'data/data_out/polys_ag_INEGI_noFMG'
  fp_out <- file.path(out_dir, stringr::str_c('inegi_noFMG_', estado, '.geojson'))
  
  # Don't proceed if file already exists
  if (file.exists(fp_out)) return(fp_out)
  
  # Load specific state from FMG
  sup_fmg <- load_and_preprocess_fmg(estado, fmg_dir, T, municipios)
  
  # Get CVE for state of FMG data
  cve_ent <- sup_fmg %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(CVE_ENT) %>% 
    dplyr::distinct %>% 
    deframe
  ag <- ag %>% 
    dplyr::mutate(CVE_ENT = as.integer(CVE_ENT)) %>% 
    dplyr::filter(CVE_ENT == cve_ent)
  
  # Remove FMG from ag
  inegi_noFMG <- clip_out_polys_from_ag(ag, sup_fmg)
  
  # Save 
  inegi_noFMG %>% sf::st_write(fp_out, delete_dsn=T)
  return(fp_out)
}

#' @export
remove_fmg_from_ag_by_muni <- function(cve_mun, ag, sup_fmg, dissolve_by=CLAVE){
  # Filter to municipio
  ag0 <- ag %>% 
    dplyr::filter(CVE_MUN == as.integer(cve_mun))
  sup_fmg0 <- sup_fmg %>% 
    dplyr::mutate(CVE_MUN = as.integer(CVE_MUN)) %>% 
    dplyr::filter(CVE_MUN == cve_mun)
  
  # Perform removal
  noFMG0 <- clip_out_polys_from_ag(ag0, sup_fmg0, dissolve_by=CLAVE)
  return(noFMG0)
}

#' @export
clip_out_polys_from_ag <- function(ag_by_munis, spec_polys, dissolve_by){
  # Remove FMG from agricultural areas
  # Preprocess ag and FMG
  ag_for_diff <- ag_by_munis  %>% 
    sf::st_transform(crs=6362) %>% 
    st_make_valid %>% 
    st_buffer(0)
  spec_for_diff <- spec_polys %>% 
    sf::st_transform(crs=6362) %>% 
    st_snap(x=., y=., tolerance=5) %>% 
    st_union %>% 
    st_make_valid %>% 
    st_buffer(0)
  
  # Remove FMG from ag
  ag_noFMG <- st_difference(ag_for_diff, spec_for_diff) %>% 
    st_collection_extract('POLYGON') %>% 
    st_make_valid
  
  if(missing(dissolve_by)){
    # If dissolve_by variable not specified, dissolve by all
    # List columns
    vars <- ag_for_diff %>% 
      dplyr::select(-area_ha) %>% 
      sf::st_drop_geometry() %>% 
      colnames()
    
    ag_noFMG <- ag_noFMG %>%
      dplyr::group_by %>% 
      summarise(dplyr::across(all_of(vars))) %>% 
      dplyr::ungroup
  } else {
    ag_noFMG <- ag_noFMG %>%
      dplyr::group_by(CVE_ENT, CVE_MUN, {{dissolve_by}}) %>% 
      summarise %>% 
      dplyr::ungroup
  }

  # Tidy the polygons - drop crumbs and fill pinholes
  ag_noFMG <- ag_noFMG %>%
    smoothr::fill_holes(threshold=1000) %>%
    st_buffer(-25) %>% 
    smoothr::drop_crumbs(4000) %>% 
    st_buffer(25, endCapStyle='FLAT', joinStyle='MITRE') %>% 
    smoothr::drop_crumbs(threshold=50000) %>%
    # st_snap(x=., y=., tolerance = 10) %>% 
    sf::st_transform(crs=4326) %>% 
    st_collection_extract('POLYGON') %>% 
    st_make_valid
  
  # Get area for new polygons
  ag_noFMG$area <- ag_noFMG %>% 
    sf::st_transform(crs=6362) %>% 
    sf::st_area %>% 
    set_units('ha') %>% 
    set_units(NULL)
  
  return(ag_noFMG)
}

#' @export
remove_FMG_from_ag_INEGI_largefile <- function(estado, ag, fmg_dir, out_dir, municipios){
  # Get path for output file
  if(missing(out_dir)) out_dir <- 'data/data_out/polys_ag_INEGI_noFMG'
  final_fp_out <- file.path(out_dir, stringr::str_c('inegi_noFMG_', estado, '.geojson'))
  
  # Don't proceed if file already exists
  if (file.exists(final_fp_out)) return(final_fp_out)
  
  # Get CVE code for state
  est_to_cve <- readRDS('data/helpers/lookup_state_codes.rds')
  cve_ent <- est_to_cve[[estado]]
  
  # Filter ag data to state of FMG data
  ag <- ag %>% 
    dplyr::mutate(CVE_ENT = as.integer(CVE_ENT),
           CVE_MUN = as.integer(CVE_MUN)) %>% 
    dplyr::filter(CVE_ENT == as.integer(cve_ent))

  # Load specific state from FMG
  sup_fmg <- 
    try({
      load_and_preprocess_fmg(estado, fmg_dir, T, municipios) %>% 
      dplyr::mutate(CVE_MUN = as.integer(CVE_MUN),
             CVE_ENT = as.integer(CVE_ENT)) %>% 
      dplyr::filter(CVE_ENT == as.integer(cve_ent))
    })
  if(class(sup_fmg) == 'try-error'){
    # Get agriculture in munis without FMG
    ag <- ag %>% 
      dplyr::group_by(CVE_ENT, CVE_MUN, CLAVE) %>% 
      summarize %>% 
      dplyr::ungroup %>% 
      sf::st_make_valid
    
    # Get area for new polygons
    ag$area <- ag %>% 
      sf::st_transform(crs=6362) %>% 
      sf::st_area %>% 
      set_units('ha') %>% 
      set_units(NULL)
    
    # Save
    ag %>% sf::st_write(final_fp_out)
    return(final_fp_out)
  }
  
  # List municipios present in both ag and FMG
  cve_muns_ag <- ag %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(CVE_MUN) %>% 
    dplyr::distinct 
  cve_muns_fmg <- sup_fmg %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(CVE_MUN) %>% 
    dplyr::distinct 
  cve_muns_int <- inner_join(cve_muns_ag, cve_muns_fmg) %>% deframe
  cve_muns_agonly <- anti_join(cve_muns_ag, cve_muns_fmg) %>% deframe

  # Process in for loop and save muni files
  state_dir <- file.path(out_dir, estado)
  unlink(state_dir, recursive=T)
  dir.create(state_dir)
  
  # Process munis with FMG 
  # FIX: Municipios where FMG is removed, end up with NA for TIP_INFO-NOM_MUN
  for (cve_mun in cve_muns_int){
    # Filter to municipio
    ag0 <- ag %>% 
      dplyr::filter(CVE_MUN == as.integer(cve_mun))
    sup_fmg0 <- sup_fmg %>% 
      dplyr::mutate(CVE_MUN = as.integer(CVE_MUN)) %>% 
      dplyr::filter(CVE_MUN == cve_mun)

    # Perform removal
    ag_noFMG0 <- clip_out_polys_from_ag(ag0, sup_fmg0, dissolve_by=CLAVE)
    
    # Save
    fp_out <- file.path(state_dir, 
                        stringr::str_c(estado, '_', cve_mun, '.geojson'))
    ag_noFMG0 %>% sf::st_write(fp_out, delete_dsn=T, driver='GeoJSON')
  }

  # Get agriculture in munis without FMG
  ag0 <- ag %>% 
    dplyr::filter(CVE_MUN %in% cve_muns_agonly) %>% 
    dplyr::group_by(CVE_ENT, CVE_MUN, CLAVE) %>% 
    summarize %>% 
    dplyr::ungroup %>% 
    sf::st_make_valid

  # Get area for new polygons
  ag0$area <- ag0 %>% 
    sf::st_transform(crs=6362) %>% 
    sf::st_area %>% 
    set_units('ha') %>% 
    set_units(NULL)
  
  # Save
  fp_out <- file.path(state_dir, 
                      stringr::str_c(estado, '_agX.geojson'))
  ag0 %>% sf::st_write(fp_out, delete_dsn=T, driver='GeoJSON')
  
  # Merge files from individual municipios
  fps <- list.files(state_dir, full.names=T)
  ag_noFMG <- fps %>% 
    lapply(sf::st_read) %>% 
    mapedit:::combine_list_of_sf()
  
  # Save
  ag_noFMG %>% sf::st_write(final_fp_out)
  return(final_fp_out)
}

#' @export
get_crop_polys <- function(var, season, crops_dir, ag_by_crop_dir, est_to_cve){

  # Load planted area table (created in prep_SIAP_data.R)
  area_cult_table <- readRDS(file.path("data/data_out/r_data",
                                       str_c('area_sembrada_', season, '_2019.RDS')))
  
  # Get matching crop name
  vars <- area_cult_table %>% colnames
  (var <- vars[[stringdist::amatch(var, vars, maxDist=1)]])
  
  # Get states where crop is grown
  state_cves <- area_cult_table %>% 
    dplyr::filter(across(all_of(var), ~ !is.na(.x))) %>% 
    dplyr::select(CVE_ENT) %>% 
    distinct
  
  # Get state codes
  est_to_cve_tbl <- est_to_cve %>% 
    as_tibble(rownames = 'estado') %>% 
    dplyr::filter(value %in% state_cves$CVE_ENT)
  
  # Get files matching state codes ----
  filter_polys <- function(fp, var, temp_dir){
    
    # Load polygons for state
    polys <- st_read(fp) %>% 
      mutate(total_noFMG = as.numeric(total_noFMG))
    
    # Get matching crop name
    vars <- polys %>% colnames
    print(var <- vars[[stringdist::amatch(var, vars, maxDist=4)]])
    
    # Filter to crop of interest
    out_polys <- polys %>%
      dplyr::filter(across(all_of(var), ~ !is.na(.x))) %>%
      dplyr::select(CVE_ENT:total_noFMG, all_of(var))
    
    # Save
    state <- basename(file_path_sans_ext(fp)) %>% 
      str_split('_') %>% last %>% last 
    fp_out <- file.path(temp_dir, str_c(var, '_', state, '.gpkg'))
    out_polys %>% st_write(fp_out, delete_dsn=T)
    
    # Return
    return(out_polys)
  }
  
  # Create temp dir
  temp_dir <- file.path(ag_by_crop_dir, 'temp')
  unlink(temp_dir, recursive=T)
  dir.create(temp_dir)
  
  # List state crop files to load, filter and merge
  cves_regex <- str_c(est_to_cve_tbl$estado, collapse='|')
  search_str <- str_c('.*_', season, '.*(', cves_regex, ')\\.(gpkg|geojson)')
  fps <- list.files(crops_dir, pattern=search_str, full.names=T)
  
  # Test
  # fp <- fps[[1]]
  # polys_crop <- filter_polys(fp, var, temp_dir)
  
  # Get crop polys for all relevant states and merge
  polys_all <- tryCatch(
    {
      fps %>% 
        purrr::map(filter_polys, var, temp_dir) %>%
        mapedit:::combine_list_of_sf()
    }, 
    
    error = function(...){
      print("\nError combining features. We'll merge those that were already created\n")
      
      list.files(temp_dir, pattern='.gpkg', full.names = T) %>% 
        purrr::map(st_read) %>% 
        mapedit:::combine_list_of_sf()
    }
  )
  
  # Change probabilities variable name to be consistent
  prob_name <- polys_all %>% colnames %>% nth(9)
  polys_all[['crop_prob']] <- polys_all[[prob_name]]
  
  # Calculate area of crop (instead of percentage)
  polys_all <- polys_all %>% 
    dplyr::select(-9) %>% # dplyr::select(matches(prob_name))
    dplyr::mutate(total_noFMG = as.numeric(total_noFMG), 
           crop_area = total_noFMG * crop_prob)
  
  # Save
  fp_out <- file.path(ag_by_crop_dir, str_c(prob_name, '_', season, '.gpkg'))
  polys_all %>% st_write(fp_out, delete_dsn=T)
  
  # Return
  return(polys_all)
}

#' @export
simplify_crop_polys <- function(polys, out_fp=NA, tol1=0.001, tol2=0.0005) {
  # Simplify polygons for mapping at national scale
  # Polygons in geographic projection
  # library(magrittr)
  
  pols_diss <- dplyr::summarise(dplyr::group_by(polys, crop_prob)) 
  pols_dis2 <- nngeo::st_remove_holes(pols_diss, .01) 
  pols_buf1 <- sf::st_buffer(pols_dis2, tol1) 
  pols_buf1 <- dplyr::summarise(dplyr::group_by(pols_buf1, crop_prob))
  pols_buf1 <- sf::st_buffer(pols_buf1, -tol1-0.0001)
  pols_buf1 <- nngeo::st_remove_holes(pols_buf1, .01) 
  pols_buf1 <- sf::st_simplify(pols_buf1, preserveTopology = T, dTolerance=tol2)
  
  if(!is.na(out_fp)) {
    sf::st_write(pols_buf1, out_fp, delete_dsn=T)
  }
  
  return(pols_buf1)
}

#' @export
model_species_rf <- function(sp_df,
                             pred, 
                             model_fp, 
                             sp_name,
                             eval_fp,
                             erf_fp,
                             mutually_exclusive_pa=TRUE,
                             unq_cells=TRUE,
                             imp_plot_fp=NULL) {
  
  # Presence points
  
  # Convert to coords DF
  sp1 <- sp_df %>% 
    dplyr::mutate(lon = unlist(purrr::map(.$geom, 1)), 
                  lat = unlist(purrr::map(.$geom, 2))) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(lon, lat)
  
  # Background points
  pres_pts <- sf::as_Spatial(sp_df)
  
  set.seed(10)
  if(mutually_exclusive_pa) {
    
    backg <- dismo::randomPoints(mask = pred, n=1000, p = pres_pts
                          # prob = T, # use mask as sampling bias grid
    )
  } else {
    
    backg <- dismo::randomPoints(pred, n=1000) 
  }
  
  backg <- backg %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(lon = 'x', lat = 'y')
  
  # Split presence into training and testing 
  set.seed(0)
  group <- dismo::kfold(sp1, 5)
  train_1 <- sp1[group != 1, ]
  test_1 <- sp1[group == 1, ]
  
  # Split background into training and testing
  set.seed(0)
  group <- dismo::kfold(backg, 5)
  train_0 <- backg[group != 1, ]
  test_0 <- backg[group == 1, ]
  
  # Extract environmental data 
  # Training dataset 
  train <- dplyr::bind_rows(train_1, train_0)
  envtrain1 <- raster::extract(pred, train, cellnumbers=T, df=T)
  
  pb_train <- c(rep(1, nrow(train_1)), rep(0, nrow(train_0)))
  envtrain1 <- data.frame( cbind(pa = pb_train, envtrain1) ) %>% 
    dplyr::mutate(dplyr::across(starts_with(c('biomes', 'ESA', 'usv')), as.factor)) %>% 
    dplyr::select(-ID)
  
  # Remove duplicated cells
  if(unq_cells) {
    envtrain1 <- envtrain1 %>% dplyr::distinct()
  }
  
  envtrain <- envtrain1 %>% dplyr::select(-cells)
  
  # Testing datasets - get predictors for test presence and background points
  testpres <- data.frame( raster::extract(pred, test_1) ) %>%
    dplyr::mutate(dplyr::across(starts_with(c('biomes', 'ESA', 'usv')), as.factor))
  
  testbackg <- data.frame( raster::extract(pred, test_0) ) %>%
    dplyr::mutate(dplyr::across(starts_with(c('biomes', 'ESA', 'usv')), as.factor))
  
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
  spc_eval <- tibble::tibble(
    species = sp_name,
    N_unq_pts = nrow(sp_df),
    N_unq_cells = nrow(dplyr::filter(envtrain, pa == 1)),
    np = erf@np, na = erf@na, auc = erf@auc,
    cor = erf@cor, pcor = erf@pcor, 
    spec_sens = dismo::threshold(erf, "spec_sens"))
  
  spc_eval %>% write_csv(eval_fp)
  
  if(!is.null(imp_plot_fp)){
    # Get plot directory
    dir.create(dirname(imp_plot_fp), recursive = T, showWarnings = F)
    
    # Save plot
    png(imp_plot_fp)
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
  raster::writeRaster(pr_rf1, likelihood_fp, overwrite=T, 
              options=c("dstnodata=-99999"), wopt=list(gdal='COMPRESS=LZW'))
  
  # Apply threshold from max TPR+TNR and save
  erf <- readRDS(erf_fp)
  tr <- threshold(erf, 'spec_sens')
  pa_rf1 <- pr_rf1 > tr
  
  # Save
  dir.create(dirname(binned_fp), recursive = T, showWarnings = F)
  raster::writeRaster(pa_rf1, binned_fp, overwrite=T,
              options=c("dstnodata=-99999"), wopt=list(gdal='COMPRESS=LZW'))
}

#' @export
stack_sdms <- function(sp_fps, rich_tif_fp, rich_plot_fp, mex){
  
  # Sum layers and save 
  pol_stack <- raster::stack(sp_fps)
  stack_n <- raster::nlayers(pol_stack)
  
  pol_rich <- sum(pol_stack, na.rm=T)
  pol_rich_msk <- raster::mask(pol_rich, 
                               sf::as_Spatial(mex))
  
  # Save
  raster::writeRaster(pol_rich_msk, 
                      rich_tif_fp, 
                      overwrite=T,
                      options=c("dstnodata=-99999"), 
                      wopt=list(gdal='COMPRESS=LZW'))
  
  # Plot richness
  pol_rich_stars <- stars::st_as_stars(pol_rich_msk)
  rich_plot  <- ggplot2::ggplot() +
    stars::geom_stars(data=pol_rich_stars) +
    ggplot2::geom_sf(data = mex, 
                     fill = "transparent", 
                     size = 0.2, 
                     color = scales::alpha("lightgray", 0.2)) +
    colormap::scale_fill_colormap(stringr::str_glue("Richness\n(N = {stack_n})"), 
                                  na.value = "transparent", 
                                  colormap = colormap::colormaps$viridis) +
    ggthemes::theme_hc() +
    ggplot2::theme(legend.position=c(.95, 1), 
                   legend.title.align=0, 
                   legend.justification = c(1,1)) +
    ggplot2::labs(x = NULL, y = NULL)
  
  # Save
  ggplot2::ggsave(rich_plot_fp, rich_plot, width=9, height=5.7, dpi=120)
  
  # Facet individual PA maps
  # pa_facets  <- rasterVis::gplot(pol_stack) +
  #   geom_tile(aes(fill = value)) +
  #   colormap::scale_fill_colormap(stringr::str_glue("Occupancy likelihood"), na.value = "transparent",
  #                                 colormap = colormap::colormaps$viridis) +
  #   theme_minimal() +
  #   theme(legend.position='bottom') +
  #   labs(x = NULL, y = NULL) +
  #   coord_equal() +
  #   facet_wrap(~ variable, nrow=3)
  # 
  # # Save
  # ggsave(file.path(rf_fig_dir, stringr::str_glue('Likhd_{nlayers(pol_stack)}species.png')), pa_facets, width=9, height=5)
  
}

# Initialize regions and states ----
#' @export
ag_dir <- 'data/input_data/ag_SIAP/FASII'

#' @export
regions <- c('CW_Col_Jal_Ags_Gto_Mich', 'C_Qro_Hgo_Mex_Tlax_Pue_Mor_DF',
             'CE_Tab_Ver', 'N_Chih_Coah_Dgo__SLP_Zac', 'NE_Tams_NL',
             'NW_BC_BCS_Son_Sin_Nay', 'S_Gro_Oax_Chis', 'SE_Camp_QRoo_Yuc')

#' @export
fmg_dir <- 'data/input_data/ag_SIAP/frijol_maiz_granos'

# SIAP FMG state codes
#' @export
est_to_cve <- c('AGS'=1, 'BC'=2, 'BCS'=3, 'CAM'=4, 'COAH'=5, 'COL'=6, 'CHIS'=7,
               'CHH'=8, 'DF'=9, 'DGO'=10, 'GTO'=11, 'GRO'=12, 'HGO'=13,
               'JAL'=14, 'MEX'=15, 'MICH'=16, 'MOR'=17, 'NAY'=18, 'NVL'=19,
               'OAX'=20, 'PUE'=21, 'QRO'=22, 'QROO'=23, 'SLP'=24, 'SIN'=25,
               'SON'=26, 'TAB'=27, 'TAM'=28, 'TLX'=29, 'VER'=30, 'YUC'=31, 'ZAC'=32)

# states by SIAP regions
#' states_by_region <- list(
#'   'CW_Col_Jal_Ags_Gto_Mich' = c('AGS', 'JAL', 'MICH','COL'),
#'   'C_Qro_Hgo_Mex_Tlax_Pue_Mor_DF' = c('QRO', 'HGO', 'TLX', 'MOR', 'DF'),
#'   'CE_Tab_Ver' = c('TAB', 'VER'),
#'   'N_Chih_Coah_Dgo__SLP_Zac' = c('CHH', 'COAH', 'DGO', 'SLP', 'ZAC', 'RL'),
#'   'NE_Tams_NL' = c('TAM', 'NVL'),
#'   'NW_BC_BCS_Son_Sin_Nay' = c('BC', 'BCS', 'SON', 'NAY'),
#'   'S_Gro_Oax_Chis' = c('GRO', 'OAX', 'CHIS'),
#'   'SE_Camp_QRoo_Yuc' = c('CAM', 'QROO', 'YUC')) %>% 
#'   enframe('region', 'state') %>% unnest_longer(state)

#' @export
states_by_region <- tidyr::unnest_longer(
  tibble::enframe(
    list(
      'CW_Col_Jal_Ags_Gto_Mich' = c('AGS', 'JAL', 'MICH','COL'),
      'C_Qro_Hgo_Mex_Tlax_Pue_Mor_DF' = c('QRO', 'HGO', 'TLX', 'MOR', 'DF'),
      'CE_Tab_Ver' = c('TAB', 'VER'),
      'N_Chih_Coah_Dgo__SLP_Zac' = c('CHH', 'COAH', 'DGO', 'SLP', 'ZAC', 'RL'),
      'NE_Tams_NL' = c('TAM', 'NVL'),
      'NW_BC_BCS_Son_Sin_Nay' = c('BC', 'BCS', 'SON', 'NAY'),
      'S_Gro_Oax_Chis' = c('GRO', 'OAX', 'CHIS'),
      'SE_Camp_QRoo_Yuc' = c('CAM', 'QROO', 'YUC')
      ), 
    'region', 'state'
    ),
  state
  )

# est_to_cve %>% saveRDS('data/helpers/lookup_state_codes.rds')
# states_by_region %>% saveRDS('data/helpers/states_by_region.rds')
# save(ag_dir, regions, fmg_dir, est_codes, states_by_region, est_to_cve,
#      file='data/helpers/initial_vars.RData')

# Save functions ----
# functions <- lsf.str()
# save(list=functions, file='data/helpers/functions.RData')







