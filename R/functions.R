# Load libraries ----
library(sf)
library(tidyverse)
library(magrittr)
library(rvest)
library(tools)
library(mapview)
library(units)

# Functions ----
html_to_df <- function(x){
  # get values from description field
  x %>% 
    minimal_html %>% 
    html_node('table') %>% 
    html_table(fill=T) %>% 
    select(X1, X2) %>% 
    slice(-1:-2) %>% 
    pivot_wider(names_from=X1, values_from=X2)
}

load_kmz_as_sf <- function(kmz_url, data_dir, out_fp, col_types=NULL){
  # Download KMZ and convert to sf dataframe
  kmz_name <- kmz_url %>% basename()
  download.file(kmz_url, dest=kmz_name)
  sf <- kmz_name %>% 
    unzip(list=F, exdir=data_dir) %>% 
    st_read
  unlink(kmz_name)
  
  # Convert KML description field to DF
  df <- sf$description %>%
    map_dfr(html_to_df) 
  if(length(col_types)>0){
    df %<>% 
      type_convert(col_types=col_types, trim_ws=T)
  }
  
  # Replace KML variables with DF
  sf %<>%
    select(Name) %>%
    cbind(df) %>%
    st_zm(drop=T, what='ZM') %>% 
    st_make_valid

  return(sf)
}

intersect_with_munis <- function(sup_ag, municipios){
  # Intersect with municipios and simplify
  ag_munis <- sup_ag  %>%
    st_simplify(dTolerance = 0.0001) %>% 
    st_intersection(municipios)  %>% 
    group_by(CVE_ENT, CVE_MUN, MOD_AGR) %>% 
    summarize(geometry = st_union(geometry)) %>% 
    as_tibble %>% 
    st_as_sf
  
  # get area for new polygons in ha
  ag_munis$area <- ag_munis %>% 
    st_transform(crs=6362) %>% 
    st_area %>% 
    set_units('ha') %>% 
    set_units(NULL)
  
  return(ag_munis)
}

load_and_preprocess_ag <- function(region, data_dir, municipios, return_df=T){
  fp_ag <- file.path(data_dir, str_c('FASII_', region, '.geojson'))
  
  if(!file.exists(fp_ag)){
    url_ag <- str_c('http://infosiap.siap.gob.mx/gobmx/datosAbiertos/', 
                    'frontera-agricola/FASII_', region, '.kmz')
    sup_ag <- load_kmz_as_sf(url_ag, data_dir, 'iicicicdcc') 
    if ('CVE_EDO' %in% colnames(sup_ag)) {
      sup_ag <- sup_ag %>% rename(CVE_ENT=CVE_EDO)
    }
    
    # Intersect with municipios
    sup_ag <- intersect_with_munis(sup_ag, municipios)
    
    # Save to file
    sup_ag %>% st_write(fp_ag, delete_dsn=T)
  } else {
    if(return_df) return(st_read(fp_ag)) else return(fp_ag)
  } 
  if(return_df) return(sup_ag) else return(fp_ag)
}

preprocess_fmg <- function(sup_frij_gran, munis){
  # Prepare munis for intersection
  mun_for_join <- munis %>%
    mutate(CVE_ENT=as.integer(CVE_ENT),
           CVE_MUN=as.integer(CVE_MUN)) %>% 
    select(-AREA, -PERIMETER, -COV_, -COV_ID)
  
  # Simplify to remove jagged edges and decrease size
  sup_frij_gran <- sup_frij_gran %>% 
    st_simplify(preserveTopology = T, dTolerance = .0001)
  
  # Get intersection with municipios
  sup_frij_gran <- sup_frij_gran %>% 
    group_by(CULTIVO) %>% 
    summarize(geometry = st_union(geometry)) %>% 
    st_intersection(mun_for_join) 
  
  # Add area column
  sup_frij_gran$area_ha <- sup_frij_gran %>% 
    st_transform(crs=6362) %>% 
    st_area %>% 
    set_units('ha') %>% 
    set_units(NULL)
  
  if(!all(!is.na(sup_frij_gran$CVE_MUN))){
    print('WARNING: not all municipios matched')
  }
  
  return(sup_frij_gran)
}

load_and_preprocess_fmg <- function(estado_code, data_dir, return_df, municipios, fp_fmg, prefix='ESA_PV2015_'){
  # Get filename
  if(missing(fp_fmg)){
    fp_fmg <- file.path(data_dir, str_c(prefix, estado_code, '.geojson'))
  }
  
  if(file.exists(fp_fmg)){
    # Read file
    sup_frij_gran <- st_read(fp_fmg) 
  } else {
    try({
      # Download and convert from KMZ
      url_fmg <- str_c('http://infosiap.siap.gob.mx/gobmx/datosAbiertos/', 
                       'estimacion-superficie-agricola/ESA_PV2015_', estado_code, '.kmz')
      sup_frij_gran <- load_kmz_as_sf(url_fmg, data_dir)
    
      # Process
      sup_frij_gran <- preprocess_fmg(sup_frij_gran, municipios)
    
      # Save
      st_write(sup_frij_gran, fp_fmg, delete_dsn=T)
    })
  }
  if(return_df) return(sup_frij_gran) else return(TRUE)
}



get_area_planted <- function(data, state_id_fld, muni_id_fld, crop_name_fld, 
                             riego_id_fld, planted_fld, planted_area_fld){
  # Summarize crops by municipio, percent of total planted area

  # get total sembrada for each crop by municipio
  cultivo_totals <- data %>% 
    group_by({{state_id_fld}}, {{ muni_id_fld }}, {{ crop_name_fld }}, {{riego_id_fld}}) %>% 
    summarize({{planted_fld}} := sum({{planted_fld}}, na.rm=T)) %>%
    spread({{ crop_name_fld }}, {{planted_fld}})
  
  # Quote variable name arguments as quosures
  state_id_fld <- rlang::enquo(state_id_fld)
  muni_id_fld <- rlang::enquo(muni_id_fld)
  riego_id_fld <- rlang::enquo(riego_id_fld)
  
  # get total sembrada area by municipio
  data %>% 
    group_by({{state_id_fld}}, {{muni_id_fld}}, {{riego_id_fld}}) %>% 
    summarize({{planted_area_fld}} := sum({{planted_fld}})) %>% 
    full_join(cultivo_totals, 
              by=c(quo_name(state_id_fld), quo_name(muni_id_fld), quo_name(riego_id_fld))) %>% 
    as_tibble()
}

remove_FMG_from_ag_by_state <- function(estado, ag_dir, fmg_dir, out_dir, states_by_region){
  # Get path for output file
  if(missing(out_dir)) out_dir <- 'data/data_out/polys_ag_SIAP_noFMG'
  fp_out <- file.path(out_dir, str_c('siap_noFMG_', estado, '.geojson'))
  
  # Don't proceed if file already exists
  if (file.exists(fp_out)) return(fp_out)
  
  # Get region code (ag) from state code (sup_fmg)
  if(missing(states_by_region)) {
    states_by_region <- readRDS('data/helpers/states_by_region.rds')
  }
  region <- states_by_region %>% 
    filter(state==estado) %>% 
    select(region) %>% 
    deframe
  
  # Load specific zone from ag data and specific state from FMG
  ag <- load_and_preprocess_ag(region, ag_dir)
  sup_fmg <- load_and_preprocess_fmg(estado, fmg_dir, T, municipios, prefix='fmg_siap15_')
  
  # Get CVE for state of FMG data
  cve_ent <- sup_fmg %>% 
    st_set_geometry(NULL) %>% 
    select(CVE_ENT) %>% 
    distinct %>% 
    deframe
  ag <- ag %>% 
    mutate(CVE_ENT = as.integer(CVE_ENT)) %>% 
    filter(CVE_ENT == cve_ent)
  
  # Remove FMG from ag
  siap_noFMG <- clip_out_polys_from_ag(ag, sup_fmg)
  
  # Save 
  siap_noFMG %>% st_write(fp_out, delete_dsn=T)
  return(fp_out)
}

remove_FMG_from_ag_INEGI <- function(estado, ag, fmg_dir, out_dir){
  # Get path for output file
  if(missing(out_dir)) out_dir <- 'data/data_out/polys_ag_INEGI_noFMG'
  fp_out <- file.path(out_dir, str_c('inegi_noFMG_', estado, '.geojson'))
  
  # Don't proceed if file already exists
  if (file.exists(fp_out)) return(fp_out)
  
  # Load specific state from FMG
  sup_fmg <- load_and_preprocess_fmg(estado, fmg_dir, T, municipios, prefix='fmg_siap15_')
  
  # Get CVE for state of FMG data
  cve_ent <- sup_fmg %>% 
    st_set_geometry(NULL) %>% 
    select(CVE_ENT) %>% 
    distinct %>% 
    deframe
  ag <- ag %>% 
    mutate(CVE_ENT = as.integer(CVE_ENT)) %>% 
    filter(CVE_ENT == cve_ent)
  
  # Remove FMG from ag
  inegi_noFMG <- clip_out_polys_from_ag(ag, sup_fmg)
  
  # Save 
  inegi_noFMG %>% st_write(fp_out, delete_dsn=T)
  return(fp_out)
}

remove_fmg_from_ag_by_muni <- function(cve_mun, ag, sup_fmg, dissolve_by=CLAVE){
  # Filter to municipio
  ag0 <- ag %>% 
    filter(CVE_MUN == as.integer(cve_mun))
  sup_fmg0 <- sup_fmg %>% 
    mutate(CVE_MUN = as.integer(CVE_MUN)) %>% 
    filter(CVE_MUN == cve_mun)
  
  # Perform removal
  noFMG0 <- clip_out_polys_from_ag(ag0, sup_fmg0, dissolve_by=CLAVE)
  return(noFMG0)
}

clip_out_polys_from_ag <- function(ag_by_munis, spec_polys, dissolve_by){
  # Remove FMG from agricultural areas
  # Preprocess ag and FMG
  ag_for_diff <- ag_by_munis  %>% 
    st_transform(crs=6362) %>% 
    st_make_valid %>% 
    st_buffer(0)
  spec_for_diff <- spec_polys %>% 
    st_transform(crs=6362) %>% 
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
      select(-area_ha) %>% 
      st_set_geometry(NULL) %>% 
      colnames()
    
    ag_noFMG <- ag_noFMG %>%
      group_by %>% 
      summarise(across(all_of(vars))) %>% 
      ungroup
  } else {
    ag_noFMG <- ag_noFMG %>%
      group_by(CVE_ENT, CVE_MUN, {{dissolve_by}}) %>% 
      summarise %>% 
      ungroup
  }

  # Tidy the polygons - drop crumbs and fill pinholes
  ag_noFMG <- ag_noFMG %>%
    smoothr::fill_holes(threshold=1000) %>%
    st_buffer(-25) %>% 
    smoothr::drop_crumbs(4000) %>% 
    st_buffer(25, endCapStyle='FLAT', joinStyle='MITRE') %>% 
    smoothr::drop_crumbs(threshold=50000) %>%
    # st_snap(x=., y=., tolerance = 10) %>% 
    st_transform(crs=4326) %>% 
    st_collection_extract('POLYGON') %>% 
    st_make_valid
  
  # Get area for new polygons
  ag_noFMG$area <- ag_noFMG %>% 
    st_transform(crs=6362) %>% 
    st_area %>% 
    set_units('ha') %>% 
    set_units(NULL)
  
  return(ag_noFMG)
}

remove_FMG_from_ag_INEGI_largefile <- function(estado, ag, fmg_dir, out_dir, municipios){
  # Get path for output file
  if(missing(out_dir)) out_dir <- 'data/data_out/polys_ag_INEGI_noFMG'
  final_fp_out <- file.path(out_dir, str_c('inegi_noFMG_', estado, '.geojson'))
  
  # Don't proceed if file already exists
  if (file.exists(final_fp_out)) return(final_fp_out)
  
  # Get CVE code for state
  est_to_cve <- readRDS('data/helpers/lookup_state_codes.rds')
  cve_ent <- est_to_cve[[estado]]
  
  # Filter ag data to state of FMG data
  ag <- ag %>% 
    mutate(CVE_ENT = as.integer(CVE_ENT),
           CVE_MUN = as.integer(CVE_MUN)) %>% 
    filter(CVE_ENT == as.integer(cve_ent))

  # Load specific state from FMG
  sup_fmg <- 
    try({
      load_and_preprocess_fmg(estado, fmg_dir, T, municipios, prefix='fmg_siap15_') %>% 
      mutate(CVE_MUN = as.integer(CVE_MUN),
             CVE_ENT = as.integer(CVE_ENT)) %>% 
      filter(CVE_ENT == as.integer(cve_ent))
    })
  if(class(sup_fmg) == 'try-error'){
    # Get agriculture in munis without FMG
    ag <- ag %>% 
      group_by(CVE_ENT, CVE_MUN, CLAVE) %>% 
      summarize %>% 
      ungroup %>% 
      st_make_valid
    
    # Get area for new polygons
    ag$area <- ag %>% 
      st_transform(crs=6362) %>% 
      st_area %>% 
      set_units('ha') %>% 
      set_units(NULL)
    
    # Save
    ag %>% st_write(final_fp_out)
    return(final_fp_out)
  }
  
  # List municipios present in both ag and FMG
  cve_muns_ag <- ag %>% 
    st_set_geometry(NULL) %>% 
    select(CVE_MUN) %>% 
    distinct 
  cve_muns_fmg <- sup_fmg %>% 
    st_set_geometry(NULL) %>% 
    select(CVE_MUN) %>% 
    distinct 
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
      filter(CVE_MUN == as.integer(cve_mun))
    sup_fmg0 <- sup_fmg %>% 
      mutate(CVE_MUN = as.integer(CVE_MUN)) %>% 
      filter(CVE_MUN == cve_mun)

    # Perform removal
    ag_noFMG0 <- clip_out_polys_from_ag(ag0, sup_fmg0, dissolve_by=CLAVE)
    
    # Save
    fp_out <- file.path(state_dir, 
                        str_c(estado, '_', cve_mun, '.geojson'))
    ag_noFMG0 %>% st_write(fp_out, delete_dsn=T, driver='GeoJSON')
  }

  # Get agriculture in munis without FMG
  ag0 <- ag %>% 
    filter(CVE_MUN %in% cve_muns_agonly) %>% 
    group_by(CVE_ENT, CVE_MUN, CLAVE) %>% 
    summarize %>% 
    ungroup %>% 
    st_make_valid

  # Get area for new polygons
  ag0$area <- ag0 %>% 
    st_transform(crs=6362) %>% 
    st_area %>% 
    set_units('ha') %>% 
    set_units(NULL)
  
  # Save
  fp_out <- file.path(state_dir, 
                      str_c(estado, '_agX.geojson'))
  ag0 %>% st_write(fp_out, delete_dsn=T, driver='GeoJSON')
  
  # Merge files from individual municipios
  fps <- list.files(state_dir, full.names=T)
  ag_noFMG <- fps %>% 
    lapply(st_read) %>% 
    mapedit:::combine_list_of_sf()
  
  # Save
  ag_noFMG %>% st_write(final_fp_out)
  return(final_fp_out)
}

# Save functions ----
functions <- lsf.str()
save(list=functions, file='data/helpers/functions.RData')

# Initialize regions and states ---
ag_dir <- 'data/input_data/SIAP/FASII'
regions <- c('CW_Col_Jal_Ags_Gto_Mich', 'C_Qro_Hgo_Mex_Tlax_Pue_Mor_DF', 
             'CE_Tab_Ver', 'N_Chih_Coah_Dgo__SLP_Zac', 'NE_Tams_NL', 
             'NW_BC_BCS_Son_Sin_Nay', 'S_Gro_Oax_Chis', 'SE_Camp_QRoo_Yuc')
fmg_dir <- 'data/input_data/SIAP/frijol_maiz_granos'
# SIAP FMG state codes
est_to_cve <- c('AGS'=1, 'BC'=2, 'BCS'=3, 'CAM'=4, 'COAH'=5, 'COL'=6, 'CHIS'=7, 
               'CHH'=8, 'DF'=9, 'DGO'=10, 'GTO'=11, 'GRO'=12, 'HGO'=13, 
               'JAL'=14, 'MEX'=15, 'MICH'=16, 'MOR'=17, 'NAY'=18, 'NVL'=19, 
               'OAX'=20, 'PUE'=21, 'QRO'=22, 'QROO'=23, 'SLP'=24, 'SIN'=25, 
               'SON'=26, 'TAB'=27, 'TAM'=28, 'TLX'=29, 'VER'=30, 'YUC'=31, 'ZAC'=32)
est_to_cve %>% saveRDS('data/helpers/lookup_state_codes.rds')

# states by SIAP regions
states_by_region <- list(
  'CW_Col_Jal_Ags_Gto_Mich' = c('AGS', 'JAL', 'MICH','COL'),
  'C_Qro_Hgo_Mex_Tlax_Pue_Mor_DF' = c('QRO', 'HGO', 'TLX', 'MOR', 'DF'), 
  'CE_Tab_Ver' = c('TAB', 'VER'), 
  'N_Chih_Coah_Dgo__SLP_Zac' = c('CHH', 'COAH', 'DGO', 'SLP', 'ZAC', 'RL'), 
  'NE_Tams_NL' = c('TAM', 'NVL'), 
  'NW_BC_BCS_Son_Sin_Nay' = c('BC', 'BCS', 'SON', 'NAY'), 
  'S_Gro_Oax_Chis' = c('GRO', 'OAX', 'CHIS'), 
  'SE_Camp_QRoo_Yuc' = c('CAM', 'QROO', 'YUC')
) %>% enframe('region', 'state') %>% unnest_longer(state)
states_by_region %>% saveRDS('data/helpers/states_by_region.rds')

save(ag_dir, regions, fmg_dir, est_codes, states_by_region, est_to_cve,
     file='data/helpers/initial_vars.RData')







