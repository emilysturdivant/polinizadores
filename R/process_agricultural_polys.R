
# Load libraries ----
library(sf)
library(tidyverse)
library(magrittr)
library(rvest)
library(tools)
library(mapview)
library(units)

# Load pre-created objects (from prep_SIAP_data.R)
load('data/helpers/initial_vars.RData')
load('data/helpers/functions.RData')
out_dir <- 'data/data_out/polys_ag_INEGI_noFMG'
fmg_dir <- 'data/data_out/polys_fmg'

# Remove frijol, maiz, granos from INEGI polygons ----
# INEGI
inegi_polys <- st_read('data/data_out/polys_ag_INEGI.geojson')
# Municipio polygons 
fp_munis <- 'data/input_data/context_Mexico/SNIB_divisionpolitica/muni_2018gw/muni_2018gw.shp'
munis <- st_read(fp_munis) %>% 
  mutate(CVE_ENT=as.integer(CVE_ENT),
         CVE_MUN=as.integer(CVE_MUN))

# Run per state
estado = 'PUE' 

# Separate by muni
remove_FMG_from_ag_INEGI_largefile(estado, inegi_polys, fmg_dir, municipios=munis)

# Join SIAP crop stats to polygons ---------------------------------------------
load("data/data_out/r_data/area_sembrada_by_season_2019.RData")

join_siap_crop_stats_to_polys <- function(estado){
  cve_ent <- est_to_cve[[estado]]
  final_fp_out <- file.path('data/data_out/polys_ag_INEGI_wFMG_pcts', 
                            str_c('inegi_pcts_prim19_', estado, '.geojson'))
  if (file.exists(final_fp_out)) return(final_fp_out)
  
  # Add polys from FMG with pct == 1.0 ----
  sup_fmg <- 
    try({
      sup_fmg <- load_and_preprocess_fmg(estado, fmg_dir, T, munis, prefix='fmg_siap15_') %>% 
        mutate(CVE_MUN = as.integer(CVE_MUN),
               CVE_ENT = as.integer(CVE_ENT)) %>% 
        filter(CVE_ENT == as.integer(cve_ent))
      
      # Create FMG columns
      try({
        sup_fmg <- sup_fmg %>%
          rename('area' = 'area_ha')
      })
      sup_fmg <- sup_fmg %>%
        mutate(CULTIVO = 
                 stringi::stri_trans_general(str=CULTIVO, id='Latin-ASCII') %>%
                 str_trim %>%
                 str_to_lower,
               'Frijol' = recode(CULTIVO, 'frijol' = 1.0, .default=NA_real_), 
               'Maíz grano' = recode(CULTIVO, 'maiz grano' = 1.0, .default=NA_real_), 
               'Sorgo grano' = recode(CULTIVO, 'sorgo grano' = 1.0, .default=NA_real_),
               'Trigo grano' = recode(CULTIVO, 'trigo grano' = 1.0, .default=NA_real_),
               count_crops = 1, 
               total_sembrada = area) %>% 
        select(-contains(c('CULTIVO', 'NOM_MUN', 'DELEGACIÓN', 'Name', 'NOM_ENT', 'CVEGEO')))
    })

  # Ag polys ----
  # Get path for input file
  ag_dir <- 'data/data_out/polys_ag_INEGI_noFMG'
  ag_fp <- file.path(ag_dir, str_c('inegi_noFMG_', estado, '.geojson'))
  
  # Read data
  ag_noFMG <- st_read(ag_fp)
  
  # create column that indicates Temporal o Riego (Idmodalidad) based on CLAVE
  ag_noFMG <- ag_noFMG %>% 
    mutate(CLAVE = str_replace(CLAVE, 'H', 'T'), 
           Idmodalidad = str_extract(CLAVE, '[HNTR]') %>% 
             str_replace_all('H|N', 'T')) %>% 
    filter(str_detect(CLAVE, '^[NRT]')) 
  
  # SIAP stats to percents ------
  # Filter [primavera] to state, remove and columns with all NA values
  df <- area_cult_prim %>% 
    filter(CVE_ENT==cve_ent) %>% 
    select_if(~sum(!is.na(.)) > 0)
  if(class(sup_fmg) != 'try-error'){
    # Remove FMG columns if FMG data exists
    df <- df %>% 
      select(!contains(c('maíz', 'frijol', 'sorgo', 'trigo', 'triticale'))) 
  }
  
  # List columns
  vars <- df %>% 
    select(-CVE_ENT:-total_sembrada) %>% 
    colnames()
  
  # Get new total area 
  df$total_noFMG <- df %>% 
    select(all_of(vars)) %>% 
    rowSums(na.rm=T)
  df %<>% relocate(total_noFMG, .after=total_sembrada)
  
  # Get number of different crops planted
  df$count_crops <- df %>% 
    select(all_of(vars)) %>% 
    is.na %>% 
    `!` %>% 
    rowSums
  df %<>% relocate(count_crops, .before=total_sembrada)
  
  # Get percent of agricultural land of each crop 
  pct_sembrada_noFMG <- df %>% 
    mutate(across(
      .cols = all_of(vars), 
      .fns = ~ .x / total_noFMG
    )) 
  
  # Join ------
  ag_pct_cult_noFMG <- left_join(ag_noFMG, pct_sembrada_noFMG, 
                                 by=c('CVE_ENT', 'CVE_MUN', 'Idmodalidad'))
  
  # Combine with general ag
  if(class(sup_fmg) != 'try-error'){
    sup_new <- mapedit:::combine_list_of_sf(list(ag_pct_cult_noFMG, sup_fmg))
  } else {
    sup_new <- ag_pct_cult_noFMG
  }

  # mapview(sup_new) #+ mapview(sup_fmg)
  
  # ***** SAVE ***** ----
  sup_new %>% 
    st_write(final_fp_out, delete_dsn=T)
  return(TRUE)
}

# estado <- 'BC'
for(estado in names(est_to_cve)){
  join_siap_crop_stats_to_polys(estado)
}

