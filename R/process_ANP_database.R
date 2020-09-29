# 
# Join pollinator data to Areas Naturales Protegidas (ANPs)
# 

# Load libraries ---------------------------------------------------------------
library(sf)
library(units)
library(tidyverse)
library(tmap)
tmap_mode('view')
library(mapview)
library(patchwork)

# Initialize -------------------------------------------------------------------
buffer_distance <- set_units(1, 'km')
anp_fp <- 'data/input_data/context_Mexico/SHAPE_ANPS/182ANP_Geo_ITRF08_Julio04_2019.shp'
anp_dir <- 'data/data_out/ANPs'
anp_terr_fp <- file.path(anp_dir, 'ANPs_terr_singlepart.geojson')
pol_groups <- c('Abejas', 'Avispas', 'Colibries', 'Mariposas', 'Moscas', 'Murcielagos')
date_range <- c(2010, 2020)
# Mexico
mex <- raster::getData('GADM', country='MEX', level=0, 
                       path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) %>% 
  st_transform(crs=6372) %>% 
  st_simplify(dTolerance=20)

# Load data --------------------------------------------------------------------
# Mexico 
# mex <- raster::getData(
#   'GADM', country='MEX', level=0, path='data/input_data/context_Mexico') %>% 
#   st_as_sf(crs=4326) %>% 
#   st_simplify(dTolerance = 0.02)
# mex_munis <- raster::getData(
#   'GADM', country='MEX', level=2, path='data/input_data/context_Mexico') %>% 
#   st_as_sf(crs=4326) 
# mex_est <- raster::getData(
#   'GADM', country='MEX', level=1, path='data/input_data/context_Mexico') %>% 
#   st_as_sf(crs=4326) 
# 
# # Biomes
# data_dir <- 'data/input_data/environment_variables/TEOW_WWF_biome'
# shp_fp <- list.files(data_dir, '.shp$', full.names = T, recursive = T)
# biomes_crop <- st_read(shp_fp) %>% 
#   st_crop(st_bbox(mex) )
# 
# # Elevation
# alt <- raster::getData(
#   'alt', country='MEX', path='data/input_data/context_Mexico')

# Functions --------------------------------------------------------------------
count_pollinators_in_polys <- function(
  pts_sfc, polys_sfc, polys_id_fld='ID_ANP', polys_area_fld='area_ha'
){
  # Group pollinator points by intersecting ANP 
  pts_sfc %>% 
    # Group pollinator points by intersecting ANP 
    st_transform(crs = st_crs(polys_sfc)) %>% 
    st_join(polys_sfc, left=F) %>% 
    st_drop_geometry %>% 
    # st_set_geometry(NULL) %>% 
    group_by(.data[[polys_id_fld]], .data[[polys_area_fld]]) %>% 
    # Get diversity and abundance of species
    summarize(no_spcs = length(unique(species)), 
              no_obs = length(species)) %>% 
    ungroup %>% 
    # Normalize by terrestrial area
    mutate(spcs_per_ha = no_spcs/.data[[polys_area_fld]], 
           obs_per_ha = no_obs/.data[[polys_area_fld]])
}

# Function
get_buffer_conditional <- function(buffer_distance, anps_proj, area_fld='buff_area_ha'){
  # NPA buffer ---- 
  buffer_fp <- str_c('data/intermediate_data/anps_buffer_', buffer_distance,'km.geojson')
  
  if(file.exists(buffer_fp)) {
    
    # Load buffer
    anps_buff <- st_read(buffer_fp)
    
  } else {
    
    print(str_glue('Creating ANP buffer of {buffer_distance} km...'))
    
    # Create buffer
    anps_buff <- anps_proj %>% 
      st_buffer(buffer_distance, validate=T) %>% 
      st_difference(st_union(anps_proj))
    
    # Get area
    anps_buff[[area_fld]] <- anps_buff %>% 
      st_area %>% 
      units::set_units('ha') %>% 
      units::set_units(NULL)
    
    # Save
    anps_buff %>% st_write(buffer_fp, delete_dsn=T)
  }
  return(anps_buff)
}

make_voronois <- function(anps_terr, mex){
  # Make Voronoi polygons from ANP polygons 
  
  if(missing(mex)){
    # Load land polygon
    mex <- raster::getData('GADM', country='MEX', level=0, 
                           path='data/input_data/context_Mexico') %>% 
      st_as_sf(crs=4326) %>% 
      st_transform(crs=6372) %>% 
      st_simplify(dTolerance=20)
  }
  
  # Dissolve polys into one
  anp_union <- anps_terr %>% 
    st_simplify(dTolerance=40) %>% 
    st_union
  
  # voronois
  vpols <- anp_union %>% 
    # Voronoi polygons
    st_voronoi %>% 
    st_cast %>% 
    st_sf %>% 
    
    # join feature IDs (rowname) from ANPs
    st_join(anps_terr, join=st_nearest_feature) %>% 
    
    # Dissolve by IDs
    group_by(rowname) %>% 
    summarize %>% 
    ungroup %>% 
    
    # Clip out ANPs - processing is faster with difference as the last step
    st_difference(anp_union) %>% 
    
    # Clip to land
    st_intersection(mex) %>% 
    st_collection_extract('POLYGON') %>% 
    group_by(rowname) %>% 
    summarize %>% 
    ungroup
  
  # Get area
  vpols$area_ha <- vpols %>% 
    st_area %>% set_units('ha') %>% set_units(NULL)
  
  return(vpols)
}

get_voronois_conditional <- function(anps_proj, buffer_distance){
  # Save
  vrnoi_fp <- str_c('data/data_out/ANPs/voronoi_outside_anps_and_buffer_', 
                    buffer_distance,'km.geojson')
  if(file.exists(vrnoi_fp)){
    
    # Load file if it already exists 
    st_read(vrnoi_fp)
    
  } else {
    
    # Create buffer polygons
    anps_buff <- anps_proj %>% 
      st_buffer(buffer_distance, validate=T) 
    
    # Make polygons
    vpols <- make_voronois(anps_buff)
    
    # Save
    vpols %>% st_write(vrnoi_fp, delete_dsn=T)
    
  }
  
  return(vpols)
}

get_pollinators_in_anp_zones <- function(name, date_range, anps_proj, buffer_distance, polys_id_fld='rowname'){
  
  # Convert date range
  date_min <- as.POSIXct(str_c(date_range[[1]], "-01-01"))
  date_max <- as.POSIXct(str_c(date_range[[2]], "-12-31"))
  
  # Load pollinator file
  pol_fp <- file.path('data/data_out/pollinator_points', str_c(name, '.geojson'))
  df <- st_read(pol_fp) %>% 
    # Filter to date range
    filter(eventDate >= date_min & eventDate <= date_max) 
  
  # Get diversity and abundance of pollinators within NPA
  anps2 <- count_pollinators_in_polys(df, anps_proj, polys_id_fld = polys_id_fld)
  
  # NPA buffer
  anps_buff <- get_buffer_conditional(buffer_distance, anps_proj, area_fld='buff_area_ha')
  
  # Get diversity and abundance of pollinators in NPA buffer
  anps3 <- count_pollinators_in_polys(df, anps_buff, polys_id_fld = polys_id_fld, polys_area_fld='buff_area_ha') %>% 
    full_join(anps2, ., by = polys_id_fld, suffix=c('', '_cerca'))
  

  # Voronoi polygons
  vpols <- get_voronois_conditional(anps_proj, buffer_distance)
  
  # Get diversity and abundance of pollinators outside NPA buffer
  anps4 <- count_pollinators_in_polys(df, vpols, polys_id_fld = polys_id_fld, polys_area_fld='area_ha') %>% 
    full_join(anps3, ., by = polys_id_fld, suffix=c('', '_afuera'))
  
  anps4$pol_group <- name
  
  # Save
  fp_out <- file.path('data/data_out/ANPs', 
                      str_c('anps_', name, '_', strftime(date_min, format="%Y"), 
                            '_to_', strftime(date_max, format="%Y"), '_buffer', 
                            buffer_distance, 'km.csv'))
  anps4 %>% write_csv(fp_out)
  
  # Return
  return(anps4)
}

# Create database --------------------------------------------------------------
# ANPs
anps <- st_read(anp_fp)

# Prep ANP polys for processing
anps_proj <- anps %>% 
  select(ID_ANP) %>% 
  st_transform(crs=6372) %>% 
  st_set_precision(1e5)  %>% 
  st_make_valid %>% 
  st_collection_extract('POLYGON') %>% 
  st_simplify(dTolerance=20) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON") %>%
  rownames_to_column

# Clip to terrestrial portions using Mexico boundary
anps_terr <- anps_proj %>% 
  st_intersection(mex)

# Get area
anps_terr$area_ha <- anps_terr %>% 
  st_area %>% set_units('ha') %>% set_units(NULL)

# Save
anps_terr %>% st_write(anp_terr_fp, delete_dsn=T)

# Pollinators
out_anps <- pol_groups %>% 
  map(get_pollinators_in_anp_zones, date_range, anps_terr, buffer_distance)
  
out_allpols <- out_anps %>% 
  reduce(rbind)

out_allpols_wide <- out_allpols %>% 
  pivot_wider(id_cols = rowname, names_from = pol_group, 
              values_from = no_spcs:obs_per_ha_afuera)

fp_out <- file.path('data/data_out/ANPs', 
                    str_c('anps_allpols_', strftime(date_min, format="%Y"), 
                          '_to_', strftime(date_max, format="%Y"), '_buffer', 
                          buffer_distance, 'km.csv'))
out_allpols %>% write_csv(fp_out)

# Load previously-created data -------------------------------------------------
fp_out <- file.path('data/data_out/ANPs', 
                    str_c('anps_allpols_', strftime(date_min, format="%Y"), 
                          '_to_', strftime(date_max, format="%Y"), '_buffer', 
                          buffer_distance, 'km.csv'))
anps_stats <- read_csv(fp_out, col_types='cniinnniinnniinnc') %>% 
  mutate(rowname = as.character(rowname))


# Data to longer format and plot -----------------------------------------------
# Initialize zone codes
buff_str <- str_c('dentro de ', buffer_distance, ' km')
outside_str <- str_c('más de ', buffer_distance, ' km')

# Abundance (plot) ----
anps_stats_longer_obs_dens <- anps_stats %>% 
  pivot_longer(cols = matches('obs_per_ha'), names_to = 'zone', 
               names_prefix = 'obs_per_ha_', values_to = 'obs_per_ha') %>% 
  mutate(zone = zone %>% 
           recode(obs_per_ha = 'adentro', 
                  cerca = buff_str, 
                  afuera = outside_str) %>% 
           factor(levels = c('adentro', buff_str, outside_str))) %>% 
  select(rowname, pol_group, zone, obs_per_ha)

# Boxplot
(abun_all_box <- ggplot(anps_stats_longer_obs_dens, aes(x=obs_per_ha, y=pol_group)) +
    geom_boxplot(aes(fill=zone), position=position_dodge(.9), outlier.shape=NA) + 
    coord_flip(xlim = c(0,0.036)) +
    scale_fill_brewer(palette="Set2", 
                      name = 'Relación con las ANPs', 
                      guide=guide_legend(order=1)) +
    # scale_x_sqrt() +
    labs(x ='Cantidad de observaciones por hectarea', 
         y = 'Polinizador',
         title = 'Abundancia de observaciones') +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45),
      legend.position = 'bottom'
    ) +
    # Add mean point
    stat_summary(fun=mean, geom="point", aes(group=zone), 
                 position=position_dodge(.9), size=2) +
    geom_point(aes(shape = "media"), alpha = 0) +
    guides(shape=guide_legend(title=NULL, override.aes = list(alpha = 1), 
                              order=2)))

# Bar chart - Mean observation density
obs_dns_means <- anps_stats_longer_obs_dens %>% 
  group_by(pol_group, zone) %>% 
  summarize(avg_obs_per_ha = mean(obs_per_ha, na.rm=T))

(abun_all <- ggplot(obs_dns_means, aes(x=avg_obs_per_ha, y=pol_group, fill=zone)) +
  geom_bar(stat = 'identity', position='dodge') +
  coord_flip() + 
  labs(fill='Relación con las ANPs', 
       x ='Media de observaciones por hectarea', 
       y = 'Polinizador',
       title = 'Abundancia de observaciones')+
  theme_minimal() +
  theme(
    axis.title.x=element_blank(),
    axis.text.x = element_text(angle = 45)
  ) +
  scale_fill_brewer(palette="Set2"))

# Species diversity (plot) ----
anps_stats_longer_spc_dens <- anps_stats %>% 
  pivot_longer(cols = matches('spcs_per_ha'), names_to = 'zone', 
               names_prefix = 'spcs_per_ha_', values_to = 'spcs_per_ha') %>% 
  mutate(zone = zone %>% 
           recode(spcs_per_ha = 'adentro', 
                  cerca = buff_str, 
                  afuera = outside_str) %>% 
           factor(levels = c('adentro', buff_str, outside_str))) %>% 
  select(rowname, pol_group, zone, spcs_per_ha)

# Boxplot
(div_all_box <- ggplot(anps_stats_longer_spc_dens, aes(x=spcs_per_ha, y=pol_group)) +
    geom_boxplot(aes(fill=zone), position=position_dodge(.9), outlier.shape=NA) +
    scale_fill_brewer(palette="Set2", name = 'Relación con las ANPs', 
                      guide=guide_legend(order=1)) +
    coord_flip(xlim = c(0,0.012)) +
    # scale_x_sqrt() +
    labs(
      # fill='Relación con las ANPs', 
         x ='Densidad de especies distintas por hectarea', 
         y = 'Polinizador',
         title = 'Diversidad de especies') +
    theme_minimal() +
    theme(
      axis.title.x=element_blank(),
      axis.text.x = element_text(angle = 45),
      legend.position = 'bottom'
    ) +
    # Add mean point
    stat_summary(fun=mean, geom="point", aes(group=zone), 
                 position=position_dodge(.9), size=2) +
    geom_point(aes(shape = "media"), alpha = 0) +
    guides(shape = guide_legend(title=NULL, override.aes = list(alpha = 1), order=2)))

# Bar chart - Mean species diversity density
spc_dns_means <- anps_stats_longer_spc_dens %>% 
  group_by(pol_group, zone) %>% 
  summarize(avg_species_per_ha = mean(spcs_per_ha, na.rm=T))

div_all <- ggplot(spc_dns_means, aes(x=avg_species_per_ha, y=pol_group, fill=zone)) +
  geom_bar(stat = 'identity', position='dodge') +
  coord_flip() + 
  labs(fill='Relación con las ANPs', 
       x ='Media de especies distintas por hectarea', 
       y = 'Polinizador',
       title = 'Diversidad de especies')+
  theme_minimal() +
  theme(
    axis.title.x=element_blank(),
    axis.text.x = element_text(angle = 45)
  ) +
  scale_fill_brewer(palette="Set2")


# Layout (boxplots) ----
combined <- abun_all_box + div_all_box & theme(legend.position = "right")
combined + plot_layout(guides = "collect")

# Save
fig_dir <- 'figures/anps_and_pollinator_exploration'
fp_out <- file.path(fig_dir, str_glue('boxplot_abunYdiv_2010to2020_buffer1km.png'))
ggsave(fp_out, width = 12, height=7)

# Join to polygons -------------------------------------------------------------
# Load polygons
anps_terr <- st_read(anp_terr_fp)

# Convert table to wide format
anps_stats_wide <- anps_stats %>% 
  pivot_wider(id_cols = rowname, names_from = pol_group, 
              values_from = no_spcs:obs_per_ha_afuera)

# Join wide table to polygons
anps_join <- anps_terr %>% 
  select(rowname, ID_ANP) %>% 
  left_join(anps_stats_wide, by='rowname')

# Look
var <- 'obs_per_ha_Colibries'

limit <- max(abs(anps_join[[var]]), na.rm=T) * c(-1, 1)
limit <- c(0,0.04)
(p3 <- ggplot() +
    geom_sf(data=mex, fill='lightgray', lwd=0.3) +
  geom_sf(data = anps_join, 
    aes(fill = .data[[var]]), 
    # alpha=0.6,
    lwd=NA
  ) +
  
  scale_fill_viridis_c(option = "viridis", name = var,
    limits= limit, oob = scales::squish) +
  # scale_fill_distiller(
  #   type='div',
  #   palette='YlGnBu',
  #   limit= limit,
  #   direction=1,
  #   name = "Abundancia por ha"
  # ) +
  
  theme_void())

# Join wide table to polygons
anps_join <- anps_terr %>% 
  select(rowname, ID_ANP) %>% 
  left_join(anps_stats_longer_obs_dens, by='rowname')

# Look
pol_grp <- 'Colibries'
var <- 'obs_per_ha'
zone <- 'adentro'

anps_filt <- anps_join %>% 
  filter(pol_group == pol_grp, 
         zone == zone)

limit <- max(abs(anps_filt[[var]]), na.rm=T) * c(-1, 1)
limit <- c(0,0.04)
(p3 <- ggplot(anps_filt) +
    geom_sf(
      aes(fill = .data[[var]]), 
      lwd=NA
    ) +
    
    scale_fill_viridis_c(option = "magma", name = "Gi* Statistic", 
                         limits= limit, oob = scales::squish) +
    # scale_fill_distiller(
    #   type='div',
    #   palette='YlGnBu',
    #   limit= limit,
    #   direction=1,
    #   name = "Abundancia por ha"
    # ) +
    
    theme_void())


# ----

tm_shape(vpols_mex) + tm_fill(col='rowname') + tm_borders() +
  tm_shape(anps_terr) + tm_fill(col='rowname') + tm_borders()

mapview(anps3, zcol='obs_per_ha') +
  mapview(df)


