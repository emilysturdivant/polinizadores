# 
# Join pollinator data to Areas Naturales Protegidas (ANPs)
# 

# Load libraries ---------------------------------------------------------------
library(sf)
library(units)
library(tmap)
tmap_mode('view')
library(mapview)
library(patchwork)
library(vegan)
library(tidyverse)
library(viridis)

# Initialize -------------------------------------------------------------------
buffer_distance <- set_units(10, 'km')
date_range <- c(2000, 2020)
date_min <- as.POSIXct(str_c(date_range[[1]], "-01-01"))
date_max <- as.POSIXct(str_c(date_range[[2]], "-12-31"))

anp_fp <- 'data/input_data/context_Mexico/SHAPE_ANPS/182ANP_Geo_ITRF08_Julio04_2019.shp'
anp_dir <- 'data/data_out/ANPs'
anp_terr_fp <- file.path(anp_dir, 'ANPs_terr_singlepart.geojson')
pol_groups <- c('Abejas', 'Avispas', 'Colibries', 'Mariposas', 'Moscas', 'Murcielagos')

anp_stats_fp <- file.path('data/data_out/ANPs', 
                    str_c('anps_allpols_', strftime(date_min, format="%Y"), 
                          '_to_', strftime(date_max, format="%Y"), '_buffer', 
                          buffer_distance, 'km.csv'))

# Mexico
mex <- raster::getData('GADM', country='MEX', level=0, 
                       path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) %>% 
  st_transform(crs=6372) %>% 
  st_simplify(dTolerance=20)

# Load data --------------------------------------------------------------------
# # Biomes
# shp_fp <- 'data/input_data/environment_variables/CONABIO/ecort08gw.shp'
# biomes <- st_read(shp_fp)
# 
# # Elevation
# alt <- raster::getData(
#   'alt', country='MEX', path='data/input_data/context_Mexico')

# Functions --------------------------------------------------------------------
brillouin <- function(x) {
  # Brillouin Index (HB) is a modification of the Shannon-Wiener Index that 
  # is preferred when sample randomness cannot be guaranteed. 
  # Use Brobdingnag to be able to calculate greater than factorial(170)
  N <- sum(x, na.rm = T)
  # Stirling's approximation of factorial
  stirling <- function(n){n^n*exp(-n)*sqrt(2*pi*n)}
  sum_logs <- x[x != 0] %>% 
    map(function(x){
      Brobdingnag::as.brob(x) %>% 
        stirling %>% 
        log
    }) %>% 
    flatten_dbl %>% 
    sum(na.rm=T)
  (log(stirling(Brobdingnag::as.brob(N))) - sum_logs)/N
}

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
              no_obs = length(species),
              # Species richness
              D = no_spcs/sqrt(no_obs),
              # Species diversity, Shannon index
              H = ) %>% 
    ungroup %>% 
    # Normalize by terrestrial area
    mutate(spcs_per_ha = no_spcs/.data[[polys_area_fld]], 
           obs_per_ha = no_obs/.data[[polys_area_fld]])
}

get_diversity_metrics <- function(df, polys_id_fld = 'rowname', polys_area_fld = 'area_ha'){
  # Group by ANP and pivot wide (separate column with count for each species)
  spec_df <- df %>% 
    st_drop_geometry %>% 
    group_by(.data[[polys_id_fld]], .data[[polys_area_fld]], species) %>% 
    summarize(n = length(species)) %>% 
    ungroup %>% 
    pivot_wider(id_cols = all_of(c(polys_id_fld, polys_area_fld)), 
                names_from = species, values_from = n) %>%
    replace(is.na(.), 0)
  
  # Get names of species columns
  spec_names <- df %>% st_drop_geometry %>% select(species) %>% distinct %>% deframe
  
  # Calculate simple richness and two indices
  div_df <- spec_df %>% 
    rowwise() %>%
    mutate(
      richness = sum(c_across(all_of(spec_names)) > 0, na.rm=T), # simple richness
      richness_norm = richness / .data[[polys_area_fld]], 
      abundance = sum(c_across(all_of(spec_names)), na.rm=T), # simple abundance
      D_menhinick = richness/sqrt(abundance), # Menhinick's index
      D_margalef = (richness-1)/log(abundance), # Margalef's index
      # Rarefaction
      # rarefy = rarefy(c_across(matches('.* .*')), sample=10, MARGIN=1), # rarefy
      # Diversity (Shannon-Weiner index) - most common index
      shannon = diversity(c_across(all_of(spec_names)), index='shannon'), # H'
      # Diversity Brillouin Index (HB) - preferred when sample randomness cannot be guaranteed
      brillouin = brillouin(c_across(where(is.numeric))), # HB
      # Diversity (Simpson's index)
      simpson = diversity(c_across(all_of(spec_names)), index='simpson'), # lambda
      # Evenness - Hill's ratios
      even_hill_shan = exp(shannon) / richness,
      # True diversity
      true_shannon = exp(shannon),
    ) %>%
    ungroup %>%
    select(-all_of(spec_names))
  
  # Density / normalized abundance
  dens_df <- spec_df %>% 
    # density for each species
    mutate(across(all_of(spec_names), ~ .x/.data[[polys_area_fld]])) %>% 
    rowwise() %>%
    mutate(
      # sum densities
      abundance_norm = sum(c_across(all_of(spec_names)), na.rm=T), # normalized abundance
      # D_menhinick = n/sqrt(abundance_norm), # Menhinick's index
      # D_margalef = (n-1)/log(abundance_norm), # Margalef's index
    ) %>% 
    select(.data[[polys_id_fld]], abundance_norm)
  
  # Join normalized abundance to the diversity metrics
  div_df <- full_join(div_df, dens_df, by=polys_id_fld)
  
  # Return
  return(div_df)
}

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
    vpols <- st_read(vrnoi_fp)
    
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

get_diversity_for_zone_polys <- function(df, zone_polys, zone_str, polys_id_fld, polys_area_fld){
  df <- df %>% 
    # Group pollinator points by intersecting ANP 
    st_transform(crs = st_crs(zone_polys)) %>% 
    st_join(zone_polys, left=F) %>% {
      if(nrow(.) > 0) {
        
        # Get diversity and abundance of species
        get_diversity_metrics(., polys_id_fld, polys_area_fld)
        
      } else {
        tibble(!!polys_id_fld := character(), !!polys_area_fld := numeric())
        } # Make empty tibble if there are no pollinators in the given zone
    }
  
  # Get just the polygon ID and area
  all_ids <- zone_polys %>% 
    st_drop_geometry() %>% 
    select(.data[[polys_id_fld]], .data[[polys_area_fld]])
  
  # Join so that we have NA anywhere statistics are missing
  full_join(all_ids, df) %>% 
    # Set zone
    mutate(zone = zone_str)
}

get_pollinators_in_anp_zones <- function(name, date_range, anps_terr, 
                                         buffer_distance, 
                                         polys_id_fld='rowname', 
                                         polys_area_fld = 'area_ha'){
  
  # Zone codes
  inside_str <- str_c('Inside NPA')
  buff_str <- str_c('Buffer ', buffer_distance, ' km')
  outside_str <- str_c('Outside buffer')
  
  # Convert date range
  date_min <- as.POSIXct(str_c(date_range[[1]], "-01-01"))
  date_max <- as.POSIXct(str_c(date_range[[2]], "-12-31"))
  
  # Load pollinator file
  pol_fp <- file.path('data/data_out/pollinator_points', str_c(name, '.geojson'))
  df <- st_read(pol_fp) %>% 
    # Filter to date range
    filter(eventDate >= date_min & eventDate <= date_max) 
  
  # Get diversity and abundance of pollinators within NPA ----
  anps_inside <- get_diversity_for_zone_polys(df, anps_terr, inside_str, polys_id_fld, polys_area_fld)
  
  # NPA buffer ----
  anps_buff <- get_buffer_conditional(buffer_distance, anps_terr, area_fld='area_ha')
  
  # Get diversity and abundance of pollinators in NPA buffer
  anps_buffer <- get_diversity_for_zone_polys(df, anps_buff, buff_str, polys_id_fld, polys_area_fld)
  
  # Voronoi polygons ----
  vpols <- get_voronois_conditional(anps_terr, buffer_distance)
  
  # Get diversity and abundance of pollinators outside NPA buffer
  anps_outside <- get_diversity_for_zone_polys(df, vpols, outside_str, polys_id_fld, polys_area_fld)
  
  # Concatenate the three DFs
  anps_df <- bind_rows(anps_inside, anps_buffer, anps_outside)

  # Set pollinator group name
  anps_df$pol_group <- name
  
  # Save
  fp_out <- file.path('data/data_out/ANPs', 
                      str_c('anps_', name, '_', strftime(date_min, format="%Y"), 
                            '_to_', strftime(date_max, format="%Y"), '_buffer', 
                            buffer_distance, 'km.csv'))
  anps_df %>% write_csv(fp_out)
  
  # Return
  return(anps_df)
}

# Prep ANP polys for processing ================================================
anps_proj <- st_read(anp_fp) %>% 
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

# Create database ==============================================================
anps_terr<- st_read(anp_terr_fp)

# Initialize zone codes
inside_str = 'Inside NPA'
buff_str <- str_c('Buffer ', buffer_distance, ' km')
outside_str <- str_c('Outside buffer')

# Pollinators
anps_stats <- pol_groups %>% 
  map(get_pollinators_in_anp_zones, date_range, anps_terr, buffer_distance) %>% 
  reduce(rbind) %>% 
  mutate(zone = factor(zone, levels = c(inside_str, buff_str, outside_str)))

anps_stats %>% write_csv(anp_stats_fp)

# Load previously-created data =================================================
# Initialize zone codes
inside_str = 'Inside NPA'
buff_str <- str_c('Buffer ', buffer_distance, ' km')
outside_str <- str_c('Outside buffer')

# Get lookup table for rownames to ID_ANP
anps_terr<- st_read(anp_terr_fp)
row2id <- anps_terr %>% st_drop_geometry %>% select(rowname, ID_ANP)

# Read data and perform some tidying
anps_stats <- read_csv(anp_stats_fp) %>% 
  mutate(rowname = as.character(rowname),
         across(everything(), replace_na, 0),
         zone = factor(zone, levels = c(inside_str, buff_str, outside_str)), 
         richness_norm = richness_norm * 100, 
         abundance_norm = abundance_norm * 100) %>% 
  full_join(row2id)

# Visualize --------------------------------------------------------------------
# Plotting functions -----
plot_stats_boxjitter_by_stat <- function(df, stat, stat_name, percentile){
  
  # Prep for plot
  # Filter to ANP subsets with values in at least one zone
  df_long <- df %>% 
    group_by(rowname, pol_group) %>% 
    filter(sum(abundance) > 0) %>% 
    ungroup %>% 
    pivot_longer(cols=all_of(stats_list), names_to = 'statistic') %>% 
    filter(statistic == stat)
  
  # Function to filter DF and make plot
  make_plot <- function(df_long, pol, percentile){
    # Plot each pollinator
    df_filt <- df_long %>% 
      filter(pol_group == pol)
    
    # Get counts of ANPs and ANP subsets 
    n_filt_anp_subs <- df_filt %>% select(rowname) %>% distinct %>% nrow
    n_filt_anps <- df_filt %>% select(ID_ANP) %>% distinct %>% nrow
    
    # Make plot
    plot_title <- str_glue('{pol}, N = {n_filt_anp_subs} NPA subsets ({n_filt_anps} NPAs)')
    display_stat_by_zones(df_filt, plot_title, percentile)
  }
  
  # Make plots
  p1 <- make_plot(df_long, 'Colibries', percentile)
  p2 <- make_plot(df_long, 'Mariposas', percentile)
  p3 <- make_plot(df_long, 'Abejas', percentile)
  p4 <- make_plot(df_long, 'Murcielagos', percentile)
  p5 <- make_plot(df_long, 'Avispas', percentile)
  p6 <- make_plot(df_long, 'Moscas', percentile)
  
  # Put it all together
  p1 / p2 / p3 / p4 / p5 / p6 + 
    plot_annotation(
      title = str_glue('{stat_name}')
    )
}

plot_stats_boxjitter_by_polgroup <- function(df, pol){
  stats_list <- c('richness_norm', 'abundance_norm', 
                  'brillouin', 'shannon')
  
  # Prep for plot
  # Filter to ANP subsets with values in at least one zone
  df_long <- df %>% 
    group_by(rowname, pol_group) %>% 
    filter(sum(abundance) > 0) %>% 
    ungroup %>% 
    pivot_longer(cols=all_of(stats_list), names_to = 'statistic') %>% 
    mutate(statistic = factor(statistic, levels = stats_list))
  
  df_filt <- df_long %>% 
    filter(pol_group == pol)
  
  # Plot each statistic
  stat <- 'abundance_norm'
  stat_name <- 'Normalized abundance (observations per km^2)'
  percentile <- 0.95
  p_abun <- display_stat_by_zones2(df_long, stat, stat_name, percentile)
  
  stat <- 'richness_norm'
  stat_name <- 'Normalized richness (unique species per km^2)'
  percentile <- 0.95
  p_rich <- display_stat_by_zones2(df_long, stat, stat_name, percentile)
  
  stat <- 'brillouin'
  stat_name <- 'Brillouin Index'
  percentile <- 0.95
  p_brill <- display_stat_by_zones2(df_long, stat, stat_name, percentile)
  
  stat <- 'shannon'
  stat_name <- 'Shannon-Wiener Index'
  percentile <- 1
  p_shann <- display_stat_by_zones2(df_long, stat, stat_name, percentile)
  
  # Get counts of ANPs and ANP subsets 
  n_filt_anp_subs <- df_long %>% select(rowname) %>% distinct %>% nrow
  n_filt_anps <- df_long %>% select(ID_ANP) %>% distinct %>% nrow
  
  # Put it all together
  p_abun / p_rich / p_brill / p_shann + 
    plot_annotation(
      title = str_glue('{pol}'),
      subtitle = str_glue('N = {n_filt_anp_subs} NPA subsets (comprising {n_filt_anps} NPAs)')
    )
}

display_stat_by_zones2 <- function(df_long, stat, stat_name, percentile){
  # Filter to stat
  df_filt <- df_long %>% 
    filter(statistic == stat)
  
  p1 <- display_stat_by_zones(df_filt, stat_name, percentile)
}

display_stat_by_zones <- function(df_filt, stat_name, percentile){
  theme_desc <- function () { 
    theme_gray() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(size = 10),
        plot.title = element_text(size = 10),
        plot.title.position = "plot",
        legend.position = "none"
      )
  }
  
  # Boxplot of all data
  box_full <- ggplot(df_filt, aes(x=zone, y=value, color=zone)) +
    # geom_jitter(size = 1, alpha = 0.5) +
    geom_boxplot(outlier.size=1, outlier.alpha = 0.5,
                 color='black', fill=NA, width = .3) +
    # scale_color_viridis(discrete=TRUE) +
    # geom_violin(width=2.1, size=0.2, color='red', fill=NA) +
    theme_desc() +
    scale_x_discrete(limits = rev(levels(df_filt$zone))) +
    coord_flip() +
    labs(title=stat_name)
  
  # Get upper axis limit for given statistic
  ylim_max <- df_filt$value %>% quantile(probs=percentile, na.rm=T) %>% deframe
  
  # Jitter points with boxplot
  jitter_zoom <- ggplot(df_filt, aes(x=zone, y=value, color=zone)) +
    geom_jitter(size = 1, alpha = 0.5) + 
    scale_color_viridis(discrete=TRUE) +
    geom_boxplot(outlier.shape=NA, color='black', fill=NA, width = .2) +
    # geom_violin(width=2.1, size=0.2, color='red', fill=NA) + 
    theme_desc() +
    scale_x_discrete(limits = rev(levels(df_filt$zone))) +
    coord_flip(ylim=c(0, ylim_max)) +
    labs(title=str_c('Zoom < ', percentile, ' percentile'))
  
  # Assemble plot
  p1 <- (box_full | jitter_zoom)
}

# Group separate parts into the original 182 ANPs ----
anps_182 <- full_join(anps_stats, row2id) %>% 
  group_by(ID_ANP, pol_group, zone) %>% 
  summarize(richness = sum(richness),
            richness_norm = mean(richness_norm), 
            abundance = sum(abundance), 
            abundance_norm = mean(abundance_norm), 
            D_menhinick = mean(D_menhinick), 
            D_margalef = mean(D_margalef), 
            shannon = mean(shannon), 
            brillouin = mean(brillouin), 
            simpson = mean(simpson), 
            even_hill_shan = mean(even_hill_shan), 
            true_shannon = mean(true_shannon)) %>% 
  ungroup

# All pollinators, one statistic ----
fig_dir <- 'figures/anps_and_pollinator_exploration'

stat_tbl <- tibble(
  stat = c('richness_norm', 'abundance_norm', 
           'brillouin', 'shannon'), 
  stat_name = c('Normalized richness (unique species per km^2)', 
                'Normalized abundance (observations per km^2)', 
                'Brillouin Index', 
                'Shannon-Wiener Index'), 
  pcntl_zoom = c(0.95, 0.95, 0.95, 1)
)

for(pol in pol_groups){
  plot_stats_boxjitter_by_polgroup(anps_stats, pol)
  
  # Save
  ggsave(file.path(fig_dir, str_c(pol, '_boxjitter_buffer', buffer_distance, 'km_', date_min, 'to', date_max, '.png')), 
         width = 9.15, height=6.03)
}

for(i in seq(1, nrow(stat_tbl))){
  stat <- stat_tbl[i, 'stat'] %>% deframe
  stat_name <- stat_tbl[i, 'stat_name'] %>% deframe
  percentile <- stat_tbl[i, 'pcntl_zoom'] %>% deframe
  
  # Make full plot 
  plot_stats_boxjitter_by_stat(anps_stats, stat, stat_name, percentile)
  
  # Save
  ggsave(file.path(fig_dir, str_c(stat, '_boxjitter_buffer', buffer_distance, 'km.png')), 
         width = 9.15, height=8.03)
}

# Old versions =================================================================
# Boxplot
(box_4stats_1pol <- ggplot(anps_182_long, aes(x=zone, y=value, fill=zone)) +
    geom_boxplot(position=position_dodge(.9),
                 outlier.alpha=0.5, outlier.size = 1
    ) + 
    coord_flip(
      ylim=c(0, 30)
      ) +
    scale_fill_viridis(discrete=TRUE, name = 'Zone', 
                                         guide=guide_legend(order=1)) +
    # theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # axis.text.x = element_text(angle = 45),
      legend.position = 'bottom'
    ) +
    # Add mean point
    stat_summary(fun=mean, geom="point", aes(group=zone), 
                 position=position_dodge(.9), size=2) +
    geom_point(aes(shape = "mean"), alpha = 0) +
    guides(shape=guide_legend(title=NULL, override.aes = list(alpha = 1), 
                              order=2)) +
    
    facet_wrap(~ .data[['statistic']], nrow = 4, scales = 'free'))

# Violin plot
(box_4stats_1pol <- ggplot(anps_182_long, aes(x=zone, y=value, fill=zone, color=zone)) +
    geom_violin(width=2.1, size=0.2) + 
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    # theme_minimal() +
    theme(
      legend.position = 'bottom'
    ) +
    coord_flip() +
    
    facet_wrap(~ .data[['statistic']], scales = 'free'))




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


