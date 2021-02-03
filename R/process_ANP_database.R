# 
# Join pollinator data to Areas Naturales Protegidas (ANPs)
# 

# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(units)
library(tmap)
tmap_mode('view')
library(mapview)
library(patchwork)
library(vegan)
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
  # NPA buffer 
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

get_voronois_conditional <- function(anps_proj, buffer_distance, vrnoi_fp=NA){
  # Save
  if(is.na(vrnoi_fp)){
    
    vrnoi_fp <- str_c('data/data_out/ANPs/voronoi_outside_anps_and_buffer_', 
                      buffer_distance,'km.geojson')
    
  }

  if(file.exists(vrnoi_fp)){
    
    # Load file if it already exists 
    vpols <- st_read(vrnoi_fp)
    
  } else {
    
    # Create buffer polygons
    anps_buff <- anps_proj %>% 
      st_buffer(buffer_distance, validate=T) 
    
    # Make polygons
    vpols <- make_voronois(anps_terr=anps_buff)
    
    # Save
    vpols %>% st_write(vrnoi_fp, delete_dsn=T)
    
  }
  
  return(vpols)
}

get_diversity_for_zone_polys <- function(df, zone_polys, zone_str=NA, polys_id_fld, 
                                         polys_area_fld='area_ha'){
  
  df <- df %>% 
    # Group pollinator points by intersecting ANP 
    st_transform(crs = st_crs(zone_polys)) %>% 
    st_join(zone_polys, left=F) %>% {
      if(nrow(.) > 0) {
        
        # Get diversity and abundance of species
        get_diversity_metrics(., polys_id_fld, polys_area_fld)
        
      } else {
        
        # Make empty tibble if there are no pollinators in the given zone
        tibble(!!polys_id_fld := character(), !!polys_area_fld := numeric())
        
        } 
    }
  
  # Get just the polygon ID and area
  all_ids <- zone_polys %>% 
    st_drop_geometry() %>% 
    select(.data[[polys_id_fld]], .data[[polys_area_fld]])
  
  # Join so that we have NA anywhere statistics are missing
  df <- full_join(all_ids, df)
  
  if(!is.na(zone_str)){
    
    df <- mutate(df, zone = zone_str)
    
  }
  
  # Return
  return(df)
  
}

get_pollinators_in_anp_zones <- function(name, date_range, anps_terr, 
                                         buffer_distance, 
                                         polys_id_fld='rowname', 
                                         polys_area_fld = 'area_ha',
                                         pol_dir = 'data/data_out/pollinator_points'){
  
  # Zone codes
  inside_str <- str_c('Inside NPA')
  buff_str <- str_c('Buffer ', buffer_distance, ' km')
  outside_str <- str_c('Outside buffer')
  
  # Convert date range
  date_min <- as.POSIXct(str_c(date_range[[1]], "-01-01"))
  date_max <- as.POSIXct(str_c(date_range[[2]], "-12-31"))
  
  # Load pollinator file
  pol_fp <- file.path(pol_dir, str_c(name, '.geojson'))
  df <- st_read(pol_fp) %>% 
    # Filter to date range
    filter(eventDate >= date_min & eventDate <= date_max) 
  
  # Get diversity and abundance of pollinators within NPA ----
  anps_inside <- get_diversity_for_zone_polys(df, anps_terr, inside_str, 
                                              polys_id_fld, polys_area_fld)
  
  # NPA buffer ----
  anps_buff <- get_buffer_conditional(buffer_distance, anps_terr, area_fld='area_ha')
  
  # Get diversity and abundance of pollinators in NPA buffer
  anps_buffer <- get_diversity_for_zone_polys(df, anps_buff, buff_str, 
                                              polys_id_fld, polys_area_fld)
  
  # Voronoi polygons ----
  vpols <- get_voronois_conditional(anps_terr, buffer_distance)
  
  # Get diversity and abundance of pollinators outside NPA buffer
  anps_outside <- get_diversity_for_zone_polys(df, vpols, outside_str, 
                                               polys_id_fld, polys_area_fld)
  
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

# Plotting functions -----
plot_stats_boxjitter_by_stat <- function(df, stat){
  
  # stat <- stat_tbl[i, 'stat'] %>% deframe
  # stat_name <- stat_tbl[i, 'stat_name'] %>% deframe
  # percentile <- stat_tbl[i, 'pcntl_zoom'] %>% deframe
  
  stat_tbl <- tibble(
    stat_code = c('richness_norm', 
             'abundance_norm', 
             'brillouin', 
             'shannon', 
             'simpson', 
             'true_shannon'), 
    stat_name = c('Normalized richness (unique species per km^2)', 
                  'Normalized abundance (observations per km^2)', 
                  'Brillouin Index', 
                  'Shannon-Wiener Index',
                  'Simpson',
                  'Exponential Shannon'), 
    pcntl_zoom = c(0.95, 0.95, 0.95, 1, 0.95, 0.95)
  )
  stat_name <- stat_tbl %>% filter(stat_code == stat) %>% select(stat_name) %>% deframe
  percentile <- stat_tbl %>% filter(stat_code == stat) %>% select(pcntl_zoom) %>% deframe
  
  # Prep for plot
  # Filter to ANP subsets with values in at least one zone
  df_long <- df %>% 
    group_by(rowname, pol_group) %>% 
    filter(sum(abundance) > 0) %>% 
    ungroup %>% 
    pivot_longer(cols=all_of(stat_tbl$stat_code), names_to = 'statistic') %>% 
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
  p_abun <- display_stat_by_zones2(df_filt, stat, stat_name, percentile)
  
  stat <- 'richness_norm'
  stat_name <- 'Normalized richness (unique species per km^2)'
  percentile <- 0.95
  p_rich <- display_stat_by_zones2(df_filt, stat, stat_name, percentile)
  
  stat <- 'brillouin'
  stat_name <- 'Brillouin Index'
  percentile <- 0.95
  p_brill <- display_stat_by_zones2(df_filt, stat, stat_name, percentile)
  
  stat <- 'shannon'
  stat_name <- 'Shannon-Wiener Index'
  percentile <- 1
  p_shann <- display_stat_by_zones2(df_filt, stat, stat_name, percentile)
  
  # Get counts of ANPs and ANP subsets 
  n_filt_anp_subs <- df_filt %>% select(rowname) %>% distinct %>% nrow
  n_filt_anps <- df_filt %>% select(ID_ANP) %>% distinct %>% nrow
  
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

display_by_biomes_zones <- function(df_filt, plot_title, percentile,
                                    biome_var = 'DESECON1', facet_by_zone=F,
                                    stat_name = "Brillouin Index"){
  
  theme_desc <- function () { 
    theme_gray() +
      theme(
        axis.title.x = element_text(size = 9),
        axis.title.y = element_blank(),
        plot.subtitle = element_text(size = 10),
        plot.title = element_text(size = 10),
        plot.title.position = "plot",
        legend.position = "bottom",
        legend.title = element_text(size = 10)
      )
  }
  
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  biome_cnt <- df_filt %>% 
    filter(!is.na(value)) %>% 
    group_by(.data[[biome_var]]) %>% 
    summarise(cnt = length(.data[[biome_var]])) %>% 
    arrange(cnt)
  
  # Initialize plot # Boxplot of all data
  box_full <- df_filt %>% 
    filter(!is.na(value)) %>% 
    ggplot(
      aes(x = factor(.data[[biome_var]],
                     levels = biome_cnt[[biome_var]]),
          y = value,  
          color = factor(zone, 
                         levels=c("Outside buffer", "Buffer 10 km", "Inside NPA")))) +
    geom_boxplot(outlier.size=1, 
                 outlier.alpha = 0.5,
                 fill=NA,
                 width = .85, 
                 notch=F,
                 position = position_dodge2(width = 0.85, preserve="single"))+
    scale_colour_manual(values=cbPalette, "Zone") +
    theme_desc() +
    scale_x_discrete(limits = rev(levels(df_filt$zone)), 
                     labels = function(x) str_wrap(x, width = 10)) +
    coord_flip() +
    ylab(label = stat_name) + 
    labs(title = plot_title)
  
  # Get upper axis limit for given statistic
  ylim_max <- df_filt$value %>% quantile(probs=percentile, na.rm=T) %>% deframe
  
  # Zoomed in plot with jitter points
  jitter_zoom <- box_full + 
    geom_point(
      position = position_jitterdodge(0.15, dodge.width = 0.85),
      size = 1, 
      alpha = 0.5, show.legend = FALSE) +
    coord_flip(ylim = c(0, ylim_max)) +
    labs(title = str_c('Zoom < ', percentile, ' percentile'))
  
  # Assemble plot
  (p1 <- (box_full | jitter_zoom) + 
      plot_annotation(
        title = plot_title
      ) + 
      plot_layout(guides='collect') &
      theme(legend.position='bottom')
  )
  
  if(facet_by_zone){
   
    # Facets instead of colors
    box_full_facet <- df_filt %>%
      filter(!is.na(value)) %>%
      ggplot(
        aes(x = factor(.data[[biome_var]],
                       levels = biome_cnt[[biome_var]]),
            y = value)) +
      geom_boxplot(outlier.size=1,
                   outlier.alpha = 0.5,
                   fill=NA,
                   width = .85,
                   notch=F,
                   position = position_dodge2(width = 0.85, preserve="single"))+
      scale_colour_manual(values=cbPalette, "Biome") +
      theme_desc() +
      scale_x_discrete(limits = rev(levels(df_filt[[biome_var]])),
                       labels = function(x) str_wrap(x, width = 30)) +
      coord_flip() +
      labs(title=pol) +
      facet_wrap(
        vars(factor(zone, levels=c("Inside NPA", "Buffer 10 km", "Outside buffer"))),
        nrow=3) +
      theme(plot.title = element_blank())

    # Zoomed in plot with jitter points
    jitter_zoom <- box_full_facet +
      geom_point(
        position = position_jitter(0.15),
        size = 1,
        alpha = 0.5, show.legend = FALSE) +
      coord_flip(ylim = c(0, ylim_max)) +
      labs(title = str_c('Zoom < ', percentile, ' percentile')) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(hjust=0))

    # Assemble plot
    (p1 <- (box_full_facet | jitter_zoom) +
        plot_annotation(
          title = plot_title
        )
    )
  }
}

plot_boxjitter <- function(df, pol, out_fp=NA, stat = 'brillouin',
                           stat_name = 'Brillouin Index',
                           percentile = 0.95){
  
  # Prep for plot
  # Filter to ANP subsets with values in at least one zone
  df_long <- df %>% 
    group_by(ID_ANP, pol_group) %>% 
    filter(sum(abundance, na.rm=T) > 0) %>% 
    ungroup %>% 
    pivot_longer(cols=any_of(stat), names_to = 'statistic') %>% 
    filter(statistic == stat)
  
  # Filter DF and make plot\
  # Plot each pollinator
  df_filt <- df_long %>% 
    filter(pol_group == pol,
           !is.na(value))
  
  # Get counts of ANPs and ANP subsets 
  n_filt_anp_subs <- df_filt %>% select(rowname) %>% distinct %>% nrow
  n_filt_anps <- df_filt %>% select(ID_ANP) %>% distinct %>% nrow
  
  # Make plot
  plot_title <- str_glue('{pol}, N = {n_filt_anp_subs} NPA subsets ({n_filt_anps} NPAs)')
  display_by_biomes_zones(df_filt, plot_title, percentile, stat_name=stat_name)
  
  # Save
  if(!is.na(out_fp)){
    ggsave(out_fp, width = 9.15, height=8.03)
  }
  
}

# Prep ANP polys for processing ================================================
if(!file.exists(anp_terr_fp)){
  
  anps_proj <- st_read(anp_fp) %>% 
    select(ID_ANP) %>% 
    st_transform(crs=6372) %>% 
    st_set_precision(1e5)  %>% 
    st_make_valid %>% 
    st_collection_extract('POLYGON') %>% 
    st_simplify(dTolerance=20, preserveTopology=T) %>% 
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
  
} else {
  
  anps_terr <- st_read(anp_terr_fp)
  
}

# Create database ==============================================================
# Initialize zone codes
inside_str = 'Inside NPA'
buff_str <- str_c('Buffer ', buffer_distance, ' km')
outside_str <- str_c('Outside buffer')

if(!file.exists(anp_stats_fp)){
  anps_terr<- st_read(anp_terr_fp)
  
  # Initialize zone codes
  inside_str = 'Inside NPA'
  buff_str <- str_c('Buffer ', buffer_distance, ' km')
  outside_str <- str_c('Outside buffer')
  
  # Pollinators
  anps_stats <- pol_groups %>% 
    map(get_pollinators_in_anp_zones, date_range, anps_terr, buffer_distance,
        pol_dir = 'data/data_out/pollinator_points/with_duplicates') %>% 
    reduce(rbind) %>% 
    mutate(zone = factor(zone, levels = c(inside_str, buff_str, outside_str)))
  
  anps_stats %>% write_csv(anp_stats_fp)
  
} else {
  
  anps_stats <- read_csv(anp_stats_fp)
  
}

# Tidy ANP metrics DF =================================================
# Get lookup table for rownames to ID_ANP
anps_terr <- st_read(anp_terr_fp)
row2id <- anps_terr %>% st_drop_geometry %>% select(rowname, ID_ANP)

# Read data and perform some tidying
anps_stats <- read_csv(anp_stats_fp) %>% 
  mutate(rowname = as.character(rowname),
         across(everything(), replace_na, 0),
         zone = factor(zone, levels = c(inside_str, buff_str, outside_str)), 
         richness_norm = richness_norm * 100, 
         abundance_norm = abundance_norm * 100) %>% 
  full_join(row2id)

anps_stats %>% write_csv(anp_stats_fp)

# Visualize ====================================================================
# All pollinators, one statistic
fig_dir <- 'figures/anps_and_pollinator_exploration/with_duplicates_gt1999'

for(pol in pol_groups){
  plot_stats_boxjitter_by_polgroup(anps_stats, pol)
  
  # Save
  ggsave(file.path(fig_dir, str_c(pol, '_boxjitter_buffer', buffer_distance, 
                                  'km_', date_min, 'to', date_max, '.png')), 
         width = 9.15, height=6.03)
}

stats = c('richness_norm', 
           'abundance_norm', 
           'brillouin', 
           'shannon')
stat <- 'true_shannon'
for(stat in stats) print(stat){

  # Make full plot 
  plot_stats_boxjitter_by_stat(df=anps_stats, stat=stat)
  
  # Save
  ggsave(file.path(fig_dir, str_c(stat, '_boxjitter_buffer', buffer_distance, 
                                  'km_', date_min, 'to', date_max, '.png')), 
         width = 9.15, height=8.03)
}

# Add biome to database ========================================================
# Biomes CONABIO
data_dir <- 'data/input_data/environment_variables/CONABIO'
shp_fp <- list.files(data_dir, '.shp$', full.names = T, recursive = T)
biom <- st_read(shp_fp) %>% 
  st_transform(st_crs(mex)) %>% 
  st_simplify(dTolerance=40)

# Dissolve to ecorregiones (7 in Mexico)
biom_diss <- biom %>% 
  group_by(DESECON1) %>% 
  summarise() %>% 
  st_cast("POLYGON") %>% 
  st_simplify(dTolerance=40)

get_pols_in_anp_biome_zones <- function(pol_group, date_range, anps_terr, 
                                        biom_diss,
                                        buffer_distance, 
                                        polys_id_fld='rowname', 
                                        polys_area_fld = 'area_ha',
                                        pol_dir = 'data/data_out/pollinator_points'){
  # Zone codes
  inside_str <- str_c('Inside NPA')
  buff_str <- str_c('Buffer ', buffer_distance, ' km')
  outside_str <- str_c('Outside buffer')
  
  # Convert date range
  date_min <- as.POSIXct(str_c(date_range[[1]], "-01-01"))
  date_max <- as.POSIXct(str_c(date_range[[2]], "-12-31"))
  
  # Load pollinator file
  pol_dir <- 'data/data_out/pollinator_points/with_duplicates'
  pol_fp <- file.path(pol_dir, str_c(pol_group, '.geojson'))
  df <- st_read(pol_fp) %>% 
    # Filter to date range
    filter(eventDate >= date_min & eventDate <= date_max) 
  
  anps_biom_fp <- "data/data_out/ANPs/ANPs_with_biomes.geojson"
  
  # Intersect ANPs with biomes
  if(!file.exists(anps_biom_fp)){
    
    anps_biom <- st_intersection(anps_terr, biom_diss) %>%
      select(-rowname) %>% 
      rownames_to_column() %>% 
      mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% set_units(NULL))
    
    anps_biom %>% st_write(anps_biom_fp, delete_dsn=T)
    
  } else {
    
    anps_biom <- st_read(anps_biom_fp)
    
  }
  
  buff_biom_fp <- str_c('data/intermediate_data/anps_buffer_', buffer_distance,'km_biomes.geojson')
  
  if(file.exists(buff_biom_fp)){
    
    anps_buff_biom <- st_read(buff_biom_fp)
    
  } else {
    
    # NPA buffer 
    anps_buff <- get_buffer_conditional(buffer_distance, anps_terr, area_fld='area_ha')
    
    # Buffer 
    anps_buff <- anps_buff %>% 
      group_by(ID_ANP) %>% 
      summarise() %>% 
      ungroup() %>% 
      st_cast('MULTIPOLYGON') %>% 
      st_cast("POLYGON")
    
    # Intersect ANP buffers with Biomes
    anps_buff_biom <- st_intersection(anps_buff, biom_diss) %>%
      # select(-rowname) %>% 
      rownames_to_column() %>% 
      mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% set_units(NULL))
    
    # Save
    anps_buff_biom %>% st_write(buffer_fp, delete_dsn=T)
    
  }
  
  vpols_biom_fp <- str_c('data/data_out/ANPs/voronoi_biom_anps_and_buffer_', 
                         buffer_distance,'km.geojson')
  
  if(file.exists(vpols_biom_fp)){
    
    vpols_biom <- st_read(vpols_biom_fp)
    
  } else {
    
    # Voronoi polygons
    vpols <- get_voronois_conditional(anps_proj=anps_terr, buffer_distance)
    
    # Intersect Voronoi polygons with Biomes
    vpols_biom <- st_intersection(vpols, biom_diss) %>%
      dplyr::rename(vpol_id = rowname) %>% 
      rownames_to_column() %>% 
      dplyr::mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% set_units(NULL)) %>% 
      dplyr::rename(vp_biom_id = 'rowname') %>% 
      sf::st_join(select(anps_buff_biom, rowname, ID_ANP), join=st_nearest_feature) %>% 
      dplyr::rename(buff_id = rowname) %>% 
      rownames_to_column() %>% 
      mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% set_units(NULL))
    
    # Save
    vpols_biom %>% st_write(vpols_biom_fp, delete_dsn=T)
    
  }
  
  # Get diversity and abundance of pollinators within NPA 
  anps_in <- get_diversity_for_zone_polys(df, anps_biom, inside_str, 
                                          polys_id_fld, polys_area_fld) %>% 
    left_join(select(anps_biom %>% st_drop_geometry(), DESECON1, ID_ANP, rowname))
  
  # Get diversity and abundance of pollinators in NPA buffer
  anps_buffer <- get_diversity_for_zone_polys(df, anps_buff_biom, buff_str, 
                                              polys_id_fld, polys_area_fld) %>% 
    left_join(select(st_drop_geometry(anps_buff_biom), DESECON1, ID_ANP, rowname))
  
  # Get diversity and abundance of pollinators outside NPA buffer
  anps_outside <- get_diversity_for_zone_polys(df, vpols_biom, outside_str, 
                                               polys_id_fld, polys_area_fld)
  anps_out <- anps_outside %>% 
    left_join(select(st_drop_geometry(vpols_biom), DESECON1, ID_ANP, rowname))
  
  # Concatenate the three DFs
  anps_df <- bind_rows(anps_in, anps_buffer, anps_out)
  
  # Set pollinator group name
  anps_df$pol_group <- pol_group
  
  return(anps_df)
}

# Run
# anps_stats <- get_pols_in_anp_biome_zones(pol_group, date_range, anps_terr, biom_diss, 
#                                           buffer_distance, pol_dir = 'data/data_out/pollinator_points/with_duplicates')

anps_terr <- st_read(anp_terr_fp)

# anp_stats_fp <- "data/data_out/ANPs/anps_allpols_2000_to_2020_buffer10km_wdupes_biomes.csv"

if(!file.exists(anp_stats_fp)){
  
  # Initialize zone codes
  inside_str = 'Inside NPA'
  buff_str <- str_c('Buffer ', buffer_distance, ' km')
  outside_str <- str_c('Outside buffer')
  
  # Pollinators
  anps_stats <- pol_groups %>% 
    map(get_pols_in_anp_biome_zones, date_range, anps_terr, biom_diss, buffer_distance,
        pol_dir = 'data/data_out/pollinator_points/no_duplicates') %>% 
    reduce(rbind)

  anps_stats %>% write_csv(anp_stats_fp)
  
} else {
  
  anps_stats <- read_csv(anp_stats_fp)
  
}

# Plot
pol_group <- 'Colibries'
fig_dir <- 'figures/anps_and_pollinator_exploration/con_biomas'
stat <- 'richness'
stat_name <- 'Richness'
stat <- 'richness_norm'
stat_name <- 'Normalized Richness (number of species per sq. km)'
plot_fn <- str_c(pol_group, '_', stat, '_boxjitter_biomes_buff', 
                 buffer_distance, 'km_', date_range[[1]], 
                 'to', date_range[[2]], '.png')
plot_boxjitter(df=anps_stats, pol=pol_group, out_fp = file.path(fig_dir, plot_fn),
               stat = stat, stat_name = stat_name, percentile = 0.95)

# Combine all zones into one SFC ----
bind_fp <- str_c('data/data_out/ANPs/ANPs_allzones_biomes_buffer_', 
                 buffer_distance,'km.geojson')

if(file.exists(bind_fp)){
  
  # Load
  anps_biom_bind <- st_read(bind_fp)
  
} else {
  
  # Set zone names before binding
  anps_biom$zone <- inside_str
  anps_buff_biom$zone <- buff_str
  vpols_biom$zone <- outside_str
  
  # Create lookup list for recode
  lu <- setNames(c('anp', 'buff', 'out'), 
                 c(inside_str, buff_str, outside_str)) 
  
  # Bind SFCs
  anps_biom_bind <- bind_rows(anps_biom, anps_buff_biom, vpols_biom) %>% 
    mutate(zone_temp = recode(zone, !!!lu),
           name = str_c(zone_temp, rowname,
                        abbreviate(DESECON1, use.classes=T), sep='_')) %>% 
    select(name, ID_ANP, area_ha, DESECON1, zone)
  
  # Save
  anps_biom_bind %>% st_write(bind_fp, delete_dsn=T)
  
}

# Compare sites ----
polys_id_fld <- 'name'
pol_group <- 'Abejas'
polys_area_fld <- 'area_ha'

# Load pollinator file
pol_dir <- 'data/data_out/pollinator_points/with_duplicates'
pol_fp <- file.path(pol_dir, str_c(pol_group, '.geojson'))
pol_df <- st_read(pol_fp) %>% 
  # Filter to date range
  filter(eventDate >= date_min & eventDate <= date_max) 

# Get diversity and abundance of pollinators
pol_df <- pol_df %>% 
  # Group pollinator points by intersecting ANP 
  st_transform(crs = st_crs(anps_biom_bind)) %>% 
  st_join(anps_biom_bind, left=F)

# anps_biom_bind_div <- pol_df %>% 
#   get_diversity_for_zone_polys(anps_biom_bind, polys_id_fld='name') %>% 
#   left_join(st_drop_geometry(anps_biom_bind))

# Group by ANP and pivot wide (separate column with count for each species)
spec_df <- pol_df %>% 
  st_drop_geometry %>% 
  group_by(.data[[polys_id_fld]], .data[[polys_area_fld]], species) %>% 
  summarize(n = length(species)) %>% 
  ungroup %>% 
  pivot_wider(id_cols = all_of(c(polys_id_fld, polys_area_fld)), 
              names_from = species, 
              values_from = n) %>%
  replace(is.na(.), 0) %>% 
  select(-area_ha) %>% 
  left_join(st_drop_geometry(anps_biom_bind)) %>% 
  # mutate(name = str_c(abbreviate(DESECON1, use.classes=T), '_', rowname)) %>% 
  column_to_rownames('name') %>%
  select(-any_of(c('rowname', 'ID_ANP', 'area_ha', 'DESECON1', 'zone')))

# Jaccard distance ----
# Get distance matrix (Jaccard)
df.jaccard <- vegdist(spec_df, method="jaccard")

# Tree plot
plot(hclust(df.jaccard),
     hang = -1, 
     main = "Sites clustered by Jaccard similarity",
     axes = FALSE, 
     ylab = "")

# Euclidean distance
df.euclidean <- dist(spec_df)

# Display using non-metric multidimensional scaling
mdsE <- vegan::metaMDS(spec_df, distance='euc')
plot(mdsE, display="sites", type="text")

# Bray-Curtis
mdsB <- metaMDS(spec_df, distance="bray", autotransform=FALSE, trace=0)
plot(mdsB, display="sites", type="text")

# Add land cover (natural, cropland, other) to database ========================
tipages_fp <- 'data/intermediate_data/polys_ag_INEGI_diss_tipages.geojson'
tipinfo_fp <- 'data/intermediate_data/polys_ag_INEGI_diss_tip_info.geojson'

# Get polygons from G
if(file.exists(tipages_fp) & file.exists(tipinfo_fp)){
  
  usv_diss_tipages <- st_read(tipages_fp)
  usv_diss_tip_info <- st_read(tipinfo_fp)
  
} else {
  
  data_dir <- 'data/input_data/INEGI_2017'
  fp_usv <- list.files(path=data_dir, pattern='usv250s6g.shp$', 
                       full.names=T, recursive=T)
  
  usv <- st_read(fp_usv, crs=6362) %>% 
    st_make_valid %>% 
    st_transform(crs=6372) %>% 
    st_simplify(dTolerance=20)
  
  usv_diss <- usv %>% 
    group_by() %>% 
    summarize()
  
  usv_diss_tipages <- usv %>% 
    group_by(TIPAGES) %>% 
    summarize()  %>% 
    st_simplify(preserveTopology = T, dTolerance = .0001)
  
  usv_diss_tip_info <- usv %>% 
    group_by(TIP_INFO) %>% 
    summarize() %>% 
    st_simplify(preserveTopology = T, dTolerance = .0001)
  
  usv_diss_tipages %>% st_write(tipages_fp)
  usv_diss_tip_info %>% st_write(tipinfo_fp)
  
}

usv_diss %>% object.size() %>% print(unit='MB')
usv_diss %>% st_write('data/intermediate_data/polys_ag_INEGI_diss.geojson', 
                      delete_dsn=T)


# Dissolve to ecorregiones (7 in Mexico)
biom_diss <- biom %>% 
  group_by(DESECON1) %>% 
  summarise() %>% 
  st_cast("POLYGON") %>% 
  st_simplify(dTolerance=40)

# Intersect ANPs with Biomes
anps_biom <- st_intersection(anps_terr, biom_diss) %>%
  select(-rowname) %>% 
  rownames_to_column() %>% 
  mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% set_units(NULL))

# 
pol_group <- 'Mariposas'
polys_id_fld='rowname'
polys_area_fld = 'area_ha'

# Zone codes
inside_str <- str_c('Inside NPA')
buff_str <- str_c('Buffer ', buffer_distance, ' km')
outside_str <- str_c('Outside buffer')

# Convert date range
date_min <- as.POSIXct(str_c(date_range[[1]], "-01-01"))
date_max <- as.POSIXct(str_c(date_range[[2]], "-12-31"))

# Load pollinator file
pol_fp <- file.path('data/data_out/pollinator_points', str_c(pol_group, '.geojson'))
df <- st_read(pol_fp) %>% 
  # Filter to date range
  filter(eventDate >= date_min & eventDate <= date_max) 

# Get diversity and abundance of pollinators within NPA 
anps_in <- get_diversity_for_zone_polys(df, anps_biom, inside_str, 
                                        polys_id_fld, polys_area_fld) %>% 
  left_join(select(anps_biom %>% st_drop_geometry(), DESECON1, ID_ANP, rowname))

buff_biom_fp <- str_c('data/intermediate_data/anps_buffer_', buffer_distance,'km_biomes.geojson')

if(file.exists(buff_biom_fp)){
  
  anps_buff_biom <- st_read(buff_biom_fp)
  
} else {
  
  # NPA buffer 
  anps_buff <- get_buffer_conditional(buffer_distance, anps_terr, area_fld='area_ha')
  
  # Buffer 
  anps_buff <- anps_buff %>% 
    group_by(ID_ANP) %>% 
    summarise() %>% 
    ungroup() %>% 
    st_cast('MULTIPOLYGON') %>% 
    st_cast("POLYGON")
  
  # Intersect ANP buffers with Biomes
  anps_buff_biom <- st_intersection(anps_buff, biom_diss) %>%
    # select(-rowname) %>% 
    rownames_to_column() %>% 
    mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% set_units(NULL))
  
  # Save
  anps_buff_biom %>% st_write(buffer_fp, delete_dsn=T)
  
}

# Get diversity and abundance of pollinators in NPA buffer
anps_buffer <- get_diversity_for_zone_polys(df, anps_buff_biom, buff_str, 
                                            polys_id_fld, polys_area_fld) %>% 
  left_join(select(st_drop_geometry(anps_buff_biom), DESECON1, ID_ANP, rowname))

vpols_biom_fp <- str_c('data/data_out/ANPs/voronoi_biom_anps_and_buffer_', 
                       buffer_distance,'km.geojson')

if(file.exists(vpols_biom_fp)){
  
  vpols_biom <- st_read(vpols_biom_fp)
  
} else {
  
  # Voronoi polygons
  vpols <- get_voronois_conditional(anps_proj=anps_terr, buffer_distance)
  
  # Intersect Voronoi polygons with Biomes
  vpols_biom <- st_intersection(vpols, biom_diss) %>%
    dplyr::rename(vpol_id = rowname) %>% 
    rownames_to_column() %>% 
    dplyr::mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% set_units(NULL)) %>% 
    dplyr::rename(vp_biom_id = 'rowname') %>% 
    sf::st_join(select(anps_buff_biom, rowname, ID_ANP), join=st_nearest_feature) %>% 
    dplyr::rename(buff_id = rowname) %>% 
    rownames_to_column() %>% 
    mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% set_units(NULL))
  
  # Save
  vpols_biom %>% st_write(vpols_biom_fp, delete_dsn=T)
  
}

# Get diversity and abundance of pollinators outside NPA buffer
anps_outside <- get_diversity_for_zone_polys(df, vpols_biom, outside_str, 
                                             polys_id_fld, polys_area_fld)
anps_out <- anps_outside %>% 
  left_join(select(st_drop_geometry(vpols_biom), DESECON1, ID_ANP, rowname))

# Concatenate the three DFs
anps_df <- bind_rows(anps_in, anps_buffer, anps_out)

# Set pollinator group name
anps_df$pol_group <- pol_group

# Plot

fig_dir <- 'figures/anps_and_pollinator_exploration'

plot_boxjitter(anps_df, pol=pol_group,
               out_fp = file.path(fig_dir, 
                                  str_c(pol_group, '_faceted_boxjitter_biomes_buff', 
                                        buffer_distance, 'km_', date_range[[1]], 
                                        'to', date_range[[2]], '.png')))

# ///Old versions/// =================================================================

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


