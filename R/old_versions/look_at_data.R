# Make heatmaps (Gi*) of pollinator points by species. 
# At the end I started to compare SIAP to QRO data

# Load libraries ---------------------------------------------------------------
library(sf)
library(rgdal)
library(mapview)
library(raster)
library(patchwork)
library(spdep)
library(units)
library(ggnewscale)
library(tidyverse)

# Load data --------------------------------------------------------------------
fig_dir <- 'figures/pol_exploration_no_iNaturalist/date_gt2009'
anps <- st_read('data/input_data/context_Mexico/SHAPE_ANPS/182ANP_Geo_ITRF08_Julio04_2019.shp')

# Mexico 
mex <- raster::getData(
  'GADM', country='MEX', level=0, path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) %>% 
  st_simplify(dTolerance = 0.02)
mex_munis <- raster::getData(
  'GADM', country='MEX', level=2, path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) 
mex_est <- raster::getData(
  'GADM', country='MEX', level=1, path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) 

# Biomes
data_dir <- 'data/input_data/environment_variables/TEOW_WWF_biome'
shp_fp <- list.files(data_dir, '.shp$', full.names = T, recursive = T)
biomes_crop <- st_read(shp_fp) %>% 
  st_crop(st_bbox(mex) )

# Elevation
alt <- raster::getData(
  'alt', country='MEX', path='data/input_data/context_Mexico') %>% 
  aggregate(2) 
alt_df <- as(alt, "SpatialPixelsDataFrame") %>% 
  as.data.frame()

# Make hillshade
alt <- alt *10
slope = terrain(alt, opt = "slope")
aspect = terrain(alt, opt = "aspect")
hill = hillShade(slope, aspect, angle = 40, direction = 270)
hill_df <- as(hill, "SpatialPixelsDataFrame") %>% 
  as.data.frame()

hill_df %>% 
  ggplot() +
  geom_tile(aes(x=x, y=y, fill=layer)) +
  # geom_tile(data=alt_df, aes(x=x, y=y, fill=MEX_msk_alt), alpha=0.5) +
  scale_fill_distiller(palette='Greys')

# Pollinators
name <- 'Murcielagos'
fp_out <- file.path('data/data_out/pollinator_points', str_c(name, '.geojson'))
fp_out <- file.path('data/data_out/pollinator_points/no_duplicates', str_c(name, '.gpkg'))
df <- st_read(fp_out)

# Gi* statistics, ggplot for mapping --------
# Work from https://www.robert-hickman.eu/post/getis-ord-heatmaps-tutorial/

# Functions --------------------------------------------------------------------
map_Gi <- function(df, hex_polys, hill_df, k=6){
  # Get number of points within each hexagon
  hex_polys$pt_no <- st_intersects(hex_polys, df) %>% lengths
  
  # Plot observations per bin (without accounting for neighbors)
  # (p2 <- ggplot(hex_polys) +
  #   geom_sf(aes(fill = pt_no)) +
  #   scale_fill_viridis_c(option = "magma", "# Polinizadores") +
  #   theme_void() +
  #   ggtitle("Binned Pollinators"))
  
  # Convert hexagon centroids to a matrix of points
  hex_pts <- do.call(rbind, st_geometry(st_centroid(hex_polys))) %>%
    unlist %>% as.matrix.data.frame
  
  # Use KNN algorithm to find neighboring shapes. Increase K for more smoothing.
  neighbor_hexes <- hex_pts %>% 
    knearneigh(k = k) %>% 
    knn2nb(row.names = rownames(hex_pts)) %>%
    include.self
  
  # Calculate the local G for point count in each hex using the neighbors
  localGvals <- localG(x = as.numeric(hex_polys$pt_no),
                       listw = nb2listw(neighbor_hexes, style = "B"),
                       zero.policy = TRUE)
  
  # Bind this back to the sf as a numeric variable column
  hex_polys$smooth_pt_no <- as.numeric(localGvals)
  
  # Plot the statistic, Gi* (z-value so greater than abs(1.68) is statistically significant)
  limit <- max(abs(hex_polys$smooth_pt_no)) * c(-1, 1)
  
  # p3 <- ggplot(hill_df) +
    # geom_tile(aes(x=x, y=y, fill=layer), show.legend = F) +
    # scale_fill_distiller(palette='Greys') +
    # 
    # new_scale_color() + 
    # new_scale_fill() + 
  # geom_sf(data = hex_polys,
  p3 <- ggplot(hex_polys) +
    geom_sf(
      aes(fill = smooth_pt_no), 
      # alpha=0.6,
      lwd=NA
      ) +
    
    # scale_fill_viridis_c(option = "magma", name = "Gi* Statistic") +
    scale_fill_distiller(
      type='div', 
      palette='RdBu',
      limit= limit,
      direction=-1, 
      name = "Gi* Statistic"
    ) +
    
    theme_void()
}

map_Gi_facets <- function(df, rank, facets, hex_polys, hill_df, k=6){
  # Convert hexagon centroids to a matrix of points
  hex_pts <- do.call(rbind, st_geometry(st_centroid(hex_polys))) %>%
    unlist %>% as.matrix.data.frame
  
  # Use KNN algorithm to find neighboring shapes. Increase K for more smoothing.
  neighbor_hexes <- hex_pts %>% 
    knearneigh(k = k) %>% 
    knn2nb(row.names = rownames(hex_pts)) %>%
    include.self
  
  # Get list of most numerous groups at given taxonomic rank
  grps_list <- df %>% 
    st_set_geometry(NULL) %>% 
    group_by(.data[[rank]]) %>% 
    summarise(cnt = length(.data[[rank]])) %>% 
    arrange(desc(cnt)) %>% 
    slice(1:facets) %>% 
    select(.data[[rank]]) %>% 
    deframe
  
  for(i in seq(length(grps_list))){
    taxon <- grps_list[[i]]
    
    df1 <- df %>% 
      filter(.data[[rank]] == taxon)
    
    tot_taxon <- df1 %>% nrow %>% format(big.mark=',', trim=T)
    
    # Get number of points within each hexagon
    hex_polys$pt_no <- st_intersects(hex_polys, df1) %>% lengths
    
    # Calculate the local G for point count in each hex using the neighbors
    localGvals <- localG(x = as.numeric(hex_polys$pt_no),
                         listw = nb2listw(neighbor_hexes, style = "B"),
                         zero.policy = TRUE)
    
    # Bind this back to the sf as a numeric variable column
    hex_polys[taxon] <- as.numeric(localGvals)
  }
  
  hex_polys_long <- hex_polys %>% 
    pivot_longer(cols=any_of(grps_list), names_to=rank, values_to='g_stat') %>% 
    mutate(across(all_of(rank), ~ factor(.x, levels=grps_list))) %>%
    st_as_sf()
  
  # Plot the statistic, Gi* (z-value so greater than abs(1.68) is statistically significant)
  limit <- max(abs(hex_polys_long$g_stat)) * c(-1, 1)
  
  # p3 <- ggplot(hill_df) +
  #     geom_tile(aes(x=x, y=y, fill=layer), show.legend = F) +
  #     scale_fill_distiller(palette='Greys') +
  #     
  #     new_scale_color() + 
  #     new_scale_fill() + 
  #   
  #     geom_sf(data = hex_polys_long,
  p3 <- ggplot(hex_polys_long) +
    
    geom_sf(
        aes(
          fill = g_stat
        ),
        # alpha = 0.6,
        lwd=NA
        ) +
    
      # scale_fill_viridis_c(option = "magma", name = "Gi* Statistic") +
      scale_fill_distiller(
        type='div', 
        palette='RdBu',
        limit= limit,
        direction=-1, 
        name = "Gi* Statistic"
      ) +
    
      facet_wrap(~ .data[[rank]]) +
    
      theme_void()
}

plot_taxon_bars <- function(df, rank, lim=10){
  theme_desc <- function () { 
    theme_minimal() +
      theme(
        plot.subtitle = element_text(size = 10),
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 45)
      )
  }
  
  # Strip geometry
  dat <- df %>% st_set_geometry(NULL)
  
  # Get number of unique taxons
  tot_sfam <- dat %>% select(all_of(rank)) %>% distinct %>% nrow
  
  # Get number of observations for each taxon
  (species_cnt <- dat %>% 
      group_by(.data[[rank]]) %>% 
      summarise(cnt = length(.data[[rank]])) %>% 
      arrange(desc(cnt)))
  
  # Plot top X taxons
  subtitle <- str_c(
    'Valores únicos de "', rank, '": ', format(tot_sfam, big.mark=',', trim=T)
  )
  species_cnt %>% 
    slice(1:lim) %>% 
    ggplot(aes(x=reorder(.data[[rank]], cnt), y=cnt)) +
    geom_bar(stat='identity', show.legend = FALSE) +
    coord_flip() +
    theme_desc() +
    scale_y_continuous(labels = comma)  +
    labs(
      x = NULL,
      y = NULL,
      subtitle = subtitle
    )
}

# Data ---------
anp_pts <- anps %>%
  mutate(area_ha = 
           st_area(geometry) %>% 
           set_units('ha') %>% 
           set_units(NULL)) %>% 
  st_centroid

# Generate hexagons within Mexico
hex_polys <- mex %>% as_Spatial %>% 
  spsample(n=2000, type = "hexagonal") %>% # Distribute points hexagonally
  HexPoints2SpatialPolygons %>%            # Create hexagons
  st_as_sf(crs = st_crs(mex)) %>%
  st_intersection(., mex)                  # clip to the Mexico boundary

# Filter points ----
df <- df %>% 
  filter(eventDate > 2009)

# Make maps ----
# All taxons
plt_all <- df %>% map_Gi(hex_polys, hill_df, k=6) 
plt_all <- plt_all +
  geom_sf(data=mex, fill=NA, lwd=0.3) +
  geom_sf(data=anps, fill='darkgray', alpha=0.1, 
          lwd=0.1) #+
  # geom_sf(data=biomes_crop, fill=NA)

# Facet by taxon groups 
rank <- 'genus'
facets <- 6
plt_genera <- map_Gi_facets(df, rank, facets, hex_polys, hill_df, k=6)
plt_genera <- plt_genera +
  geom_sf(data=mex, fill=NA, lwd=0.3) +
  geom_sf(data=anps, fill='darkgray', alpha=0.1, 
          lwd=0.1)

# Add bar chart -----
(spec <- plot_taxon_bars(df, rank))
layout <- c(
  area(t = 1, l = 1, b = 3, r = 2.5),
  area(t = 1, l = 4, b = 3, r = 4.5),
  area(t = 4, l = 1, b = 8, r = 4.5)
)
plt_all + spec + plt_genera +
  plot_layout(design = layout) + 
  plot_annotation(
    title = glue::glue('{name}, observaciones de 2010-2020')
  )

fp_out <- file.path(fig_dir, str_glue('Gi_stat_{name}_{rank}_{facets}_alt.png'))
ggsave(fp_out, width = 11, height=8)

# Species facets ----
rank <- 'species'
facets <- 12
plt_genera <- map_Gi_facets(df, rank, facets, hex_polys, k=6)
plt_genera <- plt_genera +
  geom_sf(data=mex, fill=NA, lwd=0.3) +
  geom_sf(data=anps, fill='darkgray', alpha=0.1, 
          lwd=0.1)

# Save plot
plt_genera +
  plot_annotation(
    title = glue::glue('{name}, observaciones de 2010-2020')
  )

fp_out <- file.path(fig_dir, str_glue('Gi_stat_{name}_{rank}_{facets}.png'))
ggsave(fp_out, width = 14, height=8)

# Layout -----
layout <- c(
  area(t = 1, l = 1, b = 3, r = 2.5),
  area(t = 4, l = 1, b = 8, r = 4.5)
)
plt_all + 
  plt_genera +
  plot_layout(design = layout) + 
  plot_annotation(
    title = glue::glue('{name}')
  ) 

fp_out <- file.path(fig_dir, str_glue('Gi_stat_{name}_{rank}_{facets}.png'))
ggsave(fp_out, width = 11, height=8)


# Crop region ------------------------------------------------------------------ 
bb <- st_bbox(c(xmin=-101, ymin=18.8, xmax=-98.4, ymax=20.1), 
              crs=4326)
mex <- raster::getData('GADM', country='MEX', level=0, 
                       path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) %>% 
  st_simplify(dTolerance = 0.02) %>% 
  st_crop(bb)

# Municipios
mex_munis <- raster::getData('GADM', country='MEX', level=2, 
                             path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326)  %>% 
  st_crop(bb)

# ANPs
anps <- anps %>% 
  st_transform(crs=st_crs(mex)) %>% 
  st_crop(bb)

# Generate hexagons within Mexico
hex_polys <- mex %>% as_Spatial %>% 
  spsample(n=1500, type = "hexagonal") %>% # Distribute points hexagonally
  HexPoints2SpatialPolygons %>%            # Create hexagons
  st_as_sf(crs = st_crs(mex)) %>%
  st_intersection(., mex)                  # clip to the Mexico boundary

# Filter points
df <- df %>% 
  filter(eventDate > 2009)

# Make maps
plt_all <- df %>% map_Gi(hex_polys, k=6)
plt_all + 
  geom_sf(data=mex_munis, fill=NA, lwd=0.2) +
  geom_sf(data=anps, fill=NA, color='green', lwd=0.3)


rank <- 'family'
facets <- 4
(plt_genera <- map_Gi_facets(df, rank, facets, hex_polys, k=6))
plt_genera + 
  geom_sf(data=mex_munis, fill=NA, lwd=0.2) +
  geom_sf(data=anps, fill=NA, color='green', lwd=0.3)

# Layout
layout <- c(
  area(t = 1, l = 1, b = 3, r = 2.5),
  area(t = 4, l = 1, b = 8, r = 4.5)
)
plt_all + plt_genera +
  plot_layout(design = layout) + 
  plot_annotation(
    title = glue::glue('{name}')
  )

fp_out <- file.path(fig_dir, str_glue('Gi_stat_{name}_{rank}_{facets}.png'))
ggsave(fp_out, width = 11, height=8)




# ~ MISCELLANEOUS ----
# Load libraries 
library(rvest)
library(tools)
library(units)

# Load data
# Initial variables/helpers: ag_dir, regions, fmg_dir, est_codes, states_by_region
load('data/helpers/initial_vars.RData') 
# Crop statistics: area_cult_peren, area_cult_prim, area_cult_otono, cultivos
load("data/data_out/r_data/area_sembrada_by_season_2019.RData") 
lookup <- readRDS('data/helpers/lookup_municipio_codes.rds')

# Compare SIAP to QRO SEDEA data ----
# List columns
area_cult_prim %>% colnames
vars <- area_cult_prim %>% 
  select(-CVE_ENT:-total_sembrada) %>% 
  colnames()
# Extract
prim <- area_cult_prim %>%
  left_join(lookup, by=c('CVE_ENT', 'CVE_MUN')) %>% 
  filter(NOM_ENT == 'Querétaro', Idmodalidad == 'R') %>% 
  pivot_longer(all_of(vars), names_to='Cultivo', values_to='Sup_sembrada', values_drop_na=T)
prim %>% filter(CVE_MUN == 11) %>% arrange(Cultivo)
