# Data are from GBIF
# parameters: https://www.gbif.org/developer/occurrence#parameters

# Load libraries ---------------------------------------------------------------
library(tabulizer)
library(sf)
library(rgdal)
library(mapview)
library(tools)
# library(rgdal)
library(raster)
library(tmap)
library(leaflet)
library(patchwork)
library(scales)
library(taxize)
library(tidyverse)

# Initialize -------------------------------------------------------------------
fig_dir <- 'figures/pol_exploration_no_iNaturalist/date_gt2009'
name <- 'Mariposas'
fp_out <- file.path('data/data_out/pollinator_points', str_c(name, '.geojson'))
df <- st_read(fp_out)

# Pollinator database ----
data_dir <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/Completos'
fps <- list.files(data_dir, full.names = T)
fp <- fps[[4]]

(name <- fp %>% 
  basename %>% file_path_sans_ext %>% 
  str_split('_') %>% last %>% last)

# Load data
dat <- read_csv(fp)

fam_list <- dat %>% 
  dplyr::select(family) %>% 
  distinct %>% 
  mutate(superfamily = tax_name(family, get='superfamily', db='ncbi')[[3]])

dat <- dat %>% 
  left_join(fam_list) 

if(name == 'Mariposas'){
  dat <- dat %>% 
    mutate(nocturna = case_when(
      superfamily %in% c('Papilionoidea', 'Hesperioidea', 'Hedyloidea') ~ 'diurna',
      !is.na(superfamily) ~ 'nocturna'
    ))
}

dat %>% write_csv(file.path(data_dir, 'extras', str_c(name, '.csv')))

# Data tidying steps: drop coords with NAs, drop duplicates, flag/drop imprecise
vars <- c('species', 'genus', 'family', 'superfamily', 'order', 'class', 'nocturna', 
          'decimalLongitude', 'decimalLatitude', 
          'eventDate', 'coordinateUncertaintyInMeters', 'habitat', 
          'basisOfRecord', 'country', 'stateProvince', 'institutionCode')

dat <- read_csv(file.path(data_dir, 'extras', str_c(name, '.csv')))

df <- dat %>% 
  drop_na(decimalLongitude, decimalLatitude) %>% 
  filter(decimalLongitude != 0 & decimalLatitude != 0,
         coordinateUncertaintyInMeters < 1000 | is.na(coordinateUncertaintyInMeters),
         !str_detect(issues, 'COUNTRY_COORDINATE_MISMATCH'),
         institutionCode != 'iNaturalist') %>% 
  dplyr::select(matches(vars)) %>% 
  distinct %>% 
  st_as_sf(x = .,                         
           coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

# Save ----
fp_out <- file.path('data/data_out/pollinator_points', str_c(name, '.geojson'))
df %>% st_write(fp_out, delete_dsn=T)

# Look around ------------------------------------------------------------------
tot <- dat %>% nrow
dat <- dat %>% 
  drop_na(decimalLongitude, decimalLatitude) %>% 
  filter(decimalLongitude != 0 & decimalLatitude != 0,
         !str_detect(issues, 'COUNTRY_COORDINATE_MISMATCH'),
         institutionCode != 'iNaturalist')
tot_goodcoords <- dat %>% nrow 
tot_badcoords <- tot - tot_goodcoords

# Tables -----
(species_cnt <- dat %>% 
   group_by(genus) %>% 
   summarise(cnt = length(genus)) %>% 
   arrange(desc(cnt)))

(basisOfRecord_cnt <- dat %>% 
    group_by(basisOfRecord) %>% 
    summarise(cnt = length(basisOfRecord)) %>% 
    arrange(desc(cnt)))

# Unique habitat values and quantity
(habitat_cnt <- dat %>% 
    group_by(habitat) %>% 
    summarise(cnt = length(habitat)) %>% 
    arrange(desc(cnt)))
tot_hab <- dat %>% select(habitat) %>% distinct %>% nrow
# habitat_cnt %>% write_csv(file.path(fig_dir, str_c('pol_', name, '_habitats.csv')))

# Number of records from each institution
institutionCode_owners <- dat %>% 
  group_by(institutionCode, ownerInstitutionCode) %>% 
  summarise() %>% 
  drop_na(ownerInstitutionCode)
(institution_cnt <- dat %>% 
    mutate(institutionCode = case_when(
      institutionCode =='C.A. Triplehorn Insect Collection, Ohio State University, Columbus, OH (OSUC)' ~ 'OSUC',
      institutionCode =='Cleveland Museum of Natural History, OH (CLEV)' ~ 'CLEV',
      institutionCode !='C.A. Triplehorn Insect Collection, Ohio State University, Columbus, OH (OSUC)' ~ institutionCode )
    ) %>% 
    group_by(institutionCode) %>% 
    summarise(cnt = length(institutionCode)) %>% 
    left_join(institutionCode_owners) %>% 
    arrange(desc(cnt)))
# institution_cnt %>% write_csv(file.path(fig_dir, str_c('pol_', name, '_institutions.csv')))

# Data exploration plots -------------------------------------------------------
theme_desc <- function () { 
  theme_minimal() +
    theme(
      plot.subtitle = element_text(size = 10),
      plot.title.position = "plot",
      axis.text.x = element_text(angle = 45)
    )
}

# Species
tot_species <- dat %>% select(species) %>% distinct %>% nrow
tot_genus <- dat %>% select(genus) %>% distinct %>% nrow
if (tot_species < 17){
  title <- 'Especies'
  # Species bar chart
  species_cnt <- dat %>% 
     group_by(species) %>% 
     summarise(cnt = length(species)) %>% 
     arrange(desc(cnt))
  spec1 <- species_cnt %>% 
    ggplot(aes(x=reorder(species, cnt), y=cnt))
} else {
  title <- 'Géneros'
  # Genus bar chart
  species_cnt <- dat %>% 
     group_by(genus) %>% 
     summarise(cnt = length(genus)) %>% 
     arrange(desc(cnt))
  spec1 <- species_cnt %>% 
      slice(1:40) %>% 
      ggplot(aes(x=reorder(genus, cnt), y=cnt))
}
if(name=='Mariposas'){
  title <- 'Familias'
  tot_fam <- dat %>% select(family) %>% distinct %>% nrow
  # Family bar chart
  (species_cnt <- dat %>% 
    group_by(family) %>% 
    summarise(cnt = length(family)) %>% 
    arrange(desc(cnt)))
  spec1 <- species_cnt %>% 
    slice(1:40) %>% 
    ggplot(aes(x=reorder(family, cnt), y=cnt))
  
  # superfamily bar chart (Mariposas)
  title <- 'Super-Familias'
  tot_sfam <- dat %>% select(superfamily) %>% distinct %>% nrow
  (species_cnt <- dat %>% 
      group_by(superfamily) %>% 
      summarise(cnt = length(superfamily)) %>% 
      arrange(desc(cnt)))
  spec1 <- species_cnt %>% 
    slice(1:40) %>% 
    ggplot(aes(x=reorder(superfamily, cnt), y=cnt))
}

subtitle <- str_c(
  'Valores únicos de "family": ', tot_fam %>% format(big.mark=',', trim=T), 
  '\nValores únicos de "genus": ', tot_genus %>% format(big.mark=',', trim=T), 
  '\nValores únicos de "species": ', tot_species %>% format(big.mark=',', trim=T)
  )
(spec <- spec1 +
    geom_bar(stat='identity', show.legend = FALSE) +
    coord_flip() +
    theme_desc() +
    scale_y_continuous(labels = comma)  +
    labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = subtitle
    ))

# basisOfRecord
rec1 <- ggplot(dat, aes(x=basisOfRecord)) +
  geom_bar() +
  coord_flip() +
  theme_desc() +
  scale_y_continuous(labels = comma)  +
  labs(
    x = NULL,
    y = NULL,
    title = 'basisOfRecord'
  )

ord_unc_class <- c("NA", "<1 km", "1-2 km", "2-3 km", ">3 km")
uncertainty_cnts <- dat %>%
  mutate(coordinateUncertainty = case_when(
    is.na(.$coordinateUncertaintyInMeters) ~ "NA",
    .$coordinateUncertaintyInMeters >=  0 & .$coordinateUncertaintyInMeters <= 1000   ~ "<1 km",
    .$coordinateUncertaintyInMeters >  1000 & .$coordinateUncertaintyInMeters <= 2000    ~ "1-2 km",
    .$coordinateUncertaintyInMeters >  2000 & .$coordinateUncertaintyInMeters <= 3000   ~ "2-3 km",
    .$coordinateUncertaintyInMeters >  3000 & .$coordinateUncertaintyInMeters <= 10000000  ~ ">3 km"
  ),
  coordinateUncertainty = fct_relevel(coordinateUncertainty, ord_unc_class))
(coords1 <- ggplot(uncertainty_cnts, aes(x = coordinateUncertainty)) +
    geom_bar(stat='count', show.legend = FALSE) +
    coord_flip() +
    theme_desc() +
    scale_y_continuous(labels = comma)  +
    labs(
      x = NULL,
      y = NULL,
      title = 'coordinateUncertainty'
    ))

# Dates
(edates <- ggplot(dat, aes(eventDate, ..count..)) + 
    geom_histogram() +
    coord_flip() +
    theme_desc() +
    scale_y_continuous(labels = comma)  +
    labs(
      x = NULL,
      y = NULL,
      title = 'eventDate'
    ) + 
    scale_x_datetime(breaks = date_breaks("25 years"),
                     labels = date_format("%Y"),
                     limits = c(as.POSIXct("1835-01-01"),
                                as.POSIXct("2020-12-01")) ))

# Institutions
tot_inst <- dat %>% select(institutionCode) %>% distinct %>% nrow
(insts <- institution_cnt %>% 
    slice(1:40) %>% 
    ggplot(aes(x=reorder(institutionCode, cnt), y=cnt)) +
    geom_bar(stat='identity', show.legend = FALSE) +
    coord_flip() +
    theme_desc() +
    labs(
      x = NULL,
      y = NULL,
      title = 'institutionCode',
      subtitle = glue::glue('Valores únicos: {tot_inst}')
      ) +
    scale_y_continuous(labels = comma) 
    )

# Simple
tot_goodcoords <- tot_goodcoords %>% format(big.mark=',', trim=T)
tot_badcoords <- tot_badcoords %>% format(big.mark=',', trim=T)
# (spec + coords1) / rec1
# (spec | rec1) / (coords_hist | edates)
# patchwork <- spec | (rec1 / coords1 / edates)
patchwork <- spec | ((rec1 / coords1) / edates) | insts
patchwork + plot_annotation(
  title = glue::glue('{name}: {tot_goodcoords} registros evaluados'),
  subtitle = glue::glue('{tot_badcoords} registros eliminados (coordenadas malas, iNaturalist)')
)
ggsave(file.path(fig_dir, str_c('pol_', name, '_simple5.png')), 
       width = 9.15, height=6.03)

# Filter to dates since 2010
df <- df %>% 
  filter(eventDate > 2009)

# Convert table to simple features data frame ----------------------------------
mex <- raster::getData('GADM', country='MEX', level=1, 
                       path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) 

# Functions to create maps with tmap
get_biggest_groups <- function(df, rank, facets){

  # Get list of most numerous groups at given taxonomic rank
  grps_list <- df %>% 
    st_set_geometry(NULL) %>% 
    group_by(.data[[rank]]) %>% 
    summarise(cnt = length(.data[[rank]])) %>% 
    arrange(desc(cnt)) %>% 
    slice(1:facets) %>% 
    select(.data[[rank]]) %>% 
    deframe
  
  # Filter df to the list
  df %>% filter(.data[[rank]] %in% grps_list)
}

map_pts_taxon_facets <- function(df, rank, name, facets, fig_dir){
  # Subset to top taxa
  df_sub <- df %>% get_biggest_groups(rank, facets)
  
  if('nocturna' %in% colnames(df_sub)){
    # Map
    tm <- tm_shape(mex) +
      tm_borders(col='darkgray') + 
      tm_shape(df_sub) +
      tm_dots(alpha=0.4, size=.1,
              col = 'nocturna', palette=c('#b10026', '#0c2c84'),
              legend.show=F) +
      tm_facets(by = rank, free.coords=F)
  } else {
    # Map
    tm <- tm_shape(mex) +
      tm_borders(col='darkgray') + 
      tm_shape(df_sub) +
      tm_dots(alpha=0.4, size=.1, col = '#b10026') +
      tm_facets(by = rank, free.coords=F)
  }

  # Save
  fp_out <- file.path(fig_dir, 
                      str_c('pol_', name, '_map_', rank, '_', facets, '.png'))
  tmap_save(tm, filename = fp_out)
}

# Map points by taxonomy -----
map_pts_taxon_facets(df, rank='family', name=name, facets=12, fig_dir=fig_dir)
map_pts_taxon_facets(df, rank='genus', name=name, facets=16, fig_dir=fig_dir)
map_pts_taxon_facets(df, rank='species', name=name, facets=25, fig_dir=fig_dir)

# Mariposas
try(map_pts_taxon_facets(df, rank='superfamily', name=name, facets=12, fig_dir=fig_dir))
try(map_pts_taxon_facets(df, rank='nocturna', name=name, facets=2, fig_dir=fig_dir))



# ggplot for mapping --------
# Work from https://www.robert-hickman.eu/post/getis-ord-heatmaps-tutorial/
library(spdep)

# Gi* statistics ----
map_Gi <- function(df, hex_polys, k=6){
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
  p3 <- ggplot(hex_polys) +
      geom_sf(aes(fill = smooth_pt_no), lwd=.1) +
      scale_fill_viridis_c(option = "magma", name = "Gi* Statistic") +
      theme_void()
}

map_Gi_facets <- function(df, rank, facets, hex_polys, k=6){
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
    st_as_sf
  
  # Plot the statistic, Gi* (z-value so greater than abs(1.68) is statistically significant)
  (p3 <- ggplot(hex_polys_long) +
      geom_sf(aes(fill = g_stat), lwd=.1) +
      scale_fill_viridis_c(option = "magma", name = "Gi* Statistic") +
      facet_wrap(~ .data[[rank]]) +
      theme_void())
}

# Mexico 
mex <- raster::getData('GADM', country='MEX', level=0, 
                       path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) %>% 
  st_simplify(dTolerance = 0.02)

# Municipios
mex_munis <- raster::getData('GADM', country='MEX', level=2, 
                             path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) 

# Generate hexagons within Mexico
hex_polys <- mex %>% as_Spatial %>% 
  spsample(n=1500, type = "hexagonal") %>% # Distribute points hexagonally
  HexPoints2SpatialPolygons %>%            # Create hexagons
  st_as_sf(crs = st_crs(mex)) %>%
  st_intersection(., mex)                  # clip to the Mexico boundary

# Filter points
df <- df %>% 
  filter(eventDate > 2009)

plt_all <- df %>% map_Gi(hex_polys, k=6)

rank <- 'family'
facets <- 6
plt_genera <- map_Gi_facets(df, rank, facets, hex_polys, k=6)

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

# superfamily bar chart (Mariposas)
title <- 'Super-Familias'
tot_sfam <- dat %>% select(superfamily) %>% distinct %>% nrow
(species_cnt <- dat %>% 
    group_by(superfamily) %>% 
    summarise(cnt = length(superfamily)) %>% 
    arrange(desc(cnt)))
spec1 <- species_cnt %>% 
  slice(1:10) %>% 
  ggplot(aes(x=reorder(superfamily, cnt), y=cnt))

subtitle <- str_c(
  'Valores únicos de "superfamily": ', tot_sfam %>% format(big.mark=',', trim=T)
)
(spec <- spec1 +
    geom_bar(stat='identity', show.legend = FALSE) +
    coord_flip() +
    theme_desc() +
    scale_y_continuous(labels = comma)  +
    labs(
      x = NULL,
      y = NULL,
      subtitle = subtitle
    ))
layout <- c(
  area(t = 1, l = 1, b = 3, r = 2.5),
  area(t = 1, l = 4, b = 3, r = 4.5),
  area(t = 4, l = 1, b = 8, r = 4.5)
)
plt_all + spec + plt_genera +
  plot_layout(design = layout) + 
  plot_annotation(
    title = glue::glue('{name}')
  )

fp_out <- file.path(fig_dir, str_glue('Gi_stat_{name}_{rank}_{facets}.png'))
ggsave(fp_out, width = 11, height=8)






# Quality checking ----
# are all points in WGS84?
dat %>% 
  select(geodeticDatum) %>% distinct
# according to geodeticDatum field, they are.
poss_issues <- dat %>%
  select(issues) %>% 
  mutate(issues = str_replace_all(issues, "\\[|\\]|'", '')) %>% 
  separate_rows(issues, sep=", ") %>% 
  distinct
# RECORDED_DATE_UNLIKELY, PRESUMED_NEGATED_LATITUDE, RECORDED_DATE_INVALID,
# COORDINATE_REPROJECTED, COORDINATE_INVALID, ZERO_COORDINATE, 
# BASIS_OF_RECORD_INVALID, COORDINATE_ROUNDED
dat %>% 
  select(issues, contains('date', ignore.case=T)) %>% 
  filter(str_detect(issues, 'RECORDED_DATE_UNLIKELY'))
# All dates are NA where issues include RECORDED_DATE_UNLIKELY
d1 <- dat %>% 
  filter(str_detect(issues, 'PRESUMED_NEGATED_LATITUDE')) %>% 
  # select(issues, decimalLatitude, decimalLongitude, species, eventDate, basisOfRecord) %>% 
  select(-key) %>% 
  distinct
d1
d1[[1, 1]]
d1 %>% select(eventID) %>% distinct()
dat %>% colnames
# Points with 'PRESUMED_NEGATED_LATITUDE' appear to duplicates: 
# Same: coords(16.5625, -96.0311), date, species, recordedBy
# Different keys.

# What is basisOfRecord?
dat %>% select(basisOfRecord) %>% distinct

# Get tallies -----
dat %>% nrow
# 151,192
dat %>% select(key) %>% distinct %>% nrow
# Unique keys: 151,145
dat %>% 
  filter(is.na(decimalLongitude) | is.na(decimalLatitude)) %>% 
  nrow
# Observations with incomplete coordinates: 21,783
dat %>% 
  filter(decimalLongitude == 0 | decimalLatitude == 0) %>% 
  nrow
# Observations with 0 coordinates: 145
dat %>% 
  select(species, decimalLongitude, decimalLatitude, 
         eventDate, recordedBy, coordinateUncertaintyInMeters, habitat) %>% 
  duplicated() %>% 
  sum(na.rm = TRUE)
# Observations with same species, coordinates, data collector: 100,939
dat %>% 
  filter(coordinateUncertaintyInMeters > 1000) %>% 
  nrow
# coord uncertainty greater than 1000: 5,180
dat %>% 
  drop_na(decimalLongitude, decimalLatitude) %>% 
  filter(decimalLongitude != 0 & decimalLatitude != 0,
         coordinateUncertaintyInMeters < 1000 | is.na(coordinateUncertaintyInMeters)) %>% 
  distinct %>% 
  filter(str_detect(issues, 'COUNTRY_COORDINATE_MISMATCH')) -> df1
# After default filtering: 
#   COUNTRY_COORDINATE_MISMATCH: 4
#   COORDINATE_ROUNDED: 88,991 (out of 123,487)
#   COORDINATE_PRECISION_INVALID: 877 (all of these records have coordinateUncertaintyInMeters == NA)
#   COORDINATE_INVALID: 0
#   COORDINATE_UNCERTAINTY_METERS_INVALID: 2
df1 %>% 
  select(decimalLongitude, decimalLatitude, coordinateUncertaintyInMeters, country)
dat %>% 
  select(decimalLongitude, decimalLatitude, coordinateUncertaintyInMeters, country, stateProvince) %>% 
  drop_na(decimalLongitude, decimalLatitude) %>% 
  st_as_sf(x = .,                         
           coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326) %>% 
  mapview()

# Look ----
bb <- st_bbox(c(xmin=-104, ymin=20.2, xmax=-103.2, ymax=21), crs=4326)
df1 <- df %>% st_crop(bb)
mapview(df1)

fp_munis <- 'data/input_data/context_Mexico/SNIB_divisionpolitica/muni_2018gw/muni_2018gw.shp'
munis <- st_read(fp_munis) %>% 
  mutate(CVE_ENT=as.integer(CVE_ENT),
         CVE_MUN=as.integer(CVE_MUN))
munis_jal <- munis %>% 
  st_within(bb)
st_sf(bb) %>% class
mapview(df1) + 
  mapview(munis)






# Table from PDF attempts ----
library(tabulizer)

# Convert Apendice 1 to a dataframe.
fname <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice 1.pdf'

# Extract and format part 1 as dataframe
test <- extract_tables(fname, pages=seq(2,12), method='stream', output='data.frame')
tbls <- list()
for (i in c(1,3,4,5,6,7,8,10,11)){
  t <- test[i] %>% 
    as.data.frame() %>% 
    slice(-c(1,2)) %>% 
    rename('Parte.utilizada.principal'=X, 
           'Categoría.de.uso'=X.1, 
           'Tipo.de.manejo'=X.2, 
           'Importancia.de.la.polinización'=X.3)
  tbls[[i]] <- t
}
for (i in c(2,9)){
  t <- test[i] %>% 
    as.data.frame() %>% 
    slice(-c(1,2)) %>% 
    rename('Parte.utilizada.principal'=X.1, 
           'Categoría.de.uso'=X.2, 
           'Tipo.de.manejo'=X.3, 
           'Importancia.de.la.polinización'=X.4)
  t$Especie <- t$X
  t <- t[2:length(t)]
  tbls[[i]] <- t
}
df1 <- bind_rows(tbls)
df1$Especie <- df1$Especie %>% 
  str_squish()

# Extract and format part 2 as dataframe
test <- extract_tables(fname, pages=seq(13,23), method='stream', output='data.frame')
tbls <- list()
for (i in c(1,3,5,6,7,8,10,11)){
  t <- test[i] %>% 
    as.data.frame() %>% 
    slice(-c(1,2)) %>% 
    rename('Especie'=X, 
           'Superficie.sembrada'=Superficie, 
           'Superficie.cosechada'=Superficie.1, 
           'Volumen.de.produccion'=Volumen.de,
           'Valor.de.produccion'=Valor.de,
           'Rendimiento'=Rendimiento.en,
           'Precio.medio.rural'=Precio.medio)
  tbls[[i]] <- t
}
for (i in c(2,4,9)){
  t <- test[i] %>% 
    as.data.frame() %>% 
    slice(-c(1,2)) %>% 
    rename('Especie'=X, 
           'Superficie.sembrada'=Superficie, 
           'Superficie.cosechada'=Superficie.1, 
           'Volumen.de.produccion'=Volumen.de,
           'Valor.de.produccion'=Valor.de,
           'Rendimiento'=Rendimiento.en,
           'Precio.medio.rural'=Precio.medio)
  t <- t[-2]
  tbls[[i]] <- t
}
df2 <- bind_rows(tbls)
df2$Especie <- df2$Especie %>% 
  str_squish()

# Extract and format part 3 as dataframe
test <- extract_tables(fname, pages=seq(24,34), method='stream', output='data.frame')
tbls <- list()
for (i in c(1,3,5,6,7,8,10,11)){
  t <- test[i] %>% 
    as.data.frame() %>% 
    slice(-c(1,2)) %>% 
    rename('Especie'=X, 
           'Valor.de.exportación'=Valor.de, 
           'Valor.del.polinizador'=Valor.del, 
           'Valor.de.producción.por.área'=Valor.de.producción,
           'Tasa.de.cambio.por.cambio.climático'=Tasa.de.cambio.por,
           'Superficie.cosechada.esperada.por.cambio.climático'=Superficie.cosechada)
  tbls[[i]] <- t
}
for (i in c(2,4,9)){
  t <- test[i] %>% 
    as.data.frame() %>% 
    slice(-c(1,2)) %>% 
    rename('Especie'=X, 
           'Valor.de.exportación'=Valor.de, 
           'Valor.del.polinizador'=Valor.del, 
           'Valor.de.producción.por.área'=Valor.de.producción,
           'Tasa.de.cambio.por.cambio.climático'=Tasa.de.cambio.por,
           'Superficie.cosechada.esperada.por.cambio.climático'=Superficie.cosechada)
  t <- t[-2]
  tbls[[i]] <- t
}
df3 <- bind_rows(tbls)
df3$Especie <- df3$Especie %>% 
  str_squish()


# 
# # Extract and format part 4 as dataframe
# tbls <- list()
# test <- extract_tables(fname, pages=c(35,39,42,43,45), method='stream', output='data.frame')
# View(as.data.frame(test[5]))
# for (i in c(1,2,3,5)){
#   t <- test[i] %>% 
#     as.data.frame() %>% 
#     slice(-c(1,2)) %>% 
#     rename('Especie'=X, 
#            'Valor.de.producción.2050.tendencia'=Valor.de.producción.esperada, 
#            'Valor.de.producción.2050.segun.cambio.climatico'=Valor.de.producción)
#   tbls[[i]] <- t
# }
# for (i in c(4)){
#   t <- test[i] %>% 
#     as.data.frame() %>% 
#     slice(-c(1,2)) %>% 
#     rename('Especie'=X, 
#            'Valor.de.producción.2050.tendencia'=Valor.de.producción.esperada, 
#            'Valor.de.producción.2050.segun.cambio.climatico'=Valor.de.producción)
#   t <- t[-2]
#   tbls[[i]] <- t
# }
# df4a <- bind_rows(tbls)
# 
# badpages <- c(36,37,38,40,41,44)
# test <- extract_tables(fname, pages=36, method='lattice', output='data.frame')
# View(as.data.frame(test))
# t1 <- as.data.frame(test)
# v2 <- t1$Valor.de.producción.esperadaValor.de.producción
# View(v2)
# v3 <- strsplit(v2, split=',')
# View(v3)
# 
# 
# for (i in c(1)){
#   t <- test[i] %>% 
#     as.data.frame() %>% 
#     slice(-c(1,2)) %>% 
#     rename('Especie'=X, 
#            'Valor.de.producción.2050.tendencia'=Valor.de.producción.esperada, 
#            'Valor.de.producción.2050.segun.cambio.climatico'=Valor.de.producción)
#   tbls[[i]] <- t
# }
# colnames(as.data.frame(test[1]))
# 
# tbls <- list()
# for (i in c(1,3,5,6,7,8,10,11)){
#   t <- test[i] %>% 
#     as.data.frame() %>% 
#     slice(-c(1,2)) %>% 
#     rename('Especie'=X, 
#            'Valor.de.producción.2050.tendencia'=Valor.de.producción.esperada, 
#            'Valor.de.producción.2050.segun.cambio.climatico'=Valor.de.producción)
#   tbls[[i]] <- t
# }
# for (i in c(2,4,9)){
#   t <- test[i] %>% 
#     as.data.frame() %>% 
#     slice(-c(1,2)) %>% 
#     rename('Especie'=X, 
#            'Valor.de.producción.2050.tendencia'=Valor.de.producción.esperada, 
#            'Valor.de.producción.2050.segun.cambio.climatico'=Valor.de.producción)
#   t <- t[-2]
#   tbls[[i]] <- t
# }
# df4 <- bind_rows(tbls)

# Combine
repstrs <- c('ca cao', 'me xicana', 'Neobuxbaumi a', 'strept acantha', 'dive rsifolia', 'dig yna', 'olera cea')
for (s in repstrs){
  df1$Especie <- gsub(s, sub(' ', '', s), df1$Especie)
  df2$Especie <- gsub(s, sub(' ', '', s), df2$Especie)
  df3$Especie <- gsub(s, sub(' ', '', s), df3$Especie)
}
df <- df1 %>% 
  full_join(df2) %>% 
  full_join(df3)
write_csv(df, path='~/GitHub/polinizadores/data_in/app1_cultivos.csv')

# Convert Apendice 2 to a dataframe. -------------------------------------------
# Adrián converted PDF to Excel in Adobe, but pages 11 and 12 didn't work, 
# so I saved them as text and ran the following.
fname <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/pp11.txt'
fname_out <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/pp11_a.csv'

read_file(fname) %>% 
  str_replace_all('\n ', '\n ,') %>% 
  str_replace_all('[:blank:]{2,}', ';') %>% 
  write_file(fname_out)

fname <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/pp12.txt'
fname_out <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/pp12_a.csv'

read_file(fname) %>% 
  str_replace_all('\n ', '\n ,') %>% 
  str_replace_all('[:blank:]{2,}', ';') %>% 
  write_file(fname_out)

# Manual steps to get all apendice2 data into file Apendice2.csv

# Import Apendice2 data
fname <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/Apendice2.csv'
file.exists(fname)

df <- read_delim(fname, delim=';', trim_ws=T) %>% 
  fill(Familia, Cultivo)
df %>% dplyr::select(Cultivo) %>% distinct %>% nrow







# Work with Appendice 1 data ---------------------------------------------------
fname <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/appendices/app1_cultivos.csv'
df <- read_csv(fname, na=c('- -'), trim_ws=T) %>% 
  mutate(across(all_of(names(df)), ~ str_replace_all(.x, ',', ''))) %>%
  type_convert(cols(Superficie.sembrada=col_integer(), 
                    Superficie.cosechada=col_integer(),
                    Volumen.de.produccion=col_integer(),
                    Valor.de.produccion=col_integer(),
                    Rendimiento=col_double(),
                    Precio.medio.rural=col_double(),
                    Valor.de.exportación=col_integer(),
                    Valor.del.polinizador=col_integer(),
                    Valor.de.producción.por.área=col_integer(),
                    Tasa.de.cambio.por.cambio.climático=col_integer(),
                    Superficie.cosechada.esperada.por.cambio.climático=col_integer()
  )) %>% 
  mutate(Especie = str_replace_all(Especie, '[:space:]+', ' ') %>% 
           str_replace_all(' \\(.*\\)', '') %>% 
           str_trim, 
         Nombre.común.en.español = 
           stringi::stri_trans_general(Nombre.común.en.español, id='Latin-ASCII'))

# Get crops that are dependent on pollinators
dependents <- df %>% 
  filter(str_detect(Importancia.de.la.polinización, '[D\\?]$'))

# Sort crops by certain columns
df %>% 
  arrange(desc(Valor.del.polinizador)) %>% 
  dplyr::select(Especie, Nombre.común.en.español, Tipo.de.manejo, Importancia.de.la.polinización, Valor.de.produccion, Valor.del.polinizador) %>% 
  head(20)
df %>% 
  arrange(desc(Valor.de.produccion)) %>% 
  dplyr::select(Especie, Nombre.común.en.español, Tipo.de.manejo, Importancia.de.la.polinización, Valor.de.produccion, Valor.del.polinizador) %>% 
  head(20)
df %>% 
  arrange(desc(Valor.de.producción.por.área)) %>% 
  dplyr::select(Especie, Nombre.común.en.español, Tipo.de.manejo, Importancia.de.la.polinización, Valor.de.produccion, Valor.del.polinizador) %>% 
  head(20)
df %>% 
  arrange(desc(Precio.medio.rural)) %>% 
  dplyr::select(Especie, Nombre.común.en.español, Tipo.de.manejo, Importancia.de.la.polinización, Valor.de.produccion, Valor.del.polinizador) %>% 
  head(20)
df[250, c("Valor.del.polinizador", 'Especie')]


dependents %>% 
  arrange(desc(Valor.de.produccion)) %>% 
  dplyr::select(Especie, Nombre.común.en.español, Tipo.de.manejo, Importancia.de.la.polinización, Valor.de.produccion, Valor.del.polinizador) %>% 
  head(20)
dependents %>% 
  arrange(desc(Valor.de.producción.por.área)) %>% 
  dplyr::select(Especie, Nombre.común.en.español, Tipo.de.manejo, Importancia.de.la.polinización, Valor.de.produccion, Valor.del.polinizador) %>% 
  head(20)
dependents %>% 
  arrange(desc(Precio.medio.rural)) %>% 
  dplyr::select(Especie, Nombre.común.en.español, Tipo.de.manejo, Importancia.de.la.polinización, Valor.de.produccion, Valor.del.polinizador) %>% 
  head(20)

# Import Ashworth table from Martín -----
fname <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/BaseCultivosAsworth_MXname.xlsx'
df_ashworth <- readxl::read_xlsx(fname, trim_ws=T, .name_repair='universal', n_max=146) %>% 
  mutate(Scientific.name = str_replace_all(Scientific.name, '[:space:]+', ' ') %>% 
           str_replace_all('Lecythis sp\\.', 'Lecythis usitata') %>% 
           str_replace_all('Rubus liebmanii','Rubus liebmannii') %>% 
           str_replace_all('Macadamia integrifolia', 'Macadamia ternifolia') %>% 
           str_replace_all('Citrus aurantifolia', 'Citrus x aurantifolia') %>% 
           str_replace_all('Citrus limon', 'Citrus x limon') %>% 
           str_replace_all('Citrus paradisii', 'Citrus paradisi') %>% 
           str_replace_all('Capsicum frutescen', 'Capsicum frutescens') %>% 
           str_replace_all('Nopalxochia phyllanthoides', 'Disocactus phyllanthoides')
  )

# Join
only_app <- setdiff(df$Especie, df_ashworth$Scientific.name)
only_ash <- setdiff(df_ashworth$Scientific.name, df$Especie)

only_app_df <- df %>% 
  filter(Especie %in% only_app) %>% 
  select(Especie, Nombre.común.en.español) %>% 
  mutate(Nombre.común.en.español = str_to_lower(Nombre.común.en.español))
only_ash_df <- df_ashworth %>% 
  filter(Scientific.name %in% only_ash) %>% 
  select(Scientific.name, Common.name.Mex) %>% 
  mutate(Common.name.Mex = str_to_lower(Common.name.Mex) %>% 
           stringi::stri_trans_general(id='Latin-ASCII'))

# Look for matches----
i <- 6
only_ash_df[[i,1]]
(nm <- only_ash_df[[i,2]])
(app_results <- only_app_df %>% filter(str_detect(Nombre.común.en.español, str_c('.*', nm, '.*'))))
(app_results <- only_app_df %>% filter(str_detect(Nombre.común.en.español, str_c('.*', str_extract(nm, '\\w*'), '.*'))))

(spec1 <- app_results[1,1])
spec1 %>% gnr_resolve(best_match_only=T) %>% select(matched_name)
(spec2 <- only_ash_df[i,1])
spec2 %>% gnr_resolve(best_match_only=T) %>% select(matched_name)

# Join ----
df_join <- dependents %>% full_join(df_ashworth, by=c('Especie'='Scientific.name'))
df_join %>% colnames

df_join %>% 
  filter(Level.of.pollinator.dependence=='E') %>% 
  arrange(desc(Valor.del.polinizador)) %>% 
  dplyr::select(Especie, Nombre.común.en.español, Tipo.de.manejo, Importancia.de.la.polinización, Valor.de.produccion, Valor.del.polinizador) %>% 
  head(20)
df_join %>% 
  filter(Level.of.pollinator.dependence=='E') %>% 
  arrange(desc(Valor.de.produccion)) %>% 
  dplyr::select(Especie, Nombre.común.en.español, Tipo.de.manejo, Importancia.de.la.polinización, Valor.de.produccion, Valor.del.polinizador) %>% 
  head(20)
df_join %>% 
  filter(Level.of.pollinator.dependence=='E') %>% 
  arrange(desc(Valor.de.producción.por.área)) %>% 
  dplyr::select(Especie, Nombre.común.en.español, Tipo.de.manejo, Importancia.de.la.polinización, Valor.de.produccion, Valor.del.polinizador) %>% 
  head(20)

