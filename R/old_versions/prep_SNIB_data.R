# Data are from GBIF
# parameters: https://www.gbif.org/developer/occurrence#parameters
# Require some sort of aggregation. You can see there are more data in more populated areas, which is probably not representative of pollinator hotspots.
# There are comparatively few points for flies and we know that can't be representative.

# library(tabulizer)
library(sf)
library(mapview)
library(tools)
library(rgdal)
library(raster)
library(tmap)
library(leaflet)
library(tidyverse)
library(patchwork)
library(scales)

# Pollinator database ----
data_dir <- 'data/input_data/SNIB'
fps <- list.files(data_dir, pattern='.shp', full.names = T, recursive=T)
fp <- fps[[2]]

(name <- fp %>% 
  basename %>% 
  file_path_sans_ext %>% 
  str_split('_') %>% 
  last %>% 
  last )
dat <- st_read(fp)

# Look around ------------------------------------------------------------------
dat %>% colnames # OBJECTI, grid_id, idCAT, fchClct, especie, geometry
# idCAT seems to be the ID for the species
(tot <- dat %>% nrow)
tot_goodcoords <- dat %>% 
  nrow %>% 
  format(big.mark=',', trim=T)
tot_badcoords <- 0

# Tables -----
(species_cnt <- dat %>% 
   group_by(especie) %>% 
   summarise(cnt = length(especie)) %>% 
   arrange(desc(cnt)))

(dates_cnt <- dat %>% 
    group_by(fchClct) %>% 
    summarise(cnt = length(fchClct)) %>% 
    arrange(desc(fchClct)))

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
title <- 'Especies'
tot_species <- dat %>% 
  select(especie) %>% 
  distinct %>% 
  nrow %>% 
  format(big.mark=',', trim=T)
(spec <- species_cnt %>% 
  slice(1:40) %>% 
  ggplot(aes(x=reorder(especie, cnt), y=cnt)) +
    geom_bar(stat='identity', show.legend = FALSE) +
    coord_flip() +
    theme_desc() +
    scale_y_continuous(labels = comma)  +
    labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = glue::glue('Valores únicos de "especie": {tot_species}')
    ))

# Dates
title <- 'Fechas'
tot_fechas <- dates_cnt %>% 
  nrow %>% 
  format(big.mark=',', trim=T)
tot_nulls <- dates_cnt %>% 
  st_set_geometry(NULL) %>% 
  filter(fchClct == 9999) %>% 
  select(cnt) %>% deframe %>% 
  format(big.mark=',', trim=T)
(dat1 <- dat %>% 
  mutate(fchClct = na_if(fchClct, 9999) %>% 
           replace_na(1850) %>% 
           as.character() %>%
           as.POSIXct(format='%Y'),
         ToHighlight = ifelse( fchClct == as.POSIXct('1850', format='%Y'), "yes", "no" )))

(edates <- dat1 %>% 
    ggplot(aes(fchClct, ..count.., fill = ToHighlight )) +
    geom_histogram() +
    coord_flip() +
    theme_desc()  +
    labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = glue::glue('Años únicos ("fchClct"): {tot_fechas}\nValores NULL: {tot_nulls}')
    ) + 
    scale_x_datetime(breaks = date_breaks("25 years"),
                     labels = date_format("%Y"),
                     limits = c(as.POSIXct("1835-01-01"),
                                as.POSIXct("2025-12-01")) ) +
    scale_y_continuous(labels = comma) +
    scale_fill_manual( values = c( "yes"="gray68", "no"="grey34" ), guide = FALSE ))

(dates_cnt1 <- dat1 %>% 
    group_by(fchClct) %>% 
    summarise(cnt = length(fchClct)) %>% 
    arrange(desc(fchClct)))
(edates2 <- dates_cnt1 %>% 
    ggplot(aes(x=fchClct, y=cnt)) +
    geom_bar(stat='identity', show.legend = FALSE) +
    coord_flip() +
    theme_desc() +
    scale_x_datetime(breaks = date_breaks("25 years"),
                     labels = date_format("%Y"),
                     limits = c(as.POSIXct("1835-01-01"),
                                as.POSIXct("2025-12-01")) ) +
    scale_y_continuous(labels = comma)  +
    labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = glue::glue('Años únicos ("fchClct"): {tot_fechas}\nValores NULL: {tot_nulls}')
    ))

# Simple
# (spec + edates)
# (spec | rec1) / (coords_hist | edates)
# patchwork <- spec | (rec1 / coords1 / edates)
rec1 <- plot_spacer() 
coords1 <- plot_spacer()
insts <- plot_spacer()
patchwork <- spec | ((rec1 / coords1) / edates) | insts
patchwork + plot_annotation(
  title = glue::glue('{name}: {tot_goodcoords} registros evaluados'),
  subtitle = glue::glue('{tot_badcoords} registros eliminados por falta de coordenadas')
)
ggsave(file.path('figures', name, str_c('pol_', name, '_simple5_snib.png')), width = 9.15, height=6.03)

# Convert table to simple features data frame ----------------------------------
mex <- raster::getData('GADM', country='MEX', level=1, 
                       path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) 

# Maps -----
df <- dat 
spec_list <- df %>% 
  st_set_geometry(NULL) %>% 
  group_by(especie) %>% 
  summarise(cnt = length(especie)) %>% 
  arrange(desc(cnt)) %>% 
  slice(1:16) %>% 
  select(especie) %>% 
  deframe
df_spec1 <- df %>% filter(especie %in% spec_list)
(tm <- tm_shape(mex) +
    tm_borders(col='darkgray') + 
  tm_shape(df_spec1) +
    tm_dots(alpha=0.4, size=.1, col = '#0c2c84') +
  tm_facets(by = 'especie', free.coords=F))
tmap_save(tm, filename = file.path('figures', name, str_c('pol_', name, '_map_species_16_snib.png')))

# Compare with Quesada/GBIF data -----------------------------------------------
name <- 'abejas'
dat_snib <- dat
dat_q <- df
(q_goodcoords <- dat_q %>% 
  nrow %>% 
  format(big.mark=',', trim=T))
(snib_goodcoords <- dat_snib %>% 
  nrow %>% 
  format(big.mark=',', trim=T))

# Standardize -----
# SNIB especies --> species; remove things in parentheses
dat_snib <- dat_snib %>% 
  mutate(species = especie %>% 
           str_replace_all(' \\(.*\\)', '') %>% 
           str_trim(), 
         fchClct = fchClct %>% as.character()) 
dat_snib %>% 
  st_set_geometry(NULL) %>% 
  select(species) %>% 
  distinct %>% 
  nrow
# Q eventDate --> fchClct; %Y
dat_q <- dat_q %>% 
  mutate(fchClct = eventDate %>% as.character %>% str_sub(1,4))


# Compare
library(arsenal)

d1 <- dat_q %>% 
  st_set_geometry(NULL) %>% 
  select(species, fchClct)
d2 <- dat_snib %>% 
  st_set_geometry(NULL) %>% 
  select(species, fchClct)
d2 %>% select(species) %>% 
  distinct %>% 
  nrow
diff <- comparedf(d1, d2)

d1 <- dat_q %>% 
  select(species, fchClct) %>% 
  st_transform(crs=4326)
d2 <- dat_snib %>% 
  select(species, fchClct) %>% 
  st_transform(crs=4326)
diffs <- setdiff(d1, d2)

st_equals(d1, d2)

# 
(snib1 <- dat_snib %>% filter(species %>% str_detect('Bombus weisi')))
(q1 <- dat_q %>% filter(species %>% str_detect('Bombus weisi')))

mapview(snib1, fill='yellow') +
  mapview(q1, fill='blue')
