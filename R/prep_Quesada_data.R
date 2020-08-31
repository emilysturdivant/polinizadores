# Data are from GBIF
# parameters: https://www.gbif.org/developer/occurrence#parameters
# Require some sort of aggregation. You can see there are more data in more populated areas, which is probably not representative of pollinator hotspots.
# There are comparatively few points for flies and we know that can't be representative.

# library(tabulizer)
library(sf)
library(mapview)
library(tools)
library(rgdal)
# library(spatstat)
library(raster)
library(tmap)
library(leaflet)
library(tidyverse)
library(patchwork)
library(scales)

# Pollinator database ----
data_dir <- 'data/input_data/Quesada_bioclim_pol_y_cultivos/Completos'
fps <- list.files(data_dir, full.names = T)
fp <- fps[[6]]

(name <- fp %>% 
  basename %>% 
  file_path_sans_ext %>% 
  str_split('_') %>% 
  last %>% 
  last )
dat <- read_csv(fp)

# Look around ------------------------------------------------------------------
tot <- dat %>% nrow
tot_badcoords <- dat %>% 
  filter(is.na(decimalLongitude) | is.na(decimalLatitude) |
                 decimalLongitude == 0 | decimalLatitude == 0) %>% 
  nrow %>% 
  format(big.mark=',', trim=T)
dat <- dat %>% 
  drop_na(decimalLongitude, decimalLatitude) %>% 
  filter(decimalLongitude != 0 & decimalLatitude != 0,
         !str_detect(issues, 'COUNTRY_COORDINATE_MISMATCH'))
tot_goodcoords <- dat %>% 
  nrow %>% 
  format(big.mark=',', trim=T)

# Tables -----
(species_cnt <- dat %>% 
   group_by(genus) %>% 
   summarise(cnt = length(genus)) %>% 
   arrange(desc(cnt)))

(basisOfRecord_cnt <- dat %>% 
    group_by(basisOfRecord) %>% 
    summarise(cnt = length(basisOfRecord)) %>% 
    arrange(desc(cnt)))
# Number of species in each basisOfRecord
# (species_basis_cnt <- dat %>% 
#     group_by(species, basisOfRecord) %>% 
#     summarise(cnt = length(species)) %>% 
#     pivot_wider(id_cols=species, names_from=basisOfRecord, values_from=cnt) %>% 
#     left_join(species_cnt) %>% 
#     relocate(cnt, .after=species))
# Unique habitat values and quantity
(habitat_cnt <- dat %>% 
    group_by(habitat) %>% 
    summarise(cnt = length(habitat)) %>% 
    arrange(desc(cnt)))
tot_hab <- dat %>% select(habitat) %>% distinct %>% nrow
habitat_cnt %>% write_csv(file.path('figures', str_c('pol_', name, '_habitats.csv')))

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
institution_cnt %>% write_csv(file.path('figures', str_c('pol_', name, '_institutions.csv')))

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
(spec <- spec1 +
    geom_bar(stat='identity', show.legend = FALSE) +
    coord_flip() +
    theme_desc() +
    scale_y_continuous(labels = comma)  +
    labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = glue::glue('Valores únicos de "genus": {tot_genus}\nValores únicos de "species": {tot_species}')
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
# by species
rec2 <- ggplot(dat, aes(fill=basisOfRecord, x = species)) +
  geom_bar(position='stack') +
  coord_flip() +
  theme_desc() +
  theme(
    legend.position='bottom', 
    legend.title=element_blank()
  ) +
  scale_y_continuous(labels = comma)  + 
  labs(
    x = NULL,
    y = NULL,
    title = 'basisOfRecord'
  )

coords_hist <- dat %>% 
    mutate(coordinateUncertaintyInMeters = replace_na(coordinateUncertaintyInMeters, -5000)) %>% 
ggplot(aes(x=coordinateUncertaintyInMeters)) +
  geom_histogram() +
  # coord_flip() +
  theme_desc() +
  scale_y_continuous(labels = comma)  +
  labs(
    x = NULL,
    y = NULL,
    title = 'coordinateUncertaintyInMeters'
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
coords2 <- uncertainty_cnts %>% 
  group_by(coordinateUncertainty, basisOfRecord) %>%
  summarize(cnt = length(coordinateUncertainty)) %>% 
  ggplot(aes(fill=basisOfRecord, x = coordinateUncertainty, y =cnt)) +
  geom_bar(position='stack', stat='identity', show.legend = FALSE) +
  # coord_flip() +
  theme_desc() +
  scale_y_continuous(labels = comma)  +
  labs(
    x = NULL,
    y = NULL,
    title = 'coordinateUncertainty'
  )

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
# (spec + coords1) / rec1
# (spec | rec1) / (coords_hist | edates)
# patchwork <- spec | (rec1 / coords1 / edates)
patchwork <- spec | ((rec1 / coords1) / edates) | insts
patchwork + plot_annotation(
  title = glue::glue('{name}: {tot_goodcoords} registros evaluados'),
  subtitle = glue::glue('{tot_badcoords} registros eliminados por falta de coordenadas')
)
ggsave(file.path('figures', str_c('pol_', name, '_simple5.png')), width = 9.15, height=6.03)

# --------------------
# dates
(eventDate_cnt <- dat %>% 
    group_by(eventDate) %>% 
    summarise(cnt = length(eventDate)) %>% 
    arrange(desc(cnt)))
(eventDate_cnt <- dat %>% 
    group_by(dateIdentified) %>% 
    summarise(cnt = length(dateIdentified)) %>% 
    arrange(desc(cnt)))
(eventDate_cnt <- dat %>% 
    group_by(eventDate, dateIdentified) %>% 
    summarise(cnt = length(eventDate)) %>% 
    arrange(desc(cnt)))
# All points with eventDate are also missing dateIdentified

# Convert table to simple features data frame ----------------------------------
mex <- raster::getData('GADM', country='MEX', level=1, 
                       path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) 
# Data tidying steps: drop coords with NAs, drop duplicates, flag/drop imprecise
df <- dat %>% 
  drop_na(decimalLongitude, decimalLatitude) %>% 
  filter(decimalLongitude != 0 & decimalLatitude != 0,
         coordinateUncertaintyInMeters < 1000 | is.na(coordinateUncertaintyInMeters),
         !str_detect(issues, 'COUNTRY_COORDINATE_MISMATCH')) %>% 
  dplyr::select(species, genus, decimalLongitude, decimalLatitude, 
                eventDate, coordinateUncertaintyInMeters, habitat, 
                basisOfRecord, country, stateProvince, institutionCode) %>% 
  distinct %>% 
  st_as_sf(x = .,                         
           coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326)

# Maps -----
# df %>% mapview(zcol='coordinateUncertaintyInMeters', cex=4)

gen_list <- species_cnt %>% slice(1:16) %>% select(genus) %>% deframe
df_gen2 <- df %>% filter(genus %in% gen_list)
(tm <- tm_shape(mex) +
  tm_borders(col='darkgray') + 
  tm_shape(df_gen2) +
  tm_dots(alpha=0.4, size=.1, col = '#b10026') +
  tm_facets(by = 'genus', free.coords=F))
tmap_save(tm, filename = file.path('figures', str_c('pol_', name, '_map_genus_16.png')))

spec_list <- df %>% 
  st_set_geometry(NULL) %>% 
  group_by(species) %>% 
  summarise(cnt = length(species)) %>% 
  arrange(desc(cnt)) %>% 
  slice(1:16) %>% 
  select(species) %>% 
  deframe
df_spec1 <- df %>% filter(species %in% spec_list)
(tm <- tm_shape(mex) +
    tm_borders(col='darkgray') + 
  tm_shape(df_spec1) +
    tm_dots(alpha=0.4, size=.1, col = '#0c2c84') +
  tm_facets(by = 'species', free.coords=F))
tmap_save(tm, filename = file.path('figures', str_c('pol_', name, '_map_species_16.png')))

# Save -------------------------------------------------------------------------
fp_out <- file.path('data/data_out/pollinator_points', str_c(name, '.geojson'))
df %>% st_write(fp_out, delete_dsn=T)

# Hotspot ----------------------------------------------------------------------
mex <- raster::getData('GADM', country='MEX', level=0, 
               path='data/input_data/context_Mexico') %>%
  spTransform(CRSobj=CRS('+proj=utm +zone=13 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '))

# Using spatstat https://maczokni.github.io/crimemapping_textbook_bookdown/studying-spatial-point-patterns.html#inspecting-our-data-with-spatstat ----
fp_out <- file.path('data/data_out/pollinator_points', str_c(name, '.geojson'))
df <- st_read(fp_out)
mex <- raster::getData('GADM', country='MEX', level=0, 
               path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) 
pts <- df #%>% st_crop(st_bbox(mex))
tm_shape(mex) + 
  tm_fill() +
  tm_shape(pts) +
  tm_dots(alpha=0.4, size=1)

# start using spatstat
mex_sp <- as(mex, 'Spatial') %>% 
  spTransform(CRSobj=CRS('+proj=utm +zone=13 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '))
window <- maptools::as.owin.SpatialPolygons(mex_sp)

# extract coordinates into a matrix
pts <- pts %>% 
  st_transform('+proj=utm +zone=13 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ')
pts_coords <- matrix(unlist(pts$geometry), ncol = 2, byrow = T)

pts_ppp <- ppp(x = pts_coords[,1], y = pts_coords[,2],
               window = window, check = T)
jitter_pts <- rjitter(pts_ppp, retry=T, nsim=1, drop=T)

# Count quadrants
# Q <- quadratcount(jitter_pts, nx = 4, ny = 3)
# plot(jitter_pts)
# plot(Q, add = TRUE, cex = 2)

# Run a Chi Square test to check whether there's a statistically significant pattern
# quadrat.test(jitter_pts, nx = 3, ny = 2)

# Generate the density raster
dmap1 <- density.ppp(jitter_pts, sigma = bw.ppl(jitter_pts), edge=T)
r1 <- raster(dmap1)
r1[r1 < 0.00000001 ] <- NA
crs(r1) <- sp::CRS('+proj=utm +zone=13 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ')
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r1),
                    na.color = "transparent")
leaflet() %>% 
  # setView(lng = -2.225814, lat = 53.441315, zoom = 14) %>% 
  addTiles() %>%
  addRasterImage(r1, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(r1),
            title = "Bat map")

dmap2 <- density.ppp(jitter_pts, sigma = bw.diggle(jitter_pts), edge=T)
r2 <- raster(dmap1)
r2[r2 < 0.00000001 ] <- NA
crs(r2) <- sp::CRS('+proj=utm +zone=13 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ')
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r2),
                    na.color = "transparent")
leaflet() %>% 
  # setView(lng = -2.225814, lat = 53.441315, zoom = 14) %>% 
  addTiles() %>%
  addRasterImage(r2, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(r2),
            title = "Mariposas")

fp_out <- file.path('data/data_out/r_data', str_c('density_', name, '.rdata'))
save(dmap1, dmap2, jitter_pts, file=fp_out)



# Using adehabitatHR and guide by James Cheshire https://spatial.ly/2017/12/pointpatterns/ ----
crop_dir <- file.path('data', 'input_data', 'environment_variables', 'cropped')
ext <- mex %>%
  extent
resolution <- 500

# Create empty grid
x <- seq(ext[1],ext[2],by=resolution)  # where resolution is the pixel size you desire
y <- seq(ext[3],ext[4],by=resolution)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
plot(xy)
plot(mex, border="red", add=T)

# Create regular sample (not necessary)
xy <- st_sample(st_as_sfc(st_bbox(ext)), size=7000, type="regular")
plot(xy)
plot(mex, add=T)

# Run density estimation
pts_sp <- df %>% 
  # st_transform(crs=6368) %>% 
  as('Spatial') %>%
  spTransform(CRSobj=CRS('+proj=utm +zone=13 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '))
all <- raster(kernelUD(pts_sp, h="href", grid = xy)) 
#First results
plot(all)
plot(pts_sp)
plot(mex, border="red", add=T)


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
# Convert Apendice 1 to a dataframe. It looks like this produces some errors. Better just to ask for 

fname <- '/Users/emilysturdivant/GitHub/polinizadores/data_in/Apendice 1.pdf'

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

library(magrittr)

colnames(df)
# df <- gsub(',', '', df)
df[] <- lapply(df, gsub, pattern=',', replacement='')
df %<>% type_convert(cols(Superficie.sembrada=col_integer(), 
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
                          ))

df %>% 
  arrange(desc(Valor.del.polinizador)) %>% 
  head()
df[250, c("Valor.del.polinizador", 'Especie')]

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
