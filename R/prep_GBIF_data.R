
# Load libraries ----
library(sf)
library(tools)
library(maptools)
library(mapview)
library(units)
library(rgbif)
library(scrubr)
library(dismo)
# library(gdalUtils)
library(raster)
library(stars)
# library(osmdata)
library(tidyverse)
library(tmap)
tmap_mode('view')

# Get list of species
# splist <- readxl::read_excel(
#   'data/input_data/Quesada_bioclim_pol_y_cultivos/Informacion_general.xlsx', 
#   sheet='Polinizadores', skip=1) %>% 
#   select(Especie) %>% 
#   deframe
spdf <- readxl::read_excel(
  'data/input_data/Quesada_bioclim_pol_y_cultivos/Informacion_general.xlsx', 
  sheet='Polinizadores', skip=1) %>% 
  select(ID_numerico, ID_código, Especie) %>% 
  # drop_na(ID_numerico) %>% 
  mutate(Especie = str_replace_all(Especie, 'pennsylvanicus', 'pensylvanicus'))
keys <- spdf %>% 
  drop_na(ID_numerico) %>% 
  select(ID_numerico) %>% 
  deframe
mex_code <- isocodes[grep("Mexico", isocodes$name), "code"]

# rgbif Tutorial ----
occ_count(taxonKey = 2433207, georeferenced = T, country=mex_code)
taxrank()
out <- name_lookup(query = 'apis')
out$meta
head(out$data)
out$facets
out$names

head(name_lookup(query='Apis mellifera', rank="species", return="data"))

out <- name_usage(key=339333, language="SPANISH", data='vernacularNames')
head(out$data)
name_backbone(name='Apis', rank='genus', kingdom='Insecta')

out <- occ_get(key=c(2433206, 2433207, 8339697), return='data')

# Get all matches for given species in Mexico
sp <- 'Choeronycteris mexicana'
out <- name_suggest(q=sp, rank='species')$data
key <- out$key[1]
bats <- occ_search(taxonKey=key, country=mex_code, return='data')
bats <- bats$data

# Search for all species matches in Mexico
keys <- sapply(splist, function(x) name_suggest(x)$data$key[1], USE.NAMES=FALSE)
occ_search(taxonKey=keys, country='MX', hasCoordinate = T, return='data')

dat <- occ_search(taxonKey=keys[[1]], country='MX', hasCoordinate = T, return='data')
dat <- dat$data
plot(dat$decimalLongitude, dat$decimalLatitude)

# x <- occ_download_list(user=myuser, pwd=mypwd)
# (x$results <- tibble::as_tibble(x$results))

gbif_issues() %>% filter(code %in% c('cdround','cudc','gass84','txmathi'))

dat %>% occ_issues(gass84) %>% select(source, individualCount, behavior, country)

# SDM in R tutorial (https://rspatial.org/raster/sdm) --------------------------
# Use rgbif to download species matches in Mexico ----
# Trigona acapulconis doesn't exist in GBIF
sp <- 'Triepeolus utahensis'
(out <- name_suggest(q=sp, rank='species')$data)
key <- out$key[[1]]
out <- occ_search(taxonKey=key, country=mex_code)
(out <- out$data %>% select(taxonKey, species) %>% distinct)
out <- occ_search(taxonKey=6175457, country=mex_code)
(out <- out$data %>% select(taxonKey, species) %>% distinct)

# Get GBIF taxonKeys from species list 
get_key <- function(x) {
  out <- name_suggest(x, rank='species')$data 
  try(out$key[[1]])
}
splist <- spdf$Especie
keys <- map(splist, get_key) %>% compact
setdiff(keys, spdf$ID_numerico)
setdiff(spdf$ID_numerico, keys)

# Cue download and perform - only need to run once ----
res <- occ_download(pred_in('taxonKey', keys), 
                    pred('country', 'MX'))
occ_download_meta(res)
dat <- occ_download_get(res[[1]], overwrite=T) %>% 
  occ_download_import
dat %>% 
  saveRDS(file=str_c('data/data_out/r_data/gbif_polinizadores.RData'))

# Load previously downloaded data ----
dat <- readRDS(str_c('data/data_out/r_data/gbif_polinizadores.RData'))

# Looking around... ----
dat %>% colnames
dat %>% select(contains('issues'), contains('status')) %>% distinct
dat %>% select(contains('date')) %>% colnames
dat %>% select(occurrenceRemarks) %>% distinct
dat %>% select(hasGeospatialIssues, issue) %>% distinct
dat %>% select(basisOfRecord) %>% distinct
dat %>% select(dateIdentified) %>% distinct

dat %>% select(pointRadiusSpatialFit) %>% distinct
dat %>% select(previousIdentifications) %>% distinct
dat %>% select(contains('inst')) %>% distinct
dat %>% select(institutionID, institutionCode, ownerInstitutionCode, recordedBy) %>% distinct
dat %>% select(institutionCode) %>% distinct

sp_out <- dat %>% select(species) %>% distinct
setdiff(spdf$Especie, sp_out$species)
setdiff(sp_out$species, spdf$Especie)
keys_out <- dat %>% select(taxonKey) %>% distinct
setdiff(spdf$ID_numerico, keys_out$taxonKey)
setdiff(keys_out$taxonKey, spdf$ID_numerico)

# Filter ----
# "SDM with R" recommends: 
# - clean by geospatial issues, 
# - check for duplicate records (might have different institutions...) 
# - cross-check location with place (by spatial joining to administrative boundaries)
# - considering georeferencing observations w/o coordinates (does GBIF already do this? - I looked for records without coordinates, but they are almost entirely preserved specimens)
df <- dat %>% 
  drop_na(decimalLongitude, decimalLatitude) %>% 
  filter(!(basisOfRecord == 'PRESERVED_SPECIMEN' & is.na(year)),
         year > 1999 | is.na(year),
         !hasGeospatialIssues, 
         decimalLongitude != 0 & decimalLatitude != 0,
         coordinateUncertaintyInMeters < 3000 | is.na(coordinateUncertaintyInMeters),
         !str_detect(issue, 'COUNTRY_COORDINATE_MISMATCH'))

# Select variables
df <- df %>% 
  select(gbifID, species, 
         # place
         countryCode, stateProvince, county, municipality, locality, verbatimLocality,
         # location
         decimalLongitude, decimalLatitude, coordinateUncertaintyInMeters, 
         georeferenceRemarks, 
         # elevation
         verbatimElevation, elevation, elevationAccuracy,
         # time
         year, month, day,
         # collection info
         institutionID, institutionCode, ownerInstitutionCode, recordedBy,
         basisOfRecord)

# Look at duplicates
# dups2 <- duplicated(df[, c('decimalLongitude', 'decimalLatitude')])
# View(dups2 <- df[dups2,] )
# dups2 %>%  
#   st_as_sf(x = .,                         
#            coords = c("decimalLongitude", "decimalLatitude"),
#            crs = 4326,
#            remove=F) %>% 
#   mapview

# Clean using scrubr
# df <- dat %>%
#   coord_impossible(lat='decimalLatitude', lon='decimalLongitude') %>%
#   coord_incomplete(lat='decimalLatitude', lon='decimalLongitude') %>%
#   coord_unlikely(lat='decimalLatitude', lon='decimalLongitude')

df <- df %>%  
  st_as_sf(x = .,                         
           coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326,
           remove=F)
# mapview(df, zcol='species')
df %>% saveRDS('data/data_out/r_data/gbif_pol_points.rds')








