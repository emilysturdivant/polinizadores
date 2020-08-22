
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

df <- readRDS('data/data_out/r_data/gbif_pol_points.rds')

# Filter to one species
sp <- 'Apis mellifera'
df_sp <- df %>% 
  filter(species == sp)


# Sampling bias ----
# - basic option is subsampling with gridding
# - Phillips et al. (2009) propose target-group background data

# Presence and background points
library(dismo)
library(tmap)
tmap_mode('view')

files <- list.files(path=paste(system.file(package='dismo'), '/ex', sep=''), 
                    pattern='grd', full.names=T)

# Create random points ----
# Use raster from dismo cropped to Mexico extent
mask <- raster(files[1]) %>% 
  crop(extent(df))
# Create 500 random points
set.seed(1963)
bg <- randomPoints(mask, 500) %>% 
  as_tibble %>% 
  st_as_sf(x = .,                         
           coords = c('x', "y"),
           crs = 4326)
# Look
mapview(mask) + mapview(bg)


# Environmental data ----
mex <- getData('GADM', country='MEX', level=0, 
               path='data/input_data/context_Mexico')
bb <- st_bbox(mex) 
worldclim_dir <- 'data/input_data/environment_variables/WorldClim'
crop_dir <- file.path('data', 'input_data', 'environment_variables', 'cropped')
biomes_dir <- 'data/input_data/environment_variables/TEOW_WWF_biome'

# Download WorldClim Bioclimatic variables
# Fick and Hijmans 2017
data_dir <- worldclim_dir
url <- 'https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_bio.zip'
fname <- url %>% basename()
download.file(url, dest=fname)
fps <- fname %>% 
  unzip(list=F, exdir=data_dir) 
unlink(fname)

# Crop files
srclist <- list.files(path=data_dir, pattern='.tif', full.names = TRUE)
dstlist <- file.path(crop_dir, basename(srclist))

for (i in seq_along(srclist)) {
  gdalUtils::gdalwarp(srcfile=srclist[[i]], 
           dstfile=dstlist[[i]], 
           te=bb, 
           overwrite=T)
}

# Terrestrial biome
# Olson et al. 2001: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
url <- 'https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip'
data_dir <- biomes_dir
fname <- url %>% basename()
download.file(url, dest=fname)
dir.create(data_dir)
f <- fname %>% 
  unzip(list=F, exdir=data_dir) 
unlink(fname)

# Load and crop polygons
data_dir <- biomes_dir
shp_fp <- list.files(data_dir, '.shp$', full.names = T, recursive = T)
biomes_crop <- st_read(shp_fp) %>% 
  st_crop(bb)

# Rasterize
crop_flist <- list.files(crop_dir, '.tif', full.names = T)
temp <- raster(crop_flist[[2]])
biomes_rst <- biomes_crop %>% 
  as('Spatial') %>% 
  rasterize(y = temp, field='BIOME') %>% 
  as.factor()
plot(biomes_rst)
writeRaster(biomes_rst, filename=file.path(crop_dir, 'biomes.tif'), overwrite=T)

biomes_rst <- read_stars(file.path(crop_dir, 'biomes.tif'))

# Create raster stack of the predictor variables
fps <- list.files(crop_dir, '.tif', full.names=T)
predictors <- stack(fps)
names(predictors) <- names(predictors) %>% 
  str_sub(start=-6) %>% 
  str_replace_all('_', '')

# Extract values at presence and background points
presvals <- raster::extract(predictors, df_sp)
set.seed(0)

# Create pseudo-absence points ----
# sample within radius of presence points
mex <- mex %>% 
  st_as_sf %>% 
  st_transform(crs=6372) %>% 
  st_simplify(dTolerance=5000)
# Create buffer zone
buff_zone <- df_sp %>% 
  st_transform(crs=6372) %>% 
  st_buffer(50000) %>% 
  group_by %>% 
  summarize %>% 
  st_intersection(mex)
mapview(buff_zone) + mapview(mex)
# Sample randomly
samp1 <- buff_zone %>% 
  st_sample(size=600) %>% 
  st_transform(crs=4326) %>% 
  as_Spatial()

# Get unique cells from raster
mask <- predictors[[2]]
cells <- cellFromXY(mask, xy=samp1)
cells <- unique(cells)
xy <- xyFromCell(mask, cells) %>% 
  as_tibble %>% 
  drop_na() %>% 
  st_as_sf(x = .,                         
           coords = c('x', "y"),
           crs = 4326)

# Look
tm_shape(mask) + tm_raster() +
  tm_shape(buff_zone) + tm_borders() +
  tm_shape(xy) + tm_dots()

# Extract background values ----
absvals <- raster::extract(predictors, xy)

# Create DF for SDM with presence-background column
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals))) %>% 
  mutate(biomes = as.factor(biomes))
head(sdmdata)

# Visually check for colinearity - see Dormann et al. 2013
pairs(sdmdata[,3:6], cex=0.1)

saveRDS(sdmdata, "data/data_out/r_data/sdm.Rds")
saveRDS(presvals, "data/data_out/r_data/pvals.Rds")

# Model fitting ----------------------------------------------------------------
sdmdata <- readRDS("data/data_out/r_data/sdm.Rds")
presvals <- readRDS("data/data_out/r_data/pvals.Rds")

# GLM
m1 <- glm(pb ~ bio1 + bio5 + bio12, data=sdmdata)
summary(m1)
m2 <- glm(pb ~ ., data=sdmdata)
summary(m2)





