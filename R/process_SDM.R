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

# Environmental data -----------------------------------------------------------
# Load data -----
mex <- getData('GADM', country='MEX', level=0, 
               path='data/input_data/context_Mexico')
bb <- st_bbox(mex) 
crop_dir <- file.path('data', 'input_data', 'environment_variables', 'cropped')

# Preprocess environmental variables (reproject & crop) ------------------------
worldclim_dir <- 'data/input_data/environment_variables/WorldClim'
biomes_dir <- 'data/input_data/environment_variables/TEOW_WWF_biome'
lc_dir <- 'data/input_data/ESA_LandCover_2016_2018'

# Download WorldClim Bioclimatic variables ----
# Fick and Hijmans 2017 
# res: 0.5 deg (?)
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

# Terrestrial biome -----
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

# Land cover ---- http://maps.elie.ucl.ac.be/CCI/viewer/download.php
# original res: 300 m, much finer than the worldclim variables
lc_fp <- list.files(path=lc_dir, pattern='.tif', full.names = T)
crop_fp <- file.path(crop_dir, basename(lc_fp))
crop_flist <- list.files(crop_dir, '.tif', full.names = T)
temp <- raster(crop_flist[[2]])
gdalUtils::gdalwarp(srcfile=lc_fp, dstfile=crop_fp, 
                    te=st_bbox(temp), 
                    tr=c(xres(temp), yres(temp)), 
                    r='mode',
                    overwrite=T)
newr <- raster(crop_fp) %>% as.factor()

# alternative land cover from ESA
nc_fp <- 'data/input_data/ESA_LandCover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7cds.tif'
# Code to convert netCDF to GeoTiff and crop/reproject appropriately:
# gdalwarp -of Gtiff -co COMPRESS=LZW -co TILED=YES -ot Byte -te -118.36889 14.53292 -86.71014 32.71804 -tr 0.008333444 0.008334154 -t_srs EPSG:4326 -r mode NETCDF:ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7cds.nc:lccs_class ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7cds.tif 
newr <- raster(nc_fp) 

# Elevation ----
alt_orig_fp <- 'data/input_data/environment_variables/MEX_msk_alt.tif'
alt <- getData('alt', country='MEX', mask=TRUE)
alt %>% writeRaster(alt_orig_fp)
crop_fp <- file.path(crop_dir, basename(alt_orig_fp))
crop_flist <- list.files(crop_dir, '.tif', full.names = T)
temp <- raster(crop_flist[[3]])
gdalUtils::gdalwarp(srcfile=alt_orig_fp, dstfile=crop_fp, 
                    te=st_bbox(temp), 
                    tr=c(xres(temp), yres(temp)), 
                    r='bilinear',
                    overwrite=T)
newr_alt <- raster(crop_fp)


# Analysis ---------------------------------------------------------------------

# Get GBIF data (previously process) and filter to one species
df <- readRDS('data/data_out/r_data/gbif_pol_points.rds')
sp <- 'Apis mellifera'
df_sp <- df %>% 
  filter(species == sp)

# Create raster stack of the predictor variables
fps <- list.files(crop_dir, '.tif', full.names=T)
predictors <- stack(fps)
names(predictors) <- names(predictors) %>% 
  str_sub(start=-6) %>% 
  str_replace_all('_', '') %>% 
  str_replace_all('skalt', 'alt') %>% 
  str_replace_all('.*cds$', 'lc')
predictors[['lc']] <- predictors[['lc']] %>% as.factor


mapview(predictors)

# Extract values for ANPs ------------------------------------------------------
anp_fp <- 'data/input_data/context_Mexico/SHAPE_ANPS/182ANP_Geo_ITRF08_Julio04_2019.shp'
anps <- st_read(anp_fp) %>% 
  st_transform(crs=4326) %>% 
  as('Spatial')

lyrs <- c('lc', # ESA land cover (LCCS product)
          'alt', # SRTM altitude
          'bio1', # mean annual temp
          'bio4', # temp seasonality
          'bio12', # total annual precipitation
          'bio15' # precipitation seasonality
)
predictors_slim <- predictors[[lyrs]]
predictors_num <- predictors_slim[[-1]]
anp_vals <- raster::extract(predictors_num, anps, fun=mean, na.rm=T, sp=T)
anp_vals %>% head

# Extract values for (basic) modeling ------------------------------------------
presvals <- raster::extract(predictors, df_sp)

# Create pseudo-absence points ----
# Sampling bias
# - basic option is subsampling with gridding; sample within radius
# - Phillips et al. (2009) propose target-group background data

# Sample within radius of presence points
set.seed(10)
# Create buffer zone around points and within Mex
# Mexico boundaries
mex <- mex %>% 
  st_as_sf %>% 
  st_transform(crs=6372) %>% 
  st_simplify(dTolerance=5000)
# Buffer zone
buff_zone <- df_sp %>% 
  st_transform(crs=6372) %>% 
  st_buffer(50000) %>% 
  group_by %>% 
  summarize %>% 
  st_intersection(mex)

# Look
mapview(predictors[[1]]) + mapview(buff_zone) + mapview(mex)

# Sample randomly
samp1 <- buff_zone %>% 
  st_sample(size=1100) %>% 
  st_transform(crs=4326) %>% 
  as_Spatial()

# Use random points to sample grid cells
mask <- predictors[[2]]
cells <- cellFromXY(mask, xy=samp1) %>% 
  unique
xy <- xyFromCell(mask, cells) %>% 
  as_tibble %>% 
  drop_na %>% 
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
  mutate(lc = as.factor(lc))
head(sdmdata)

# Visually check for colinearity - see Dormann et al. 2013
pairs(sdmdata[,3:6], cex=0.1)

saveRDS(sdmdata, "data/data_out/r_data/sdm.Rds")
saveRDS(presvals, "data/data_out/r_data/pvals.Rds")

# Model fitting ----------------------------------------------------------------
sdmdata <- readRDS("data/data_out/r_data/sdm.Rds")
presvals <- readRDS("data/data_out/r_data/pvals.Rds")

# GLM
m1 <- glm(pb ~ lc + alt + bio1 + bio4 + bio12 + bio15, data=sdmdata)
summary(m1)
m2 <- glm(pb ~ ., data=sdmdata)
summary(m2)
pairs(sdmdata, cex=0.1)

# Look at dismo::bioclim, which only uses presense points
bio_vars <- presvals %>% colnames
bc <- bioclim(presvals[,bio_vars[-1]])
response(bc)

# Suitability map from Bioclim model (without biomes)
p_bc <- predict(predictors[[-1]], bc)

# Bioclim with all predictors
bc <- bioclim(presvals)
class(bc)
bc

# Make suitability map
p <- predict(predictors, m1)
p2 <- predict(predictors, m2)
p_bc <- predict(predictors, bc)
mapview(p) + 
  mapview(p_bc) +
  mapview(df_sp, cex=.2)

# Model evaluation -----------
# Does the model make sense ecologically?
# Do the fitted functions make sense?
# Do the predictions seem reasonable?
# Are there spatial patterns in the residuals?


# MaxEnt -----------------------------------------------------------------------
# Split presence points into training and testing sets
set.seed(0)
group <- kfold(df_sp, 5)
pres_train <- df_sp[group != 1, ] %>% as('Spatial')
pres_test <- df_sp[group == 1, ] %>% as('Spatial')

# Background data for training and testing
set.seed(10)
# random points throughout Mexico
pred_nf <- dropLayer(predictors, c('lc', 'biomes'))
ext <- extent(mex)
backg <- randomPoints(pred_nf, n=1000, ext=ext, extf = 1.25)
backg %>% 
  as_tibble %>% 
  drop_na %>% 
  st_as_sf(x = .,                         
           coords = c('x', "y"),
           crs = 4326) %>% 
  mapview(cex=4) + mapview(xy, cex=4)
colnames(backg) = c('lon', 'lat')
# random points from within buffer of presence points
backg <- xy
# divide into train and test
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ] %>% as('Spatial')
backg_test <- backg[group == 1, ] %>% as('Spatial')
mapview(backg_test) + mapview(backg_train)

# Fit MaxEnt model ----
# All predictors, all points
xm <- maxent(predictors, pres_train, factors=c('lc', 'biomes'))
plot(xm)
response(xm)
e <- evaluate(pres_test, backg_test, xm, predictors)

# All predictors, remove duplicate points
xm_rm <- maxent(predictors, pres_train, factors=c('lc', 'biomes'), removeDuplicates=T)
plot(xm_rm)
response(xm_rm)
e <- evaluate(pres_test, backg_test, xm_rm, predictors)
px <- predict(predictors, xm_rm, ext=ext, progress='')
(tr <- threshold(e, 'spec_sens'))
mapview(px) + mapview(px > tr)

# Slimmed predictors
# Variables in Nogué et al. 2016: 
lyrs <- c('lc', # ESA land cover (LCCS product)
          'alt', # SRTM altitude
          'bio1', # mean annual temp
          'bio4', # temp seasonality
          'bio12', # total annual precipitation
          'bio15' # precipitation seasonality
)
predictors_slim <- predictors[[lyrs]]
xm_slim <- maxent(predictors_slim, pres_train, factors='lc', removeDuplicates=T)
plot(xm_slim)
response(xm_slim)
e_slim <- evaluate(pres_test, backg_test, xm_slim, predictors)
px_slim <- predict(predictors, xm_slim, ext=ext, progress='')
(tr <- threshold(e_slim, 'spec_sens'))
mapview(px_slim) + mapview(px_slim > tr)
