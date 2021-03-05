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

# Initialize -----
# Mexico
# mex_fp <- 'data/input_data/context_Mexico/SNIB_divisionpolitica/dest2018gw/dest2018gw.shp'
# crs <- 6362 # used by INEGI for all of Mexico
# mex <- st_read(mex_fp) %>% 
#   st_transform(crs=crs) %>% 
#   st_simplify(dTolerance=40, preserveTopology=T) 
mex <- getData('GADM', country='MEX', level=0,
               path='data/input_data/context_Mexico')
bb <- st_bbox(mex) 
crop_dir <- file.path('data', 'input_data', 'environment_variables', 'cropped')

# PRE-PROCESS environmental variables (reproject & crop) ------------------------
worldclim_dir <- 'data/input_data/environment_variables/WorldClim'
wwf_biomes_dir <- 'data/input_data/environment_variables/TEOW_WWF_biome'
biomes_dir <- 'data/input_data/environment_variables/CONABIO'
lc_dir <- 'data/input_data/ESA_LandCover'

# WorldClim Bioclimatic variables ----
# Fick and Hijmans 2017 
srclist <- list.files(path=worldclim_dir, pattern='.tif', full.names = F)
crplist <- list.files(path=crop_dir, pattern='.tif', full.names = F)
if(!all(srclist %in% crplist)) {
  # res: 0.5 deg (?)
  url <- 'https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_bio.zip'
  fname <- url %>% basename()
  
  # Download and unzip
  download.file(url, dest=fname)
  fps <- fname %>% 
    unzip(list=F, exdir=worldclim_dir) 
  unlink(fname)
  
  # Crop files
  srclist <- list.files(path=worldclim_dir, pattern='.tif', full.names = TRUE)
  dstlist <- file.path(crop_dir, basename(srclist))
  
  # Crop files
  for (i in seq_along(srclist)) {
    gdalUtils::gdalwarp(srcfile=srclist[[i]], 
                        dstfile=dstlist[[i]], 
                        te=bb, 
                        overwrite=T)
  }
}

# WWF ecoregions ----
# Olson et al. 2001: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
if(!file.exists(file.path(wwf_biomes_dir, 'wwf_terr_ecos.shp'))) {
  data_dir <- wwf_biomes_dir
  url <- 'https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip'
  fname <- url %>% basename()
  download.file(url, dest=fname)
  dir.create(data_dir)
  f <- fname %>% 
    unzip(list=F, exdir=data_dir) 
  unlink(fname)
}

# CONABIO Mexican ecoregions -----
# Load and crop polygons
data_dir <- biomes_dir
shp_fp <- list.files(data_dir, '.shp$', full.names = T, recursive = T)
biomes_crop <- st_read(shp_fp)
# biomes_crop <- biomes_crop %>% st_crop(bb)

# Get template raster
crop_flist <- list.files(crop_dir, 'wc.*.tif$', full.names = T)
temp <- raster(crop_flist[[5]])

# See unique values of value field
val_flds <- biomes_crop %>% st_drop_geometry() %>% select(starts_with('CVE')) %>% tbl_vars

for (val_fld in val_flds) {
  
  # Output filepaths
  ras_fp <- file.path(crop_dir, str_glue('biomes_{val_fld}.tif'))
  csv_fp <- file.path(crop_dir, str_glue('biomes_{val_fld}.csv'))
  
  if (!(file.exists(ras_fp) & file.exists(csv_fp))){
    
    # Dissolve by value field
    biomes_crp1 <- biomes_crop %>% 
      group_by(.data[[val_fld]]) %>% 
      summarize() %>% 
      rownames_to_column()
    
    # Drop all columns
    biomes_spat <- biomes_crp1 %>% 
      select() %>%
      as('Spatial')
    
    # Convert to raster
    biomes_rst <- biomes_spat %>% 
      rasterize(y = temp)
    
    # Save raster
    writeRaster(biomes_rst, filename=ras_fp, overwrite=T)
    
    # Save ID to biome lookup table
    biomes_crp1 %>% st_drop_geometry() %>% write_csv(csv_fp)
  }
}

# ESA land cover ---- 
# ESA CCI land cover, two products
# http://maps.elie.ucl.ac.be/CCI/viewer/download.php
# original res: 300 m, much finer than the worldclim variables
crop_flist <- list.files(crop_dir, 'wc.*.tif$', full.names = T)
temp <- raster(crop_flist[[1]])

lc_fps <- list.files(path=lc_dir, pattern='.tif$', full.names = T)
crop_fps <- file.path(crop_dir, basename(lc_fp))

for (i in seq(1, length(crop_fps))){
  gdalUtils::gdalwarp(srcfile=lc_fps[[i]], dstfile=crop_fps[[i]], 
                      te=st_bbox(temp), 
                      tr=c(xres(temp), yres(temp)), 
                      r='mode',
                      overwrite=T)
}

# Get percent X land cover for each 30s grid cell - IN PROGRESS
lc_fp <- lc_fps[[1]]
f = extent(-100, -98, 23, 25)
lc <- raster(lc_fp) %>% crop(f)

lc_msk <- lc %>% 
  reclassify(rcl= cbind(from    = c(9, -1, 19), 
                        to      = c(19, 9, 255), 
                        becomes = c(1, 0, 0)
))

lc_agg10 <- lc_msk %>% aggregate(10, mean)

tm_shape(lc_msk) + tm_raster() +
  tm_shape(lc_agg10) + tm_raster()

# alternative land cover from ESA: 
# 300m 2015 product with typology defined by LCCS, for compliance with GLC2000, GlobCover 2005 and 2009
nc_fp <- 'data/input_data/ESA_LandCover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7cds.tif'
# Code to convert netCDF to GeoTiff and crop/reproject appropriately:
# gdalwarp -of Gtiff -co COMPRESS=LZW -co TILED=YES -ot Byte -te -118.36889 14.53292 -86.71014 32.71804 -tr 0.008333444 0.008334154 -t_srs EPSG:4326 -r mode NETCDF:ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7cds.nc:lccs_class ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7cds.tif 

# INEGI land cover ----
fp_usv <- "data/input_data/ag_INEGI_2017/other_sources/usv250s6gw.shp"

# Load and crop polygons
usv_sf <- st_read(fp_usv)

# Get template raster
crop_flist <- list.files(crop_dir, 'wc.*.tif$', full.names = T)
temp <- raster(crop_flist[[5]])

# See unique values of value field
# usv_sf %>% st_drop_geometry() %>% distinct(USV_SVI, CVE_UNION)
val_fld <- 'USV_SVI'
val_flds <- c('USV_SVI', 'CVE_UNION')

# Output filepaths
fn <- basename(fp_usv) %>% tools::file_path_sans_ext()
ras_fp <- file.path(crop_dir, str_glue('{fn}_{val_fld}.tif'))
csv_fp <- file.path(crop_dir, str_glue('{fn}_{val_fld}.csv'))

if (!(file.exists(ras_fp) & file.exists(csv_fp))){
  
  # Dissolve by value field
  sf_diss <- usv_sf %>% 
    st_transform(crs = 6362) %>% 
    st_simplify(dTolerance=80, preserveTopology=T) %>%
    st_make_valid() %>% 
    group_by(across(val_flds)) %>% 
    summarize() %>% 
    rownames_to_column()
  
  # Save dissolved file for GDAL
  diss_fp <- file.path('data/intermediate_data/usv', 
                       str_glue('{fn}_diss{val_fld}_tol80.gpkg'))
  sf_diss %>% st_write(diss_fp)
  
  # Rasterize
  writeRaster(raster(ext = extent(bb), resolution = res(temp)), 
              filename=ras_fp, overwrite=T, datatypeCharacter = 'INT2U')
  gdalUtils::gdal_rasterize(diss_fp, 
                            ras_fp, 
                            l = tools::file_path_sans_ext(basename(diss_fp)), 
                            a ='rowname')
  
  # Write lookup table
  sf_diss %>% st_drop_geometry() %>% write_csv(csv_fp)
}

# Elevation ----
alt_orig_fp <- 'data/input_data/environment_variables/MEX_msk_alt.tif'
crop_fp <- file.path(crop_dir, basename(alt_orig_fp))

if(!file.exists(crop_fp)) {
  
  # download altitude data 
  alt <- raster::getData('alt', country='MEX', mask=TRUE)
  alt %>% writeRaster(alt_orig_fp)
  
  # Get template file
  crop_flist <- list.files(crop_dir, 'wc.*.tif', full.names = T)
  temp <- raster(crop_flist[[1]])
  
  # resample
  gdalUtils::gdalwarp(srcfile=alt_orig_fp, dstfile=crop_fp, 
                      te=st_bbox(temp), 
                      tr=c(xres(temp), yres(temp)), 
                      r='bilinear',
                      overwrite=T)
}
