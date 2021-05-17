# Get GBIF data based on orders/classes in Cultivo-Polinizadores table 

# Load libraries
library('rgbif')
library('tidyverse')

# Filepaths
crp_pllntrs_fp <- 'data/tidy/Quesada/crop_pllntrs_from_appendix2.csv'
raw_fp <- 'data/input_data/GBIF/gbif_crop_pllntrs.rds'
gbif_slim_fp <- 'data/input_data/GBIF/gbif_crop_pllntrs_slim.rds'

# Load data
ap2_df <- read_csv(crp_pllntrs_fp)

# List pollinator orders and families in crop table
upper_ranks <- ap2_df %>% 
  distinct(family, order) %>% 
  filter(!(is.na(family) & is.na(order)))

# Only get orders where family is blank
orders <- upper_ranks %>% 
  filter(is.na(family)) %>% 
  dplyr::select(name = order) %>% 
  mutate(rank = 'order')

# Only get families whose orders aren't already included 
families <- upper_ranks %>% 
  anti_join(orders, by = c(order = 'name')) %>% 
  dplyr::select(name = family) %>% 
  mutate(rank = 'family')

upp_ranks <- bind_rows(orders, families)

# Query GBIF ----
keys_df <- upp_ranks %>% 
  pmap_dfr(~ name_suggest(.x, rank = .y)$data )

# ~ Single download (prior method) ----
# keys <- keys_df %>% dplyr::select(key) %>% deframe
# 
# # Cue download and perform - only need to run once 
# res <- occ_download(pred_in('taxonKey', keys), 
#                     pred('country', 'MX'),
#                     pred_notnull('decimalLongitude'),
#                     pred_notnull('decimalLatitude'),
#                     pred_not(pred('decimalLongitude', 0)),
#                     pred_not(pred('decimalLatitude', 0)))
# 
# # Wait for download to be ready
# occ_download_meta(res)
# 
# # Perform download
# download_out <- occ_download_get(res[[1]], overwrite=T) 
# dat <- download_out %>% occ_download_import()
# datA <- dat %>%
#   filter(!str_detect(issue, 'COUNTRY_COORDINATE_MISMATCH')) 
# 
# # Save raw
# dat %>% saveRDS(raw_fp)
# 
# # Load raw ----
# dat <- readRDS(raw_fp)
# 
# # select only necessary columns
# vars <- c('gbifID',
#           # taxonomic
#           'species', 'genus', 'family', 'order', 'class',
#           # location
#           'decimalLongitude', 'decimalLatitude', 'coordinateUncertaintyInMeters', 
#           'georeferenceRemarks', 'issue',
#           # place
#           'country', 'countryCode', 'stateProvince', 'county', 
#           'municipality', 'locality', 'verbatimLocality',
#           # time
#           'eventDate',
#           # collection
#           'basisOfRecord', 'country', 'institutionCode',
#           'institutionID', 'ownerInstitutionCode', 'recordedBy',
#           'datasetName',
#           # elevation
#           'verbatimElevation', 'elevation', 'elevationAccuracy',
#           # misc
#           'occurrenceStatus', 'establishmentMeans'
# )
# dat1 <- dat %>% dplyr::select(matches(vars))
# 
# # Save
# dat1 %>% saveRDS(gbif_slim_fp)
# 
# dat1 <- readRDS(gbif_slim_fp)
# 
# 
# t1 <- dat1 %>% count(datasetName)
# t1 <- dat1 %>% count(establishmentMeans)

# ~ Iteratively cue GBIF download and perform ----
# Functions to improve iteration
create_gbif_download <- function(taxonKey, filter_uncertainty = TRUE){
  if(filter_uncertainty) {
    occ_download(pred_in('taxonKey', taxonKey), 
                 pred('country', 'MX'),
                 pred_notnull('decimalLongitude'),
                 pred_notnull('decimalLatitude'),
                 pred_not(pred('decimalLongitude', 0)),
                 pred_not(pred('decimalLatitude', 0)),
                 pred_not(pred_gte('coordinateUncertaintyInMeters', 1000))
    )
  } else {
    occ_download(pred_in('taxonKey', taxonKey), 
                 pred('country', 'MX'),
                 pred_notnull('decimalLongitude'),
                 pred_notnull('decimalLatitude'),
                 pred_not(pred('decimalLongitude', 0)),
                 pred_not(pred('decimalLatitude', 0))
    )
  }

}

download_and_save <- function(dl_res, raw_fp){
  
  # Perform download
  download_out <- occ_download_get(dl_res[[1]], overwrite=T) 
  dat <- download_out %>% occ_download_import()
  datA <- dat %>%
    filter(!str_detect(issue, 'COUNTRY_COORDINATE_MISMATCH')) 
  
  # Save raw
  datA %>% saveRDS(raw_fp)
  
  return(datA)
}

# Smaller class
(key_row12 <- keys_df %>% slice(12))
res12 <- create_gbif_download(key_row12$key)
(key_row11 <- keys_df %>% slice(11))
res11 <- create_gbif_download(key_row11$key, FALSE)
(key_row10 <- keys_df %>% slice(10))
res10 <- create_gbif_download(key_row10$key, FALSE)
(key_row9 <- keys_df %>% slice(9))
res9 <- create_gbif_download(key_row9$key, FALSE)
(key_row8 <- keys_df %>% slice(8))
res8 <- create_gbif_download(key_row8$key, FALSE)
(key_row7 <- keys_df %>% slice(7))
res7 <- create_gbif_download(key_row7$key, FALSE)
(key_row6 <- keys_df %>% slice(6))
res6 <- create_gbif_download(key_row6$key, FALSE)
(key_row5 <- keys_df %>% slice(5))
res5 <- create_gbif_download(key_row5$key, FALSE)
(key_row4 <- keys_df %>% slice(4))
res4 <- create_gbif_download(key_row4$key, FALSE)
(key_row3 <- keys_df %>% slice(3))
res3 <- create_gbif_download(key_row3$key, FALSE)
(key_row2 <- keys_df %>% slice(2))
res2 <- create_gbif_download(key_row2$key, FALSE)

(key_row1<- keys_df %>% slice(1))
res1 <- create_gbif_download(key_row1$key, FALSE)

# COMPLETED, includes NAs
raw_fp <- str_glue("data/input_data/GBIF/gbif_{key_row12$canonicalName}.rds")
if(!file.exists(raw_fp)){
  # Wait for download to be ready
  (dl_meta <- occ_download_meta(res12))
  if(dl_meta$status == 'SUCCEEDED'){
    dat <- download_and_save(res12, raw_fp)
  }
}

# COMPLETED, includes NAs
raw_fp <- str_glue("data/input_data/GBIF/gbif_{key_row11$canonicalName}.rds")
if(!file.exists(raw_fp)){
  # Wait for download to be ready
  (dl_meta <- occ_download_meta(res11))
  if(dl_meta$status == 'SUCCEEDED'){
    dat <- download_and_save(res11, raw_fp)
  }
}

# COMPLETED
raw_fp <- str_glue("data/input_data/GBIF/gbif_{key_row10$canonicalName}.rds")
if(!file.exists(raw_fp)){
  # Wait for download to be ready
  (dl_meta <- occ_download_meta(res10))
  if(dl_meta$status == 'SUCCEEDED'){
    dat <- download_and_save(res10, raw_fp)
  }
}

# COMPLETED
raw_fp <- str_glue("data/input_data/GBIF/gbif_{key_row9$canonicalName}.rds")
if(!file.exists(raw_fp)){
  # Wait for download to be ready
  (dl_meta <- occ_download_meta(res9))
  if(dl_meta$status == 'SUCCEEDED'){
    dat <- download_and_save(res9, raw_fp)
  }
}

# COMPLETED
raw_fp <- str_glue("data/input_data/GBIF/gbif_{key_row8$canonicalName}.rds")
if(!file.exists(raw_fp)){
  # Wait for download to be ready
  (dl_meta <- occ_download_meta(res8))
  if(dl_meta$status == 'SUCCEEDED'){
    dat <- download_and_save(res8, raw_fp)
  }
}

# COMPLETED
raw_fp <- str_glue("data/input_data/GBIF/gbif_{key_row7$canonicalName}.rds")
if(!file.exists(raw_fp)){
  # Wait for download to be ready
  (dl_meta <- occ_download_meta(res7))
  if(dl_meta$status == 'SUCCEEDED'){
    dat <- download_and_save(res7, raw_fp)
  }
}

# COMPLETED
raw_fp <- str_glue("data/input_data/GBIF/gbif_{key_row6$canonicalName}.rds")
if(!file.exists(raw_fp)){
  # Wait for download to be ready
  (dl_meta <- occ_download_meta(res6))
  if(dl_meta$status == 'SUCCEEDED'){
    dat <- download_and_save(res6, raw_fp)
  }
}

# COMPLETED
raw_fp <- str_glue("data/input_data/GBIF/gbif_{key_row5$canonicalName}.rds")
if(!file.exists(raw_fp)){
  # Wait for download to be ready
  (dl_meta <- occ_download_meta(res5))
  if(dl_meta$status == 'SUCCEEDED'){
    dat <- download_and_save(res5, raw_fp)
  }
}

# COMPLETED
raw_fp <- str_glue("data/input_data/GBIF/gbif_{key_row4$canonicalName}.rds")
if(!file.exists(raw_fp)){
  # Wait for download to be ready
  (dl_meta <- occ_download_meta(res4))
  if(dl_meta$status == 'SUCCEEDED'){
    dat <- download_and_save(res4, raw_fp)
  }
}

# COMPLETED
raw_fp <- str_glue("data/input_data/GBIF/gbif_{key_row3$canonicalName}.rds")
if(!file.exists(raw_fp)){
  # Wait for download to be ready
  (dl_meta <- occ_download_meta(res3))
  if(dl_meta$status == 'SUCCEEDED'){
    dat <- download_and_save(res3, raw_fp)
  }
}

# COMPLETED
raw_fp <- str_glue("data/input_data/GBIF/gbif_{key_row2$canonicalName}.rds")
if(!file.exists(raw_fp)){
  # Wait for download to be ready
  (dl_meta <- occ_download_meta(res2))
  if(dl_meta$status == 'SUCCEEDED'){
    dat <- download_and_save(res2, raw_fp)
  }
}

# XX
raw_fp <- str_glue("data/input_data/GBIF/gbif_{key_row1$canonicalName}.rds")
if(!file.exists(raw_fp)){
  # Wait for download to be ready
  (dl_meta <- occ_download_meta(res1))
  if(dl_meta$status == 'SUCCEEDED'){
    dat <- download_and_save(res1, raw_fp)
  }
}

# Compare Aphididae downloaded with the different uncertainty predicates
no_nulls <- readRDS(gbif_slim_fp)
w_nulls <- readRDS(raw_fp)

no_nulls %>% filter(order == 'Chiroptera') %>% nrow
w_nulls %>% filter(order == 'Chiroptera') %>% nrow

no_nulls %>% filter(is.na(coordinateUncertaintyInMeters)) %>% nrow
w_nulls %>% filter(is.na(coordinateUncertaintyInMeters)) %>% nrow


no_nulls %>%
  filter(order == 'Chiroptera') %>% 
  distinct(family)

# Assemble lowest taxonomic ranks that separate out pollinators ----
# bats based on apendice2 ----
query_name <- 'bat_genera'

# Convert apendice2 table to bat taxon lookup
bat_genera <- ap2_df %>% 
  filter(order == 'Chiroptera') %>% 
  distinct(genus) %>% 
  filter(!is.na(genus)) %>% 
  pivot_longer(genus, names_to = 'rank', values_to = 'name') %>% 
  select(name, rank)

# Check that entries aren't getting left out
ap2_df %>% 
  filter(order == 'Chiroptera') %>% 
  distinct(Polinizador, genus, family) %>% 
  anti_join(bat_genera, by = c(genus = 'name'))
  
# Query GBIF 
bat_genera_keys <- bat_genera %>% 
  pmap_dfr(~ name_suggest(.x, rank = .y)$data )

# Single download (prior method) 
keys <- bat_genera_keys %>% dplyr::select(key) %>% deframe

# Cue download
res <- occ_download(pred_in('taxonKey', keys), 
                    pred('country', 'MX'),
                    pred_notnull('decimalLongitude'),
                    pred_notnull('decimalLatitude'),
                    pred_not(pred('decimalLongitude', 0)),
                    pred_not(pred('decimalLatitude', 0)))

# Wait for download to be ready
(dl_meta <- occ_download_meta(res))
raw_fp <- str_glue("data/input_data/GBIF/gbif_{query_name}.rds")
bat_genera_dat <- download_and_save(res, raw_fp)

# Look
bat_genera_dat %>% count(species, genus, family)
bat_genera_dat %>% filter(family == 'Pteropodidae')

# My concern with this approach is that I might be missing some nectarivorous bats that were overlooked in appendix 2.


# Nectarivorous bats ----
# Appears that the tribe and subfamily values aren't maintained for bats. This didn't work.
query_name <- 'bat_query'

bat_query <- c('Glossophaginae', 'subfamily', 
               'Lonchophyllinae', 'subfamily', 
               'Glossophagini', 'tribe', 
               'Lonchophyllinigi', 'tribe', 
               'Pteropodidae', 'family') %>% 
  matrix(ncol = 2, byrow=TRUE) %>% 
  as_tibble()

# Query GBIF 
bat_query_keys <- bat_query %>% 
  pmap_dfr(~ name_suggest(.x, rank = .y)$data )

# birds based on apendice2 ----
query_name <- 'birds_genera'

df_filt <- ap2_df %>% filter(family == 'Trochilidae')
df_filt <- ap2_df %>% filter(class == 'Aves')

# Convert apendice2 table to bat taxon lookup
genera <- df_filt %>% 
  distinct(genus) %>% 
  filter(!is.na(genus)) %>% 
  pivot_longer(genus, names_to = 'rank', values_to = 'name') %>% 
  select(name, rank)

# Check that entries aren't getting left out
df_filt %>% 
  count(Polinizador, species, genus, family) 

df_filt %>% 
  distinct(Polinizador, species, genus, family)  %>% 
  anti_join(genera, by = c(genus = 'name'))

df_filt %>% filter(is.na(species))

# Query GBIF 
genera_keys <- genera %>% 
  pmap_dfr(~ name_suggest(.x, rank = .y)$data )

# Single download (prior method) 
keys <- genera_keys %>% dplyr::select(key) %>% deframe

# Cue download - only need to run once 
res <- occ_download(pred_in('taxonKey', keys), 
                    pred('country', 'MX'),
                    pred_notnull('decimalLongitude'),
                    pred_notnull('decimalLatitude'),
                    pred_not(pred('decimalLongitude', 0)),
                    pred_not(pred('decimalLatitude', 0)))

# Wait for download to be ready
(dl_meta <- occ_download_meta(res))
raw_fp <- str_glue("data/input_data/GBIF/gbif_{query_name}.rds")
bird_genera_dat <- download_and_save(res, raw_fp)

# Look
bird_genera_dat %>% count(species, genus, family)
bird_genera_dat %>% filter(family == 'Pteropodidae')

# Genera from literature
query <- c(
  # Agave salmiana
  'Cynanthus', 'genus', # 889
  'Hylocharis', 'genus', # 52 visits
  'Lampornis', 'genus', # 6 visits
  'Eugenes', 'genus', # 591
  'Calothorax', 'genus', # 148
  # passeriformes
  'Colaptes', 'genus',
  'Melanerpes', 'genus', # 13 visits
  'Campylorhynchus', 'genus', # 48 visits
  'Taxostoma', 'genus', 
  'Melanotis', 'genus',
  'Diglossa', 'genus', 
  'Piranga', 'genus',
  'Pheucticus', 'genus', # 1 visit
  'Icterus', 'genus', 
  'Carpodacus', 'genus', # 804 visits
  
  # Only observed by Gómez-Aíza and Zuria, who weren't focused on pollination
  'Archilochus', 'genus', 
  'Selasphorus', 'genus', 
  'Columbina', 'genus', 
  'Picoides', 'genus', # 97 visits
  'Empidonax', 'genus', 
  'Tyrannus', 'genus', 
  'Lanius', 'genus', # 16 visits
  'Thryomanes', 'genus', 
  'Regulus', 'genus', 
  'Polioptila', 'genus',
  'Mimus', 'genus', 
  'Sturnus', 'genus', #59 visits
  'Vermivora', 'genus', 
  'Dendroica', 'genus', 
  'Pipilo', 'genus', 
  'Spizella', 'genus', 
  'Agelaius', 'genus', 
  'Quiscalus', 'genus', 
  'Molothrus', 'genus', 
  'Spinus', 'genus', 
  'Passer', 'genus'# 16 visits
) %>%  
  matrix(ncol = 2, byrow=TRUE) %>% 
  as_tibble()

# Query GBIF 
genera_keys <- query %>% 
  pmap_dfr(~ name_suggest(.x, rank = .y)$data )

# crops
# Agave salmiana - important crop and requires pollinators
# Bomarea edulis - ornamental plant, not important?
# Carica papaya - papaya, requires pollinators
# - bees, hawkmoths (gen. Hyles, fam. Sphingidae), thrips, hummingbirds
# Lactuca sativa - lettuce, doesn't require pollination
# - bees, small insects
# Parmentiera eludis - cuajilote, not common for fruit
# - bats
# Ananas comosus - pineapple, doesn't require pollination
# Backebergia militaris - 


# Thrips ----


