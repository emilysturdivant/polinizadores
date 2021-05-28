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

pol_groups <- c('Abejas', 'Avispas', 'Colibries', 'Mariposas', 'Moscas', 'Murcielagos')

anp_fp <- 'data/input_data/context_Mexico/SHAPE_ANPS/182ANP_Geo_ITRF08_Julio04_2019.shp'
anp_dir <- 'data/data_out/ANPs'
anp_terr_fp <- file.path(anp_dir, 'ANPs_terr_singlepart.geojson')
anp_stats_fp <- file.path('data/data_out/ANPs', 
                          str_c('anps_allpols_', strftime(date_min, format="%Y"), 
                                '_to_', strftime(date_max, format="%Y"), '_buffer', 
                                buffer_distance, 'km.csv'))

fig_dir <- 'figures/anps_and_pollinator_exploration/with_duplicates_gt1999'

# Mexico
mex <- raster::getData('GADM', country='MEX', level=0, 
                       path='data/input_data/context_Mexico') %>% 
  st_as_sf(crs=4326) %>% 
  st_transform(crs=6372) %>% 
  st_simplify(dTolerance=20)



# Combine all zones into one SFC ----
# This process is also in prep_create_ANP_zones_and_plot_v1.R
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
polys_area_fld <- 'area_ha'
pol_group <- 'Colibries'

# Load pollinator file
pol_dir <- 'data/data_out/pollinator_points/with_duplicates'
pol_fp <- file.path(pol_dir, str_c(pol_group, '.geojson'))
pol_df <- st_read(pol_fp) %>% 
  # Filter to date range
  filter(eventDate >= date_min & eventDate <= date_max) 

# Group pollinator points by intersecting ANP 
pol_df <- pol_df %>% 
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
  left_join(st_drop_geometry(anps_biom_bind))

spec_df_slim <- spec_df %>% 
  # mutate(name = str_c(abbreviate(DESECON1, use.classes=T), '_', rowname)) %>% 
  column_to_rownames('name') %>%
  select(-any_of(c('rowname', 'ID_ANP', 'area_ha', 'DESECON1', 'zone')))

# Filter to single ANP ----
anp_list <- spec_df %>% count(ID_ANP) %>% arrange(desc(n))
anp_id <- anp_list %>% slice(2) %>% select(ID_ANP) %>% deframe

spec_sub <- spec_df %>% 
  filter(ID_ANP == anp_id) %>% 
  column_to_rownames('name') %>%
  select(-any_of(c('rowname', 'ID_ANP', 'area_ha', 'DESECON1', 'zone')))

# Filter to single Biome ----
var <- 'DESECON1'
anp_list <- spec_df %>% count(.data[[var]]) %>% arrange(desc(n))
anp_id <- anp_list %>% slice(1) %>% select(.data[[var]]) %>% deframe

spec_sub <- spec_df %>% 
  filter(.data[[var]] == anp_id) %>% 
  column_to_rownames('name') %>%
  select(-any_of(c('rowname', 'ID_ANP', 'area_ha', 'DESECON1', 'zone')))

# Species accumulation curve ----
sp1 <- specaccum(spec_sub)
sp2 <- specaccum(spec_sub, "random")

sac_tbl <- tibble(sites = sp1$sites,
       richness = sp1$richness,
       sd = sp1$sd)
ggplot(sac_tbl, aes(x=sites, y=richness)) +
  geom_errorbar(aes(ymin=richness-sd, ymax=richness+sd), width=.1, alpha=0.5) +
  geom_line() +
  theme_minimal()

plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

png(file.path(fig_dir, 'specaccum_allpts', str_c('specaccum_', pol_group, '_', anp_id, '.png')))
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
# boxplot(sp2, col="yellow", add=TRUE, pch="+")
dev.off()


mod1 <- fitspecaccum(sp1, "lomolino")
coef(mod1)

png(file.path(fig_dir, 'specaccum_allpts', str_c('specaccum_', pol_group, '_lomolino.png')))
plot(sp1)
plot(mod1, add = TRUE, col=2, lwd=2)
dev.off()

mods <- fitspecaccum(sp2, "arrh")
png(file.path(fig_dir, 'specaccum_allpts', str_c('specaccum_', pol_group, '_arrh.png')))
plot(mods, col="hotpink")
dev.off()
boxplot(sp2, col = "yellow", border = "blue", lty=1, cex=0.3, add= TRUE)

## Use nls() methods to the list of models
sapply(mods$models, AIC)

# iNEXT ------------------------------------------------------------------------
library(iNEXT)

# https://cran.r-project.org/web/packages/iNEXT/vignettes/Introduction.html















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
