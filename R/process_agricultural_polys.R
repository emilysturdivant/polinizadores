
# Load libraries
library(tidyverse)

# Remove frijol, maiz, granos from INEGI polygons (cross-check first)
# INEGI ----
inegi_polys <- st_read('data/data_out/polys_ag_INEGI_munis.geojson')
load('data/data_out/r_data/sup_frij_gran.RData')
inegi_noFMG <- clip_out_polys_from_ag(inegi_polys, sup_frij_gran)

# Save
save(inegi_noFMG, file='data/data_out/r_data/polys_ag_INEGI_noFMG_AGS.RData')
load('data/data_out/r_data/polys_ag_INEGI_noFMG_AGS.RData')


# SIAP ----
load('data/data_out/r_data/polys_ag_noFMG_AGS.RData')
siap_noFMG <- ag_noFMG

region='CW'
siap_ag_fp <- file.path('data/input_data/SIAP/FASII',
                    str_glue('FASII_{region}_by_municipio.geojson'))
siap_ag <- st_read(siap_ag_fp)
