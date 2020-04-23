# Convert Apendice 1 to a dataframe. It looks like this produces some errors. Better just to ask for 

library(tabulizer)
library(tidyverse)

#---
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
